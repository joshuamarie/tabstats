#' Display a formatted table in the console
#'
#' @param x A data frame or tibble.
#' @param justify_cols Alignment: a single string, vector, or named list of "left"/"right"/"center".
#' @param digits Digits to round numeric columns to. Default `3`.
#' @param digits_by_col Named list of per-column digit overrides.
#' @param scientific Display numerics in scientific notation? Default `FALSE`.
#' @param na_print String for missing values. Default `""`.
#' @param min_width Minimum column width. Default `NULL`.
#' @param border_char Character for borders. Default `"\u2500"`.
#' @param show_row_names Show row names? Default `FALSE`.
#' @param center_table Center table in terminal? Default `FALSE`.
#' @param n_space Spaces between columns. Default `2`.
#' @param title Optional title string above the table.
#' @param style_colnames Styling for column headers (cli string or lambda).
#' @param style_columns Styling for data cells (cli string or lambda).
#' @param nrows Max rows to display before truncation.
#' @param vb Vertical border spec: `list(char = "\u2502", after = c(1, 3))`.
#' @param auto_wrap Auto-wrap wide tables? Default `TRUE`.
#' @param wrap_threshold Fraction of console width before wrapping. Default `1`.
#' @param ... Reserved for future use.
#'
#' @examples
#' table_default(head(mtcars))
#' table_default(head(mtcars), style_columns = list(mpg = "cyan", cyl = "magenta"))
#'
#' @importFrom cli console_width col_red col_blue col_green cli_alert_info cli_alert_warning
#' @importFrom utils head
#' @export
table_default = function(
    x,
    justify_cols = "center",
    digits = 3,
    digits_by_col = NULL,
    scientific = FALSE,
    na_print = "",
    min_width = NULL,
    border_char = options("tab_default")$tab_default$border_char,
    show_row_names = FALSE,
    center_table = FALSE,
    n_space = 2,
    title = NULL,
    style_colnames = NULL,
    style_columns = NULL,
    nrows = getOption("tab_default")$nrows,
    vb = list(),
    auto_wrap = TRUE,
    wrap_threshold = 1,
    ...
) {

    # ---- Coerce input --------------------------------------------------------
    if (!inherits(x, "data.frame")) {
        x = try(as.data.frame(x), silent = TRUE)
        if (inherits(x, "try-error"))
            stop("`x` must be a data frame or coercible to one.", call. = FALSE)
    }

    # ---- Validate scalar params ----------------------------------------------
    if (!is.numeric(n_space) || n_space < 0) {
        cli::cli_warn("`n_space` must be non-negative; using default 2.")
        n_space = 2
    }
    if (!is.numeric(nrows) || nrows < 0) {
        cli::cli_warn("`nrows` must be non-negative; using default 10.")
        nrows = 10
    }
    n_space = floor(n_space)
    nrows = floor(nrows)

    # ---- Truncation ----------------------------------------------------------
    original_nrow = nrow(x)
    truncated = original_nrow > nrows
    if (truncated) x = head(x, nrows)

    # ---- Row names -----------------------------------------------------------
    x = tibble::as_tibble(x, rownames = if (show_row_names) "row_names" else NA)
    if (show_row_names && !"row_names" %in% names(x))
        x = dplyr::mutate(x, row_names = as.character(seq_len(nrow(x))), .before = 1)
    else if (show_row_names)
        x = dplyr::relocate(x, "row_names", .before = 1)

    original_x = x

    # ---- Early exits ---------------------------------------------------------
    if (nrow(x) == 0 && ncol(x) == 0) {
        cat("Empty data frame (0 rows, 0 columns)\n")
        return(invisible(NULL))
    }
    if (ncol(x) == 0) {
        cat("Data frame has 0 columns.\n")
        return(invisible(NULL))
    }

    # ---- Format & measure ----------------------------------------------------
    x = data_formatter(x, digits, digits_by_col, scientific, na_print)
    col_names = colnames(x)
    x_chars = as.matrix(x)
    col_widths = col_widths_total(x, x_chars, col_names, style_columns, original_x, min_width)

    # ---- Vertical border setup -----------------------------------------------
    vb_setup = vb_resolve(vb, col_names, col_widths)

    # ---- Terminal & wrap check -----------------------------------------------
    term_width = tryCatch(
        cli::console_width(),
        error = function(e) as.integer(getOption("width")) %||% 80
    )
    total_width = width_total(col_widths, n_space, vb_setup$n_borders, nchar(vb_setup$char))
    should_wrap = auto_wrap && total_width > (term_width * wrap_threshold) && length(col_widths) > 2

    # ---- Print ---------------------------------------------------------------
    if (truncated)
        cli::cli_alert_info("Showing {nrows} of {original_nrow} rows")

    if (should_wrap) {
        .print_wrapped(
            x_chars, col_names, col_widths, vb_setup,
            justify_cols, n_space, title,
            style_colnames, style_columns, original_x,
            center_table, term_width, border_char, wrap_threshold
        )
    } else {
        .print_table(
            x_chars, col_names, col_widths, vb_setup,
            justify_cols, n_space, title,
            style_colnames, style_columns, original_x,
            center_table, term_width, total_width, border_char
        )
    }

    if (truncated)
        cli::cli_alert_warning("Displayed only {nrows} of {original_nrow} rows. Set `nrows` to show more.")

    invisible(x)
}

data_formatter = function(
    x,
    digits,
    digits_by_col,
    scientific,
    na_print
) {
    x = dplyr::mutate(x, dplyr::across(tidyselect::where(is.factor), as.character))

    fmt_num = function(col, col_name) {
        d = digits_by_col[[col_name]] %||% digits
        int_like = all(is.na(col) | col %% 1 == 0)
        out = character(length(col))
        out[is.na(col)] = na_print
        nna = !is.na(col)

        out[nna] = if (int_like) {
            format(col[nna], scientific = FALSE, trim = TRUE)
        } else if (scientific) {
            format(col[nna], digits = d, scientific = TRUE, trim = TRUE)
        } else {
            format(round(col[nna], d), nsmall = d, scientific = FALSE, trim = TRUE)
        }
        out
    }

    for (nm in names(x)[vapply(x, is.numeric, FUN.VALUE = logical(1))])
        x[[nm]] = fmt_num(x[[nm]], nm)

    x = dplyr::mutate(
        x,
        dplyr::across(tidyselect::where(\(c) !is.numeric(c) && !is.character(c)), as.character),
        dplyr::across(tidyselect::where(\(c) !is.numeric(c)), \(c) ifelse(is.na(c), "NA", c)),
        dplyr::across(dplyr::everything(), as.character)
    )
    x
}

#' Total Widths by Column
#'
#' @keywords internal
#' @noRd
col_widths_total = function(
    x,
    x_chars,
    col_names,
    style_columns,
    original_x,
    min_width
) {
    widths = x |>
        lapply(nchar) |>
        vapply(max, integer(1), na.rm = TRUE) |>
        pmax(nchar(col_names))

    if (is.list(style_columns)) {
        for (i in seq_along(col_names)) {
            fn = style_mapper(style_columns, col_names[i], i)
            if (!is.function(fn)) next

            plain_vals = vapply(seq_len(nrow(x)), function(r) {
                ctx = ctx_maker(x_chars[r, i], col_names[i], i, FALSE, original_x, widths[i])
                res = tryCatch(fn(ctx), error = function(e) x_chars[r, i])
                if (is.character(res) && length(res) == 1)
                    gsub("\033\\[[0-9;]*m", "", res)
                else
                    x_chars[r, i]
            }, character(1))

            widths[i] = max(widths[i], nchar(plain_vals), na.rm = TRUE)
        }
    }

    if (!is.null(min_width) && is.numeric(min_width))
        widths = pmax(widths, floor(min_width[1]))

    widths
}

#' Vertical border resolution
#'
#' @keywords internal
#' @noRd
vb_resolve = function(
    vb,
    col_names,
    col_widths
) {
    char = vb$char %||% getOption("tab_default")$vb_char
    spec = vb$after
    idx = integer(0)

    if (length(spec) > 0) {
        idx = if (is.numeric(spec)) spec else match(spec, col_names)
        idx = sort(unique(idx[!is.na(idx) & idx > 0 & idx < length(col_widths)]))
    }

    list(char = char, idx = idx, n_borders = length(idx))
}

#' Total width calculator
#'
#' @keywords internal
#' @noRd
width_total = function(
    col_widths,
    n_space,
    n_borders,
    vb_char_w
) {
    sum(col_widths) + (length(col_widths) - 1) * n_space + n_borders * (1 + vb_char_w) + 4
}

left_padding = function(center_table, term_width, total_width) {
    if (center_table && term_width > total_width)
        strrep(" ", floor((term_width - total_width) / 2))
    else
        ""
}

#' Optimal horizontal line drawer
#'
#' @keywords internal
#' @noRd
horizontal_line = function(col_widths, n_space, vb_idx, border_char, mid_char = border_char) {
    line = paste0(border_char, border_char)

    for (i in seq_along(col_widths)) {
        line = paste0(line, strrep(border_char, col_widths[i]))
        if (i < length(col_widths)) {
            sep = if (i %in% vb_idx)
                paste0(border_char, mid_char, strrep(border_char, n_space))
            else
                strrep(border_char, n_space)
            line = paste0(line, sep)
        }
    }

    paste0(line, border_char, border_char)
}

line_maker = function(col_widths, n_space, vb_idx, vb_char, border_char, n_borders) {
    if (n_borders == 0) {
        w = sum(col_widths) + max(0, length(col_widths) - 1) * n_space + 4
        line = strrep(border_char, w)
        return(list(top = line, mid = line, bot = line, title = line))
    }

    con = getOption("tab_default")
    list(
        top = horizontal_line(col_widths, n_space, vb_idx, border_char, con$vb_top),
        mid = horizontal_line(col_widths, n_space, vb_idx, border_char, con$vb_mid),
        bot = horizontal_line(col_widths, n_space, vb_idx, border_char, con$vb_bottom),
        title = horizontal_line(col_widths, n_space, integer(0), border_char)
    )
}

title_display = function(title, lines, justify_cols, style_colnames, original_x, pad) {
    inner_w = nchar(lines$title) - 2
    just = if (is.list(justify_cols) && "title" %in% names(justify_cols))
        justify_cols[["title"]]
    else
        "center"
    fmt = justify_text(title, inner_w, just)

    if (is.list(style_colnames) && "title" %in% names(style_colnames)) {
        fn = style_colnames[["title"]]
        if (is.function(fn)) {
            ctx = list(
                value = title,
                formatted_value = fmt,
                col_name = "title",
                col_index = 0,
                is_header = TRUE,
                data = original_x,
                justify = just,
                width = inner_w
            )
            styled = tryCatch(fn(ctx), error = function(e) fmt)
            if (is.character(styled) && length(styled) == 1) fmt = styled
        }
    }

    cat(pad, lines$title, "\n", sep = "")
    cat(pad, " ", fmt, " ", "\n", sep = "")
}

col_header_display = function(col_names, col_widths, justify_cols, n_space,
                          style_colnames, original_x, vb, pad, mid_line) {
    header = format_row(
        stats::setNames(col_names, col_names),
        col_widths, justify_cols, n_space,
        style_colnames, original_x, TRUE, vb
    )
    cat(pad, header, "\n", sep = "")
    cat(pad, mid_line, "\n", sep = "")
}

row_display = function(x_chars, col_names, col_widths, justify_cols, n_space,
                        style_columns, original_x, vb, pad) {
    for (i in seq_len(nrow(x_chars))) {
        row = stats::setNames(x_chars[i, ], col_names)
        cat(
            pad,
            format_row(row, col_widths, justify_cols, n_space, style_columns, original_x, FALSE, vb),
            "\n",
            sep = ""
        )
    }
}

.print_table = function(
        x_chars, col_names, col_widths, vb_setup,
        justify_cols, n_space, title,
        style_colnames, style_columns, original_x,
        center_table, term_width, total_width, border_char
    ) {
    lines = line_maker(col_widths, n_space, vb_setup$idx, vb_setup$char, border_char, vb_setup$n_borders)
    pad = left_padding(center_table, term_width, total_width)
    vb = list(char = vb_setup$char, after = vb_setup$idx)

    if (!is.null(title)) {
        title_display(title, lines, justify_cols, style_colnames, original_x, pad)
    }
    cat(pad, lines$top, "\n", sep = "")
    col_header_display(col_names, col_widths, justify_cols, n_space, style_colnames, original_x, vb, pad, lines$mid)
    row_display(x_chars, col_names, col_widths, justify_cols, n_space, style_columns, original_x, vb, pad)
    cat(pad, lines$bot, "\n", sep = "")
}

.print_wrapped = function(
        x_chars, col_names, col_widths, vb_setup,
        justify_cols, n_space, title,
        style_colnames, style_columns, original_x,
        center_table, term_width, border_char, wrap_threshold
    ) {
    first_w = col_widths[1] + 4 + n_space
    avail_w = term_width * wrap_threshold - first_w
    col_groups = group_cols(2:length(col_widths), col_widths, avail_w, n_space)

    for (g in seq_along(col_groups)) {
        cur_cols = c(1L, col_groups[[g]])
        cur_names = col_names[cur_cols]
        cur_w = col_widths[cur_cols]
        cur_chars = x_chars[, cur_cols, drop = FALSE]
        cur_vb_idx = vb_remap(vb_setup$idx, cur_cols)
        cur_vb = list(char = vb_setup$char, after = cur_vb_idx)
        cur_nb = length(cur_vb_idx)
        cur_total = width_total(cur_w, n_space, cur_nb, nchar(vb_setup$char))
        pad = left_padding(center_table, term_width, cur_total)
        lines = line_maker(cur_w, n_space, cur_vb_idx, vb_setup$char, border_char, cur_nb)

        if (g == 1) {
            if (!is.null(title))
                title_display(title, lines, justify_cols, style_colnames, original_x, pad)
        } else {
            cat("\n---\n\n")
        }

        cat(pad, lines$top, "\n", sep = "")
        col_header_display(cur_names, cur_w, justify_cols, n_space, style_colnames, original_x, cur_vb, pad, lines$mid)
        row_display(cur_chars, cur_names, cur_w, justify_cols, n_space, style_columns, original_x, cur_vb, pad)
        cat(pad, lines$bot, "\n", sep = "")
    }
}

group_cols = function(cols, col_widths, avail_w, n_space) {
    groups = vector("list", length(cols))
    g = 0L
    cur = integer(length(cols))
    cur_n = 0L
    cur_w = 0

    for (i in cols) {
        w = col_widths[i] + n_space
        if (cur_n == 0L || cur_w + w <= avail_w) {
            cur_n = cur_n + 1L
            cur[cur_n] = i
            cur_w = cur_w + w
        } else {
            g = g + 1L
            groups[[g]] = cur[seq_len(cur_n)]
            cur[1L] = i
            cur_n = 1L
            cur_w = w
        }
    }
    if (cur_n > 0L) {
        g = g + 1L
        groups[[g]] = cur[seq_len(cur_n)]
    }

    groups[seq_len(g)]
}

vb_remap = function(vb_idx, cur_cols) {
    if (length(vb_idx) == 0) return(integer(0))
    mapped = match(vb_idx, cur_cols)
    sort(unique(mapped[!is.na(mapped) & mapped < length(cur_cols)]))
}

ctx_maker = function(value, col_name, col_idx, is_header, data, width) {
    list(
        value = value,
        formatted_value = value,
        col_name = col_name,
        col_index = col_idx,
        is_header = is_header,
        data = data,
        justify = "center",
        width = width
    )
}

style_mapper = function(styles, col_name, col_idx) {
    if (is.null(styles)) return(NULL)
    if (!is.null(names(styles))) {
        if (col_name %in% names(styles)) return(styles[[col_name]])
        if (as.character(col_idx) %in% names(styles)) return(styles[[as.character(col_idx)]])
    }
    if (is.null(names(styles)) && col_idx <= length(styles)) return(styles[[col_idx]])
    NULL
}
