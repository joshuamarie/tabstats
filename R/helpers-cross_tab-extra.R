digits_resolver = function(digits) {
    defaults = getOption("tab_digits")
    if (is.null(digits)) return(defaults)
    if (is.numeric(digits) && length(digits) == 1)
        return(list(ex = digits, row_pct = digits, col_pct = digits, total_pct = digits))
    if (is.list(digits))
        return(utils::modifyList(defaults, digits))
    defaults
}

pct_resolver = function(percentage) {
    if (is.null(percentage) || length(percentage) == 0 || identical(percentage, FALSE))
        return(list(row = FALSE, col = FALSE, total = FALSE))
    if (identical(percentage, TRUE) || any(c("all", "ALL") %in% percentage))
        return(list(row = TRUE, col = TRUE, total = TRUE))
    list(
        row = any(c("by_row", "row", "rows") %in% percentage),
        col = any(c("by_col", "col", "cols", "column", "columns") %in% percentage),
        total = any(c("by_total", "total") %in% percentage)
    )
}

style_resolver = function(style) {
    identity_fn = function(ctx) ctx$formatted_text
    defaults = list(
        observed = identity_fn,
        expected = identity_fn,
        row_percentage = identity_fn,
        col_percentage = identity_fn,
        total_percentage = identity_fn,
        total = identity_fn,
        title = identity_fn,
        border = options("tab_default")$tab_default$border_char,
        border_text = identity_fn
    )
    utils::modifyList(defaults, if (is.null(style)) list() else style)
}

styler_ct = function(
        style_item,
        text,
        type,
        value = NULL,
        is_total = FALSE
) {
    ctx = list(
        formatted_text = text,
        value = if (is.null(value)) text else value,
        type = type,
        is_total = is_total,
        raw = TRUE
    )
    if (is.function(style_item)) {
        res = tryCatch(style_item(ctx), error = function(e) text)
        return(if (is.character(res)) res else text)
    }
    if (is.character(style_item) && requireNamespace("cli", quietly = TRUE)) {
        for (part in unlist(strsplit(style_item, "_"))) {
            fn = tryCatch(
                if (existsFunction(paste0("col_", part), "cli")) get(paste0("col_", part), envir = asNamespace("cli"))
                else if (existsFunction(paste0("style_", part), "cli")) get(paste0("style_", part), envir = asNamespace("cli"))
                else if (existsFunction(part, "cli")) get(part, envir = asNamespace("cli"))
                else NULL,
                error = function(e) NULL
            )
            if (!is.null(fn)) text = tryCatch(fn(text), error = function(e) text)
        }
    }
    text
}

existsFunction = function(name, pkg) {
    exists(name, where = asNamespace(pkg), mode = "function")
}

pct_formatter_ct = function(x, d) {
    if (is.na(x) || x == 100) return(NULL)
    sprintf(paste0("%.", d, "f%%"), x)
}

num_formatter_ct = function(x, d = 1) {
    if (abs(x - round(x)) < 1e-10) sprintf("%d", as.integer(round(x)))
    else sprintf(paste0("%.", d, "f"), x)
}

cell_width_ct = function(cell) {
    if (!is.character(cell) || cell == "") return(0L)
    max(nchar(strip_ansi(unlist(strsplit(cell, "\n")))))
}

format_cell_ct = function(
        obs,
        exp,
        row_pct,
        col_pct,
        grand_pct,
        is_total,
        show_expected,
        pct_flags,
        digits,
        style
) {
    obs_text = sprintf("%d", as.integer(obs))
    if (show_expected && !is_total) {
        exp_text = sprintf("(%s)", num_formatter_ct(exp, digits$ex))
        main = paste(
            styler_ct(style$observed, obs_text, "observed", obs, is_total),
            styler_ct(style$expected, exp_text, "expected", exp, is_total)
        )
    } else {
        s = if (is_total) style$total else style$observed
        main = styler_ct(s, obs_text, if (is_total) "total" else "observed", obs, is_total)
    }

    lines = main
    if (pct_flags$row && !is.null(txt <- pct_formatter_ct(row_pct, digits$row_pct)))
        lines = paste0(lines, "\n", styler_ct(style$row_percentage, txt, "row_percentage", row_pct, is_total))
    if (pct_flags$col && !is.null(txt <- pct_formatter_ct(col_pct, digits$col_pct)))
        lines = paste0(lines, "\n", styler_ct(style$col_percentage, txt, "col_percentage", col_pct, is_total))
    if (pct_flags$total && !is.null(txt <- pct_formatter_ct(grand_pct, digits$total_pct)))
        lines = paste0(lines, "\n", styler_ct(style$total_percentage, txt, "total_percentage", grand_pct, is_total))

    lines
}

print_ct_layout = function(
        show_expected,
        pct_flags,
        style,
        layout_center,
        center_table
) {
    w = 36L
    top = paste0("\u250C", strrep("\u2500", w - 2L), "\u2510")
    mid = paste0("\u251C", strrep("\u2500", w - 2L), "\u2524")
    bot = paste0("\u2514", strrep(getOption("tab_default")$border_char, w - 2L), "\u2518")
    inner = function(txt) paste0("| ", center_text_x2(txt, w - 4L), " |")

    obs_styled = styler_ct(style$observed, "< Freq >", "observed")
    header = if (show_expected) {
        paste(obs_styled, styler_ct(style$expected, "(< Expected Value >)", "expected"))
    } else {
        obs_styled
    }

    box = c(
        top,
        inner(styler_ct(style$title, "Layout for Cont. Table", "title")),
        mid,
        inner(header)
    )
    if (pct_flags$row)
        box = c(box, inner(styler_ct(style$row_percentage, "< % by total row >", "row_percentage")))
    if (pct_flags$col)
        box = c(box, inner(styler_ct(style$col_percentage, "< % by total column >", "col_percentage")))
    if (pct_flags$total)
        box = c(box, inner(styler_ct(style$total_percentage, "< % by grand total >", "total_percentage")))

    box = c(box, bot)

    cat("\n")
    for (line in box) {
        if (layout_center || center_table) {
            term_w = tryCatch(
                cli::console_width(),
                error = function(e) as.integer(getOption("width"))
            )
            pad = strrep(" ", max(0L, floor((term_w - nchar(strip_ansi(line))) / 2L)))
            cat(pad, line, "\n", sep = "")
        } else {
            cat(line, "\n")
        }
    }
    cat("\n")
}

col_formatter_ct = function(text, width, j) {
    if (j == 1) sprintf("  %-*s", width, text)
    else center_text_x2(text, width)
}

line_builder_ct = function(
        combined,
        all_col_names,
        col_widths,
        main_col,
        main_col_width,
        total_width,
        header_underline,
        center_table,
        hline,
        style,
        border_char,
        pct_flags
) {
    nc = ncol(combined)
    nr = nrow(combined)

    title_text = paste("Cross Tabulation:", all_col_names[1], "by", main_col)
    title_styled = center_text_x2(styler_ct(style$title, title_text, "title"), total_width)

    header_row = paste(
        vapply(seq_len(nc), function(j) {
            col_formatter_ct(all_col_names[j], col_widths[j], j)
        }, character(1)),
        collapse = "   "
    )

    col_label = sprintf("%-*s%s", col_widths[1], "", center_text_x2(main_col, main_col_width))

    sub_hline = if (header_underline) {
        prefix = sprintf("  %-*s", col_widths[1] + 1L, "")
        paste0(prefix, strrep(options("tab_default")$tab_default[[5]], main_col_width - 4L), "  ")
    } else {
        hline
    }

    lines = c(title_styled, hline, col_label, sub_hline, paste0(header_row, "  "), hline)

    for (i in seq_len(nr - 1L)) {
        lines = c(lines, row_renderer_ct(combined, i, col_widths, style, is_total = FALSE))
        if (i < nr - 1L) lines = c(lines, "")
    }

    lines = c(lines, hline, row_renderer_ct(combined, nr, col_widths, style, is_total = TRUE), hline)
    lines
}

row_renderer_ct = function(
        combined,
        i,
        col_widths,
        style,
        is_total
) {
    nc = ncol(combined)
    cell_lines = lapply(seq_len(nc), function(j) {
        unlist(strsplit(as.character(combined[i, j]), "\n"))
    })
    n_lines = max(vapply(cell_lines, length, integer(1)))

    vapply(seq_len(n_lines), function(ln) {
        parts = vapply(seq_len(nc), function(j) {
            txt = if (ln <= length(cell_lines[[j]])) cell_lines[[j]][ln] else ""
            if (j == 1 && ln == 1) {
                txt = styler_ct(
                    style$x_side, txt, "x_side",
                    value = combined[i, 1], is_total = is_total
                )
            }
            col_formatter_ct(txt, col_widths[j], j)
        }, character(1))
        paste0(paste(parts, collapse = "   "), "  ")
    }, character(1))
}
