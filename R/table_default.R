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
#'   from [td_style()], or a named list where each name is a column name or
#'   `"title"`, and each value is either a cli style string (e.g. `"blue_bold"`)
#'   or a function `\(ctx) ...` receiving a context list.
#' @param style_colnames Styling for column header cells. A `td_style` object
#'   from [td_style()], or a named list where each name is a column name or
#'   `"title"`, and each value is either a cli style string (e.g. `"blue_bold"`)
#'   or a function `\(ctx) ...` receiving a context list.
#' @param style_columns Styling for data cells. A `td_style` object from
#'   [td_style()], or a named list where each name is a column name or column
#'   index as a string, and each value is a cli style string or a function
#'   `\(ctx) ...` receiving a context list with elements `value`,
#'   `formatted_value`, `col_name`, `col_index`, `is_header`, `data`,
#'   `justify`, and `width`.
#' @param nrows Max rows to display before truncation.
#' @param vb Vertical border spec: `list(char = "\u2502", after = c(1, 3))`.
#' @param auto_wrap Auto-wrap wide tables? Default `TRUE`.
#' @param wrap_threshold Fraction of console width before wrapping. Default `1`.
#' @param ... Reserved for future use.
#'
#' @examples
#' table_default(head(mtcars))
#' table_default(head(mtcars), style_columns = td_style(mpg = "cyan", cyl = "magenta"))
#'
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

    # ---- Validate style ----

    if (!is.null(style_colnames) && !inherits(style_colnames, "td_style")) {
        cli::cli_abort(
            "{.arg style_colnames} must be a {.cls td_style} object.",
            "i" = "Use {.fn td_style} to build one."
        )
    }

    if (!is.null(style_columns) && !inherits(style_columns, "td_style")) {
        cli::cli_abort(
            "{.arg style_columns} must be a {.cls td_style} object.",
            "i" = "Use {.fn td_style} to build one."
        )
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
    if (truncated) x = utils::head(x, nrows)

    # ---- Row names -----------------------------------------------------------
    x = tibble::as_tibble(x, rownames = if (show_row_names) "row_names" else NA)
    if (show_row_names && !"row_names" %in% names(x)) {
        # x = dplyr::mutate(x, row_names = as.character(seq_len(nrow(x))), .before = 1)
        x$row_names = as.character(seq_len(nrow(x)))
        x = x[, c("row_names", setdiff(names(x), "row_names")), drop = FALSE]
    } else if (show_row_names) {
        # x = dplyr::relocate(x, "row_names", .before = 1)
        x = x[, c("row_names", setdiff(names(x), "row_names")), drop = FALSE]
    }

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
