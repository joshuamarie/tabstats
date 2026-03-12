#' Display a Correlation Matrix Table in the Console
#'
#' @param display A `corr_spec` object from `new_corr_data()`, or a plain
#'   symmetric matrix (e.g. from `cor()`).
#' @param title Label shown in the title (e.g. `"Pearson Correlation Matrix"`).
#'   Auto-detected from a `title` attribute on the spec if present.
#' @param diag_1 If `TRUE`, diagonal cells always show `"1"`. Default `TRUE`.
#' @param digits Decimal places for numeric formatting. Default `3`.
#' @param layout_view Show a layout legend box above the table? Default `FALSE`.
#' @param layout_center Center the layout box in the terminal? Default `FALSE`.
#' @param center_table Center table in terminal? Default `FALSE`.
#' @param border_char Border character. Default from `getOption("tab_default")`.
#' @param style A `cm_style()` object. Keys match the extra field names passed
#'   to `new_corr_data()` (e.g. `rho`, `pval`, `bf`), plus `title` and
#'   `border_text`.
#' @param ... Reserved for future use.
#'
#' @return Invisibly returns the rendered character matrix.
#'
#' @examples
#' # From a plain correlation matrix, e.g. using `cor()`
#' corr_matrix(cor(mtcars[, 1:4]), title = "Pearson Correlation Matrix")
#'
#' # Customizable example
#' spec = new_corr_data(
#'     var1 = c("a", "a", "b"),
#'     var2 = c("b", "c", "c"),
#'     rho = c("0.89", "0.79", "0.66"),
#'     pval = c("<0.001", "<0.001", "<0.001")
#' )
#' corr_matrix(spec, title = "Pearson Correlation Matrix", layout_view = TRUE)
#'
#' @export
corr_matrix = function(
        display,
        title = NULL,
        diag_1 = TRUE,
        digits = 3,
        layout_view = FALSE,
        layout_center = FALSE,
        center_table = FALSE,
        border_char = getOption("tab_default")$border_char,
        style = list(),
        ...
) {
    if (!requireNamespace("cli", quietly = TRUE))
        stop("The 'cli' package is required. Please install it.")

    # ---- Validate style ----

    if (!is.null(style) && !is.list(style))
        cli::cli_abort("{.arg style} must be a list or a style object (e.g. {.fn cm_style}).")

    if (inherits(style, "tabstats_style") && !inherits(style, "cm_style")) {
        cli::cli_abort(
            "{.arg style} must be an {.cls cm_style} object for {.fn table_summary}.",
            "x" = "Got {.cls {class(style)[1]}}."
        )
    }

    # ---- Coerce input --------------------------------------------------------
    spec = if (is.matrix(display)) {
        matrix_spec_resolver_cm(display, digits)
    } else if (inherits(display, "corr_spec")) {
        display
    } else {
        stop("`display` must be a `corr_spec` object or a symmetric matrix.", call. = FALSE)
    }

    title = if (is.null(title)) {
        t = attr(spec, "title")
        if (is.null(t)) "Correlation Matrix" else t
    } else {
        title
    }

    style = style_resolver_cm(style, names(spec$extras))

    field_names = names(spec$extras)
    if (length(field_names) == 0) field_names = "corr"
    n_fields = length(field_names)

    # ---- Layout view ---------------------------------------------------------
    if (layout_view)
        print_layout_cm(field_names, style, border_char, layout_center, center_table)

    # ---- Build render matrix -------------------------------------------------
    vars = spec$vars
    n = length(vars)

    mat = matrix("", nrow = n * n_fields, ncol = n + 1L)

    for (i in seq_len(n))
        mat[(i - 1L) * n_fields + 1L, 1L] = vars[i]

    for (p in seq_along(spec$var1)) {
        v1 = spec$var1[p]
        v2 = spec$var2[p]

        if (v1 == v2) {
            i = match(v1, vars)
            row_base = (i - 1L) * n_fields + 1L
            if (diag_1) {
                mat[row_base, i + 1L] = "1"
            } else {
                mat[row_base, i + 1L] = spec$extras[[1L]][p]
                for (k in seq_along(spec$extras)[-1L])
                    mat[row_base + k - 1L, i + 1L] = spec$extras[[k]][p]
            }
            next
        }

        ri = match(v1, vars)
        ci = match(v2, vars)

        if (spec$pattern %in% c("lt", "lte")) {
            if (ri < ci) { tmp = ri; ri = ci; ci = tmp }
        } else if (spec$pattern %in% c("gt", "gte")) {
            if (ri > ci) { tmp = ri; ri = ci; ci = tmp }
        }

        row_base = (ri - 1L) * n_fields + 1L
        mat[row_base, ci + 1L] = spec$extras[[1L]][p]

        for (k in seq_along(spec$extras)[-1L])
            mat[row_base + k - 1L, ci + 1L] = spec$extras[[k]][p]
    }

    # ---- Column widths -------------------------------------------------------
    col_names = c("Variable", vars)
    strip = function(x) nchar(gsub("\033\\[[0-9;]*m", "", x))

    col_widths = vapply(
        seq_len(ncol(mat)),
        function(j) {
            max(
                strip(col_names[j]),
                vapply(seq_len(nrow(mat)), function(i) strip(mat[i, j]), integer(1))
            )
        },
        integer(1)
    )

    total_width = sum(col_widths) + 3L * (length(col_widths) - 1L) + 4L
    hline = style[["border_text"]](strrep(border_char, total_width))
    prefix = left_pad_cm(center_table, total_width)

    # ---- Apply styles --------------------------------------------------------
    styled = matrix_styler_cm(mat, field_names, style)

    # ---- Print ---------------------------------------------------------------
    cat("\n", prefix, style[["title"]](align_center(title, total_width)), "\n", sep = "")
    cat(prefix, hline, "\n", sep = "")
    cat(prefix, format_row_cm(col_names, col_widths, left_align_first = TRUE), "\n", sep = "")
    cat(prefix, hline, "\n", sep = "")

    for (i in seq_len(nrow(mat))) {
        cat(prefix, format_row_cm(styled[i, ], col_widths, left_align_first = TRUE), "\n", sep = "")
        if (i %% n_fields == 0L)
            cat(prefix, hline, "\n", sep = "")
    }

    invisible(mat)
}
