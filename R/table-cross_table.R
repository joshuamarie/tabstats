#' Generate and Display a Cross Tabulation Table
#'
#' @param data A matrix or table of observed frequencies.
#' @param percentage Which percentages to show. Options: `TRUE`/`"all"`, `"by_row"`,
#'   `"by_col"`, `"by_total"`, `FALSE`/`NULL`.
#' @param layout Show layout legend box? Default `TRUE`.
#' @param expected Show expected frequencies? Default `TRUE`.
#' @param layout_center Center the layout box? Default `FALSE`.
#' @param header_underline Shorten the underline beneath the column header? Default `FALSE`.
#' @param digits Named list with keys `ex`, `row_pct`, `col_pct`, `total_pct`.
#'   Falls back to `getOption("ct_digits")`.
#' @param center_table Center entire table in terminal? Default `FALSE`.
#' @param style Named list supplied using [ct_style()].
#' @param ... Reserved for future use.
#'
#' @return Invisibly returns the formatted cross-tabulation matrix.
#'
#' @examples
#' cross_table(matrix(c(10, 20, 30, 40), nrow = 2))
#' cross_table(matrix(c(10, 20, 30, 40), nrow = 2), percentage = "all")
#'
#' @export
cross_table = function(
        data,
        percentage = NULL,
        layout = TRUE,
        expected = TRUE,
        layout_center = FALSE,
        header_underline = FALSE,
        digits = NULL,
        center_table = FALSE,
        style = NULL,
        ...
) {
    # ---- Validate style ----
    if (!is.null(style) && !is.list(style))
        cli::cli_abort("{.arg style} must be a list or a style object (e.g. {.fn ct_style}).")

    if (inherits(style, "tabstats_style") && !inherits(style, "ct_style")) {
        cli::cli_abort(
            "{.arg style} must be an {.cls ct_style} object for {.fn table_summary}.",
            "x" = "Got {.cls {class(style)[1]}}."
        )
    }

    # ---- Data ----
    observed = as.matrix(data)
    row_totals = rowSums(observed)
    col_totals = colSums(observed)
    grand_total = sum(observed)
    expected_vals = outer(row_totals, col_totals) / grand_total

    digits = digits_resolver(digits)
    pct_flags = pct_resolver(percentage)
    style = style_resolver(style)

    row_pcts = sweep(observed, 1, row_totals, "/") * 100
    col_pcts = sweep(observed, 2, col_totals, "/") * 100
    grand_pcts = observed / grand_total * 100

    # ---- Metadata: To display ----
    row_names = if (is.null(rownames(observed))) paste0("Row", seq_len(nrow(observed))) else rownames(observed)
    col_names = if (is.null(colnames(observed))) paste0("Col", seq_len(ncol(observed))) else colnames(observed)
    main_row = if (is.null(names(dimnames(observed))[1])) "x" else names(dimnames(observed))[1]
    main_col = if (is.null(names(dimnames(observed))[2])) "y" else names(dimnames(observed))[2]

    border_char =
        if (is.character(style$border) && nchar(style$border) == 1) {
            style$border
        } else {
            options("tab_default")$tab_default$border_char
        }

    # ---- Build cell matrix ----
    nr = nrow(observed)
    nc = ncol(observed)
    combined = matrix("", nrow = nr + 1, ncol = nc + 1)

    for (i in seq_len(nr)) {
        for (j in seq_len(nc)) {
            combined[i, j] = format_cell_ct(
                observed[i, j], expected_vals[i, j],
                row_pcts[i, j], col_pcts[i, j], grand_pcts[i, j],
                FALSE, expected, pct_flags, digits, style
            )
        }
        combined[i, nc + 1] = format_cell_ct(
            row_totals[i], row_totals[i],
            100, NA, row_totals[i] / grand_total * 100,
            TRUE, expected, pct_flags, digits, style
        )
    }
    for (j in seq_len(nc)) {
        combined[nr + 1, j] = format_cell_ct(
            col_totals[j], col_totals[j],
            NA, 100, col_totals[j] / grand_total * 100,
            TRUE, expected, pct_flags, digits, style
        )
    }
    grand_pct_val = if (is.null(percentage) || length(percentage) == 0) NA else 100
    combined[nr + 1, nc + 1] = format_cell_ct(
        grand_total, grand_total, NA, NA, grand_pct_val,
        TRUE, expected, pct_flags, digits, style
    )

    all_row_names = c(row_names, "TOTAL")
    all_col_names = c(main_row, col_names, "TOTAL")
    combined = cbind(all_row_names, combined)

    # ---- Column widths ----
    col_widths = vapply(seq_len(ncol(combined)), function(j) {
        max(
            nchar(strip_ansi(all_col_names[j])),
            vapply(seq_len(nrow(combined)), function(i) {
                cell_width_ct(combined[i, j])
            }, integer(1))
        )
    }, integer(1))

    total_width = sum(col_widths) + 3 * (ncol(combined) - 1) + 4
    main_col_width = sum(col_widths[-1]) + 3 * (length(col_widths) - 2)

    hline_raw = strrep(border_char, total_width)
    hline = styler_ct(style$border_text, hline_raw, "border")

    # ---- Layout box -----
    if (layout) {
        print_ct_layout(expected, pct_flags, style, layout_center, center_table)
    }

    # ---- Table lines ----
    lines = line_builder_ct(
        combined, all_col_names, col_widths,
        main_col, main_col_width, total_width,
        header_underline, center_table, hline, style,
        border_char, pct_flags
    )

    pad = if (center_table) {
        term_w = tryCatch(
            cli::console_width(),
            error = function(e) as.integer(getOption("width"))
        )
        strrep(" ", max(0L, floor((term_w - nchar(strip_ansi(lines[[1]]))) / 2L)))
    } else {
        ""
    }

    for (line in lines) cat(pad, line, "\n", sep = "")

    invisible(combined)
}
