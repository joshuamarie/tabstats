#' Display a Correlation Matrix Table in the Console
#'
#' @param display A `corr_spec` object from `new_corr_spec()`, or a plain
#'   symmetric matrix (e.g. from `cor()`).
#' @param method Label shown in the title (e.g. `"Pearson"`). Auto-detected
#'   from a `method` attribute on the spec if present.
#' @param diag_1 If `TRUE`, diagonal cells always show `"1"`. Default `TRUE`.
#' @param digits Decimal places for numeric formatting. Default `3`.
#' @param center_table Center table in terminal? Default `FALSE`.
#' @param border_char Border character. Default from `getOption("tab_default")`.
#' @param style Named list of style specs. Keys: `corr` and any extra field
#'   names passed to `new_corr_spec()` (e.g. `pval`, `bf`), plus `title` and
#'   `border_text`. Each value can be a cli-style string or a `function(x)`.
#' @param ... Reserved for future use.
#'
#' @return Invisibly returns the rendered character matrix.
#'
#' @examples
#' # From a plain matrix
#' corr_matrix(cor(mtcars[, 1:4]), method = "Pearson")
#'
#' # From a corr_spec
#' spec = new_corr_spec(
#'     var1 = c("a", "a", "b"),
#'     var2 = c("b", "c", "c"),
#'     rho  = c("0.89", "0.79", "0.66"),
#'     pval = c("<0.001", "<0.001", "<0.001")
#' )
#' corr_matrix(spec, method = "Pearson")
#'
#' @export
corr_matrix = function(
        display,
        method = NULL,
        diag_1 = TRUE,
        digits = 3,
        center_table = FALSE,
        border_char = getOption("tab_default")$border_char,
        style = list(),
        ...
) {
    if (!requireNamespace("cli", quietly = TRUE))
        stop("The 'cli' package is required. Please install it.")

    # ---- Coerce input --------------------------------------------------------
    spec = if (is.matrix(display)) {
        matrix_spec_resolver_cm(display, digits)
    } else if (inherits(display, "corr_spec")) {
        display
    } else {
        stop("`display` must be a `corr_spec` object or a symmetric matrix.", call. = FALSE)
    }

    method = if (is.null(method)) {
        m = attr(spec, "method")
        if (is.null(m)) "Unknown" else m
    } else {
        method
    }

    style = style_resolver_cm(style, names(spec$extras))

    # ---- Build render matrix -------------------------------------------------
    vars = spec$vars
    n = length(vars)
    field_names = names(spec$extras)
    if (length(field_names) == 0) field_names = "corr"
    n_fields = length(field_names)

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

        corr_val = spec$extras[[1L]][p]
        mat[row_base, ci + 1L] = corr_val

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
    title = paste(method, "Correlation Matrix")
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

matrix_spec_resolver_cm = function(m, digits) {
    if (!isSymmetric(unname(m)))
        stop("Matrix must be symmetric.", call. = FALSE)

    vars = if (!is.null(colnames(m))) colnames(m) else paste0("Var", seq_len(ncol(m)))
    pairs = which(lower.tri(m), arr.ind = TRUE)
    corr_vals = sprintf(paste0("%.", digits, "f"), m[pairs])

    spec = new_corr_spec(
        var1 = vars[pairs[, 1]],
        var2 = vars[pairs[, 2]],
        corr = corr_vals
    )
    attr(spec, "method") = "Unknown"
    spec
}

style_resolver_cm = function(style, extra_names) {
    identity_fn = function(x) x
    all_keys = unique(c(extra_names, "title", "border_text"))
    defaults = stats::setNames(rep(list(identity_fn), length(all_keys)), all_keys)
    resolved = utils::modifyList(defaults, if (is.null(style)) list() else style)

    for (nm in names(resolved))
        if (is.character(resolved[[nm]]))
            resolved[[nm]] = style_fn_cm(resolved[[nm]])

    resolved
}

style_fn_cm = function(style_string) {
    style_map = list(
        red = cli::col_red, blue = cli::col_blue, green = cli::col_green,
        yellow = cli::col_yellow, magenta = cli::col_magenta, cyan = cli::col_cyan,
        white = cli::col_white, black = cli::col_black,
        bold = cli::style_bold, italic = cli::style_italic,
        underline = cli::style_underline
    )
    parts = unlist(strsplit(style_string, "_"))
    fns = lapply(parts, function(p) {
        if (p %in% names(style_map)) style_map[[p]]
        else { warning("Unknown style: '", p, "'. Ignored."); function(x) x }
    })
    Reduce(function(f, g) function(x) f(g(x)), fns, right = TRUE)
}

matrix_styler_cm = function(mat, field_names, style) {
    n_fields = length(field_names)
    styled = mat

    for (i in seq_len(nrow(mat))) {
        field = field_names[(i - 1L) %% n_fields + 1L]
        fn = style[[field]]
        if (is.null(fn)) fn = function(x) x

        for (j in seq_len(ncol(mat))) {
            if (j == 1L || mat[i, j] == "") next
            styled[i, j] = fn(mat[i, j])
        }
    }
    styled
}

left_pad_cm = function(center_table, total_width) {
    if (!center_table) return("")
    term_w = tryCatch(cli::console_width(), error = function(e) as.integer(getOption("width")))
    strrep(" ", max(0L, floor((term_w - total_width) / 2L)))
}
