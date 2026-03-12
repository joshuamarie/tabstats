matrix_spec_resolver_cm = function(m, digits) {
    if (!isSymmetric(unname(m)))
        stop("Matrix must be symmetric.", call. = FALSE)

    new_m = as.table(m) |>
        as.data.frame()

    spec = new_corr_data(
        var1 = vctrs::vec_cast(new_m[[1]], character()),
        var2 = vctrs::vec_cast(new_m[[2]], character()),
        corr = format(new_m[[3]], digits = digits)
    )
    attr(spec, "title") = "Correlation Matrix"
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

print_layout_cm = function(field_names, style, border_char, layout_center, center_table) {
    w = 29L
    top = paste0("\u250C", strrep("\u2500", w - 2L), "\u2510")
    mid = paste0("\u251C", strrep("\u2500", w - 2L), "\u2524")
    bot = paste0("\u2514", strrep(border_char, w - 2L), "\u2518")
    inner = function(txt) paste0("| ", align_center(txt, w - 4L), " |")

    box = c(
        top,
        inner(style[["title"]]("Layout for Corr. Matrix")),
        mid
    )
    for (nm in field_names) {
        label = paste0("< ", nm, " >")
        styled_label = tryCatch(
            style[[nm]](label),
            warning = function(w) NULL,
            error = function(e) NULL
        )
        if (is.null(styled_label)) {
            probe = tryCatch(style[[nm]]("0.001"), error = function(e) NULL)
            styled_label = if (!is.null(probe)) {
                pre  = regmatches(probe, regexpr("^(\033\\[[0-9;]*m)+", probe))
                post = regmatches(probe, regexpr("(\033\\[[0-9;]*m)+$", probe))
                if (length(pre) && length(post)) paste0(pre, label, post) else label
            } else {
                label
            }
        }
        box = c(box, inner(styled_label))
    }

    box = c(box, bot)

    term_w = tryCatch(cli::console_width(), error = function(e) as.integer(getOption("width")))
    pad = if (layout_center || center_table)
        strrep(" ", max(0L, floor((term_w - w) / 2L)))
    else
        ""

    cat("\n")
    for (line in box) cat(pad, line, "\n", sep = "")
    cat("\n")
}
