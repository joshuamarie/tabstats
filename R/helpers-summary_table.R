align_text = function(text, width, alignment = "left") {
    switch(
        alignment,
        left = sprintf("%-*s", width, text),
        right = sprintf("%*s",  width, text),
        center = {
            padding   = width - nchar(text)
            left_pad  = floor(padding / 2)
            right_pad = ceiling(padding / 2)
            paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
        },
        sprintf("%-*s", width, text)
    )
}

apply_style = function(style, text) {
    out = if (is.function(style)) {
        style(text)
    } else {
        switch(
            style,
            bold = cli::style_bold(text),
            italic = cli::style_italic(text),
            blue = cli::col_blue(text),
            red = cli::col_red(text),
            green = cli::col_green(text),
            yellow = cli::col_yellow(text),
            blue_bold = cli::col_blue(cli::style_bold(text)),
            red_italic = cli::col_red(cli::style_italic(text)),
            text
        )
    }

    out
}

resolve_alignment = function(align) {
    left_align = "left"
    right_align = "right"

    if (!is.null(align)) {
        if (is.character(align) && length(align) == 1) {
            left_align = right_align = align
        } else if (is.character(align) && length(align) == 2) {
            left_align = align[1]
            right_align = align[2]
        } else if (is.list(align)) {
            if (!is.null(align$left_col))  left_align = align$left_col
            if (!is.null(align$right_col)) right_align = align$right_col
        }
    }

    list(left = left_align, right = right_align)
}

compute_widths = function(data_matrix, col_names, l = NULL, sep_width = 4) {
    n_rows = nrow(data_matrix)
    is_split = !is.null(l) && l < n_rows

    if (!is_split) {
        lw = max(nchar(data_matrix[, 1]), nchar(col_names[1]))
        rw = max(nchar(data_matrix[, 2]), nchar(col_names[2]))
        return(list(
            is_split   = FALSE,
            full_width = lw + rw + 4 + sep_width,
            left_width = lw, right_width = rw
        ))
    }

    lt = data_matrix[1:l, ]
    rt = data_matrix[(l + 1):n_rows, ]

    ll = max(nchar(lt[, 1]), nchar(col_names[1]))
    lr = max(nchar(lt[, 2]), nchar(col_names[2]))
    rl = max(nchar(rt[, 1]), nchar(col_names[1]))
    rr = max(nchar(rt[, 2]), nchar(col_names[2]))

    list(
        is_split = TRUE,
        full_width = (ll + lr + 4 + sep_width) + (rl + rr + 4 + sep_width),
        left_table = lt,
        right_table = rt,
        left_left_width = ll,
        left_right_width = lr,
        right_left_width = rl,
        right_right_width = rr
    )
}

format_row_summary = function(left, right, left_width, right_width, align = NULL, style = NULL) {
    al = resolve_alignment(align)

    left_fmt = align_text(left, left_width, al$left)
    right_fmt = align_text(right, right_width, al$right)

    if (!is.null(style$left_col)) left_fmt = apply_style(style$left_col, left_fmt)
    if (!is.null(style$right_col)) right_fmt = apply_style(style$right_col, right_fmt)

    sep = if (!is.null(style$sep)) paste0(" ", style$sep, " ") else "    "
    paste0("  ", left_fmt, sep, right_fmt)
}
