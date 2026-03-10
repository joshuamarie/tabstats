align_test = function(text, width) {
    padding = width - nchar(text)
    left_pad = floor(padding / 2)
    right_pad = ceiling(padding / 2)
    paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
}

format_row_summary =
    function(
        left,
        right,
        left_width,
        right_width,
        align = NULL,
        style = NULL
    ) {

    left_align = "left"
    right_align = "right"

    if (!is.null(align)) {
        if (is.character(align) && length(align) == 1) {
            left_align = align
        } else if (is.character(align) && length(align) == 2) {
            left_align = align[1]
            right_align = align[2]
        } else if (is.list(align)) {
            if (!is.null(align$left_col)) left_align = align$left_col
            if (!is.null(align$right_col)) right_align = align$right_col
        }
    }

    if (left_align == "left") {
        left_formatted = sprintf("%-*s", left_width, left)
    } else if (left_align == "right") {
        left_formatted = sprintf("%*s", left_width, left)
    } else if (left_align == "center") {
        left_formatted = align_test(left, left_width)
    }

    if (right_align == "left") {
        right_formatted = sprintf("%-*s", right_width, right)
    } else if (right_align == "right") {
        right_formatted = sprintf("%*s", right_width, right)
    } else if (right_align == "center") {
        right_formatted = align_test(right, right_width)
    }

    if (!is.null(style)) {
        if (!is.null(style$left_col)) {
            if (is.function(style$left_col)) {
                left_formatted = style$left_col(list(value = left_formatted))
            } else if (is.character(style$left_col)) {
                if (style$left_col == "bold") {
                    left_formatted = cli::style_bold(left_formatted)
                } else if (style$left_col == "italic") {
                    left_formatted = cli::style_italic(left_formatted)
                } else if (style$left_col == "blue") {
                    left_formatted = cli::col_blue(left_formatted)
                } else if (style$left_col == "red") {
                    left_formatted = cli::col_red(left_formatted)
                } else if (style$left_col == "green") {
                    left_formatted = cli::col_green(left_formatted)
                } else if (style$left_col == "yellow") {
                    left_formatted = cli::col_yellow(left_formatted)
                } else if (style$left_col == "blue_bold") {
                    left_formatted = cli::col_blue(cli::style_bold(left_formatted))
                } else if (style$left_col == "red_italic") {
                    left_formatted = cli::col_red(cli::style_italic(left_formatted))
                }
            }
        }

        if (!is.null(style$right_col)) {
            if (is.function(style$right_col)) {
                right_formatted = style$right_col(list(value = right_formatted))
            } else if (is.character(style$right_col)) {
                if (style$right_col == "bold") {
                    right_formatted = cli::style_bold(right_formatted)
                } else if (style$right_col == "italic") {
                    right_formatted = cli::style_italic(right_formatted)
                } else if (style$right_col == "blue") {
                    right_formatted = cli::col_blue(right_formatted)
                } else if (style$right_col == "red") {
                    right_formatted = cli::col_red(right_formatted)
                } else if (style$right_col == "green") {
                    right_formatted = cli::col_green(right_formatted)
                } else if (style$right_col == "yellow") {
                    right_formatted = cli::col_yellow(right_formatted)
                } else if (style$right_col == "blue_bold") {
                    right_formatted = cli::col_blue(cli::style_bold(right_formatted))
                } else if (style$right_col == "red_italic") {
                    right_formatted = cli::col_red(cli::style_italic(right_formatted))
                }
            }
        }
    }

    sep_value = "    "
    if (!is.null(style) && !is.null(style$sep)) {
        sep_value = paste0(" ", style$sep, " ")
    }

    paste0("  ", left_formatted, sep_value, right_formatted)
}
