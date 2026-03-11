#' Summarize and Display a Two-Column Data Frame as a Formatted Table
#'
#' This function takes a two-column data frame and formats it into a summary-like table.
#' The table can be optionally split into two parts, centered, and given a title.
#' It is useful for displaying summary information in a clean, tabular format.
#' The function also supports styling with ANSI colors and text formatting through
#' the `{cli}` package and column alignment options.
#'
#' @param data A data frame with exactly two columns. The data to be summarized and displayed.
#' @param title A character string. An optional title to be displayed above the table.
#' @param l An integer. The number of rows to include in the left part of a split table.
#'           If `NULL`, the table is not split.
#' @param header A logical value. If `TRUE`, the column names of `data` are displayed as a header.
#' @param center_table A logical value. If `TRUE`, the table is centered in the terminal.
#' @param border_char Character used for borders. Default is `"\u2500"`.
#' @param style A list controlling the visual styling of table elements using ANSI formatting.
#'   Can include the following components:
#'   - `left_col`: Styling for the left column values.
#'   - `right_col`: Styling for the right column values.
#'   - `border_text`: Styling for the border.
#'   - `title`: Styling for the title.
#'   - `sep`: Separator character between left and right column.
#'
#'   Each style component can be either a predefined style string (e.g., "blue", "red_italic", "bold")
#'   or a function that takes a context list with/without a `value` element and returns the styled text.
#' @param align Controls the alignment of column values.
#'   Can be specified in three ways:
#'   - A single string: affects only the left column (e.g., "left", "center", "right").
#'   - A vector of two strings: affects both columns in order (e.g., c("left", "right")).
#'   - A list with named components: explicitly specifies alignment for each column
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return This function does not return a value. It prints the formatted table to the console.
#'
#' @examples
#' # Create a sample data frame
#' df = data.frame(
#'     Category = c("A", "B", "C", "D", "E"),
#'     Value = c(10, 20, 30, 40, 50)
#' )
#'
#' # Display the table with a title and header
#' table_summary(df, title = "Sample Table", header = TRUE)
#'
#' # Split the table after the second row and center it
#' table_summary(df, l = 2, center_table = TRUE)
#'
#' # Use styling and alignment
#' table_summary(
#'     df,
#'     header = TRUE,
#'     style = list(
#'         left_col = "blue_bold",
#'         right_col = "red",
#'         title = "green",
#'         border_text = "yellow"
#'     ),
#'     align = c("center", "right")
#' )
#'
#' # Use custom styling with lambda functions
#' table_summary(
#'     df,
#'     header = TRUE,
#'     style = sm_style(
#'         left_col = \(ctx) cli::col_red(ctx), # ctx$value is another option
#'         right_col = \(ctx) cli::col_blue(ctx)
#'     ),
#'     align = list(left_col = "left", right_col = "right")
#' )
#'
#' @export
table_summary =
    function(
        data,
        title = NULL,
        l = NULL,
        header = FALSE,
        center_table = FALSE,
        border_char = "-",
        style = list(), align = NULL, ...
    ) {

        if (!is.data.frame(data) || ncol(data) != 2)
            cli::cli_abort("Input must be a data frame with exactly 2 columns.")

        if (!is.null(style) && !is.list(style))
            cli::cli_abort("{.arg style} must be a list or a style object (e.g. {.fn sm_style}).")

        if (inherits(style, "tabstats_style") && !inherits(style, "sm_style")) {
            cli::cli_abort(
                "{.arg style} must be an {.cls sm_style} object for {.fn table_summary}.",
                "x" = "Got {.cls {class(style)[1]}}."
            )
        }

        col_names = colnames(data)
        data_matrix = as.matrix(data)
        sep_width = if (!is.null(style$sep)) nchar(paste0(" ", style$sep, " ")) else 4

        w = compute_widths(data_matrix, col_names, l, sep_width)

        # ---- Border line ----
        h_line = strrep(border_char, w$full_width)
        styled_h_line =
            if (!is.null(style$border_text)) {
                apply_style(style$border_text, h_line)
            } else {
                h_line
            }

        # ---- Optional centering prefix ----
        prefix = ""
        if (center_table) {
            term_width = tryCatch(
                as.numeric(system("tput cols", intern = TRUE)),
                error = function(e)
                    as.double(options("width"))
            )
            prefix = strrep(" ", max(0L, floor((term_width - w$full_width) / 2)))
        }

        # ---- Title ----
        if (!is.null(title)) {
            fmt_title = center_text(title, w$full_width)
            if (!is.null(style$title))
                fmt_title = apply_style(style$title, fmt_title)

            cat("\n", prefix, fmt_title, "\n", sep = "")
        }

        cat(prefix, styled_h_line, "\n", sep = "")

        # ---- Header row ----
        if (header) {
            header_row = if (w$is_split) {
                paste0(
                    format_row_summary(col_names[1], col_names[2],
                                       w$left_left_width, w$left_right_width,
                                       style = NULL),
                    "  ",
                    format_row_summary(col_names[1], col_names[2],
                                       w$right_left_width, w$right_right_width,
                                       style = NULL)
                )
            } else {
                format_row_summary(col_names[1], col_names[2],
                                   w$left_width, w$right_width,
                                   style = NULL)
            }
            cat(prefix, header_row, "\n", sep = "")
            cat(prefix, styled_h_line, "\n", sep = "")
        }

        # ---- Data rows ----
        if (!w$is_split) {
            for (i in seq_len(nrow(data_matrix))) {
                cat(
                    prefix,
                    format_row_summary(data_matrix[i, 1], data_matrix[i, 2],
                                       w$left_width, w$right_width,
                                       align = align, style = style),
                    "\n",
                    sep = ""
                )
            }
        } else {
            n_rows_out = max(nrow(w$left_table), nrow(w$right_table))
            for (i in seq_len(n_rows_out)) {
                lr = if (i <= nrow(w$left_table))  w$left_table[i, ]  else c("", "")
                rr = if (i <= nrow(w$right_table)) w$right_table[i, ] else c("", "")

                cat(
                    prefix,
                    paste0(
                        format_row_summary(lr[1], lr[2],
                                           w$left_left_width, w$left_right_width,
                                           align = align, style = style),
                        "  ",
                        format_row_summary(rr[1], rr[2],
                                           w$right_left_width, w$right_right_width,
                                           align = align, style = style)
                    ),
                    "\n",
                    sep = ""
                )
            }
        }

        cat(prefix, styled_h_line, "\n", sep = "")
        invisible(NULL)
    }
