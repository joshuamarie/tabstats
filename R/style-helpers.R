#' Style specification for table_default()
#'
#' Constructs a validated style object for use with [table_default()].
#'
#' @param ... Named style entries. Each name must be a column name, a column
#'   index as a string (e.g. `"1"`), or `"title"`. Each value is a string or
#'   a function `\(ctx) ...` where `ctx` is a context list.
#'
#' @return An object of class `c("td_style", "tabstats_style")`.
#'
#' @examples
#' td_style(mpg = "cyan", cyl = "magenta")
#' td_style(mpg = \(ctx) cli::col_red(ctx$value), title = "bold")
#'
#' @export
td_style = function(...) {
    args = list(...)
    for (nm in names(args)) {
        val = args[[nm]]
        if (!is.character(val) && !is.function(val)) {
            cli::cli_abort(
                "Style entry {.arg {nm}} must be a string or a function."
            )
        }
    }

    vctrs::new_vctr(args, class = c("td_style", "tabstats_style"))
}

#' Style specification for table_summary()
#'
#' Constructs a validated style object for use with [table_summary()].
#'
#' @param left_col Style for the left column. A string (e.g. `"blue_bold"`)
#'   or a function `\(x) ...`.
#' @param right_col Style for the right column.
#' @param border_text Style for the horizontal border lines.
#' @param title Style for the title text.
#' @param sep A single character used as the column separator (e.g. `"|"`).
#'
#' @return An object of class `c("sm_style", "tabstats_style")`.
#'
#' @examples
#' sm_style(left_col = "blue_bold", right_col = "green", title = "bold")
#' sm_style(left_col = \(x) cli::col_red(x), sep = "|")
#'
#' @export
sm_style = function(left_col = NULL, right_col = NULL, border_text = NULL, title = NULL, sep = NULL) {
    args = Filter(
        Negate(is.null),
        list(
            left_col = left_col,
            right_col = right_col,
            border_text = border_text,
            title = title,
            sep = sep
        )
    )

    new_tabstats_style(
        args,
        valid_keys = c("left_col", "right_col", "border_text", "title", "sep"),
        class_name = "sm_style"
    )
}

#' Style specification for cross_table()
#'
#' Constructs a validated style object for use with [cross_table()].
#'
#' @param observed Style for observed frequency cells.
#' @param expected Style for expected frequency values.
#' @param row_percentage Style for row percentage values.
#' @param col_percentage Style for column percentage values.
#' @param total_percentage Style for grand total percentage values.
#' @param total Style for total row/column cells.
#' @param title Style for the table title.
#' @param border_text Style for the horizontal border lines.
#'
#' @return An object of class `c("ct_style", "tabstats_style")`.
#'
#' @examples
#' ct_style(observed = "blue", expected = "yellow", title = "bold")
#' ct_style(observed = \(ctx) cli::col_green(ctx$formatted_text))
#'
#' @export
ct_style =
    function(
        observed = NULL,
        expected = NULL,
        row_percentage = NULL,
        col_percentage = NULL,
        total_percentage = NULL,
        total = NULL,
        title = NULL,
        border_text = NULL
    ) {
        args = Filter(Negate(is.null), list(
            observed = observed,
            expected = expected,
            row_percentage = row_percentage,
            col_percentage = col_percentage,
            total_percentage = total_percentage,
            total = total,
            title = title,
            border_text = border_text
        ))

        new_tabstats_style(
            args,
            valid_keys = c(
                "observed",
                "expected",
                "row_percentage",
                "col_percentage",
                "total_percentage",
                "total",
                "title",
                "border_text"
            ),
            class_name = "ct_style"
        )
    }

#' Style specification for corr_matrix()
#'
#' Constructs a validated style object for use with [corr_matrix()].
#'
#' @param ... Named style entries. Names should match the extra field names
#'   passed to [new_corr_spec()] (e.g. `rho`, `pval`, `bf`), or the reserved
#'   keys `title` and `border_text`.
#'
#' @return An object of class `c("cm_style", "tabstats_style")`.
#'
#' @examples
#' cm_style(rho = "blue_bold", pval = "red", title = "bold")
#' cm_style(rho = \(x) cli::col_cyan(x))
#'
#' @export
cm_style = function(...) {
    args = list(...)
    for (nm in names(args)) {
        val = args[[nm]]
        if (!is.character(val) && !is.function(val)) {
            cli::cli_abort(
                "Style entry {.arg {nm}} must be a string or a function."
            )
        }
    }

    vctrs::new_vctr(args, class = c("cm_style", "tabstats_style"))
}

new_tabstats_style = function(style_list, valid_keys, class_name) {
    unknown = setdiff(names(style_list), valid_keys)
    if (length(unknown) > 0) {
        cli::cli_abort(c(
            "Unknown style {cli::qty(unknown)} key{?s}: {.arg {unknown}}.",
            "i" = "Valid keys are: {.arg {valid_keys}}."
        ))
    }

    for (nm in names(style_list)) {
        val = style_list[[nm]]
        if (!is.character(val) && !is.function(val)) {
            cli::cli_abort(
                "Style key {.arg {nm}} must be a string or a function."
            )
        }
    }

    vctrs::new_vctr(
        style_list,
        valid_keys = valid_keys,
        class = c(class_name, "tabstats_style")
    )
}

#' @export
format.tabstats_style = function(x, ...) {
    cls = setdiff(class(x), "tabstats_style")[1]
    header = sprintf("<tabstats_style / %s>", cls)
    if (length(x) == 0)
        return(c(header, "  (no keys set)"))

    entries = vapply(
        names(x),
        function(nm) {
            val = x[[nm]]
            desc = if (is.function(val)) "<function>" else paste0('"', val, '"')
            sprintf("  %s: %s", nm, desc)
        },
        character(1)
    )

    c(header, entries)
}

#' @export
print.tabstats_style = function(x, ...) {
    cat(format(x, ...), sep = "\n")
    invisible(x)
}
