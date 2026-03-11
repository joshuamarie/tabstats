center_text = function(text, width, pos = FALSE) {
    if (pos) {
        padding = pmax(width - nchar(text), 0)
    } else {
        padding = width - nchar(text)
    }
    left_pad = floor(padding / 2)
    right_pad = ceiling(padding / 2)
    paste0(paste0(rep(" ", left_pad), collapse = ""), text, paste0(rep(" ", right_pad), collapse = ""))
}

justify_text = function(text, width, justify = "center") {
    justify = rep_len(justify, length(text))

    width = pmax(width, 0)

    side = vctrs::vec_if_else(
        justify == "left", "right",
        vctrs::vec_if_else(justify == "right", "left", "both")
    )

    vctrs::vec_c(
        !!!mapply(
            function(txt, w, s) {
                pad = w - nchar(txt)
                switch(
                    s,
                    right = paste0(txt, strrep(" ", pad)),
                    left = paste0(strrep(" ", pad), txt),
                    both = paste0(strrep(" ", floor(pad / 2)), txt, strrep(" ", ceiling(pad / 2)))
                )
            },
            text,
            width,
            side,
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
    )
}

format_row =
    function(
        row,
        col_widths,
        justify_cols = NULL,
        n_space = 2,
        styles = NULL,
        col_data = NULL, is_header = FALSE, vb = list()
    ) {

    original_names = names(row)
    if (is.null(original_names) && length(row) > 0) {
        original_names = as.character(seq_along(row))
    } else if (length(original_names) != length(row)) {
        original_names = as.character(seq_along(row))
    }

    styled_row = character(length(row))
    raw_content = character(length(row))

    for (i in seq_along(row)) {
        col_name_or_index = original_names[i]
        value = row[i]
        raw_content[i] = value

        if (!is.null(styles)) {
            style_fn_or_name = NULL
            lookup_key = if (is_header) value else col_name_or_index

            if (is.list(styles)) {
                if (!is.null(names(styles))) {
                    if (!is.null(lookup_key) && lookup_key %in% names(styles)) {
                        style_fn_or_name = styles[[lookup_key]]
                    } else if (as.character(i) %in% names(styles)) {
                        style_fn_or_name = styles[[as.character(i)]]
                    }
                }
                if (is.null(style_fn_or_name) && is.null(names(styles)) && i <= length(styles)) {
                    style_fn_or_name = styles[[i]]
                }
            } else if (length(styles) >= i) {
                style_fn_or_name = styles[[i]]
            }

            if (!is.null(style_fn_or_name)) {
                if (is.function(style_fn_or_name)) {
                    context = list(
                        value = value,
                        formatted_value = value,
                        col_name = col_name_or_index,
                        col_index = i,
                        is_header = is_header,
                        data = col_data,
                        justify = "center",
                        width = col_widths[i]
                    )

                    result = tryCatch({
                        styled_result = style_fn_or_name(context)
                        if (is.character(styled_result) && length(styled_result) == 1) {
                            styled_row[i] = styled_result
                        } else {
                            styled_row[i] = value
                        }
                    }, error = function(e) {
                        warning("Styling function failed for column '", col_name_or_index, "': ", e$message, call. = FALSE)
                        value
                    })
                } else if (is.character(style_fn_or_name)) {
                    style_parts = unlist(strsplit(style_fn_or_name, "_"))
                    styled_text = value

                    for (style_part in style_parts) {
                        styled_text = tryCatch({
                            if (exists(paste0("col_", style_part), where = asNamespace("cli"))) {
                                style_fn = get(paste0("col_", style_part), envir = asNamespace("cli"))
                                style_fn(styled_text)
                            } else if (exists(paste0("style_", style_part), where = asNamespace("cli"))) {
                                style_fn = get(paste0("style_", style_part), envir = asNamespace("cli"))
                                style_fn(styled_text)
                            } else if (exists(style_part, where = asNamespace("cli"))) {
                                style_fn = get(style_part, envir = asNamespace("cli"))
                                style_fn(styled_text)
                            } else {
                                warning("Unknown cli style '", style_part, "' in '", style_fn_or_name, "'.", call. = FALSE)
                                styled_text
                            }
                        }, error = function(e) {
                            warning("Applying cli style '", style_part, "' failed: ", e$message, call. = FALSE)
                            value
                        })
                    }
                    styled_row[i] = styled_text
                }
            } else {
                styled_row[i] = value
            }
        } else {
            styled_row[i] = value
        }
    }

    justify_values = character(length(row))
    for (i in seq_along(row)) {
        col_name_or_index = original_names[i]
        justify_value = "center"

        if (!is.null(justify_cols)) {
            temp_justify = NULL
            if (is.list(justify_cols)) {
                if (!is.null(names(justify_cols))) {
                    if (col_name_or_index %in% names(justify_cols)) {
                        temp_justify = justify_cols[[col_name_or_index]]
                    } else if (as.character(i) %in% names(justify_cols)) {
                        temp_justify = justify_cols[[as.character(i)]]
                    }
                }
                if (is.null(temp_justify) && is.null(names(justify_cols)) && i <= length(justify_cols)) {
                    temp_justify = justify_cols[[i]]
                }
            } else if (is.character(justify_cols)) {
                temp_justify = justify_cols[min(i, length(justify_cols))]
            }
            if (!is.null(temp_justify) && temp_justify %in% c("left", "right", "center")) {
                justify_value = temp_justify
            }
        }
        justify_values[i] = justify_value
    }

    displayed_widths = numeric(length(row))
    for (i in seq_along(row)) {
        plain_text = gsub("\033\\[[0-9;]*m", "", styled_row[i])
        displayed_widths[i] = max(nchar(plain_text), nchar(raw_content[i]))
    }

    formatted = character(length(row))
    for (i in seq_along(row)) {
        value = raw_content[i]
        target_width = max(col_widths[i], displayed_widths[i])
        plain_text = gsub("\033\\[[0-9;]*m", "", styled_row[i])
        justified_text = justify_text(plain_text, target_width, justify_values[i])
        if (!grepl("\033\\[", styled_row[i])) {
            formatted[i] = justified_text
            next
        }
        ansi_codes = gregexpr("\033\\[[0-9;]*m", styled_row[i])
        if (length(ansi_codes[[1]]) > 0 && ansi_codes[[1]][1] != -1) {
            code_positions = ansi_codes[[1]]
            ansi_codes_text = regmatches(styled_row[i], ansi_codes)[[1]]

            total_codes_len = sum(nchar(ansi_codes_text))
            text_len = nchar(styled_row[i]) - total_codes_len

            padding = target_width - text_len

            if (justify_values[i] == "left") {
                padding_right = padding
                padding_left = 0
            } else if (justify_values[i] == "right") {
                padding_right = 0
                padding_left = padding
            } else {
                padding_left = floor(padding / 2)
                padding_right = padding - padding_left
            }

            left_pad = paste0(rep(" ", padding_left), collapse = "")
            right_pad = paste0(rep(" ", padding_right), collapse = "")

            first_code_pos = code_positions[1]
            if (first_code_pos == 1) {
                result = paste0(
                    ansi_codes_text[1],
                    left_pad,
                    substr(
                        styled_row[i], nchar(ansi_codes_text[1]) + 1, nchar(styled_row[i])
                    ),
                    right_pad
                )
            } else {
                result = paste0(left_pad, styled_row[i], right_pad)
            }

            formatted[i] = result
        } else {
            formatted[i] = justified_text
        }
    }

    # ---- Vertical Borders ----
    border_char = vb$char %||% getOption("tab_default")$vb_char
    after_cols_spec = vb$after

    if (length(after_cols_spec) > 0) {
        after_cols_idx = integer(0)
        if (is.numeric(after_cols_spec)) {
            after_cols_idx = after_cols_spec
        } else if (is.character(after_cols_spec)) {
            after_cols_idx = match(after_cols_spec, original_names)
        }

        valid_indices = !is.na(after_cols_idx) & after_cols_idx > 0 & after_cols_idx < length(formatted)
        after_cols_idx = sort(unique(after_cols_idx[valid_indices]))

        if (length(after_cols_idx) > 0) {
            for (i in after_cols_idx) {
                if (grepl("\033\\[", formatted[i])) {
                    last_code_pos = max(gregexpr("\033\\[[0-9;]*m", formatted[i])[[1]])
                    if (last_code_pos != -1) {
                        last_code = regmatches(formatted[i], regexpr("\033\\[[0-9;]*m$", formatted[i]))
                        if (length(last_code) > 0 && last_code == "\033[0m") {
                            formatted[i] = sub("\033\\[0m$", paste0(" ", border_char, "\033[0m"), formatted[i])
                        } else {
                            formatted[i] = paste0(formatted[i], " ", border_char)
                        }
                    } else {
                        formatted[i] = paste0(formatted[i], " ", border_char)
                    }
                } else {
                    formatted[i] = paste0(formatted[i], " ", border_char)
                }
            }
        }
    }

    # ---- Combine Cells with Separators ----
    separator = paste0(rep(" ", n_space), collapse = "")
    paste0("  ", paste(formatted, collapse = separator), "  ")
}
