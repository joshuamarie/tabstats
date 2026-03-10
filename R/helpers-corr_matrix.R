align_center = function(text, width, pos = FALSE) {
    clean_text = gsub("\033\\[[0-9;]*m", "", text)
    padding = if (pos) max(width - nchar(clean_text), 0) else width - nchar(clean_text)

    if (padding < 0) return(substr(text, 1, width))

    paste0(strrep(" ", floor(padding / 2)), text, strrep(" ", ceiling(padding / 2)))
}

format_row_cm = function(
    row,
    col_widths,
    left_align_first = FALSE,
    pos = FALSE
) {
    row = as.character(row)
    formatted = vapply(seq_along(row), function(i) {
        if (i == 1 && left_align_first)
            format(row[i], width = col_widths[i], justify = "left")
        else
            align_center(row[i], col_widths[i], pos = pos)
    }, character(1))
    paste0("  ", paste(formatted, collapse = "   "), "  ")
}
