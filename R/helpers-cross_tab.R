center_text_x2 = function(text, width) {
    text_width = nchar(strip_ansi(text))
    padding = max(0, width - text_width)
    left_padding = floor(padding / 2)
    right_padding = ceiling(padding / 2)
    paste0(strrep(" ", left_padding), text, strrep(" ", right_padding))
}

format_number_x2 = function(x) {
    dplyr::if_else(
        abs(x - round(x)) < 1e-10,
        sprintf("%d", round(x)),
        sprintf("%.1f", x)
    )
}

strip_ansi = function(text) {
    gsub("\033\\[[0-9;]*m", "", text)
}
