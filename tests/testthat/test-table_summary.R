df = data.frame(
    Category = c("A", "B", "C", "D", "E"),
    Value = c(10, 20, 30, 40, 50)
)

df_long = data.frame(
    Variable = c("N", "Mean", "SD", "Median", "Min", "Max", "Skew", "Kurt", "SE", "IQR"),
    Value = c("200", "4.12", "0.98", "4.00", "1.50", "6.50", "0.23", "2.87", "0.07", "1.25")
)

# ---- Input validation ----

test_that("rejects non-data-frame input", {
    expect_error(table_summary(matrix(1:4, 2, 2)), "data frame")
})

test_that("rejects data frame with more than 2 columns", {
    expect_error(table_summary(data.frame(a = 1, b = 2, c = 3)), "2 columns")
})

test_that("rejects data frame with fewer than 2 columns", {
    expect_error(table_summary(data.frame(a = 1)), "2 columns")
})

# ---- Basic output ----

test_that("prints without error with default args", {
    expect_no_error(table_summary(df))
})

test_that("returns NULL invisibly", {
    expect_invisible(table_summary(df))
    expect_null(table_summary(df))
})

test_that("output contains all left-column values", {
    out = capture.output(table_summary(df))
    for (v in c("A", "B", "C", "D", "E"))
        expect_true(any(grepl(v, out)))
})

test_that("output contains all right-column values", {
    out = capture.output(table_summary(df))
    for (v in c("10", "20", "30", "40", "50"))
        expect_true(any(grepl(v, out)))
})

test_that("output has exactly two border lines by default", {
    out = capture.output(table_summary(df))
    borders = sum(grepl("^-+$", trimws(out)))
    expect_equal(borders, 2L)
})

# ---- Title ----

test_that("title appears in output when provided", {
    out = capture.output(table_summary(df, title = "My Table"))
    expect_true(any(grepl("My Table", out)))
})

test_that("title is absent when title = NULL", {
    out = capture.output(table_summary(df, title = NULL))
    expect_false(any(grepl("My Table", out)))
})

test_that("title with style runs without error", {
    expect_no_error(
        table_summary(df, title = "Styled", style = sm_style(title = "bold"))
    )
})

# ---- Header ----

test_that("header = TRUE prints both column names", {
    out = capture.output(table_summary(df, header = TRUE))
    expect_true(any(grepl("Category", out)))
    expect_true(any(grepl("Value", out)))
})

test_that("header = TRUE adds a third border line", {
    out = capture.output(table_summary(df, header = TRUE))
    borders = sum(grepl("^-+$", trimws(out)))
    expect_equal(borders, 3L)
})

test_that("header = FALSE does not print column names", {
    out = capture.output(table_summary(df, header = FALSE))
    expect_false(any(grepl("Category", out)))
})

# ---- Split table (l) ----

test_that("split table prints without error", {
    expect_no_error(table_summary(df, l = 2))
})

test_that("split table contains all data values", {
    out = capture.output(table_summary(df, l = 2))
    for (v in c("A", "B", "C", "D", "E", "10", "50"))
        expect_true(any(grepl(v, out)))
})

test_that("l >= nrow does not split the table", {
    out_normal = capture.output(table_summary(df))
    out_nosplit = capture.output(table_summary(df, l = nrow(df)))
    expect_equal(out_normal, out_nosplit)
})

test_that("split table with header prints without error", {
    expect_no_error(table_summary(df_long, l = 5, header = TRUE))
})

test_that("split table with uneven halves prints without error", {
    expect_no_error(table_summary(df_long, l = 3))
})

# ---- Border char ----

test_that("custom border_char appears in output", {
    out = capture.output(table_summary(df, border_char = "="))
    expect_true(any(grepl("=", out)))
})

test_that("default border_char is not present when overridden", {
    out = capture.output(table_summary(df, border_char = "="))
    expect_false(any(grepl("^-+$", trimws(out))))
})

test_that("unicode border_char works", {
    expect_no_error(table_summary(df, border_char = "\u2500"))
})

# ---- Align ----

test_that("align as single string runs without error", {
    expect_no_error(table_summary(df, align = "center"))
    expect_no_error(table_summary(df, align = "left"))
    expect_no_error(table_summary(df, align = "right"))
})

test_that("align as length-2 vector runs without error", {
    expect_no_error(table_summary(df, align = c("center", "right")))
    expect_no_error(table_summary(df, align = c("left", "center")))
})

test_that("align as named list runs without error", {
    expect_no_error(
        table_summary(df, align = list(left_col = "left", right_col = "right"))
    )
})

test_that("align as partial named list (left_col only) runs without error", {
    expect_no_error(table_summary(df, align = list(left_col = "center")))
})

# ---- sm_style() (named strings) ----

test_that("sm_style() with named strings runs without error", {
    expect_no_error(
        table_summary(
            df,
            header = TRUE,
            style = sm_style(
                left_col = "blue_bold",
                right_col = "red",
                title = "green",
                border_text = "yellow"
            )
        )
    )
})

test_that("sm_style() rejects non-string, non-function values", {
    expect_error(sm_style(left_col = 123), "must be a string or a function")
})

test_that("sm_style() returns correct class", {
    s = sm_style(left_col = "blue")
    expect_s3_class(s, "sm_style")
    expect_s3_class(s, "tabstats_style")
})

test_that("sm_style() with custom sep runs without error", {
    expect_no_error(table_summary(df, style = sm_style(sep = "|")))
})

test_that("custom sep appears in output", {
    out = capture.output(table_summary(df, style = sm_style(sep = "|")))
    expect_true(any(grepl("|", out, fixed = TRUE)))
})

# ---- sm_style() (anonymous functions) ----

test_that("sm_style() with lambda functions runs without error", {
    expect_no_error(
        table_summary(
            df,
            header = TRUE,
            style = sm_style(
                left_col = \(x) cli::col_red(x),
                right_col = \(x) cli::col_blue(x)
            )
        )
    )
})

test_that("sm_style() stores functions correctly", {
    fn = \(x) cli::col_red(x)
    s = sm_style(left_col = fn)
    expect_identical(s$left_col, fn)
})

# ---- print.tabstats_style ----

test_that("print.tabstats_style runs without error", {
    expect_no_error(print(sm_style(left_col = "blue", right_col = "red")))
})

test_that("print.tabstats_style handles empty style object", {
    expect_no_error(print(sm_style()))
})

# ---- Combinations ----

test_that("title + header + split + sm_style + align all together", {
    expect_no_error(
        table_summary(
            df_long,
            title = "Full Test",
            l = 5,
            header = TRUE,
            style = sm_style(left_col = "bold", border_text = "blue"),
            align = c("left", "right")
        )
    )
})

test_that("center_table = TRUE runs without error", {
    expect_no_error(table_summary(df, center_table = TRUE))
})
