df = data.frame(
    Category = c("A", "B", "C", "D", "E"),
    Value = c(10, 20, 30, 40, 50)
)

# ---- Input validation ----

test_that("rejects non-data-frame input", {
    expect_error(table_summary(matrix(1:4, 2, 2)), "data frame")
})

test_that("rejects data frame with != 2 columns", {
    expect_error(table_summary(data.frame(a = 1, b = 2, c = 3)), "2 columns")
    expect_error(table_summary(data.frame(a = 1)), "2 columns")
})

# ---- Basic output ----

test_that("prints without error (default args)", {
    expect_no_error(table_summary(df))
})

test_that("returns NULL invisibly", {
    expect_null(table_summary(df))
})

test_that("output contains data values", {
    out = capture.output(table_summary(df))
    expect_true(any(grepl("A", out)))
    expect_true(any(grepl("50", out)))
})

test_that("output has border lines", {
    out = capture.output(table_summary(df))
    expect_true(any(grepl("^-+$", trimws(out))))
})

# ---- Title ----

test_that("title appears in output when provided", {
    out = capture.output(table_summary(df, title = "My Table"))
    expect_true(any(grepl("My Table", out)))
})

test_that("no title line when title = NULL", {
    out = capture.output(table_summary(df, title = NULL))
    expect_false(any(grepl("My Table", out)))
})

# ---- Header ----

test_that("header = TRUE prints column names", {
    out = capture.output(table_summary(df, header = TRUE))
    expect_true(any(grepl("Category", out)))
    expect_true(any(grepl("Value", out)))
})

test_that("header = FALSE does not print column names", {
    out = capture.output(table_summary(df, header = FALSE))
    expect_false(any(grepl("Category", out)))
})

# ---- Split table (l) ----

test_that("split table prints without error", {
    expect_no_error(table_summary(df, l = 2))
})

test_that("split table still contains all data values", {
    out = capture.output(table_summary(df, l = 2))
    for (v in c("A", "B", "C", "D", "E", "10", "50"))
        expect_true(any(grepl(v, out)))
})

test_that("l >= nrow does not split (treated as normal table)", {
    out_normal = capture.output(table_summary(df))
    out_nosplit = capture.output(table_summary(df, l = nrow(df)))
    expect_equal(out_normal, out_nosplit)
})

# ---- Border char ----

test_that("custom border_char appears in output", {
    out = capture.output(table_summary(df, border_char = "="))
    expect_true(any(grepl("=", out)))
    expect_false(any(grepl("^-+$", trimws(out))))
})

# ---- Align ----

test_that("align as single string runs without error", {
    expect_no_error(table_summary(df, align = "center"))
})

test_that("align as length-2 vector runs without error", {
    expect_no_error(table_summary(df, align = c("center", "right")))
})

test_that("align as named list runs without error", {
    expect_no_error(table_summary(df, align = list(left_col = "left", right_col = "right")))
})

# ---- Style (named strings) ----

test_that("named string styles run without error", {
    expect_no_error(
        table_summary(
            df,
            header = TRUE,
            style = list(
                left_col = "blue_bold",
                right_col = "red",
                title = "green",
                border_text = "yellow"
            )
        )
    )
})

test_that("custom sep in style runs without error", {
    expect_no_error(table_summary(df, style = list(sep = "|")))
})

test_that("custom sep appears in output", {
    out = capture.output(table_summary(df, style = list(sep = "|")))
    expect_true(any(grepl("|", out, fixed = TRUE)))
})

# ---- Style (anonymous functions) ----

test_that("lambda style functions run without error", {
    expect_no_error(
        table_summary(
            df,
            header = TRUE,
            style = list(
                left_col = \(x) cli::col_red(x),
                right_col = \(x) cli::col_blue(x)
            )
        )
    )
})

# ---- Combinations ----

test_that("title + header + split + style all together", {
    expect_no_error(
        table_summary(
            df,
            title = "Full Test",
            l = 3,
            header = TRUE,
            style = list(left_col = "bold", border_text = "blue"),
            align = c("left", "right")
        )
    )
})
