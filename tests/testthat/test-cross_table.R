m = matrix(
    c(10, 20, 30, 40),
    nrow = 2,
    dimnames = list(c("Yes", "No"), c("A", "B"))
)

m_named = matrix(
    c(10, 20, 30, 40),
    nrow = 2,
    dimnames = list(
        c("Yes", "No"),
        c("A", "B")
    )
)

# ---- pct_resolver() ----

test_that("returns all FALSE when percentage = NULL", {
    flags = pct_resolver(NULL)
    expect_false(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("returns all FALSE when percentage = FALSE", {
    flags = pct_resolver(FALSE)
    expect_false(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("returns all TRUE when percentage = TRUE", {
    flags = pct_resolver(TRUE)
    expect_true(flags$row)
    expect_true(flags$col)
    expect_true(flags$total)
})

test_that("returns all TRUE for 'all'", {
    flags = pct_resolver("all")
    expect_true(flags$row)
    expect_true(flags$col)
    expect_true(flags$total)
})

test_that("handles 'by_row' correctly", {
    flags = pct_resolver("by_row")
    expect_true(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("handles 'by_col' correctly", {
    flags = pct_resolver("by_col")
    expect_false(flags$row)
    expect_true(flags$col)
    expect_false(flags$total)
})

test_that("handles 'by_total' correctly", {
    flags = pct_resolver("by_total")
    expect_false(flags$row)
    expect_false(flags$col)
    expect_true(flags$total)
})

test_that("handles vector of multiple options", {
    flags = pct_resolver(c("by_row", "by_col"))
    expect_true(flags$row)
    expect_true(flags$col)
    expect_false(flags$total)
})

# ---- digits_resolver() -------------------------------------------------------

test_that("expands scalar to all fields", {
    resolved = digits_resolver(2)
    expect_equal(resolved$ex, 2)
    expect_equal(resolved$row_pct, 2)
    expect_equal(resolved$col_pct, 2)
    expect_equal(resolved$total_pct, 2)
})

test_that("returns defaults when digits = NULL", {
    resolved = digits_resolver(NULL)
    expect_type(resolved, "list")
    expect_true("ex" %in% names(resolved))
})

test_that("partial list overrides only specified fields", {
    defaults = getOption("tab_digits")
    resolved = digits_resolver(list(ex = 3))
    expect_equal(resolved$ex, 3)
    expect_equal(resolved$row_pct, defaults$row_pct)
})

# ---- num_formatter_ct() ------------------------------------------------------

test_that("formats whole numbers without decimals", {
    expect_equal(num_formatter_ct(5.0), "5")
    expect_equal(num_formatter_ct(0), "0")
})

test_that("formats non-integer to given decimal places", {
    expect_equal(num_formatter_ct(3.14159, 2), "3.14")
})

# ---- pct_formatter_ct() ------------------------------------------------------

test_that("returns NULL for 100 (total row/col suppressed)", {
    expect_null(pct_formatter_ct(100, 1))
})

test_that("returns NULL for NA", {
    expect_null(pct_formatter_ct(NA, 1))
})

test_that("formats a percentage correctly", {
    expect_equal(pct_formatter_ct(50, 1), "50.0%")
})

# ---- cell_width_ct() ---------------------------------------------------------

test_that("returns 0 for empty string", {
    expect_equal(cell_width_ct(""), 0L)
})

test_that("returns max line width for multi-line cell", {
    expect_equal(cell_width_ct("10\n50.0%\n25.0%"), 5L)
})

test_that("handles single-line cell", {
    expect_equal(cell_width_ct("123"), 3L)
})

# ---- cross_table() -----

test_that("returns invisible matrix", {
    result = expect_invisible(cross_table(m, layout = FALSE))
    expect_true(is.matrix(result))
})

test_that("accepts a table object", {
    tbl = table(mtcars$cyl, mtcars$gear)
    result = expect_invisible(cross_table(tbl, layout = FALSE))
    expect_true(is.matrix(result))
})

test_that("output contains row names", {
    out = capture.output(cross_table(m_named, layout = FALSE))
    expect_true(any(grepl("Yes|No", out)))
})

test_that("output contains column names", {
    out = capture.output(cross_table(m_named, layout = FALSE))
    expect_true(any(grepl("A|B", out)))
})

test_that("output contains TOTAL label", {
    out = capture.output(cross_table(m, layout = FALSE))
    expect_true(any(grepl("TOTAL", out)))
})

test_that("output includes percentage signs when percentage = 'all'", {
    out = capture.output(cross_table(m, percentage = "all", layout = FALSE))
    expect_true(any(grepl("%", out)))
})

test_that("output has no percentage signs when percentage = FALSE", {
    out = capture.output(cross_table(m, percentage = FALSE, layout = FALSE))
    expect_false(any(grepl("%", out)))
})

test_that("expected = FALSE removes parentheses from output", {
    out = capture.output(cross_table(m, expected = FALSE, layout = FALSE))
    expect_false(any(grepl("\\(", out)))
})

test_that("expected = TRUE shows parenthesised values", {
    out = capture.output(cross_table(m, expected = TRUE, layout = FALSE))
    expect_true(any(grepl("\\(", out)))
})

test_that("layout = TRUE prints without error", {
    expect_no_error(cross_table(m, layout = TRUE))
})

# ---- ct_style() --------

test_that("ct_style() with named strings runs without error", {
    expect_no_error(
        cross_table(
            m,
            layout = FALSE,
            style = ct_style(observed = "blue", expected = "yellow")
        )
    )
})

test_that("ct_style() with lambda runs without error", {
    expect_no_error(
        cross_table(
            m,
            layout = FALSE,
            style = ct_style(observed = \(ctx) cli::col_green(ctx$formatted_text))
        )
    )
})

test_that("ct_style() rejects non-string, non-function values", {
    expect_error(ct_style(observed = 123), "must be a string or a function")
})

test_that("ct_style() returns correct class", {
    s = ct_style(observed = "blue")
    expect_s3_class(s, "ct_style")
    expect_s3_class(s, "tabstats_style")
})

test_that("ct_style() stores entries correctly", {
    fn = \(ctx) ctx$formatted_text
    s = ct_style(observed = fn)
    expect_identical(s$observed, fn)
})

test_that("ct_style() with all keys runs without error", {
    expect_no_error(
        ct_style(
            observed = "blue",
            expected = "yellow",
            row_percentage = "green",
            col_percentage = "cyan",
            total_percentage = "magenta",
            total = "bold",
            title = "bold",
            border_text = "yellow"
        )
    )
})
