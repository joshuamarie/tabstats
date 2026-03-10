test_that("pct_resolver returns all FALSE when percentage = NULL", {
    flags = pct_resolver(NULL)
    expect_false(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("pct_resolver returns all FALSE when percentage = FALSE", {
    flags = pct_resolver(FALSE)
    expect_false(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("pct_resolver returns all TRUE when percentage = TRUE", {
    flags = pct_resolver(TRUE)
    expect_true(flags$row)
    expect_true(flags$col)
    expect_true(flags$total)
})

test_that("pct_resolver returns all TRUE for 'all'", {
    flags = pct_resolver("all")
    expect_true(flags$row)
    expect_true(flags$col)
    expect_true(flags$total)
})

test_that("pct_resolver handles 'by_row' correctly", {
    flags = pct_resolver("by_row")
    expect_true(flags$row)
    expect_false(flags$col)
    expect_false(flags$total)
})

test_that("pct_resolver handles 'by_col' correctly", {
    flags = pct_resolver("by_col")
    expect_false(flags$row)
    expect_true(flags$col)
    expect_false(flags$total)
})

test_that("pct_resolver handles 'by_total' correctly", {
    flags = pct_resolver("by_total")
    expect_false(flags$row)
    expect_false(flags$col)
    expect_true(flags$total)
})

test_that("digits_resolver expands scalar to all fields", {
    resolved = digits_resolver(2)
    expect_equal(resolved$ex, 2)
    expect_equal(resolved$row_pct, 2)
    expect_equal(resolved$col_pct, 2)
    expect_equal(resolved$total_pct, 2)
})

test_that("num_formatter_ct formats whole numbers without decimals", {
    expect_equal(num_formatter_ct(5.0), "5")
    expect_equal(num_formatter_ct(0), "0")
})

test_that("num_formatter_ct formats non-integer to given decimal places", {
    expect_equal(num_formatter_ct(3.14159, 2), "3.14")
})

test_that("cell_width_ct returns 0 for empty string", {
    expect_equal(cell_width_ct(""), 0L)
})

test_that("cell_width_ct returns max line width for multi-line cell", {
    cell = "10\n50.0%\n25.0%"
    expect_equal(cell_width_ct(cell), 5L)
})

test_that("cross_table() returns invisible matrix", {
    m = matrix(c(10, 20, 30, 40), nrow = 2)
    result = expect_invisible(cross_table(m, layout = FALSE))
    expect_true(is.matrix(result))
})

test_that("cross_table() uses table input without error", {
    tbl = table(mtcars$cyl, mtcars$gear)
    result = expect_invisible(cross_table(tbl, layout = FALSE))
    expect_true(is.matrix(result))
})

test_that("cross_table() includes percentage rows when percentage = 'all'", {
    m = matrix(c(10, 20, 30, 40), nrow = 2)
    out = capture.output(cross_table(m, percentage = "all", layout = FALSE))
    expect_true(any(grepl("%", out)))
})

test_that("cross_table() has no percentage lines when percentage = FALSE", {
    m = matrix(c(10, 20, 30, 40), nrow = 2)
    out = capture.output(cross_table(m, percentage = FALSE, layout = FALSE))
    expect_false(any(grepl("%", out)))
})

test_that("cross_table() respects expected = FALSE (no parentheses in output)", {
    m = matrix(c(10, 20, 30, 40), nrow = 2)
    out = capture.output(cross_table(m, expected = FALSE, layout = FALSE))
    expect_false(any(grepl("\\(", out)))
})

test_that("cross_table() uses row/col names from named matrix", {
    m = matrix(c(10, 20, 30, 40), nrow = 2,
               dimnames = list(c("Yes", "No"), c("A", "B")))
    out = capture.output(cross_table(m, layout = FALSE))
    expect_true(any(grepl("Yes|No", out)))
    expect_true(any(grepl("A|B", out)))
})
