small = head(mtcars, 5)

# ---- Input validation --------------------------------------------------------

test_that("errors when x cannot be coerced to data frame", {
    expect_error(table_default(function() NULL), "data frame")
})

test_that("coerces a matrix to data frame silently", {
    m = matrix(1:9, 3, 3, dimnames = list(NULL, c("a", "b", "c")))
    expect_no_error(table_default(m))
})

test_that("errors on invalid style_columns type", {
    expect_error(
        table_default(small, style_columns = list(mpg = "cyan")),
        "td_style"
    )
})

test_that("errors on invalid style_colnames type", {
    expect_error(
        table_default(small, style_colnames = list(mpg = "cyan")),
        "td_style"
    )
})

test_that("warns when n_space is negative and resets to 2", {
    expect_warning(table_default(small, n_space = -1), "n_space")
})

test_that("warns when nrows is negative and resets to 10", {
    expect_warning(table_default(small, nrows = -1), "nrows")
})

# ---- Basic output ------------------------------------------------------------

test_that("returns a data frame invisibly", {
    result = expect_invisible(table_default(small))
    expect_s3_class(result, "data.frame")
})

test_that("output contains column names", {
    out = capture.output(table_default(small))
    expect_true(any(grepl("mpg", out)))
    expect_true(any(grepl("cyl", out)))
})

test_that("prints title when supplied", {
    out = capture.output(table_default(small, title = "My Table"))
    expect_true(any(grepl("My Table", out)))
})

test_that("empty data frame with 0 cols prints message and returns NULL", {
    df = data.frame()
    result = expect_invisible(table_default(df))
    expect_null(result)
})

# ---- Truncation ----

test_that("truncates to nrows rows", {
    result = table_default(mtcars, nrows = 3)
    expect_equal(nrow(result), 3L)
})

test_that("truncation alert is shown when rows are truncated", {
    expect_message(
        table_default(mtcars, nrows = 3),
        regexp = "Showing 3 of 32 rows"
    )
})

# ---- Row names -----

test_that("show_row_names = TRUE adds row_names column", {
    result = table_default(small, show_row_names = TRUE)
    expect_true("row_names" %in% names(result))
})

test_that("show_row_names = FALSE does not add row_names column", {
    result = table_default(small, show_row_names = FALSE)
    expect_false("row_names" %in% names(result))
})

# ---- td_style() ----

test_that("td_style() with named string runs without error", {
    expect_no_error(
        table_default(small, style_columns = td_style(mpg = "cyan", cyl = "magenta"))
    )
})

test_that("td_style() with lambda runs without error", {
    expect_no_error(
        table_default(
            small,
            style_columns = td_style(mpg = \(ctx) cli::col_red(ctx$value))
        )
    )
})

test_that("td_style() rejects non-string, non-function values", {
    expect_error(td_style(mpg = 123), "must be a string or a function")
})

test_that("td_style() returns correct class", {
    s = td_style(mpg = "cyan")
    expect_s3_class(s, "td_style")
    expect_s3_class(s, "tabstats_style")
})

test_that("td_style() stores entries correctly", {
    fn = \(ctx) ctx$value
    s = td_style(mpg = fn)
    expect_identical(s$mpg, fn)
})

# ---- data_formatter() --------------------------------------------------------

test_that("rounds numeric columns to given digits", {
    df = data.frame(x = c(1.23456, 2.34567))
    fmt = data_formatter(df, digits = 2, digits_by_col = NULL,
                         scientific = FALSE, na_print = "")
    expect_equal(fmt$x, c("1.23", "2.35"))
})

test_that("formats integer-like numerics without decimals", {
    df = data.frame(x = c(1.0, 2.0, 3.0))
    fmt = data_formatter(df, digits = 3, digits_by_col = NULL,
                         scientific = FALSE, na_print = "")
    expect_equal(fmt$x, c("1", "2", "3"))
})

test_that("replaces NA with na_print string", {
    df = data.frame(x = c(1.0, NA))
    fmt = data_formatter(df, digits = 1, digits_by_col = NULL,
                         scientific = FALSE, na_print = "-")
    expect_equal(fmt$x[2], "-")
})

test_that("converts factors to character", {
    df = data.frame(f = factor(c("a", "b")))
    fmt = data_formatter(df, digits = 1, digits_by_col = NULL,
                         scientific = FALSE, na_print = "")
    expect_type(fmt$f, "character")
})

test_that("digits_by_col overrides global digits for named column", {
    df = data.frame(x = c(1.23456), y = c(1.23456))
    fmt = data_formatter(df, digits = 3, digits_by_col = list(x = 1),
                         scientific = FALSE, na_print = "")
    expect_equal(fmt$x, "1.2")
    expect_equal(fmt$y, "1.235")
})

test_that("scientific = TRUE formats in scientific notation", {
    df = data.frame(x = c(12345.678))
    fmt = data_formatter(df, digits = 2, digits_by_col = NULL,
                         scientific = TRUE, na_print = "")
    expect_true(grepl("e", fmt$x, ignore.case = TRUE))
})

# ---- col_widths_total() ------------------------------------------------------

test_that("returns widths >= column name lengths", {
    df = data.frame(a = "x", longcolname = "y")
    x_chars = as.matrix(df)
    widths = col_widths_total(df, x_chars, colnames(df), NULL, df, NULL)
    expect_gte(widths[2], nchar("longcolname"))
})

test_that("respects min_width", {
    df = data.frame(a = "x", b = "y")
    x_chars = as.matrix(df)
    widths = col_widths_total(df, x_chars, colnames(df), NULL, df, 20)
    expect_true(all(widths >= 20))
})

# ---- vb_resolve() ------------------------------------------------------------

test_that("returns empty idx when vb has no after spec", {
    setup = vb_resolve(list(), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_length(setup$idx, 0)
})

test_that("maps character column names to indices", {
    setup = vb_resolve(list(after = "b"), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_equal(setup$idx, 2L)
})

test_that("maps numeric indices directly", {
    setup = vb_resolve(list(after = 2L), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_equal(setup$idx, 2L)
})

test_that("excludes out-of-range border indices", {
    setup = vb_resolve(list(after = 10L), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_length(setup$idx, 0)
})

# ---- width_total() -----------------------------------------------------------

test_that("computes correct total width", {
    expect_equal(width_total(c(5L, 5L, 5L), 2L, 0L, 1L), 23L)
})

test_that("accounts for vertical borders in total width", {
    without = width_total(c(5L, 5L), 2L, 0L, 1L)
    with_vb = width_total(c(5L, 5L), 2L, 1L, 1L)
    expect_gt(with_vb, without)
})

# ---- left_padding() ----------------------------------------------------------

test_that("returns empty string when center_table = FALSE", {
    expect_equal(left_padding(FALSE, 120, 80), "")
})

test_that("returns non-empty string when table is narrower than terminal", {
    pad = left_padding(TRUE, 200, 80)
    expect_gt(nchar(pad), 0)
})

test_that("returns empty string when table is wider than terminal", {
    expect_equal(left_padding(TRUE, 80, 200), "")
})

# ---- style_mapper() ----------------------------------------------------------

test_that("returns function from named list by column name", {
    fn = function(ctx) ctx$value
    result = style_mapper(list(mpg = fn), "mpg", 1)
    expect_identical(result, fn)
})

test_that("returns function by column index string", {
    fn = function(ctx) ctx$value
    result = style_mapper(list("1" = fn), "mpg", 1)
    expect_identical(result, fn)
})

test_that("returns NULL for unknown column", {
    expect_null(style_mapper(list(mpg = function(ctx) ctx$value), "cyl", 2))
})

test_that("returns NULL when styles is NULL", {
    expect_null(style_mapper(NULL, "mpg", 1))
})

test_that("returns entry by position for unnamed list", {
    fn = function(ctx) ctx$value
    result = style_mapper(list(fn), "anything", 1)
    expect_identical(result, fn)
})

# ---- ctx_maker() -------------------------------------------------------------

test_that("returns list with all required keys", {
    ctx = ctx_maker("val", "col1", 1L, FALSE, mtcars, 10L)
    expect_named(
        ctx,
        c("value", "formatted_value", "col_name", "col_index",
          "is_header", "data", "justify", "width"),
        ignore.order = TRUE
    )
})

test_that("value and formatted_value are equal on construction", {
    ctx = ctx_maker("val", "col1", 1L, FALSE, mtcars, 10L)
    expect_equal(ctx$value, ctx$formatted_value)
})

# ---- group_cols() ------------------------------------------------------------

test_that("puts all columns in one group when they fit", {
    groups = group_cols(2:5, c(5L, 5L, 5L, 5L, 5L), avail_w = 100, n_space = 2)
    expect_length(groups, 1)
    expect_equal(groups[[1]], 2:5)
})

test_that("splits into multiple groups when columns overflow", {
    groups = group_cols(2:5, rep(10L, 5), avail_w = 14, n_space = 2)
    expect_gt(length(groups), 1)
})

test_that("each group always starts a new column when avail_w is tiny", {
    groups = group_cols(2:4, rep(10L, 4), avail_w = 1, n_space = 2)
    expect_length(groups, 3)
})

# ---- vb_remap() ----

test_that("returns empty when vb_idx is empty", {
    expect_equal(vb_remap(integer(0), 1:4), integer(0))
})

test_that("remaps border index to position within cur_cols", {
    expect_equal(vb_remap(3L, c(1L, 3L, 5L)), 2L)
})

test_that("excludes last column position", {
    expect_length(vb_remap(5L, c(1L, 3L, 5L)), 0)
})

test_that("handles multiple remapped indices", {
    result = vb_remap(c(2L, 4L), c(1L, 2L, 4L, 6L))
    expect_equal(result, c(2L, 3L))
})
