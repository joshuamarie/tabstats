test_that("table_default() returns a data frame invisibly", {
    result = expect_invisible(table_default(head(mtcars, 3)))
    expect_s3_class(result, "data.frame")
})

test_that("table_default() errors when x cannot be coerced to data frame", {
    expect_error(table_default(function() NULL), "data frame")
})

test_that("table_default() coerces a matrix to a data frame silently", {
    m = matrix(1:9, 3, 3, dimnames = list(NULL, c("a", "b", "c")))
    expect_invisible(table_default(m))
})

test_that("table_default() handles 0-row data frame throws an error message", {
    df = mtcars[0, ]
    expect_error(suppressWarnings(table_default(df)))
})

test_that("table_default() truncates to nrows rows", {
    result = table_default(mtcars, nrows = 3)
    expect_equal(nrow(result), 3L)
})

test_that("table_default() shows row names when show_row_names = TRUE", {
    result = table_default(head(mtcars, 2), show_row_names = TRUE)
    expect_true("row_names" %in% names(result))
})

test_that("table_default() prints a title when supplied", {
    out = capture.output(table_default(head(mtcars, 2), title = "My Table"))
    expect_true(any(grepl("My Table", out)))
})

test_that("data_formatter rounds numeric columns to given digits", {
    df = data.frame(x = c(1.23456, 2.34567))
    fmt = data_formatter(df, digits = 2, digits_by_col = NULL,
                         scientific = FALSE, na_print = "")
    expect_equal(fmt$x, c("1.23", "2.35"))
})

test_that("data_formatter formats integer-like numerics without decimals", {
    df = data.frame(x = c(1.0, 2.0, 3.0))
    fmt = data_formatter(df, digits = 3, digits_by_col = NULL,
                         scientific = FALSE, na_print = "")
    expect_equal(fmt$x, c("1", "2", "3"))
})

test_that("data_formatter replaces NA with na_print", {
    df = data.frame(x = c(1.0, NA))
    fmt = data_formatter(
        df,
        digits = 1,
        digits_by_col = NULL,
        scientific = FALSE,
        na_print = "\u2500"
    )
    expect_equal(fmt$x[2], "\u2500")
})

test_that("data_formatter converts factors to character", {
    df = data.frame(f = factor(c("a", "b")))
    fmt = data_formatter(
        df,
        digits = 1,
        digits_by_col = NULL,
        scientific = FALSE,
        na_print = ""
    )
    expect_type(fmt$f, "character")
})

test_that("col_widths_total returns widths >= column name lengths", {
    df = data.frame(a = c("x"), longname = c("y"))
    x_chars = as.matrix(df)
    col_names = colnames(df)
    widths = col_widths_total(df, x_chars, col_names, NULL, df, NULL)
    expect_gte(widths[2], nchar("longname"))
})

test_that("vb_resolve returns empty idx when vb has no 'after' spec", {
    setup = vb_resolve(list(), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_length(setup$idx, 0)
})

test_that("vb_resolve maps character column names to indices", {
    setup = vb_resolve(list(after = "b"), c("a", "b", "c"), c(4L, 4L, 4L))
    expect_equal(setup$idx, 2L)
})

test_that("width_total computes correct total", {
    expect_equal(width_total(c(5L, 5L, 5L), 2L, 0L, 1L), 23L)
})

test_that("left_padding returns empty string when center_table = FALSE", {
    expect_equal(left_padding(FALSE, 120, 80), "")
})

test_that("left_padding returns non-empty string when table is narrower than terminal", {
    pad = left_padding(TRUE, 200, 80)
    expect_gt(nchar(pad), 0)
})

test_that("style_mapper returns function from named list by col name", {
    fn = function(ctx) ctx$value
    styles = list(mpg = fn)
    result = style_mapper(styles, "mpg", 1)
    expect_identical(result, fn)
})

test_that("style_mapper returns NULL for unknown column", {
    styles = list(mpg = function(ctx) ctx$value)
    expect_null(style_mapper(styles, "cyl", 2))
})

test_that("style_mapper returns NULL when styles is NULL", {
    expect_null(style_mapper(NULL, "mpg", 1))
})

test_that("ctx_maker returns a list with required keys", {
    ctx = ctx_maker("val", "col1", 1L, FALSE, mtcars, 10L)
    expect_named(
        ctx,
        c("value", "formatted_value", "col_name", "col_index", "is_header", "data", "justify", "width"),
        ignore.order = TRUE
    )
})

test_that("group_cols groups all columns when they fit in avail_w", {
    groups = group_cols(2:5, c(5L, 5L, 5L, 5L, 5L), avail_w = 100, n_space = 2)
    expect_length(groups, 1)
    expect_equal(groups[[1]], 2:5)
})

test_that("group_cols splits into multiple groups when columns overflow", {
    groups = group_cols(2:5, rep(10L, 5), avail_w = 14, n_space = 2)
    expect_gt(length(groups), 1)
})

test_that("vb_remap returns empty when vb_idx is empty", {
    expect_equal(vb_remap(integer(0), 1:4), integer(0))
})

test_that("vb_remap remaps border index to position within cur_cols", {
    result = vb_remap(3L, c(1L, 3L, 5L))
    expect_equal(result, 2L)
})

test_that("vb_remap excludes last column position (border must be between cols)", {
    result = vb_remap(5L, c(1L, 3L, 5L))
    expect_length(result, 0)
})
