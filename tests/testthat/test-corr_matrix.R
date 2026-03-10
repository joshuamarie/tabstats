# test_that("corr_matrix() invisibly returns a character matrix", {
#     m = cor(mtcars[, 1:4])
#     out = expect_invisible(corr_matrix(m))
#     expect_true(is.matrix(out))
#     expect_type(out, "character")
# })

test_that("corr_matrix() errors on non-matrix, non-corr_spec input", {
    expect_error(corr_matrix(list(a = 1)), "corr_spec.*symmetric matrix")
})

test_that("corr_matrix() errors on non-symmetric matrix", {
    bad = matrix(1:4, 2, 2)
    expect_error(corr_matrix(bad), "symmetric")
})

test_that("corr_matrix() accepts a corr_spec object", {
    spec = new_corr_spec(
        var1 = c("a", "a", "b"),
        var2 = c("b", "c", "c"),
        rho = c("0.89", "0.79", "0.66")
    )
    out = expect_invisible(corr_matrix(spec, method = "Pearson"))
    expect_true(is.matrix(out))
})

# test_that("corr_matrix() uses 'Unknown' as method when not supplied", {
#     m = cor(mtcars[, 1:3])
#     out = capture.output(corr_matrix(m))
#     expect_true(any(grepl("Unknown", out)))
# })

test_that("matrix_spec_resolver_cm creates auto column names when colnames is NULL", {
    m = matrix(c(1, 0.5, 0.5, 1), 2, 2)
    out = expect_invisible(corr_matrix(m))
    expect_true(is.matrix(out))
})

test_that("style_resolver_cm fills missing style keys with identity function", {
    style = style_resolver_cm(list(), c("rho", "pval"))
    expect_true(is.function(style[["rho"]]))
    expect_true(is.function(style[["title"]]))
    expect_true(is.function(style[["border_text"]]))
})

test_that("style_fn_cm converts 'red' string to a function", {
    fn = style_fn_cm("red")
    expect_type(fn, "closure")
})

test_that("style_fn_cm warns on unknown style string component", {
    expect_warning(style_fn_cm("notacolor"), "Unknown style")
})
