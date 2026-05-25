spec_bf = new_pairwise_data(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    bf = c("12.4", "3.1", "0.8")
)

spec_effect = new_pairwise_data(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    d = c("0.82", "0.45", "0.31"),
    ci = c("[0.61, 1.03]", "[0.24, 0.66]", "[0.10, 0.52]")
)

spec_icc = new_pairwise_data(
    var1 = c("rater1", "rater1", "rater2"),
    var2 = c("rater2", "rater3", "rater3"),
    icc = c("0.91", "0.87", "0.89"),
    ci = c("[0.85, 0.95]", "[0.79, 0.92]", "[0.82, 0.94]")
)

test_that("errors on non-matrix, non-pairwise_spec input", {
    expect_error(pairwise_matrix(list(a = 1)), "pairwise_spec.*symmetric matrix")
})

test_that("errors on non-symmetric matrix", {
    expect_error(pairwise_matrix(matrix(1:4, 2, 2)), "symmetric")
})

# ---- new_pairwise_data() ----

test_that("returns a pairwise_spec object", {
    expect_s3_class(spec_bf, "pairwise_spec")
})

test_that("errors when var1 and var2 have different lengths", {
    expect_error(
        new_pairwise_data(var1 = c("a", "b"), var2 = "c", bf = c("1.2", "3.4")),
        "same length"
    )
})

test_that("errors when extra vector has wrong length", {
    expect_error(
        new_pairwise_data(var1 = c("a"), var2 = c("b"), bf = c("1.2", "3.4")),
        "same length"
    )
})

test_that("accepts multiple extra fields", {
    expect_no_error(
        new_pairwise_data(
            var1 = c("a", "a", "b"),
            var2 = c("b", "c", "c"),
            d = c("0.82", "0.45", "0.31"),
            ci = c("[0.61, 1.03]", "[0.24, 0.66]", "[0.10, 0.52]")
        )
    )
})

# ---- pairwise_spec input ----

test_that("accepts a pairwise_spec and returns a matrix invisibly", {
    out = expect_invisible(pairwise_matrix(spec_bf, title = "Bayes Factor Matrix"))
    expect_true(is.matrix(out))
    expect_type(out, "character")
})

test_that("accepts a multi-field pairwise_spec (effect size + CI)", {
    expect_no_error(pairwise_matrix(spec_effect, title = "Cohen's d Matrix"))
})

test_that("accepts a multi-field pairwise_spec (ICC + CI)", {
    expect_no_error(pairwise_matrix(spec_icc, title = "ICC Matrix"))
})

test_that("name appears in layout legend header", {
    out = capture.output(
        pairwise_matrix(spec_bf, name = "BF Matrix", layout_view = TRUE)
    )
    expect_true(any(grepl("BF Matrix", out)))
})

test_that("name is used as fallback title when title is not supplied", {
    out = capture.output(pairwise_matrix(spec_bf, name = "Bayes Factor Matrix"))
    expect_true(any(grepl("Bayes Factor Matrix", out)))
})

test_that("accepts a symmetric matrix and returns invisibly", {
    m = cor(mtcars[, 1:4])
    out = expect_invisible(pairwise_matrix(m, title = "Pearson Correlation Matrix"))
    expect_true(is.matrix(out))
})

test_that("diagonal cells show '1' when diag_1 = TRUE (default)", {
    out = pairwise_matrix(cor(mtcars[, 1:3]))
    expect_true(any(out == "1"))
})

test_that("diagonal cells do not show '1' when diag_1 = FALSE", {
    m = cor(mtcars[, 1:3])
    out = pairwise_matrix(m, diag_1 = FALSE)
    diag_positions = seq(1, nrow(out) * ncol(out), by = ncol(out) + 1)
    expect_false(all(out[diag_positions] == "1"))
})

test_that("layout_view = TRUE prints without error", {
    expect_no_error(pairwise_matrix(spec_effect, layout_view = TRUE))
})

test_that("cm_style() works with pairwise_matrix", {
    expect_no_error(
        pairwise_matrix(spec_effect, style = cm_style(d = "blue_bold", ci = "red"))
    )
})
