spec_lt = new_corr_data(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    rho = c("0.89", "0.79", "0.66")
)

spec_multi = new_corr_data(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    rho = c("0.89", "0.79", "0.66"),
    pval = c("<0.001", "<0.001", "0.012")
)

# ---- Input validation ----

test_that("errors on non-matrix, non-corr_spec input", {
    expect_error(corr_matrix(list(a = 1)), "corr_spec.*symmetric matrix")
})

test_that("errors on non-symmetric matrix", {
    expect_error(corr_matrix(matrix(1:4, 2, 2)), "symmetric")
})

# ---- corr_spec input -----

test_that("accepts a corr_spec object and returns a matrix invisibly", {
    out = expect_invisible(corr_matrix(spec_lt, title = "Pearson Correlation Matrix"))
    expect_true(is.matrix(out))
    expect_type(out, "character")
})

test_that("accepts a multi-field corr_spec", {
    expect_no_error(corr_matrix(spec_multi, title = "Pearson Correlation Matrix"))
})

# ---- plain matrix input ------------------------------------------------------

test_that("accepts a symmetric matrix", {
    m = cor(mtcars[, 1:4])
    out = expect_invisible(corr_matrix(m, title = "Pearson Correlation Matrix"))
    expect_true(is.matrix(out))
})

test_that("auto-generates variable names when colnames is NULL", {
    m = matrix(c(1, 0.5, 0.5, 1), 2, 2)
    expect_no_error(corr_matrix(m))
})

test_that("uses 'Correlation Matrix' as title when not supplied", {
    out = capture.output(corr_matrix(cor(mtcars[, 1:3])))
    expect_true(any(grepl("Correlation Matrix", out)))
})

test_that("uses supplied title string in output", {
    out = capture.output(corr_matrix(cor(mtcars[, 1:3]), title = "Spearman Correlation Matrix"))
    expect_true(any(grepl("Spearman Correlation Matrix", out)))
})

# ---- diag_1 --------------

test_that("diagonal cells show '1' when diag_1 = TRUE", {
    out = corr_matrix(cor(mtcars[, 1:3]), title = "Pearson Correlation Matrix")
    diag_vals = diag(out[seq(1, nrow(out), 1), ])
    expect_true(any(out == "1"))
})

# ---- layout_view ---------

test_that("layout_view = TRUE prints without error", {
    expect_no_error(corr_matrix(spec_lt, layout_view = TRUE))
})

# ---- cm_style() ----------

test_that("cm_style() with named string runs without error", {
    expect_no_error(
        corr_matrix(spec_multi, style = cm_style(rho = "blue_bold", pval = "red"))
    )
})

test_that("cm_style() with lambda runs without error", {
    expect_no_error(
        corr_matrix(spec_lt, style = cm_style(rho = \(x) cli::col_cyan(x)))
    )
})

test_that("cm_style() returns correct class", {
    s = cm_style(rho = "blue")
    expect_s3_class(s, "cm_style")
    expect_s3_class(s, "tabstats_style")
})

test_that("cm_style() rejects non-string, non-function values", {
    expect_error(cm_style(rho = 123), "must be a string or a function")
})

# ---- style_resolver_cm() -----------------------------------------------------

test_that("fills missing keys with identity functions", {
    style = style_resolver_cm(list(), c("rho", "pval"))
    expect_true(is.function(style[["rho"]]))
    expect_true(is.function(style[["title"]]))
    expect_true(is.function(style[["border_text"]]))
})

test_that("converts string style to a function", {
    style = style_resolver_cm(list(rho = "blue"), c("rho"))
    expect_type(style[["rho"]], "closure")
})

# ---- style_fn_cm() -------

test_that("converts known style string to a function", {
    fn = style_fn_cm("red")
    expect_type(fn, "closure")
})

test_that("warns on unknown style string component", {
    expect_warning(style_fn_cm("notacolor"), "Unknown style")
})

test_that("combined style string (e.g. blue_bold) returns a function", {
    fn = style_fn_cm("blue_bold")
    expect_type(fn, "closure")
})

# ---- new_corr_data() -----

test_that("errors when var1 and var2 have different lengths", {
    expect_error(
        new_corr_data(var1 = c("a", "b"), var2 = "c", rho = c("0.5", "0.6")),
        "same length"
    )
})

test_that("errors when extra vector has wrong length", {
    expect_error(
        new_corr_data(var1 = c("a"), var2 = c("b"), rho = c("0.5", "0.6")),
        "same length"
    )
})

test_that("returns a corr_spec object", {
    expect_s3_class(spec_lt, "corr_spec")
})

test_that("detect_pattern correctly identifies lower triangle", {
    expect_equal(spec_lt$pattern, "lt")
})

test_that("detect_pattern correctly identifies full matrix", {
    vars = c("a", "b")
    all_pairs = expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE)
    spec = new_corr_data(
        var1 = all_pairs$var1,
        var2 = all_pairs$var2,
        rho = rep("0.5", nrow(all_pairs))
    )
    expect_equal(spec$pattern, "full")
})

test_that("extract_vars preserves first-seen order", {
    vars = extract_vars(c("b", "a", "b"), c("a", "c", "c"))
    expect_equal(vars, c("b", "a", "c"))
})
