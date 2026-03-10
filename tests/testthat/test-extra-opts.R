test_that("tabstats_options() with no args returns a named list with all categories", {
    opts = tabstats_options()
    expect_type(opts, "list")
    expect_true("tab_default" %in% names(opts))
    expect_true("tab_digits" %in% names(opts))
})

test_that("tabstats_options('tab_default') returns the tab_default list", {
    td = tabstats_options("tab_default")
    expect_type(td, "list")
    expect_true("border_char" %in% names(td))
    expect_true("nrows" %in% names(td))
})

test_that("tabstats_options('tab_digits') returns the tab_digits list", {
    tdig = tabstats_options("tab_digits")
    expect_type(tdig, "list")
    expect_true("ex" %in% names(tdig))
})

test_that("tabstats_options retrieves a specific option by category + name", {
    val = tabstats_options("tab_default", "nrows")
    expect_type(val, "double")
    expect_equal(val, 10)
})

test_that("tabstats_options backward-compat: option name used as category", {
    val = tabstats_options("nrows")
    expect_equal(val, 10)
})

test_that("tabstats_options sets an option and retrieves updated value", {
    original = tabstats_options("tab_default", "nrows")
    on.exit(tabstats_options("tab_default", "nrows", original))

    tabstats_options("tab_default", "nrows", 25)
    expect_equal(tabstats_options("tab_default", "nrows"), 25)
})

test_that("tabstats_options backward-compat set: option name used as category", {
    original = tabstats_options("nrows")
    on.exit(tabstats_options("nrows", original))

    tabstats_options("nrows", 5)
    expect_equal(tabstats_options("nrows"), 5)
})

test_that("tabstats_options errors on invalid category", {
    expect_error(tabstats_options("nonexistent_category"), "Invalid category")
})

test_that("tabstats_options errors on unknown option within a valid category", {
    expect_error(
        tabstats_options("tab_default", "not_an_option"),
        "not found in category"
    )
})
