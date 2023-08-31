test_that("make_rel() function test", {
    expect_equal(make_rel(c(1, 2, 3)), c(0, 0.5, 1))
    expect_equal(make_rel(c(10, 20, 30)), c(0, 0.5, 1))
})

test_that("make_rel() give correct output", {
    rel <- make_rel(c(-1000, -1, 0, 1, 10))
    expect_equal(range(rel), c(0, 1))
    expect_equal(round(make_rel(c(965.86, 82.75, -458.28, -0.98, -62.94)), 4),
                 c(1.0000, 0.3799, 0.0000, 0.3211, 0.2776))
})

test_that("make_rel() give correct output with OpenSpecy objects", {
    data("raman_hdpe")
    expect_silent(rel <- make_rel(raman_hdpe))
    expect_s3_class(rel, "OpenSpecy")
    expect_true(check_OpenSpecy(rel))
    expect_equal(range(rel$spectra), c(0, 1))
    expect_true(check_OpenSpecy(rel))
})
