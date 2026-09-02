test_that("adj_neg() give correct output", {
  expect_true(all(adj_neg(c(-1000, -1, 0, 1, 10)) >= 1))
  expect_equal(adj_neg(c(965.86, 82.75, -458.28, -0.98, -62.94)),
               c(1425.14, 542.03, 1.00, 458.30, 396.34))
})

test_that("adj_res() function test", {
    expect_equal(adj_res(seq(500, 4000, 4), 5), round(seq(500, 4000, 4)/5)*5)
})

test_that("mean_replace() fills vectors globally and matrices by spectrum", {
  expect_equal(mean_replace(c(1, NA, 3)), c(1, 2, 3))
  spectra <- matrix(c(1, NA, 3, 10, 20, NA), nrow = 3)
  expect_equal(
    mean_replace(spectra),
    matrix(c(1, 2, 3, 10, 20, 15), nrow = 3)
  )
})

test_that("conform_res() function test", {
    x <- seq(500, 4000, 4)
    expect_equal(conform_res(x, 5), seq(floor(min(x)/5)*5, ceiling(max(x)/5)*5, by=5))
})

test_that("adj_neg() function test", {
    expect_equal(adj_neg(c(-1000, -1, 0, 1, 10)), c(1, 1000, 1001, 1002, 1011))
    expect_equal(adj_neg(c(1, 2, 3)), c(1, 2, 3))
})
