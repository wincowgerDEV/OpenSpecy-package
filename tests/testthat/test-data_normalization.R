test_that("adjust_negative() give correct output", {
  expect_true(all(adjust_negative(c(-1000, -1, 0, 1, 10)) >= 1))
  expect_equal(adjust_negative(c(965.86, 82.75, -458.28, -0.98, -62.94)),
               c(1425.14, 542.03, 1.00, 458.30, 396.34))
})

test_that("make_relative() give correct output", {
  rel <- make_relative(c(-1000, -1, 0, 1, 10))
  expect_equal(range(rel), c(0, 1))
  expect_equal(round(make_relative(c(965.86, 82.75, -458.28, -0.98, -62.94)), 4),
               c(1.0000, 0.3799, 0.0000, 0.3211, 0.2776))
})
