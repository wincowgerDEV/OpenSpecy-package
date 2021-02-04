test_that("adjust_negative() give correct output", {
  expect_equal(adjust_negative(-5:5), 1:11)
  expect_equal(adjust_negative(c(0.2, 0.7, 1)), c(1.4, 1.9, 2.2))
})
