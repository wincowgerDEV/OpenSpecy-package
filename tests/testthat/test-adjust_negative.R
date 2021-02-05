test_that("adjust_negative() give correct output", {
  expect_true(all(adjust_negative(c(-1000, -1, 0, 1, 10)) >= 1))
})
