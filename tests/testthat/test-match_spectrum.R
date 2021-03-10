data("raman_hdpe")
data("test_lib")

test_that("match_spectrum() gives expected results", {
  expect_message(
    ms <- match_spectrum(raman_hdpe, test_lib, which = "test")
    )
  expect_error(match_spectrum(raman_hdpe, test_lib))
  expect_length(ms, 4)
  h1 <- c(ms[1,1:3])
  expect_equal(h1$sample_name, 5373)
  expect_equal(h1$spectrum_identity, "HDPE")
  expect_equal(h1$rsq, 0.91)
})
