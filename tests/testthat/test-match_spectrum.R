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

test_that("find_spectrum() works as expected", {
  expect_silent(
    fs <- find_spectrum(test_lib, which = "test", sample_name == 5373)
  )
  expect_error(find_spectrum(test_lib, sample_name == 5373))
  expect_length(fs, 4)
  h2 <- c(fs[1,c(1,3,4)])
  expect_equal(h2$sample_name, 5373)
  expect_equal(h2$spectrum_identity, "HDPE")
})
