data("raman_hdpe")

test_that("subtr_bg() works as expected", {
  expect_silent(sb <- subtr_bg(raman_hdpe, degree = 8))
  expect_equal(as.numeric(
    round(cor(sb$spectra$intensity,
              subtr_bg(raman_hdpe, degree = 1)$spectra$intensity), 4)
  ), 0.9763, ignore_attr = F)
  expect_s3_class(sb, "OpenSpecy")
  expect_equal(nrow(sb$spectra), nrow(raman_hdpe$spectra))
  expect_equal(sb$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(sb$spectra), c(0, 1))
})
