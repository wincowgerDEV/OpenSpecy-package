data("raman_hdpe")

test_that("smooth_intens() works as expected", {
  smt <- smooth_intens(raman_hdpe, p = 3) |> expect_silent()

  cor(smt$spectra$intensity,
      smooth_intens(raman_hdpe, p = 1)$spectra$intensity) |> round(4) |>
    expect_equal(0.9756, ignore_attr = F)
  expect_s3_class(smt, "OpenSpecy")
  expect_equal(nrow(smt$spectra), nrow(raman_hdpe$spectra))
  expect_equal(smt$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(smt$spectra), c(0, 1))
})
