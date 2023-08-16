data("raman_hdpe")

test_that("subtr_baseline() works as expected", {
  sb <- subtr_baseline(raman_hdpe, degree = 8) |> expect_silent()

  cor(sb$spectra$intensity,
      subtr_baseline(raman_hdpe, degree = 1)$spectra$intensity) |> round(4) |>
    expect_equal(0.9763, ignore_attr = F)
  expect_s3_class(sb, "OpenSpecy")
  expect_equal(nrow(sb$spectra), nrow(raman_hdpe$spectra))
  expect_equal(sb$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(sb$spectra), c(0, 1))
})
