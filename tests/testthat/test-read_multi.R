data("raman_hdpe")

test_that("reading in multi files doesn't throw error", {
  expect_silent(multi <- read_extdata("testdata_zipped.zip") |> read_any())
  expect_s3_class(multi, "OpenSpecy")

  expect_equal(multi$wavenumber, raman_hdpe$wavenumber)
  expect_equal(multi$spectra$intensity, raman_hdpe$spectra$intensity)
  expect_equal(multi$spectra$intensity.1, raman_hdpe$spectra$intensity)
  expect_equal(multi$spectra$intensity.2, raman_hdpe$spectra$intensity)
})
