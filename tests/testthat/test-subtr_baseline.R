data("raman_hdpe")

test_that("polynomial subtr_baseline() works as expected", {
  poly <- subtr_baseline(raman_hdpe, degree = 8) |> expect_silent()

  cor(poly$spectra$intensity,
      subtr_baseline(raman_hdpe, degree = 1)$spectra$intensity) |> round(4) |>
    expect_equal(0.9763, ignore_attr = F)
  expect_s3_class(poly, "OpenSpecy")
  expect_true(check_OpenSpecy(poly))
  
  expect_equal(nrow(poly$spectra), nrow(raman_hdpe$spectra))
  expect_equal(poly$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(poly$spectra), c(0, 1))
})

test_that("manual subtr_baseline() works as expected", {
  subtr_baseline(raman_hdpe, type = "manual") |> expect_error()

  bl <- raman_hdpe
  bl$spectra$intensity <- bl$spectra$intensity / 2

  man <- subtr_baseline(raman_hdpe, type = "manual", baseline = bl) |>
    expect_silent()
  expect_true(check_OpenSpecy(man))

  cor(raman_hdpe$spectra$intensity, man$spectra$intensity) |>
    expect_equal(1, ignore_attr = F)
  expect_equal(nrow(man$spectra), nrow(raman_hdpe$spectra))
  expect_equal(man$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(man$spectra), c(0, 1))
})
