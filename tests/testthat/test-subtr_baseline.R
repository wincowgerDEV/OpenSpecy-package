data("raman_hdpe")

test_that("polynomial subtr_baseline() works as expected", {
  poly <- subtr_baseline(raman_hdpe, degree = 8) |> expect_silent()

  cor(poly$spectra$intensity,
      subtr_baseline(raman_hdpe, degree = 1)$spectra$intensity) |> round(4) |>
    expect_equal(0.9929, ignore_attr = F)
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

test_that("smodpoly subtr_baseline() works as expected", {
    smod <- subtr_baseline(x = raman_hdpe, type = "polynomial",full = F,
                           iterations = 10, refit_at_end = T,
                           remove_peaks = T,
                           peak_width_mult = 3, 
                           degree = 10, degree_part = 2) |> 
        expect_silent()
    
    # Check output structure and validity
    expect_s3_class(smod, "OpenSpecy")
    expect_true(check_OpenSpecy(smod))
    
    # Validate dimensions remain consistent
    expect_equal(nrow(smod$spectra), nrow(raman_hdpe$spectra))
    expect_equal(smod$wavenumber, raman_hdpe$wavenumber)
    
    # Check intensity range is normalized
    expect_equal(range(smod$spectra), c(0, 1))
    
    # Correlation between original and corrected spectra should be less than polynomial fitting
    poly <- subtr_baseline(raman_hdpe, degree = 8)
    cor_poly <- cor(poly$spectra$intensity, smod$spectra$intensity)
    expect_true(cor_poly > 0.95)

})


