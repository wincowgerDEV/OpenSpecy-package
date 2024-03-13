data("raman_hdpe")

test_that("smooth_intens() handles input errors correctly", {
  smooth_intens(1:1000) |> expect_error()
})

test_that("calc_window_points() will return consistent values", {
    calc_window_points(raman_hdpe, 70) |> expect_equal(23)
    calc_window_points(raman_hdpe, 50) |> expect_equal(15)
    calc_window_points(raman_hdpe, 140) |> expect_equal(45)
    calc_window_points(raman_hdpe, 10000) |> expect_error()
})

test_that("calc_window_points() works with vectors", {
    calc_window_points(raman_hdpe$wavenumber, 70) |> expect_equal(23)
    calc_window_points(raman_hdpe$wavenumber, 50) |> expect_equal(15)
    calc_window_points(raman_hdpe$wavenumber, 140) |> expect_equal(45)
    calc_window_points(raman_hdpe$wavenumber, 10000) |> expect_error()
})

test_that("smooth_intens() works as expected", {
  smt <- smooth_intens(raman_hdpe, polynomial = 3) |> expect_silent()

  cor(smt$spectra$intensity,
      smooth_intens(raman_hdpe, polynomial = 1)$spectra$intensity) |>
      round(4) |>
    expect_equal(0.8043, ignore_attr = F)
  
  smt2 <- smooth_intens(raman_hdpe, lambda = 1600, d = 2, lag = 2, type = "wh") |> expect_silent()
  
  cor(smt2$spectra$intensity,smooth_intens(raman_hdpe, lambda = 100, d = 2, lag = 2, type = "wh")$spectra$intensity) |>
      round(4) |>
      expect_equal(0.8572, ignore_attr = F)
  
  expect_true(check_OpenSpecy(smt2))
  expect_true(check_OpenSpecy(smt))
  expect_s3_class(smt, "OpenSpecy")
  expect_equal(nrow(smt$spectra), nrow(raman_hdpe$spectra))
  expect_equal(smt$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(smt$spectra), c(0, 1))
})
