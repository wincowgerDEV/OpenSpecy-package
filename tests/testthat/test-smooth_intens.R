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

  cor(smt$spectra[, "intensity"],
      smooth_intens(raman_hdpe, polynomial = 1)$spectra[, "intensity"]) |>
      round(4) |>
    expect_equal(0.8043, ignore_attr = F)
  
  smt2 <- smooth_intens(raman_hdpe, lambda = 1600, d = 2, lag = 2, type = "wh") |> expect_silent()
  
  cor(smt2$spectra[, "intensity"],
      smooth_intens(raman_hdpe, lambda = 100, d = 2, lag = 2, type = "wh")$spectra[, "intensity"]) |>
      round(4) |>
      expect_equal(0.8572, ignore_attr = F)
  
  expect_true(check_OpenSpecy(smt2))
  expect_true(check_OpenSpecy(smt))
  expect_s3_class(smt, "OpenSpecy")
  expect_equal(nrow(smt$spectra), nrow(raman_hdpe$spectra))
  expect_equal(smt$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(smt$spectra), c(0, 1))
})

test_that("matrix Savitzky-Golay filtering matches the columnwise algorithm", {
  set.seed(52)
  y <- matrix(rnorm(101 * 7), nrow = 101, ncol = 7,
              dimnames = list(NULL, paste0("s", seq_len(7))))
  p <- 3L
  n <- 15L
  m <- 1L
  filt <- OpenSpecy:::.sgolay_filter(p = p, n = n, m = m)
  k <- floor(n / 2)
  legacy <- vapply(seq_len(ncol(y)), function(i) {
    values <- numeric(nrow(y))
    values[seq_len(k)] <- filt[seq_len(k), , drop = FALSE] %*%
      y[seq_len(n), i]
    values[(k + 1L):(nrow(y) - k)] <- as.numeric(
      stats::filter(y[, i], rev(filt[k + 1L, ]), sides = 2L)
    )[(k + 1L):(nrow(y) - k)]
    values[(nrow(y) - k + 1L):nrow(y)] <-
      filt[(k + 2L):n, , drop = FALSE] %*%
      y[(nrow(y) - n + 1L):nrow(y), i]
    values
  }, numeric(nrow(y)))
  colnames(legacy) <- colnames(y)

  expect_equal(
    OpenSpecy:::.sgfilt_matrix(y, p = p, n = n, m = m),
    legacy,
    tolerance = 1e-12
  )
})
