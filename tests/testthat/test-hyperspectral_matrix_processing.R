data("raman_hdpe")

test_that("make_rel() normalizes matrix spectra by column", {
  mat <- matrix(c(-2, 0, 2, 10, 20, 50),
                nrow = 3,
                dimnames = list(NULL, c("a", "b")))
  expected <- matrix(c(0, 0.5, 1, 0, 0.25, 1),
                     nrow = 3,
                     dimnames = dimnames(mat))

  expect_equal(make_rel(mat), expected)

  os <- as_OpenSpecy(1:3, spectra = as.data.frame(mat))
  expect_equal(make_rel(os)$spectra, expected)
  expect_equal(dim(make_rel(matrix(42, nrow = 1, ncol = 1))), c(1L, 1L))
})

test_that("Savitzky-Golay smoothing uses the same numerical filter for matrices", {
  spec <- cbind(
    raw = raman_hdpe$spectra[, 1L],
    shifted = raman_hdpe$spectra[, 1L] + seq_len(nrow(raman_hdpe$spectra)) / 100
  )
  multi <- as_OpenSpecy(raman_hdpe$wavenumber, spectra = as.data.frame(spec))

  fast <- smooth_intens(multi, polynomial = 3, window = 11, derivative = 1,
                        abs = FALSE, make_rel = FALSE)

  filt <- signal::sgolay(p = 3, n = 11, m = 1)
  legacy <- vapply(seq_len(ncol(spec)), function(i) {
    as.numeric(signal::filter(filt = filt, x = spec[, i]))
  }, FUN.VALUE = numeric(nrow(spec)))
  colnames(legacy) <- colnames(spec)

  expect_equal(fast$spectra, legacy, tolerance = 1e-12)
  expect_equal(fast$wavenumber, multi$wavenumber)
})

test_that("conform_spec() interpolation reuses one index map for all spectra", {
  spec <- cbind(
    raw = raman_hdpe$spectra[, 1L],
    scaled = raman_hdpe$spectra[, 1L] * 1.5
  )
  multi <- as_OpenSpecy(raman_hdpe$wavenumber, spectra = as.data.frame(spec))
  target <- seq(600, 700, by = 3)

  fast <- conform_spec(multi, range = target, res = NULL, type = "interp")
  legacy <- vapply(seq_len(ncol(spec)), function(i) {
    approx(x = multi$wavenumber, y = spec[, i], xout = target)$y
  }, FUN.VALUE = numeric(length(target)))
  colnames(legacy) <- colnames(spec)

  expect_equal(fast$wavenumber, target)
  expect_equal(fast$spectra, legacy, tolerance = 1e-12)
})

test_that("manual baseline subtraction is matrix native and equivalent", {
  spec <- cbind(
    raw = raman_hdpe$spectra[, 1L],
    offset = raman_hdpe$spectra[, 1L] + 5
  )
  multi <- as_OpenSpecy(raman_hdpe$wavenumber, spectra = as.data.frame(spec))
  baseline <- as_OpenSpecy(
    raman_hdpe$wavenumber,
    spectra = data.frame(baseline = raman_hdpe$spectra[, 1L] / 10)
  )

  base_y <- approx(baseline$wavenumber, baseline$spectra[, 1L],
                   xout = multi$wavenumber, rule = 2, method = "linear",
                   ties = mean)$y
  expected <- multi$spectra - base_y

  fast <- subtr_baseline(multi, type = "manual", baseline = baseline,
                         make_rel = FALSE)
  expect_equal(fast$spectra, expected)

  fast_rel <- subtr_baseline(multi, type = "manual", baseline = baseline,
                             make_rel = TRUE)
  expect_equal(fast_rel$spectra, make_rel(expected))
})
