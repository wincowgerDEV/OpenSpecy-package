make_peak_ratio_fixture <- function() {
  as_OpenSpecy(
    c(1000, 1010, 1020),
    cbind(
      alpha = c(2, 6, 12),
      beta = c(8, 4, 2)
    )
  )
}

test_that("peak_ratio() uses exact measured points and preserves spectrum names", {
  x <- make_peak_ratio_fixture()

  result <- peak_ratio(x, numerator = 1000, denominator = 1020)

  expect_equal(result, c(alpha = 1 / 6, beta = 4))
  expect_named(result, colnames(x$spectra))
})

test_that("peak_ratio() defaults to nearest with lower-wavenumber midpoint ties", {
  x <- make_peak_ratio_fixture()

  tied <- peak_ratio(x, numerator = 1005, denominator = 1015)
  closer <- peak_ratio(x, numerator = 1006, denominator = 1016)

  expect_equal(tied, c(alpha = 1 / 3, beta = 2))
  expect_equal(closer, c(alpha = 1 / 2, beta = 2))
})

test_that("peak_ratio() linearly interpolates only adjacent point values", {
  x <- make_peak_ratio_fixture()

  result <- peak_ratio(
    x, numerator = 1005, denominator = 1015, method = "linear"
  )

  expect_equal(result, c(alpha = 4 / 9, beta = 2))
  expect_equal(
    peak_ratio(x, numerator = 1000, denominator = 1020,
               method = "linear"),
    c(alpha = 1 / 6, beta = 4)
  )
})

test_that("peak_ratio() returns named NA values outside the shared axis", {
  x <- make_peak_ratio_fixture()

  expect_warning(
    result <- peak_ratio(x, numerator = 999, denominator = 1010),
    "does not cover"
  )

  expect_identical(
    result,
    stats::setNames(c(NA_real_, NA_real_), c("alpha", "beta"))
  )
})

test_that("peak_ratio() replaces only invalid per-spectrum results with NA", {
  x <- as_OpenSpecy(
    c(1000, 1010),
    cbind(
      good = c(4, 2),
      zero = c(4, 0),
      missing_numerator = c(NA, 2),
      missing_denominator = c(4, NA),
      infinite_numerator = c(Inf, 2)
    )
  )

  expect_warning(
    result <- peak_ratio(x, numerator = 1000, denominator = 1010),
    "zero or non-finite denominator"
  )

  expect_identical(unname(result["good"]), 2)
  expect_true(all(is.na(result[-1L])))
  expect_false(any(is.infinite(result)))
})

test_that("peak_ratio() propagates missing adjacent values when interpolating", {
  x <- as_OpenSpecy(
    c(1000, 1010, 1020),
    cbind(good = c(2, 6, 10), missing = c(2, NA, 10))
  )

  expect_warning(
    result <- peak_ratio(
      x, numerator = 1005, denominator = 1015, method = "linear"
    ),
    "non-finite numerator"
  )

  expect_equal(result["good"], c(good = 0.5))
  expect_true(is.na(result["missing"]))
})

test_that("peak_ratio() validates its object, points, and method", {
  x <- make_peak_ratio_fixture()

  expect_error(
    peak_ratio("not OpenSpecy", numerator = 1000, denominator = 1010),
    "class 'OpenSpecy'"
  )
  expect_error(
    peak_ratio(x, numerator = c(1000, 1010), denominator = 1020),
    "finite numeric scalar"
  )
  expect_error(
    peak_ratio(x, numerator = Inf, denominator = 1020),
    "finite numeric scalar"
  )
  expect_error(
    peak_ratio(x, numerator = 1000, denominator = NA_real_),
    "finite numeric scalar"
  )
  expect_error(
    peak_ratio(x, numerator = 1000, denominator = 1020,
               method = "spline")
  )
})
