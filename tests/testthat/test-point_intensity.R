make_point_intensity_fixture <- function(descending = FALSE) {
  wavenumber <- c(1000, 1010, 1020)
  spectra <- cbind(
    alpha = c(2, 6, 12),
    beta = c(8, 4, 2)
  )
  if (descending) {
    wavenumber <- rev(wavenumber)
    spectra <- spectra[3:1, , drop = FALSE]
  }
  as_OpenSpecy(wavenumber, spectra)
}

test_that("point_intensity() uses exact measured points and preserves names", {
  x <- make_point_intensity_fixture()

  result <- point_intensity(x, wavenumber = 1010)

  expect_identical(result, c(alpha = 6, beta = 4))
  expect_named(result, colnames(x$spectra))
})

test_that("point_intensity() resolves nearest midpoint ties downward", {
  x <- make_point_intensity_fixture()

  tied <- point_intensity(x, wavenumber = 1005)
  closer <- point_intensity(x, wavenumber = 1006)

  expect_identical(tied, c(alpha = 2, beta = 8))
  expect_identical(closer, c(alpha = 6, beta = 4))
})

test_that("point_intensity() linearly interpolates adjacent measurements", {
  ascending <- make_point_intensity_fixture()
  descending <- make_point_intensity_fixture(descending = TRUE)

  expected <- c(alpha = 4, beta = 6)
  expect_identical(
    point_intensity(ascending, wavenumber = 1005, method = "linear"),
    expected
  )
  expect_identical(
    point_intensity(descending, wavenumber = 1005, method = "linear"),
    expected
  )
})

test_that("point_intensity() returns named NA outside the shared axis", {
  x <- make_point_intensity_fixture()

  expect_warning(
    result <- point_intensity(x, wavenumber = 999),
    "does not cover"
  )

  expect_identical(
    result,
    stats::setNames(c(NA_real_, NA_real_), c("alpha", "beta"))
  )
})

test_that("point_intensity() replaces only non-finite results with NA", {
  x <- as_OpenSpecy(
    c(1000, 1010, 1020),
    cbind(
      good = c(2, 6, 10),
      missing = c(2, NA, 10),
      infinite = c(2, Inf, 10)
    )
  )

  expect_warning(
    exact <- point_intensity(x, wavenumber = 1010),
    "non-finite"
  )
  expect_identical(exact, c(good = 6, missing = NA_real_,
                            infinite = NA_real_))

  expect_warning(
    interpolated <- point_intensity(
      x, wavenumber = 1005, method = "linear"
    ),
    "non-finite"
  )
  expect_identical(interpolated, c(good = 4, missing = NA_real_,
                                   infinite = NA_real_))
})

test_that("point_intensity() validates its object, point, method, and axis", {
  x <- make_point_intensity_fixture()

  expect_error(
    point_intensity("not OpenSpecy", wavenumber = 1010),
    "class 'OpenSpecy'"
  )
  expect_error(
    point_intensity(x, wavenumber = c(1000, 1010)),
    "finite numeric scalar"
  )
  expect_error(
    point_intensity(x, wavenumber = Inf),
    "finite numeric scalar"
  )
  expect_error(
    point_intensity(x, wavenumber = 1010, method = "spline")
  )

  duplicate_axis <- x
  duplicate_axis$wavenumber[2L] <- duplicate_axis$wavenumber[1L]
  expect_error(
    point_intensity(duplicate_axis, wavenumber = 1000),
    "unique values"
  )

  nonfinite_axis <- x
  nonfinite_axis$wavenumber[2L] <- NA_real_
  expect_error(
    point_intensity(nonfinite_axis, wavenumber = 1000),
    "finite values"
  )
})
