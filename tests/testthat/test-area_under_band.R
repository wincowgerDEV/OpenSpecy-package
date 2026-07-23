data("raman_hdpe")

test_that("area_under_band() throws an errors", {
  # Create a non-OpenSpecy object
  area_under_band(x = "d", min = 100, max = 1000) |>
    expect_error()
    
    area_under_band(x = raman_hdpe) |>
        expect_error()
})

test_that("area_under_band() produces expected values", {
    # Create a non-OpenSpecy object
    expect_gt(area_under_band(x = raman_hdpe, min = 2700, max = 3000), area_under_band(x = raman_hdpe, min = 1500, max = 2000))
    expect_identical(unname(area_under_band(x = raman_hdpe, min = 2700, max = 3000)), 21421)
    expect_gt(1, area_under_band(x = raman_hdpe, min = 1500, max = 1600)/area_under_band(x = raman_hdpe, min = 1400, max = 1500))
    expect_gt(area_under_band(x = raman_hdpe, min = 1400, max = 1500)/ area_under_band(x = raman_hdpe, min = 1500, max = 1600), 1)
})

index_bands <- list(
  carbonyl_index_saub = list(
    numerator = c(1650, 1850), denominator = c(1420, 1500)
  ),
  carbonyl_index_pe = list(
    numerator = c(1700, 1770), denominator = c(1423, 1495)
  ),
  hydroxyl_index_pe = list(
    numerator = c(3021, 3353), denominator = c(1467, 1504)
  ),
  hydroxyl_index_pp = list(
    numerator = c(3300, 3400), denominator = c(952, 986)
  ),
  carbon_oxygen_index_pe = list(
    numerator = c(924, 1197), denominator = c(2866, 2987)
  ),
  carbon_oxygen_index_pp = list(
    numerator = c(1000, 1200), denominator = c(2885, 2940)
  )
)

test_that("area_under_band() calculates every named area ratio", {
  wavenumber <- 900:3400
  spectra <- cbind(
    second = seq_along(wavenumber) / 10,
    first = rev(seq_along(wavenumber)) / 7 - 2
  )
  x <- as_OpenSpecy(wavenumber, spectra)

  for (index in names(index_bands)) {
    bands <- index_bands[[index]]
    numerator <- colSums(spectra[
      wavenumber >= bands$numerator[1L] &
        wavenumber <= bands$numerator[2L], , drop = FALSE
    ])
    denominator <- colSums(spectra[
      wavenumber >= bands$denominator[1L] &
        wavenumber <= bands$denominator[2L], , drop = FALSE
    ])

    result <- area_under_band(x, index = index)
    expect_equal(result, numerator / denominator, info = index)
    expect_named(result, c("second", "first"), info = index)
  }
})

test_that("area_under_band() validates custom and named modes", {
  expect_error(
    area_under_band(raman_hdpe, min = 1000),
    "both required"
  )
  expect_error(
    area_under_band(raman_hdpe, min = 1000, max = 2000,
                    index = "carbonyl_index_saub"),
    "must be omitted"
  )
  expect_error(
    area_under_band(raman_hdpe,
                    index = c("carbonyl_index_saub", "carbonyl_index_pe")),
    "one supported"
  )
  expect_error(
    area_under_band(raman_hdpe, index = "unknown_index"),
    "Unsupported"
  )
  expect_error(
    area_under_band(raman_hdpe, min = 2000, max = 1000),
    "min <= max"
  )
})

test_that("named area ratios require full band coverage", {
  x <- as_OpenSpecy(
    1000:3000,
    cbind(alpha = rep(1, 2001), beta = rep(2, 2001))
  )

  expect_warning(
    result <- area_under_band(x, index = "hydroxyl_index_pe"),
    "does not fully cover"
  )
  expect_identical(
    result,
    stats::setNames(c(NA_real_, NA_real_), c("alpha", "beta"))
  )
})

test_that("named area ratios replace invalid results with named NA values", {
  wavenumber <- 900:3400
  spectra <- matrix(0, nrow = length(wavenumber), ncol = 2L,
                    dimnames = list(NULL, c("zero", "nonfinite")))
  numerator <- wavenumber >= 1650 & wavenumber <= 1850
  denominator <- wavenumber >= 1420 & wavenumber <= 1500
  spectra[numerator, ] <- 1
  spectra[denominator, "nonfinite"] <- Inf
  x <- as_OpenSpecy(wavenumber, spectra)

  expect_warning(
    result <- area_under_band(x, index = "carbonyl_index_saub"),
    "zero or non-finite"
  )
  expect_named(result, c("zero", "nonfinite"))
  expect_true(all(is.na(result)))
  expect_false(any(is.infinite(result)))
})

test_that("named area ratios preserve signed intensities", {
  wavenumber <- 900:3400
  values <- rep(0, length(wavenumber))
  values[wavenumber >= 1650 & wavenumber <= 1850] <- 1
  values[wavenumber >= 1420 & wavenumber <= 1500] <- -1
  x <- as_OpenSpecy(wavenumber, cbind(signed = values))

  result <- area_under_band(x, index = "carbonyl_index_saub")
  expect_lt(unname(result), 0)
})

