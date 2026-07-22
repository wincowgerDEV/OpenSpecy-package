make_assess_test_spec <- function(values = NULL) {
  wavenumber <- seq(1000, 2500, by = 10)
  if (is.null(values)) {
    values <- rep(1, length(wavenumber))
    values[wavenumber == 1200] <- 10
  }

  as_OpenSpecy(
    x = wavenumber,
    spectra = data.frame(sample = values)
  )
}

test_that("assess_spec() handles input errors correctly", {
  assess_spec(1:1000) |> expect_error()
  assess_spec(make_assess_test_spec(), checks = "not_a_check") |>
    expect_error()
})

test_that("assess_spec() returns an empty issue table for clean spectra", {
  res <- assess_spec(make_assess_test_spec()) |> expect_silent()

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 0)
  expect_equal(
    names(res),
    c("spectrum_index", "spectrum_id", "check", "issue", "description",
      "likely_cause", "potential_fix", "metric", "value", "threshold",
      "candidate_max", "control_max", "region_min", "region_max")
  )
})

test_that("assess_spec() finds high tail values", {
  os <- make_assess_test_spec()
  os$spectra[1:5, "sample"] <- 40
  os$spectra[(nrow(os$spectra) - 4):nrow(os$spectra), "sample"] <- 40

  res <- assess_spec(os, checks = "high_tail") |> expect_silent()

  expect_equal(nrow(res), 1)
  expect_equal(res$check, "high_tail")
  expect_equal(res$spectrum_id, "sample")
  expect_equal(res$metric, "artifact_max_ratio")
  expect_gte(res$value, res$threshold)
  expect_gt(res$candidate_max, res$control_max)
})

test_that("assess_spec() finds high silent and CO2 regions", {
  silent <- make_assess_test_spec()
  silent$spectra[silent$wavenumber == 1900, "sample"] <- 12
  silent_res <- assess_spec(silent, checks = "silent_region") |>
    expect_silent()

  expect_equal(silent_res$check, "silent_region")
  expect_equal(silent_res$region_min, 1800)
  expect_equal(silent_res$region_max, 2000)

  co2 <- make_assess_test_spec()
  co2$spectra[co2$wavenumber == 2300, "sample"] <- 40
  co2_res <- assess_spec(co2, checks = "co2_region") |>
    expect_silent()

  expect_equal(co2_res$check, "co2_region")
  expect_equal(co2_res$region_min, 2200)
  expect_equal(co2_res$region_max, 2420)
})

test_that("assess_spec() reports only affected spectra in multispectrum input", {
  clean <- make_assess_test_spec()$spectra[, "sample"]
  tail <- clean
  tail[4] <- 40
  os <- as_OpenSpecy(
    x = seq(1000, 2500, by = 10),
    spectra = data.frame(clean = clean, tail = tail)
  )

  res <- assess_spec(os, checks = "high_tail") |> expect_silent()

  expect_equal(nrow(res), 1)
  expect_equal(res$spectrum_id, "tail")
  expect_equal(res$spectrum_index, 2L)
})

test_that("assess_spec() flags core extra QC issues", {
  missing <- make_assess_test_spec()
  missing$spectra[1:3, "sample"] <- c(NA, NaN, Inf)
  missing_res <- assess_spec(missing, checks = "missing_values") |>
    expect_silent()
  expect_equal(missing_res$check, "missing_values")
  expect_equal(missing_res$value, 3)

  flat <- make_assess_test_spec(rep(1, length(seq(1000, 2500, by = 10))))
  flat_res <- assess_spec(flat, checks = "flat_spectrum") |>
    expect_silent()
  expect_equal(flat_res$check, "flat_spectrum")

  negative <- make_assess_test_spec()
  negative$spectra[10, "sample"] <- -0.1
  negative_res <- assess_spec(negative, checks = "negative_intensity") |>
    expect_silent()
  expect_equal(negative_res$check, "negative_intensity")
  expect_lt(negative_res$value, negative_res$threshold)

  low_snr <- make_assess_test_spec(rep(1, length(seq(1000, 2500, by = 10))))
  low_snr_res <- assess_spec(low_snr, checks = "low_snr") |>
    expect_silent()
  expect_equal(low_snr_res$check, "low_snr")
  expect_lt(low_snr_res$value, low_snr_res$threshold)
})

test_that("assess_spec() respects check parameters", {
  os <- make_assess_test_spec()
  os$spectra[4, "sample"] <- 40

  assess_spec(os, checks = "high_tail", tail_n = 5) |>
    nrow() |>
    expect_equal(1)
  assess_spec(os, checks = "high_tail", tail_n = 3) |>
    nrow() |>
    expect_equal(0)
  assess_spec(os, checks = "high_tail", artifact_ratio = 100) |>
    nrow() |>
    expect_equal(0)

  snr <- make_assess_test_spec()
  assess_spec(snr, checks = "low_snr", snr_threshold = 4) |>
    nrow() |>
    expect_equal(0)
  assess_spec(snr, checks = "low_snr", snr_threshold = 11) |>
    nrow() |>
    expect_equal(1)
})

test_that("artifact checks use the normalized ratio boundary", {
  wavenumber <- seq(1000, 2500, by = 10)
  values <- rep(0, length(wavenumber))
  values[wavenumber == 1200] <- 1
  values[1] <- 2.999
  below <- as_OpenSpecy(x = wavenumber, spectra = data.frame(sample = values))
  expect_equal(nrow(assess_spec(below, checks = "high_tail")), 0)

  values[1] <- 3
  boundary <- as_OpenSpecy(
    x = wavenumber,
    spectra = data.frame(sample = values)
  )
  result <- assess_spec(boundary, checks = "high_tail")
  expect_equal(result$value, 3, tolerance = 1e-12)
  expect_equal(result$threshold, 3)
})

test_that("artifact checks do not classify unstructured noise", {
  wavenumber <- seq(1000, 2500, by = 10)
  noise <- rep(c(0.2, 0.8, 0.5, 1), length.out = length(wavenumber))
  os <- as_OpenSpecy(x = wavenumber, spectra = data.frame(noise = noise))

  result <- assess_spec(os, checks = c("high_tail", "co2_region"))
  expect_equal(nrow(result), 0)
})

test_that("CO2 and tail checks do not mask one another", {
  wavenumber <- seq(1000, 2500, by = 10)
  values <- rep(0, length(wavenumber))
  values[wavenumber == 1200] <- 2
  values[1] <- 9
  values[wavenumber == 2300] <- 12
  os <- as_OpenSpecy(x = wavenumber, spectra = data.frame(sample = values))

  result <- assess_spec(os, checks = c("high_tail", "co2_region"))
  expect_setequal(result$check, c("high_tail", "co2_region"))
  expect_true(all(result$value >= 3))
})

test_that("flat spectra do not produce infinite artifact findings", {
  os <- make_assess_test_spec(rep(0, length(seq(1000, 2500, by = 10))))
  result <- assess_spec(os, checks = c("high_tail", "co2_region"))
  expect_equal(nrow(result), 0)
})
