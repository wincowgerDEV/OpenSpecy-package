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

test_that("full assessment reports passes, warnings, and errors", {
  clean <- make_assess_test_spec()
  full <- assess_spec(
    clean,
    checks = c("negative_intensity", "missing_values"),
    report = "all"
  )
  expect_equal(nrow(full), 2L)
  expect_true(all(full$status == "pass"))
  expect_true(all(c("scope", "status", "finding_count", "regions",
                    "correction_applied", "correction_summary", "test_id") %in%
                  names(full)))
  expect_equal(length(unique(full$test_id)), nrow(full))
  expect_equal(
    full$value[full$check == "missing_values"],
    0
  )
  expect_true(is.finite(
    full$value[full$check == "negative_intensity"]
  ))
  expect_true(all(nzchar(full$metric)))

  damaged <- clean
  damaged$spectra[1, 1] <- NA_real_
  damaged$spectra[2, 1] <- -1
  damaged$spectra[3, 1] <- NA_real_
  full_damaged <- assess_spec(
    damaged,
    checks = c("negative_intensity", "missing_values"),
    report = "all"
  )
  expect_setequal(full_damaged$status, c("warning", "error"))
  expect_equal(
    full_damaged$finding_count[full_damaged$check == "missing_values"],
    2L
  )
  expect_equal(
    full_damaged$finding_count[full_damaged$check == "negative_intensity"],
    1L
  )
})

test_that("assess_spec() uses shared saturation detection and diagnostics", {
  axis <- 0:10
  values <- c(1, 2, 4, 8, 10, 10, 8, 4, 2, 1, 0)
  os <- as_OpenSpecy(axis, data.frame(sample = values))
  issues <- assess_spec(os, checks = "saturation", saturation = "auto")
  expect_equal(issues$check, "saturation")
  expect_equal(issues$value, 1)
  expect_equal(c(issues$region_min, issues$region_max), c(4, 5))

  corrected <- restrict_range(
    os, saturation = "auto", saturation_guard = 0, make_rel = FALSE
  )
  corrected_report <- assess_spec(
    corrected, checks = "saturation", saturation = "auto", report = "all"
  )
  expect_identical(corrected_report$status, "pass")
  expect_true(corrected_report$correction_applied)
  expect_equal(corrected_report$finding_count, 1L)
  expect_equal(nrow(corrected_report$regions[[1L]]), 1L)

  too_wide <- os
  too_wide$spectra[, 1] <- c(0, rep(10, 9), 0)
  expect_warning(
    rejected <- restrict_range(too_wide, saturation = 10,
                               saturation_guard = 0, make_rel = FALSE,
                               max_saturation_loss = 0.7)
  )
  full <- assess_spec(rejected, checks = "saturation", saturation = 10,
                      report = "all")
  expect_true(any(full$scope == "batch" & full$status == "error"))
  expect_true(any(full$test_id == "batch:batch:saturation"))

  batch <- as_OpenSpecy(
    axis,
    data.frame(
      clipped = values,
      clean = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
    )
  )
  batch_corrected <- restrict_range(
    batch, saturation = "auto", saturation_guard = 0, make_rel = FALSE
  )
  batch_report <- assess_spec(
    batch_corrected, checks = "saturation", report = "all"
  )
  expect_true(all(batch_report$status == "pass"))
  expect_true(all(batch_report$correction_applied))
  expect_true(all(batch_report$finding_count == 1L))
})

test_that("assess_spec() can select exact breakpoint SNR", {
  axis <- seq_len(10)
  os <- as_OpenSpecy(axis, data.frame(sample = c(rep(1, 8), 10, 10)))
  expect_equal(
    nrow(assess_spec(os, checks = "low_snr", snr_metric = "breakpoint_snr",
                     snr_threshold = 9)),
    0L
  )
  expect_equal(
    nrow(assess_spec(os, checks = "low_snr", snr_metric = "breakpoint_snr",
                     snr_threshold = 11)),
    1L
  )

  zero_noise <- as_OpenSpecy(
    axis, data.frame(sample = c(rep(0, 9), 10))
  )
  zero_noise_report <- assess_spec(
    zero_noise, checks = "low_snr", snr_metric = "breakpoint_snr",
    report = "all"
  )
  expect_identical(zero_noise_report$status, "pass")
  expect_identical(zero_noise_report$value, Inf)
})

test_that("full assessment preserves disjoint detector regions", {
  axis <- 0:10
  values <- c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0, 0)
  os <- as_OpenSpecy(axis, data.frame(sample = values))
  report <- assess_spec(
    os, checks = "saturation", saturation = 10, report = "all"
  )

  expect_identical(report$status, "warning")
  expect_equal(report$finding_count, 2L)
  expect_equal(
    report$regions[[1L]][, c("region_min", "region_max")],
    data.frame(region_min = c(1, 7), region_max = c(2, 8))
  )
})

test_that("full assessment never calls an all-missing check a pass", {
  os <- as_OpenSpecy(
    seq_len(30), data.frame(sample = rep(NA_real_, 30))
  )
  report <- suppressWarnings(assess_spec(
    os,
    checks = c("missing_values", "flat_spectrum", "negative_intensity",
               "low_snr", "spike", "saturation"),
    report = "all"
  ))

  expect_true(all(report$status == "error"))
  expect_true(all(report$issue[report$check != "missing_values"] ==
                    "Check unavailable"))

  short <- as_OpenSpecy(
    seq_len(10), data.frame(sample = seq_len(10))
  )
  short_report <- suppressWarnings(assess_spec(
    short, checks = "low_snr", report = "all"
  ))
  expect_identical(short_report$status, "error")
  expect_identical(short_report$issue, "Check unavailable")

  uncovered <- assess_spec(
    short, checks = "silent_region", report = "all"
  )
  expect_identical(uncovered$status, "error")
  expect_identical(uncovered$issue, "Check unavailable")
})

test_that("empty full assessment retains its schema", {
  os <- make_assess_test_spec()
  empty <- assess_spec(os, checks = character(), report = "all")
  reference <- assess_spec(os, checks = "missing_values", report = "all")

  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(reference))
})

test_that("spike assessment surfaces uncorrectable and rejected candidates", {
  values <- rep(0, 101)
  values[2] <- 50
  original <- as_OpenSpecy(seq_len(101), data.frame(sample = values))
  attempted <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 10,
    width_threshold = 4,
    interpolation_points = 5L
  )
  report <- assess_spec(
    attempted,
    checks = "spike",
    report = "all",
    spike_args = list(
      method = "prominence_fwhm",
      direction = "positive",
      prominence_threshold = 10,
      width_threshold = 4,
      interpolation_points = 5L
    )
  )

  expect_identical(report$status, "warning")
  expect_false(report$regions[[1L]]$correctable)
  expect_true("boundary_interval" %in% report$regions[[1L]]$reason)
  expect_match(report$correction_summary,
               "Spike correction was not applied")

  history_only <- assess_spec(
    attempted,
    checks = "spike",
    report = "all",
    spike_args = list(residual_threshold = 1e9)
  )
  expect_identical(history_only$status, "warning")
  expect_match(history_only$issue, "Previous spike correction")
  expect_true("boundary_interval" %in% history_only$regions[[1L]]$reason)

  too_few <- as_OpenSpecy(
    seq_len(9), data.frame(sample = c(0, 1, 0, 1, 0, 20, 0, 1, 0))
  )
  unavailable <- assess_spec(
    too_few,
    checks = "spike",
    report = "all",
    spike_args = list(
      method = "prominence_fwhm_ratio",
      direction = "positive",
      min_peaks = 20L,
      interpolation_points = 1L
    )
  )
  expect_identical(unavailable$status, "error")
  expect_identical(unavailable$issue, "Check unavailable")
})
