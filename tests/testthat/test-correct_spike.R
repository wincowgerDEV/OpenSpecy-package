make_spike_test_spec <- function(values, axis = seq_along(values),
                                 id = "sample") {
  spectra <- data.frame(values)
  names(spectra) <- id
  as_OpenSpecy(axis, spectra)
}

test_that("correct_spike() validates dispatch and method-specific inputs", {
  expect_error(correct_spike(1:20), "OpenSpecy")

  clean <- make_spike_test_spec(seq_len(51))
  expect_error(
    correct_spike(clean, method = "prominence_fwhm"),
    "prominence_threshold.*width_threshold"
  )
  expect_error(correct_spike(clean, residual_window = 0),
               "residual_window")
  expect_error(correct_spike(clean, rel_height = 1.1), "rel_height")
  expect_error(
    correct_spike(clean, threshold = 5),
    "unused argument.*'threshold'"
  )
})

test_that("residual correction handles both spike signs and is idempotent", {
  axis <- seq(400, 1800, length.out = 101)
  baseline <- sin(axis / 200)
  positive <- negative <- baseline
  positive[51] <- positive[51] + 20
  negative[61] <- negative[61] - 20
  original <- as_OpenSpecy(
    axis,
    data.frame(positive = positive, negative = negative)
  )
  attr(original, "source_tag") <- "residual fixture"

  corrected <- correct_spike(original, interpolation_points = 5L)
  diagnostic <- attr(corrected, "automatic_spike")

  expect_true(diagnostic$applied)
  expect_identical(diagnostic$method, "residual")
  expect_setequal(diagnostic$affected_spectra, c("positive", "negative"))
  expect_equal(diagnostic$before_count, 2L)
  expect_equal(diagnostic$after_count, 0L)
  expect_equal(unname(corrected$spectra[51, "positive"]), baseline[51],
               tolerance = 0.01)
  expect_equal(unname(corrected$spectra[61, "negative"]), baseline[61],
               tolerance = 0.01)
  expect_identical(
    correct_spike(corrected, interpolation_points = 5L),
    corrected
  )
})

test_that("descending axes preserve orientation for positive and negative residuals", {
  descending_axis <- rev(seq(400, 1800, length.out = 121))
  baseline <- 2 + 0.01 * descending_axis
  cases <- list(
    list(direction = "positive", delta = 30),
    list(direction = "negative", delta = -30)
  )

  for (case in cases) {
    values <- baseline
    values[61] <- values[61] + case$delta
    # as_OpenSpecy() canonicalizes new input; reverse a valid object explicitly
    # to exercise the orientation accepted from existing OpenSpecy workflows.
    original <- as_OpenSpecy(
      rev(descending_axis),
      data.frame(sample = rev(values))
    )
    original$wavenumber <- rev(original$wavenumber)
    original$spectra <- original$spectra[
      rev(seq_len(nrow(original$spectra))), , drop = FALSE
    ]
    attr(original, "axis_source") <- "descending fixture"

    corrected <- correct_spike(
      original,
      direction = case$direction,
      interpolation_points = 5L
    )
    diagnostic <- attr(corrected, "automatic_spike")

    expect_true(all(diff(corrected$wavenumber) < 0))
    expect_identical(corrected$wavenumber, original$wavenumber)
    expect_identical(corrected$metadata, original$metadata)
    expect_identical(dimnames(corrected$spectra),
                     dimnames(original$spectra))
    expect_identical(attr(corrected, "axis_source"), "descending fixture")
    expect_equal(as.numeric(corrected$spectra[, 1]), baseline,
                 tolerance = 1e-12)
    expect_true(diagnostic$applied)
    expect_true(all(diagnostic$corrected_regions$region_min <=
                      diagnostic$corrected_regions$region_max))
    expect_identical(
      correct_spike(
        corrected,
        direction = case$direction,
        interpolation_points = 5L
      ),
      corrected
    )
  }
})

test_that("residual correction preserves the OpenSpecy contract", {
  axis <- cumsum(seq(0.5, 1.5, length.out = 121))
  baseline <- 2 + 0.03 * axis
  values <- baseline
  values[70] <- values[70] + 25
  original <- make_spike_test_spec(values, axis, id = "irregular")
  original$metadata$sample_note <- "kept"
  attr(original, "source_tag") <- list(owner = "test")
  original_attributes <- attributes(original)

  detection <- OpenSpecy:::.detect_spikes(
    original,
    interpolation_points = 5L
  )
  corrected <- correct_spike(original, interpolation_points = 5L)
  changed <- corrected$spectra != original$spectra
  changed[is.na(changed)] <- FALSE

  expect_identical(corrected$wavenumber, original$wavenumber)
  expect_identical(dim(corrected$spectra), dim(original$spectra))
  expect_identical(dimnames(corrected$spectra), dimnames(original$spectra))
  expect_identical(corrected$metadata, original$metadata)
  expect_identical(attr(corrected, "source_tag"),
                   original_attributes$source_tag)
  expect_identical(class(corrected), class(original))
  expect_false(any(changed & !detection$flagged))
  expect_equal(unname(corrected$spectra[70, 1]), baseline[70],
               tolerance = 1e-12)
})

test_that("a clean spectrum is an exact no-op", {
  axis <- cumsum(seq(0.8, 1.2, length.out = 101))
  clean <- make_spike_test_spec(3 + 0.01 * axis, axis)
  attr(clean, "custom") <- "unchanged"

  expect_identical(correct_spike(clean, interpolation_points = 5L), clean)
  expect_null(attr(clean, "automatic_spike"))
})

test_that("the detector exposes a stable reusable result structure", {
  values <- rep(0, 101)
  values[51] <- 30
  detected <- OpenSpecy:::.detect_spikes(
    make_spike_test_spec(values),
    interpolation_points = 5L
  )

  expect_identical(
    names(detected),
    c("method", "parameters", "candidates", "flagged",
      "candidate_count", "correctable_count", "reason")
  )
  expect_identical(
    names(detected$candidates),
    c("spectrum_index", "spectrum_id", "direction", "peak_index",
      "peak_wavenumber", "start_index", "end_index", "region_min",
      "region_max", "residual", "score", "prominence", "width",
      "prominence_width_ratio", "correctable", "reason")
  )
  expect_identical(dim(detected$flagged), c(length(values), 1L))
  expect_equal(detected$correctable_count, 1L)
  expect_true(detected$flagged[51, 1])
})

test_that("the Coca-Lopez manual thresholds use sample-unit width", {
  axis <- seq_len(201)
  baseline <- sin(axis / 30)
  values <- baseline
  values[101] <- values[101] + 50
  original <- make_spike_test_spec(values, axis)

  detection <- OpenSpecy:::.detect_spikes(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 40,
    width_threshold = 4,
    rel_height = 0.8,
    interpolation_points = 5L
  )
  corrected <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 40,
    width_threshold = 4,
    rel_height = 0.8,
    interpolation_points = 5L
  )

  expect_equal(detection$candidates$peak_index, 101L)
  expect_lt(detection$candidates$width, 4)
  expect_gt(detection$candidates$prominence, 40)
  expect_equal(unname(corrected$spectra[101, 1]), baseline[101],
               tolerance = 0.01)
  expect_identical(attr(corrected, "automatic_spike")$parameters$rel_height,
                   0.8)
})

test_that("prominence/FWHM ratio mode applies the paper's upper Z rule", {
  axis <- seq_len(801)
  baseline <- sin(2 * pi * axis / 25) + 0.15 * sin(2 * pi * axis / 7)
  values <- baseline
  values[401] <- values[401] + 30
  original <- make_spike_test_spec(values, axis)

  detection <- OpenSpecy:::.detect_spikes(
    original,
    method = "prominence_fwhm_ratio",
    direction = "positive",
    min_peaks = 20L,
    interpolation_points = 5L
  )
  corrected <- correct_spike(
    original,
    method = "prominence_fwhm_ratio",
    direction = "positive",
    min_peaks = 20L,
    interpolation_points = 5L
  )

  expect_identical(detection$reason, "detected")
  expect_equal(detection$candidates$peak_index, 401L)
  expect_gt(detection$candidates$score, 3.5)
  expect_lt(abs(unname(corrected$spectra[401, 1]) - baseline[401]), 0.1)

  too_few <- OpenSpecy:::.detect_spikes(
    make_spike_test_spec(c(0, 1, 0, 1, 0, 20, 0, 1, 0)),
    method = "prominence_fwhm_ratio",
    direction = "positive",
    min_peaks = 20L,
    interpolation_points = 1L
  )
  expect_identical(too_few$reason, "insufficient_peaks")
  expect_equal(too_few$candidate_count, 0L)
})

test_that("adjacent paper spikes share clean interpolation neighbors", {
  values <- rep(5, 201)
  values[100:101] <- 105
  original <- make_spike_test_spec(values)

  corrected <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 40,
    width_threshold = 4,
    interpolation_points = 5L
  )
  diagnostic <- attr(corrected, "automatic_spike")

  expect_true(diagnostic$applied)
  expect_equal(corrected$spectra[100:101, 1], c(5, 5))
  expect_equal(diagnostic$corrected_regions$start_index, 100L)
  expect_equal(diagnostic$corrected_regions$end_index, 101L)
  expect_false(any(!is.finite(corrected$spectra)))
})

test_that("boundary intervals are rejected without wrapping", {
  values <- rep(0, 101)
  values[2] <- 50
  original <- make_spike_test_spec(values)

  corrected <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 10,
    width_threshold = 4,
    interpolation_points = 5L
  )
  diagnostic <- attr(corrected, "automatic_spike")

  expect_identical(corrected$spectra, original$spectra)
  expect_false(diagnostic$applied)
  expect_identical(diagnostic$reason, "no_correctable_regions")
  expect_true("boundary_interval" %in% diagnostic$rejected_regions$reason)
})

test_that("narrow real bands are rejected conservatively", {
  axis <- seq_len(201)
  narrow_band <- 100 * exp(-0.5 * ((axis - 101) / 1.5)^2)
  original <- make_spike_test_spec(narrow_band, axis)

  residual <- correct_spike(original, interpolation_points = 5L)
  residual_diagnostic <- attr(residual, "automatic_spike")
  expect_identical(residual$spectra, original$spectra)
  expect_false(residual_diagnostic$applied)
  expect_setequal(
    residual_diagnostic$rejected_regions$reason,
    c("candidate_too_wide", "spectral_band_shoulder")
  )

  calibrated <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 10,
    width_threshold = 1,
    interpolation_points = 5L
  )
  expect_identical(calibrated, original)
})

test_that("the Fig. 6-style broad band is not truncated", {
  axis <- seq_len(201)
  truth <- 100 * exp(-((axis - 101) / 18)^2)
  values <- truth
  values[110:112] <- values[110:112] + c(200, 400, 200)
  original <- make_spike_test_spec(values, axis)

  corrected <- correct_spike(
    original,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 40,
    width_threshold = 4,
    rel_height = 0.8,
    interpolation_points = 10L
  )

  expect_true(attr(corrected, "automatic_spike")$applied)
  expect_equal(corrected$spectra[110:112, 1], truth[110:112],
               tolerance = 1)
  expect_identical(corrected$spectra[-(110:112), 1],
                   original$spectra[-(110:112), 1])
})

test_that("correction preserves existing non-finite values and adds none", {
  axis <- seq_len(151)
  values <- sin(axis / 20)
  values[20] <- NA_real_
  values[90] <- values[90] + 30
  original <- make_spike_test_spec(values, axis)

  corrected <- correct_spike(original, interpolation_points = 5L)

  expect_identical(which(!is.finite(corrected$spectra)),
                   which(!is.finite(original$spectra)))
  expect_true(attr(corrected, "automatic_spike")$applied)
})
