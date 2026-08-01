test_that("flatten_range() error handling", {
  test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))

  expect_s3_class(test, "OpenSpecy")
  expect_true(check_OpenSpecy(test))

  expect_error(flatten_range(test))
  expect_error(flatten_range(test, min = c(1000),
                             max = c(2000, 3000)))
  expect_error(flatten_range(test, min = c(2000), max = c(1000)))
})

test_that("restrict_range() provides correct range", {
  test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
                             spectra = data.table(intensity = rnorm(361)))
  single_range <- restrict_range(test_noise, min = 1000,
                                 max = 2000) |>
    expect_silent()

  double_range <- restrict_range(test_noise, min = c(1000, 2000),
                                 max = c(1500, 2500)) |>
    expect_silent()

  check_OpenSpecy(single_range) |> expect_true()
  check_OpenSpecy(double_range) |> expect_true()

  expect_identical(single_range$wavenumber, seq(1000,2000, by = 10))
  expect_identical(double_range$wavenumber, c(seq(1000,1500, by = 10),
                                              seq(2000,2500, by = 10)))
  expect_error(restrict_range(test_noise, min = 5000, max = 6000),
               "do not overlap")
})

test_that("flatten_range() function test", {
  sam <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
  flat_sam <- flatten_range(sam, min = c(4, 7), max = c(5, 10),
                             make_rel = F) |>
    expect_silent()

  expect_true(check_OpenSpecy(flat_sam))

  expect_equal(flat_sam$spectra[4:5, "V1"], c(4.5, 4.5))
  expect_equal(flat_sam$spectra[7:10, "V1"], c(8.5, 8.5, 8.5, 8.5))

  data("raman_hdpe")
  flat_hdpe <- flatten_range(raman_hdpe, min = c(500, 1000),
                             max = c(700, 1500)) |>
    expect_silent()
  expect_true(check_OpenSpecy(flat_hdpe))

  expect_equal(flat_hdpe$spectra[1:50, "intensity"],
               make_rel(raman_hdpe$spectra[, "intensity"])[1:50])
  expect_equal(flat_hdpe$spectra[60:100, "intensity"] |> unique() |> round(6),
               0.036709)

  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
  flat_map <- flatten_range(tiny_map, min = c(1000, 2000),
                            max = c(1200, 2400), make_rel = F) |>
    expect_silent()
  expect_true(check_OpenSpecy(flat_map))

  expect_false(all.equal(flat_map$spectra, tiny_map$spectra) |> isTRUE())
  expect_equal(flat_map$spectra[1:20, ], tiny_map$spectra[1:20, ])

  flat_map$spectra[40:60, 1:5] |> unique() |> round(2) |> as.numeric() |>
    expect_equal(c(-0.87, -1.25, -0.83, -1.19, -0.79))
})

make_automatic_range_spec <- function(left = 0L, right = 0L, co2 = FALSE,
                                      second = FALSE) {
  wavenumber <- seq(1000, 2500, by = 10)
  one <- rep(0, length(wavenumber))
  one[wavenumber == 1200] <- 2
  if (left > 0L) one[seq_len(left)] <- 10
  if (right > 0L) one[seq.int(length(one) - right + 1L, length(one))] <- 10
  if (co2) one[wavenumber == 2300] <- 10
  spectra <- data.frame(one = one)
  if (second) {
    two <- rep(0, length(wavenumber))
    two[wavenumber == 1200] <- 2
    two[(length(two) - 2L):length(two)] <- 10
    spectra$two <- two
  }
  as_OpenSpecy(x = wavenumber, spectra = spectra)
}

test_that("automated range functions are exact no-ops when checks pass", {
  clean <- make_automatic_range_spec()

  expect_identical(
    restrict_range(clean, automate = TRUE, make_rel = FALSE),
    clean
  )
  expect_identical(
    flatten_range(clean, automate = TRUE, make_rel = FALSE),
    clean
  )
})

test_that("automated flattening only corrects a flagged CO2 region", {
  os <- make_automatic_range_spec(co2 = TRUE)
  attr(os, "example_attribute") <- "preserved"
  corrected <- flatten_range(os, automate = TRUE, make_rel = FALSE)

  expect_false(identical(corrected$spectra, os$spectra))
  expect_true(attr(corrected, "automatic_flatten")$applied)
  expect_identical(attr(corrected, "example_attribute"), "preserved")
  expect_equal(
    nrow(assess_spec(corrected, checks = "co2_region")),
    0L
  )
})

test_that("automated restriction finds minimal shared batch bounds", {
  os <- make_automatic_range_spec(left = 2L, second = TRUE)
  corrected <- restrict_range(os, automate = TRUE, make_rel = FALSE)

  expect_equal(min(corrected$wavenumber), 1020)
  expect_equal(max(corrected$wavenumber), 2470)
  expect_true(attr(corrected, "automatic_tail")$applied)
  expect_equal(ncol(corrected$spectra), 2L)
  expect_equal(nrow(corrected$metadata), 2L)
  expect_equal(nrow(assess_spec(corrected, checks = "high_tail")), 0L)
})

test_that("automated restriction is transactional at 20 percent", {
  os <- make_automatic_range_spec()
  os$spectra[seq_len(32L), "one"] <- 4^(32:1)
  corrected <- restrict_range(os, automate = TRUE, make_rel = FALSE)

  expect_identical(corrected$wavenumber, os$wavenumber)
  expect_identical(corrected$spectra, os$spectra)
  expect_false(attr(corrected, "automatic_tail")$applied)
  expect_identical(attr(corrected, "automatic_tail")$reason,
                   "max_crop_exceeded")
})

test_that("automated range arguments are validated", {
  os <- make_automatic_range_spec(left = 2L)
  expect_error(restrict_range(os, min = 1000, max = 2000,
                              automate = TRUE), "either")
  expect_error(restrict_range(os, automate = TRUE, artifact_ratio = 1),
               "greater than 1")
  expect_error(flatten_range(os, min = c(1000, 2200),
                             max = c(1100, 2400), automate = TRUE),
               "one flattening range")
})

make_saturated_range_spec <- function(first = 3:4, second = 7:8) {
  axis <- 0:10
  one <- seq_along(axis)
  two <- rev(one)
  one[first] <- 100
  two[second] <- 100
  as_OpenSpecy(axis, data.frame(one = one, two = two))
}

test_that("saturation restriction removes one shared union", {
  os <- make_saturated_range_spec()
  attr(os, "example_attribute") <- "preserved"
  restricted <- restrict_range(
    os, saturation = 100, saturation_guard = 0,
    make_rel = FALSE, max_saturation_loss = 0.7
  )

  expect_true(attr(restricted, "saturation_restriction")$applied)
  expect_equal(restricted$wavenumber, setdiff(os$wavenumber, c(2, 3, 6, 7)))
  expect_equal(ncol(restricted$spectra), 2L)
  expect_equal(nrow(restricted$metadata), 2L)
  expect_identical(attr(restricted, "example_attribute"), "preserved")
  diagnostic <- attr(restricted, "saturation_restriction")
  expect_equal(diagnostic$detected_interval_count, 2L)
  expect_equal(diagnostic$excluded_interval_count, 2L)
  expect_equal(diagnostic$affected_spectrum_count, 2L)
  expect_equal(diagnostic$retained_points, nrow(restricted$spectra))
  expect_equal(
    diagnostic$axis_signature,
    digest::digest(restricted$wavenumber, algo = "md5")
  )
  expect_true(nrow(diagnostic$retained_ranges) > 0L)
  expect_true(check_OpenSpecy(restricted))
})

test_that("automatic saturation detection is conservative", {
  axis <- 0:10
  rounded <- c(1, 2, 4, 8, 10, 9.999, 8, 4, 2, 1, 0)
  hard <- c(1, 2, 4, 8, 10, 10, 8, 4, 2, 1, 0)
  broad <- c(0, 2, 5, 8, 10, 10, 10, 8, 5, 2, 0)
  edge <- c(10, 10, 8, 4, 2, 1, 0, 0, 0, 0, 0)
  os <- as_OpenSpecy(axis, data.frame(rounded = rounded, hard = hard,
                                      broad = broad, edge = edge))
  detection <- OpenSpecy:::.detect_saturation(os, "auto", tolerance = 1e-8)

  expect_identical(unique(detection$regions$spectrum_id), "hard")
  restricted <- restrict_range(os, saturation = "auto", saturation_guard = 0,
                               make_rel = FALSE)
  expect_equal(restricted$wavenumber, setdiff(axis, c(4, 5)))

  clean <- as_OpenSpecy(axis, data.frame(sample = seq_along(axis)))
  clean$metadata[, file_id := NULL]
  expect_true(check_OpenSpecy(clean))
  expect_identical(
    restrict_range(clean, saturation = "auto", make_rel = FALSE),
    clean
  )
})

test_that("saturation loss boundary is inclusive and rollback preserves data", {
  axis <- 0:10
  exact <- rep(0, length(axis))
  exact[3:9] <- 10
  os <- as_OpenSpecy(axis, data.frame(sample = exact))

  accepted <- restrict_range(os, saturation = 10, saturation_guard = 0,
                             make_rel = FALSE, max_saturation_loss = 0.70)
  expect_true(attr(accepted, "saturation_restriction")$applied)
  expect_equal(attr(accepted, "saturation_restriction")$
                 saturation_loss_fraction, 0.70)

  decimal_axis <- seq(0, 1, length.out = 11)
  decimal <- as_OpenSpecy(decimal_axis, data.frame(sample = exact))
  decimal_accepted <- restrict_range(
    decimal, saturation = 10, saturation_guard = 0, make_rel = FALSE,
    max_saturation_loss = 0.70
  )
  expect_true(attr(decimal_accepted, "saturation_restriction")$applied)

  too_wide <- os
  too_wide$spectra[2, 1] <- 10
  expect_warning(
    rejected <- restrict_range(too_wide, saturation = 10,
                               saturation_guard = 0, make_rel = FALSE,
                               max_saturation_loss = 0.70),
    "interpretation may be unreliable"
  )
  expect_identical(rejected$wavenumber, too_wide$wavenumber)
  expect_identical(rejected$spectra, too_wide$spectra)
  expect_false(attr(rejected, "saturation_restriction")$applied)
  expect_identical(attr(rejected, "saturation_restriction")$reason,
                   "exceeds_max_saturation_loss")
  rejected_diagnostic <- attr(rejected, "saturation_restriction")
  expect_equal(rejected_diagnostic$retained_points,
               nrow(rejected$spectra))
  expect_equal(rejected_diagnostic$saturation_loss_fraction, 0)
  expect_gt(rejected_diagnostic$proposed_saturation_loss_fraction, 0.70)
  expect_equal(nrow(rejected_diagnostic$excluded_ranges), 0L)
  expect_gt(nrow(rejected_diagnostic$proposed_excluded_ranges), 0L)
  expect_equal(
    rejected_diagnostic$axis_signature,
    digest::digest(rejected$wavenumber, algo = "md5")
  )
})

test_that("shared saturation diagnostics distinguish detection from impact", {
  axis <- 0:10
  spectra <- data.frame(
    clipped = c(1, 2, 4, 8, 10, 10, 8, 4, 2, 1, 0),
    clean_one = seq_along(axis),
    clean_two = rev(seq_along(axis))
  )
  os <- as_OpenSpecy(axis, spectra)
  restricted <- restrict_range(
    os, saturation = "auto", saturation_guard = 0, make_rel = FALSE
  )
  diagnostic <- attr(restricted, "saturation_restriction")

  expect_identical(diagnostic$detected_spectra, "clipped")
  expect_setequal(diagnostic$affected_spectra, names(spectra))
  expect_equal(diagnostic$detected_spectrum_count, 1L)
  expect_equal(diagnostic$affected_spectrum_count, 3L)
})

test_that("automatic saturation does not join separate manual ranges", {
  axis <- 0:10
  values <- c(1, 2, 3, 10, 4, 4, 4, 10, 3, 2, 1)
  os <- as_OpenSpecy(axis, data.frame(sample = values))
  restricted <- restrict_range(
    os,
    min = c(0, 7), max = c(3, 10),
    saturation = "auto", saturation_guard = 0, make_rel = FALSE
  )

  expect_identical(restricted$wavenumber, c(0:3, 7:10))
  expect_null(attr(restricted, "saturation_restriction"))

  boundary_values <- c(1, 2, 3, 4, 4, 4, 4, 10, 10, 3, 2)
  boundary <- as_OpenSpecy(axis, data.frame(sample = boundary_values))
  boundary_restricted <- restrict_range(
    boundary,
    min = c(0, 7), max = c(3, 10),
    saturation = "auto", saturation_guard = 0, make_rel = FALSE
  )
  expect_identical(boundary_restricted$wavenumber, c(0:3, 7:10))
  expect_null(attr(boundary_restricted, "saturation_restriction"))

  numeric_values <- c(1, 2, 10, 10, 4, 4, 4, 3, 2, 1, 0)
  numeric <- as_OpenSpecy(axis, data.frame(sample = numeric_values))
  numeric_restricted <- restrict_range(
    numeric,
    min = c(0, 7), max = c(3, 10),
    saturation = 10, saturation_guard = 0, make_rel = FALSE,
    max_saturation_loss = 0.30
  )
  numeric_diagnostic <- attr(numeric_restricted, "saturation_restriction")
  expect_true(numeric_diagnostic$applied)
  expect_equal(numeric_diagnostic$saturation_loss_fraction, 0.25)

  edge_values <- c(1, 2, 3, 10, 4, 4, 4, 3, 2, 1, 0)
  edge_numeric <- as_OpenSpecy(axis, data.frame(sample = edge_values))
  edge_restricted <- restrict_range(
    edge_numeric,
    min = c(0, 7), max = c(3, 10),
    saturation = 10, saturation_guard = 1L, make_rel = FALSE
  )
  expect_identical(edge_restricted$wavenumber, c(0:1, 7:10))
  expect_equal(
    nrow(attr(edge_restricted, "saturation_restriction")$excluded_ranges),
    1L
  )
})

test_that("saturation loss uses irregular-axis cell coverage", {
  axis <- c(0, 1, 2, 3, 4, 100)
  values <- c(1, 2, 3, 4, 10, 10)
  os <- as_OpenSpecy(axis, data.frame(sample = values))

  expect_warning(
    restricted <- restrict_range(
      os, saturation = 10, saturation_guard = 0, make_rel = FALSE,
      max_saturation_loss = 0.70
    ),
    "interpretation may be unreliable"
  )
  diagnostic <- attr(restricted, "saturation_restriction")
  expect_identical(restricted$wavenumber, axis)
  expect_gt(diagnostic$proposed_saturation_loss_fraction, 0.95)
})

test_that("saturation arguments are validated", {
  os <- make_saturated_range_spec()
  expect_error(restrict_range(os, saturation = "possible"), "must be")
  expect_error(restrict_range(os, saturation = Inf), "must be")
  expect_error(restrict_range(os, saturation = 100, saturation_guard = -1),
               "non-negative")
  expect_error(restrict_range(os, saturation = 100,
                              max_saturation_loss = 1.1), "in [0, 1]",
               fixed = TRUE)
  expect_error(restrict_range(os, automate = TRUE, saturation = "auto"),
               "separate stages")
})
