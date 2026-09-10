.thermal_radiance_unit <- "W m^-2 sr^-1 (cm^-1)^-1"

.thermal_planck_reference <- function(wavenumber, temperature_k) {
  h <- 6.62607015e-34
  c <- 299792458
  k <- 1.380649e-23
  wavenumber_m <- wavenumber * 100

  2 * h * c^2 * wavenumber_m^3 * 100 /
    expm1(h * c * wavenumber_m / (k * temperature_k))
}

.thermal_trapezoid_weights <- function(x) {
  ord <- order(x)
  sorted <- x[ord]
  gaps <- diff(sorted)
  weights <- c(gaps[1] / 2,
               (gaps[-length(gaps)] + gaps[-1]) / 2,
               gaps[length(gaps)] / 2)
  out <- numeric(length(x))
  out[ord] <- weights
  out
}

.thermal_make_object <- function(wavenumber, spectra,
                                 x_coord = seq_len(ncol(spectra)),
                                 y_coord = rep(1, ncol(spectra))) {
  spectra <- as.data.frame(spectra, check.names = FALSE)
  n_spectra <- ncol(spectra)
  metadata <- data.frame(
    spectrum_identity = paste0("thermal_", seq_len(n_spectra)),
    intensity_units = rep(.thermal_radiance_unit, n_spectra),
    radiance_level = rep("surface_leaving", n_spectra),
    radiometric_calibration = rep("synthetic SI forward model", n_spectra)
  )

  as_OpenSpecy(
    x = wavenumber,
    spectra = spectra,
    metadata = metadata,
    coords = data.frame(x = x_coord, y = y_coord),
    attributes = list(
      intensity_unit = .thermal_radiance_unit,
      spectra_type = "ftir"
    ),
    compute_file_id = FALSE
  )
}

.thermal_forward_model <- function(wavenumber, temperature_k, emissivity,
                                   downwelling) {
  emissivity * .thermal_planck_reference(wavenumber, temperature_k) +
    (1 - emissivity) * downwelling
}

test_that("wavenumber Planck radiance has correct units and integral", {
  checkpoint <- OpenSpecy:::.planck_radiance_wavenumber(1000, 300)
  expect_equal(checkpoint, 0.0992403333007, tolerance = 1e-10)

  wavenumber <- seq(1, 10000, by = 1)
  radiance <- OpenSpecy:::.planck_radiance_wavenumber(wavenumber, 300)
  integral <- sum(diff(wavenumber) *
                    (radiance[-1] + radiance[-length(radiance)]) / 2)
  stefan_boltzmann_radiance <- 5.670374419e-8 * 300^4 / pi

  expect_equal(integral, stefan_boltzmann_radiance, tolerance = 2e-5)
  expect_equal(
    radiance,
    .thermal_planck_reference(wavenumber, 300),
    tolerance = 1e-12
  )
})

test_that("estimate_temperature recovers forward-modelled in-memory spectra", {
  wavenumber <- seq(760, 1240, by = 4)
  downwelling <- 0.85 * .thermal_planck_reference(wavenumber, 292)
  temperatures <- c(325, 335)
  emissivities <- c(0.76, 0.84)
  spectra <- vapply(seq_along(temperatures), function(i) {
    .thermal_forward_model(
      wavenumber, temperatures[i], emissivities[i], downwelling
    )
  }, numeric(length(wavenumber)))
  colnames(spectra) <- c("cool_particle", "warm_particle")
  object <- .thermal_make_object(
    wavenumber, spectra, x_coord = c(10, 11), y_coord = c(7, 7)
  )

  result <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = c(760, 1240),
    emissivity_stat = "mean"
  )
  one_at_a_time <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = c(760, 1240),
    emissivity_stat = "mean",
    block_size = 1
  )

  expect_s3_class(result, "data.table")
  expect_contains(
    names(result),
    c("spectrum_index", "spectrum_id", "x", "y",
      "estimated_material_temperature_k", "emissivity_value",
      "emissivity_statistic", "emissivity_roughness",
      "emissivity_physical_fraction", "valid_band_fraction", "status")
  )
  expect_identical(result$spectrum_index, 1:2)
  expect_identical(result$spectrum_id, colnames(object$spectra))
  expect_equal(result$x, c(10, 11))
  expect_equal(result$y, c(7, 7))
  expect_identical(result$status, rep("ok", 2))
  expect_identical(result$emissivity_statistic, rep("mean", 2))
  expect_equal(
    result$estimated_material_temperature_k,
    temperatures,
    tolerance = 0.5
  )
  expect_equal(result$emissivity_value, emissivities, tolerance = 0.005)
  expect_equal(result$emissivity_physical_fraction, rep(1, 2))
  expect_equal(result$valid_band_fraction, rep(1, 2))
  expect_equal(one_at_a_time, result, tolerance = 1e-12)
})

test_that("estimate_temperature returns each scalar emissivity summary", {
  z <- seq(0, 1, length.out = 121)^2
  wavenumber <- 760 + 480 * z
  emissivity <- 0.55 + 0.35 * z
  temperature <- 334
  downwelling <- 0.8 * .thermal_planck_reference(wavenumber, 291)
  radiance <- .thermal_forward_model(
    wavenumber, temperature, emissivity, downwelling
  )
  object <- .thermal_make_object(
    wavenumber, cbind(asymmetric = radiance)
  )

  stats <- c("planck_weighted", "mean", "median", "max")
  fits <- lapply(stats, function(statistic) {
    estimate_temperature(
      object,
      downwelling = downwelling,
      temperature_range_k = c(315, 350),
      fit_range_cm1 = range(wavenumber),
      emissivity_stat = statistic
    )
  })
  default_fit <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(315, 350),
    fit_range_cm1 = range(wavenumber)
  )

  expect_true(all(vapply(fits, function(x) x$status == "ok", logical(1))))
  expect_equal(
    vapply(fits, function(x) x$estimated_material_temperature_k, numeric(1)),
    rep(temperature, length(stats)),
    tolerance = 0.5
  )
  expect_identical(
    vapply(fits, function(x) x$emissivity_statistic, character(1)),
    stats
  )

  expected <- c(
    planck_weighted = sum(
      emissivity * .thermal_planck_reference(wavenumber, temperature) *
        .thermal_trapezoid_weights(wavenumber)
    ) / sum(
      .thermal_planck_reference(wavenumber, temperature) *
        .thermal_trapezoid_weights(wavenumber)
    ),
    mean = mean(emissivity),
    median = median(emissivity),
    max = max(emissivity)
  )
  observed <- vapply(fits, function(x) x$emissivity_value, numeric(1))
  expect_equal(unname(observed), unname(expected), tolerance = 0.005)
  expect_gt(length(unique(round(observed, 3))), 2)
  expect_identical(default_fit$emissivity_statistic, "planck_weighted")
  expect_equal(default_fit$emissivity_value, observed[1], tolerance = 1e-12)
})

test_that("calculate_emissivity preserves alignment and diagnostic values", {
  wavenumber <- seq(780, 1220, by = 4)
  downwelling <- 0.75 * .thermal_planck_reference(wavenumber, 290)
  temperatures <- c(328, 341)
  epsilon_a <- seq(-0.05, 0.95, length.out = length(wavenumber))
  epsilon_b <- seq(0.35, 1.08, length.out = length(wavenumber))
  spectra <- cbind(
    selected_a = .thermal_forward_model(
      wavenumber, temperatures[1], epsilon_a, downwelling
    ),
    selected_b = .thermal_forward_model(
      wavenumber, temperatures[2], epsilon_b, downwelling
    )
  )
  object <- .thermal_make_object(
    wavenumber, spectra, x_coord = c(4, 9), y_coord = c(2, 5)
  )
  object$metadata[, intensity_unit := intensity_units]
  object$metadata[, intensity_units := NULL]
  attr(object, "intensity_units") <- .thermal_radiance_unit
  object$spectra[5, "selected_a"] <- NA_real_
  object$spectra[9, "selected_b"] <- Inf
  expected_epsilon_a <- epsilon_a
  expected_epsilon_a[5] <- NA_real_
  expected_epsilon_b <- epsilon_b
  expected_epsilon_b[9] <- NA_real_
  original_spectra <- object$spectra[, , drop = FALSE]
  original_metadata <- data.table::copy(object$metadata)
  original_attributes <- attributes(object)

  result <- calculate_emissivity(
    object,
    temperature_k = temperatures,
    downwelling = downwelling
  )

  expect_s3_class(result, "OpenSpecy")
  expect_true(check_OpenSpecy(result))
  expect_identical(object$spectra, original_spectra)
  expect_identical(object$metadata, original_metadata)
  expect_identical(attributes(object), original_attributes)
  expect_identical(result$wavenumber, object$wavenumber)
  expect_identical(colnames(result$spectra), colnames(object$spectra))
  expect_equal(result$metadata$x, original_metadata$x)
  expect_equal(result$metadata$y, original_metadata$y)
  expect_equal(result$metadata$spectrum_identity,
               original_metadata$spectrum_identity)
  expect_equal(result$spectra[, 1], expected_epsilon_a, tolerance = 1e-10)
  expect_equal(result$spectra[, 2], expected_epsilon_b, tolerance = 1e-10)
  expect_true(is.na(result$spectra[5, "selected_a"]))
  expect_true(is.na(result$spectra[9, "selected_b"]))
  expect_lt(min(result$spectra, na.rm = TRUE), 0)
  expect_gt(max(result$spectra, na.rm = TRUE), 1)
  expect_identical(attr(result, "intensity_unit"), "emissivity")
  expect_identical(attr(result, "intensity_units"), "emissivity")
  expect_true(all(result$metadata$intensity_unit == "emissivity"))
  expect_true(all(result$metadata$intensity_units == "emissivity"))
  expect_identical(attr(result, "spectra_type"), "ftir")
  expect_identical(result$metadata$material_temperature_k, temperatures)
  expect_identical(result$metadata$temperature_source, rep("supplied", 2))

  transformations <- attr(result, "transformations", exact = TRUE)
  expect_length(transformations, 1)
  transformation <- transformations[[1]]
  expect_identical(transformation$method, "calculate_emissivity")
  expect_identical(
    transformation$model,
    "opaque_isothermal_surface_radiance"
  )
  expect_identical(
    transformation$temperature_k_sha256,
    digest::digest(as.numeric(temperatures), algo = "sha256")
  )
  expect_identical(
    transformation$downwelling_sha256,
    digest::digest(as.numeric(downwelling), algo = "sha256")
  )
  expect_true(transformation$lossy)
})

test_that("thermal retrieval rejects non-radiance and processed inputs", {
  wavenumber <- seq(800, 1200, by = 5)
  downwelling <- .thermal_planck_reference(wavenumber, 290)
  radiance <- .thermal_forward_model(
    wavenumber, 330, rep(0.8, length(wavenumber)), downwelling
  )
  object <- .thermal_make_object(wavenumber, cbind(sample = radiance))
  call_estimator <- function(x) {
    estimate_temperature(
      x,
      downwelling = downwelling,
      temperature_range_k = c(310, 350),
      fit_range_cm1 = c(800, 1200)
    )
  }

  conflicting_unit <- object
  conflicting_unit$metadata$intensity_units <- "absorbance"
  expect_error(call_estimator(conflicting_unit), "radiance|unit")

  conflicting_type <- object
  conflicting_type$metadata$spectrum_type <- "raman"
  expect_error(call_estimator(conflicting_type), "FTIR|ftir")

  no_provenance <- object
  no_provenance$metadata$radiance_level <- NULL
  no_provenance$metadata$radiometric_calibration <- NULL
  expect_error(call_estimator(no_provenance), "surface|calibr|provenance")

  derivative <- object
  attr(derivative, "derivative_order") <- 1
  expect_error(call_estimator(derivative), "derivative|process")

  normalized <- object
  normalized$metadata$data_processing_procedure <- "min-max normalized"
  expect_error(call_estimator(normalized), "normalized|process")

  no_baseline <- object
  no_baseline$metadata$data_processing_procedure <-
    "radiometric calibration; no baseline correction"
  expect_no_error(no_baseline_result <- call_estimator(no_baseline))
  expect_identical(no_baseline_result$status, "ok")

  relative <- make_rel(object)
  expect_error(call_estimator(relative), "normalized|process")

  duplicate_axis <- object
  duplicate_axis$wavenumber[2] <- duplicate_axis$wavenumber[1]
  expect_error(call_estimator(duplicate_axis), "unique|wavenumber")

  invalid_axis <- object
  invalid_axis$wavenumber[1] <- 0
  expect_error(call_estimator(invalid_axis), "positive|wavenumber")

  expect_error(
    estimate_temperature(
      object,
      downwelling = downwelling[-1],
      temperature_range_k = c(310, 350),
      fit_range_cm1 = c(800, 1200)
    ),
    "downwelling|length"
  )
})

test_that("downwelling spectra and uncertainty use radiance-scale safeguards", {
  wavenumber <- seq(780, 1220, by = 4)
  downwelling <- 0.82 * .thermal_planck_reference(wavenumber, 291)
  radiance <- .thermal_forward_model(
    wavenumber, 332, rep(0.79, length(wavenumber)), downwelling
  )
  object <- .thermal_make_object(wavenumber, cbind(sample = radiance))
  background <- .thermal_make_object(
    wavenumber, cbind(downwelling = downwelling)
  )
  fit <- estimate_temperature(
    object,
    downwelling = background,
    temperature_range_k = c(315, 350),
    fit_range_cm1 = range(wavenumber),
    radiance_uncertainty = rep(1e-6, length(wavenumber))
  )
  rejected <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(315, 350),
    fit_range_cm1 = range(wavenumber),
    radiance_uncertainty = rep(1, length(wavenumber))
  )

  expect_identical(fit$status, "ok")
  expect_equal(fit$estimated_material_temperature_k, 332, tolerance = 0.5)
  expect_identical(rejected$status, "near_singular")
  expect_true(is.na(rejected$estimated_material_temperature_k))
  expect_error(
    estimate_temperature(
      object,
      downwelling = downwelling,
      temperature_range_k = c(315, 350),
      fit_range_cm1 = range(wavenumber),
      radiance_uncertainty = 0
    ),
    "positive"
  )
})

test_that("ambiguous and boundary temperature fits remain aligned failures", {
  wavenumber <- seq(760, 1240, by = 4)
  downwelling <- 0.8 * .thermal_planck_reference(wavenumber, 290)
  boundary_radiance <- .thermal_forward_model(
    wavenumber, 310, rep(0.82, length(wavenumber)), downwelling
  )
  flat_radiance <- downwelling
  spectra <- cbind(boundary = boundary_radiance, flat = flat_radiance)
  object <- .thermal_make_object(wavenumber, spectra)

  result <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = c(760, 1240)
  )

  expect_identical(result$spectrum_index, 1:2)
  expect_identical(result$spectrum_id, c("boundary", "flat"))
  expect_true(all(result$status != "ok"))
  expect_true(all(is.na(result$estimated_material_temperature_k)))
  expect_true(all(is.na(result$emissivity_value)))
})

test_that("objective selection rejects ties and ignores nonfinite scores", {
  tied_upper <- .thermal_select_local_minimum(
    c(5, 1, 5, 1), rep(100, 4)
  )
  tied_lower <- .thermal_select_local_minimum(
    c(1, 5, 1, 5), rep(100, 4)
  )
  expect_identical(tied_upper$status, "multiple_minima")
  expect_identical(tied_lower$status, "multiple_minima")
  expect_true(is.na(tied_upper$index))
  expect_true(is.na(tied_lower$index))

  nonfinite_shoulder <- .thermal_select_local_minimum(
    c(5, 3, 1, 3, Inf), rep(100, 5)
  )
  expect_identical(nonfinite_shoulder$status, "candidate")
  expect_identical(nonfinite_shoulder$index, 3L)

  repeated_scores <- cbind(
    first = c(5, 3, 1, 3, 5),
    second = c(6, 4, 2, 4, 6)
  )
  expect_silent(
    broadcast <- .thermal_select_local_minimum(repeated_scores, 100)
  )
  expect_identical(broadcast$status, rep("candidate", 2))
  expect_identical(broadcast$index, rep(3L, 2))
})

test_that("noise cannot promote a boundary truth to an interior fit", {
  # This seed creates shallow interior wiggles in the roughness objective.
  # Without a prominence guard, all 100 spectra were falsely accepted near
  # 318 K even though the forward-model temperature is the 310 K boundary.
  set.seed(1)
  wavenumber <- seq(760, 1240, by = 4)
  scaled_wavenumber <- (wavenumber - min(wavenumber)) /
    diff(range(wavenumber))
  downwelling <- 0.8 * .thermal_planck_reference(wavenumber, 290)
  emissivity <- 0.7 + 0.1 * scaled_wavenumber
  boundary_radiance <- .thermal_forward_model(
    wavenumber, 310, emissivity, downwelling
  )
  spectra <- matrix(rep(boundary_radiance, 100), nrow = length(wavenumber)) +
    matrix(
      stats::rnorm(length(wavenumber) * 100, sd = 1e-8),
      nrow = length(wavenumber)
  )
  colnames(spectra) <- sprintf("boundary_noise_%03d", seq_len(ncol(spectra)))
  object <- .thermal_make_object(wavenumber, spectra)
  original_spectra <- object$spectra[, , drop = FALSE]
  original_metadata <- data.table::copy(object$metadata)
  original_attributes <- attributes(object)

  result <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "mean",
    block_size = 100
  )

  expect_identical(object$spectra, original_spectra)
  expect_identical(object$metadata, original_metadata)
  expect_identical(attributes(object), original_attributes)
  expect_identical(result$spectrum_index, seq_len(100))
  expect_identical(result$spectrum_id, colnames(spectra))
  expect_true(all(result$status != "ok"))
  expect_true(all(is.na(result$estimated_material_temperature_k)))
  expect_true(all(is.na(result$emissivity_value)))
})

test_that("missing and reversed irregular axes preserve row semantics", {
  z <- seq(0, 1, length.out = 141)^1.6
  wavenumber <- 750 + 500 * z
  downwelling <- 0.82 * .thermal_planck_reference(wavenumber, 289)
  radiance <- .thermal_forward_model(
    wavenumber, 333, 0.62 + 0.2 * z, downwelling
  )
  spectra <- cbind(partial = radiance, insufficient = radiance)
  object <- .thermal_make_object(wavenumber, spectra)
  object$spectra[c(11, 57, 103), "partial"] <- NA_real_
  object$spectra[-c(1, nrow(object$spectra)), "insufficient"] <- NA_real_

  result <- estimate_temperature(
    object,
    downwelling = downwelling,
    temperature_range_k = c(315, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "median"
  )

  expect_identical(result$spectrum_id, c("partial", "insufficient"))
  expect_identical(result$status[1], "ok")
  expect_equal(result$estimated_material_temperature_k[1], 333,
               tolerance = 0.75)
  expect_lt(result$valid_band_fraction[1], 1)
  expect_gt(result$valid_band_fraction[1], 0.9)
  expect_false(result$status[2] == "ok")
  expect_true(is.na(result$estimated_material_temperature_k[2]))

  reversed <- .thermal_make_object(wavenumber, cbind(partial = radiance))
  reversed$wavenumber <- rev(reversed$wavenumber)
  reversed$spectra <- reversed$spectra[nrow(reversed$spectra):1, , drop = FALSE]
  reversed_downwelling <- rev(downwelling)
  reversed_result <- estimate_temperature(
    reversed,
    downwelling = reversed_downwelling,
    temperature_range_k = c(315, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "median"
  )

  expect_identical(reversed_result$status, "ok")
  expect_equal(
    reversed_result$estimated_material_temperature_k,
    333,
    tolerance = 0.75
  )
  expect_equal(reversed_result$emissivity_value,
               result$emissivity_value[1], tolerance = 0.005)
})

test_that("FileSpecs streams the same thermal kernel and reuses its cache", {
  skip_if_not_installed("hdf5r")
  directory <- tempfile("thermal-filespec-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(directory, "thermal.h5")
  wavenumber <- seq(760, 1240, length.out = 121)
  downwelling <- 0.8 * .thermal_planck_reference(wavenumber, 290)
  temperatures <- c(326, 338)
  emissivity <- cbind(
    0.68 + 0.12 * (wavenumber - min(wavenumber)) / diff(range(wavenumber)),
    0.76 + 0.08 * (wavenumber - min(wavenumber)) / diff(range(wavenumber))
  )
  radiance <- vapply(seq_along(temperatures), function(i) {
    .thermal_forward_model(
      wavenumber, temperatures[i], emissivity[, i], downwelling
    )
  }, numeric(length(wavenumber)))

  h5 <- hdf5r::H5File$new(path, mode = "w")
  info <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">760</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">1240</VAR>",
    "<VAR TYPE=\"System.Int32\" NAME=\"SpectrumPoints\">121</VAR>"
  )
  info[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  region <- regions$create_group("Region1")
  region[["Dataset"]] <- array(radiance, dim = c(121, 1, 2))
  h5$close_all()

  declare_radiance <- function(x) {
    attr(x, "intensity_unit") <- .thermal_radiance_unit
    attr(x, "spectra_type") <- "ftir"
    attr(x, "radiance_level") <- "surface_leaving"
    attr(x, "radiometric_calibration") <- "synthetic H5 radiance"
    x
  }
  cache_one <- file.path(directory, "cache-one")
  cache_two <- file.path(directory, "cache-two")
  file_specs_one <- declare_radiance(open_specs(path, cache_dir = cache_one))
  file_specs_two <- declare_radiance(open_specs(path, cache_dir = cache_two))
  streamed <- estimate_temperature(
    file_specs_one,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "planck_weighted",
    block_size = 1
  )
  blocked <- estimate_temperature(
    file_specs_two,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "planck_weighted",
    block_size = 2
  )
  cache_files <- list.files(cache_two, pattern = "\\.rds$", recursive = TRUE,
                            full.names = TRUE)
  expect_gt(length(cache_files), 0)
  cache_mtime <- file.info(cache_files)$mtime
  cached <- estimate_temperature(
    file_specs_two,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "planck_weighted",
    block_size = 1
  )

  eager <- decompress_spec(file_specs_one, index = 1:2)
  attr(eager, "intensity_unit") <- .thermal_radiance_unit
  attr(eager, "spectra_type") <- "ftir"
  eager$metadata[, intensity_units := .thermal_radiance_unit]
  eager$metadata[, radiance_level := "surface_leaving"]
  eager$metadata[, radiometric_calibration := "synthetic H5 radiance"]
  in_memory <- estimate_temperature(
    eager,
    downwelling = downwelling,
    temperature_range_k = c(310, 350),
    fit_range_cm1 = range(wavenumber),
    emissivity_stat = "planck_weighted",
    block_size = 2
  )

  expect_identical(streamed$status, rep("ok", 2))
  expect_equal(streamed$estimated_material_temperature_k, temperatures,
               tolerance = 0.5)
  expect_equal(streamed, blocked, tolerance = 1e-12)
  expect_identical(cached, blocked)
  expect_identical(file.info(cache_files)$mtime, cache_mtime)
  expect_equal(streamed$emissivity_value, in_memory$emissivity_value,
               tolerance = 1e-12)
  expect_equal(streamed$estimated_material_temperature_k,
               in_memory$estimated_material_temperature_k,
               tolerance = 1e-12)
  expect_identical(streamed$source_index, 1:2)
})
