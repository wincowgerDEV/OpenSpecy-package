# In-memory temperature/emissivity scalability and scientific benchmark.
# Run manually from the package root with:
# Rscript benchmarks/temperature_emissivity.R
#
# The 850 x 100,000 case contains about 648 MiB of radiance values before
# object metadata or working memory. It is opt-in and runs only when the
# measured 10,000-spectrum time and analytical memory projection remain below
# the plan's stop thresholds.
#
# Optional environment variables:
# OPENSPECY_BENCH_REPETITIONS (default 3; the opt-in 100k case runs once)
# OPENSPECY_BENCH_BLOCK_SIZE (default package automatic block size)
# OPENSPECY_BENCH_RUN_100K (default false)
# OPENSPECY_BENCH_INTERMEDIATE_N (optional staged size above 10,000)
# OPENSPECY_BENCH_RUN_COMMON_MISSING (default false)
#   Runs 100/10,000 spectra with one shared detector band missing. Its 100,000
#   case additionally requires OPENSPECY_BENCH_RUN_100K=true and both guards.
# OPENSPECY_BENCH_TRACE_MEMORY (default false; traces source copies and reports
#   allocations of at least 100 KiB during the 10,000-spectrum case)
# OPENSPECY_BENCH_MAX_MISSING_RUNTIME_RATIO (default 1.5)
# OPENSPECY_BENCH_MAX_PROJECTED_SECONDS (default 300)
# OPENSPECY_BENCH_MAX_PROJECTED_MIB (default 2048)

devtools::load_all(export_all = TRUE, quiet = TRUE)

positive_integer_env <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

positive_number_env <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
    stop(name, " must be a positive finite number", call. = FALSE)
  }
  value
}

logical_env <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false", call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

repetitions <- positive_integer_env("OPENSPECY_BENCH_REPETITIONS", 3L)
block_size_setting <- Sys.getenv("OPENSPECY_BENCH_BLOCK_SIZE", "")
block_size <- if (nzchar(block_size_setting)) {
  positive_integer_env("OPENSPECY_BENCH_BLOCK_SIZE", 1L)
} else {
  NULL
}
run_100k <- logical_env("OPENSPECY_BENCH_RUN_100K", FALSE)
intermediate_setting <- Sys.getenv("OPENSPECY_BENCH_INTERMEDIATE_N", "")
intermediate_n <- if (nzchar(intermediate_setting)) {
  positive_integer_env("OPENSPECY_BENCH_INTERMEDIATE_N", 10000L)
} else {
  integer()
}
run_common_missing <- logical_env(
  "OPENSPECY_BENCH_RUN_COMMON_MISSING", FALSE
)
trace_memory <- logical_env("OPENSPECY_BENCH_TRACE_MEMORY", FALSE)
max_missing_runtime_ratio <- positive_number_env(
  "OPENSPECY_BENCH_MAX_MISSING_RUNTIME_RATIO", 1.5
)
max_projected_seconds <- positive_number_env(
  "OPENSPECY_BENCH_MAX_PROJECTED_SECONDS", 300
)
max_projected_mib <- positive_number_env(
  "OPENSPECY_BENCH_MAX_PROJECTED_MIB", 2048
)

radiance_unit <- "W m^-2 sr^-1 (cm^-1)^-1"
wavenumber <- seq(650, 1499, length.out = 850L)
fit_range_cm1 <- range(wavenumber)
temperature_range_k <- c(300, 370)
effective_block_size <- getFromNamespace(
  ".thermal_block_size", "OpenSpecy"
)(block_size, length(wavenumber))
target_incremental_mib <- 640
target_total_mib <- 1536

# Independent benchmark oracle, deliberately not shared with package code.
benchmark_planck <- function(wavenumber_cm1, temperature_k) {
  h <- 6.62607015e-34
  c <- 299792458
  k <- 1.380649e-23
  wavenumber_m1 <- wavenumber_cm1 * 100
  2 * h * c^2 * wavenumber_m1^3 * 100 /
    expm1(h * c * wavenumber_m1 / (k * temperature_k))
}

planck_checkpoint <- benchmark_planck(1000, 300)
stopifnot(isTRUE(all.equal(
  planck_checkpoint, 0.0992403333007, tolerance = 5e-13
)))
package_planck <- getFromNamespace(
  ".planck_radiance_wavenumber", "OpenSpecy"
)
stopifnot(isTRUE(all.equal(
  package_planck(c(700, 1000, 1400), 315),
  benchmark_planck(c(700, 1000, 1400), 315),
  tolerance = 5e-13
)))

downwelling <- 0.96 * benchmark_planck(wavenumber, 290)
scaled_wavenumber <- (wavenumber - min(wavenumber)) / diff(range(wavenumber))

# Linear emissivity curves make the true temperature a zero-curvature oracle.
# The four patterns avoid benchmarking a single repeated numeric column while
# keeping the forward model exactly within the stated opaque/isothermal scope.
emissivity_templates <- cbind(
  0.72 + 0.16 * scaled_wavenumber,
  0.74 + 0.13 * scaled_wavenumber,
  0.68 + 0.18 * scaled_wavenumber,
  0.76 + 0.10 * scaled_wavenumber
)
template_temperatures <- c(322, 326, 330, 334)

make_thermal_fixture <- function(n_spectra, missing_band_index = integer()) {
  stopifnot(n_spectra >= 1L)
  if (!is.numeric(missing_band_index) || anyNA(missing_band_index) ||
      any(!is.finite(missing_band_index)) ||
      any(missing_band_index != floor(missing_band_index)) ||
      any(missing_band_index < 1L) ||
      any(missing_band_index > length(wavenumber))) {
    stop("'missing_band_index' must contain valid whole-number row indices",
         call. = FALSE)
  }
  missing_band_index <- unique(as.integer(missing_band_index))
  ids <- seq_len(n_spectra)
  template_id <- (ids - 1L) %% ncol(emissivity_templates) + 1L
  spectra <- matrix(NA_real_, nrow = length(wavenumber), ncol = n_spectra)
  for (index in seq_len(ncol(emissivity_templates))) {
    columns <- which(template_id == index)
    radiance <- downwelling + emissivity_templates[, index] *
      (benchmark_planck(wavenumber, template_temperatures[index]) -
         downwelling)
    spectra[, columns] <- radiance
  }
  if (length(missing_band_index)) {
    spectra[missing_band_index, ] <- NA_real_
  }
  colnames(spectra) <- sprintf("pixel_%06d", ids)
  grid_width <- ceiling(sqrt(n_spectra))
  metadata <- data.frame(
    x = (ids - 1L) %% grid_width,
    y = (ids - 1L) %/% grid_width,
    spectrum_type = "ftir",
    intensity_units = radiance_unit,
    radiance_level = "surface_leaving",
    radiometric_calibration = "synthetic SI-radiance benchmark",
    data_processing_procedure = "none",
    stringsAsFactors = FALSE
  )
  object <- as_OpenSpecy(
    wavenumber,
    spectra,
    metadata = metadata,
    attributes = list(
      intensity_unit = radiance_unit,
      derivative_order = "0",
      baseline = "raw",
      spectra_type = "ftir"
    ),
    compute_file_id = FALSE
  )
  list(
    x = object,
    temperature_k = template_temperatures[template_id],
    template_id = template_id,
    missing_band_index = missing_band_index
  )
}

estimate_fixture <- function(fixture, emissivity_stat = "planck_weighted",
                             requested_block_size = block_size) {
  estimate_temperature(
    fixture$x,
    downwelling = downwelling,
    temperature_range_k = temperature_range_k,
    fit_range_cm1 = fit_range_cm1,
    emissivity_stat = emissivity_stat,
    block_size = requested_block_size
  )
}

validate_result <- function(result, fixture) {
  required <- c(
    "estimated_material_temperature_k", "emissivity_value",
    "emissivity_statistic", "status"
  )
  if (!all(required %in% names(result))) {
    stop("temperature result is missing benchmark contract columns", call. = FALSE)
  }
  if (nrow(result) != ncol(fixture$x$spectra)) {
    stop("temperature result lost source alignment", call. = FALSE)
  }
  success <- result$status == "ok"
  if (!any(success)) {
    stop("synthetic oracle produced no successful fits", call. = FALSE)
  }
  error_k <- abs(
    result$estimated_material_temperature_k[success] -
      fixture$temperature_k[success]
  )
  expected_valid_fraction <-
    (nrow(fixture$x$spectra) - length(fixture$missing_band_index)) /
    nrow(fixture$x$spectra)
  if (length(fixture$missing_band_index) &&
      (!"valid_band_fraction" %in% names(result) ||
       any(abs(result$valid_band_fraction[success] -
                 expected_valid_fraction) > 1e-12))) {
    stop("shared missing detector bands changed valid-band accounting",
         call. = FALSE)
  }
  list(
    success_fraction = mean(success),
    median_absolute_error_k = stats::median(error_k),
    max_absolute_error_k = max(error_k)
  )
}

# This is an analytical numeric-payload estimate, not a measured process RSS.
# It includes the dense source, eight bands x block work arrays, coarse/fine
# score arrays, and a conservative 128 bytes per returned row.
memory_projection <- function(n_spectra) {
  active_columns <- min(n_spectra, effective_block_size)
  source_bytes <- 8 * length(wavenumber) * n_spectra
  block_bytes <- 8 * length(wavenumber) * active_columns * 8
  score_bytes <- 8 * (17 + 9) * active_columns * 2
  result_bytes <- 128 * n_spectra
  c(
    source_mib = source_bytes / 1024^2,
    incremental_mib = (block_bytes + score_bytes + result_bytes) / 1024^2,
    estimated_peak_mib = (source_bytes + block_bytes + score_bytes +
                            result_bytes) / 1024^2
  )
}

run_timed_case <- function(n_spectra, missing_band_index = integer()) {
  gc(FALSE)
  fixture <- make_thermal_fixture(n_spectra, missing_band_index)
  trace_token <- if (trace_memory) tracemem(fixture$x$spectra) else NULL
  trace_active <- !is.null(trace_token)
  allocation_log <- NULL
  profile_active <- FALSE
  on.exit({
    if (profile_active) Rprofmem(NULL)
    if (!is.null(allocation_log) && file.exists(allocation_log)) {
      unlink(allocation_log)
    }
    if (trace_active) untracemem(fixture$x$spectra)
  }, add = TRUE)
  object_mib <- as.numeric(object.size(fixture$x)) / 1024^2
  case_repetitions <- if (n_spectra >= 100000L) 1L else repetitions
  elapsed <- numeric(case_repetitions)
  heap_baseline_mib <- numeric(case_repetitions)
  heap_peak_mib <- numeric(case_repetitions)
  heap_end_mib <- numeric(case_repetitions)
  result <- NULL
  for (index in seq_len(case_repetitions)) {
    before <- gc(FALSE, reset = TRUE)
    heap_baseline_mib[index] <- sum(before[, 2L])
    allocation_log <- if (trace_memory && n_spectra == 10000L) {
      tempfile("openspecy-thermal-rprofmem-", fileext = ".log")
    } else {
      NULL
    }
    if (!is.null(allocation_log)) {
      Rprofmem(allocation_log, threshold = 100 * 1024)
      profile_active <- TRUE
    }
    elapsed[index] <- unname(system.time({
      candidate <- estimate_fixture(fixture)
    })[["elapsed"]])
    if (!is.null(allocation_log)) {
      Rprofmem(NULL)
      profile_active <- FALSE
      allocation_lines <- readLines(allocation_log, warn = FALSE)
      allocation_bytes <- suppressWarnings(as.numeric(
        sub(" .*", "", allocation_lines)
      ))
      keep <- is.finite(allocation_bytes)
      first_call <- sub("^[0-9]+ :", "", allocation_lines[keep])
      first_call <- sub('^"([^"]+)".*$', "\\1", first_call)
      if (any(keep)) {
        allocation_summary <- stats::aggregate(
          allocation_bytes[keep], by = list(call = first_call), sum
        )
        allocation_summary$total_mib <- allocation_summary$x / 1024^2
        allocation_summary <- allocation_summary[
          order(allocation_summary$total_mib, decreasing = TRUE),
          c("call", "total_mib")
        ]
        print(utils::head(allocation_summary, 15L), row.names = FALSE)
      }
      unlink(allocation_log)
      allocation_log <- NULL
    }
    after <- gc(FALSE)
    heap_peak_mib[index] <- sum(after[, 6L])
    heap_end_mib[index] <- sum(after[, 2L])
    if (is.null(result)) result <- candidate
  }
  validation <- validate_result(result, fixture)
  projection <- memory_projection(n_spectra)
  row <- data.frame(
    spectra = n_spectra,
    bands = length(wavenumber),
    shared_missing_bands = length(missing_band_index),
    shared_missing_wavenumber_cm1 = if (length(missing_band_index)) {
      wavenumber[missing_band_index[[1L]]]
    } else {
      NA_real_
    },
    block_size = effective_block_size,
    repetitions = case_repetitions,
    median_seconds = stats::median(elapsed),
    source_mib = unname(projection[["source_mib"]]),
    object_mib = object_mib,
    estimated_incremental_mib = unname(projection[["incremental_mib"]]),
    estimated_peak_mib = unname(projection[["estimated_peak_mib"]]),
    measured_incremental_mib = max(heap_peak_mib - heap_baseline_mib),
    measured_r_heap_peak_mib = max(heap_peak_mib),
    measured_retained_mib = max(heap_end_mib - heap_baseline_mib),
    success_fraction = validation$success_fraction,
    median_absolute_error_k = validation$median_absolute_error_k,
    max_absolute_error_k = validation$max_absolute_error_k,
    stringsAsFactors = FALSE
  )
  if (trace_active) {
    untracemem(fixture$x$spectra)
    trace_active <- FALSE
  }
  rm(candidate, result, fixture)
  gc(FALSE)
  if (n_spectra >= 100000L &&
      (row$measured_incremental_mib > target_incremental_mib ||
       row$measured_r_heap_peak_mib > target_total_mib)) {
    stop(
      "100,000-spectrum measured R heap exceeds the memory target: ",
      sprintf("%.1f MiB incremental / %.1f MiB total",
              row$measured_incremental_mib,
              row$measured_r_heap_peak_mib),
      call. = FALSE
    )
  }
  row
}

# Check all scalar summary policies on a bounded object. Each call must retain
# one value per spectrum and agree with a direct emissivity calculation at the
# temperature selected by the package.
summary_fixture <- make_thermal_fixture(100L)
summary_names <- c("planck_weighted", "mean", "median", "max")
summary_elapsed <- numeric(length(summary_names))
summary_results <- lapply(seq_along(summary_names), function(index) {
  summary_elapsed[index] <- unname(system.time({
    result <- estimate_fixture(
      summary_fixture,
      emissivity_stat = summary_names[index],
      requested_block_size = 37L
    )
  })[["elapsed"]])
  result
})
names(summary_results) <- summary_names

for (statistic in summary_names) {
  result <- summary_results[[statistic]]
  validation <- validate_result(result, summary_fixture)
  if (validation$success_fraction != 1) {
    stop(statistic, " summary failed on the noiseless oracle", call. = FALSE)
  }
  selected_temperature <- result$estimated_material_temperature_k
  selected_planck <- vapply(
    selected_temperature,
    function(value) benchmark_planck(wavenumber, value),
    numeric(length(wavenumber))
  )
  emissivity <- (summary_fixture$x$spectra - downwelling) /
    (selected_planck - downwelling)
  gaps <- diff(wavenumber)
  trapezoid_weights <- c(
    gaps[1L] / 2,
    (gaps[-length(gaps)] + gaps[-1L]) / 2,
    gaps[length(gaps)] / 2
  )
  expected <- switch(
    statistic,
    planck_weighted = colSums(
      emissivity * selected_planck * trapezoid_weights
    ) / colSums(selected_planck * trapezoid_weights),
    mean = colMeans(emissivity),
    median = apply(emissivity, 2L, stats::median),
    max = apply(emissivity, 2L, max)
  )
  if (!isTRUE(all.equal(
    result$emissivity_value, expected, tolerance = 1e-10,
    check.attributes = FALSE
  ))) {
    stop(statistic, " emissivity summary differs from the scalar oracle",
         call. = FALSE)
  }
  if (!all(result$emissivity_statistic == statistic)) {
    stop("emissivity statistic label is not aligned with its value",
         call. = FALSE)
  }
}
summary_comparison <- data.frame(
  emissivity_statistic = summary_names,
  seconds = summary_elapsed,
  result_kib = vapply(
    summary_results, function(x) as.numeric(object.size(x)) / 1024,
    numeric(1)
  ),
  mean_emissivity_value = vapply(
    summary_results, function(x) mean(x$emissivity_value), numeric(1)
  ),
  stringsAsFactors = FALSE
)
print(summary_comparison, row.names = FALSE)

# Block-size invariance is part of the in-memory contract even though the
# control is an advanced memory/time choice rather than a scientific setting.
block_fixture <- make_thermal_fixture(137L)
block_small <- estimate_fixture(block_fixture, requested_block_size = 17L)
block_large <- estimate_fixture(block_fixture, requested_block_size = 131L)
if (!isTRUE(all.equal(
  as.data.frame(block_small), as.data.frame(block_large),
  tolerance = 1e-12, check.attributes = FALSE
))) {
  stop("in-memory temperature results depend on block size", call. = FALSE)
}
rm(block_fixture, block_small, block_large, summary_fixture, summary_results)
gc(FALSE)

# Stage the dense in-memory cases. A full FileSpecs object is intentionally not
# needed here: the standard OpenSpecy matrix is the primary performance target.
benchmark_sizes <- unique(c(1L, 100L, 10000L, intermediate_n))
benchmark_rows <- lapply(benchmark_sizes, run_timed_case)
benchmark_results <- do.call(rbind, benchmark_rows)
projected_100k_seconds <- benchmark_results$median_seconds[
  benchmark_results$spectra == 10000L
] * 10
analytical_100k_peak_mib <-
  memory_projection(100000L)[["estimated_peak_mib"]]

stop_guard_passes <- projected_100k_seconds <= max_projected_seconds &&
  analytical_100k_peak_mib <= max_projected_mib
if (run_100k && stop_guard_passes) {
  benchmark_results <- rbind(benchmark_results, run_timed_case(100000L))
} else {
  reason <- if (!run_100k) {
    "OPENSPECY_BENCH_RUN_100K is false"
  } else {
    "10,000-spectrum time or memory projection exceeds the stop threshold"
  }
  message("Skipping 850 x 100,000 benchmark: ", reason, ".")
}

actual_100k <- benchmark_results[benchmark_results$spectra == 100000L, ]
actual_100k_seconds <- if (nrow(actual_100k)) {
  actual_100k$median_seconds[[1L]]
} else {
  NA_real_
}
actual_100k_incremental_mib <- if (nrow(actual_100k)) {
  actual_100k$measured_incremental_mib[[1L]]
} else {
  NA_real_
}
actual_100k_total_mib <- if (nrow(actual_100k)) {
  actual_100k$measured_r_heap_peak_mib[[1L]]
} else {
  NA_real_
}
print(benchmark_results, row.names = FALSE)
print(data.frame(
  projected_100k_seconds = projected_100k_seconds,
  analytical_payload_peak_mib = analytical_100k_peak_mib,
  target_100k_seconds = 120,
  projected_time_target_met = projected_100k_seconds <= 120,
  actual_100k_seconds = actual_100k_seconds,
  actual_time_target_met = actual_100k_seconds <= 120,
  target_incremental_mib = target_incremental_mib,
  actual_incremental_mib = actual_100k_incremental_mib,
  actual_incremental_target_met =
    actual_100k_incremental_mib <= target_incremental_mib,
  target_total_mib = target_total_mib,
  actual_total_mib = actual_100k_total_mib,
  actual_total_target_met = actual_100k_total_mib <= target_total_mib,
  stop_seconds = max_projected_seconds,
  stop_peak_mib = max_projected_mib,
  opt_in = run_100k,
  stop_guard_passes = stop_guard_passes,
  ran_100k = any(benchmark_results$spectra == 100000L),
  stringsAsFactors = FALSE
), row.names = FALSE)

# A detector failure is commonly shared by every pixel in an image. This lane
# verifies that one identical missing band retains the blocked matrix path
# rather than falling back to one optimizer per spectrum. It is opt-in because
# the 10,000 case is itself a throughput probe; the 100,000 case is doubly
# gated by this flag, the ordinary 100k flag, and the time/memory thresholds.
common_missing_band_index <- which.min(abs(wavenumber - 1000))
if (run_common_missing) {
  common_missing_fixture <- make_thermal_fixture(
    137L, common_missing_band_index
  )
  if (!all(colSums(!is.finite(common_missing_fixture$x$spectra)) == 1L)) {
    stop("common-missing fixture must contain exactly one shared missing band",
         call. = FALSE)
  }
  common_missing_small <- estimate_fixture(
    common_missing_fixture, requested_block_size = 17L
  )
  common_missing_large <- estimate_fixture(
    common_missing_fixture, requested_block_size = 131L
  )
  common_missing_small_validation <- validate_result(
    common_missing_small, common_missing_fixture
  )
  common_missing_large_validation <- validate_result(
    common_missing_large, common_missing_fixture
  )
  if (common_missing_small_validation$success_fraction != 1 ||
      common_missing_large_validation$success_fraction != 1) {
    stop("common-missing block-invariance fixture contains rejected fits",
         call. = FALSE)
  }
  if (!isTRUE(all.equal(
    as.data.frame(common_missing_small),
    as.data.frame(common_missing_large),
    tolerance = 1e-12, check.attributes = FALSE
  ))) {
    stop("common-missing results depend on block size", call. = FALSE)
  }
  rm(
    common_missing_fixture, common_missing_small, common_missing_large,
    common_missing_small_validation, common_missing_large_validation
  )
  gc(FALSE)

  common_missing_rows <- lapply(c(100L, 10000L), function(n_spectra) {
    run_timed_case(n_spectra, common_missing_band_index)
  })
  common_missing_results <- do.call(rbind, common_missing_rows)
  if (any(common_missing_results$success_fraction != 1)) {
    stop("common-missing throughput fixture contains rejected fits",
         call. = FALSE)
  }
  baseline_10k_seconds <- benchmark_results$median_seconds[
    benchmark_results$spectra == 10000L
  ][[1L]]
  common_missing_10k_seconds <- common_missing_results$median_seconds[
    common_missing_results$spectra == 10000L
  ][[1L]]
  common_missing_10k_ratio <-
    common_missing_10k_seconds / baseline_10k_seconds
  projected_common_missing_100k_seconds <- common_missing_10k_seconds * 10
  common_missing_stop_guard_passes <-
    stop_guard_passes &&
    common_missing_10k_ratio <= max_missing_runtime_ratio &&
    projected_common_missing_100k_seconds <= max_projected_seconds &&
    analytical_100k_peak_mib <= max_projected_mib

  if (run_100k && common_missing_stop_guard_passes) {
    common_missing_results <- rbind(
      common_missing_results,
      run_timed_case(100000L, common_missing_band_index)
    )
  } else {
    common_missing_reason <- if (!run_100k) {
      "OPENSPECY_BENCH_RUN_100K is false"
    } else {
      "common-missing throughput, time, or memory guard failed"
    }
    message(
      "Skipping common-missing 850 x 100,000 benchmark: ",
      common_missing_reason, "."
    )
  }

  baseline_positions <- match(
    common_missing_results$spectra, benchmark_results$spectra
  )
  common_missing_results$complete_median_seconds <-
    benchmark_results$median_seconds[baseline_positions]
  common_missing_results$missing_to_complete_runtime <-
    common_missing_results$median_seconds /
    common_missing_results$complete_median_seconds
  actual_common_missing_scaling <- if (
    any(common_missing_results$spectra == 100000L)
  ) {
    common_missing_results$median_seconds[
      common_missing_results$spectra == 100000L
    ][[1L]] / common_missing_10k_seconds
  } else {
    NA_real_
  }
  actual_common_missing_seconds <- if (
    any(common_missing_results$spectra == 100000L)
  ) {
    common_missing_results$median_seconds[
      common_missing_results$spectra == 100000L
    ][[1L]]
  } else {
    NA_real_
  }
  print(common_missing_results, row.names = FALSE)
  print(data.frame(
    missing_band_index = common_missing_band_index,
    missing_wavenumber_cm1 = wavenumber[common_missing_band_index],
    projected_100k_seconds = projected_common_missing_100k_seconds,
    target_100k_seconds = 120,
    projected_time_target_met =
      projected_common_missing_100k_seconds <= 120,
    actual_100k_seconds = actual_common_missing_seconds,
    actual_time_target_met = actual_common_missing_seconds <= 120,
    max_missing_to_complete_runtime = max_missing_runtime_ratio,
    observed_10k_to_complete_runtime = common_missing_10k_ratio,
    actual_100k_to_10k_runtime = actual_common_missing_scaling,
    throughput_guard_passes =
      common_missing_10k_ratio <= max_missing_runtime_ratio,
    combined_stop_guard_passes = common_missing_stop_guard_passes,
    ran_100k = any(common_missing_results$spectra == 100000L),
    stringsAsFactors = FALSE
  ), row.names = FALSE)
  if (common_missing_10k_ratio > max_missing_runtime_ratio) {
    stop(
      "common-missing 10,000-spectrum runtime regression: missing/complete = ",
      sprintf("%.3f", common_missing_10k_ratio), " (failure limit ",
      sprintf("%.3f", max_missing_runtime_ratio), ")",
      call. = FALSE
    )
  }
} else {
  message(
    "Skipping common-missing-band throughput benchmark: ",
    "OPENSPECY_BENCH_RUN_COMMON_MISSING is false."
  )
}

# Labelled particle/background simulation: lower-emissivity particles are made
# warmer so mean radiance is deliberately confounded. This records whether the
# TES statistic complements direct mean-radiance and direct spectral S/N; it is
# not a substitute for the measured-acquisition validation gate.
contrast_to_noise <- function(value, label) {
  first <- value[label == levels(label)[1L]]
  second <- value[label == levels(label)[2L]]
  pooled_sd <- sqrt((stats::var(first) + stats::var(second)) / 2)
  difference <- abs(mean(first) - mean(second))
  if (pooled_sd == 0) {
    if (difference == 0) 0 else Inf
  } else {
    difference / pooled_sd
  }
}

make_contrast_fixture <- function(replicates = 200L) {
  label <- factor(rep(c("background", "particle"), each = replicates))
  variant <- rep(rep(seq_len(5L), length.out = replicates), 2L)
  offsets <- seq(-0.012, 0.012, length.out = 5L)
  background_temperature <- 325
  background_emissivity <- 0.84 + 0.04 * scaled_wavenumber
  particle_emissivity <- 0.64 + 0.08 * scaled_wavenumber
  target_radiance <- mean(
    downwelling + background_emissivity *
      (benchmark_planck(wavenumber, background_temperature) - downwelling)
  )
  particle_temperature <- stats::uniroot(
    function(temperature) {
      mean(downwelling + particle_emissivity *
             (benchmark_planck(wavenumber, temperature) - downwelling)) -
        target_radiance
    },
    interval = c(background_temperature, max(temperature_range_k))
  )$root
  spectra <- matrix(NA_real_, nrow = length(wavenumber), ncol = length(label))
  truth <- numeric(length(label))
  for (index in seq_along(label)) {
    is_particle <- label[index] == "particle"
    emissivity <- if (is_particle) {
      particle_emissivity + offsets[variant[index]]
    } else {
      background_emissivity + offsets[variant[index]]
    }
    temperature <- if (is_particle) {
      particle_temperature
    } else {
      background_temperature
    }
    truth[index] <- temperature
    spectra[, index] <- downwelling + emissivity *
      (benchmark_planck(wavenumber, temperature) - downwelling)
  }
  colnames(spectra) <- sprintf("contrast_%04d", seq_along(label))
  metadata <- data.frame(
    x = seq_along(label) - 1L,
    y = 0L,
    class = label,
    spectrum_type = "ftir",
    intensity_units = radiance_unit,
    radiance_level = "surface_leaving",
    radiometric_calibration = "synthetic SI-radiance benchmark",
    data_processing_procedure = "none",
    stringsAsFactors = FALSE
  )
  list(
    x = as_OpenSpecy(
      wavenumber,
      spectra,
      metadata = metadata,
      attributes = list(
        intensity_unit = radiance_unit,
        derivative_order = "0",
        baseline = "raw",
        spectra_type = "ftir"
      ),
      compute_file_id = FALSE
    ),
    label = label,
    temperature_k = truth
  )
}

contrast_fixture <- make_contrast_fixture()
contrast_tes <- estimate_fixture(
  contrast_fixture, emissivity_stat = "mean",
  requested_block_size = 113L
)
contrast_validation <- validate_result(contrast_tes, contrast_fixture)
if (contrast_validation$success_fraction != 1) {
  stop("labelled contrast simulation contains rejected TES fits", call. = FALSE)
}
direct_mean_radiance <- colMeans(contrast_fixture$x$spectra)
direct_spectral_sn <- sig_noise(contrast_fixture$x, step = 17L)
contrast_results <- data.frame(
  metric = c(
    "direct_mean_radiance", "direct_spectral_signal_noise",
    "tes_mean_emissivity"
  ),
  particle_background_cnr = c(
    contrast_to_noise(direct_mean_radiance, contrast_fixture$label),
    contrast_to_noise(direct_spectral_sn, contrast_fixture$label),
    contrast_to_noise(contrast_tes$emissivity_value, contrast_fixture$label)
  ),
  spectra = ncol(contrast_fixture$x$spectra),
  bands = nrow(contrast_fixture$x$spectra),
  stringsAsFactors = FALSE
)
print(contrast_results, row.names = FALSE)
