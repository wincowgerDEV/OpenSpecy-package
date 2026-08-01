# Repeated bundled-app latency benchmark.
# Run manually from the package root with:
# Rscript benchmarks/app_latency.R
#
# Optional environment variables:
# OPENSPECY_BENCH_REPETITIONS controls the app-stage repetitions (default 5).
# OPENSPECY_BENCH_LIBRARY_REPETITIONS controls library repetitions (default 2).
# OPENSPECY_SHINY_LIBRARY_PATH can point at an existing reference-library cache.
# This script never downloads reference libraries.

devtools::load_all(export_all = TRUE, quiet = TRUE)
app_path <- run_app(test_mode = TRUE)
sys.source(file.path(app_path, "global.R"), envir = environment())

positive_integer_env <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

stage_repetitions <- positive_integer_env("OPENSPECY_BENCH_REPETITIONS", 5L)
library_repetitions <- positive_integer_env(
  "OPENSPECY_BENCH_LIBRARY_REPETITIONS", 2L
)

elapsed_samples <- function(fun, repetitions) {
  vapply(seq_len(repetitions), function(i) {
    gc(FALSE)
    system.time(invisible(fun()))[["elapsed"]]
  }, numeric(1))
}

stage_results <- list()
record_stage <- function(stage, fun, repetitions = stage_repetitions) {
  runs <- elapsed_samples(fun, repetitions)
  result <- data.frame(
    stage = stage,
    repetitions = repetitions,
    median_seconds = stats::median(runs),
    runs_seconds = paste(sprintf("%.3f", runs), collapse = ", "),
    stringsAsFactors = FALSE
  )
  stage_results[[length(stage_results) + 1L]] <<- result
  message(
    stage, ": median ", sprintf("%.3fs", result$median_seconds),
    " [", result$runs_seconds, "]"
  )
  invisible(result)
}

query <- suppressMessages(
  read_any(read_extdata("raman_hdpe.csv")) |>
    c_spec(range = "common", res = 6) |>
    manage_na(ig = c(NA, 0), type = "remove")
)

default_corrections <- function() {
  app_apply_spectral_corrections(
    query,
    spike = TRUE,
    spike_args = list(
      method = "residual", direction = "both",
      residual_threshold = 8, residual_window = 5L
    ),
    saturation = "auto",
    saturation_args = list(max_saturation_loss = 0.70)
  )
}

corrected <- default_corrections()

default_processing <- function(x = corrected) {
  target_axis <- app_conform_axis(x, 6)
  processed <- process_spec(
    x = x,
    active = TRUE,
    adj_intens = FALSE,
    conform_spec = TRUE,
    conform_spec_args = list(
      range = target_axis, res = NULL, type = "interp"
    ),
    restrict_range = FALSE,
    flatten_range = FALSE,
    subtr_baseline = FALSE,
    smooth_intens = TRUE,
    smooth_intens_args = list(
      polynomial = 3,
      window = calc_window_points(target_axis, 90),
      derivative = 1,
      abs = TRUE
    ),
    make_rel = TRUE
  )
  app_copy_correction_history(x, processed)
}

processed <- default_processing()

default_automatic_ranges <- function(x = processed) {
  app_apply_range_automation(
    x,
    flatten = TRUE,
    restrict = TRUE,
    flatten_args = list(min = 2200, max = 2400, artifact_ratio = 3),
    restrict_args = list(artifact_ratio = 3)
  )$data |>
    app_attach_correction_metadata()
}

final_query <- default_automatic_ranges()

default_assessment <- function(x = final_query) {
  assess_spec(
    x,
    checks = app_quality_checks,
    report = "all",
    snr_metric = "run_sig_over_noise",
    spike_args = list(
      method = "residual", direction = "both",
      residual_threshold = 8, residual_window = 5L
    ),
    saturation = "auto"
  )
}

default_plot_build <- function(x = final_query) {
  app_spectrum_plot(
    active = x,
    raw = query,
    reference = NULL,
    make_rel = TRUE,
    source = "B",
    plot_width = 1000
  ) |>
    app_style_plotly() |>
    plotly::plotly_build()
}

default_core_analysis <- function() {
  acquisition_corrected <- default_corrections()
  ordinarily_processed <- default_processing(acquisition_corrected)
  result <- default_automatic_ranges(ordinarily_processed)
  assessment <- default_assessment(result)
  plot <- default_plot_build(result)
  list(data = result, assessment = assessment, plot = plot)
}

stopifnot(
  check_OpenSpecy(query),
  check_OpenSpecy(corrected),
  check_OpenSpecy(processed),
  check_OpenSpecy(final_query),
  ncol(final_query$spectra) == 1L,
  nrow(default_assessment()) == length(app_quality_checks)
)

message(
  "Default single-spectrum case: ", length(query$wavenumber),
  " imported points; ", length(final_query$wavenumber),
  " final processed points."
)
record_stage("acquisition corrections", default_corrections)
record_stage("ordinary processing", default_processing)
record_stage("automatic flatten and tail", default_automatic_ranges)
record_stage("selected-spectrum assessment", default_assessment)
record_stage("spectrum plot build", default_plot_build)
record_stage("complete non-library analysis", default_core_analysis)

quiet_library_call <- function(fun) {
  suppressMessages(suppressWarnings(fun()))
}

library_cache_paths <- unique(c(
  Sys.getenv("OPENSPECY_SHINY_LIBRARY_PATH", ""),
  file.path(tools::R_user_dir("OpenSpecy", "cache"),
            "reference_libraries")
))
library_cache_paths <- library_cache_paths[
  nzchar(library_cache_paths) & dir.exists(library_cache_paths)
]

resolve_library <- function(type) {
  installed <- tryCatch(
    quiet_library_call(function() load_lib(type)),
    error = function(e) NULL
  )
  if (!is.null(installed)) {
    return(list(
      source = "installed package",
      data = installed,
      load = function() quiet_library_call(function() load_lib(type))
    ))
  }

  for (path in library_cache_paths) {
    cached <- tryCatch(
      quiet_library_call(function() load_lib(type, path = path)),
      error = function(e) NULL
    )
    if (!is.null(cached)) {
      return(local({
        library_path <- path
        list(
          source = normalizePath(library_path, winslash = "/"),
          data = cached,
          load = function() quiet_library_call(function() {
            load_lib(type, path = library_path)
          })
        )
      }))
    }
  }
  NULL
}

prepare_library <- function(library, target_axis) {
  prepared <- conform_spec(
    library,
    range = target_axis,
    res = NULL,
    allow_na = TRUE,
    type = "roll"
  )
  keep <- !apply(prepared$spectra, 2L, function(values) all(is.na(values)))
  prepared <- filter_spec(prepared, logic = keep)
  if ("spectrum_type" %in% names(prepared$metadata)) {
    prepared <- filter_spec(
      prepared,
      logic = prepared$metadata$spectrum_type == "raman"
    )
  }
  prepared
}

match_library <- function(library) {
  cor_spec(final_query, library = library, conform = TRUE, type = "roll")
}

library_specs <- data.frame(
  library = c("medoid", "full"),
  type = c("medoid_derivative", "derivative"),
  stringsAsFactors = FALSE
)
library_results <- list()

for (i in seq_len(nrow(library_specs))) {
  label <- library_specs$library[[i]]
  type <- library_specs$type[[i]]
  resolved <- resolve_library(type)
  if (is.null(resolved)) {
    message(
      "SKIP ", label, " library benchmark: '", type,
      "' was not found in the installed package or configured/user cache; ",
      "no download was attempted."
    )
    next
  }

  loaded <- resolved$data
  prepared <- prepare_library(loaded, final_query$wavenumber)
  if (ncol(prepared$spectra) == 0L) {
    message(
      "SKIP ", label, " library correlation: no references overlap the ",
      "default Raman query after conformation."
    )
    next
  }

  load_runs <- elapsed_samples(resolved$load, library_repetitions)
  conform_runs <- elapsed_samples(
    function() prepare_library(loaded, final_query$wavenumber),
    library_repetitions
  )
  correlation_runs <- elapsed_samples(
    function() match_library(prepared),
    library_repetitions
  )

  # The uncached path reloads and reconforms the same immutable reference data.
  # The cached path reuses the prepared library and performs only correlation.
  freshly_loaded <- resolved$load()
  freshly_prepared <- prepare_library(
    freshly_loaded, final_query$wavenumber
  )
  uncached_result <- match_library(freshly_prepared)
  cached_result <- match_library(prepared)
  equivalent <- identical(uncached_result, cached_result)
  if (!equivalent) {
    stop(
      label, " cached and uncached library matches are not identical",
      call. = FALSE
    )
  }

  load_median <- stats::median(load_runs)
  conform_median <- stats::median(conform_runs)
  correlation_median <- stats::median(correlation_runs)
  uncached_median <- load_median + conform_median + correlation_median
  cached_median <- correlation_median
  cached_ratio <- cached_median / max(uncached_median, .Machine$double.eps)
  if (is.finite(cached_ratio) && cached_ratio > 1.10) {
    warning(
      sprintf(
        "%s cached-library runtime regression flag: %.1f%% slower than uncached",
        label, 100 * (cached_ratio - 1)
      ),
      call. = FALSE
    )
  }

  library_results[[length(library_results) + 1L]] <- data.frame(
    library = label,
    type = type,
    source = resolved$source,
    input_references = ncol(loaded$spectra),
    matching_references = ncol(prepared$spectra),
    load_seconds = load_median,
    conform_seconds = conform_median,
    correlation_seconds = correlation_median,
    uncached_path_seconds = uncached_median,
    cached_path_seconds = cached_median,
    cached_to_uncached_ratio = cached_ratio,
    results_identical = equivalent,
    stringsAsFactors = FALSE
  )
  message(
    label, " library (", format(ncol(prepared$spectra), big.mark = ","),
    " matching references): uncached ", sprintf("%.3fs", uncached_median),
    "; cached ", sprintf("%.3fs", cached_median),
    "; identical results."
  )
}

cat("\nDefault single-spectrum stages\n")
print(do.call(rbind, stage_results), row.names = FALSE)

cat("\nOptional reference-library stages\n")
if (length(library_results)) {
  print(do.call(rbind, library_results), row.names = FALSE)
} else {
  cat("All optional library cases were skipped; no libraries were downloaded.\n")
}
