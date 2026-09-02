# Benchmark Raman multinomial model alpha values on a completed build.
#
# Usage:
#   Rscript benchmarks/reference_library_raman_alpha.R \
#     <reference_library_build.rds> <derivative|nobaseline> <output_dir> \
#     [comma-separated-alpha-values]
#
# Each alpha reuses the same medoid training object, seed, folds, calibrated
# settings, and source-local production holdout. Results are checkpointed after
# every fit so an interrupted benchmark can resume without repeating work.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "Supply the completed build RDS, recipe, and benchmark output directory",
    call. = FALSE
  )
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("This benchmark requires devtools", call. = FALSE)
}

build_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
recipe <- match.arg(args[[2L]], c("derivative", "nobaseline"))
output_dir <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)
alphas <- if (length(args) >= 4L) {
  as.numeric(strsplit(args[[4L]], ",", fixed = TRUE)[[1L]])
} else {
  seq(0, 1, by = 0.1)
}
if (any(!is.finite(alphas)) || any(alphas < 0 | alphas > 1)) {
  stop("Alpha values must be finite and between zero and one", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(quiet = TRUE)
build <- readRDS(build_path)
training <- build$medoids[[recipe]]$raman
released <- build$models[[recipe]]$raman
candidate <- build$libraries[[recipe]]$raman
if (!is_OpenSpecy(training) || !is_OpenSpecy(candidate) || is.null(released)) {
  stop("The completed build lacks the requested Raman artifacts", call. = FALSE)
}

eligible <- OpenSpecy:::.lib_restrict_model_range(candidate, "raman")
holdout_ids <- as.character(released$tests$spectrum_id)
eligible_ids <- as.character(OpenSpecy:::.lib_ids(eligible, "sample_name"))
holdout_rows <- match(holdout_ids, eligible_ids)
if (anyNA(holdout_rows)) {
  stop("Released Raman holdout identifiers are absent from the library",
       call. = FALSE)
}
holdout <- filter_spec(eligible, holdout_rows)
if (ncol(holdout$spectra) != nrow(released$tests)) {
  stop("Raman holdout reconstruction changed its denominator", call. = FALSE)
}

alpha_label <- function(alpha) {
  sub("\\.", "p", sprintf("%.1f", alpha))
}
checkpoint_path <- function(alpha) {
  file.path(output_dir, paste0(recipe, "_alpha_", alpha_label(alpha), ".rds"))
}

summarize_fit <- function(alpha) {
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  message(sprintf(
    "Raman alpha benchmark: %s alpha=%.1f starting (training=%d, holdout=%d)",
    recipe, alpha, ncol(training$spectra), ncol(holdout$spectra)
  ))
  fit <- withCallingHandlers(
    build_model_lib(training, alpha = alpha, seed = 123),
    warning = function(condition) {
      warnings <<- unique(c(warnings, conditionMessage(condition)))
      invokeRestart("muffleWarning")
    }
  )
  metrics <- data.table::as.data.table(fit$lambda_metrics)
  selected <- metrics[metrics$selected %in% TRUE][1L]
  tests <- OpenSpecy:::.lib_model_holdout_test(
    fit, holdout, recipe, "raman", source = "new",
    provenance = "candidate_model_fixed_production_holdout_alpha_benchmark"
  )
  valid <- !is.na(tests$correct)
  class_accuracy <- tests[valid, .(
    spectra = .N, class_accuracy = mean(correct)
  ), by = expected_class]
  summary <- data.table::data.table(
    recipe = recipe,
    alpha = alpha,
    selected_lambda = as.numeric(fit$lambda_selected),
    cv_macro_accuracy = as.numeric(selected$macro_class_accuracy),
    cv_overall_accuracy = as.numeric(selected$overall_accuracy),
    holdout_macro_accuracy = mean(class_accuracy$class_accuracy),
    holdout_overall_accuracy = mean(tests$correct[valid]),
    holdout_coverage = sum(valid) / nrow(tests),
    holdout_spectra = nrow(tests),
    holdout_classes = nrow(class_accuracy),
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    warning_count = length(warnings),
    warning_text = paste(warnings, collapse = " | ")
  )
  message(sprintf(
    paste0("Raman alpha benchmark: %s alpha=%.1f complete ",
           "(CV macro=%.4f, holdout macro=%.4f, %.1fs)"),
    recipe, alpha, summary$cv_macro_accuracy,
    summary$holdout_macro_accuracy, summary$elapsed_seconds
  ))
  list(
    summary = summary,
    class_accuracy = class_accuracy,
    lambda_metrics = fit$lambda_metrics
  )
}

for (alpha in alphas) {
  path <- checkpoint_path(alpha)
  if (file.exists(path)) {
    message(sprintf(
      "Raman alpha benchmark: %s alpha=%.1f reused", recipe, alpha
    ))
  } else {
    saveRDS(summarize_fit(alpha), path)
  }
  checkpoints <- list.files(
    output_dir, pattern = paste0("^", recipe, "_alpha_.*\\.rds$"),
    full.names = TRUE
  )
  results <- data.table::rbindlist(lapply(checkpoints, function(item) {
    readRDS(item)$summary
  }), fill = TRUE)
  data.table::setorder(results, alpha)
  data.table::fwrite(
    results, file.path(output_dir, paste0(recipe, "_raman_alpha_results.csv"))
  )
}

results <- data.table::fread(
  file.path(output_dir, paste0(recipe, "_raman_alpha_results.csv"))
)
print(results[, .(
  recipe, alpha, selected_lambda, cv_macro_accuracy, cv_overall_accuracy,
  holdout_macro_accuracy, holdout_overall_accuracy, holdout_coverage,
  holdout_spectra, holdout_classes, elapsed_seconds, warning_count
)])
