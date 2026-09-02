# Benchmark NA-tolerant reference medoid/model preparation.
#
# Usage:
#   Rscript benchmarks/reference_library_na_models.R [completed-build-or-dir]
#
# With no path, a deterministic synthetic Raman-like matrix is used for the
# same-output fill timing. Supplying a completed build additionally reports the
# complete-case versus 10%-support coverage of every processed spectrum type.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("This benchmark requires devtools")
}
devtools::load_all(quiet = TRUE)

legacy_spectrum_fill <- function(spectra) {
  spectra[!is.finite(spectra)] <- NA_real_
  out <- vapply(seq_len(ncol(spectra)), function(i) {
    as.numeric(mean_replace(spectra[, i], na.rm = TRUE))
  }, numeric(nrow(spectra)))
  dimnames(out) <- dimnames(spectra)
  out
}

set.seed(20260902)
probe <- matrix(stats::rnorm(401L * 1000L), nrow = 401L, ncol = 1000L)
probe[matrix(stats::runif(length(probe)) < 0.2, nrow = nrow(probe))] <- NA_real_

invisible(legacy_spectrum_fill(probe))
invisible(OpenSpecy:::.lib_spectrum_mean_replace(probe))
repetitions <- 5L
elapsed <- function(fun) {
  replicate(repetitions, system.time(fun(probe))[["elapsed"]])
}
old_elapsed <- elapsed(legacy_spectrum_fill)
new_elapsed <- elapsed(OpenSpecy:::.lib_spectrum_mean_replace)
stopifnot(isTRUE(all.equal(
  legacy_spectrum_fill(probe),
  OpenSpecy:::.lib_spectrum_mean_replace(probe), tolerance = 1e-14
)))
timing <- data.table::data.table(
  implementation = c("literal_mean_replace_by_spectrum", "vectorized_mean_replace"),
  median_seconds = c(stats::median(old_elapsed), stats::median(new_elapsed)),
  minimum_seconds = c(min(old_elapsed), min(new_elapsed)),
  repetitions = repetitions,
  wavenumbers = nrow(probe), spectra = ncol(probe)
)
print(timing)
if (timing[implementation == "vectorized_mean_replace", median_seconds] >
    1.1 * timing[implementation == "literal_mean_replace_by_spectrum",
                 median_seconds]) {
  stop("Vectorized spectrum-wise fill regressed by more than 10%")
}

filled_probe <- make_rel(mean_replace(probe, na.rm = TRUE), na.rm = TRUE)
filled_probe[!is.finite(filled_probe)] <- 0
correlation <- stats::cor(filled_probe)
correlation[is.na(correlation)] <- 0
diag(correlation) <- 1
distance <- stats::as.dist(1 - pmax(pmin(correlation, 1), -1))
pam_run <- function(variant) {
  set.seed(123)
  started <- proc.time()[["elapsed"]]
  fit <- if (variant == "build_then_fast_swap") {
    cluster::pam(distance, k = 50, diss = TRUE, pamonce = 6)
  } else {
    cluster::pam(distance, k = 50, diss = TRUE, variant = "faster")
  }
  c(
    elapsed = proc.time()[["elapsed"]] - started,
    objective = unname(fit$objective[[2L]])
  )
}
invisible(pam_run("build_then_fast_swap"))
invisible(pam_run("faster_random_init"))
pam_results <- data.table::rbindlist(lapply(seq_len(3L), function(i) {
  data.table::rbindlist(lapply(
    c("build_then_fast_swap", "faster_random_init"), function(variant) {
      values <- pam_run(variant)
      data.table::data.table(
        variant = variant, elapsed_seconds = values[["elapsed"]],
        objective = values[["objective"]]
      )
    }
  ))
}))
pam_summary <- pam_results[, .(
  median_seconds = stats::median(elapsed_seconds),
  objective_mean = mean(objective)
), by = variant]
print(pam_summary)
old_pam <- pam_summary[variant == "build_then_fast_swap"]
new_pam <- pam_summary[variant == "faster_random_init"]
if (new_pam$median_seconds > 1.1 * old_pam$median_seconds) {
  stop("FasterPAM initialization regressed by more than 10%")
}
if (new_pam$objective_mean > 1.02 * old_pam$objective_mean) {
  stop("FasterPAM objective degraded by more than 2%")
}

arguments <- commandArgs(trailingOnly = TRUE)
if (!length(arguments)) quit(save = "no", status = 0L)
input <- OpenSpecy:::.lib_resolve_rebuild_input(arguments[[1L]])
coverage <- list()
for (recipe in intersect(c("derivative", "nobaseline"),
                         names(input$libraries))) {
  for (type in names(input$libraries[[recipe]])) {
    restricted <- OpenSpecy:::.lib_restrict_model_range(
      input$libraries[[recipe]][[type]], type
    )
    audit <- attr(restricted, "identification_support", exact = TRUE)
    complete <- stats::complete.cases(t(restricted$spectra))
    supported_classes <- table(restricted$metadata$material_class)
    complete_classes <- table(restricted$metadata$material_class[complete])
    coverage[[paste(recipe, type, sep = "_")]] <- data.table::data.table(
      artifact = recipe, spectrum_type = type,
      input_spectra = nrow(audit),
      support_retained = sum(audit$retained),
      complete_cases = sum(complete),
      support_classes_min_10 = sum(supported_classes >= 10L),
      complete_case_classes_min_10 = sum(complete_classes >= 10L),
      missing_values_to_fill = sum(is.na(restricted$spectra))
    )
  }
}
coverage <- data.table::rbindlist(coverage, fill = TRUE)
print(coverage)
stopifnot(all(coverage$support_retained >= coverage$complete_cases))
