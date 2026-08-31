# Compare the former nested report expansion with the vectorized indexed path.
# The numerical assessment is prepared once and excluded from measured time.
devtools::load_all(quiet = TRUE)

count <- 400L
spectra <- matrix(
  rep(raman_hdpe$spectra[, 1L], count), nrow = nrow(raman_hdpe$spectra)
)
spectra <- sweep(spectra, 2L, seq(0, 0.02, length.out = count), "+")
ids <- paste0("assessment_", seq_len(count))
colnames(spectra) <- ids
x <- as_OpenSpecy(
  raman_hdpe$wavenumber,
  spectra,
  metadata = data.table::data.table(sample_name = ids, col_id = ids)
)
checks <- c(
  "high_tail", "silent_region", "co2_region", "missing_values",
  "flat_spectrum", "negative_intensity", "low_snr"
)
expanded <- assess_spec(x, checks = checks, report = "all")
base_fields <- c(
  "spectrum_index", "spectrum_id", "check", "issue", "description",
  "likely_cause", "potential_fix", "metric", "value", "threshold",
  "candidate_max", "control_max", "region_min", "region_max"
)
issues <- expanded[
  status != "pass" & issue != "Check unavailable", ..base_fields
]
evidence <- expanded[, c(
  list(spectrum_index, check, metric, value, threshold, candidate_max,
       control_max, region_min, region_max),
  list(evaluable = issue != "Check unavailable")
)]

old_expand_standard <- function(x, checks, issues, evidence) {
  ids <- colnames(x$spectra)
  severity <- c(
    high_tail = "warning", silent_region = "warning",
    co2_region = "warning", missing_values = "error",
    flat_spectrum = "error", negative_intensity = "warning",
    low_snr = "warning"
  )
  rows <- vector("list", length(checks) * length(ids))
  cursor <- 0L
  for (check in checks) {
    for (i in seq_along(ids)) {
      cursor <- cursor + 1L
      found <- issues[
        issues$check == check & issues$spectrum_index == i, , drop = FALSE
      ]
      evidence_row <- evidence[
        evidence$check == check & evidence$spectrum_index == i,
        , drop = FALSE
      ]
      if (nrow(found)) {
        row <- found[1L]
        status <- unname(severity[[check]])
        count_metric <- row$metric[[1L]] %in% c(
          "non_finite_count", "spike_count", "saturated_interval_count"
        )
        finding_count <- if (count_metric && is.finite(row$value[[1L]])) {
          as.integer(row$value[[1L]])
        } else {
          nrow(found)
        }
      } else {
        row <- data.table::data.table(
          spectrum_index = as.integer(i), spectrum_id = ids[[i]],
          check = check, issue = "No issue detected",
          description = paste0("The ", gsub("_", " ", check),
                               " check passed."),
          likely_cause = NA_character_, potential_fix = "No action required.",
          metric = NA_character_, value = NA_real_, threshold = NA_real_,
          candidate_max = NA_real_, control_max = NA_real_,
          region_min = NA_real_, region_max = NA_real_
        )
        status <- "pass"
        finding_count <- 0L
        if (nrow(evidence_row)) {
          for (field in c(
            "metric", "value", "threshold", "candidate_max", "control_max",
            "region_min", "region_max"
          )) row[[field]] <- evidence_row[[field]][[1L]]
        }
        evaluable <- nrow(evidence_row) > 0L &&
          isTRUE(evidence_row$evaluable[[1L]])
        if (!evaluable) {
          row$issue <- "Check unavailable"
          row$description <- paste0(
            "The ", gsub("_", " ", check),
            " check could not be evaluated from the available finite data ",
            "and wavenumber coverage."
          )
          row$likely_cause <- if (!any(is.finite(x$spectra[, i]))) {
            paste(
              "The imported spectrum contains only missing or non-finite",
              "intensity values."
            )
          } else {
            paste(
              "The spectrum has too few usable points or does not cover the",
              "region required by this check."
            )
          }
          row$potential_fix <- paste(
            "Inspect the source file, repair the import, or recollect the",
            "spectrum before interpretation."
          )
          status <- "error"
          finding_count <- 1L
        }
      }
      region <- if (is.finite(row$region_min[[1L]]) &&
                    is.finite(row$region_max[[1L]])) {
        data.frame(
          region_min = row$region_min[[1L]], region_max = row$region_max[[1L]]
        )
      } else {
        data.frame(region_min = numeric(), region_max = numeric())
      }
      row$scope <- "spectrum"
      row$status <- status
      row$finding_count <- as.integer(finding_count)
      row$regions <- list(region)
      row$correction_applied <- FALSE
      row$correction_summary <- NA_character_
      row$test_id <- paste("spectrum", i, ids[[i]], check, sep = ":")
      rows[[cursor]] <- row
    }
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

old <- old_expand_standard(x, checks, issues, evidence)
current <- OpenSpecy:::.expand_standard_assessment_report(
  x, checks, issues, evidence, ids
)
stopifnot(isTRUE(all.equal(old, current, check.attributes = FALSE)))

elapsed <- function(fun, repetitions = 3L) {
  vapply(seq_len(repetitions), function(i) {
    unname(system.time(fun())[["elapsed"]])
  }, numeric(1))
}
old_time <- elapsed(function() old_expand_standard(x, checks, issues, evidence))
current_time <- elapsed(function() {
  OpenSpecy:::.expand_standard_assessment_report(
    x, checks, issues, evidence, ids
  )
})
cat(sprintf(
  "legacy median: %.3fs; vectorized median: %.3fs; speedup: %.1fx\n",
  stats::median(old_time), stats::median(current_time),
  stats::median(old_time) / stats::median(current_time)
))
stopifnot(stats::median(current_time) <= stats::median(old_time) * 1.1)
