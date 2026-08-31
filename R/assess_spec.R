#' @rdname assess_spec
#' @title Assess common spectral quality issues
#'
#' @description
#' \code{assess_spec()} scans spectra for common quality-control issues and
#' returns one row for each issue found.
#'
#' @param x an \code{OpenSpecy} object.
#' @param checks character; checks to run. Options include
#' \code{"high_tail"}, \code{"silent_region"}, \code{"co2_region"},
#' \code{"missing_values"}, \code{"flat_spectrum"},
#' \code{"negative_intensity"}, \code{"low_snr"}, \code{"spike"}, and
#' \code{"saturation"}. Spike and saturation checks are opt-in.
#' @param high_prob numeric; spectrum-wide quantile used as the high intensity
#' threshold for the silent-region check.
#' @param artifact_ratio numeric; minimum ratio between the normalized maximum
#' in a tail or carbon dioxide region and the normalized maximum outside both
#' artifact regions required to flag an issue. The default \code{2} flags a
#' candidate at or above twice the control-region maximum.
#' @param tail_n integer; number of points to check at each end of the spectrum.
#' @param silent_region numeric length two; wavenumber range expected to be
#' mostly silent. The default is \code{c(2420, 2550)} cm^-1.
#' @param co2_region numeric length two; carbon dioxide wavenumber range.
#' @param snr_threshold numeric; spectra with run signal-to-noise below this
#' value are flagged.
#' @param flat_tol numeric; maximum finite intensity range considered flat.
#' @param negative_tol numeric; minimum allowed intensity before a spectrum is
#' flagged as negative.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating thresholds and metrics.
#' @param report character; \code{"issues"} preserves the issue-only return
#' contract, while \code{"all"} returns an explicit pass, warning, or error row
#' for every requested check and spectrum.
#' @param snr_metric character; signal-to-noise metric passed to
#' \code{\link{sig_noise}()} for the \code{"low_snr"} check.
#' @param spike_args named list of arguments passed to the shared internal spike
#' detector when \code{"spike"} is requested.
#' @param saturation \code{"auto"} or one finite numeric detector ceiling used
#' by the \code{"saturation"} check.
#' @param saturation_min_run integer or \code{NULL}; minimum saturated run used
#' by the shared detector.
#' @param saturation_tolerance numeric; relative equality tolerance for
#' automatic detector plateaus.
#' @param \ldots further arguments passed to \code{\link{sig_noise}()} for the
#' \code{"low_snr"} check.
#'
#' @return
#' With \code{report = "issues"}, a
#' \code{\link[data.table]{data.table-class}()} with one row per issue found
#' and columns describing the spectrum, check, issue, likely cause, potential
#' fix, metric value, threshold, and region. If no issues are found, an empty
#' table with the same columns is returned. With \code{report = "all"}, one
#' status row is returned for every requested spectrum/check pair, plus any
#' applicable batch-level error, with stable IDs and correction diagnostics.
#'
#' @examples
#' data("raman_hdpe")
#' assess_spec(raman_hdpe)
#'
#' @author
#' Win Cowger
#'
#' @importFrom data.table data.table rbindlist
#' @export
assess_spec <- function(x, ...) {
  UseMethod("assess_spec")
}

#' @rdname assess_spec
#'
#' @export
assess_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = FALSE)
}

#' @rdname assess_spec
#'
#' @export
assess_spec.OpenSpecy <- function(x,
                                  checks = c(
                                    "high_tail", "silent_region", "co2_region",
                                    "missing_values", "flat_spectrum",
                                    "negative_intensity", "low_snr"
                                  ),
                                  high_prob = 0.9,
                                  artifact_ratio = 2,
                                  tail_n = 5L,
                                  silent_region = c(2420, 2550),
                                  co2_region = c(2200, 2420),
                                  snr_threshold = 4,
                                   flat_tol = sqrt(.Machine$double.eps),
                                   negative_tol = 0,
                                   na.rm = TRUE,
                                   report = c("issues", "all"),
                                   snr_metric = "run_sig_over_noise",
                                   spike_args = list(),
                                   saturation = "auto",
                                   saturation_min_run = NULL,
                                   saturation_tolerance =
                                     sqrt(.Machine$double.eps),
                                   ...) {
  x <- as_OpenSpecy(x)
  spectra <- x$spectra
  report <- match.arg(report)

  if (is.complex(spectra))
    stop("assess_spec() requires real-valued spectral intensities",
         call. = FALSE)

  valid_checks <- c("high_tail", "silent_region", "co2_region",
                    "missing_values", "flat_spectrum", "negative_intensity",
                    "low_snr", "spike", "saturation")
  bad_checks <- checks[!checks %in% valid_checks]
  if (length(bad_checks) > 0L) {
    stop("'checks' contains unsupported values: ",
         paste(unique(bad_checks), collapse = ", "),
         call. = FALSE)
  }
  checks <- unique(checks)

  if (!is.numeric(high_prob) || length(high_prob) != 1L ||
      is.na(high_prob) || high_prob < 0 || high_prob > 1) {
    stop("'high_prob' must be a single numeric value between 0 and 1",
         call. = FALSE)
  }
  if (!is.numeric(artifact_ratio) || length(artifact_ratio) != 1L ||
      is.na(artifact_ratio) || artifact_ratio <= 1) {
    stop("'artifact_ratio' must be a single numeric value greater than 1",
         call. = FALSE)
  }
  if (!is.numeric(tail_n) || length(tail_n) != 1L ||
      is.na(tail_n) || tail_n < 1) {
    stop("'tail_n' must be a positive integer", call. = FALSE)
  }
  tail_n <- as.integer(tail_n)

  .check_region <- function(region, name) {
    if (!is.numeric(region) || length(region) != 2L || any(is.na(region)) ||
        region[1L] > region[2L]) {
      stop("'", name, "' must be a numeric vector of length two with ",
           "minimum followed by maximum",
           call. = FALSE)
    }
  }
  .check_region(silent_region, "silent_region")
  .check_region(co2_region, "co2_region")

  .empty_assessment <- function() {
    data.table(
      spectrum_index = integer(),
      spectrum_id = character(),
      check = character(),
      issue = character(),
      description = character(),
      likely_cause = character(),
      potential_fix = character(),
      metric = character(),
      value = numeric(),
      threshold = numeric(),
      candidate_max = numeric(),
      control_max = numeric(),
      region_min = numeric(),
      region_max = numeric()
    )
  }

  spectrum_ids <- colnames(spectra)
  if (is.null(spectrum_ids))
    spectrum_ids <- paste0("V", seq_len(ncol(spectra)))

  .issue_table <- function(check, idx, issue, description, likely_cause,
                           potential_fix, metric, value, threshold,
                           candidate_max = NA_real_, control_max = NA_real_,
                           region = c(NA_real_, NA_real_)) {
    idx <- as.integer(idx)
    if (length(idx) == 0L) return(NULL)

    data.table(
      spectrum_index = idx,
      spectrum_id = spectrum_ids[idx],
      check = check,
      issue = issue,
      description = description,
      likely_cause = likely_cause,
      potential_fix = potential_fix,
      metric = metric,
      value = as.numeric(value),
      threshold = as.numeric(threshold),
      candidate_max = as.numeric(candidate_max),
      control_max = as.numeric(control_max),
      region_min = as.numeric(region[1L]),
      region_max = as.numeric(region[2L])
    )
  }

  finite_spectra <- spectra
  non_finite <- !is.finite(finite_spectra)
  if (any(non_finite))
    finite_spectra[non_finite] <- NA_real_

  need_high <- "silent_region" %in% checks
  high_threshold <- NULL
  if (need_high) {
    high_threshold <- matrixStats::colQuantiles(finite_spectra,
                                                probs = high_prob,
                                                na.rm = na.rm,
                                                drop = TRUE)
    high_threshold <- as.numeric(high_threshold)
  }

  .col_max <- function(mat) {
    if (any(!is.finite(mat))) {
      mat <- mat
      mat[!is.finite(mat)] <- NA_real_
    }
    values <- matrixStats::colMaxs(mat, na.rm = na.rm)
    values[!is.finite(values)] <- NA_real_
    as.numeric(values)
  }

  .high_flags <- function(values) {
    !is.na(values) & !is.na(high_threshold) & values > high_threshold
  }

  .evidence_table <- function(check, metric, value, threshold,
                              candidate_max = NA_real_,
                              control_max = NA_real_,
                              region = c(NA_real_, NA_real_),
                              evaluable = !is.na(value)) {
    count <- ncol(spectra)
    data.table(
      spectrum_index = seq_len(count),
      check = check,
      metric = rep_len(as.character(metric), count),
      value = rep_len(as.numeric(value), count),
      threshold = rep_len(as.numeric(threshold), count),
      candidate_max = rep_len(as.numeric(candidate_max), count),
      control_max = rep_len(as.numeric(control_max), count),
      region_min = rep_len(as.numeric(region[[1L]]), count),
      region_max = rep_len(as.numeric(region[[2L]]), count),
      evaluable = rep_len(as.logical(evaluable), count)
    )
  }

  issues <- list()
  assessment_evidence <- list()
  spike_detection <- NULL
  saturation_detection <- NULL

  artifact_metrics <- NULL
  if (any(checks %in% c("high_tail", "co2_region"))) {
    artifact_metrics <- .artifact_ratio_metrics(
      x,
      tail_n = tail_n,
      co2_region = co2_region,
      na.rm = na.rm
    )
  }

  if ("high_tail" %in% checks) {
    tail_ratio <- artifact_metrics$tail_ratio
    assessment_evidence[["high_tail"]] <- .evidence_table(
      "high_tail", "artifact_max_ratio", tail_ratio, artifact_ratio,
      candidate_max = artifact_metrics$tail_max,
      control_max = artifact_metrics$control_max
    )
    idx <- which(!is.na(tail_ratio) & tail_ratio >= artifact_ratio)
    issues[[length(issues) + 1L]] <- .issue_table(
      "high_tail", idx,
      "High tail intensity",
      paste0("The normalized maximum in the first or last ",
             artifact_metrics$tail_n, " spectrum points is at least ",
             artifact_ratio, " times the maximum outside the tail and CO2 ",
             "regions."),
      "Instrument artifact, fluorescence, uncorrected or poorly corrected baseline, or a real peak that is being cropped at the edge.",
      "Inspect edge regions, restrict the spectral range, subtract baseline, or rerun the measurement.",
      "artifact_max_ratio",
      tail_ratio[idx],
      artifact_ratio,
      artifact_metrics$tail_max[idx],
      artifact_metrics$control_max[idx]
    )
  }

  .add_high_region <- function(check, region, issue, likely_cause,
                               potential_fix) {
    rows <- x$wavenumber >= region[1L] & x$wavenumber <= region[2L]
    if (!any(rows)) return(NULL)

    region_max <- .col_max(spectra[rows, , drop = FALSE])
    assessment_evidence[[check]] <<- .evidence_table(
      check, "max_region_intensity", region_max, high_threshold,
      region = region
    )
    idx <- which(.high_flags(region_max))
    .issue_table(
      check, idx,
      issue,
      paste0("The maximum intensity in ", region[1L], "-", region[2L],
             " is above the spectrum-wide high quantile."),
      likely_cause,
      potential_fix,
      "max_region_intensity",
      region_max[idx],
      high_threshold[idx],
      region = region
    )
  }

  if ("silent_region" %in% checks) {
    issues[[length(issues) + 1L]] <- .add_high_region(
      "silent_region", silent_region,
      "High intensity in silent region",
      "Fluorescence, uncorrected or poorly corrected baseline, or rare material bands.",
      "Inspect the region, subtract baseline, remove the region, flatten region, or rerun the spectrum."
    )
    if (is.null(assessment_evidence[["silent_region"]])) {
      assessment_evidence[["silent_region"]] <- .evidence_table(
        "silent_region", "max_region_intensity", NA_real_, high_threshold,
        region = silent_region, evaluable = FALSE
      )
    }
  }

  if ("co2_region" %in% checks) {
    co2_ratio <- artifact_metrics$co2_ratio
    assessment_evidence[["co2_region"]] <- .evidence_table(
      "co2_region", "artifact_max_ratio", co2_ratio, artifact_ratio,
      candidate_max = artifact_metrics$co2_max,
      control_max = artifact_metrics$control_max,
      region = co2_region
    )
    idx <- which(!is.na(co2_ratio) & co2_ratio >= artifact_ratio)
    issues[[length(issues) + 1L]] <- .issue_table(
      "co2_region", idx,
      "High intensity in CO2 region (infrared spectra)",
      paste0("The normalized maximum in ", co2_region[1L], "-",
             co2_region[2L], " is at least ", artifact_ratio,
             " times the maximum outside the tail and CO2 regions."),
      "Carbon dioxide present in signal, baseline correction issues, or background collection issues.",
      "Flatten or remove the CO2 region, add the instrument's atmospheric correction, purge the instrument, or rerun the background or spectrum.",
      "artifact_max_ratio",
      co2_ratio[idx],
      artifact_ratio,
      artifact_metrics$co2_max[idx],
      artifact_metrics$control_max[idx],
      region = co2_region
    )
  }

  if ("missing_values" %in% checks) {
    missing_count <- colSums(!is.finite(spectra))
    assessment_evidence[["missing_values"]] <- .evidence_table(
      "missing_values", "non_finite_count", missing_count, 0,
      evaluable = TRUE
    )
    idx <- which(missing_count > 0L)
    issues[[length(issues) + 1L]] <- .issue_table(
      "missing_values", idx,
      "Missing or non-finite intensity values",
      "The spectrum contains NA, NaN, Inf, or -Inf intensity values.",
      "File parsing issue, detector issues, failed interpolation, or unsupported numeric values.",
      "Review the instrument's source data, repair or remove non-finite values, use manage_na() before downstream analysis, recollect spectra.",
      "non_finite_count",
      missing_count[idx],
      0
    )
  }

  need_range <- any(checks %in% c("flat_spectrum", "negative_intensity"))
  finite_min <- finite_max <- NULL
  if (need_range) {
    finite_min <- matrixStats::colMins(finite_spectra, na.rm = TRUE)
    finite_max <- matrixStats::colMaxs(finite_spectra, na.rm = TRUE)
    no_finite <- colSums(is.finite(spectra)) == 0L
    finite_min[no_finite] <- NA_real_
    finite_max[no_finite] <- NA_real_
  }

  if ("flat_spectrum" %in% checks) {
    ranges <- as.numeric(finite_max - finite_min)
    assessment_evidence[["flat_spectrum"]] <- .evidence_table(
      "flat_spectrum", "finite_intensity_range", ranges, flat_tol
    )
    idx <- which(!is.na(ranges) & ranges <= flat_tol)
    issues[[length(issues) + 1L]] <- .issue_table(
      "flat_spectrum", idx,
      "Flat spectrum",
      "The finite intensity range is at or below the flat-spectrum tolerance.",
      "No sample signal, failed acquisition, or completely saturated detector.",
      "Inspect the raw file and acquisition settings for issues, rerun the measurement",
      "finite_intensity_range",
      ranges[idx],
      flat_tol
    )
  }

  if ("negative_intensity" %in% checks) {
    finite_min <- as.numeric(finite_min)
    assessment_evidence[["negative_intensity"]] <- .evidence_table(
      "negative_intensity", "min_finite_intensity", finite_min,
      negative_tol
    )
    idx <- which(!is.na(finite_min) & finite_min < negative_tol)
    issues[[length(issues) + 1L]] <- .issue_table(
      "negative_intensity", idx,
      "Negative intensity values",
      "The minimum finite intensity is below the allowed negative threshold.",
      "Baseline oversubtraction, poor unit conversion, or instrument noise.",
      "Increase smoothing, adjust baseline correction, convert intensity units, or min-max normalize spectra.",
      "min_finite_intensity",
      finite_min[idx],
      negative_tol
    )
  }

  if ("low_snr" %in% checks) {
    snr <- sig_noise(x, metric = snr_metric, na.rm = na.rm, ...)
    assessment_evidence[["low_snr"]] <- .evidence_table(
      "low_snr", snr_metric, snr, snr_threshold
    )
    idx <- which(!is.na(snr) & snr < snr_threshold)
    issues[[length(issues) + 1L]] <- .issue_table(
      "low_snr", idx,
      "Low signal-to-noise",
      paste0("The ", snr_metric,
             " signal-to-noise metric is below the configured threshold."),
      "Weak sample signal, short acquisition, high detector noise, or poor focus.",
      "Increase acquisition time or accumulations, improve focus, smooth the spectrum, or rerun the measurement.",
      snr_metric,
      snr[idx],
      snr_threshold
    )
  }

  if ("spike" %in% checks) {
    spike_arg_names <- names(spike_args)
    if (!is.list(spike_args) ||
        (length(spike_args) > 0L &&
         (is.null(spike_arg_names) || any(!nzchar(spike_arg_names)) ||
          "x" %in% spike_arg_names))) {
      stop("'spike_args' must be a named list", call. = FALSE)
    }
    spike_detection <- do.call(
      .detect_spikes,
      c(list(x = x), spike_args)
    )
    candidates <- spike_detection$candidates
    spike_counts <- tabulate(
      candidates$spectrum_index, nbins = ncol(spectra)
    )
    spike_max <- rep(NA_real_, ncol(spectra))
    if (nrow(candidates) > 0L) {
      score_groups <- split(candidates$score, candidates$spectrum_index)
      for (group_name in names(score_groups)) {
        scores <- score_groups[[group_name]]
        scores <- scores[is.finite(scores)]
        if (length(scores)) spike_max[[as.integer(group_name)]] <- max(scores)
      }
    }
    spike_evaluable <- colSums(is.finite(spectra)) > 0L
    if (startsWith(spike_detection$reason, "insufficient_")) {
      spike_evaluable[] <- FALSE
    }
    assessment_evidence[["spike"]] <- .evidence_table(
      "spike", "spike_candidate_count", spike_counts, 0,
      candidate_max = spike_max, evaluable = spike_evaluable
    )
    if (nrow(candidates) > 0L) {
      by_spectrum <- split(candidates, candidates$spectrum_index)
      for (group in by_spectrum) {
        idx <- unique(group$spectrum_index)
        correctable_count <- sum(group$correctable %in% TRUE)
        issues[[length(issues) + 1L]] <- .issue_table(
          "spike", idx,
          "Spike candidate detected",
          paste0(
            nrow(group), " isolated impulse candidate(s) were detected; ",
            correctable_count, " passed conservative correction safeguards."
          ),
          "Cosmic ray, detector impulse, electrical transient, or acquisition artifact.",
          "Inspect the raw spectrum, apply correct_spike(), or recollect if the feature is ambiguous.",
          "spike_count",
          nrow(group),
          0,
          candidate_max = suppressWarnings(max(group$score, na.rm = TRUE)),
          region = c(min(group$region_min), max(group$region_max))
        )
      }
    }
  }

  if ("saturation" %in% checks) {
    saturation_detection <- .detect_saturation(
      x,
      saturation = saturation,
      min_run = saturation_min_run,
      tolerance = saturation_tolerance
    )
    saturation_counts <- tabulate(
      saturation_detection$regions$spectrum_index,
      nbins = ncol(spectra)
    )
    saturation_thresholds <- if (is.numeric(saturation)) {
      rep(saturation, ncol(spectra))
    } else {
      .col_max(spectra)
    }
    assessment_evidence[["saturation"]] <- .evidence_table(
      "saturation", "saturated_interval_count", saturation_counts,
      0,
      candidate_max = saturation_thresholds,
      evaluable = colSums(is.finite(spectra)) > 0L
    )
    if (nrow(saturation_detection$regions) > 0L) {
      by_spectrum <- split(saturation_detection$regions,
                           saturation_detection$regions$spectrum_index)
      for (group in by_spectrum) {
        idx <- unique(group$spectrum_index)
        issues[[length(issues) + 1L]] <- .issue_table(
          "saturation", idx,
          "Saturated spectral interval detected",
          paste0(nrow(group), " saturated interval(s) were detected in ",
                 "the raw intensity values."),
          "Detector clipping, excessive exposure, laser power, or integration time.",
          "Apply one shared restrict_range() saturation restriction to the batch and library, or recollect at lower exposure.",
          "saturated_interval_count",
          nrow(group),
          0,
          candidate_max = suppressWarnings(max(group$threshold, na.rm = TRUE)),
          region = c(min(group$region_min), max(group$region_max))
        )
      }
    }
  }

  issues <- Filter(Negate(is.null), issues)
  issue_table <- if (length(issues) == 0L) {
    .empty_assessment()
  } else {
    rbindlist(issues, use.names = TRUE)
  }
  if (identical(report, "issues")) return(issue_table)
  evidence_table <- if (length(assessment_evidence)) {
    rbindlist(assessment_evidence, use.names = TRUE)
  } else {
    data.table::data.table()
  }
  .expand_assessment_report(
    x, checks, issue_table,
    evidence = evidence_table,
    spike_detection = spike_detection,
    saturation_detection = saturation_detection
  )
}

.empty_full_assessment <- function(issues) {
  out <- data.table::copy(issues)
  out$scope <- character()
  out$status <- character()
  out$finding_count <- integer()
  out$regions <- vector("list", 0L)
  out$correction_applied <- logical()
  out$correction_summary <- character()
  out$test_id <- character()
  out
}

.expand_assessment_report <- function(x, checks, issues, evidence,
                                      spike_detection = NULL,
                                      saturation_detection = NULL) {
  ids <- colnames(x$spectra)
  if (is.null(ids)) ids <- paste0("V", seq_len(ncol(x$spectra)))
  if (length(checks) == 0L || length(ids) == 0L) {
    return(.empty_full_assessment(issues))
  }
  if (!any(checks %in% c("spike", "saturation"))) {
    return(.expand_standard_assessment_report(
      x = x, checks = checks, issues = issues, evidence = evidence, ids = ids
    ))
  }
  severity <- c(
    high_tail = "warning", silent_region = "warning", co2_region = "warning",
    missing_values = "error", flat_spectrum = "error",
    negative_intensity = "warning", low_snr = "warning", spike = "warning",
    saturation = "warning"
  )
  spike_diag <- attr(x, "automatic_spike")
  saturation_diag <- attr(x, "saturation_restriction")
  rows <- vector("list", length(checks) * length(ids) + 1L)
  cursor <- 0L
  for (check in checks) {
    current_check <- check
    for (i in seq_along(ids)) {
      cursor <- cursor + 1L
      found <- issues[issues$check == current_check &
                        issues$spectrum_index == i, ,
                      drop = FALSE]
      evidence_row <- evidence[
        evidence$check == current_check & evidence$spectrum_index == i,
        , drop = FALSE
      ]
      if (nrow(found) > 0L) {
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
        if (nrow(evidence_row) > 0L) {
          for (field in c("metric", "value", "threshold", "candidate_max",
                          "control_max", "region_min", "region_max")) {
            row[[field]] <- evidence_row[[field]][[1L]]
          }
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
      corrected_spike <- identical(check, "spike") &&
        isTRUE(spike_diag$applied) && ids[[i]] %in% spike_diag$affected_spectra
      corrected_saturation <- identical(check, "saturation") &&
        isTRUE(saturation_diag$applied) &&
        ids[[i]] %in% saturation_diag$affected_spectra
      rejected_spike_regions <- NULL
      if (identical(check, "spike") && is.list(spike_diag) &&
          !isTRUE(spike_diag$applied) &&
          !is.null(spike_diag$rejected_regions)) {
        rejected_spike_regions <- as.data.frame(spike_diag$rejected_regions)
        rejected_spike_regions <- rejected_spike_regions[
          which(rejected_spike_regions[["spectrum_id"]] == ids[[i]]),
          , drop = FALSE
        ]
      }
      rejected_spike <- !is.null(rejected_spike_regions) &&
        nrow(rejected_spike_regions) > 0L
      correction_applied <- corrected_spike || corrected_saturation
      correction_summary <- if (corrected_spike) {
        "Previously detected spikes were corrected successfully."
      } else if (corrected_saturation) {
        "A shared saturated-range restriction was applied successfully."
      } else if (rejected_spike) {
        paste0("Spike correction was not applied: ", spike_diag$reason, ".")
      } else {
        NA_character_
      }
      if (rejected_spike && nrow(found) == 0L) {
        if (identical(status, "pass")) status <- "warning"
        row$issue <- "Previous spike correction was rejected"
        row$description <- correction_summary
        row$likely_cause <- paste(
          "The candidate failed boundary, interpolation, band-protection,",
          "or transactional safeguards."
        )
        row$potential_fix <- paste(
          "Inspect the rejected intervals and raw spectrum; calibrate the",
          "detector or recollect rather than forcing a correction."
        )
        finding_count <- nrow(rejected_spike_regions)
      }
      if (correction_applied && nrow(found) == 0L) {
        row$description <- correction_summary
      }
      detected_regions <- NULL
      if (identical(check, "spike") && is.list(spike_detection) &&
          nrow(spike_detection$candidates) > 0L) {
        candidates <- as.data.frame(spike_detection$candidates)
        detected_regions <- candidates[
          which(candidates[["spectrum_index"]] == i),
          c("region_min", "region_max", "correctable", "reason", "score"),
          drop = FALSE
        ]
      } else if (identical(check, "saturation") &&
                 is.list(saturation_detection) &&
                 nrow(saturation_detection$regions) > 0L) {
        detected <- as.data.frame(saturation_detection$regions)
        detected_regions <- detected[
          which(detected[["spectrum_index"]] == i),
          c("region_min", "region_max", "threshold", "mode"), drop = FALSE
        ]
      }
      if (!is.null(detected_regions)) {
        detected_regions <- unique(detected_regions)
      }
      corrected_regions <- NULL
      if (corrected_spike && nrow(found) == 0L) {
        corrected_regions <- as.data.frame(spike_diag$corrected_regions)
        corrected_regions <- corrected_regions[
          which(corrected_regions[["spectrum_id"]] == ids[[i]]),
          c("region_min", "region_max"),
          drop = FALSE
        ]
      } else if (corrected_saturation && nrow(found) == 0L) {
        corrected_regions <- as.data.frame(saturation_diag$excluded_ranges)[
          , c("region_min", "region_max"), drop = FALSE
        ]
      } else if (rejected_spike && nrow(found) == 0L) {
        corrected_regions <- rejected_spike_regions[
          , c("region_min", "region_max", "reason"), drop = FALSE
        ]
      }
      region <- if (!is.null(detected_regions) && nrow(detected_regions)) {
        detected_regions
      } else if (!is.null(corrected_regions)) {
        as.data.frame(corrected_regions)
      } else if (is.finite(row$region_min[[1L]]) &&
                 is.finite(row$region_max[[1L]])) {
        data.frame(region_min = row$region_min[[1L]],
                   region_max = row$region_max[[1L]])
      } else {
        data.frame(region_min = numeric(), region_max = numeric())
      }
      if (!is.null(detected_regions) && nrow(detected_regions)) {
        finding_count <- nrow(detected_regions)
      } else if (correction_applied && nrow(found) == 0L) {
        finding_count <- nrow(region)
      }
      row$scope <- "spectrum"
      row$status <- status
      row$finding_count <- as.integer(finding_count)
      row$regions <- list(region)
      row$correction_applied <- correction_applied
      row$correction_summary <- correction_summary
      row$test_id <- paste("spectrum", i, ids[[i]], check, sep = ":")
      rows[[cursor]] <- row
    }
  }
  if (is.list(saturation_diag) && !isTRUE(saturation_diag$applied) &&
      !is.null(saturation_diag$reason) && "saturation" %in% checks) {
    cursor <- cursor + 1L
    rows[[cursor]] <- data.table::data.table(
      spectrum_index = NA_integer_, spectrum_id = "batch",
      check = "saturation", issue = "Saturation restriction rejected",
      description = paste0("The shared saturation restriction was not applied: ",
                           saturation_diag$reason, "."),
      likely_cause = "The proposed shared exclusion removed too much coverage or left too few matching points.",
      potential_fix = "Recollect at lower exposure, laser power, or integration time; otherwise interpret identification and quantification cautiously.",
      metric = "saturation_loss_fraction",
      value = saturation_diag$proposed_saturation_loss_fraction,
      threshold = saturation_diag$max_saturation_loss,
      candidate_max = NA_real_, control_max = NA_real_,
      region_min = NA_real_, region_max = NA_real_, scope = "batch",
      status = "error", finding_count = saturation_diag$detected_spectrum_count,
      regions = list(saturation_diag$proposed_excluded_ranges),
      correction_applied = FALSE,
      correction_summary = paste0("Rejected: ", saturation_diag$reason),
      test_id = "batch:batch:saturation"
    )
  }
  data.table::rbindlist(rows[seq_len(cursor)], use.names = TRUE, fill = TRUE)
}

.expand_standard_assessment_report <- function(x, checks, issues, evidence,
                                               ids) {
  count <- length(ids)
  out <- data.table::data.table(
    spectrum_index = rep(seq_len(count), times = length(checks)),
    spectrum_id = rep(ids, times = length(checks)),
    check = rep(checks, each = count)
  )
  keys <- paste(out$check, out$spectrum_index, sep = "\r")
  evidence_index <- match(
    keys, paste(evidence$check, evidence$spectrum_index, sep = "\r")
  )
  issue_index <- match(
    keys, paste(issues$check, issues$spectrum_index, sep = "\r")
  )
  found <- !is.na(issue_index)

  out[, `:=`(
    issue = paste0("No issue detected"),
    description = paste0("The ", gsub("_", " ", check), " check passed."),
    likely_cause = NA_character_,
    potential_fix = "No action required.",
    metric = NA_character_,
    value = NA_real_,
    threshold = NA_real_,
    candidate_max = NA_real_,
    control_max = NA_real_,
    region_min = NA_real_,
    region_max = NA_real_
  )]

  evidence_fields <- c(
    "metric", "value", "threshold", "candidate_max", "control_max",
    "region_min", "region_max"
  )
  has_evidence <- !is.na(evidence_index)
  if (any(has_evidence)) {
    for (field in evidence_fields) {
      data.table::set(
        out, which(has_evidence), field,
        evidence[[field]][evidence_index[has_evidence]]
      )
    }
  }

  issue_fields <- c(
    "issue", "description", "likely_cause", "potential_fix", "metric",
    "value", "threshold", "candidate_max", "control_max", "region_min",
    "region_max"
  )
  if (any(found)) {
    for (field in issue_fields) {
      data.table::set(
        out, which(found), field, issues[[field]][issue_index[found]]
      )
    }
  }

  severity <- c(
    high_tail = "warning", silent_region = "warning",
    co2_region = "warning", missing_values = "error",
    flat_spectrum = "error", negative_intensity = "warning",
    low_snr = "warning"
  )
  out[, `:=`(
    scope = "spectrum",
    status = "pass",
    finding_count = 0L
  )]
  if (any(found)) {
    out$status[found] <- unname(severity[out$check[found]])
    count_metric <- out$metric[found] %in% c(
      "non_finite_count", "spike_count", "saturated_interval_count"
    )
    issue_counts <- rep.int(1L, sum(found))
    finite_counts <- count_metric & is.finite(out$value[found])
    issue_counts[finite_counts] <- as.integer(out$value[found][finite_counts])
    out$finding_count[found] <- issue_counts
  }

  evaluable <- rep(FALSE, nrow(out))
  evaluable[has_evidence] <- evidence$evaluable[evidence_index[has_evidence]]
  unavailable <- !found & !evaluable
  if (any(unavailable)) {
    finite_spectrum <- colSums(is.finite(x$spectra)) > 0L
    out$issue[unavailable] <- "Check unavailable"
    out$description[unavailable] <- paste0(
      "The ", gsub("_", " ", out$check[unavailable]),
      " check could not be evaluated from the available finite data and ",
      "wavenumber coverage."
    )
    no_finite <- !finite_spectrum[out$spectrum_index[unavailable]]
    causes <- rep(
      paste(
        "The spectrum has too few usable points or does not cover the",
        "region required by this check."
      ),
      sum(unavailable)
    )
    causes[no_finite] <- paste(
      "The imported spectrum contains only missing or non-finite",
      "intensity values."
    )
    out$likely_cause[unavailable] <- causes
    out$potential_fix[unavailable] <- paste(
      "Inspect the source file, repair the import, or recollect the",
      "spectrum before interpretation."
    )
    out$status[unavailable] <- "error"
    out$finding_count[unavailable] <- 1L
  }

  empty_region <- data.frame(region_min = numeric(), region_max = numeric())
  regions <- rep(list(empty_region), nrow(out))
  region_rows <- which(is.finite(out$region_min) & is.finite(out$region_max))
  if (length(region_rows)) {
    region_groups <- split(
      region_rows,
      paste(out$region_min[region_rows], out$region_max[region_rows], sep = ":")
    )
    for (rows in region_groups) {
      region <- data.frame(
        region_min = out$region_min[rows[[1L]]],
        region_max = out$region_max[rows[[1L]]]
      )
      regions[rows] <- rep(list(region), length(rows))
    }
  }
  out$regions <- regions
  out$correction_applied <- FALSE
  out$correction_summary <- NA_character_
  out$test_id <- paste(
    "spectrum", out$spectrum_index, out$spectrum_id, out$check, sep = ":"
  )
  out
}

.artifact_ratio_metrics <- function(x, tail_n = 5L,
                                    co2_region = c(2200, 2420),
                                    na.rm = TRUE) {
  spectra <- x$spectra
  nr <- nrow(spectra)
  tail_n <- min(as.integer(tail_n), nr)
  left_rows <- seq_len(tail_n)
  right_rows <- seq.int(max(1L, nr - tail_n + 1L), nr)
  tail_rows <- unique(c(left_rows, right_rows))
  co2_rows <- x$wavenumber >= co2_region[1L] &
    x$wavenumber <= co2_region[2L]
  control_rows <- !(seq_len(nr) %in% tail_rows) & !co2_rows

  normalized <- .normalize_artifact_spectra(spectra)
  col_max <- function(rows) {
    if (!any(rows)) return(rep(NA_real_, ncol(spectra)))
    values <- matrixStats::colMaxs(normalized[rows, , drop = FALSE],
                                   na.rm = na.rm)
    values[!is.finite(values)] <- NA_real_
    as.numeric(values)
  }
  ratio <- function(candidate, control) {
    out <- candidate / control
    out[candidate > 0 & control == 0] <- Inf
    out[candidate == 0 & control == 0] <- 1
    out[is.na(candidate) | is.na(control)] <- NA_real_
    out
  }

  left_max <- col_max(seq_len(nr) %in% left_rows)
  right_max <- col_max(seq_len(nr) %in% right_rows)
  tail_max <- pmax(left_max, right_max, na.rm = TRUE)
  tail_max[!is.finite(tail_max)] <- NA_real_
  co2_max <- col_max(co2_rows)
  control_max <- col_max(control_rows)

  list(
    tail_n = tail_n,
    left_max = left_max,
    right_max = right_max,
    tail_max = tail_max,
    co2_max = co2_max,
    control_max = control_max,
    left_ratio = ratio(left_max, control_max),
    right_ratio = ratio(right_max, control_max),
    tail_ratio = ratio(tail_max, control_max),
    co2_ratio = ratio(co2_max, control_max)
  )
}

.normalize_artifact_spectra <- function(spectra) {
  finite <- spectra
  finite[!is.finite(finite)] <- NA_real_
  mins <- matrixStats::colMins(finite, na.rm = TRUE)
  maxs <- matrixStats::colMaxs(finite, na.rm = TRUE)
  spans <- maxs - mins
  out <- sweep(finite, 2L, mins, "-")
  usable <- is.finite(spans) & spans > 0
  if (any(usable)) {
    out[, usable] <- sweep(out[, usable, drop = FALSE], 2L,
                           spans[usable], "/")
  }
  if (any(!usable)) out[, !usable] <- 0
  out
}
