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
#' \code{"negative_intensity"}, and \code{"low_snr"}.
#' @param high_prob numeric; spectrum-wide quantile used as the high intensity
#' threshold for the silent-region check.
#' @param artifact_ratio numeric; minimum ratio between the normalized maximum
#' in a tail or carbon dioxide region and the normalized maximum outside both
#' artifact regions required to flag an issue.
#' @param tail_n integer; number of points to check at each end of the spectrum.
#' @param silent_region numeric length two; wavenumber range expected to be
#' mostly silent.
#' @param co2_region numeric length two; carbon dioxide wavenumber range.
#' @param snr_threshold numeric; spectra with run signal-to-noise below this
#' value are flagged.
#' @param flat_tol numeric; maximum finite intensity range considered flat.
#' @param negative_tol numeric; minimum allowed intensity before a spectrum is
#' flagged as negative.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating thresholds and metrics.
#' @param \ldots further arguments passed to \code{\link{sig_noise}()} for the
#' \code{"low_snr"} check.
#'
#' @return
#' A \code{\link[data.table]{data.table-class}()} with one row per issue found
#' and columns describing the spectrum, check, issue, likely cause, potential
#' fix, metric value, threshold, and region. If no issues are found, an empty
#' \code{data.table} with the same columns is returned.
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
                                  artifact_ratio = 3,
                                  tail_n = 5L,
                                  silent_region = c(1800, 2000),
                                  co2_region = c(2200, 2420),
                                  snr_threshold = 4,
                                  flat_tol = sqrt(.Machine$double.eps),
                                  negative_tol = 0,
                                  na.rm = TRUE,
                                  ...) {
  x <- as_OpenSpecy(x)
  spectra <- x$spectra

  if (is.complex(spectra))
    stop("assess_spec() requires real-valued spectral intensities",
         call. = FALSE)

  valid_checks <- c("high_tail", "silent_region", "co2_region",
                    "missing_values", "flat_spectrum", "negative_intensity",
                    "low_snr")
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

  issues <- list()

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
  }

  if ("co2_region" %in% checks) {
    co2_ratio <- artifact_metrics$co2_ratio
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
    snr <- sig_noise(x, metric = "run_sig_over_noise", na.rm = na.rm, ...)
    idx <- which(!is.na(snr) & snr < snr_threshold)
    issues[[length(issues) + 1L]] <- .issue_table(
      "low_snr", idx,
      "Low signal-to-noise",
      "The run signal-to-noise metric is below the configured threshold.",
      "Weak sample signal, short acquisition, high detector noise, or poor focus.",
      "Increase acquisition time or accumulations, improve focus, smooth the spectrum, or rerun the measurement.",
      "run_sig_over_noise",
      snr[idx],
      snr_threshold
    )
  }

  issues <- Filter(Negate(is.null), issues)
  if (length(issues) == 0L) return(.empty_assessment())

  rbindlist(issues, use.names = TRUE)
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
