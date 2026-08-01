#' @rdname adj_range
#' @title Range restriction and flattening for spectra
#'
#' @description
#' \code{restrict_range()} restricts wavenumber ranges to user specified values.
#' Multiple ranges can be specified by inputting a series of max and min
#' values in order.
#' \code{flatten_range()} will flatten ranges of the spectra that should have no
#' peaks.
#' Multiple ranges can be specified by inputting the series of max and min
#' values in order.
#'
#' @param x an \code{OpenSpecy} object.
#' @param min a vector of minimum values for the range to be flattened.
#' @param max a vector of maximum values for the range to be flattened.
#' @param make_rel logical; should the output intensities be normalized to the
#' range \[0, 1\] using `make_rel()` function?
#' @param automate logical; if `TRUE`, first assess the relevant artifact and
#' only restrict a high tail or flatten a high CO2 region when detected.
#' @param artifact_ratio numeric; minimum artifact-to-control maximum ratio.
#' @param tail_n integer; number of points defining each spectral tail.
#' @param co2_region numeric length two; carbon dioxide exclusion region used
#' by automatic tail assessment.
#' @param max_crop numeric; maximum fraction of the full wavenumber span that
#' automatic tail restriction may remove across both ends.
#' @param saturation \code{NULL}, \code{"auto"}, or one finite numeric detector
#' ceiling. Non-\code{NULL} values trigger shared saturation restriction.
#' @param saturation_min_run integer or \code{NULL}; minimum adjacent saturated
#' values. The conservative automatic default accepts a two-sample hard
#' plateau; supply an explicit value to calibrate longer plateaus. A known
#' numeric detector ceiling defaults to one sample.
#' @param saturation_tolerance numeric; relative tolerance used to recognize an
#' effectively constant automatic detector plateau.
#' @param saturation_guard integer; sampled points added to both sides of each
#' detected saturated interval before the shared union is removed.
#' @param max_saturation_loss numeric; largest fraction of wavenumber coverage
#' that saturation restriction may remove. The default is 0.70.
#' @param min_remaining integer; minimum retained wavenumber count after
#' saturation restriction.
#' @param \ldots additional arguments passed to subfunctions; currently not
#' in use.
#'
#' @return
#' An \code{OpenSpecy} object with the spectral intensities within specified
#' ranges restricted or flattened.
#'
#' @examples
#' test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
#'                            spectra = data.frame(intensity = rnorm(361)))
#' plot(test_noise)
#'
#' restrict_range(test_noise, min = 1000, max = 2000)
#' restrict_range(test_noise, automate = TRUE, make_rel = FALSE)
#'
#' flattened_intensities <- flatten_range(test_noise, min = c(1000, 2000),
#'                                        max = c(1500, 2500))
#' plot(flattened_intensities)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{conform_spec}()} for conforming wavenumbers to be matched with
#' a reference library;
#' \code{\link{adj_intens}()} for log transformation functions;
#' \code{\link[base]{min}()} and \code{\link[base]{round}()}
#'
#' @importFrom data.table as.data.table .SD
#' @export
restrict_range <- function(x, ...) {
  UseMethod("restrict_range")
}

#' @rdname adj_range
#'
#' @export
restrict_range.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
restrict_range.OpenSpecy <- function(x, min = NULL, max = NULL,
                                     make_rel = TRUE, automate = FALSE,
                                     artifact_ratio = 3, tail_n = 5L,
                                     co2_region = c(2200, 2420),
                                     max_crop = 0.2,
                                     saturation = NULL,
                                     saturation_min_run = NULL,
                                     saturation_tolerance =
                                       sqrt(.Machine$double.eps),
                                     saturation_guard = 1L,
                                     max_saturation_loss = 0.70,
                                     min_remaining = 3L,
                                     ...) {
  x <- as_OpenSpecy(x, compute_file_id = FALSE)

  if (isTRUE(automate)) {
    if (!is.null(saturation)) {
      stop("Apply high-tail and saturation restriction as separate stages",
           call. = FALSE)
    }
    if (!is.null(min) || !is.null(max)) {
      stop("Use either 'min'/'max' or 'automate = TRUE', not both",
           call. = FALSE)
    }
    issues <- assess_spec(
      x,
      checks = "high_tail",
      artifact_ratio = artifact_ratio,
      tail_n = tail_n,
      co2_region = co2_region
    )
    if (nrow(issues) == 0L) return(x)
    out <- .auto_restrict_tail(
      x,
      ratio = artifact_ratio,
      tail_n = tail_n,
      co2_region = co2_region,
      max_crop = max_crop
    )
    if (make_rel && isTRUE(attr(out, "automatic_tail")$applied)) {
      out$spectra <- make_rel(out$spectra)
    }
    return(out)
  }

  manual <- !is.null(min) || !is.null(max)
  manual_breaks <- NULL
  if (manual) {
    if (is.null(min) || is.null(max) || length(min) != length(max) ||
        any(!is.finite(c(min, max))) || any(min > max)) {
      stop("'min' and 'max' must be equal-length finite vectors with min <= max",
           call. = FALSE)
    }
    test <- vapply(seq_along(min), function(y) {
      x$wavenumber >= min[y] & x$wavenumber <= max[y]
    }, FUN.VALUE = logical(length(x$wavenumber)))
    vals <- rowSums(test) > 0
    if (!any(vals)) stop("The requested ranges do not overlap 'x'", call. = FALSE)
    selected <- which(vals)
    manual_breaks <- c(FALSE, diff(selected) != 1L)
    x$wavenumber <- x$wavenumber[vals]
    x$spectra <- x$spectra[vals, , drop = FALSE]
  } else if (is.null(saturation)) {
    stop("'min' and 'max', 'saturation', or 'automate = TRUE' is required",
         call. = FALSE)
  }

  saturation_applied <- FALSE
  if (!is.null(saturation)) {
    x <- .restrict_saturation(
      x,
      saturation = saturation,
      min_run = saturation_min_run,
      tolerance = saturation_tolerance,
      guard = saturation_guard,
      max_loss = max_saturation_loss,
      min_remaining = min_remaining,
      break_before = manual_breaks
    )
    saturation_applied <- isTRUE(attr(x, "saturation_restriction")$applied)
  }

  if (make_rel && (manual || saturation_applied)) {
    x$spectra <- make_rel(x$spectra)
  }

  return(x)
}

.contiguous_true_runs <- function(flag, break_before = NULL) {
  flag <- as.logical(flag)
  if (is.null(break_before)) break_before <- rep(FALSE, length(flag))
  indices <- which(flag)
  if (length(indices) == 0L) {
    return(data.frame(start_index = integer(), end_index = integer()))
  }
  new_group <- c(
    TRUE,
    diff(indices) != 1L | break_before[indices[-1L]]
  )
  groups <- split(indices, cumsum(new_group))
  data.frame(
    start_index = vapply(groups, min, integer(1)),
    end_index = vapply(groups, max, integer(1)),
    row.names = NULL
  )
}

.saturation_axis_breaks <- function(x) {
  breaks <- rep(FALSE, length(x$wavenumber))
  diagnostic <- attr(x, "saturation_restriction")
  if (!isTRUE(diagnostic$applied) || length(x$wavenumber) < 2L ||
      is.null(diagnostic$excluded_ranges)) {
    return(breaks)
  }
  excluded <- as.data.frame(diagnostic$excluded_ranges)
  if (!nrow(excluded) ||
      !all(c("region_min", "region_max") %in% names(excluded))) {
    return(breaks)
  }
  for (i in 2:length(x$wavenumber)) {
    gap <- sort(x$wavenumber[c(i - 1L, i)])
    breaks[[i]] <- any(vapply(seq_len(nrow(excluded)), function(j) {
      removed <- sort(c(excluded$region_min[[j]], excluded$region_max[[j]]))
      removed[[1L]] > gap[[1L]] && removed[[2L]] < gap[[2L]]
    }, logical(1)))
  }
  breaks
}

.detect_saturation <- function(x, saturation = "auto", min_run = NULL,
                               tolerance = sqrt(.Machine$double.eps),
                               break_before = NULL) {
  if (is.character(saturation)) {
    if (length(saturation) != 1L || is.na(saturation) ||
        !identical(saturation, "auto")) {
      stop("'saturation' must be NULL, 'auto', or one finite numeric ceiling",
           call. = FALSE)
    }
    mode <- "auto"
  } else if (is.numeric(saturation) && length(saturation) == 1L &&
             is.finite(saturation)) {
    mode <- "threshold"
  } else {
    stop("'saturation' must be NULL, 'auto', or one finite numeric ceiling",
         call. = FALSE)
  }
  conservative_auto <- identical(mode, "auto") && is.null(min_run)
  if (is.null(min_run)) min_run <- if (mode == "auto") 2L else 1L
  if (!is.numeric(min_run) || length(min_run) != 1L || !is.finite(min_run) ||
      min_run < 1 || min_run != as.integer(min_run)) {
    stop("'saturation_min_run' must be NULL or a positive integer",
         call. = FALSE)
  }
  min_run <- as.integer(min_run)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0) {
    stop("'saturation_tolerance' must be one non-negative finite number",
         call. = FALSE)
  }

  ids <- colnames(x$spectra)
  if (is.null(ids)) ids <- paste0("V", seq_len(ncol(x$spectra)))
  axis_breaks <- .saturation_axis_breaks(x)
  if (!is.null(break_before)) {
    if (!is.logical(break_before) ||
        length(break_before) != length(axis_breaks)) {
      stop("internal saturation segment breaks are invalid", call. = FALSE)
    }
    axis_breaks <- axis_breaks | break_before
  }
  regions <- vector("list", ncol(x$spectra))
  for (column in seq_len(ncol(x$spectra))) {
    values <- x$spectra[, column]
    finite <- is.finite(values)
    if (!any(finite)) next
    threshold <- if (mode == "auto") max(values[finite]) else saturation
    tol_abs <- tolerance * max(1, abs(threshold))
    flag <- if (mode == "auto") {
      finite & abs(values - threshold) <= tol_abs
    } else {
      finite & values >= threshold
    }
    runs <- .contiguous_true_runs(flag, break_before = axis_breaks)
    if (nrow(runs) == 0L) next
    runs <- runs[(runs$end_index - runs$start_index + 1L) >= min_run, ,
                 drop = FALSE]
    if (conservative_auto && nrow(runs) > 0L) {
      run_length <- runs$end_index - runs$start_index + 1L
      runs <- runs[run_length == 2L, , drop = FALSE]
    }
    if (nrow(runs) == 0L) next
    if (mode == "auto") {
      shoulders <- vapply(seq_len(nrow(runs)), function(i) {
        lo <- runs$start_index[[i]]
        hi <- runs$end_index[[i]]
        lo > 1L && !axis_breaks[[lo]] && hi < length(values) &&
          !axis_breaks[[hi + 1L]] && is.finite(values[[lo - 1L]]) &&
          is.finite(values[[hi + 1L]]) &&
          values[[lo - 1L]] < threshold - tol_abs &&
          values[[hi + 1L]] < threshold - tol_abs
      }, FUN.VALUE = logical(1))
      runs <- runs[shoulders, , drop = FALSE]
      if (nrow(runs) == 0L) next
    }
    runs$spectrum_index <- column
    runs$spectrum_id <- ids[[column]]
    runs$region_min <- vapply(seq_len(nrow(runs)), function(i) {
      min(x$wavenumber[runs$start_index[[i]]:runs$end_index[[i]]])
    }, FUN.VALUE = numeric(1))
    runs$region_max <- vapply(seq_len(nrow(runs)), function(i) {
      max(x$wavenumber[runs$start_index[[i]]:runs$end_index[[i]]])
    }, FUN.VALUE = numeric(1))
    runs$threshold <- threshold
    runs$mode <- mode
    regions[[column]] <- runs
  }
  regions <- Filter(Negate(is.null), regions)
  if (length(regions) == 0L) {
    empty <- data.frame(
      start_index = integer(), end_index = integer(),
      spectrum_index = integer(), spectrum_id = character(),
      region_min = numeric(), region_max = numeric(), threshold = numeric(),
      mode = character()
    )
    return(list(mode = mode, min_run = min_run, tolerance = tolerance,
                regions = empty))
  }
  list(mode = mode, min_run = min_run, tolerance = tolerance,
       regions = do.call(rbind, regions))
}

.axis_cell_widths <- function(wavenumber, break_before = NULL) {
  n <- length(wavenumber)
  if (n < 2L) return(rep(0, n))
  if (!is.null(break_before) && any(break_before)) {
    groups <- split(seq_len(n), cumsum(as.logical(break_before)))
    widths <- numeric(n)
    for (indices in groups) {
      widths[indices] <- .axis_cell_widths(wavenumber[indices])
    }
    return(widths)
  }
  gaps <- abs(diff(wavenumber))
  c(gaps[[1L]] / 2,
    if (n > 2L) (gaps[-length(gaps)] + gaps[-1L]) / 2 else numeric(),
    gaps[[length(gaps)]] / 2)
}

.restrict_saturation <- function(x, saturation, min_run = NULL,
                                 tolerance = sqrt(.Machine$double.eps),
                                 guard = 1L, max_loss = 0.70,
                                 min_remaining = 3L,
                                 break_before = NULL) {
  if (!is.numeric(guard) || length(guard) != 1L || !is.finite(guard) ||
      guard < 0 || guard != as.integer(guard)) {
    stop("'saturation_guard' must be a non-negative integer", call. = FALSE)
  }
  guard <- as.integer(guard)
  if (!is.numeric(max_loss) || length(max_loss) != 1L ||
      !is.finite(max_loss) || max_loss < 0 || max_loss > 1) {
    stop("'max_saturation_loss' must be one number in [0, 1]",
         call. = FALSE)
  }
  if (!is.numeric(min_remaining) || length(min_remaining) != 1L ||
      !is.finite(min_remaining) || min_remaining < 2 ||
      min_remaining != as.integer(min_remaining)) {
    stop("'min_remaining' must be an integer of at least two", call. = FALSE)
  }
  min_remaining <- as.integer(min_remaining)

  detection <- .detect_saturation(
    x, saturation, min_run, tolerance, break_before = break_before
  )
  if (nrow(detection$regions) == 0L) return(x)
  coverage_breaks <- .saturation_axis_breaks(x)
  if (!is.null(break_before)) coverage_breaks <- coverage_breaks | break_before
  segment_id <- cumsum(coverage_breaks)
  excluded <- rep(FALSE, length(x$wavenumber))
  for (i in seq_len(nrow(detection$regions))) {
    segment <- segment_id[[detection$regions$start_index[[i]]]]
    segment_rows <- which(segment_id == segment)
    lo <- max(min(segment_rows), detection$regions$start_index[[i]] - guard)
    hi <- min(max(segment_rows), detection$regions$end_index[[i]] + guard)
    excluded[lo:hi] <- TRUE
  }
  merged <- .contiguous_true_runs(excluded, break_before = coverage_breaks)
  merged$region_min <- vapply(seq_len(nrow(merged)), function(i) {
    min(x$wavenumber[merged$start_index[[i]]:merged$end_index[[i]]])
  }, FUN.VALUE = numeric(1))
  merged$region_max <- vapply(seq_len(nrow(merged)), function(i) {
    max(x$wavenumber[merged$start_index[[i]]:merged$end_index[[i]]])
  }, FUN.VALUE = numeric(1))
  retained_ranges <- .contiguous_true_runs(
    !excluded, break_before = coverage_breaks
  )
  retained_ranges$region_min <- vapply(
    seq_len(nrow(retained_ranges)),
    function(i) {
      min(x$wavenumber[
        retained_ranges$start_index[[i]]:retained_ranges$end_index[[i]]
      ])
    },
    FUN.VALUE = numeric(1)
  )
  retained_ranges$region_max <- vapply(
    seq_len(nrow(retained_ranges)),
    function(i) {
      max(x$wavenumber[
        retained_ranges$start_index[[i]]:retained_ranges$end_index[[i]]
      ])
    },
    FUN.VALUE = numeric(1)
  )
  widths <- .axis_cell_widths(x$wavenumber, break_before = coverage_breaks)
  total_width <- sum(widths)
  loss <- if (total_width > 0) sum(widths[excluded]) / total_width else 1
  retained <- sum(!excluded)
  loss_tolerance <- 16 * .Machine$double.eps *
    max(1, abs(loss), abs(max_loss))
  reason <- if (loss > max_loss + loss_tolerance) {
    "exceeds_max_saturation_loss"
  } else if (retained < min_remaining) {
    "insufficient_remaining_points"
  } else {
    "corrected"
  }
  applied <- identical(reason, "corrected")
  full_retained_ranges <- .contiguous_true_runs(
    rep(TRUE, length(x$wavenumber)), break_before = coverage_breaks
  )
  full_retained_ranges$region_min <- vapply(
    seq_len(nrow(full_retained_ranges)),
    function(i) min(x$wavenumber[
      full_retained_ranges$start_index[[i]]:
        full_retained_ranges$end_index[[i]]
    ]),
    numeric(1)
  )
  full_retained_ranges$region_max <- vapply(
    seq_len(nrow(full_retained_ranges)),
    function(i) max(x$wavenumber[
      full_retained_ranges$start_index[[i]]:
        full_retained_ranges$end_index[[i]]
    ]),
    numeric(1)
  )
  actual_excluded_ranges <- if (applied) merged else merged[0L, , drop = FALSE]
  actual_retained_ranges <- if (applied) retained_ranges else full_retained_ranges
  spectrum_ids <- colnames(x$spectra)
  if (is.null(spectrum_ids)) {
    spectrum_ids <- paste0("V", seq_len(ncol(x$spectra)))
  }
  detected_spectra <- unique(detection$regions$spectrum_id)
  affected_spectra <- if (applied) spectrum_ids else character()
  diagnostic <- list(
    mode = detection$mode,
    applied = applied,
    reason = reason,
    threshold = if (identical(detection$mode, "threshold")) {
      saturation
    } else {
      NA_real_
    },
    thresholds = unique(detection$regions$threshold),
    detected_ranges = detection$regions,
    detected_spectra = detected_spectra,
    detected_spectrum_count = length(detected_spectra),
    proposed_excluded_ranges = merged,
    proposed_retained_ranges = retained_ranges,
    proposed_retained_points = retained,
    proposed_saturation_loss_fraction = loss,
    excluded_ranges = actual_excluded_ranges,
    retained_ranges = actual_retained_ranges,
    affected_spectra = affected_spectra,
    affected_spectrum_count = length(affected_spectra),
    detected_interval_count = nrow(detection$regions),
    proposed_excluded_interval_count = nrow(merged),
    proposed_retained_interval_count = nrow(retained_ranges),
    excluded_interval_count = nrow(actual_excluded_ranges),
    retained_interval_count = nrow(actual_retained_ranges),
    saturation_loss_fraction = if (applied) loss else 0,
    max_saturation_loss = max_loss,
    saturation_guard = guard,
    min_remaining = min_remaining,
    retained_points = if (applied) retained else length(excluded),
    original_points = length(excluded),
    axis_signature = if (applied) {
      digest::digest(x$wavenumber[!excluded], algo = "md5")
    } else {
      digest::digest(x$wavenumber, algo = "md5")
    }
  )
  if (!identical(reason, "corrected")) {
    attr(x, "saturation_restriction") <- diagnostic
    warning(
      sprintf(
        paste0("Saturation restriction would remove %.1f%% of spectral ",
               "coverage (maximum %.1f%%) or leave too few points; no ",
               "saturation ranges were removed. Identification and ",
               "quantitative interpretation may be unreliable. Recollect ",
               "with lower exposure, laser power, or integration time."),
        100 * loss, 100 * max_loss
      ),
      call. = FALSE
    )
    return(x)
  }
  out <- x
  out$wavenumber <- x$wavenumber[!excluded]
  out$spectra <- x$spectra[!excluded, , drop = FALSE]
  attr(out, "saturation_restriction") <- diagnostic
  out
}

#' @rdname adj_range
#'
#' @export
flatten_range <- function(x, ...) {
  UseMethod("flatten_range")
}

#' @rdname adj_range
#'
#' @export
flatten_range.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
flatten_range.OpenSpecy <- function(x, min = 2200, max = 2400, make_rel = TRUE,
                                    automate = FALSE, artifact_ratio = 3,
                                    tail_n = 5L,
                                    ...) {
  x <- as_OpenSpecy(x)

  if(length(min) != length(max)) {
    stop("min and max need to be the same length", call. = F)
  }
  if(any(vapply(1:length(min), function(y) {
    min[y] > max[y]
  }, FUN.VALUE = logical(1)))) {
    stop("all min values must be lower than corresponding max", call. = F)
  }
  if (isTRUE(automate)) {
    if (length(min) != 1L) {
      stop("'automate = TRUE' requires one flattening range", call. = FALSE)
    }
    issues <- assess_spec(
      x,
      checks = "co2_region",
      artifact_ratio = artifact_ratio,
      tail_n = tail_n,
      co2_region = c(min, max)
    )
    if (nrow(issues) == 0L) return(x)
  }
  if(all(min > max(x$wavenumber)) ||  all(max < min(x$wavenumber)))
    stop("'min' or 'max' out of range")

  flat <- x$spectra
  for(i in seq_along(min)) {
    rows <- x$wavenumber >= min[i] & x$wavenumber <= max[i]
    left <- min(which(x$wavenumber >= min[i]))
    right <- max(which(x$wavenumber <= max[i]))
    vals <- colMeans(flat[c(left, right), , drop = FALSE])
    flat[rows, ] <- matrix(rep(vals, each = sum(rows)),
                           nrow = sum(rows),
                           ncol = ncol(flat),
                           dimnames = list(NULL, colnames(flat)))
  }

  if (make_rel) x$spectra <- make_rel(flat) else x$spectra <- flat
  if (isTRUE(automate)) {
    attr(x, "automatic_flatten") <- list(
      applied = TRUE,
      region = c(min, max),
      artifact_ratio = artifact_ratio
    )
  }

  return(x)
}

.auto_restrict_tail <- function(x, ratio = 3, tail_n = 5L,
                                co2_region = c(2200, 2420),
                                max_crop = 0.2) {
  if (!is.numeric(ratio) || length(ratio) != 1L || is.na(ratio) ||
      ratio <= 1) {
    stop("'ratio' must be a single numeric value greater than 1",
         call. = FALSE)
  }
  if (!is.numeric(tail_n) || length(tail_n) != 1L || is.na(tail_n) ||
      tail_n < 1) {
    stop("'tail_n' must be a positive integer", call. = FALSE)
  }
  tail_n <- as.integer(tail_n)
  if (!is.numeric(co2_region) || length(co2_region) != 2L ||
      any(!is.finite(co2_region)) || co2_region[1L] > co2_region[2L]) {
    stop("'co2_region' must contain a finite minimum and maximum",
         call. = FALSE)
  }
  if (!is.numeric(max_crop) || length(max_crop) != 1L || is.na(max_crop) ||
      max_crop < 0 || max_crop >= 1) {
    stop("'max_crop' must be a single numeric value in [0, 1)",
         call. = FALSE)
  }

  original <- x
  original_span <- diff(range(x$wavenumber))
  if (!is.finite(original_span) || original_span <= 0 ||
      nrow(x$spectra) <= 2L * tail_n) {
    attr(original, "automatic_tail") <- list(
      applied = FALSE, reason = "insufficient_range", crop_fraction = 0
    )
    return(original)
  }

  lo <- 1L
  hi <- nrow(x$spectra)
  repeat {
    current <- x
    keep <- seq.int(lo, hi)
    current$wavenumber <- x$wavenumber[keep]
    current$spectra <- x$spectra[keep, , drop = FALSE]
    metrics <- .artifact_ratio_metrics(
      current, tail_n = tail_n, co2_region = co2_region
    )
    trim_left <- any(metrics$left_ratio >= ratio, na.rm = TRUE)
    trim_right <- any(metrics$right_ratio >= ratio, na.rm = TRUE)
    if (!trim_left && !trim_right) break

    next_lo <- lo + as.integer(trim_left)
    next_hi <- hi - as.integer(trim_right)
    if (next_hi - next_lo + 1L <= 2L * tail_n) {
      attr(original, "automatic_tail") <- list(
        applied = FALSE, reason = "insufficient_range", crop_fraction = 0
      )
      return(original)
    }
    removed_span <- abs(x$wavenumber[next_lo] - x$wavenumber[1L]) +
      abs(x$wavenumber[nrow(x$spectra)] - x$wavenumber[next_hi])
    crop_fraction <- removed_span / original_span
    if (crop_fraction > max_crop) {
      attr(original, "automatic_tail") <- list(
        applied = FALSE, reason = "max_crop_exceeded",
        crop_fraction = crop_fraction
      )
      return(original)
    }
    lo <- next_lo
    hi <- next_hi
  }

  if (lo == 1L && hi == nrow(x$spectra)) return(original)
  keep <- seq.int(lo, hi)
  out <- x
  out$wavenumber <- x$wavenumber[keep]
  out$spectra <- x$spectra[keep, , drop = FALSE]
  removed_span <- abs(x$wavenumber[lo] - x$wavenumber[1L]) +
    abs(x$wavenumber[nrow(x$spectra)] - x$wavenumber[hi])
  attr(out, "automatic_tail") <- list(
    applied = TRUE, reason = "corrected",
    crop_fraction = removed_span / original_span,
    original_range = range(x$wavenumber),
    corrected_range = range(out$wavenumber)
  )
  out
}

.flatten_range <- function(y, x, min, max) {
  if(all(min > max(x)) ||  all(max < min(x)))
    stop("'min' or 'max' out of range")

  for(i in 1:length(min)) {
    y[x >= min[i] & x <= max[i]] <-
      mean(c(y[min(which(x >= min[i]))],
             y[max(which(x <= max[i]))]))
  }
  return(y)
}

apmdt <- function(spectra,  ...){
    if(is.data.table(spectra)){
        spectra <- .as_spectra_matrix(spectra, message_conversion = TRUE)
    }
    if(is.matrix(spectra) && length(dim(spectra)) == 2L){
        return(.apply_spectra(spectra, ...))
    }
    else{
        stop("Spectra needs to be either a 2D matrix or a data.table")
    }

}
