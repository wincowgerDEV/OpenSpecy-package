#' @rdname correct_spike
#' @title Detect and correct isolated spikes in spectra
#'
#' @description
#' `correct_spike()` detects isolated positive or negative intensity artifacts
#' and replaces only accepted spike intervals with local interpolation. The
#' default residual method compares each point with a wavenumber-aware local
#' interpolation and scales the residual by a local median absolute deviation.
#' Two paper-backed methods use peak prominence and width measured in sample
#' (CCD-pixel) units.
#'
#' @details
#' `method = "prominence_fwhm"` requires user-supplied
#' `prominence_threshold` and `width_threshold` values. These thresholds depend
#' on the material, instrument, spectral resolution, and acquisition settings;
#' the graphene values reported by Coca-Lopez are deliberately not universal
#' defaults. `method = "prominence_fwhm_ratio"` instead treats
#' prominence/FWHM values above `z_threshold` standard deviations as spikes and
#' requires at least `min_peaks` measurable peaks.
#'
#' Peak widths and flagged intervals follow the prominence contour definition
#' used by `scipy.signal.peak_widths()`: FWHM is measured at
#' `rel_height = 0.5`, while the interval replaced is measured at the requested
#' `rel_height`. Corrections require `interpolation_points` finite, unflagged
#' samples on both sides; boundary values are never wrapped. Close spike
#' intervals are merged before interpolation so one spike cannot be used to
#' repair another. Linear interpolation that materially disagrees with a local
#' quadratic reconstruction over a multi-point interval is rejected to avoid
#' silently truncating an underlying broad band.
#'
#' Correction proceeds through bounded transactional passes while the detector's
#' correctable count strictly decreases. This lets a newly revealed spike be
#' corrected without rolling back safe earlier replacements. Processing stops
#' on no progress; boundary, interpolation, and band-protection safeguards stay
#' in force, and any remaining safeguarded candidates are recorded rather than
#' forced.
#'
#' No single-spectrum method can always distinguish a cosmic-ray spike from a
#' genuine band with the same shape. Calibrate paper thresholds on representative
#' standards, especially for narrow-band materials such as calcite and
#' polystyrene, and inspect the `automatic_spike` diagnostic attribute.
#'
#' @param x an `OpenSpecy` object.
#' @param method character; detection method. One of `"residual"`,
#'   `"prominence_fwhm"`, or `"prominence_fwhm_ratio"`.
#' @param direction character; detect `"both"` positive and negative spikes,
#'   only `"positive"` spikes, or only `"negative"` spikes.
#' @param residual_window positive integer; points on each side used by the
#'   local residual predictor.
#' @param residual_threshold positive numeric; absolute robust residual score
#'   required by the residual method.
#' @param residual_max_width positive integer; widest consecutive candidate
#'   interval accepted by the residual method. The one-point default is
#'   deliberately conservative.
#' @param prominence_threshold positive numeric or `NULL`; minimum peak
#'   prominence for the manual prominence/FWHM method.
#' @param width_threshold positive numeric or `NULL`; maximum peak FWHM in
#'   sample (CCD-pixel) units for the manual prominence/FWHM method.
#' @param rel_height numeric in `(0, 1]`; prominence fraction at which the
#'   interval replaced by paper methods is measured. Coca-Lopez used `0.8` for
#'   most examples.
#' @param interpolation_points positive integer; finite, unflagged neighboring
#'   points required on each side of an accepted interval. This is the paper's
#'   `m` parameter.
#' @param interpolation character; `"linear"` (default) or `"quadratic"`
#'   local interpolation.
#' @param z_threshold positive numeric; upper Z-score threshold for automated
#'   prominence/FWHM-ratio detection. The paper uses values greater than `3.5`.
#' @param min_peaks integer of at least two; minimum number of measurable peaks
#'   used to estimate automated ratio outliers.
#' @param \ldots must be empty. Unexpected arguments are rejected so detector
#'   tuning misspellings cannot be silently ignored.
#'
#' @return
#' An `OpenSpecy` object with accepted spike intervals corrected. The
#' wavenumber axis, spectra dimensions and names, metadata alignment, and
#' existing attributes are preserved. A successful or rejected attempted
#' correction stores an `automatic_spike` attribute containing the method,
#' parameters, corrected and rejected regions, affected spectra, detector
#' counts, pass count, and transaction reason. If nothing is detected, `x` is
#' returned unchanged.
#'
#' @examples
#' wave <- seq(400, 1800, length.out = 101)
#' values <- sin(wave / 200)
#' values[51] <- values[51] + 20
#' spectrum <- as_OpenSpecy(wave, data.frame(sample = values))
#' corrected <- correct_spike(spectrum)
#'
#' @references
#' Coca-Lopez N (2024). "An intuitive approach for spike removal in Raman
#' spectra based on peaks' prominence and width." *Analytica Chimica Acta*,
#' **1295**, 342312. \doi{10.1016/j.aca.2024.342312}.
#'
#' @author Win Cowger
#' @export
correct_spike <- function(x, ...) {
  UseMethod("correct_spike")
}

#' @rdname correct_spike
#' @export
correct_spike.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = FALSE)
}

#' @rdname correct_spike
#' @export
correct_spike.OpenSpecy <- function(
    x,
    method = c("residual", "prominence_fwhm", "prominence_fwhm_ratio"),
    direction = c("both", "positive", "negative"),
    residual_window = 5L,
    residual_threshold = 8,
    residual_max_width = 1L,
    prominence_threshold = NULL,
    width_threshold = NULL,
    rel_height = 0.8,
    interpolation_points = 10L,
    interpolation = c("linear", "quadratic"),
    z_threshold = 3.5,
    min_peaks = 20L,
    ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names)) dot_names <- rep.int("", length(dots))
    labels <- ifelse(
      nzchar(dot_names),
      paste0("'", dot_names, "'"),
      paste0("argument ", seq_along(dots))
    )
    stop(
      "unused argument(s) supplied to correct_spike.OpenSpecy(): ",
      paste(labels, collapse = ", "),
      call. = FALSE
    )
  }
  x <- as_OpenSpecy(x, compute_file_id = FALSE)
  method <- match.arg(method)
  direction <- match.arg(direction)
  interpolation <- match.arg(interpolation)

  parameters <- .validate_spike_parameters(
    method = method,
    direction = direction,
    residual_window = residual_window,
    residual_threshold = residual_threshold,
    residual_max_width = residual_max_width,
    prominence_threshold = prominence_threshold,
    width_threshold = width_threshold,
    rel_height = rel_height,
    interpolation_points = interpolation_points,
    z_threshold = z_threshold,
    min_peaks = min_peaks
  )
  .validate_spike_input(x)

  detection <- do.call(
    .detect_spikes,
    c(list(x = x), parameters)
  )

  # Preserve a byte-for-byte no-op, including attributes, when the detector did
  # not identify a candidate. This also makes a completed correction idempotent.
  if (detection$candidate_count == 0L) return(x)

  diagnostic_parameters <- c(
    detection$parameters,
    list(interpolation = interpolation)
  )
  original <- x
  previous_diagnostic <- attr(original, "automatic_spike", exact = TRUE)
  initial_correctable_count <- detection$correctable_count
  initial_candidate_count <- detection$candidate_count
  attempt_signature <- .spike_result_signature(
    original, method, diagnostic_parameters
  )
  current <- x
  corrected <- rejected <- .empty_spike_regions()
  pass_count <- 0L
  stop_reason <- "no_correctable_regions"
  # Each accepted pass strictly decreases this non-negative count, so the
  # detected state supplies a finite bound without another public argument.
  maximum_passes <- max(1L, initial_correctable_count)

  repeat {
    rejected <- data.table::rbindlist(
      list(rejected, .candidate_rejected_regions(detection$candidates)),
      use.names = TRUE
    )
    regions <- .merge_spike_regions(detection$candidates)
    if (nrow(regions) == 0L) {
      stop_reason <- "no_correctable_regions"
      break
    }

    prepared <- .prepare_spike_corrections(
      x = current,
      regions = regions,
      flagged = detection$flagged,
      interpolation_points = parameters$interpolation_points,
      interpolation = interpolation
    )
    rejected <- data.table::rbindlist(
      list(rejected, prepared$rejected), use.names = TRUE
    )
    if (nrow(prepared$accepted) == 0L) {
      stop_reason <- "no_correctable_regions"
      break
    }

    trial <- current
    for (replacement in prepared$replacements) {
      trial$spectra[replacement$rows, replacement$spectrum_index] <-
        replacement$values
    }
    transaction_reason <- .validate_spike_transaction(
      original = current,
      trial = trial,
      flagged = detection$flagged
    )
    after <- do.call(
      .detect_spikes,
      c(list(x = trial), parameters)
    )
    if (is.null(transaction_reason) &&
        after$correctable_count >= detection$correctable_count) {
      transaction_reason <- "detector_count_not_reduced"
    }
    if (!is.null(transaction_reason)) {
      rolled_back <- data.table::copy(prepared$accepted)
      data.table::set(
        rolled_back, j = "reason", value = transaction_reason
      )
      rejected <- data.table::rbindlist(
        list(rejected, rolled_back), use.names = TRUE
      )
      stop_reason <- transaction_reason
      break
    }

    accepted <- data.table::copy(prepared$accepted)
    data.table::set(accepted, j = "reason", value = "corrected")
    corrected <- data.table::rbindlist(
      list(corrected, accepted), use.names = TRUE
    )
    current <- trial
    detection <- after
    pass_count <- pass_count + 1L
    if (detection$correctable_count == 0L ||
        detection$candidate_count == 0L) {
      stop_reason <- "corrected"
      break
    }
    if (pass_count >= maximum_passes) {
      stop_reason <- "iteration_limit"
      break
    }
  }

  corrected <- unique(corrected)
  rejected <- unique(rejected)
  if (nrow(corrected) == 0L) {
    # Repeating a completed correction with only the same safeguarded residual
    # candidate is an exact no-op, including its prior diagnostic attribute.
    if (is.list(previous_diagnostic) &&
        isTRUE(previous_diagnostic$applied) &&
        identical(previous_diagnostic$result_signature, attempt_signature)) {
      return(original)
    }
    attr(original, "automatic_spike") <- .spike_diagnostic(
      applied = FALSE,
      method = method,
      parameters = diagnostic_parameters,
      corrected = corrected,
      rejected = rejected,
      before_count = initial_correctable_count,
      after_count = detection$correctable_count,
      candidate_count = initial_candidate_count,
      pass_count = pass_count,
      reason = stop_reason,
      result_signature = attempt_signature
    )
    return(original)
  }

  final_reason <- if (detection$correctable_count > 0L ||
                      nrow(rejected) > 0L) {
    "corrected_with_safeguards"
  } else {
    "corrected"
  }
  attr(current, "automatic_spike") <- .spike_diagnostic(
    applied = TRUE,
    method = method,
    parameters = diagnostic_parameters,
    corrected = corrected,
    rejected = rejected,
    before_count = initial_correctable_count,
    after_count = detection$correctable_count,
    candidate_count = initial_candidate_count,
    pass_count = pass_count,
    reason = final_reason,
    result_signature = .spike_result_signature(
      current, method, diagnostic_parameters
    )
  )
  current
}

.validate_spike_input <- function(x) {
  if (!inherits(x, "OpenSpecy") || !is.numeric(x$wavenumber) ||
      !is.matrix(x$spectra) || !is.numeric(x$spectra)) {
    stop("'x' must be a numeric OpenSpecy object", call. = FALSE)
  }
  if (is.complex(x$spectra)) {
    stop("correct_spike() requires real-valued spectral intensities",
         call. = FALSE)
  }
  if (length(x$wavenumber) != nrow(x$spectra) ||
      nrow(x$metadata) != ncol(x$spectra)) {
    stop("OpenSpecy wavenumber, spectra, and metadata dimensions are not aligned",
         call. = FALSE)
  }
  axis_steps <- diff(x$wavenumber)
  strictly_monotonic <- length(axis_steps) > 0L &&
    (all(axis_steps > 0) || all(axis_steps < 0))
  if (length(x$wavenumber) < 3L || any(!is.finite(x$wavenumber)) ||
      !strictly_monotonic) {
    stop("'x$wavenumber' must be a finite, strictly monotonic axis",
         call. = FALSE)
  }
  invisible(NULL)
}

.validate_spike_parameters <- function(
    method,
    direction,
    residual_window,
    residual_threshold,
    residual_max_width,
    prominence_threshold,
    width_threshold,
    rel_height,
    interpolation_points,
    z_threshold,
    min_peaks) {
  positive_integer <- function(value, name, minimum = 1L) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < minimum || value != as.integer(value)) {
      stop("'", name, "' must be a single integer of at least ", minimum,
           call. = FALSE)
    }
    as.integer(value)
  }
  positive_number <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value <= 0) {
      stop("'", name, "' must be a single positive finite number",
           call. = FALSE)
    }
    as.numeric(value)
  }

  residual_window <- positive_integer(residual_window, "residual_window")
  residual_threshold <- positive_number(
    residual_threshold, "residual_threshold"
  )
  residual_max_width <- positive_integer(
    residual_max_width, "residual_max_width"
  )
  interpolation_points <- positive_integer(
    interpolation_points, "interpolation_points"
  )
  z_threshold <- positive_number(z_threshold, "z_threshold")
  min_peaks <- positive_integer(min_peaks, "min_peaks", minimum = 2L)

  if (!is.numeric(rel_height) || length(rel_height) != 1L ||
      !is.finite(rel_height) || rel_height <= 0 || rel_height > 1) {
    stop("'rel_height' must be a single finite number in (0, 1]",
         call. = FALSE)
  }
  if (method == "prominence_fwhm") {
    if (is.null(prominence_threshold) || is.null(width_threshold)) {
      stop("'prominence_threshold' and 'width_threshold' are required for ",
           "method = 'prominence_fwhm'", call. = FALSE)
    }
    prominence_threshold <- positive_number(
      prominence_threshold, "prominence_threshold"
    )
    width_threshold <- positive_number(width_threshold, "width_threshold")
  } else {
    if (!is.null(prominence_threshold)) {
      prominence_threshold <- positive_number(
        prominence_threshold, "prominence_threshold"
      )
    }
    if (!is.null(width_threshold)) {
      width_threshold <- positive_number(width_threshold, "width_threshold")
    }
  }

  list(
    method = method,
    direction = direction,
    residual_window = residual_window,
    residual_threshold = residual_threshold,
    residual_max_width = residual_max_width,
    prominence_threshold = prominence_threshold,
    width_threshold = width_threshold,
    rel_height = as.numeric(rel_height),
    interpolation_points = interpolation_points,
    z_threshold = z_threshold,
    min_peaks = min_peaks
  )
}

# Shared detector contract used by correct_spike() and assess_spec(). It returns
# a stable candidate table plus a logical matrix marking correctable intervals.
.detect_spikes <- function(
    x,
    method = c("residual", "prominence_fwhm", "prominence_fwhm_ratio"),
    direction = c("both", "positive", "negative"),
    residual_window = 5L,
    residual_threshold = 8,
    residual_max_width = 1L,
    prominence_threshold = NULL,
    width_threshold = NULL,
    rel_height = 0.8,
    interpolation_points = 10L,
    z_threshold = 3.5,
    min_peaks = 20L) {
  x <- as_OpenSpecy(x, compute_file_id = FALSE)
  method <- match.arg(method)
  direction <- match.arg(direction)
  parameters <- .validate_spike_parameters(
    method = method,
    direction = direction,
    residual_window = residual_window,
    residual_threshold = residual_threshold,
    residual_max_width = residual_max_width,
    prominence_threshold = prominence_threshold,
    width_threshold = width_threshold,
    rel_height = rel_height,
    interpolation_points = interpolation_points,
    z_threshold = z_threshold,
    min_peaks = min_peaks
  )
  .validate_spike_input(x)

  ids <- colnames(x$spectra)
  if (is.null(ids)) ids <- paste0("V", seq_len(ncol(x$spectra)))
  flagged <- matrix(
    FALSE,
    nrow = nrow(x$spectra),
    ncol = ncol(x$spectra),
    dimnames = dimnames(x$spectra)
  )

  if (method == "residual") {
    candidates <- .detect_residual_spikes(x, ids, parameters)
    reason <- if (nrow(candidates) == 0L) "no_candidates" else "detected"
  } else {
    paper <- .detect_prominence_spikes(x, ids, parameters)
    candidates <- paper$candidates
    reason <- paper$reason
  }

  if (nrow(candidates) > 0L && any(candidates$correctable)) {
    accepted <- candidates[candidates$correctable]
    for (i in seq_len(nrow(accepted))) {
      flagged[
        seq.int(accepted$start_index[i], accepted$end_index[i]),
        accepted$spectrum_index[i]
      ] <- TRUE
    }
  }

  structure(
    list(
      method = method,
      parameters = parameters,
      candidates = candidates,
      flagged = flagged,
      candidate_count = nrow(candidates),
      correctable_count = as.integer(sum(candidates$correctable)),
      reason = reason
    ),
    class = "OpenSpecy_spike_detection"
  )
}

.empty_spike_candidates <- function() {
  data.table::data.table(
    spectrum_index = integer(),
    spectrum_id = character(),
    direction = character(),
    peak_index = integer(),
    peak_wavenumber = numeric(),
    start_index = integer(),
    end_index = integer(),
    region_min = numeric(),
    region_max = numeric(),
    residual = numeric(),
    score = numeric(),
    prominence = numeric(),
    width = numeric(),
    prominence_width_ratio = numeric(),
    correctable = logical(),
    reason = character()
  )
}

.detect_residual_spikes <- function(x, ids, parameters) {
  records <- list()
  record_index <- 0L
  for (spectrum_index in seq_len(ncol(x$spectra))) {
    metrics <- .local_residual_metrics(
      x$wavenumber,
      x$spectra[, spectrum_index],
      window = parameters$residual_window
    )
    selected <- switch(
      parameters$direction,
      both = abs(metrics$score) >= parameters$residual_threshold,
      positive = metrics$score >= parameters$residual_threshold,
      negative = metrics$score <= -parameters$residual_threshold
    )
    selected[is.na(selected)] <- FALSE
    runs <- .logical_runs(selected)
    if (nrow(runs) == 0L) next

    for (i in seq_len(nrow(runs))) {
      rows <- seq.int(runs$start[i], runs$end[i])
      peak <- rows[which.max(abs(metrics$score[rows]))]
      width <- length(rows)
      correctable <- TRUE
      reason <- "detected"
      if (width > parameters$residual_max_width) {
        correctable <- FALSE
        reason <- "candidate_too_wide"
      } else if (runs$start[i] <= parameters$interpolation_points ||
                 runs$end[i] >
                   nrow(x$spectra) - parameters$interpolation_points) {
        correctable <- FALSE
        reason <- "boundary_interval"
      }
      signed_direction <- if (metrics$residual[peak] >= 0) {
        "positive"
      } else {
        "negative"
      }
      record_index <- record_index + 1L
      records[[record_index]] <- list(
        spectrum_index = as.integer(spectrum_index),
        spectrum_id = ids[spectrum_index],
        direction = signed_direction,
        peak_index = as.integer(peak),
        peak_wavenumber = x$wavenumber[peak],
        start_index = as.integer(runs$start[i]),
        end_index = as.integer(runs$end[i]),
        region_min = min(x$wavenumber[rows]),
        region_max = max(x$wavenumber[rows]),
        residual = metrics$residual[peak],
        score = metrics$score[peak],
        prominence = NA_real_,
        width = as.numeric(width),
        prominence_width_ratio = NA_real_,
        correctable = correctable,
        reason = reason
      )
    }
  }
  if (length(records) == 0L) return(.empty_spike_candidates())
  candidates <- data.table::rbindlist(records, use.names = TRUE)

  # A resolved spectral band can produce a wide residual run at its apex and
  # paired, opposite-sign residuals on the shoulders. Once the apex is rejected
  # by the conservative width rule, do not silently "correct" those shoulders.
  wide <- which(candidates$reason == "candidate_too_wide")
  if (length(wide) > 0L) {
    for (i in wide) {
      same_spectrum <- candidates$spectrum_index ==
        candidates$spectrum_index[i]
      opposite <- candidates$direction != candidates$direction[i]
      distance <- pmax(
        candidates$start_index - candidates$end_index[i],
        candidates$start_index[i] - candidates$end_index,
        0L
      )
      shoulders <- which(
        same_spectrum & opposite &
          distance <= parameters$residual_window &
          candidates$correctable
      )
      if (length(shoulders) > 0L) {
        candidates$correctable[shoulders] <- FALSE
        candidates$reason[shoulders] <- "spectral_band_shoulder"
      }
    }
  }
  candidates
}

.local_residual_metrics <- function(axis, values, window = 5L) {
  n <- length(values)
  predicted <- residual <- score <- rep(NA_real_, n)
  if (n <= 2L * window) {
    return(list(predicted = predicted, residual = residual, score = score))
  }

  # The app commonly processes complete map matrices. Vectorize that dominant
  # path across candidate positions while retaining the pointwise fallback
  # below for spectra with non-finite values. The calculations intentionally
  # use the same left-to-right chords and local MAD definition as the fallback.
  if (all(is.finite(values)) && all(is.finite(axis))) {
    return(.local_residual_metrics_complete(axis, values, window))
  }

  for (i in seq.int(window + 1L, n - window)) {
    if (!is.finite(values[i])) next
    left <- seq.int(i - window, i - 1L)
    right <- seq.int(i + 1L, i + window)
    left <- left[is.finite(values[left])]
    right <- right[is.finite(values[right])]
    if (length(left) < ceiling(window / 2) ||
        length(right) < ceiling(window / 2)) next

    # Evaluate every left-to-right interpolation chord at the candidate
    # wavenumber and take their median. This remains wavenumber-aware on an
    # irregular axis while a single contaminated neighbor affects only a
    # minority of the window's predictions.
    chord_predictions <- unlist(lapply(left, function(left_index) {
      values[left_index] +
        (values[right] - values[left_index]) *
        (axis[i] - axis[left_index]) /
        (axis[right] - axis[left_index])
    }), use.names = FALSE)
    chord_predictions <- chord_predictions[is.finite(chord_predictions)]
    if (length(chord_predictions) == 0L) next
    predicted[i] <- stats::median(chord_predictions)
    residual[i] <- values[i] - predicted[i]
  }

  finite <- is.finite(residual)
  if (!any(finite)) {
    return(list(predicted = predicted, residual = residual, score = score))
  }
  global_center <- stats::median(residual[finite])
  global_scale <- stats::mad(
    residual[finite], center = global_center, constant = 1.4826
  )
  intensity_scale <- max(abs(values[is.finite(values)]), 1)
  numerical_floor <- sqrt(.Machine$double.eps) * intensity_scale
  if (!is.finite(global_scale)) global_scale <- 0
  scale_window <- max(2L * window, 5L)

  for (i in which(finite)) {
    neighborhood <- seq.int(max(1L, i - scale_window),
                            min(n, i + scale_window))
    neighborhood <- setdiff(neighborhood, i)
    local <- residual[neighborhood]
    local <- local[is.finite(local)]
    center <- if (length(local) >= 3L) stats::median(local) else global_center
    local_scale <- if (length(local) >= 3L) {
      stats::mad(local, center = center, constant = 1.4826)
    } else {
      global_scale
    }
    denominator <- max(local_scale, global_scale * 0.25, numerical_floor)
    score[i] <- (residual[i] - center) / denominator
  }
  list(predicted = predicted, residual = residual, score = score)
}

.local_residual_metrics_complete <- function(axis, values, window) {
  n <- length(values)
  predicted <- residual <- score <- rep(NA_real_, n)
  centers <- seq.int(window + 1L, n - window)

  chord_predictions <- matrix(
    NA_real_,
    nrow = length(centers),
    ncol = window * window
  )
  column <- 0L
  # Match the fallback's ordering: oldest-to-newest left points, then
  # nearest-to-farthest right points for each left point.
  for (left_offset in rev(seq_len(window))) {
    left <- centers - left_offset
    for (right_offset in seq_len(window)) {
      right <- centers + right_offset
      column <- column + 1L
      chord_predictions[, column] <- values[left] +
        (values[right] - values[left]) *
        (axis[centers] - axis[left]) /
        (axis[right] - axis[left])
    }
  }
  predicted[centers] <- matrixStats::rowMedians(chord_predictions)
  residual[centers] <- values[centers] - predicted[centers]

  global_center <- stats::median(residual[centers])
  global_scale <- stats::mad(
    residual[centers], center = global_center, constant = 1.4826
  )
  intensity_scale <- max(abs(values), 1)
  numerical_floor <- sqrt(.Machine$double.eps) * intensity_scale
  if (!is.finite(global_scale)) global_scale <- 0

  scale_window <- max(2L * window, 5L)
  offsets <- c(seq.int(-scale_window, -1L), seq_len(scale_window))
  neighbor_indices <- outer(centers, offsets, `+`)
  valid <- neighbor_indices >= 1L & neighbor_indices <= n
  neighbor_residuals <- matrix(
    NA_real_,
    nrow = nrow(neighbor_indices),
    ncol = ncol(neighbor_indices)
  )
  neighbor_residuals[valid] <- residual[neighbor_indices[valid]]
  usable <- rowSums(is.finite(neighbor_residuals)) >= 3L

  local_center <- rep(global_center, length(centers))
  local_scale <- rep(global_scale, length(centers))
  if (any(usable)) {
    local_center[usable] <- matrixStats::rowMedians(
      neighbor_residuals[usable, , drop = FALSE], na.rm = TRUE
    )
    deviations <- abs(
      neighbor_residuals[usable, , drop = FALSE] - local_center[usable]
    )
    local_scale[usable] <- matrixStats::rowMedians(
      deviations, na.rm = TRUE
    ) * 1.4826
  }

  denominator <- pmax(
    local_scale,
    global_scale * 0.25,
    numerical_floor
  )
  score[centers] <- (residual[centers] - local_center) / denominator
  list(predicted = predicted, residual = residual, score = score)
}

.logical_runs <- function(values) {
  indices <- which(values)
  if (length(indices) == 0L) {
    return(data.frame(start = integer(), end = integer()))
  }
  groups <- cumsum(c(TRUE, diff(indices) != 1L))
  data.frame(
    start = as.integer(vapply(split(indices, groups), min, numeric(1))),
    end = as.integer(vapply(split(indices, groups), max, numeric(1)))
  )
}

.detect_prominence_spikes <- function(x, ids, parameters) {
  directions <- if (parameters$direction == "both") {
    c("positive", "negative")
  } else {
    parameters$direction
  }
  features <- list()
  feature_index <- 0L

  for (spectrum_index in seq_len(ncol(x$spectra))) {
    original <- x$spectra[, spectrum_index]
    for (candidate_direction in directions) {
      signal <- if (candidate_direction == "positive") original else -original
      peaks <- .spike_local_peaks(signal)
      if (length(peaks) == 0L) next
      for (peak in peaks) {
        feature <- .spike_peak_feature(
          signal = signal,
          peak = peak,
          rel_height = parameters$rel_height
        )
        if (is.null(feature) || !is.finite(feature$width) ||
            feature$width <= 0 || !is.finite(feature$prominence) ||
            feature$prominence <= 0) next
        start <- max(1L, ceiling(feature$interval_left))
        end <- min(length(signal), floor(feature$interval_right))
        if (start > peak) start <- peak
        if (end < peak) end <- peak
        feature_index <- feature_index + 1L
        features[[feature_index]] <- list(
          spectrum_index = as.integer(spectrum_index),
          spectrum_id = ids[spectrum_index],
          direction = candidate_direction,
          peak_index = as.integer(peak),
          peak_wavenumber = x$wavenumber[peak],
          start_index = as.integer(start),
          end_index = as.integer(end),
          region_min = min(x$wavenumber[seq.int(start, end)]),
          region_max = max(x$wavenumber[seq.int(start, end)]),
          residual = NA_real_,
          score = NA_real_,
          prominence = feature$prominence,
          width = feature$width,
          prominence_width_ratio = feature$prominence / feature$width,
          correctable = FALSE,
          reason = "not_selected"
        )
      }
    }
  }
  if (length(features) == 0L) {
    return(list(candidates = .empty_spike_candidates(),
                reason = "no_candidates"))
  }
  features <- data.table::rbindlist(features, use.names = TRUE)

  if (parameters$method == "prominence_fwhm") {
    features$score <- pmin(
      features$prominence / parameters$prominence_threshold,
      parameters$width_threshold / features$width
    )
    selected <- features$prominence >= parameters$prominence_threshold &
      features$width <= parameters$width_threshold
    reason <- if (any(selected)) "detected" else "no_candidates"
  } else {
    usable <- is.finite(features$prominence_width_ratio)
    if (sum(usable) < parameters$min_peaks) {
      return(list(candidates = .empty_spike_candidates(),
                  reason = "insufficient_peaks"))
    }
    ratios <- features$prominence_width_ratio[usable]
    ratio_sd <- stats::sd(ratios)
    if (!is.finite(ratio_sd) || ratio_sd <= 0) {
      return(list(candidates = .empty_spike_candidates(),
                  reason = "insufficient_ratio_variation"))
    }
    features$score[usable] <-
      (ratios - mean(ratios)) / ratio_sd
    selected <- is.finite(features$score) &
      features$score > parameters$z_threshold
    reason <- if (any(selected)) "detected" else "no_candidates"
  }

  candidates <- features[selected]
  if (nrow(candidates) == 0L) {
    return(list(candidates = .empty_spike_candidates(), reason = reason))
  }
  boundary <- candidates$start_index <= parameters$interpolation_points |
    candidates$end_index >
      nrow(x$spectra) - parameters$interpolation_points
  candidates$correctable <- !boundary
  candidates$reason <- ifelse(boundary, "boundary_interval", "detected")
  list(candidates = candidates, reason = reason)
}

.spike_local_peaks <- function(signal) {
  n <- length(signal)
  if (n < 3L) return(integer())
  runs <- rle(signal)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  valid <- which(
    starts > 1L & ends < n & is.finite(runs$values)
  )
  if (length(valid) == 0L) return(integer())
  left_values <- signal[starts[valid] - 1L]
  right_values <- signal[ends[valid] + 1L]
  keep <- is.finite(left_values) & is.finite(right_values) &
    left_values < runs$values[valid] & right_values < runs$values[valid]
  keep[is.na(keep)] <- FALSE
  valid <- valid[keep]
  as.integer(floor((starts[valid] + ends[valid]) / 2))
}

.spike_peak_feature <- function(signal, peak, rel_height = 0.8) {
  if (!is.finite(signal[peak])) return(NULL)
  n <- length(signal)
  left_bound <- 1L
  right_bound <- n
  if (peak > 1L) {
    bad <- which(!is.finite(signal[seq_len(peak - 1L)]))
    if (length(bad) > 0L) left_bound <- max(bad) + 1L
    if (left_bound < peak) {
      higher <- which(signal[seq.int(left_bound, peak - 1L)] > signal[peak])
      if (length(higher) > 0L) {
        left_bound <- left_bound + max(higher) - 1L
      }
    }
  }
  if (peak < n) {
    right_sequence <- seq.int(peak + 1L, n)
    bad <- which(!is.finite(signal[right_sequence]))
    if (length(bad) > 0L) right_bound <- right_sequence[min(bad)] - 1L
    if (right_bound > peak) {
      higher <- which(signal[seq.int(peak + 1L, right_bound)] > signal[peak])
      if (length(higher) > 0L) {
        right_bound <- peak + min(higher)
      }
    }
  }
  if (left_bound >= peak || right_bound <= peak) return(NULL)

  left_values <- signal[seq.int(left_bound, peak)]
  right_values <- signal[seq.int(peak, right_bound)]
  left_base <- left_bound + which.min(left_values) - 1L
  right_base <- peak + which.min(right_values) - 1L
  contour <- max(signal[left_base], signal[right_base])
  prominence <- signal[peak] - contour
  if (!is.finite(prominence) || prominence <= 0) return(NULL)

  fwhm_height <- signal[peak] - prominence * 0.5
  left_half <- .spike_left_crossing(
    signal, peak, left_base, fwhm_height
  )
  right_half <- .spike_right_crossing(
    signal, peak, right_base, fwhm_height
  )
  interval_height <- signal[peak] - prominence * rel_height
  interval_left <- .spike_left_crossing(
    signal, peak, left_base, interval_height
  )
  interval_right <- .spike_right_crossing(
    signal, peak, right_base, interval_height
  )
  list(
    prominence = as.numeric(prominence),
    width = as.numeric(right_half - left_half),
    interval_left = as.numeric(interval_left),
    interval_right = as.numeric(interval_right)
  )
}

.spike_left_crossing <- function(signal, peak, base, height) {
  indices <- seq.int(peak, base)
  below <- indices[signal[indices] <= height]
  if (length(below) == 0L) return(as.numeric(base))
  lower <- below[1L]
  if (lower == peak || signal[lower] == height) return(as.numeric(lower))
  upper <- lower + 1L
  lower + (height - signal[lower]) /
    (signal[upper] - signal[lower])
}

.spike_right_crossing <- function(signal, peak, base, height) {
  indices <- seq.int(peak, base)
  below <- indices[signal[indices] <= height]
  if (length(below) == 0L) return(as.numeric(base))
  upper <- below[1L]
  if (upper == peak || signal[upper] == height) return(as.numeric(upper))
  lower <- upper - 1L
  lower + (height - signal[lower]) /
    (signal[upper] - signal[lower])
}

.empty_spike_regions <- function() {
  data.table::data.table(
    spectrum_index = integer(),
    spectrum_id = character(),
    direction = character(),
    start_index = integer(),
    end_index = integer(),
    region_min = numeric(),
    region_max = numeric(),
    peak_count = integer(),
    reason = character()
  )
}

.candidate_rejected_regions <- function(candidates) {
  rejected <- candidates[!candidates$correctable]
  if (nrow(rejected) == 0L) return(.empty_spike_regions())
  data.table::data.table(
    spectrum_index = rejected$spectrum_index,
    spectrum_id = rejected$spectrum_id,
    direction = rejected$direction,
    start_index = rejected$start_index,
    end_index = rejected$end_index,
    region_min = rejected$region_min,
    region_max = rejected$region_max,
    peak_count = rep.int(1L, nrow(rejected)),
    reason = rejected$reason
  )
}

.merge_spike_regions <- function(candidates) {
  accepted <- candidates[candidates$correctable]
  if (nrow(accepted) == 0L) return(.empty_spike_regions())
  accepted <- accepted[order(accepted$spectrum_index,
                             accepted$start_index,
                             accepted$end_index)]
  output <- list()
  output_index <- 0L
  for (spectrum_index in unique(accepted$spectrum_index)) {
    target_index <- spectrum_index
    current <- accepted[accepted[["spectrum_index"]] == target_index]
    start <- current$start_index[1L]
    end <- current$end_index[1L]
    directions <- current$direction[1L]
    peak_count <- 1L
    region_min <- current$region_min[1L]
    region_max <- current$region_max[1L]
    flush <- function() {
      output_index <<- output_index + 1L
      output[[output_index]] <<- data.table::data.table(
        spectrum_index = as.integer(spectrum_index),
        spectrum_id = current$spectrum_id[1L],
        direction = if (length(unique(directions)) == 1L) {
          unique(directions)
        } else {
          "both"
        },
        start_index = as.integer(start),
        end_index = as.integer(end),
        region_min = region_min,
        region_max = region_max,
        peak_count = as.integer(peak_count),
        reason = "detected"
      )
    }
    if (nrow(current) > 1L) {
      for (i in 2:nrow(current)) {
        if (current$start_index[i] <= end + 1L) {
          end <- max(end, current$end_index[i])
          directions <- c(directions, current$direction[i])
          peak_count <- peak_count + 1L
          region_min <- min(region_min, current$region_min[i])
          region_max <- max(region_max, current$region_max[i])
        } else {
          flush()
          start <- current$start_index[i]
          end <- current$end_index[i]
          directions <- current$direction[i]
          peak_count <- 1L
          region_min <- current$region_min[i]
          region_max <- current$region_max[i]
        }
      }
    }
    flush()
  }
  data.table::rbindlist(output, use.names = TRUE)
}

.prepare_spike_corrections <- function(
    x,
    regions,
    flagged,
    interpolation_points,
    interpolation) {
  accepted <- rejected <- .empty_spike_regions()
  replacements <- list()
  replacement_index <- 0L

  for (i in seq_len(nrow(regions))) {
    region <- data.table::copy(regions[i])
    spectrum_index <- region$spectrum_index
    rows <- seq.int(region$start_index, region$end_index)
    values <- x$spectra[, spectrum_index]
    available <- !flagged[, spectrum_index] & is.finite(values)
    left <- which(seq_along(values) < region$start_index & available)
    right <- which(seq_along(values) > region$end_index & available)
    left <- utils::tail(left, interpolation_points)
    right <- utils::head(right, interpolation_points)
    if (length(left) < interpolation_points ||
        length(right) < interpolation_points) {
      region$reason <- "insufficient_interpolation_neighbors"
      rejected <- data.table::rbindlist(list(rejected, region),
                                        use.names = TRUE)
      next
    }

    neighbors <- c(left, right)
    neighbor_order <- order(x$wavenumber[neighbors])
    interpolation_neighbors <- neighbors[neighbor_order]
    linear <- stats::approx(
      x = x$wavenumber[interpolation_neighbors],
      y = values[interpolation_neighbors],
      xout = x$wavenumber[rows],
      ties = "ordered"
    )$y
    quadratic <- .quadratic_spike_interpolation(
      x = x$wavenumber[neighbors],
      y = values[neighbors],
      xout = x$wavenumber[rows]
    )
    replacement <- if (interpolation == "linear") linear else quadratic
    if (any(!is.finite(replacement))) {
      region$reason <- "non_finite_interpolation"
      rejected <- data.table::rbindlist(list(rejected, region),
                                        use.names = TRUE)
      next
    }

    if (interpolation == "linear" && length(rows) > 1L &&
        all(is.finite(quadratic)) &&
        .linear_spike_band_risk(values[neighbors], linear, quadratic)) {
      region$reason <- "underlying_band_risk"
      rejected <- data.table::rbindlist(list(rejected, region),
                                        use.names = TRUE)
      next
    }
    if (identical(as.numeric(values[rows]), as.numeric(replacement))) {
      region$reason <- "interpolation_no_change"
      rejected <- data.table::rbindlist(list(rejected, region),
                                        use.names = TRUE)
      next
    }

    accepted <- data.table::rbindlist(list(accepted, region), use.names = TRUE)
    replacement_index <- replacement_index + 1L
    replacements[[replacement_index]] <- list(
      spectrum_index = spectrum_index,
      rows = rows,
      values = as.numeric(replacement)
    )
  }
  list(accepted = accepted, rejected = rejected,
       replacements = replacements)
}

.quadratic_spike_interpolation <- function(x, y, xout) {
  center <- mean(range(x))
  scale <- max(abs(x - center))
  if (!is.finite(scale) || scale <= 0 || length(unique(x)) < 3L) {
    return(rep(NA_real_, length(xout)))
  }
  fit_data <- data.frame(
    response = y,
    z = (x - center) / scale
  )
  fit <- tryCatch(
    stats::lm(response ~ z + I(z^2), data = fit_data),
    error = function(e) NULL
  )
  if (is.null(fit) || any(!is.finite(stats::coef(fit)))) {
    return(rep(NA_real_, length(xout)))
  }
  as.numeric(stats::predict(
    fit,
    newdata = data.frame(z = (xout - center) / scale)
  ))
}

.linear_spike_band_risk <- function(neighbor_values, linear, quadratic) {
  differences <- diff(neighbor_values)
  noise <- stats::mad(differences, constant = 1.4826)
  if (!is.finite(noise)) noise <- 0
  numerical_floor <- sqrt(.Machine$double.eps) *
    max(abs(neighbor_values), 1)
  tolerance <- max(3 * noise, numerical_floor)
  max(abs(quadratic - linear)) > tolerance
}

.validate_spike_transaction <- function(original, trial, flagged) {
  if (!identical(original$wavenumber, trial$wavenumber)) {
    return("wavenumber_changed")
  }
  if (!identical(dim(original$spectra), dim(trial$spectra)) ||
      !identical(dimnames(original$spectra), dimnames(trial$spectra))) {
    return("spectra_shape_changed")
  }
  if (!identical(original$metadata, trial$metadata)) {
    return("metadata_changed")
  }
  if (!identical(original$spectra[!flagged], trial$spectra[!flagged])) {
    return("values_changed_outside_regions")
  }
  if (any(is.finite(original$spectra) & !is.finite(trial$spectra))) {
    return("new_non_finite_values")
  }
  if (any(flagged[1L, ]) || any(flagged[nrow(flagged), ])) {
    return("boundary_value_flagged")
  }
  NULL
}

.spike_result_signature <- function(x, method, parameters) {
  digest::digest(
    list(
      wavenumber = x$wavenumber,
      spectra = x$spectra,
      method = method,
      parameters = parameters
    ),
    algo = "xxhash64"
  )
}

.spike_diagnostic <- function(
    applied,
    method,
    parameters,
    corrected,
    rejected,
    before_count,
    after_count,
    candidate_count,
    pass_count = 0L,
    reason,
    result_signature = NULL) {
  affected <- unique(corrected$spectrum_id)
  list(
    applied = isTRUE(applied),
    method = method,
    parameters = parameters,
    corrected_regions = corrected,
    rejected_regions = rejected,
    affected_spectra = as.character(affected),
    candidate_count = as.integer(candidate_count),
    before_count = as.integer(before_count),
    after_count = as.integer(after_count),
    pass_count = as.integer(pass_count),
    reason = reason,
    result_signature = result_signature
  )
}
