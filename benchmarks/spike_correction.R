# Repeated spike-correction regression benchmark.
#
# This file intentionally keeps a literal prominence/FWHM implementation out of
# tests and the package namespace. It follows the four paper steps directly:
# local maxima, prominence/FWHM thresholds, a rel_height interval, and linear
# interpolation from m clean neighbors on both sides. The benchmark verifies
# spectra equality on controlled one-pixel fixtures before comparing timings.

devtools::load_all(export_all = TRUE, quiet = TRUE)

reference_peak_crossing <- function(y, peak, base, height, side) {
  indices <- if (side == "left") seq.int(peak, base) else seq.int(peak, base)
  found <- indices[y[indices] <= height]
  if (length(found) == 0L) return(as.numeric(base))
  crossing <- found[1L]
  if (y[crossing] == height) return(as.numeric(crossing))
  if (side == "left") {
    other <- crossing + 1L
  } else {
    other <- crossing - 1L
  }
  crossing + (height - y[crossing]) /
    (y[other] - y[crossing]) * (other - crossing)
}

reference_peak_feature <- function(y, peak, rel_height) {
  higher_left <- which(y[seq_len(peak - 1L)] > y[peak])
  left_limit <- if (length(higher_left)) max(higher_left) else 1L
  right_indices <- seq.int(peak + 1L, length(y))
  higher_right <- right_indices[y[right_indices] > y[peak]]
  right_limit <- if (length(higher_right)) min(higher_right) else length(y)
  left_base <- left_limit + which.min(y[seq.int(left_limit, peak)]) - 1L
  right_base <- peak + which.min(y[seq.int(peak, right_limit)]) - 1L
  prominence <- y[peak] - max(y[left_base], y[right_base])
  half_height <- y[peak] - 0.5 * prominence
  flag_height <- y[peak] - rel_height * prominence
  half_left <- reference_peak_crossing(
    y, peak, left_base, half_height, "left"
  )
  half_right <- reference_peak_crossing(
    y, peak, right_base, half_height, "right"
  )
  flag_left <- reference_peak_crossing(
    y, peak, left_base, flag_height, "left"
  )
  flag_right <- reference_peak_crossing(
    y, peak, right_base, flag_height, "right"
  )
  c(prominence = prominence, width = half_right - half_left,
    start = ceiling(flag_left), end = floor(flag_right))
}

reference_paper_correct <- function(x, prominence_threshold = 40,
                                    width_threshold = 4,
                                    rel_height = 0.8,
                                    interpolation_points = 10L) {
  out <- x
  for (column in seq_len(ncol(x$spectra))) {
    y <- x$spectra[, column]
    peaks <- which(
      y[2:(length(y) - 1L)] > y[1:(length(y) - 2L)] &
        y[2:(length(y) - 1L)] > y[3:length(y)]
    ) + 1L
    if (length(peaks) == 0L) next
    features <- lapply(peaks, function(peak) {
      reference_peak_feature(y, peak, rel_height)
    })
    features <- do.call(rbind, features)
    selected <- which(
      features[, "prominence"] >= prominence_threshold &
        features[, "width"] <= width_threshold
    )
    if (length(selected) == 0L) next
    intervals <- features[selected, c("start", "end"), drop = FALSE]
    flagged <- logical(length(y))
    for (i in seq_len(nrow(intervals))) {
      flagged[seq.int(intervals[i, "start"], intervals[i, "end"])] <- TRUE
    }
    for (i in seq_len(nrow(intervals))) {
      rows <- seq.int(intervals[i, "start"], intervals[i, "end"])
      clean <- !flagged & is.finite(y)
      left <- utils::tail(which(seq_along(y) < min(rows) & clean),
                          interpolation_points)
      right <- utils::head(which(seq_along(y) > max(rows) & clean),
                           interpolation_points)
      if (length(left) < interpolation_points ||
          length(right) < interpolation_points) next
      neighbors <- c(left, right)
      out$spectra[rows, column] <- stats::approx(
        x$wavenumber[neighbors], y[neighbors],
        xout = x$wavenumber[rows], ties = "ordered"
      )$y
    }
  }
  out
}

make_spike_benchmark_case <- function(spectra_count, points = 1001L) {
  axis <- seq(400, 3200, length.out = points)
  baseline <- sin(axis / 150) + 0.2 * cos(axis / 37)
  spectra <- vapply(seq_len(spectra_count), function(i) {
    values <- baseline + i / max(1, spectra_count) / 100
    location <- 101L + ((i - 1L) %% (points - 201L))
    values[location] <- values[location] + 100
    values
  }, FUN.VALUE = numeric(points))
  colnames(spectra) <- paste0("s", seq_len(spectra_count))
  as_OpenSpecy(axis, spectra)
}

# Literal pre-vectorization residual helper retained for same-output timing.
# Keep this out of package code and tests: it is a benchmark oracle for the
# complete-spectrum fast path in .local_residual_metrics().
reference_local_residual_metrics <- function(axis, values, window = 5L) {
  n <- length(values)
  predicted <- residual <- score <- rep(NA_real_, n)
  if (n <= 2L * window) {
    return(list(predicted = predicted, residual = residual, score = score))
  }

  for (i in seq.int(window + 1L, n - window)) {
    left <- seq.int(i - window, i - 1L)
    right <- seq.int(i + 1L, i + window)
    chord_predictions <- unlist(lapply(left, function(left_index) {
      values[left_index] +
        (values[right] - values[left_index]) *
        (axis[i] - axis[left_index]) /
        (axis[right] - axis[left_index])
    }), use.names = FALSE)
    predicted[i] <- stats::median(chord_predictions)
    residual[i] <- values[i] - predicted[i]
  }

  finite <- is.finite(residual)
  global_center <- stats::median(residual[finite])
  global_scale <- stats::mad(
    residual[finite], center = global_center, constant = 1.4826
  )
  intensity_scale <- max(abs(values), 1)
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

residual_metric_batch <- function(x, implementation) {
  lapply(seq_len(ncol(x$spectra)), function(column) {
    implementation(x$wavenumber, x$spectra[, column], window = 5L)
  })
}

benchmark_residual_case <- function(label, x, repetitions) {
  reference <- residual_metric_batch(x, reference_local_residual_metrics)
  current <- residual_metric_batch(x, OpenSpecy:::.local_residual_metrics)
  if (!identical(current, reference)) {
    stop(label, ": vectorized residual metrics differ from the old helper")
  }

  reference_times <- current_times <- numeric(repetitions)
  for (i in seq_len(repetitions)) {
    reference_times[i] <- system.time({
      candidate <- residual_metric_batch(
        x, reference_local_residual_metrics
      )
    })[["elapsed"]]
    current_times[i] <- system.time({
      candidate_current <- residual_metric_batch(
        x, OpenSpecy:::.local_residual_metrics
      )
    })[["elapsed"]]
    if (!identical(candidate_current, candidate)) {
      stop(label, ": repeated residual equivalence check failed")
    }
  }
  reference_median <- stats::median(reference_times)
  current_median <- stats::median(current_times)
  ratio <- current_median / max(reference_median, .Machine$double.eps)
  message(sprintf(
    "%s residual helper: reference %.4fs, current %.4fs, current/reference %.2fx",
    label, reference_median, current_median, ratio
  ))
  if (ratio > 1.10) {
    warning(sprintf(
      "%s residual runtime regression flag: %.1f%% slower than reference",
      label, 100 * (ratio - 1)
    ), call. = FALSE)
  }
  invisible(c(reference = reference_median, current = current_median,
              ratio = ratio))
}

benchmark_spike_case <- function(label, x, repetitions) {
  reference <- reference_paper_correct(x)
  current <- OpenSpecy:::correct_spike.OpenSpecy(
    x,
    method = "prominence_fwhm",
    direction = "positive",
    prominence_threshold = 40,
    width_threshold = 4,
    rel_height = 0.8,
    interpolation_points = 10L
  )
  if (!identical(current$spectra, reference$spectra)) {
    stop(label, ": current output differs from the literal paper reference")
  }

  reference_times <- current_times <- numeric(repetitions)
  for (i in seq_len(repetitions)) {
    reference_times[i] <- system.time({
      candidate <- reference_paper_correct(x)
    })[["elapsed"]]
    current_times[i] <- system.time({
      candidate_current <- OpenSpecy:::correct_spike.OpenSpecy(
        x,
        method = "prominence_fwhm",
        direction = "positive",
        prominence_threshold = 40,
        width_threshold = 4,
        rel_height = 0.8,
        interpolation_points = 10L
      )
    })[["elapsed"]]
    if (!identical(candidate_current$spectra, candidate$spectra)) {
      stop(label, ": repeated output-equivalence check failed")
    }
  }
  reference_median <- stats::median(reference_times)
  current_median <- stats::median(current_times)
  ratio <- if (reference_median >= 0.005) {
    current_median / reference_median
  } else {
    NA_real_
  }
  message(sprintf(
    "%s: reference %.4fs, current %.4fs, current/reference %s",
    label, reference_median, current_median,
    if (is.finite(ratio)) sprintf("%.2fx", ratio) else "not measurable"
  ))
  if (is.finite(ratio) && ratio > 1.10) {
    warning(sprintf(
      "%s material runtime regression flag: %.1f%% slower than reference",
      label, 100 * (ratio - 1)
    ), call. = FALSE)
  }
  invisible(c(reference = reference_median, current = current_median,
              ratio = ratio))
}

results <- list(
  single = benchmark_spike_case(
    "single spectrum", make_spike_benchmark_case(1L), repetitions = 7L
  ),
  batch_100 = benchmark_spike_case(
    "100 spectra", make_spike_benchmark_case(100L), repetitions = 5L
  ),
  map = benchmark_spike_case(
    "12x12 map", make_spike_benchmark_case(144L), repetitions = 3L
  )
)

print(do.call(rbind, results))

residual_results <- list(
  single = benchmark_residual_case(
    "single spectrum", make_spike_benchmark_case(1L, 701L),
    repetitions = 5L
  ),
  batch_100 = benchmark_residual_case(
    "100 spectra", make_spike_benchmark_case(100L, 701L),
    repetitions = 2L
  ),
  map = benchmark_residual_case(
    "12x12 map", make_spike_benchmark_case(144L, 701L),
    repetitions = 2L
  )
)

print(do.call(rbind, residual_results))
