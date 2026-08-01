# Repeated point-intensity and peak-ratio regression benchmark.
# Run manually from the package root with:
# Rscript benchmarks/quantification.R
#
# The literal helpers retain the pre-refactor peak-ratio point lookup outside
# package code and tests. Each representative case checks exact output for both
# lookup methods before comparing repeated linear-lookup timings.

devtools::load_all(export_all = TRUE, quiet = TRUE)

# Compare method bodies directly so unchanged generic-dispatch overhead is not
# charged only to the current implementation. Direct lookup also lets this
# benchmark run before roxygen registers the new method in NAMESPACE.
current_point_intensity <- OpenSpecy:::point_intensity.OpenSpecy
current_peak_ratio <- OpenSpecy:::peak_ratio.OpenSpecy

reference_point_values <- function(wavenumber, spectra, point, method,
                                   spectrum_names) {
  exact <- match(point, wavenumber)
  if (!is.na(exact)) {
    return(stats::setNames(as.numeric(spectra[exact, ]), spectrum_names))
  }

  left <- findInterval(point, wavenumber)
  right <- left + 1L
  if (identical(method, "nearest")) {
    row <- if (abs(wavenumber[right] - point) <
               abs(point - wavenumber[left])) right else left
    return(stats::setNames(as.numeric(spectra[row, ]), spectrum_names))
  }

  weight <- (point - wavenumber[left]) /
    (wavenumber[right] - wavenumber[left])
  values <- spectra[left, ] +
    (spectra[right, ] - spectra[left, ]) * weight
  stats::setNames(as.numeric(values), spectrum_names)
}

reference_peak_ratio <- function(x, numerator, denominator,
                                 method = c("nearest", "linear")) {
  x <- as_OpenSpecy(x)
  method <- match.arg(method)

  validate_point <- function(value, name) {
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L ||
        !is.finite(value)) {
      stop("'", name, "' must be a finite numeric scalar", call. = FALSE)
    }
    as.numeric(value)
  }

  numerator <- validate_point(numerator, "numerator")
  denominator <- validate_point(denominator, "denominator")
  wavenumber <- x$wavenumber
  if (!length(wavenumber) || any(!is.finite(wavenumber))) {
    stop("'x$wavenumber' must contain finite values", call. = FALSE)
  }
  if (anyDuplicated(wavenumber)) {
    stop("'x$wavenumber' must contain unique values", call. = FALSE)
  }

  spectrum_names <- colnames(x$spectra)
  named_na <- function() {
    stats::setNames(rep(NA_real_, ncol(x$spectra)), spectrum_names)
  }
  axis_range <- range(wavenumber)
  if (numerator < axis_range[1L] || numerator > axis_range[2L] ||
      denominator < axis_range[1L] || denominator > axis_range[2L]) {
    warning("The wavenumber axis does not cover both requested peak-ratio ",
            "points; returning NA", call. = FALSE)
    return(named_na())
  }

  ord <- order(wavenumber)
  wavenumber <- wavenumber[ord]
  spectra <- x$spectra[ord, , drop = FALSE]
  numerator_values <- reference_point_values(
    wavenumber, spectra, numerator, method, spectrum_names
  )
  denominator_values <- reference_point_values(
    wavenumber, spectra, denominator, method, spectrum_names
  )
  values <- numerator_values / denominator_values
  invalid <- !is.finite(numerator_values) |
    !is.finite(denominator_values) |
    denominator_values == 0 |
    !is.finite(values)
  if (any(invalid)) {
    warning("One or more peak ratios had a non-finite numerator, a zero or ",
            "non-finite denominator, or a non-finite result; returning NA ",
            "for those spectra", call. = FALSE)
    values[invalid] <- NA_real_
  }
  stats::setNames(as.numeric(values), spectrum_names)
}

reference_point_intensity <- function(x, wavenumber,
                                      method = c("nearest", "linear")) {
  x <- as_OpenSpecy(x)
  method <- match.arg(method)
  axis <- x$wavenumber
  ord <- order(axis)
  values <- reference_point_values(
    axis[ord], x$spectra[ord, , drop = FALSE], wavenumber, method,
    colnames(x$spectra)
  )
  stats::setNames(as.numeric(values), colnames(x$spectra))
}

make_quantification_case <- function(spectra_count, map = FALSE,
                                     points = 1401L) {
  axis <- seq(401.25, 3200.75, length.out = points)
  spectra <- vapply(seq_len(spectra_count), function(i) {
    2 + sin(axis / 130 + i / 17) +
      0.2 * cos(axis / 31 - i / 29) + i / max(1, spectra_count) / 50
  }, numeric(points))
  colnames(spectra) <- paste0("sample_", seq_len(spectra_count))
  metadata <- if (map) {
    side <- ceiling(sqrt(spectra_count))
    data.frame(
      x = (seq_len(spectra_count) - 1L) %% side,
      y = (seq_len(spectra_count) - 1L) %/% side
    )
  } else {
    data.frame(sample_name = colnames(spectra))
  }
  as_OpenSpecy(axis, spectra, metadata = metadata)
}

repeated_pair <- function(reference, current, repetitions, iterations) {
  reference_value <- reference()
  current_value <- current()
  if (!identical(current_value, reference_value)) {
    stop("Warmup output-equivalence check failed", call. = FALSE)
  }

  reference_times <- current_times <- numeric(repetitions)
  for (repetition in seq_len(repetitions)) {
    reference_times[repetition] <- system.time({
      for (iteration in seq_len(iterations)) reference_value <- reference()
    })[["elapsed"]] / iterations
    current_times[repetition] <- system.time({
      for (iteration in seq_len(iterations)) current_value <- current()
    })[["elapsed"]] / iterations
    if (!identical(current_value, reference_value)) {
      stop("Repeated output-equivalence check failed", call. = FALSE)
    }
  }

  c(
    reference = stats::median(reference_times),
    current = stats::median(current_times)
  )
}

benchmark_quantification_case <- function(label, x, repetitions,
                                           iterations) {
  axis <- sort(x$wavenumber)
  numerator <- axis[round(length(axis) * 0.71)] +
    0.37 * diff(axis[round(length(axis) * 0.71) + 0:1])
  denominator <- axis[round(length(axis) * 0.29)] +
    0.63 * diff(axis[round(length(axis) * 0.29) + 0:1])

  for (method in c("nearest", "linear")) {
    if (!identical(
      current_point_intensity(x, numerator, method = method),
      reference_point_intensity(x, numerator, method = method)
    )) {
      stop(label, ": point-intensity equivalence failed for ", method)
    }
    if (!identical(
      current_peak_ratio(x, numerator, denominator, method = method),
      reference_peak_ratio(x, numerator, denominator, method = method)
    )) {
      stop(label, ": peak-ratio equivalence failed for ", method)
    }
  }

  point_times <- repeated_pair(
    function() reference_point_intensity(x, numerator, method = "linear"),
    function() current_point_intensity(x, numerator, method = "linear"),
    repetitions, iterations
  )
  ratio_times <- repeated_pair(
    function() reference_peak_ratio(
      x, numerator, denominator, method = "linear"
    ),
    function() current_peak_ratio(
      x, numerator, denominator, method = "linear"
    ),
    repetitions, iterations
  )
  ratio <- ratio_times[["current"]] /
    max(ratio_times[["reference"]], .Machine$double.eps)
  if (is.finite(ratio) && ratio > 1.10) {
    warning(sprintf(
      "%s peak-ratio runtime regression flag: %.1f%% slower",
      label, 100 * (ratio - 1)
    ), call. = FALSE)
  }

  message(sprintf(
    paste0(
      "%s: point reference %.6fs/current %.6fs; ",
      "peak ratio reference %.6fs/current %.6fs (%.2fx)"
    ),
    label, point_times[["reference"]], point_times[["current"]],
    ratio_times[["reference"]], ratio_times[["current"]], ratio
  ))
  c(
    point_reference = point_times[["reference"]],
    point_current = point_times[["current"]],
    ratio_reference = ratio_times[["reference"]],
    ratio_current = ratio_times[["current"]],
    ratio_current_over_reference = ratio
  )
}

results <- list(
  single = benchmark_quantification_case(
    "single spectrum", make_quantification_case(1L),
    repetitions = 7L, iterations = 1000L
  ),
  batch_100 = benchmark_quantification_case(
    "100 spectra", make_quantification_case(100L),
    repetitions = 5L, iterations = 120L
  ),
  map = benchmark_quantification_case(
    "12x12 map", make_quantification_case(144L, map = TRUE),
    repetitions = 5L, iterations = 100L
  )
)

print(do.call(rbind, results))
