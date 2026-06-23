#' @rdname sig_noise
#' @title Calculate signal and noise metrics for OpenSpecy objects
#'
#' @description
#' This function calculates common signal and noise metrics for \code{OpenSpecy}
#' objects.
#'
#' @param x an \code{OpenSpecy} object.
#' @param metric character; specifying the desired metric to calculate.
#' Options include \code{"sig"} (mean intensity), \code{"noise"} (standard
#' deviation of intensity), \code{"sig_times_noise"} (absolute value of
#' signal times noise), \code{"sig_over_noise"} (absolute value of signal /
#' noise), \code{"run_sig_over_noise"} (absolute value of signal /
#' noise where signal is estimated as the max intensity and noise is
#' estimated as the height of a low intensity region.),
#' \code{"log_tot_sig"} (sum of the inverse log intensities, useful for spectra  in log units),
#' \code{"tot_sig"} (sum of intensities), or \code{"entropy"} (Shannon entropy of intensities)..
#' @param step numeric; the step size of the region to look for the run_sig_over_noise option.
#' @param prob numeric single value; the probability to retrieve for the quantile where
#' the noise will be interpreted with the run_sig_over_noise option.
#' @param breaks numeric; the number or positions of the breaks for entropy calculation.
#' Defaults to infer a decent value from the data.
#' @param sig_min numeric; the minimum wavenumber value for the signal region.
#' @param sig_max numeric; the maximum wavenumber value for the signal region.
#' @param noise_min numeric; the minimum wavenumber value for the noise region.
#' @param noise_max numeric; the maximum wavenumber value for the noise region.
#' @param abs logical; whether to return the absolute value of the result
#' @param spatial_smooth logical; whether to spatially smooth the sig/noise using the xy
#' coordinates and a gaussian smoother.
#' @param sigma numeric; two value vector describing standard deviation for smoother in
#' each dimension, y is specified first followed by x, should be the same for each in most cases.
#' @param threshold numeric; if NULL, no threshold is set, otherwise use a numeric value
#' to set the target threshold which true signal or noise should be above. The
#' function will return a logical value instead of numeric if a threshold is set.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating signal and noise. Default is \code{TRUE}.
#' @param \ldots further arguments passed to subfunctions; currently not used.
#'
#' @return
#' A numeric vector containing the calculated metric for each spectrum in the
#' \code{OpenSpecy} object or logical value if threshold is set describing if
#' the numbers where above or equal to (TRUE) the threshold.
#'
#' @seealso [restrict_range()]
#' @examples
#' data("raman_hdpe")
#'
#' sig_noise(raman_hdpe, metric = "sig")
#' sig_noise(raman_hdpe, metric = "noise")
#' sig_noise(raman_hdpe, metric = "sig_times_noise")
#'
#' @importFrom stats median IQR quantile
#' @importFrom data.table frollapply
#' @importFrom mmand gaussianSmooth
#'
#' @export
sig_noise <- function(x, ...) {
  UseMethod("sig_noise")
}

#' @rdname sig_noise
#'
#' @export
sig_noise.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname sig_noise
#'
#' @export
sig_noise.OpenSpecy <- function(x, metric = "run_sig_over_noise",
                                na.rm = TRUE, prob = 0.5, step = 20,
                                breaks = seq(min(unlist(x$spectra)),
                                             max(unlist(x$spectra)),
                                             length = ((nrow(x$spectra)^(1/3)) *
                                                         (max(unlist(x$spectra)) -
                                                            min(unlist(x$spectra)))) /
                                               (2*IQR(unlist(x$spectra)))),
                                sig_min = NULL, sig_max = NULL,
                                noise_min = NULL, noise_max = NULL, abs = T,
                                spatial_smooth = F, sigma = c(1,1), threshold = NULL, ...) {
  x <- as_OpenSpecy(x)

  values <- if (metric == "run_sig_over_noise") {
    .run_sig_over_noise_matrix(x$spectra, step = step, prob = prob,
                               na.rm = na.rm)
  } else if (metric == "entropy") {
    .sig_noise_entropy(x, breaks = breaks)
  } else {
    .sig_noise_simple_matrix(
      x,
      metric = metric,
      na.rm = na.rm,
      sig_min = sig_min,
      sig_max = sig_max,
      noise_min = noise_min,
      noise_max = noise_max
    )
  }

  if(spatial_smooth){
    values <- matrix(values, ncol = max(x$metadata$x) + 1, byrow = T) |>
      gaussianSmooth(sigma = sigma) |>
      t() |>
      as.vector()
  }
  if(abs) {
    values <- abs(values)
  }

  if(!is.null(threshold)) {
    return(values >= threshold)
  }
  else {
    return(values)
  }
}

.sig_noise_simple_matrix <- function(x, metric, na.rm, sig_min, sig_max,
                                     noise_min, noise_max) {
  if(metric %in% c("tot_sig", "log_tot_sig")) {
    y <- if (metric == "log_tot_sig") exp(x$spectra) else x$spectra
    return(colSums(y, na.rm = FALSE))
  }

  sig_intens <- if(!is.null(sig_min) & !is.null(sig_max)){
    x$spectra[x$wavenumber >= sig_min & x$wavenumber <= sig_max, ,
              drop = FALSE]
  } else {
    x$spectra
  }
  noise_intens <- if(!is.null(noise_min) & !is.null(noise_max)){
    x$spectra[x$wavenumber >= noise_min & x$wavenumber <= noise_max, ,
              drop = FALSE]
  } else {
    x$spectra
  }

  signal <- colMeans(sig_intens, na.rm = na.rm)
  noise <- .col_sd(noise_intens, na.rm = na.rm)

  if(metric == "sig") return(signal)
  if(metric == "noise") return(noise)
  if(metric == "sig_times_noise") return(signal * noise)
  if(metric == "sig_over_noise") return(signal/noise)
  stop("Unknown signal-to-noise metric: ", metric, call. = FALSE)
}

.sig_noise_entropy <- function(x, breaks) {
  vapply(seq_len(ncol(x$spectra)), function(i) {
    binarize <- cut(x$spectra[, i], breaks)
    freq <- table(binarize)/length(binarize)
    vec <- as.vector(freq)
    vec <- vec[vec > 0]
    -sum(vec * log2(vec))
  }, FUN.VALUE = numeric(1))
}

.run_sig_over_noise_matrix <- function(spectra, step = 20, prob = 0.5,
                                       na.rm = TRUE) {
  if (!is.numeric(step) || length(step) != 1L || is.na(step) || step < 1) {
    stop("'step' must be a positive number", call. = FALSE)
  }
  step <- as.integer(step)

  values <- rep(NA_real_, ncol(spectra))
  valid_n <- colSums(!is.na(spectra))
  short <- valid_n < step
  if (any(short)) {
    warning(paste0("Need at least ", step, " intensity values to calculate ",
                   "the signal or noise values accurately with ",
                   "run_sig_over_noise; returning NA"), call. = FALSE)
  }
  cols <- which(!short)
  if (length(cols) == 0L || !isTRUE(na.rm)) return(values)

  work <- spectra[, cols, drop = FALSE]
  if (!anyNA(work)) {
    values[cols] <- .run_sig_over_noise_complete_matrix(work, step, prob)
    return(values)
  }

  ignored <- is.na(work)
  ignored_info <- .spectra_ignore_info(work, lead_tail_only = TRUE, ig = NA)
  edge_only <- colSums(ignored != ignored_info$ignored) == 0L

  if (any(edge_only)) {
    edge_positions <- which(edge_only)
    groups <- split(
      edge_positions,
      paste(ignored_info$first[edge_positions],
            ignored_info$last[edge_positions],
            sep = ":")
    )
    for (group in groups) {
      rows <- seq.int(ignored_info$first[group[1L]],
                      ignored_info$last[group[1L]])
      values[cols[group]] <- .run_sig_over_noise_complete_matrix(
        work[rows, group, drop = FALSE],
        step,
        prob
      )
    }
  }

  fallback <- cols[!edge_only]
  if (length(fallback) > 0L) {
    values[fallback] <- vapply(fallback, function(i) {
      .run_sig_over_noise_vector(spectra[, i], step = step, prob = prob,
                                 na.rm = na.rm)
    }, FUN.VALUE = numeric(1))
  }

  values
}

.run_sig_over_noise_complete_matrix <- function(y, step, prob,
                                                batch_size = 5000L) {
  values <- rep(NA_real_, ncol(y))
  out_n <- nrow(y) - 2L * step + 1L
  if (out_n <= 0L) return(values)

  batches <- split(seq_len(ncol(y)),
                   ceiling(seq_len(ncol(y)) / batch_size))
  for (cols in batches) {
    roll <- y[seq_len(out_n), cols, drop = FALSE]
    if (step > 1L) {
      for (i in 2:step) {
        roll <- pmax(roll, y[i:(i + out_n - 1L), cols, drop = FALSE])
      }
    }
    signal <- apply(roll, 2L, max)
    noise <- vapply(seq_along(cols), function(i) {
      vals <- roll[, i]
      vals <- vals[vals != 0]
      if (length(vals) == 0L) return(NA_real_)
      as.numeric(quantile(vals, probs = prob, na.rm = TRUE, names = FALSE))
    }, FUN.VALUE = numeric(1))
    values[cols] <- signal/noise
  }

  values
}

.run_sig_over_noise_vector <- function(y, step, prob, na.rm) {
  if(length(y[!is.na(y)]) < step) {
    warning(paste0("Need at least ", step, " intensity values to calculate ",
                   "the signal or noise values accurately with ",
                   "run_sig_over_noise; returning NA"), call. = FALSE)
    return(NA_real_)
  }
  rolling_max <- frollapply(y[!is.na(y)], step, max)
  rolling_max[(length(rolling_max) - (step-1)):length(rolling_max)] <- NA
  signal <- max(rolling_max, na.rm = na.rm)
  noise <- quantile(rolling_max[rolling_max != 0], probs = prob,
                    na.rm = na.rm)
  signal/noise
}

.col_sd <- function(y, na.rm = TRUE) {
  if (!isTRUE(na.rm) && anyNA(y)) {
    missing <- colSums(is.na(y)) > 0L
  } else {
    missing <- rep(FALSE, ncol(y))
  }
  n <- if (isTRUE(na.rm)) colSums(!is.na(y)) else rep(nrow(y), ncol(y))
  sums <- colSums(y, na.rm = na.rm)
  sums_sq <- colSums(y^2, na.rm = na.rm)
  out <- sqrt((sums_sq - (sums^2 / n)) / (n - 1L))
  out[n < 2L | missing] <- NA_real_
  out
}
