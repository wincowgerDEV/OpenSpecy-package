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
                                     ...) {
  x <- as_OpenSpecy(x)

  if (isTRUE(automate)) {
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

  if (is.null(min) || is.null(max)) {
    stop("'min' and 'max' are required unless 'automate = TRUE'",
         call. = FALSE)
  }
  if (length(min) != length(max) || any(!is.finite(c(min, max))) ||
      any(min > max)) {
    stop("'min' and 'max' must be equal-length finite vectors with min <= max",
         call. = FALSE)
  }

  test <- vapply(seq_along(min), function(y) {
    x$wavenumber >= min[y] & x$wavenumber <= max[y]
  }, FUN.VALUE = logical(length(x$wavenumber)))

  vals <- rowSums(test) > 0
  filt <- x$spectra[vals, , drop = FALSE]
  x$wavenumber <- x$wavenumber[vals]

  if (make_rel) {
    x$spectra <- make_rel(filt)
  } else {
    x$spectra <- filt
  }

  return(x)
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
