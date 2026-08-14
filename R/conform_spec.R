#' @rdname conform_spec
#' @title Conform spectra to a standard wavenumber series
#'
#' @description
#' Spectra can be conformed to a standard suite of wavenumbers to be compared
#' with a reference library or to be merged to other spectra.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param range a vector of new wavenumber values, can be just supplied as a
#' min and max value.
#' @param res spectral resolution adjusted to or \code{NULL} if the raw range
#' should be used.
#' @param allow_na logical; should NA values in places beyond the wavenumbers
#' of the dataset be allowed?
#' @param type the type of wavenumber adjustment to make. \code{"interp"}
#' results in linear interpolation while \code{"roll"} conducts a nearest
#' rolling join of the wavenumbers. \code{"mean_up"} assigns source values by
#' midpoint and returns one row for every requested wavenumber: occupied bins
#' are averaged, while requested positions with no source value are linearly
#' interpolated. This can maintain smaller peaks, match either a coarser or
#' finer target axis, and avoid full-size grouping copies. The mean-up option is
#' still experimental.
#'
#' @param \ldots further arguments passed to \code{\link[stats]{approx}()}
#'
#' @return
#' \code{adj_intens()} returns a data frame containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}
#'
#' @examples
#' data("raman_hdpe")
#' conform_spec(raman_hdpe, c(1000, 2000))
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{restrict_range}()} and  \code{\link{flatten_range}()} for
#' adjusting wavenumber ranges;
#' \code{\link{subtr_baseline}()} for spectral background correction
#'
#' @importFrom data.table .SD
#' @export
conform_spec <- function(x, ...) {
  UseMethod("conform_spec")
}

#' @rdname conform_spec
#'
#' @export
conform_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname conform_spec
#'
#' @export
conform_spec.OpenSpecy <- function(x, range = NULL, res = 5, allow_na = F,
                                   type = "interp",
                                   ...) {
  x <- as_OpenSpecy(x)

  if(!any(type %in% c("interp", "roll", "mean_up")))
    stop("type must be either 'interp', 'roll', or 'mean_up'")

  raw_wave = x$wavenumber
    
  if(is.null(range)) range <- raw_wave

  if(!is.null(res)) {
    range2 <- c(max(min(range), min(raw_wave)),
               min(max(range), max(raw_wave)))

    wn <- conform_res(range2, res = res)
  } else {
    wn <- range[range >= min(raw_wave) & range <= max(raw_wave)]
  }

  if(type == "interp")
    spec <- .conform_intens_matrix(x = raw_wave, y = x$spectra, xout = wn,
                                   ...)

  if(type == "roll") {
    idx <- findInterval(wn, raw_wave)
    left <- pmax(idx, 1L)
    right <- pmin(idx + 1L, length(raw_wave))
    right[idx == length(raw_wave)] <- length(raw_wave)
    use_right <- abs(raw_wave[right] - wn) < abs(raw_wave[left] - wn)
    pick <- left
    pick[use_right] <- right[use_right]
    spec <- x$spectra[pick, , drop = FALSE]
  }

  if(type == "mean_up"){
    spec <- .conform_mean_up_matrix(raw_wave, x$spectra, wn)
  }

  if(allow_na){
    if(min(range) < min(wn) | max(range) > max(wn)){
      if(!is.null(res)){
        filler_range  <- conform_res(range, res = res)
      }
      else{
        filler_range <- range
      }
      filler <- matrix(NA_real_, nrow = length(filler_range),
                       ncol = ncol(spec),
                       dimnames = list(NULL, colnames(spec)))
      filler[match(wn, filler_range), ] <- spec
      spec <- filler
      wn <- filler_range
    }
  }

  x$wavenumber <- wn
  x$spectra <- spec

  return(x)
}

.conform_intens <- function(...) {
  approx(...)$y
}

.conform_mean_up_matrix <- function(x, y, xout) {
  if (!length(xout)) {
    return(matrix(numeric(), nrow = 0L, ncol = ncol(y),
                  dimnames = list(NULL, colnames(y))))
  }
  if (length(xout) == 1L) {
    out <- matrix(colMeans(y), nrow = 1L)
    colnames(out) <- colnames(y)
    return(out)
  }
  if (is.unsorted(x) || is.unsorted(xout) || anyDuplicated(xout) ||
      anyNA(x) || anyNA(xout)) {
    stop("mean_up requires finite, increasing, unique wavenumbers",
         call. = FALSE)
  }

  # Assign every source row to its nearest requested wavenumber. Midpoint
  # boundaries produce exactly one output row per target and avoid the list,
  # character labels, and unlist copy used by cut().
  boundaries <- (xout[-1L] + xout[-length(xout)]) / 2
  group <- findInterval(x, boundaries) + 1L
  rows <- split(seq_along(group), factor(group, levels = seq_along(xout)))
  out <- matrix(NA_real_, nrow = length(xout), ncol = ncol(y),
                dimnames = list(NULL, colnames(y)))
  occupied <- which(lengths(rows) > 0L)
  for (i in occupied) {
    out[i, ] <- colMeans(y[rows[[i]], , drop = FALSE])
  }

  # A finer target axis necessarily contains empty midpoint bins. Fill only
  # those rows by interpolation, one row at a time, so preserving a dense
  # uploaded axis does not allocate another target-by-library matrix.
  empty <- which(lengths(rows) == 0L)
  for (i in empty) {
    if (length(x) == 1L) {
      out[i, ] <- y[1L, ]
      next
    }
    interval <- findInterval(xout[[i]], x)
    left <- max(1L, min(interval, length(x) - 1L))
    right <- left + 1L
    weight <- (xout[[i]] - x[[left]]) / (x[[right]] - x[[left]])
    out[i, ] <- y[left, ] + (y[right, ] - y[left, ]) * weight
  }
  out
}

.conform_intens_matrix <- function(x, y, xout, ...) {
  dots <- list(...)

  if (length(xout) == 0L) {
    out <- matrix(numeric(0), nrow = 0L, ncol = ncol(y),
                  dimnames = list(NULL, colnames(y)))
    return(out)
  }

  use_fast <- length(dots) == 0L &&
    length(x) >= 2L &&
    isTRUE(all(diff(x) > 0)) &&
    !anyDuplicated(x) &&
    !anyNA(x) &&
    !anyNA(xout) &&
    !anyNA(y)

  if (!use_fast) {
    return(.apply_spectra(y, function(intens) {
      .conform_intens(x = x, y = intens, xout = xout, ...)
    }, value_length = length(xout)))
  }

  idx <- findInterval(xout, x)
  left <- pmax(1L, pmin(idx, length(x) - 1L))
  right <- left + 1L
  weight <- (xout - x[left]) / (x[right] - x[left])

  # Precompute the interpolation rows and weights once, then apply them to all
  # spectra columns together instead of calling approx() for every spectrum.
  out <- y[left, , drop = FALSE]
  out <- out + (y[right, , drop = FALSE] - out) * weight
  colnames(out) <- colnames(y)
  out
}
