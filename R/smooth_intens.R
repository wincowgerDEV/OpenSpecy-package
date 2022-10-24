#' @title Smooth spectral intensities
#'
#' @description
#' This smoother can enhance the signal to noise ratio of the data and uses a
#' Savitzky-Golay filter with a running window of data points and the polynomial
#' specified.
#'
#' @details
#' This is a wrapper around the filter function in the signal package to improve
#' integration with other Open Specy functions.
#' A typical good smooth can be achieved with 11 data point window and a 3rd or
#' 4th order polynomial.
#'
#' @param object a list object of class \code{OpenSpecy}.
#' @param p polynomial order for the filter
#' @param n number of data points in the window, filter length (must be odd).
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to \code{\link[signal]{sgolay}()}.
#'
#' @return
#' \code{smooth_intens()} returns a data frame containing two columns named
#' \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' smooth_intens(raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[signal]{sgolay}()}
#'
#' @references
#' Savitzky A, Golay MJ (1964). “Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures.” \emph{Analytical Chemistry},
#' \strong{36}(8), 1627--1639.
#'
#' @importFrom magrittr %>%
#' @export
smooth_intens <- function(object, ...) {
  UseMethod("smooth_intens")
}

#' @rdname smooth_intens
#'
#' @export
smooth_intens.default <- function(object, ...) {
  stop("object needs to be of class 'OpenSpecy'")
}

#' @rdname smooth_intens
#'
#' @importFrom signal filter sgolay
#' @importFrom data.table .SD
#' @export
smooth_intens.OpenSpecy <- function(object, p = 3, n = 11, make_rel = TRUE,
                                  ...) {
  filt <- object$spectra[, lapply(.SD, .sgfilt, p = p, n = n, ...)]

  if (make_rel) object$spectra <- make_rel(filt) else object$spectra <- filt

  return(object)
}

.sgfilt <- function(y, p, n, ...) {
  signal::filter(filt = sgolay(p = p, n = n, ...), x = y)
}
