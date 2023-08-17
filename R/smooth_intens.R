#' @rdname smooth_intens
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
#' @param x an object of class \code{OpenSpecy}.
#' @param polynomial polynomial order for the filter
#' @param window number of data points in the window, filter length (must be
#' odd).
#' @param derivative the derivative order if you want to calculate the
#' derivative. Zero (default) is no derivative.
#' @param abs logical; whether you want to calculate the absolute value of the
#' resulting output.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to \code{\link[signal]{sgolay}()}.
#'
#' @return
#' \code{smooth_intens()} returns an \code{OpenSpecy} object.
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
#' @importFrom data.table .SD
#' @export
smooth_intens <- function(x, ...) {
  UseMethod("smooth_intens")
}

#' @rdname smooth_intens
#'
#' @export
smooth_intens.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname smooth_intens
#'
#' @export
smooth_intens.OpenSpecy <- function(x, polynomial = 3, window = 11,
                                    derivative = 0, abs = FALSE,
                                    make_rel = TRUE, ...) {
  filt <- x$spectra[, lapply(.SD, .sgfilt, p = polynomial, n = window,
                             m = derivative, abs = abs, ...)]

  if(make_rel) x$spectra <- filt[, lapply(.SD, make_rel)] else x$spectra <- filt

  return(x)
}

#' @importFrom signal filter sgolay
.sgfilt <- function(y, p, n, m, abs = F, ...) {
  out <- signal::filter(filt = sgolay(p = p, n = n, m = m, ...), x = y)
  if(abs) out <- abs(out)

  return(out)
}
