#' @title Adjust spectral intensities
#'
#' @description
#' This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with a running window of data points and the polynomial specified.
#'
#' @details
#' This is a wrapper around the filter function in the signal package to improve integration with other Open Specy functions.
#' A typical good smooth can be achieved with 11 data point window and a 3rd or 4th order polynomial.
#'
#' @param x a numeric vector containing the spectral wavenumbers; alternatively
#' a data frame containing spectral data as \code{"wavenumber"} and
#' \code{"intensity"} can be supplied.
#' @param y a numeric vector containing the spectral intensities.
#' @param formula an object of class '\code{\link[stats]{formula}}' of the form
#' \code{intensity ~ wavenumber}.
#' @param data a data frame containing the variables in \code{formula}.
#' @param p polynomial order for the filter
#' @param n number of data points in the window, filter length (must be odd).
#' @param make_relative logical; if \code{TRUE} spectra are automatically
#' normalized with \code{\link{make_relative}()}.
#' @param \ldots further arguments passed to \code{\link[signal]{sgolay}()}.
#'
#' @seealso
#' \code{\link[signal]{sgolay}()}
#'
#' @examples
#' data("raman_hdpe")
#' smooth_intensity(raman_hdpe)
#'
#' @importFrom magrittr %>%
#' @export
smooth_intensity <- function(x, ...) {
  UseMethod("smooth_intensity")
}

#' @rdname smooth_intensity
#'
#' @export
smooth_intensity.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("smooth_intensity", c(lst, list(...)))
}

#' @rdname smooth_intensity
#'
#' @export
smooth_intensity.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'")

  do.call("smooth_intensity", list(x$wavenumber, x$intensity, ...))
}

#' @rdname smooth_intensity
#' @importFrom signal filter sgolay
#' @export
smooth_intensity.default <- function(x, y, p = 3, n = 11, make_relative = TRUE,
                                     ...) {
  yflt <- signal::filter(filt = sgolay(p = p, n = n, ...), x = y)

  if (make_relative) yout <- make_relative(yflt) else yout <- yflt

  data.frame(wavenumber = x, intensity = yout)
}
