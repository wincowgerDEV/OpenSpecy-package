#' @title Adjust spectral intensities
#'
#' @description
#' This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with a running window of data points and the polynomial specified.
#'
#' @details
#' This is a wrapper around the filter function in the signal package to improve integration with other Open Specy functions.
#'
#' @param x Wavenumber column
#' @param y Intensity column
#' @param p Polynomial order for the filter
#' @param n Number of data points in the window.
#' @param make_relative Normalization function.
#' @param formula formula
#' @param data data
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' data("raman_hdpe")
#' smooth_intensity(intensity ~ wavenumber, data = raman_hdpe)
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
