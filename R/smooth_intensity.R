#' @title Adjust spectral intensities
#'
#' @description
#' This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified.
#'
#' @param x wavenumber
#' @param y intensity
#' @param p p
#' @param n n
#' @param make_relative make_relative
#' @param formula formula
#' @param data data
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' data("raman_hdpe")
#' smooth_intensity(absorbance ~ wavenumber, data = raman_hdpe)
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
  names(lst) <- c("x", "y")

  do.call("smooth_intensity", c(lst, list(...)))
}

#' @rdname smooth_intensity
#'
#' @export
smooth_intensity.default <- function(x, y, p = 3, n = 11, make_relative = TRUE,
                                     ...) {
  # TODO: Decide for best smoothing function
  #
  # ysmt <- signal::filter(filt = sgolay(p = p, n = n, ...), x = y)
  #
  # if (make_relative) yout <- make_relative(ysmt) else yout <- ysmt
  #
  # data.frame(wavenumber = x, intensity = yout)
}
