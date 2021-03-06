#' @title Adjust spectral intensities to absorbance units
#'
#' @description
#' Converts reflectance or transmittance intensity units to absorbance units.
#'
#' @details
#' Many of the Open Specy functions will assume that the spectrum is in absorbance units. For example, see \code{\link{match_spectrum}} and
#'      \code{\link{subtract_background}}. To run those functions properly, you will need to first convert any spectra from transmittance or reflectance to absorbance using this function. 
#'       
#'      The transmittance adjustment uses the log10(1/T) calculation which does not correct for system and particle characteristics. 
#'      The reflectance adjustment uses the Kubelka-Munk equation (1-R)2/(2*R).
#'
#' @param x wavenumber The wavenumber value of the spectrum.
#' @param y intensity The intenisity units in transmission or reflectance of the spectrum.
#' @param type type
#' @param make_relative make_relative
#' @param formula formula
#' @param data data
#' @param \ldots ...
#'
#' @seealso
#' \code{\link{match_spectrum}}
#' \code{\link{subtract_background}}
#' @keywords
#' spectra, intensity conversion
#'
#' @examples
#' data("raman_hdpe")
#' adjust_intensity(intensity ~ wavenumber, data = raman_hdpe)
#'
#' @importFrom magrittr %>%
#' @export
adjust_intensity <- function(x, ...) {
  UseMethod("adjust_intensity")
}

#' @rdname adjust_intensity
#'
#' @export
adjust_intensity.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("adjust_intensity", c(lst, list(...)))
}

#' @rdname adjust_intensity
#'
#' @export
adjust_intensity.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'")

  do.call("adjust_intensity", list(x$wavenumber, x$intensity, ...))
}

#' @rdname adjust_intensity
#'
#' @export
adjust_intensity.default <- function(x, y, type = "none", make_relative = TRUE,
                                     ...) {
  yadj <- switch(type,
                 "reflectance" = (1 - adjust_negative(y))^2 / (2 * adjust_negative(y)),
                 "transmittance" = log10(1/adjust_negative(y)),
                 "none" = adjust_negative(y)
                )
  if (make_relative) yout <- make_relative(yadj) else yout <- yadj

  data.frame(wavenumber = x, intensity = yout)
}
