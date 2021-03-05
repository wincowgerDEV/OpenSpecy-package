#' @title Adjust spectral intensities
#'
#' @description
#' description
#'
#' @details
#' If the uploaded spectrum is not in absorbance units, use this input to specify the units to convert from.Open Specy can adjust reflectance or transmittance spectra to Absorbance units using this drop down in the upload file tab. All of the preceding tabs assume that the data is in absorbance units so you should make the correction before continuing if needed. The transmittance adjustment uses the log10(1/T) calculation which does not correct for system and particle characteristics. The reflectance adjustment uses the Kubelka-Munk equation (1-R)2/(2*R). If none is selected, Open Specy assumes that the uploaded data is an absorbance spectrum.
#'
#' @param x wavenumber
#' @param y absorbance
#' @param type type
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
#' adjust_intensity(absorbance ~ wavenumber, data = raman_hdpe)
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
  names(lst) <- c("x", "y")

  do.call("adjust_intensity", c(lst, list(...)))
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

  data.frame(wavenumber = x, absorbance = yout)
}
