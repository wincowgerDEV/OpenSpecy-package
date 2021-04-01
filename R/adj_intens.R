#' @title Adjust spectral intensities to absorbance units
#'
#' @description
#' Converts reflectance or transmittance intensity units to absorbance units.
#'
#' @details
#' Many of the Open Specy functions will assume that the spectrum is in
#' absorbance units. For example, see \code{\link{match_spec}()} and
#' \code{\link{subtr_bg}()}.
#' To run those functions properly, you will need to first convert any spectra
#' from transmittance or reflectance to absorbance using this function.
#' The transmittance adjustment uses the \eqn{log10(1 / T)}
#' calculation which does not correct for system and particle characteristics.
#' The reflectance adjustment uses the Kubelka-Munk equation
#' \eqn{(1 - R)^2 / 2R}. We assume that the reflectance intensity
#' is a percent from 1-100 and first correct the intensity by dividing by 100
#' so that it fits the form expected by the equation.
#'
#' @param x a numeric vector containing the spectral wavenumbers; alternatively
#' a data frame containing spectral data as \code{"wavenumber"} and
#' \code{"intensity"} can be supplied.
#' @param y a numeric vector containing the spectral intensities.
#' @param formula an object of class '\code{\link[stats]{formula}}' of the form
#' \code{intensity ~ wavenumber}.
#' @param data a data frame containing the variables in \code{formula}.
#' @param type a character string specifying whether the input spectrum is
#' in absorbance units (\code{"none"}, default) or needs additional conversion
#' from \code{"reflectance"} or \code{"transmittance"} data.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' \code{adj_intens()} returns a data frame containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' adj_intens(raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{subtr_bg}()} for spectral background correction;
#' \code{\link{match_spec}()} matches spectra with the Open Specy or other
#' reference libraries
#'
#' @importFrom magrittr %>%
#' @export
adj_intens <- function(x, ...) {
  UseMethod("adj_intens")
}

#' @rdname adj_intens
#'
#' @export
adj_intens.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) ||
      (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("adj_intens", c(lst, list(...)))
}

#' @rdname adj_intens
#'
#' @export
adj_intens.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'")

  do.call("adj_intens", list(x$wavenumber, x$intensity, ...))
}

#' @rdname adj_intens
#'
#' @export
adj_intens.default <- function(x, y, type = "none", make_rel = TRUE,
                               ...) {
  yadj <- switch(type,
                 "reflectance" = (1 - y/100)^2 / (2 * y/100),
                 "transmittance" = log10(1/adj_neg(y)),
                 "none" = adj_neg(y)
  )
  if (make_rel) yout <- make_rel(yadj) else yout <- yadj

  data.frame(wavenumber = x, intensity = yout)
}
