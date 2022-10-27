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
#' @param object a list object of class \code{OpenSpecy}.
#' @param type a character string specifying whether the input spectrum is
#' in absorbance units (\code{"none"}, default) or needs additional conversion
#' from \code{"reflectance"} or \code{"transmittance"} data.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to submethods; this is
#' to \code{\link{adj_neg}()} for \code{adj_intens()} and
#' to \code{\link{conform_res}()} for \code{conform_intens()}.
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
#' @importFrom data.table .SD
#' @export
adj_intens <- function(object, ...) {
  UseMethod("adj_intens")
}

#' @rdname adj_intens
#'
#' @export
adj_intens.default <- function(object, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname adj_intens
#'
#' @export
adj_intens.OpenSpecy <- function(object, type = "none", make_rel = TRUE, ...) {
  spec <- object$spectra

  adj <- switch(type,
                "reflectance" = (1 - spec/100)^2 / (2 * spec/100),
                "transmittance" = log10(1/adj_neg(spec, ...)),
                "none" = adj_neg(spec, ...)
  )
  if (make_rel) object$spectra <- make_rel(adj) else object$spectra <- adj

  return(object)
}

#' @rdname adj_intens
#'
#' @export
conform_spec <- function(object, ...) {
  UseMethod("conform_spec")
}

#' @rdname adj_intens
#'
#' @export
conform_spec.default <- function(object, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname adj_intens
#'
#' @export
conform_spec.OpenSpecy <- function(object, type = "none", make_rel = TRUE, ...) {
  wn <- conform_res(object$wavenumber, ...)

  spec <- object$spectra[, lapply(.SD, .clean_spec,
                                  x = object$wavenumber,
                                  xout = wn)]

  object$wavenumber <- wn
  object$spectra <- spec

  adj_intens(object, type = type, make_rel = make_rel, na.rm = T)
}

.clean_spec <- function(...) {
  approx(...)$y
}
