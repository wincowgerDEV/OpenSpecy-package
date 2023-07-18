#' @rdname conform_spec
#'
#' @title Conform spectra
#'
#' @description
#'
#'
#' @details
#' Many of
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param new_wavenumbers a vector of new wavenumber values, can be just supplied as a min and max value.
#' @param res spectral resolution adjusted to.
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
conform_spec <- function(x, new_wavenumbers, res = 5) {
  UseMethod("conform_spec")
}

#' @rdname conform_spec
#'
#' @export
conform_spec.default <- function(x, new_wavenumbers, res = 5) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname conform_spec
#'
#' @export
conform_spec.OpenSpecy <- function(x, new_wavenumbers, res = 5) {
  wn <- conform_res(new_wavenumbers, res = res)

  spec <- x$spectra[, lapply(.SD, .conform_intens,
                             x = x$wavenumber,
                             xout = wn)]

  x$wavenumber <- wn
  x$spectra <- spec

  return(x)
}

.conform_intens <- function(...) {
  approx(...)$y
}
