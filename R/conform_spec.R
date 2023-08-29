#' @rdname conform_spec
#' @title Conform spectra to a standard wavenumber series
#'
#' @description
#' Spectra can be conformed to a standard suite of wavenumbers to be compared
#' with a reference library or to be merged to other spectra.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param range a vector of new wavenumber values, can be just supplied as a
#' min and max value.
#' @param res spectral resolution adjusted to or NULL if the raw range should be used.
#' @param \ldots further arguments passed to \code{\link[stats]{approx}()}
#'
#' @return
#' \code{adj_intens()} returns a data frame containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}
#'
#' @examples
#' data("raman_hdpe")
#' conform_spec(raman_hdpe, c(1000, 2000))
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{restrict_range}()} and  \code{\link{flatten_range}()} for
#' adjusting wavenumber ranges;
#' \code{\link{subtr_baseline}()} for spectral background correction
#'
#' @importFrom data.table .SD
#' @export
conform_spec <- function(x, ...) {
  UseMethod("conform_spec")
}

#' @rdname conform_spec
#'
#' @export
conform_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname conform_spec
#'
#' @export
conform_spec.OpenSpecy <- function(x, range = NULL, res = 5, ...) {
  if(is.null(range)) range <- x$wavenumber
  
  if(!is.null(res)){
      range <- c(max(min(range), min(x$wavenumber)),
             min(max(range), max(x$wavenumber))
                )

    wn <- conform_res(range, res = res)
  }
  else{
      wn = range
  }
  

  spec <- x$spectra[, lapply(.SD, .conform_intens,
                             x = x$wavenumber,
                             xout = wn, ...)]

  x$wavenumber <- wn
  x$spectra <- spec

  return(x)
}

.conform_intens <- function(...) {
  approx(...)$y
}
