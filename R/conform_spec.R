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
#' @param res spectral resolution adjusted to or \code{NULL} if the raw range
#' should be used.
#' @param allow_na logical; should NA values in places beyond the wavenumbers
#' of the dataset be allowed?
#' @param type the type of wavenumber adjustment to make. \code{"interp"}
#' results in linear interpolation while \code{"roll"} conducts a nearest
#' rolling join of the wavenumbers. \code{"mean_up"} only works when
#' Spectra are being aggregated, we take the mean of the intensities within the
#' wavenumber specified. This can maintain smaller peaks and make spectra more
#' similar to it's less resolved relatives. mean_up option is still experimental.
#'
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
conform_spec.OpenSpecy <- function(x, range = NULL, res = 5, allow_na = F,
                                   type = "interp",
                                   ...) {
  if(!any(type %in% c("interp", "roll", "mean_up")))
    stop("type must be either 'interp', 'roll', or 'mean_up'")

  if(is.null(range)) range <- x$wavenumber

  if(!is.null(res)) {
    range2 <- c(max(min(range), min(x$wavenumber)),
               min(max(range), max(x$wavenumber)))

    wn <- conform_res(range2, res = res)
  } else {
    wn <- range[range >= min(x$wavenumber) & range <= max(x$wavenumber)]
  }

  if(type == "interp")
    spec <- x$spectra[, lapply(.SD, .conform_intens, x = x$wavenumber,
                               xout = wn, ...)]

  if(type == "roll") {
    join <- data.table("wavenumber" = wn)
    # Rolling join option
    spec <- x$spectra
    spec$wavenumber <- x$wavenumber
    spec <- spec[join, roll = "nearest", on = "wavenumber"]
    spec <- spec[,-"wavenumber"]
  }

  if(type == "mean_up"){
    spec <- x$spectra[,lapply(.SD, mean),
                      by = cut(x = x$wavenumber, breaks = wn)][,-"cut"]
  }

  if(allow_na){
    if(min(range) < min(wn) | max(range) > max(wn)){
      if(!is.null(res)){
        filler_range  <- conform_res(range, res = res)
      }
      else{
        filler_range <- range
      }
      filler = data.table("wavenumber" = filler_range)
      spec <- spec[,"wavenumber" := wn][filler, on = "wavenumber"][,-"wavenumber"]
      wn <- filler_range
    }
  }

  x$wavenumber <- wn
  x$spectra <- spec

  return(x)
}

.conform_intens <- function(...) {
  approx(...)$y
}
