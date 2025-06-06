#' @rdname area_under_band
#' @title Measure the area under band of spectra
#'
#' @description
#' Area under the band calculations are useful for quantifying spectral features. 
#' Specialized fields in spectroscopy have different area under the band regions of interest
#' and their ratios that help with understanding differences in materials. Additional processing 
#' is typically required prior to calculating these values for accuracy and reproducibility.
#'
#' @param x an \code{OpenSpecy} object.
#' @param min a numeric value of the smallest wavenumber to begin calculation.
#' @param max a numeric value of the smallest wavenumber to begin calculation.
#' @param na.rm a logical value for whether to ignore NA values. 
#' @param \ldots additional arguments passed to vapply.
#'
#' @return
#' Numeric vector of area under the band calculations for each spectrum in the Open Specy object. 
#'
#' @examples
#' data("raman_hdpe")
#' #Single area calculation
#' area_under_band(raman_hdpe, min = 1000,max = 2000)
#' #Ratio of two areas. 
#' area_under_band(raman_hdpe, min = 1000,max = 2000)/area_under_band(raman_hdpe, min = 500,max = 700)
#' @author
#' Win Cowger
#'
#'
#' @importFrom data.table .SD
#' @export
area_under_band <- function(x, ...) {
  UseMethod("area_under_band")
}

#' @rdname area_under_band
#'
#' @export
area_under_band.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname area_under_band
#'
#' @export
area_under_band.OpenSpecy <- function(x, min, max, na.rm = F, ...) {
    logic <- x$wavenumber >= min & x$wavenumber <= max
    vapply(x$spectra, function(x){sum(x[logic])}, FUN.VALUE = numeric(1), ...)
}

