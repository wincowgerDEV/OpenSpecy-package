#' @rdname data_norm
#'
#' @title Range Flattening for Spectra
#'
#' @description
#' \code{flatten_range()} will flatten ranges of the spectra that should have no peaks. 
#' Multiple ranges can be specified by inputting the series of max and min values in order.
#'
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}; \code{x} should contain the spectral
#' wavenumbers.
#' @param y a numeric vector containing the spectral intensities.
#' @param res spectral resolution supplied to \code{fun}.
#' @param fun the function to be applied to each element of \code{x}; defaults
#' to \code{\link[base]{round}()} to round to a specific resolution \code{res}.
#' @param na.rm logical. Should missing values be removed?
#'
#' @return
#' \code{adj_res()} abd \code{conform_res()} return a numeric vector with
#' resolution-conformed wavenumbers.
#' \code{adj_neg()} and \code{make_rel()} return numeric vectors
#' with the normalized intensity data.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(OpenSpecy)
#' test_noise = as_OpenSpecy(x = seq(400,4000, by = 10), spectra = data.table(intensity = rnorm(361)))
#'   flattened_intensites <- .flatten_range(wavenumber = test_noise$wavenumber, spectra = test_noise$spectra[[1]], min_range = c(1000, 2000) , max_range = c(1500, 2500))
#'  ggplot()+
#'    geom_line(aes(x = test_noise$wavenumber, y = flattened_intensites))
#' flattened_intensites <- flatten_range.OpenSpecy(object = test_noise, min_range = c(1000, 2000) , max_range = c(1500, 2500))
#' ggplot()+
#'    geom_line(aes(x = flattened_intensites$wavenumber, y = flattened_intensites$spectra[[1]]))

#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} and \code{\link[base]{round}()};
#' \code{\link{adj_intens}()} for log transformation functions
#'
#'
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @export
flatten_range <- function(x, ...) {
    UseMethod("restrict_range")
}

#' @export
flatten_range.default <- function(object, ...) {
    stop("object needs to be of class 'OpenSpecy'")
}

flatten_range.OpenSpecy <- function(object, 
                                    min_range = 0, 
                                    max_range = 6000, 
                                    make_rel = TRUE,
                                    ...) {
    filt <- object$spectra[,lapply(.SD, function(x){
        .flatten_range(wavenumber = object$wavenumber, 
                       spectra = x, 
                       min_range = min_range, 
                       max_range = max_range)
    })]
    
    if (make_rel) object$spectra <- filt[, lapply(.SD, make_rel)] else object$spectra <- filt
    
    return(object)
}

.flatten_range <- function(wavenumber, spectra, min_range, max_range){
    
    for(x in 1:length(min_range)){
        spectra[wavenumber >= min_range[x] & wavenumber <= max_range[x]] <- mean(c(spectra[min(which(wavenumber >= min_range[x]))], 
                                                                                   spectra[max(which(wavenumber <= max_range[x]))]))
    }
    
    return(spectra)
}




