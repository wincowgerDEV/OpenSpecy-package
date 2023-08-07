#' @rdname data_norm
#'
#' @title Range Flattening for Spectra
#'
#' @description
#' \code{flatten_range()} will flatten ranges of the spectra that should have no peaks.
#' Multiple ranges can be specified by inputting the series of max and min values in order.
#'
#' @param object An OpenSpecy object containing spectral wavenumbers and intensities.
#' @param min_range A vector of minimum values for the range to be flattened.
#' @param max_range A vector of maximum values for the range to be flattened.
#' @param make_rel Logical. Should the output intensities be normalized to the range [0, 1] using make_rel() function?
#'
#' @return An OpenSpecy object with the spectral intensities within specified ranges flattened.
#'
#' @examples
#' test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
#'                            spectra = data.frame(intensity = rnorm(361)))
#' plot(test_noise)
#'
#' flattened_intensities <- flatten_range(test_noise, min_range = c(1000, 2000),
#'                                        max_range = c(1500, 2500))
#' plot(flattened_intensities)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} and \code{\link[base]{round}()};
#' \code{\link{adj_intens}()} for log transformation functions
#'
#'
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table .SD
#' @export
flatten_range <- function(x, ...) {
  UseMethod("flatten_range")
}

#' @export
flatten_range.default <- function(object, ...) {
  stop("object needs to be of class 'OpenSpecy'")
}

#' @export
flatten_range.OpenSpecy <- function(object,
                                    min_range = NULL,
                                    max_range = NULL,
                                    make_rel = TRUE,
                                    ...) {
    .flatten_range <- function(wavenumber, spectra, min_range, max_range){

        for(x in 1:length(min_range)){
            spectra[wavenumber >= min_range[x] & wavenumber <= max_range[x]] <- mean(c(spectra[min(which(wavenumber >= min_range[x]))],
                                                                                       spectra[max(which(wavenumber <= max_range[x]))]))
        }

        return(spectra)
    }
    if(is.null(min_range)|is.null(max_range)){
        stop("You need to specify a min and max range to flatten.")
    }
    if(length(min_range) != length(max_range)){
        stop("min_range and max_range need to be the same length.")
    }
    if(any(vapply(1:length(min_range), function(x){
        min_range[x] > max_range[x]
    }, FUN.VALUE = logical(1)))){
        stop("all min_range values must be lower than corresponding max_range")
    }
    filt <- object$spectra[,lapply(.SD, function(x){
        .flatten_range(wavenumber = object$wavenumber,
                       spectra = x,
                       min_range = min_range,
                       max_range = max_range)
    })]

    if (make_rel) object$spectra <- filt[, lapply(.SD, make_rel)] else object$spectra <- filt

    return(object)
}
