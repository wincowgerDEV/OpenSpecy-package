#' @rdname restrict_range
#'
#' @title Range Restriction for spectra
#'
#' @description
#' \code{restrict_range()} restricts wavenumber ranges to user specified values.
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
#' test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
#'                            spectra = data.frame(intensity = rnorm(361)))
#' restrict_range(test_noise, min_range = 1000, max_range = 2000)
#' restrict_range(test_noise, min_range = c(1000, 2000),
#'                max_range = c(1500, 2500))
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
#' @importFrom data.table as.data.table
#' @export
restrict_range <- function(x, ...) {
    UseMethod("restrict_range")
}
#' @rdname restrict_range
#' @export
restrict_range.default <- function(object, ...) {
    stop("object needs to be of class 'OpenSpecy'")
}

#' @rdname restrict_range
#'
#' @export
restrict_range.OpenSpecy <- function(object,
                                     min_range = 0,
                                     max_range = 6000,
                                     make_rel = TRUE,
                                        ...) {
    test <- as.data.table(lapply(1:length(min_range), function(x){
        object$wavenumber >= min_range[x] & object$wavenumber <= max_range[x]})
    )

    vals = rowSums(test) > 0

    filt <- object$spectra[vals,]

    object$wavenumber <- object$wavenumber[vals]

    if (make_rel) object$spectra <- make_rel(filt) else object$spectra <- filt

    return(object)
}



