#' @rdname data_norm
#'
#' @title Normalization and conversion of spectral data
#'
#' @description
#' \code{adj_res()} and \code{conform_res()} are helper functions to conform
#' wavenumbers to different spectral resolutions.
#' \code{adj_neg()} converts numeric intensities \code{y} < 1 into values
#' >= 1, keeping absolute differences between intensity values by shifting each
#' value by the minimum intensity.
#' \code{make_rel()} converts intensities \code{y} into relative values between
#' 0 and 1 using the standard normalization equation.
#' If \code{na.rm} is \code{TRUE}, missing values are removed before the
#' computation proceeds.
#'
#' @details
#' \code{adj_res()} and \code{conform_res()} are used in Open Specy to
#' faciliate comparisons of spectra with different resolutions.
#' \code{adj_neg()} is used to avoid errors that could arise from log
#' transforming spectra when using \code{\link{adj_intens}()} and other
#' functions.
#' \code{make_rel()} is used to retain the relative height proportions between
#' spectra while avoiding the large numbers that can result from some spectral
#' instruments.
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
#' adj_res(seq(500, 4000, 4), 5)
#' conform_res(seq(500, 4000, 4))
#' adj_neg(c(-1000, -1, 0, 1, 10))
#' make_rel(c(-1000, -1, 0, 1, 10))
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} and \code{\link[base]{round}()};
#' \code{\link{adj_intens}()} for log transformation functions
#'
#' @importFrom magrittr %>%
#' @export
adj_res <- function(x, res = 1, fun = round) {
  fun(x / res) * res
}

#' @rdname data_norm
#'
#' @export
conform_res <- function(x, res = 5) {
  seq(adj_res(min(x), res, ceiling), adj_res(max(x), res, floor), by = res)
}

#' @rdname data_norm
#'
#' @export
conform_spec <- function(spec, x, xout){
        c(
            approx(x = x, y = spec, xout = xout)$y
        )
}

#' @rdname data_norm
#'
#' @export
conform_spectra <- function(data, xout, coords = NULL){
    if(is_OpenSpecy(data)){
        as_OpenSpecy(
            x = xout,
            spectra = data$spectra[,lapply(.SD, function(x){
                conform_spec(x = data$wavenumber, spec = x, xout = xout)})],
            metadata = data$metadata,
            coords = coords
        )
    }
}

#' @rdname data_norm
#'
#' @export
combine_OpenSpecy <- function(files, wavenumbers = NULL, coords = NULL){

    if(!is.list(files)){
        lof <- lapply(files, read_spec, coords = NULL)
    }
    else{
        lof <- files
    }

    if(!is.null(wavenumbers)){
        if(wavenumbers == "first"){
            lof <- lapply(lof, function(x) {
                conform_spectra(data = x,
                                xout = lof[[1]]$wavenumber,
                                coords = NULL)
            })
        }
        if(wavenumbers == "range"){
            all = unique(unlist(lapply(lof, function(x) x$wavenumber)))
            lof <- lapply(lof, function(x) {
                                        conform_spectra(data = x,
                                        xout = conform_res(all),
                                        coords = NULL)})
        }
    }

    unlisted <- unlist(lof, recursive = F)

    list <- tapply(unlisted, names(unlisted), FUN = function(x) unname((x)))

    if(length(unique(vapply(list$wavenumber, length, FUN.VALUE = numeric(1)))) > 1 & is.null(wavenumbers)){
        stop("Wavenumbers are not the same between spectra, you need to specify how the wavenumbers should be merged.", call. = F)
    }

    as_OpenSpecy(
        x = list$wavenumber[[1]],
        spectra = as.data.table(list$spectra),
        metadata = rbindlist(list$metadata, fill = T)
    )
}

#' @rdname data_norm
#'
#' @export
adj_neg <- function(y, na.rm = FALSE) {
  if (min(y, na.rm = na.rm) < 1) {
    y + min(y, na.rm = na.rm) %>% abs() + 1
  } else {
    y
  }
}

#' @rdname data_norm
#'
#' @export
make_rel <- function(y, na.rm = FALSE) {
  (y - min(y, na.rm = na.rm)) / (max(y, na.rm = na.rm) - min(y, na.rm = na.rm))
}
