#' @rdname spec_res
#'
#' @title Spectral resolution
#'
#' @description
#' Helper function for calculating the spectral resolution from
#' \code{wavenumber} data.
#'
#' @details
#' The spectral resolution is the the minimum wavenumber, wavelength, or
#' frequency difference between two lines in a spectrum that can still be
#' distinguished.
#'
#' @param object a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}; \code{x} should be \code{wavenumber} data.
#'
#' @return
#' \code{spec_res()} returns a single numeric value.
#'
#' @examples
#' data("raman_hdpe")
#' spec_res(raman_hdpe$wavenumber)
#' spec_res(raman_hdpe)
#' 
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom magrittr %>%
#' @export
spec_res <- function(object, ...) {
UseMethod("spec_res")
}

#' @rdname spec_res
#'
#' @export
spec_res.default <- function(object, ...) {
    freq <- table(diff(object))
    as.numeric(names(freq)[which.max(freq)])
}

#' @rdname spec_res
#'
#' @export
spec_res.OpenSpecy <- function(object) {
  spec_res.default(object$wavenumber)
}

