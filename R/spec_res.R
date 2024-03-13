#' @rdname spec_res
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
#' @param x a numeric vector with \code{wavenumber} data or an \code{OpenSpecy}
#' object.
#' @param \ldots further arguments passed to subfunctions; currently not used.
#'
#' @return
#' \code{spec_res()} returns a single numeric value.
#'
#' @examples
#' data("raman_hdpe")
#'
#' spec_res(raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @export
spec_res <- function(x, ...) {
  UseMethod("spec_res")
}

#' @rdname spec_res
#'
#' @export
spec_res.default <- function(x, ...) {
    (max(x) - min(x)) / length(x)
}

#' @rdname spec_res
#'
#' @export
spec_res.OpenSpecy <- function(x, ...) {
  do.call("spec_res", list(x = x$wavenumber))
}
