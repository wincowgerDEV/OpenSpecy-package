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
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}; \code{x} should be \code{wavenumber} data.
#'
#' @return
#' \code{spec_res()} returns a single numeric value.
#'
#' @examples
#' data("raman_hdpe")
#' spec_res(raman_hdpe$wavenumber)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom dplyr %>%
#' @export
spec_res <- function(x) {
  (max(x) - min(x)) / length(x)
}

