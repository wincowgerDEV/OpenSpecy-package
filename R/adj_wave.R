#' @rdname adj_wave
#' @title Adjust wavelength to wavenumbers for Raman
#'
#' @description
#' Functions for converting between wave* units. 
#'
#' @return
#' An \code{OpenSpecy} object with new units converted from wavelength 
#' to wavenumbers or a vector with the same conversion. 
#'
#' @examples
#' data("raman_hdpe")
#' raman_wavelength <- raman_hdpe
#' raman_wavelength$wavenumber <- (-1*(raman_wavelength$wavenumber/10^7-1/530))^(-1)
#' adj_wave(raman_wavelength, laser = 530)
#' adj_wave(raman_wavelength$wavenumber, laser = 530)
#'
#' @param x an \code{OpenSpecy} object with wavenumber units specified as 
#' wavelength in nm or a wavelength vector.
#' @param laser the wavelength in nm of the Raman laser.
#' @param \ldots additional arguments passed to submethods.
#' 
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @export
adj_wave <- function(x, ...) {
    UseMethod("adj_wave")
}

#' @rdname adj_wave
#'
#' @export
adj_wave.default <- function(x, laser, ...) {
    10^7*(1/laser - 1/x)
}

#' @rdname adj_wave
#'
#' @export
adj_wave.OpenSpecy <- function(x, laser, ...) {
    x$wavenumber <- adj_wave(x$wavenumber, laser)
    return(x)
}