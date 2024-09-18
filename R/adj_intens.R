#' @rdname adj_intens
#' @title Adjust spectral intensities to standard absorbance units.
#'
#' @description
#' Converts reflectance or transmittance intensity units to absorbance units and 
#' adjust log or exp transformed units. 
#'
#' @details
#' Many of the Open Specy functions will assume that the spectrum is in
#' absorbance units. For example, see \code{\link{subtr_baseline}()}.
#' To run those functions properly, you will need to first convert any spectra
#' from transmittance or reflectance to absorbance using this function.
#' The transmittance adjustment uses the \eqn{log(1 / T)}
#' calculation which does not correct for system and particle characteristics.
#' The reflectance adjustment uses the Kubelka-Munk equation
#' \eqn{(1 - R)^2 / 2R}. We assume that the reflectance intensity
#' is a percent from 1-100 and first correct the intensity by dividing by 100
#' so that it fits the form expected by the equation.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param type a character string specifying whether the input spectrum is
#' in absorbance units (\code{"none"}, default) or needs additional conversion
#' from \code{"reflectance"} or \code{"transmittance"} data.
#' @param log_exp a character string specifying whether the input needs to be log 
#' transformed \code{"log"}, exp transformed \code{"exp"}, or not (\code{"none"}, default). 
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to submethods; this is
#' to \code{\link{adj_neg}()} for \code{adj_intens()} and
#' to \code{\link{conform_res}()} for \code{conform_intens()}.
#'
#' @return
#' \code{adj_intens()} returns a data frame containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' adj_intens(raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{subtr_baseline}()} for spectral background correction.
#'
#' @importFrom data.table .SD
#' @export
adj_intens <- function(x, ...) {
  UseMethod("adj_intens")
}

#' @rdname adj_intens
#'
#' @export
adj_intens.default <- function(x, 
                               type = "none", 
                               make_rel = TRUE,
                               log_exp = "none", 
                               ...) {
    
    if(!type %in% c("none", "transmittance", "reflectance"))
        stop('type argument must be one of "none", "transmittance", or "reflectance"')
    
    if(!log_exp %in% c("none", "log", "exp"))
        stop('log argument must be one of "none", "log", or "exp"')

    adj <- switch(type,
                  "reflectance" = (1 - x/100)^2 / (2 * x/100),
                  "transmittance" = log(1/adj_neg(x, ...)),
                  "none" = x
    )
    
    adj <- switch(log_exp,
                  "log" = log(adj),
                  "exp" = exp(adj),
                  "none" = adj
    )
    
    if (make_rel) adj <- make_rel(adj) 
    
    return(adj)
}

#' @rdname adj_intens
#'
#' @export
adj_intens.OpenSpecy <- function(x, 
                                 type = "none", 
                                 make_rel = TRUE,
                                 log_exp = "none", 
                                 ...) {

  x$spectra <- x$spectra[, lapply(.SD, adj_intens, type = type, 
                                  make_rel = make_rel,
                                  log_exp = log_exp, 
                                  ...)] 

  return(x)
}
