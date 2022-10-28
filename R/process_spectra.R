#' @title Adjust spectral intensities to absorbance units
#'
#' @description
#' Converts reflectance or transmittance intensity units to absorbance units.
#'
#' @details
#' Many of the Open Specy functions will assume that the spectrum is in
#' absorbance units. For example, see \code{\link{match_spec}()} and
#' \code{\link{subtr_bg}()}.
#' To run those functions properly, you will need to first convert any spectra
#' from transmittance or reflectance to absorbance using this function.
#' The transmittance adjustment uses the \eqn{log10(1 / T)}
#' calculation which does not correct for system and particle characteristics.
#' The reflectance adjustment uses the Kubelka-Munk equation
#' \eqn{(1 - R)^2 / 2R}. We assume that the reflectance intensity
#' is a percent from 1-100 and first correct the intensity by dividing by 100
#' so that it fits the form expected by the equation.
#'
#' @param object a list object of class \code{OpenSpecy}.
#' @param type a character string specifying whether the input spectrum is
#' in absorbance units (\code{"none"}, default) or needs additional conversion
#' from \code{"reflectance"} or \code{"transmittance"} data.
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
#' \code{\link{subtr_bg}()} for spectral background correction;
#' \code{\link{match_spec}()} matches spectra with the Open Specy or other
#' reference libraries
#'
#' @importFrom magrittr %>%
#' @importFrom data.table .SD
#' @export
#' 
#' @examples
#' test_noise = as_OpenSpecy(x = seq(400,4000, by = 10), spectra = data.table(intensity = rnorm(361)))
#' test_noise = process_spectra(test_noise, range_decision = T, min_range = 1000, max_range = 3000, carbon_dioxide_decision = T, abs = F)
#' ggplot() +
#' geom_line(aes(x = test_noise$wavenumber, y = test_noise$spectra[[1]]))

process_spectra <- function(object, 
                            active_preprocessing = T, 
                            range_decision = F, 
                            min_range = 0, 
                            max_range = 6000, 
                            carbon_dioxide_decision = F,
                            carbon_dioxide_min = 2200,
                            carbon_dioxide_max = 2420,
                            smooth_decision = F, 
                            smooth_polynomial = 3, 
                            smooth_window = 11,
                            baseline_decision = F, 
                            baseline_selection = "Polynomial", 
                            raw_baseline = F, 
                            baseline_polynomial = 8, 
                            wavenumber_fit = NULL, 
                            intensity_fit = NULL,
                            derivative_decision = T,
                            derivative_order = 1,
                            derivative_polynomial = 3,
                            abs = T,
                            derivative_window = 11){
    if(active_preprocessing){
        object %>% 
            {if(range_decision) restrict_range.OpenSpecy(., 
                                                         min_range = min_range, 
                                                         max_range = max_range, 
                                                         make_rel = F) else . } %>%
            {if(baseline_decision) subtr_bg.OpenSpecy(., 
                                                      degree = baseline_polynomial, 
                                                      wavenumber_fit = wavenumber_fit, 
                                                      intensity_fit = intensity_fit,
                                                      raw = raw_baseline, 
                                                      make_rel = F,
                                                      type = baseline_selection) else . } %>%
            {if(carbon_dioxide_decision) flatten_range.OpenSpecy(., 
                                                                 min_range = 2200,
                                                                 max_range = 2420,
                                                                 make_rel = F) else .} %>%
            {if(smooth_decision) smooth_intens(., 
                                                         p = smooth_polynomial,
                                                         n = smooth_window, 
                                                         m = 0, 
                                                         make_rel = F) else .} %>%
            {if(derivative_decision) smooth_intens(., 
                                                             p = derivative_polynomial,
                                                             n = derivative_window, 
                                                             m = derivative_order, 
                                                             abs = abs,
                                                             make_rel = F) else .}
            
    }
    else{
        object
    }
}


