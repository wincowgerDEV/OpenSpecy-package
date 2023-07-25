#' Preprocess Spectra
#'
#' Process spectra data by applying various preprocessing steps. This is a monolithic function for all common preprocessing steps in one place.
#'
#' @param object An OpenSpecy object containing metadata and spectral data.
#' @param active_preprocessing Logical value indicating whether to perform preprocessing. If \code{TRUE}, the preprocessing steps will be applied. If \code{FALSE}, the original data will be returned.
#' @param range_decision Logical value indicating whether to restrict the wavenumber range of the spectra.
#' @param min_range Numeric value specifying the minimum wavenumber for range restriction.
#' @param max_range Numeric value specifying the maximum wavenumber for range restriction.
#' @param carbon_dioxide_decision Logical value indicating whether to flatten the range around the carbon dioxide region.
#' @param carbon_dioxide_min Numeric value specifying the minimum wavenumber for the carbon dioxide region.
#' @param carbon_dioxide_max Numeric value specifying the maximum wavenumber for the carbon dioxide region.
#' @param smooth_decision Logical value indicating whether to apply a smoothing filter to the spectra.
#' @param smooth_polynomial Integer value specifying the polynomial order for smoothing.
#' @param smooth_window Integer value specifying the window size for smoothing.
#' @param baseline_decision Logical value indicating whether to subtract the baseline from the spectra.
#' @param baseline_selection Character value specifying the type of baseline subtraction method. Options are "Polynomial", "Linear", "Horizontal", or "Vertical".
#' @param raw_baseline Logical value indicating whether to use the raw baseline values for subtraction.
#' @param baseline_polynomial Integer value specifying the polynomial order for baseline subtraction.
#' @param wavenumber_fit Numeric vector of wavenumbers used for baseline fitting.
#' @param intensity_fit Numeric vector of intensities used for baseline fitting.
#' @param derivative_decision Logical value indicating whether to apply derivative to the spectra.
#' @param derivative_order Integer value specifying the order of the derivative.
#' @param derivative_polynomial Integer value specifying the polynomial order for derivative calculation.
#' @param abs Logical value indicating whether to calculate the absolute values of the derivative.
#' @param derivative_window Integer value specifying the window size for derivative calculation.
#'
#' @return An OpenSpecy object with preprocessed spectra based on the specified parameters.
#'
#' @importFrom magrittr %>%
#' @examples
#' 
#' data <- read_any(read_extdata("CA_tiny_map.zip"))
#' # Process spectra with range restriction and baseline subtraction
#' processed_data <- process_spectra(data, 
#'                                   active_preprocessing = TRUE,
#'                                   range_decision = TRUE, 
#'                                   min_range = 500, 
#'                                   max_range = 3000, 
#'                                   baseline_decision = TRUE,
#'                                   baseline_selection = "Polynomial", 
#'                                   baseline_polynomial = 8,
#'                                   derivative_decision = FALSE)
#'
#' # Process spectra with smoothing and derivative
#' processed_data <- process_spectra(data, 
#'                                   active_preprocessing = TRUE,
#'                                   smooth_decision = TRUE, 
#'                                   smooth_polynomial = 3, 
#'                                   smooth_window = 11,
#'                                   derivative_decision = TRUE,
#'                                   derivative_order = 1,
#'                                   derivative_polynomial = 3,
#'                                   derivative_window = 11)
#'
#' @export
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


