#' @title Collapse Particles
#'
#' @description
#' Converts spectral map oriented OpenSpecy object to particle centroids based on metadata values.
#'
#' @details
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
#' @examples
#' test_noise = as_OpenSpecy(x = seq(400,4000, by = 10), spectra = data.table(intensity = rnorm(361)))
#' test_noise = process_spectra(test_noise, range_decision = T, min_range = 1000, max_range = 3000, carbon_dioxide_decision = T, abs = F)
#' ggplot() +
#' geom_line(aes(x = test_noise$wavenumber, y = test_noise$spectra[[1]]))

#' @importFrom magrittr %>%
#' @importFrom data.table .SD
#' @export
#' 
#' 
collapse_particles <- function(object, particle_filter = NULL){
    if (!is.null(filter)) {
        object$metadata <- object$metadata[, particle_id := eval(parse(text=filter))]
        
        db$vectors <- db$vectors[, db$metadata$id, with = FALSE]
    }
}


# Characterize particles
#' @param binary_matrix A binary matrix with detected particles
#' @return A list with area and Feret max for each particle
#' @importFrom imager label as.cimg
#' @importFrom data.table as.data.table
#' @export


