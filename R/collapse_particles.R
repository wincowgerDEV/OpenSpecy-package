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
characterize_particles <- function(object, particles) {
    .characterize_particles <- function(object, binary, name = NULL){
        # Label connected components in the binary image
        binary_matrix <- matrix(binary, ncol = max(object$metadata$y)+1, byrow = T)
        labeled_image <- imager::label(imager::as.cimg(binary_matrix), high_connectivity = T)
        
        # Create a dataframe with particle IDs for each true pixel
        particle_points_df <- data.frame(x = object$metadata$x, 
                                         y = object$metadata$y, 
                                         particle_ids = as.character(as.vector(t(ifelse(binary_matrix, labeled_image, -88)))))
        
        # Apply the logic to clean components
        cleaned_components <- ifelse(binary_matrix, labeled_image, -88)
        
        areas <- as.data.table(table(cleaned_components))[cleaned_components != "-88",]
        
        # Calculate the convex hull for each particle
        convex_hulls <- lapply(split(as.data.frame(which(cleaned_components >= 0, arr.ind = TRUE)), cleaned_components[cleaned_components >= 0]), function(coords) {
            coords[unique(chull(coords[,2], coords[,1])),]
        })
        
        # Calculate area and Feret max for each particle
        particles_df <- lapply(1:length(convex_hulls), function(coords) {
            # Area
            area <- areas[[coords, "N"]]
            id <- areas[[coords, "cleaned_components"]]
            
            # Calculate Feret dimensions
            dist_matrix <- as.matrix(dist(convex_hulls[[coords]]))
            feret_max <- max(dist_matrix) + 1
            
            return(data.frame(particle_ids = id, area = area, feret_max = feret_max))
        }) |> bind_rows() |>
            filter(!is.na(particle_ids)) |> 
            inner_join(particle_points_df, by = "particle_ids") |>
            mutate(particle_ids = ifelse(!is.null(name), paste0(name, "_", particle_ids), particle_ids))
        
        return(particles_df)
    }
    
    if(is.logical(particles)){
        particles_df <- .characterize_particles(object, particles)
    } else if(is.character(particles)){
        particles_df <- lapply(particles, function(x){
            logical_particles <- particles == x
            .characterize_particles(object, logical_particles)
        }) |> bind_rows()
    } else {
        stop("Particles needs to be a character or logical vector.", call. = F)
    }
    # Join object$metadata and particles_df on x and y
    left_join(object$metadata, particles_df, by = c("x", "y"))
}

