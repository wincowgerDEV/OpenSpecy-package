#' @rdname characterize_particles
#' @title Characterize Particles
#'
#' @description
#' Functions for analyzing particles in spectral map oriented OpenSpecy object.
#'
#' @details
#' `characterize_particles()` accepts an OpenSpecy object and a logical or character vector describing which pixels correspond to particles.
#' `collapse_spectra()` takes an OpenSpecy object with particle-specific metadata
#' (from `characterize_particles()`) and collapses the spectra to median intensities for each unique particle.
#' It also updates the metadata with centroid coordinates, while preserving the particle information on area and Feret max.
#'
#' @return
#' An Open Specy object appended metadata about the particles or collapsed for the particles.
#'
#' @examples
#' #Logical example
#' map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' map$metadata$particles <- map$metadata$x == 0
#' identified_map <- characterize_particles(map, map$metadata$particles)
#' test_collapsed <- collapse_spectra(identified_map)
#'
#' #Character example
#' map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' map$metadata$particles <- ifelse(map$metadata$x == 1, "particle", "not_particle")
#' identified_map <- characterize_particles(map, map$metadata$particles)
#' test_collapsed <- collapse_spectra(identified_map)
#'
#' @param object An OpenSpecy object
#' @param particles A logical vector or character vector describing which of the spectra are
#' of particles (TRUE) and which are not (FALSE). If a character vector is provided, it should
#' represent the different particle types present in the spectra.
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom data.table data.table as.data.table setDT rbindlist transpose .SD :=
#' @export
collapse_spectra <- function(object) {

    # Calculate the median spectra for each unique particle_id
    object$spectra <- transpose(object$spectra)[,id := object$metadata$particle_id][,lapply(.SD, median, na.rm=TRUE), by = id] |>
        transpose(make.names = "id")

    object$metadata <- object$metadata |>
        unique(by = c("particle_ids", "area", "feret_max", "centroid_y",
                      "centroid_x"))

    return(object)
}

#' @rdname characterize_particles
#'
#' @importFrom imager label as.cimg
#' @importFrom data.table as.data.table setDT rbindlist data.table
#' @export
characterize_particles <- function(object, particles) {
    if(is.logical(particles)){
        if(all(particles) | all(!particles)){
            stop("Particles cannot be all TRUE or all FALSE values because that would indicate that there are no distinct particles.")
        }
        particles_df <- .characterize_particles(object, particles)
    } else if(is.character(particles)){
        if(length(unique(particles)) == 1){
            stop("Particles cannot all have a single name because that would indicate that there are no distinct particles.")
        }
        particles_df <- rbindlist(lapply(unique(particles), function(x){
            logical_particles <- particles == x
            .characterize_particles(object, logical_particles)
        }))
    } else {
        stop("Particles needs to be a character or logical vector.", call. = F)
    }

    object$metadata <- particles_df[setDT(object$metadata), on = .(x, y)][,particle_ids := ifelse(is.na(particle_ids), "-88", particle_ids)][,centroid_x := mean(x), by = "particle_ids"][,centroid_y := mean(y), by = "particle_ids"]

    return(object)
}

#' @importFrom grDevices chull
#' @importFrom stats dist
.characterize_particles <- function(x, binary, name = NULL) {
  # Label connected components in the binary image
  binary_matrix <- matrix(binary, ncol = max(x$metadata$y)+1, byrow = T)
  labeled_image <- imager::label(imager::as.cimg(binary_matrix), high_connectivity = T)

  # Create a dataframe with particle IDs for each true pixel
  particle_points_dt <- data.table(x = x$metadata$x,
                                   y = x$metadata$y,
                                   particle_ids = as.character(as.vector(t(ifelse(binary_matrix, labeled_image, -88)))))

  # Apply the logic to clean components
  cleaned_components <- ifelse(binary_matrix, labeled_image, -88)

  # Calculate the convex hull for each particle
  # Calculate the convex hull for each particle
  convex_hulls <- lapply(
    split(
      as.data.frame(which(cleaned_components >= 0, arr.ind = TRUE)),
      cleaned_components[cleaned_components >= 0]
      ),
    function(coords) {coords[unique(chull(coords[,2], coords[,1])),]
  })

  # Calculate area, Feret max, and particle IDs for each particle
  particles_dt <- rbindlist(lapply(seq_along(convex_hulls), function(i) {
    hull <- convex_hulls[[i]]
    id <- names(convex_hulls)[i]

    # Calculate Feret dimensions
    dist_matrix <- as.matrix(dist(hull))
    feret_max <- max(dist_matrix) + 1

    # Area
    area <- sum(cleaned_components == as.integer(id))

    data.table(particle_ids = id, area = area, feret_max = feret_max)
  }), fill = TRUE)

  # Join with the coordinates from the binary image
  particle_points_dt[particles_dt[, particle_ids := if (!is.null(name)) paste0(name, "_", particle_ids) else particle_ids], on = "particle_ids"]
}
