#' @rdname def_features
#' @title Define features
#'
#' @description
#' Functions for analyzing features, like particles, fragments, or fibers, in
#' spectral map oriented OpenSpecy object.
#'
#' @details
#' `def_features()` accepts an OpenSpecy object and a logical or character vector describing which pixels correspond to particles.
#' `collapse_spec()` takes an OpenSpecy object with particle-specific metadata
#' (from `def_features()`) and collapses the spectra to median intensities for each unique particle.
#' It also updates the metadata with centroid coordinates, while preserving the particle information on area and Feret max.
#'
#' @return
#' An Open Specy object appended metadata about the particles or collapsed for the particles.
#'
#' @examples
#' #Logical example
#' map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' map$metadata$particles <- map$metadata$x == 0
#' identified_map <- def_features(map, map$metadata$particles)
#' test_collapsed <- collapse_spec(identified_map)
#'
#' #Character example
#' map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' map$metadata$particles <- ifelse(map$metadata$x == 1, "particle", "not_particle")
#' identified_map <- def_features(map, map$metadata$particles)
#' test_collapsed <- collapse_spec(identified_map)
#'
#' @param x an OpenSpecy object
#' @param particles a logical vector or character vector describing which of the spectra are
#' of particles (TRUE) and which are not (FALSE). If a character vector is provided, it should
#' represent the different particle types present in the spectra.
#' @param \ldots additional arguments passed to subfunctions.
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom data.table data.table as.data.table setDT rbindlist transpose .SD :=
#' @export
collapse_spec <- function(x, ...) {
  UseMethod("collapse_spec")
}

#' @rdname def_features
#'
#' @export
collapse_spec.default <- function(x, ...) {
  stop("'x' needs to be of class 'OpenSpecy'")
}

#' @rdname def_features
#'
#' @export
collapse_spec.OpenSpecy <- function(x, ...) {

  # Calculate the median spectra for each unique particle_id
  ts <- transpose(x$spectra)
  ts$id <- x$metadata$particle_id
  x$spectra <- ts[, lapply(.SD, median, na.rm = T), by = "id"] |>
    transpose(make.names = "id")

  x$metadata <- x$metadata |>
    unique(by = c("particle_id", "area", "feret_max", "centroid_y",
                  "centroid_x"))

  return(x)
}

#' @rdname def_features
#'
#' @export
def_features <- function(x, ...) {
  UseMethod("def_features")
}

#' @rdname def_features
#'
#' @export
def_features.default <- function(x, ...) {
  stop("'x' needs to be of class 'OpenSpecy'")
}

#' @rdname def_features
#'
#' @importFrom imager label as.cimg
#' @importFrom data.table as.data.table setDT rbindlist data.table
#' @export
def_features.OpenSpecy <- function(x, particles, ...) {
  if(is.logical(particles)) {
    if(all(particles) | all(!particles)){
      stop("Features cannot be all TRUE or all FALSE values because that ",
           "would indicate that there are no distinct features")
    }
    particles_df <- .def_features(x, particles)
  } else if(is.character(particles)) {
    if(length(unique(particles)) == 1) {
      stop("Features cannot all have a single name because that would ",
           "indicate that there are no distinct features.")
    }
    particles_df <- rbindlist(lapply(unique(particles), function(y) {
      logical_particles <- particles == y
      .def_features(x, logical_particles)
    }))
  } else {
    stop("Features needs to be a character or logical vector.", call. = F)
  }

  obj <- x
  x <- y <- centroid_x <- centroid_y <- particle_id <- NULL # work around for
  # data.table non-standard evaluation
  md <- particles_df[setDT(obj$metadata), on = c("x", "y")]
  md[, particle_id := ifelse(is.na(particle_id), "-88", particle_id)]
  md[, centroid_x := mean(x), by = "particle_id"]
  md[, centroid_y := mean(y), by = "particle_id"]

  obj$metadata <- md

  return(obj)
}

#' @importFrom grDevices chull
#' @importFrom stats dist
.def_features <- function(x, binary, name = NULL) {
  # Label connected components in the binary image
  binary_matrix <- matrix(binary, ncol = max(x$metadata$y) + 1, byrow = T)
  labeled_image <- imager::label(imager::as.cimg(binary_matrix),
                                 high_connectivity = T)

  # Create a dataframe with particle IDs for each true pixel
  particle_points_dt <- data.table(x = x$metadata$x,
                                   y = x$metadata$y,
                                   particle_id = as.character(as.vector(t(ifelse(binary_matrix, labeled_image, -88)))))

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

    data.table(particle_id = id, area = area, feret_max = feret_max)
  }), fill = T)

  # Join with the coordinates from the binary image
  if (!is.null(name)) {
    particles_dt$particle_id <- paste0(name, "_", particles_dt$particle_id)
  }

  particle_points_dt[particles_dt, on = "particle_id"]
}
