#' @rdname def_features
#' @title Define features
#'
#' @description
#' Functions for analyzing features, like particles, fragments, or fibers, in
#' spectral map oriented \code{OpenSpecy} object.
#'
#' @details
#' `def_features()` accepts an \code{OpenSpecy} object and a logical or
#' character vector describing which pixels correspond to particles.
#' `collapse_spec()` takes an \code{OpenSpecy} object with particle-specific
#' metadata (from `def_features()`) and collapses the spectra to median
#' intensities for each unique particle.
#' It also updates the metadata with centroid coordinates, while preserving the
#' feature information on area and Feret max.
#'
#' @return
#' An \code{OpenSpecy} object appended with metadata about the features or
#' collapsed for the features.
#'
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' identified_map <- def_features(tiny_map, tiny_map$metadata$x == 0)
#' collapse_spec(identified_map)
#'
#' @param x an \code{OpenSpecy} object
#' @param features a logical vector or character vector describing which of the
#' spectra are of features (\code{TRUE}) and which are not (\code{FALSE}).
#' If a character vector is provided, it should represent the different feature
#' types present in the spectra.
#' @param shape_kernel the width and height of the area in pixels to search for
#' connecting features, c(3,3) is typically used but larger numbers will smooth
#' connections between particles more. 
#' @param \ldots additional arguments passed to subfunctions.
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom data.table data.table as.data.table setDT rbindlist transpose .SD :=
#' @importFrom mmand shapeKernel components
#' @export
collapse_spec <- function(x, ...) {
  UseMethod("collapse_spec")
}

#' @rdname def_features
#'
#' @export
collapse_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname def_features
#'
#' @export
collapse_spec.OpenSpecy <- function(x, ...) {
  # Calculate the median spectra for each unique feature_id
  ts <- transpose(x$spectra)
  ts$id <- x$metadata$feature_id
  x$spectra <- ts[, lapply(.SD, median, na.rm = T), by = "id"] |>
    transpose(make.names = "id")

  x$metadata <- x$metadata |>
    unique(by = c("feature_id", "area", "feret_max", "centroid_y",
                  "centroid_x", "first_x", "first_y", "rand_x", "rand_y"))

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
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname def_features
#'
#' @importFrom data.table as.data.table setDT rbindlist data.table
#' @export
def_features.OpenSpecy <- function(x, features, shape_kernel = c(3,3), ...) {
  if(is.logical(features)) {
    if(all(features) | all(!features))
      stop("features cannot be all TRUE or FALSE because that would indicate ",
           "that there are no distinct features", call. = F)

    features_df <- .def_features(x, features)
  } else if(is.character(features)) {
    if(length(unique(features)) == 1)
      stop("features cannot all have a single name because that would ",
           "indicate that there are no distinct features", call. = F)

    features_df <- rbindlist(lapply(unique(features),
                                    function(y) .def_features(x, features == y, name = y))
    )[!endsWith(feature_id, "-88"),]
  } else {
    stop("features needs to be a character or logical vector", call. = F)
  }

  obj <- x
  x <- y <- feature_id <- NULL # workaround for data.table non-standard
                               # evaluation
  md <- features_df[setDT(obj$metadata), on = c("x", "y")]
  md[, feature_id := ifelse(is.na(feature_id), "-88", feature_id)]
  md[, "centroid_x" := mean(x), by = "feature_id"]
  md[, "centroid_y" := mean(y), by = "feature_id"]
  md[, "first_x" := x[1], by = "feature_id"]
  md[, "first_y" := y[1], by = "feature_id"]
  md[, "rand_x" := sample(x,1), by = "feature_id"]
  md[, "rand_y" := sample(y,1), by = "feature_id"]
  
  obj$metadata <- md

  return(obj)
}


#' @importFrom grDevices chull
#' @importFrom stats dist
.def_features <- function(x, binary, shape_kernel = c(3,3), name = NULL) {
  # Label connected components in the binary image
  binary_matrix <- matrix(binary, ncol = max(x$metadata$x) + 1, byrow = T)
  
  k <- shapeKernel(shape_kernel, type="box")
  labeled_image <- components(binary_matrix, k)

  # Create a dataframe with feature IDs for each true pixel
  feature_points_dt <- data.table(x = x$metadata$x,
                                  y = x$metadata$y,
                                  feature_id = ifelse(binary_matrix,
                                                      labeled_image, -88) |>
                                    t() |> as.vector() |> as.character())

  # Apply the logic to clean components
  cleaned_components <- ifelse(binary_matrix, labeled_image, -88)

  # Calculate the convex hull for each feature
  convex_hulls <- lapply(
    split(
      as.data.frame(which(cleaned_components >= 0, arr.ind = TRUE)),
      cleaned_components[cleaned_components >= 0]
    ),
    function(coords) {coords[unique(chull(coords[,2], coords[,1])),]
    })

  # Calculate area, Feret max, and feature IDs for each feature
  features_dt <- rbindlist(lapply(seq_along(convex_hulls), function(i) {
    hull <- convex_hulls[[i]]
    id <- names(convex_hulls)[i]
    if(nrow(hull) == 1)
      return(data.table(feature_id = id,
                        area = 1,
                        perimeter = 4,
                        feret_min = 1,
                        feret_max = 1)
      )

    # Calculate Feret dimensions
    dist_matrix <- as.matrix(dist(hull))
    feret_max <- max(dist_matrix) + 1

    perimeter <- 0
    cols = 1:nrow(hull)
    rows = c(2:nrow(hull), 1)
    for (j in 1:length(cols)) {
      # Fetch the distance from the distance matrix
      perimeter <- perimeter + dist_matrix[rows[j], cols[j]]
    }

    # Area
    area <- sum(cleaned_components == as.integer(id))

    feret_min = area/feret_max #Can probably calculate this better.

    data.table(feature_id = id,
               area = area,
               perimeter = perimeter,
               feret_min = feret_min,
               feret_max = feret_max
    )
  }), fill = T)

  # Join with the coordinates from the binary image

  feature_points_dt <- feature_points_dt[features_dt, on = "feature_id"]
  
  if(!is.null(name)){
      feature_points_dt$feature_id <- paste0(name, "_", feature_points_dt$feature_id)
  }
  
  feature_points_dt
}
