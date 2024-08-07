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
#' metadata (from `def_features()`) and collapses the spectra with a function
#' intensities for each unique particle.
#' It also updates the metadata with centroid coordinates, while preserving the
#' feature information on area and Feret max.
#'
#' @return
#' An \code{OpenSpecy} object appended with metadata about the features or
#' collapsed for the features. All units are in pixels. Metadata described below.
#' \describe{
#'   \item{`x`}{x coordinate of the pixel or centroid if collapsed}
#'   \item{`y`}{y coordinate of the pixel or centroid if collapsed}
#'   \item{`feature_id`}{unique identifier of each feature}
#'   \item{`area`}{area in pixels of the feature}
#'   \item{`perimeter`}{perimeter of the convex hull of the feature}
#'   \item{`feret_min`}{feret_max divided by the area}
#'   \item{`feret_max`}{largest dimension of the convex hull of the feature}
#'   \item{`convex_hull_area`}{area of the convex hull}
#'   \item{`centroid_x`}{mean x coordinate of the feature}
#'   \item{`centroid_y`}{mean y coordinate of the feature}
#'   \item{`first_x`}{first x coordinate of the feature}
#'   \item{`first_y`}{first y coordinate of the feature}
#'   \item{`rand_x`}{random x coordinate from the feature}
#'   \item{`rand_y`}{random y coordinate from the feature}
#'   \item{`r`}{if using visual imagery overlay, the red band value at that location}
#'   \item{`g`}{if using visual imagery overlay, the green band value at that location}
#'   \item{`b`}{if using visual imagery overlay, the blue band value at that location}
#' }
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
#' @param shape_type character, options are for the shape used to find connections c("box", "disc", "diamond")
#' @param close logical, whether a closing should be performed using the shape kernel before
#' estimating components.
#' @param close_kernel width and height of the area to close if using the close option. 
#' @param close_type character, options are for the shape used to find connections c("box", "disc", "diamond")
#' @param img a file location where a visual image is that corresponds to the spectral image. 
#' @param bottom_left a two value vector specifying the x,y location in image pixels where
#' the bottom left of the spectral map begins. y values are from the top down while 
#' x values are left to right.
#' @param top_right a two value vector specifying the x,y location in the visual image
#' pixels where the top right of the spectral map extent is. y values are from 
#' the top down while x values are left to right. 
#' @param fun function name to collapse by. 
#' @param column column name in metadata to collapse by. 
#' @param \ldots additional arguments passed to subfunctions.
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom data.table data.table as.data.table setDT rbindlist transpose .SD :=
#' @importFrom mmand shapeKernel components closing
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
collapse_spec.OpenSpecy <- function(x, fun = median, column = "feature_id", ...) {
  # Calculate the collapsed spectra for each unique feature_id
  ts <- transpose(x$spectra)
  ts$id <- x$metadata[[column]]
  x$spectra <- ts[, lapply(.SD, fun, ...), by = "id"] |>
    transpose(make.names = "id")
  

      x$metadata <- x$metadata |>
          unique(by = c(column))    

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
def_features.OpenSpecy <- function(x, features, shape_kernel = c(3,3), shape_type = "box", close = F, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, ...) {
  if(is.logical(features)) {
    if(all(features) | all(!features))
      stop("features cannot be all TRUE or FALSE because that would indicate ",
           "that there are no distinct features", call. = F)

    features_df <- .def_features(x, features, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right)
  } else if(is.character(features)) {
    if(length(unique(features)) == 1)
      stop("features cannot all have a single name because that would ",
           "indicate that there are no distinct features", call. = F)

    features_df <- rbindlist(lapply(unique(features),
                                    function(y) .def_features(x, features == y, shape_kernel = shape_kernel, shape_type, close = close, close_kernel, close_type,  img, bottom_left, top_right, name = y)),
                            fill = T #Allow for flexibility with convex hulls
    )[,test := fifelse(grepl("(background)|(-88)", feature_id), 0, area)][, .SD[test == max(test)], by = c("x", "y")][, .SD[1], by = c("x", "y")]
  } else {
    stop("features needs to be a character or logical vector", call. = F)
  }

  obj <- x
  x <- y <- feature_id <- area <- b <- g <- max_cor_val <- r <- snr <- test <- NULL # workaround for data.table non-standard
                               # evaluation
  md <- features_df[setDT(obj$metadata), on = c("x", "y")]
  md[, feature_id := ifelse(is.na(feature_id), "-88", feature_id)]
  if("snr" %in% names(md)){
      md[, "mean_snr" := mean(snr), by = "feature_id"]
  }
  if("max_cor_val" %in% names(md)){
      md[, "mean_cor" := mean(max_cor_val), by = "feature_id"]
  }
  if(all(c("r", "g", "b") %in% names(md))){
      md[, `:=`(mean_r = as.integer(sqrt(mean(r^2))), 
                mean_g = as.integer(sqrt(mean(g^2))), 
                mean_b = as.integer(sqrt(mean(b^2)))), by = "feature_id"]
  }
  md[, "centroid_x" := mean(x), by = "feature_id"]
  md[, "centroid_y" := mean(y), by = "feature_id"]
  md[, "first_x" := x[1], by = "feature_id"]
  md[, "first_y" := y[1], by = "feature_id"]
  md[, "rand_x" := sample(x,1), by = "feature_id"]
  md[, "rand_y" := sample(y,1), by = "feature_id"]
  
  obj$metadata <- md

  return(obj)
}


#' @importFrom grDevices chull as.raster col2rgb
#' @importFrom jpeg readJPEG
#' @importFrom stats dist
.def_features <- function(x, binary, shape_kernel = c(3,3), shape_type = "box", close = F, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, name = NULL) {
    # Label connected components in the binary image
    # Define the size of the matrix
    nrow <- max(x$metadata$y) + 1
    ncol <- max(x$metadata$x) + 1
    
    # Create an empty matrix filled with NA
    binary_matrix <- matrix(NA, 
                            nrow = nrow, 
                            ncol = ncol)
    
    # Populate the matrix with your data
    x_coords <- x$metadata$x
    y_coords <- x$metadata$y
    
    for (i in 1:length(binary)) {
        binary_matrix[y_coords[i] + 1, x_coords[i] + 1] <- binary[i]
    }
    
    k <- shapeKernel(shape_kernel, type= shape_type)
    
    if(close){
        kc <- shapeKernel(close_kernel, type=close_type)
        binary_matrix <- closing(binary_matrix, kc)
    }
    labeled_image <- components(binary_matrix, k)
    
    binary_coords <- cbind(y_coords + 1, x_coords + 1)
    # Fetch colors for all coordinates at once
    feature_ids <- labeled_image[binary_coords]
    
    feature_points_dt <- data.table(x = x$metadata$x,
                                    y = x$metadata$y,
                                    feature_id = ifelse(!is.na(feature_ids),
                                                        feature_ids, -88)|> as.character())
    
    #Add color extraction here. 
    if(!is.null(img) & !is.null(bottom_left) & !is.null(top_right)){
        mosaic <- readJPEG(img)
        map_dim <- c(length(unique(x$metadata$x)), 
                     length(unique(x$metadata$y)))
        xscale = (top_right[1]-bottom_left[1])/map_dim[1]
        yscale = (bottom_left[2]-top_right[2])/map_dim[2]
        #particle_centroid = c(875, 4675)/25
        
        x_vals = as.integer(feature_points_dt$x*xscale+bottom_left[1])
        y_vals = as.integer(bottom_left[2] - feature_points_dt$y*yscale)
        colors = character(length = length(x_vals))
        image_raster <- as.raster(mosaic)
        # Create a matrix of coordinates for indexing
        coords <- cbind(y_vals, x_vals)
        # Fetch colors for all coordinates at once
        colors <- image_raster[coords]
        rbg_colors <- col2rgb(colors)
        feature_points_dt$r <- rbg_colors[1,]
        feature_points_dt$g <- rbg_colors[2,]
        feature_points_dt$b <- rbg_colors[3,]
    }
    
    # Apply the logic to clean components
    cleaned_components <- ifelse(!is.na(labeled_image), labeled_image, -88)
    
    # Calculate the convex hull for each feature
    convex_hulls <- lapply(
        split(
            as.data.frame(which(cleaned_components >= 0, arr.ind = TRUE)),
            cleaned_components[cleaned_components >= 0]
        ),
        function(coords) {coords[unique(chull(coords[,2], coords[,1])),]
        })
    
    # Helper function to calculate the area using the shoelace formula
    polygon_area <- function(x, y) {
        n <- length(x)
        area <- 0
        j <- n
        for (i in 1:n) {
            area <- area + (x[j] + x[i]) * (y[j] - y[i])
            j <- i
        }
        return(abs(area) / 2)
    }
    
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
        
        # Calculate the convex hull area
        convex_hull_area <- polygon_area(hull[,2], hull[,1])
        
        feret_min = area/feret_max #Can probably calculate this better.
        
        data.table(feature_id = id,
                   area = area,
                   perimeter = perimeter,
                   feret_min = feret_min,
                   feret_max = feret_max,
                   convex_hull_area = convex_hull_area
        )
    }), fill = T)
    
    # Join with the coordinates from the binary image
    
    feature_points_dt <- feature_points_dt[features_dt, on = "feature_id"]
    
    if(!is.null(name)){
        feature_points_dt$feature_id <- paste0(name, "_", feature_points_dt$feature_id)
    }
    
    feature_points_dt
}
