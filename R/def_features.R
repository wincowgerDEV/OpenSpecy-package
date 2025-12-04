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
#' @importFrom sf st_as_sf st_cast st_convex_hull st_coordinates st_area st_length st_is_empty
#' @importFrom terra rast ext "crs<-" patches as.polygons cellFromRowCol
#' @export
def_features.OpenSpecy <- function(x, features, shape_kernel = c(3,3), shape_type = "box", close = F, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, ...) {
  if(is.logical(features)) {
    if(all(features) | all(!features))
      stop("features cannot be all TRUE or FALSE because that would indicate ",
           "that there are no distinct features", call. = F)

    features_df <- .def_features_spatial(x, features, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right)
  } else if(is.character(features)) {
    if(length(unique(features)) == 1)
      stop("features cannot all have a single name because that would ",
           "indicate that there are no distinct features", call. = F)

    features_df <- .def_features_spatial_categorical(
      x,
      labels = features,
      shape_kernel = shape_kernel,
      shape_type = shape_type,
      close = close,
      close_kernel = close_kernel,
      close_type = close_type,
      img = img,
      bottom_left = bottom_left,
      top_right = top_right
    )
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
.def_features_spatial <- function(x, binary, shape_kernel = c(3,3), shape_type = "box", close = F, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, name = NULL, keep_background = TRUE) {
    nrow <- max(x$metadata$y) + 1
    ncol <- max(x$metadata$x) + 1

    binary_matrix <- matrix(0, nrow = nrow, ncol = ncol)
    x_coords <- x$metadata$x
    y_coords <- x$metadata$y
    binary_matrix[cbind(y_coords + 1, x_coords + 1)] <- as.integer(binary)

    binary_matrix[binary_matrix <= 0] <- NA
    r_mask <- rast(binary_matrix)
    ext(r_mask) <- c(0, ncol, 0, nrow)
    # Keep raster unitless so sf measurements remain in pixel units
    crs(r_mask) <- NA

    patches_raster <- patches(r_mask, directions = 8)
    names(patches_raster) <- "patch_id"

    cell_ids <- cellFromRowCol(patches_raster, y_coords + 1, x_coords + 1)
    extracted_ids <- terra::values(patches_raster)[cell_ids, 1]
    feature_points_dt <- data.table(
        x = x_coords,
        y = y_coords,
        feature_id = ifelse(is.na(extracted_ids), "-88", as.character(extracted_ids))
    )

    if(!is.null(img) & !is.null(bottom_left) & !is.null(top_right)){
        mosaic <- readJPEG(img)
        map_dim <- c(length(unique(x$metadata$x)),
                     length(unique(x$metadata$y)))
        xscale = (top_right[1]-bottom_left[1])/map_dim[1]
        yscale = (bottom_left[2]-top_right[2])/map_dim[2]

        x_vals = as.integer(feature_points_dt$x*xscale+bottom_left[1])
        y_vals = as.integer(bottom_left[2] - feature_points_dt$y*yscale)
        image_raster <- as.raster(mosaic)
        coords <- cbind(y_vals, x_vals)
        colors <- image_raster[coords]
        rbg_colors <- col2rgb(colors)
        feature_points_dt$r <- rbg_colors[1,]
        feature_points_dt$g <- rbg_colors[2,]
        feature_points_dt$b <- rbg_colors[3,]
    }

    patch_vec <- as.polygons(patches_raster, dissolve = TRUE, na.rm = TRUE)
    polygon_sf <- st_as_sf(patch_vec)
    if(nrow(polygon_sf) > 0){
        polygon_sf <- do.call(rbind, lapply(seq_len(nrow(polygon_sf)), function(i) st_cast(polygon_sf[i, ], "POLYGON")))
        polygon_sf <- polygon_sf[!st_is_empty(polygon_sf), ]
    }

    if(nrow(polygon_sf) > 0){
        convex_hulls <- st_convex_hull(polygon_sf)
        hull_coords <- lapply(st_geometry(convex_hulls), function(g) {
            coords <- st_coordinates(g)
            unique(coords[, c("X", "Y"), drop = FALSE])
        })
        feret_max <- vapply(hull_coords, function(coords){
            if(nrow(coords) < 2) return(0)
            max(stats::dist(coords))
        }, numeric(1))
        feret_max <- pmax(feret_max, 1)
        metrics_dt <- data.table(
            feature_id = as.character(polygon_sf$patch_id),
            area = as.numeric(st_area(polygon_sf)),
            perimeter = as.numeric(st_length(st_cast(polygon_sf, "MULTILINESTRING"))),
            feret_min = as.numeric(st_area(polygon_sf))/feret_max,
            feret_max = feret_max,
            convex_hull_area = as.numeric(st_area(convex_hulls))
        )
    } else {
        metrics_dt <- data.table(
            feature_id = character(),
            area = numeric(),
            perimeter = numeric(),
            feret_min = numeric(),
            feret_max = numeric(),
            convex_hull_area = numeric()
        )
    }

    if(!is.null(name)){
        feature_points_dt[feature_id != "-88", feature_id := paste0(name, "_", feature_id)]
        if(nrow(metrics_dt)) metrics_dt[, feature_id := paste0(name, "_", feature_id)]
    }

    feature_points_dt <- metrics_dt[feature_points_dt, on = "feature_id"]

    if(!keep_background){
        feature_points_dt <- feature_points_dt[feature_id != "-88"]
    }

    feature_points_dt
}

.def_features_spatial_categorical <- function(x, labels, shape_kernel = c(3,3), shape_type = "box", close = FALSE, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, background_labels = c("-88", "background", "Background"), keep_background = FALSE) {
    nrow <- max(x$metadata$y) + 1
    ncol <- max(x$metadata$x) + 1

    labels_factor <- factor(labels, exclude = NULL)
    label_levels <- levels(labels_factor)

    class_matrix <- matrix(NA_integer_, nrow = nrow, ncol = ncol)
    x_coords <- x$metadata$x
    y_coords <- x$metadata$y
    class_matrix[cbind(y_coords + 1, x_coords + 1)] <- as.integer(labels_factor)

    r_classes <- rast(class_matrix)
    ext(r_classes) <- c(0, ncol, 0, nrow)
    # Keep raster unitless so sf measurements remain in pixel units
    crs(r_classes) <- NA

    patches_raster <- patches(r_classes, directions = 8)
    names(patches_raster) <- "patch_id"

    cell_ids <- cellFromRowCol(patches_raster, y_coords + 1, x_coords + 1)
    extracted_ids <- terra::values(patches_raster)[cell_ids, 1]
    label_codes <- as.integer(labels_factor)
    feature_labels <- label_levels[label_codes]

    feature_ids <- ifelse(
        is.na(extracted_ids) | feature_labels %in% background_labels,
        "-88",
        paste0(feature_labels, "_", extracted_ids)
    )

    feature_points_dt <- data.table(
        x = x_coords,
        y = y_coords,
        class_label = feature_labels,
        feature_id = feature_ids
    )

    if(!is.null(img) & !is.null(bottom_left) & !is.null(top_right)){
        mosaic <- readJPEG(img)
        map_dim <- c(length(unique(x$metadata$x)),
                     length(unique(x$metadata$y)))
        xscale = (top_right[1]-bottom_left[1])/map_dim[1]
        yscale = (bottom_left[2]-top_right[2])/map_dim[2]

        x_vals = as.integer(feature_points_dt$x*xscale+bottom_left[1])
        y_vals = as.integer(bottom_left[2] - feature_points_dt$y*yscale)
        image_raster <- as.raster(mosaic)
        coords <- cbind(y_vals, x_vals)
        colors <- image_raster[coords]
        rbg_colors <- col2rgb(colors)
        feature_points_dt$r <- rbg_colors[1,]
        feature_points_dt$g <- rbg_colors[2,]
        feature_points_dt$b <- rbg_colors[3,]
    }

    patch_vec <- as.polygons(patches_raster, dissolve = TRUE, na.rm = TRUE)
    patch_labels <- terra::extract(r_classes, patch_vec, fun = terra::modal, na.rm = TRUE)
    patch_vec$class_label <- label_levels[patch_labels[[2]]]
    polygon_sf <- st_as_sf(patch_vec)
    if(nrow(polygon_sf) > 0){
        polygon_sf <- do.call(rbind, lapply(seq_len(nrow(polygon_sf)), function(i) st_cast(polygon_sf[i, ], "POLYGON")))
        polygon_sf <- polygon_sf[!st_is_empty(polygon_sf), ]
        polygon_sf <- polygon_sf[!(polygon_sf$class_label %in% background_labels | is.na(polygon_sf$class_label)), ]
    }

    if(nrow(polygon_sf) > 0){
        convex_hulls <- st_convex_hull(polygon_sf)
        hull_coords <- lapply(st_geometry(convex_hulls), function(g) {
            coords <- st_coordinates(g)
            unique(coords[, c("X", "Y"), drop = FALSE])
        })
        feret_max <- vapply(hull_coords, function(coords){
            if(nrow(coords) < 2) return(0)
            max(stats::dist(coords))
        }, numeric(1))
        feret_max <- pmax(feret_max, 1)
        metrics_dt <- data.table(
            feature_id = paste0(polygon_sf$class_label, "_", polygon_sf$patch_id),
            area = as.numeric(st_area(polygon_sf)),
            perimeter = as.numeric(st_length(st_cast(polygon_sf, "MULTILINESTRING"))),
            feret_min = as.numeric(st_area(polygon_sf))/feret_max,
            feret_max = feret_max,
            convex_hull_area = as.numeric(st_area(convex_hulls))
        )
    } else {
        metrics_dt <- data.table(
            feature_id = character(),
            area = numeric(),
            perimeter = numeric(),
            feret_min = numeric(),
            feret_max = numeric(),
            convex_hull_area = numeric()
        )
    }

    feature_points_dt <- metrics_dt[feature_points_dt, on = "feature_id"]

    if(!keep_background){
        feature_points_dt <- feature_points_dt[feature_id != "-88"]
    }

    feature_points_dt
}
