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
  if (!column %in% names(x$metadata)) {
    stop("Column not found in metadata", call. = FALSE)
  }

  spec_matrix <- as.matrix(x$spectra)
  id <- x$metadata[[column]]

  if (length(id) != ncol(spec_matrix)) {
    stop("Length of grouping column does not match number of spectra", call. = FALSE)
  }

  unique_ids <- unique(id)
  id_factor <- factor(id, levels = unique_ids)

  if (identical(fun, mean) && ...length() == 0) {
    group_counts <- tabulate(id_factor)
    collapsed <- t(rowsum(t(spec_matrix), id_factor, reorder = FALSE) / group_counts)
  } else {
    group_cols <- split(seq_along(id_factor), id_factor)
    collapsed <- vapply(
      group_cols,
      function(idx) apply(spec_matrix[, idx, drop = FALSE], 1, fun, ...),
      numeric(nrow(spec_matrix))
    )
  }

  x$spectra <- as.data.table(collapsed)
  colnames(x$spectra) <- as.character(unique_ids)

  meta_unique <- unique(as.data.table(x$metadata), by = column)
  meta_unique <- meta_unique[match(unique_ids, meta_unique[[column]]), ]

  x$metadata <- meta_unique

  x
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

    precomp <- .def_feature_precompute(x, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right)
    binary_matrix <- matrix(FALSE, nrow = precomp$dims[1L], ncol = precomp$dims[2L])
    binary_matrix[precomp$coords] <- features

    features_df <- .def_features_core(precomp, binary_matrix)
  } else if(is.character(features)) {
    if(length(unique(features)) == 1)
      stop("features cannot all have a single name because that would ",
           "indicate that there are no distinct features", call. = F)

    precomp <- .def_feature_precompute(x, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right)
    feature_idx <- split(seq_along(features), features)

    features_df <- rbindlist(lapply(names(feature_idx), function(name) {
      idx <- feature_idx[[name]]
      binary_matrix <- matrix(FALSE, nrow = precomp$dims[1L], ncol = precomp$dims[2L])
      binary_matrix[precomp$coords[idx, , drop = FALSE]] <- TRUE
      .def_features_core(precomp, binary_matrix, name)
    }), fill = TRUE
    )[,test := fifelse(grepl("(background)|(-88)", feature_id), 0, area)][, .SD[test == max(test)], by = c("x", "y")][, .SD[1], by = c("x", "y")]
  } else {
    stop("features needs to be a character or logical vector", call. = F)
  }

  obj <- x
  x <- y <- feature_id <- area <- b <- g <- max_cor_val <- r <- snr <- test <- NULL # workaround for data.table non-standard
                               # evaluation
  md <- features_df[setDT(obj$metadata), on = c("x", "y")]
  md[, feature_id := ifelse(is.na(feature_id), "-88", feature_id)]

  has_snr <- "snr" %in% names(md)
  has_cor <- "max_cor_val" %in% names(md)
  has_rgb <- all(c("r", "g", "b") %in% names(md))

  md_summary <- md[, {
    list(
      centroid_x = mean(x),
      centroid_y = mean(y),
      first_x = x[1],
      first_y = y[1],
      rand_x = if (.N) sample(x, 1) else NA_real_,
      rand_y = if (.N) sample(y, 1) else NA_real_,
      mean_snr = if (has_snr) mean(snr) else NA_real_,
      mean_cor = if (has_cor) mean(max_cor_val) else NA_real_,
      mean_r = if (has_rgb) as.integer(sqrt(mean(r^2))) else NA_integer_,
      mean_g = if (has_rgb) as.integer(sqrt(mean(g^2))) else NA_integer_,
      mean_b = if (has_rgb) as.integer(sqrt(mean(b^2))) else NA_integer_
    )
  }, by = "feature_id"]
  md <- md_summary[md, on = "feature_id"]

  obj$metadata <- md

  return(obj)
}


#' @importFrom grDevices chull as.raster col2rgb
#' @importFrom jpeg readJPEG
#' @importFrom stats dist
.def_features <- function(x, binary, shape_kernel = c(3,3), shape_type = "box", close = F, close_kernel = c(4,4), close_type = "box", img = NULL, bottom_left = NULL, top_right = NULL, name = NULL) {
    precomp <- .def_feature_precompute(x, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right)
    binary_matrix <- matrix(FALSE, nrow = precomp$dims[1L], ncol = precomp$dims[2L])
    binary_matrix[precomp$coords] <- binary
    .def_features_core(precomp, binary_matrix, name)
}

.def_feature_precompute <- function(x, shape_kernel, shape_type, close, close_kernel, close_type, img, bottom_left, top_right) {
    x_coords <- x$metadata$x
    y_coords <- x$metadata$y
    coord_index <- cbind(y_coords + 1L, x_coords + 1L)

    list(
      dims = c(max(y_coords) + 1L, max(x_coords) + 1L),
      coords = coord_index,
      k = shapeKernel(shape_kernel, type = shape_type),
      kc = if (close) shapeKernel(close_kernel, type = close_type) else NULL,
      img = img,
      bottom_left = bottom_left,
      top_right = top_right,
      map_dim = c(length(unique(x_coords)), length(unique(y_coords))),
      x_coords = x_coords,
      y_coords = y_coords
    )
}

.def_features_core <- function(precomp, binary_matrix, name = NULL) {
    if (!is.null(precomp$kc)) {
        binary_matrix <- closing(binary_matrix, precomp$kc)
    }

    labeled_image <- components(binary_matrix, precomp$k)

    feature_ids <- labeled_image[precomp$coords]

    feature_points_dt <- data.table(x = precomp$x_coords,
                                    y = precomp$y_coords,
                                    feature_id = ifelse(!is.na(feature_ids),
                                                        feature_ids, -88)|> as.character())

    if(!is.null(precomp$img) & !is.null(precomp$bottom_left) & !is.null(precomp$top_right)){
        mosaic <- readJPEG(precomp$img)
        xscale = (precomp$top_right[1]-precomp$bottom_left[1])/precomp$map_dim[1]
        yscale = (precomp$bottom_left[2]-precomp$top_right[2])/precomp$map_dim[2]

        x_vals = as.integer(feature_points_dt$x*xscale+precomp$bottom_left[1])
        y_vals = as.integer(precomp$bottom_left[2] - feature_points_dt$y*yscale)
        image_raster <- as.raster(mosaic)
        coords <- cbind(y_vals, x_vals)
        colors <- image_raster[coords]
        rbg_colors <- col2rgb(colors)
        feature_points_dt$r <- rbg_colors[1,]
        feature_points_dt$g <- rbg_colors[2,]
        feature_points_dt$b <- rbg_colors[3,]
    }

    cleaned_components <- ifelse(!is.na(labeled_image), labeled_image, -88)

    valid_ids <- unique(cleaned_components[cleaned_components >= 0])

    polygon_area <- function(x, y) {
        n <- length(x)
        next_idx <- c(2:n, 1)
        abs(sum(x * y[next_idx] - y * x[next_idx])) / 2
    }

    features_dt <- rbindlist(lapply(valid_ids, function(id) {
        coords <- which(cleaned_components == id, arr.ind = TRUE)
        if (nrow(coords) == 1)
            return(data.table(feature_id = as.character(id),
                              area = 1,
                              perimeter = 4,
                              feret_min = 1,
                              feret_max = 1,
                              convex_hull_area = 1))

        hull_indices <- unique(chull(coords[, 2], coords[, 1]))
        hull <- coords[hull_indices, , drop = FALSE]

        feret_max <- max(dist(hull)) + 1

        edge_coords <- rbind(hull, hull[1, , drop = FALSE])
        deltas <- diff(edge_coords)
        perimeter <- sum(sqrt(rowSums(deltas^2)))

        area <- nrow(coords)
        convex_hull_area <- polygon_area(hull[, 2], hull[, 1])

        feret_min <- area/feret_max #Can probably calculate this better.

        data.table(feature_id = as.character(id),
                   area = area,
                   perimeter = perimeter,
                   feret_min = feret_min,
                   feret_max = feret_max,
                   convex_hull_area = convex_hull_area
        )
    }), fill = TRUE)

    feature_points_dt <- feature_points_dt[features_dt, on = "feature_id"]

    if(!is.null(name)){
        feature_points_dt$feature_id <- paste0(name, "_", feature_points_dt$feature_id)
    }

    feature_points_dt
}
