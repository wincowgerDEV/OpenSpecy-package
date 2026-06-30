#' @rdname visual_image
#' @title Attach visual images to spectral map objects
#'
#' @description
#' `add_visual_image()` stores a visual image and map-to-image alignment metadata
#' on an `OpenSpecy` or `Specs` object. `visual_image()` retrieves that
#' attribute. `detect_image_origin()` detects red Thermo Fisher iN10 map boxes
#' in visual images and returns the image coordinates needed for overlays and
#' feature color extraction.
#'
#' @param x an `OpenSpecy` or `Specs` object.
#' @param image image path, raster, matrix, array, raw BMP bytes, or an existing
#' visual-image list.
#' @param bottom_left,top_right numeric length-2 vectors giving the lower-left
#' and upper-right corners of the spectral map in image pixel coordinates.
#' Image x values are left-to-right and image y values are top-to-bottom.
#' @param source optional image source label or file path.
#' @param detection_method optional description of how the origin was detected.
#' @param diagnostics optional diagnostic data to store with the image.
#' @param transform optional list describing a custom transform.
#' @param require logical; should `visual_image()` error when no image is
#' attached?
#' @param red_threshold minimum red-channel value on the 0-255 scale.
#' @param red_ratio minimum red-to-green and red-to-blue ratio for red-box
#' detection.
#' @param min_pixels minimum number of red pixels required for origin detection.
#' @param \ldots reserved for future methods.
#'
#' @return
#' `add_visual_image()` returns `x` with a `visual_image` attribute.
#' `visual_image()` and `detect_image_origin()` return lists.
#'
#' @examples
#' data("raman_hdpe")
#' img <- array(1, dim = c(10, 10, 3))
#' with_image <- add_visual_image(raman_hdpe, img,
#'                                bottom_left = c(1, 10),
#'                                top_right = c(10, 1))
#' visual_image(with_image)$bottom_left
#'
#' @importFrom grDevices as.raster col2rgb readbitmap
#' @importFrom jpeg readJPEG
#' @export
add_visual_image <- function(x, image, bottom_left = NULL, top_right = NULL,
                             source = NULL, detection_method = NULL,
                             diagnostics = NULL, transform = NULL, ...) {
  if (!(is_OpenSpecy(x) || is_Specs(x))) {
    stop("'x' must be an OpenSpecy or Specs object", call. = FALSE)
  }

  vi <- if (is.list(image) && !is.null(image$image)) {
    image
  } else {
    list(image = .read_visual_image(image))
  }

  if (!is.null(bottom_left)) bottom_left <- .validate_image_corner(bottom_left)
  if (!is.null(top_right)) top_right <- .validate_image_corner(top_right)
  if (is.null(source) && is.character(image) && length(image) == 1L)
    source <- image

  vi$source <- source
  vi$bottom_left <- bottom_left
  vi$top_right <- top_right
  vi$transform <- transform
  vi$detection_method <- detection_method
  vi$diagnostics <- diagnostics

  attr(x, "visual_image") <- vi
  x
}

#' @rdname visual_image
#' @export
visual_image <- function(x, require = FALSE, ...) {
  vi <- attr(x, "visual_image")
  if (is.null(vi) && isTRUE(require))
    stop("No visual image is attached to 'x'", call. = FALSE)
  vi
}

#' @rdname visual_image
#' @export
detect_image_origin <- function(image, red_threshold = 50, red_ratio = 2,
                                min_pixels = 1, diagnostics = TRUE, ...) {
  rgb <- .image_rgb_array(.read_visual_image(image))
  red <- rgb[, , 1L] * 255
  green <- rgb[, , 2L] * 255
  blue <- rgb[, , 3L] * 255
  red_pixels <- red > red_threshold &
    red > red_ratio * green &
    red > red_ratio * blue

  red_coords <- which(red_pixels, arr.ind = TRUE)
  if (nrow(red_coords) < min_pixels) {
    stop("Could not detect enough red box pixels in the image", call. = FALSE)
  }

  x_min <- min(red_coords[, "col"])
  x_max <- max(red_coords[, "col"])
  y_top <- min(red_coords[, "row"])
  y_bottom <- max(red_coords[, "row"])

  out <- list(
    bottom_left = c(x_min, y_bottom),
    top_right = c(x_max, y_top),
    detection_method = "red_box"
  )
  if (isTRUE(diagnostics)) {
    out$diagnostics <- list(
      red_pixel_count = nrow(red_coords),
      image_dim = dim(red_pixels),
      red_threshold = red_threshold,
      red_ratio = red_ratio
    )
  }
  out
}

.validate_image_corner <- function(x) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x)) {
    stop("image corners must be numeric length-2 vectors", call. = FALSE)
  }
  as.numeric(x)
}

.read_visual_image <- function(image) {
  if (is.null(image))
    return(NULL)

  if (is.character(image) && length(image) == 1L) {
    if (!file.exists(image))
      stop("image file does not exist: ", image, call. = FALSE)
    ext <- tolower(tools::file_ext(image))
    if (ext %in% c("jpg", "jpeg")) return(jpeg::readJPEG(image))
    if (ext %in% c("bmp", "dib")) return(grDevices::readbitmap(image))
    return(tryCatch(jpeg::readJPEG(image),
                    error = function(e) grDevices::readbitmap(image)))
  }

  if (is.raw(image) || (is.numeric(image) && is.null(dim(image)))) {
    return(.read_visual_bmp_bytes(image))
  }

  image
}

.read_visual_bmp_bytes <- function(image) {
  raw_bytes <- if (is.raw(image)) image else as.raw(image)
  tmp <- tempfile(fileext = ".bmp")
  con <- file(tmp, open = "wb")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw_bytes, con)
  close(con)
  grDevices::readbitmap(tmp)
}

.image_rgb_array <- function(image) {
  if (is.null(image))
    stop("No image data supplied", call. = FALSE)

  if (inherits(image, "raster")) {
    rgb <- grDevices::col2rgb(as.vector(image)) / 255
    return(array(t(rgb), dim = c(nrow(image), ncol(image), 3L)))
  }

  if (is.matrix(image)) {
    if (is.character(image)) {
      rgb <- grDevices::col2rgb(as.vector(image)) / 255
      return(array(t(rgb), dim = c(nrow(image), ncol(image), 3L)))
    }
    z <- .normalize_image_channel(image)
    return(array(rep(z, 3L), dim = c(nrow(image), ncol(image), 3L)))
  }

  if (length(dim(image)) == 3L) {
    rgb <- image[, , seq_len(min(3L, dim(image)[3L])), drop = FALSE]
    if (dim(rgb)[3L] == 1L) rgb <- array(rep(rgb[, , 1L], 3L),
                                         dim = c(dim(rgb)[1:2], 3L))
    rgb <- .normalize_image_channel(rgb)
    return(rgb)
  }

  stop("Unsupported image object", call. = FALSE)
}

.normalize_image_channel <- function(x) {
  d <- dim(x)
  x <- as.numeric(x)
  dim(x) <- d
  if (length(x) && max(x, na.rm = TRUE) > 1) x <- x / 255
  pmin(pmax(x, 0), 1)
}

.resolve_visual_image <- function(x, img = NULL, bottom_left = NULL,
                                  top_right = NULL) {
  vi <- visual_image(x)
  if (is.null(img) && !is.null(vi$image)) img <- vi$image
  if (is.null(bottom_left) && !is.null(vi$bottom_left))
    bottom_left <- vi$bottom_left
  if (is.null(top_right) && !is.null(vi$top_right))
    top_right <- vi$top_right
  list(image = img, bottom_left = bottom_left, top_right = top_right)
}

.map_to_image_coords <- function(x, y, map_dim, bottom_left, top_right) {
  xscale <- (top_right[1L] - bottom_left[1L]) / map_dim[1L]
  yscale <- (bottom_left[2L] - top_right[2L]) / map_dim[2L]
  list(
    x = as.integer(x * xscale + bottom_left[1L]),
    y = as.integer(bottom_left[2L] - y * yscale)
  )
}

.visual_image_raster <- function(image) {
  grDevices::as.raster(.image_rgb_array(image))
}
