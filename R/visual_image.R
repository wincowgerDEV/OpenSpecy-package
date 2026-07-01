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
#' @importFrom grDevices as.raster col2rgb
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
    if (ext %in% c("bmp", "dib")) return(.read_visual_bmp_file(image))
    return(tryCatch(jpeg::readJPEG(image),
                    error = function(e) .read_visual_bmp_file(image)))
  }

  if (is.raw(image) || (is.numeric(image) && is.null(dim(image)))) {
    return(.read_visual_bmp_bytes(image))
  }

  image
}

.read_visual_bmp_file <- function(image) {
  size <- file.info(image)$size
  if (is.na(size) || size <= 0) {
    stop("image file is empty or unreadable: ", image, call. = FALSE)
  }
  con <- file(image, open = "rb")
  on.exit(close(con), add = TRUE)
  .read_visual_bmp_bytes(readBin(con, what = "raw", n = size))
}

.read_visual_bmp_bytes <- function(image) {
  raw_bytes <- if (is.raw(image)) image else as.raw(image)
  if (length(raw_bytes) < 14L)
    stop("BMP data are too short", call. = FALSE)

  has_file_header <- identical(rawToChar(raw_bytes[1:2]), "BM")
  header_start <- if (has_file_header) 15L else 1L
  if (length(raw_bytes) < header_start + 39L)
    stop("Unsupported BMP header", call. = FALSE)

  header_size <- .bmp_uint(raw_bytes[header_start:(header_start + 3L)])
  if (header_size < 40L)
    stop("Only BITMAPINFOHEADER BMP images are supported", call. = FALSE)

  width <- .bmp_int(raw_bytes[(header_start + 4L):(header_start + 7L)])
  height <- .bmp_int(raw_bytes[(header_start + 8L):(header_start + 11L)])
  bits <- .bmp_uint(raw_bytes[(header_start + 14L):(header_start + 15L)])
  compression <- .bmp_uint(raw_bytes[(header_start + 16L):(header_start + 19L)])
  colors_used <- .bmp_uint(raw_bytes[(header_start + 32L):(header_start + 35L)])

  if (width <= 0 || height == 0)
    stop("BMP image dimensions are invalid", call. = FALSE)
  if (compression != 0L)
    stop("Only uncompressed BMP images are supported", call. = FALSE)
  if (!bits %in% c(8L, 24L, 32L))
    stop("Only 8-bit, 24-bit, and 32-bit BMP images are supported",
         call. = FALSE)

  height_abs <- abs(height)
  pixel_offset <- if (has_file_header) {
    .bmp_uint(raw_bytes[11:14]) + 1L
  } else {
    palette_n <- if (bits == 8L) {
      if (colors_used > 0L) colors_used else 256L
    } else {
      0L
    }
    header_start + header_size + palette_n * 4L
  }
  row_stride <- floor((bits * width + 31L) / 32L) * 4L
  pixel_end <- pixel_offset + row_stride * height_abs - 1L
  if (pixel_offset < 1L || pixel_end > length(raw_bytes))
    stop("BMP pixel data are incomplete", call. = FALSE)

  palette <- NULL
  if (bits == 8L) {
    palette_start <- header_start + header_size
    palette_entries <- floor((pixel_offset - palette_start) / 4L)
    palette_n <- if (colors_used > 0L) colors_used else palette_entries
    if (palette_n <= 0L)
      stop("8-bit BMP image is missing a color palette", call. = FALSE)
    palette_raw <- as.integer(raw_bytes[
      palette_start:(palette_start + palette_n * 4L - 1L)
    ])
    palette_bgra <- matrix(palette_raw, ncol = 4L, byrow = TRUE)
    palette <- palette_bgra[, 3:1, drop = FALSE] / 255
  }

  out <- array(0, dim = c(height_abs, width, 3L))
  for (file_row in seq_len(height_abs)) {
    row_start <- pixel_offset + (file_row - 1L) * row_stride
    image_row <- if (height > 0L) height_abs - file_row + 1L else file_row
    if (bits == 8L) {
      idx <- as.integer(raw_bytes[row_start:(row_start + width - 1L)]) + 1L
      rgb <- palette[idx, , drop = FALSE]
    } else {
      bytes_per_pixel <- bits / 8L
      row_raw <- as.integer(raw_bytes[
        row_start:(row_start + width * bytes_per_pixel - 1L)
      ])
      bgr <- matrix(row_raw, ncol = bytes_per_pixel, byrow = TRUE)
      rgb <- bgr[, 3:1, drop = FALSE] / 255
    }
    for (channel in seq_len(3L)) out[image_row, , channel] <- rgb[, channel]
  }

  out
}

.bmp_uint <- function(x) {
  sum(as.integer(x) * 256^(seq_along(x) - 1L))
}

.bmp_int <- function(x) {
  value <- .bmp_uint(x)
  bits <- length(x) * 8L
  if (value >= 2^(bits - 1L)) value - 2^bits else value
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
