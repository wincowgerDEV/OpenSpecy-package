# Attach visual images to spectral map objects

`add_visual_image()` stores a visual image and map-to-image alignment
metadata on an `OpenSpecy` or `Specs` object. `visual_image()` retrieves
that attribute. `detect_image_origin()` detects red Thermo Fisher iN10
map boxes in visual images and returns the image coordinates needed for
overlays and feature color extraction.

## Usage

``` r
add_visual_image(
  x,
  image,
  bottom_left = NULL,
  top_right = NULL,
  source = NULL,
  detection_method = NULL,
  diagnostics = NULL,
  transform = NULL,
  ...
)

visual_image(x, require = FALSE, ...)

detect_image_origin(
  image,
  red_threshold = 50,
  red_ratio = 2,
  min_pixels = 1,
  diagnostics = TRUE,
  ...
)
```

## Arguments

- x:

  an `OpenSpecy` or `Specs` object.

- image:

  image path, raster, matrix, array, raw BMP bytes, or an existing
  visual-image list.

- bottom_left, top_right:

  numeric length-2 vectors giving the lower-left and upper-right corners
  of the spectral map in image pixel coordinates. Image x values are
  left-to-right and image y values are top-to-bottom.

- source:

  optional image source label or file path.

- detection_method:

  optional description of how the origin was detected.

- diagnostics:

  optional diagnostic data to store with the image.

- transform:

  optional list describing a custom transform.

- require:

  logical; should `visual_image()` error when no image is attached?

- red_threshold:

  minimum red-channel value on the 0-255 scale.

- red_ratio:

  minimum red-to-green and red-to-blue ratio for red-box detection.

- min_pixels:

  minimum number of red pixels required for origin detection.

- ...:

  reserved for future methods.

## Value

`add_visual_image()` returns `x` with a `visual_image` attribute.
`visual_image()` and `detect_image_origin()` return lists.

## Examples

``` r
data("raman_hdpe")
img <- array(1, dim = c(10, 10, 3))
with_image <- add_visual_image(raman_hdpe, img,
                               bottom_left = c(1, 10),
                               top_right = c(10, 1))
visual_image(with_image)$bottom_left
#> [1]  1 10
```
