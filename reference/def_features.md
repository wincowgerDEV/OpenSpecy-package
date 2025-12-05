# Define features

Functions for analyzing features, like particles, fragments, or fibers,
in spectral map oriented `OpenSpecy` object.

## Usage

``` r
collapse_spec(x, ...)

# Default S3 method
collapse_spec(x, ...)

# S3 method for class 'OpenSpecy'
collapse_spec(x, fun = median, column = "feature_id", ...)

def_features(x, ...)

# Default S3 method
def_features(x, ...)

# S3 method for class 'OpenSpecy'
def_features(
  x,
  features,
  shape_kernel = c(3, 3),
  shape_type = "box",
  close = F,
  close_kernel = c(4, 4),
  close_type = "box",
  img = NULL,
  bottom_left = NULL,
  top_right = NULL,
  ...
)
```

## Arguments

- x:

  an `OpenSpecy` object

- fun:

  function name to collapse by.

- column:

  column name in metadata to collapse by.

- features:

  a logical vector or character vector describing which of the spectra
  are of features (`TRUE`) and which are not (`FALSE`). If a character
  vector is provided, it should represent the different feature types
  present in the spectra.

- shape_kernel:

  the width and height of the area in pixels to search for connecting
  features, c(3,3) is typically used but larger numbers will smooth
  connections between particles more.

- shape_type:

  character, options are for the shape used to find connections c("box",
  "disc", "diamond")

- close:

  logical, whether a closing should be performed using the shape kernel
  before estimating components.

- close_kernel:

  width and height of the area to close if using the close option.

- close_type:

  character, options are for the shape used to find connections c("box",
  "disc", "diamond")

- img:

  a file location where a visual image is that corresponds to the
  spectral image.

- bottom_left:

  a two value vector specifying the x,y location in image pixels where
  the bottom left of the spectral map begins. y values are from the top
  down while x values are left to right.

- top_right:

  a two value vector specifying the x,y location in the visual image
  pixels where the top right of the spectral map extent is. y values are
  from the top down while x values are left to right.

- ...:

  additional arguments passed to subfunctions.

## Value

An `OpenSpecy` object appended with metadata about the features or
collapsed for the features. All units are in pixels. Metadata described
below.

- `x`:

  x coordinate of the pixel or centroid if collapsed

- `y`:

  y coordinate of the pixel or centroid if collapsed

- `feature_id`:

  unique identifier of each feature

- `area`:

  area in pixels of the feature

- `perimeter`:

  perimeter of the convex hull of the feature

- `feret_min`:

  feret_max divided by the area

- `feret_max`:

  largest dimension of the convex hull of the feature

- `convex_hull_area`:

  area of the convex hull

- `centroid_x`:

  mean x coordinate of the feature

- `centroid_y`:

  mean y coordinate of the feature

- `first_x`:

  first x coordinate of the feature

- `first_y`:

  first y coordinate of the feature

- `rand_x`:

  random x coordinate from the feature

- `rand_y`:

  random y coordinate from the feature

- `r`:

  if using visual imagery overlay, the red band value at that location

- `g`:

  if using visual imagery overlay, the green band value at that location

- `b`:

  if using visual imagery overlay, the blue band value at that location

## Details

`def_features()` accepts an `OpenSpecy` object and a logical or
character vector describing which pixels correspond to particles.
`collapse_spec()` takes an `OpenSpecy` object with particle-specific
metadata (from `def_features()`) and collapses the spectra with a
function intensities for each unique particle. It also updates the
metadata with centroid coordinates, while preserving the feature
information on area and Feret max.

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
identified_map <- def_features(tiny_map, tiny_map$metadata$x == 0)
collapse_spec(identified_map)
#> Error in loadNamespace(x): there is no package called ‘matrixStats’
```
