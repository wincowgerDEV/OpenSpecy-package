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
#>      wavenumber          1        -88
#>           <num>      <num>      <num>
#>   1:     717.42 -0.3454294 -0.1846190
#>   2:     725.13 -0.3505079 -0.2030258
#>   3:     732.85 -0.3595735 -0.2236059
#>   4:     740.56 -0.3662870 -0.2380186
#>   5:     748.28 -0.3689005 -0.2436821
#>  ---                                 
#> 423:    3972.81 -0.4140871 -0.3092059
#> 424:    3980.52 -0.4108835 -0.3107601
#> 425:    3988.24 -0.4105957 -0.3180229
#> 426:    3995.95 -0.4125072 -0.3195935
#> 427:    4003.67 -0.4132366 -0.3126730
#> 
#> $metadata
#>        x     y feature_id  area perimeter feret_min feret_max convex_hull_area
#>    <num> <num>     <char> <int>     <num>     <num>     <num>            <num>
#> 1:     0     0          1    13        24         1        13                0
#> 2:     1     0        -88    NA        NA        NA        NA               NA
#>          file_name  license
#>             <char>   <char>
#> 1: CA small UF.dat CC BY-NC
#> 2: CA small UF.dat CC BY-NC
#>                                                     description samples lines
#>                                                          <char>   <num> <num>
#> 1: Pixel 15, XPos=12549.72, YPos= 589.82, X=12362.22, Y= 614.82      16    13
#> 2: Pixel 15, XPos=12549.72, YPos= 589.82, X=12362.22, Y= 614.82      16    13
#>    bands header offset data type interleave                  z plot titles
#>    <num>         <num>     <num>     <char>                         <char>
#> 1:   427             0         4        bip Wavenumbers (cm-1), Absorbance
#> 2:   427             0         4        bip Wavenumbers (cm-1), Absorbance
#>                  pixel size col_id
#>                      <char> <char>
#> 1: 0.000025000, 0.000025000    0_0
#> 2: 0.000025000, 0.000025000    0_1
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/bd79f81e5644779e6540cc62c262599a
#> 2: d862a750602b537df0489a1dd2b06b60/bd79f81e5644779e6540cc62c262599a
#>                             file_id centroid_x centroid_y first_x first_y
#>                              <char>      <num>      <num>   <num>   <num>
#> 1: 215a0fd7e0dc3b1001799214618ce6eb          0          6       0       0
#> 2: 215a0fd7e0dc3b1001799214618ce6eb          8          6       1       0
#>    rand_x rand_y
#>     <num>  <num>
#> 1:      0     10
#> 2:      4      2
```
