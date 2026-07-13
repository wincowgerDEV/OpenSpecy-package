# Create compressed Specs objects

`Specs` objects store compressed spectral data for large hyperspectral
datasets. They use a structure similar to `OpenSpecy`, but store latent
`variables`, compressed `values`, coordinate data, and metadata
separately.

## Usage

``` r
Specs(variables, values, coords = NULL, metadata = NULL, attributes = list())

is_Specs(x)

check_Specs(x)

as_Specs(x, ...)

# Default S3 method
as_Specs(x, ...)

# S3 method for class 'Specs'
as_Specs(x, ...)

# S3 method for class 'OpenSpecy'
as_Specs(
  x,
  model = NULL,
  steps = c("pca", "hilbert"),
  n_components = NULL,
  centers = NULL,
  bits_per_variable = NULL,
  limits = NULL,
  ...
)

fit_specs_pca(x, n_components, center = TRUE, scale. = FALSE, ...)

decompress_spec(x, ...)

# Default S3 method
decompress_spec(x, ...)

# S3 method for class 'Specs'
decompress_spec(x, expand = TRUE, index = NULL, ...)

encode_specs_hilbert(x, bits_per_variable = NULL, limits = NULL, ...)

decode_specs_hilbert(x, ...)

write_specs(x, file, compress = "xz", ...)

read_specs(file, ...)

# S3 method for class 'Specs'
cor_spec(x, library, na.rm = TRUE, compute = "optimized", ...)

# S3 method for class 'Specs'
match_spec(
  x,
  library,
  top_n = NULL,
  expand = FALSE,
  add_library_metadata = NULL,
  add_object_metadata = NULL,
  compute = "optimized",
  na.rm = TRUE,
  ...
)

# S3 method for class 'Specs'
def_features(
  x,
  features,
  shape_kernel = c(3, 3),
  shape_type = "box",
  close = FALSE,
  close_kernel = c(4, 4),
  close_type = "box",
  img = NULL,
  bottom_left = NULL,
  top_right = NULL,
  ...
)

# S3 method for class 'Specs'
collapse_spec(x, fun = mean, column = "feature_id", ...)
```

## Arguments

- variables:

  vector of latent variable names.

- values:

  numeric matrix with one row per variable and one column per active
  spectrum or cluster.

- coords:

  coordinate `data.frame` or `data.table`; should include `x`, `y`,
  `source_id`, and `value_id`.

- metadata:

  metadata `data.frame` or `data.table` with one row per column in
  `values`.

- attributes:

  list of Specs attributes to attach.

- x:

  an object to test, convert, decompress, or write.

- model:

  optional `SpecsPCA` model returned by `fit_specs_pca()`; if omitted
  and `"pca"` is in `steps`, a model is fit from `x`.

- steps:

  character vector of compression steps. Supported values are `"pca"`,
  `"kmeans"`, and `"hilbert"`. K-means can be placed before, between, or
  after the other steps; PCA cannot be placed after Hilbert encoding.

- n_components:

  number of PCA components to keep.

- centers:

  passed to [`kmeans()`](https://rdrr.io/r/stats/kmeans.html) when
  K-means is used.

- bits_per_variable:

  positive whole number of bits used for each Hilbert-encoded variable.
  If `NULL`, the value is inferred from the number of variables to pack
  the available 64-bit code space.

- limits:

  optional two-column matrix, data frame, or Hilbert model with
  per-variable minimum and maximum values used for quantization.

- center, scale.:

  arguments passed to [`prcomp()`](https://rdrr.io/r/stats/prcomp.html).

- expand:

  logical; if `TRUE`, decompress or match one row per original
  coordinate; if `FALSE`, keep active spectra or clusters.

- index:

  optional positive integer vector selecting spectra to decompress. With
  `expand = TRUE`, indexes refer to rows in `x$coords`; with
  `expand = FALSE`, indexes refer to columns in `x$values`.

- file:

  file path for reading or writing a Specs object.

- compress:

  compression argument passed to
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).

- library:

  a `Specs` object to match against.

- na.rm:

  logical; should missing values be removed for latent matching?

- compute:

  correlation compute strategy, `"optimized"` or `"base"`.

- top_n:

  integer; number of top latent matches to return.

- add_library_metadata:

  name of a library metadata column to join.

- add_object_metadata:

  name of an object metadata column to join.

- features:

  logical or character vector with one value per row in `x$coords`.

- shape_kernel, shape_type, close, close_kernel, close_type, img,
  bottom_left, top_right:

  arguments passed to the feature-definition routine.

- fun:

  function used to collapse latent values.

- column:

  coordinate column used to group spectra for collapse.

- ...:

  additional arguments passed to submethods.

## Value

`Specs()`, `as_Specs()`, `encode_specs_hilbert()`, and
`decode_specs_hilbert()` return a `Specs` object. `fit_specs_pca()`
returns a `SpecsPCA` model. `decompress_spec()` returns an approximate
`OpenSpecy` object. `read_specs()` returns a `Specs` object.

## Author

Win Cowger

## Examples

``` r
data("raman_hdpe")
specs <- as_Specs(raman_hdpe, n_components = 1)
decompress_spec(specs)
#>      wavenumber intensity
#>           <num>     <num>
#>   1:    301.040        26
#>   2:    304.632        50
#>   3:    308.221        48
#>   4:    311.810        45
#>   5:    315.398        46
#>  ---                     
#> 960:   3187.990        71
#> 961:   3190.520        71
#> 962:   3193.060        75
#> 963:   3195.590        75
#> 964:   3198.120        67
#> 
#> $metadata
#>        x     y source_id  value_id    col_id                          file_id
#>    <int> <int>    <char>    <char>    <char>                           <char>
#> 1:     1     1 intensity intensity intensity df52a5cbcf0415c5b3c519308090a3c4
```
