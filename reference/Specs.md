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
as_Specs(x, model, kmeans = FALSE, centers = NULL, ...)

fit_specs_pca(x, n_components, center = TRUE, scale. = FALSE, ...)

decompress_spec(x, ...)

# Default S3 method
decompress_spec(x, ...)

# S3 method for class 'Specs'
decompress_spec(x, expand = TRUE, ...)

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

- ...:

  additional arguments passed to submethods.

- model:

  a `SpecsPCA` model returned by `fit_specs_pca()`.

- kmeans:

  logical; should spectra be compressed with K-means after PCA?

- centers:

  passed to [`kmeans()`](https://rdrr.io/r/stats/kmeans.html) when
  `kmeans` is `TRUE`.

- n_components:

  number of PCA components to keep.

- center, scale.:

  arguments passed to [`prcomp()`](https://rdrr.io/r/stats/prcomp.html).

- expand:

  logical; if `TRUE`, decompress or match one row per original
  coordinate; if `FALSE`, keep active spectra or clusters.

- file:

  file path for reading or writing a Specs object.

- compress:

  compression argument passed to
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).

- library:

  a `Specs` object to match against.

- top_n:

  integer; number of top latent matches to return.

- add_library_metadata:

  name of a library metadata column to join.

- add_object_metadata:

  name of an object metadata column to join.

- compute:

  correlation compute strategy, `"optimized"` or `"base"`.

- na.rm:

  logical; should missing values be removed for latent matching?

- features:

  logical or character vector with one value per row in `x$coords`.

- shape_kernel, shape_type, close, close_kernel, close_type, img,
  bottom_left, top_right:

  arguments passed to the feature-definition routine.

- fun:

  function used to collapse latent values.

- column:

  coordinate column used to group spectra for collapse.

## Value

`Specs()` and `as_Specs()` return a `Specs` object. `fit_specs_pca()`
returns a `SpecsPCA` model. `decompress_spec()` returns an approximate
`OpenSpecy` object. `read_specs()` returns a `Specs` object.

## Examples

``` r
data("raman_hdpe")
model <- fit_specs_pca(raman_hdpe, n_components = 1)
specs <- as_Specs(raman_hdpe, model)
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
