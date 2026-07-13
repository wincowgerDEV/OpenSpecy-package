# Automate particle analysis for spectral maps

`automate_particle_analysis()` generalizes the batch map workflow used
for particle detection, spectral matching, particle details, summaries,
and optional base-graphics particle images. Visual images attached to
map objects or read from supported H5 mosaics are used for particle
color extraction when feature definition is requested. It keeps file
output optional and returns all results as R objects.

## Usage

``` r
automate_particle_analysis(
  x,
  library,
  output_dir = NULL,
  images = NULL,
  bottom_left = NULL,
  top_right = NULL,
  origins = NULL,
  material_col = "material_class",
  library_id_col = "sample_name",
  particle_id_strategy = c("collapse", "partial_collapse", "nonspatial_collapse",
    "all_cell_id", "raw"),
  spectral_smooth = FALSE,
  sigma1 = c(1, 1, 1),
  spatial_smooth = FALSE,
  sigma2 = c(3, 3),
  close = FALSE,
  close_kernel = c(4, 4),
  sn_threshold_min = 0.04,
  sn_threshold_max = Inf,
  cor_threshold = 0.7,
  area_threshold = 1,
  label_unknown = FALSE,
  remove_materials = NULL,
  remove_unknown = FALSE,
  pixel_length = 25,
  metric = "sig_times_noise",
  abs = FALSE,
  collapse_function = stats::median,
  outputs = c("details", "summary"),
  process_args = list(),
  specs_steps = c("pca", "kmeans"),
  specs_centers = NULL,
  ...
)
```

## Arguments

- x:

  character vector of files, an `OpenSpecy`/`Specs` object, or a list of
  objects/files.

- library:

  reference `OpenSpecy` object or trained model library passed to
  [`match_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md).

- output_dir:

  optional directory for CSV/RDS/PNG outputs.

- images:

  optional image path(s) or image objects aligned with `x`.

- bottom_left, top_right:

  optional lists of image corners; if missing and an image is supplied,
  [`detect_image_origin()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/visual_image.md)
  is attempted.

- origins:

  optional list with `x` and `y` origin offsets for map-unit outputs.

- material_col:

  material/class column in matched library metadata.

- library_id_col:

  library metadata column used to join match metadata.

- particle_id_strategy:

  one of `"collapse"`, `"partial_collapse"`, `"nonspatial_collapse"`,
  `"all_cell_id"`, or `"raw"`.

- spectral_smooth, sigma1:

  apply 3D Gaussian smoothing to spectral maps; file readers apply this
  while reading and in-memory maps are smoothed after coercion.

- spatial_smooth, sigma2:

  passed to
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
  and feature detection.

- close, close_kernel:

  passed to
  [`def_features()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md).

- sn_threshold_min, sn_threshold_max:

  signal/noise thresholds.

- cor_threshold:

  minimum match value for confident particle labels.

- area_threshold:

  minimum feature area in pixels.

- label_unknown:

  logical; label low-correlation matches as `"unknown"`.

- remove_materials:

  optional material labels to remove after matching.

- remove_unknown:

  logical; remove `"unknown"` after matching.

- pixel_length:

  map pixel length used for output dimensions.

- metric, abs:

  signal/noise arguments passed to
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md).

- collapse_function:

  function used by
  [`collapse_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md).

- outputs:

  character vector containing any of `"details"`, `"summary"`,
  `"particle_image"`, `"particle_heatmap"`,
  `"particle_heatmap_thresholded"`, `"cor_heatmap"`, `"raw"`,
  `"processed"`, or `"time"`. Short aliases `"heatmap"`,
  `"thresholded"`, and `"correlation"` are also accepted.

- process_args:

  optional named list overriding
  [`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md)
  arguments for spectra before matching.

- specs_steps, specs_centers:

  compression controls for Specs-based strategies.

- ...:

  catches removed legacy arguments and otherwise is reserved.

## Value

A list with `samples`, `particle_details_all_csv`, and
`particle_summary_all_csv`. Each per-sample entry uses names matching
file exports where applicable: `particle_details_csv`,
`particle_summary_csv`, `particles_raw_rds`, `particles_rds`,
`particle_image_png`, `particle_heatmap_png`,
`particle_heatmap_thresholded_jpg`, `cor_heatmap_png`, and `time_rds`.
Image entries are recorded base-graphics plots that can be replayed with
[`replayPlot()`](https://rdrr.io/r/grDevices/recordplot.html).

## Examples

``` r
tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
data("test_lib")
res <- automate_particle_analysis(tiny_map, test_lib,
                                  outputs = c("details", "summary"),
                                  sn_threshold_min = 0.1)
names(res)
#> [1] "samples"                  "particle_details_all_csv"
#> [3] "particle_summary_all_csv"
```
