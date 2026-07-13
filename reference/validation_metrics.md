# Particle analysis validation metrics

Helpers for assessing particle crowding, spike recovery, minimum
detectable amount (MDA), and batch detection limit (BDL) from
particle-count tables.

## Usage

``` r
crowd_lookup(
  x,
  sample_col = "sample_id",
  area_col = "area_um2",
  size_col = "min_length_um",
  material_col = NULL,
  group_cols = sample_col,
  size_threshold = 500,
  surface_area = NULL,
  simulations = 10000,
  seed = NULL,
  na.rm = TRUE
)

recovery_rate(
  x,
  observed_col = "count",
  expected_col = "total_spiked",
  group_cols = NULL,
  pre_recovered_col = NULL,
  na.rm = TRUE
)

minimum_detectable_amount(
  x,
  count_col = "count",
  group_cols = NULL,
  offset = 3,
  md_multiplier = 3.29,
  bdl_multiplier = 4.65,
  spike_replicates = 4,
  round = c("integer", "ceiling", "none"),
  na.rm = TRUE
)

batch_detection_limit(
  x,
  count_col = NULL,
  offset = 3,
  multiplier = 4.65,
  round = c("integer", "ceiling", "none"),
  ...
)
```

## Arguments

- x:

  a data frame or data table.

- sample_col:

  column identifying samples.

- area_col:

  column containing particle area.

- size_col:

  optional column containing particle size. If `NULL`, size is inferred
  as `sqrt(area_col)`.

- material_col:

  optional material column to include in crowding groups.

- group_cols:

  columns used for grouped summaries.

- size_threshold:

  particles larger than this size are assessed for possible crowding.

- surface_area:

  optional analyzed surface area used to calculate percent area covered.

- simulations:

  number of simulated small-particle cumulative-area draws.

- seed:

  optional random seed for reproducible crowding simulations.

- na.rm:

  logical; remove missing values from summaries?

- observed_col, expected_col:

  columns with observed and expected spike counts.

- pre_recovered_col:

  optional column with particles recovered before the automated
  analysis.

- count_col:

  column with blank counts.

- offset, md_multiplier, bdl_multiplier:

  numeric constants used in MDA and BDL formulas.

- spike_replicates:

  number of spike replicates in the MDA formula.

- round:

  one of `"integer"`, `"ceiling"`, or `"none"` for detection-limit
  rounding. `"integer"` matches the historic workflow's
  [`as.integer()`](https://rdrr.io/r/base/integer.html).

- multiplier:

  numeric multiplier used by `batch_detection_limit()`.

- ...:

  reserved for future extensions.

## Value

A `data.table` containing the requested summary.

## Examples

``` r
blanks <- data.frame(sample_id = c("b1", "b2", "b3", "b4"),
                     area_bins = "(0,212]",
                     count = c(0, 0, 0, 11))
minimum_detectable_amount(blanks, group_cols = "area_bins")
#>    area_bins n_blanks method   MDA
#>       <char>    <int> <char> <int>
#> 1:   (0,212]        4    MDA    25
batch_detection_limit(11)
#> [1] 29
```
