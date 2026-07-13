# Build spectral libraries

Helpers for creating reference libraries from source files and OpenSpecy
objects with metadata lookup tables. `build_lib()` provides the standard
end-to-end workflow while the supporting functions remain available for
advanced composition.

## Usage

``` r
build_lib(
  x,
  recipes = .default_lib_recipes(),
  range = "full",
  res = 6,
  id_col = "sample_name",
  exclude_ids = NULL,
  dedupe = TRUE,
  metadata_lookups = NULL,
  material_hierarchy = NULL,
  metadata_name_lookup = lib_metadata_name_lookup(),
  clean_metadata_values = FALSE,
  convert_intensity = TRUE,
  restrict_range_args = NULL,
  signal_noise = TRUE,
  assess = FALSE,
  progress = TRUE,
  ...
)

make_lib_lookup_template(x, columns, add = NULL, path = NULL)

join_lib_metadata(
  x,
  lookup,
  by,
  require_complete = FALSE,
  return = c("object", "table", "report"),
  suffixes = c(".x", ".y")
)

join_material_hierarchy(
  x,
  hierarchy,
  key_col = "material",
  levels = c("material", "material_class", "material_type"),
  output_names = levels,
  require_complete = FALSE,
  return = c("object", "table", "report")
)

dedupe_spec(
  x,
  id_col = "sample_name",
  exclude_ids = NULL,
  duplicate = c("first", "remove_all", "none"),
  scale = 100,
  algo = "md5"
)

reduce_lib(
  x,
  group_cols = "material_class",
  id_col = "sample_name",
  k = 50,
  min_n = k,
  return = c("object", "ids"),
  ...
)

build_model_lib(
  x,
  class_col = "material_class",
  type_col = "spectrum_type",
  min_n = 10,
  alpha = 0.1,
  seed = 123,
  grouped = TRUE,
  weights = TRUE,
  make_relative = TRUE,
  complete_cases = TRUE,
  ...
)

assess_lib(
  x,
  class_col = NULL,
  id_col = "sample_name",
  nearest = !is.null(class_col)
)
```

## Arguments

- x:

  an `OpenSpecy` or `Specs` object for metadata helpers. For
  `build_lib()`, one `OpenSpecy`, a nonempty list containing only
  `OpenSpecy` objects, or a nonempty character vector of file paths.
  Each RDS path may store either one `OpenSpecy` or a list of them;
  other paths are read with
  [`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md).
  Large same-axis source lists are prepared in bulk to avoid repeated
  legacy object coercion.

- recipes:

  named list of
  [`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md)
  argument lists or functions. Names become names of the returned
  libraries.

- range, res:

  wavenumber range and resolution passed to
  [`c_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_spec.md)
  when `build_lib()` combines multiple sources.

- id_col:

  metadata column used as the spectrum identifier.

- exclude_ids:

  identifiers to remove before returning a library.

- dedupe:

  logical; whether to generate stable IDs and remove duplicated spectra
  in `build_lib()`.

- metadata_lookups:

  a lookup table, csv path, or list of lookup tables and paths. If
  non-`NULL`, each is joined with `join_lib_metadata()`. Automatic
  ordinary lookups use the single shared column that has overlapping
  values and unique lookup keys. Lookups with no usable shared key are
  skipped with a message; lookups with multiple usable shared keys are
  considered ambiguous and stop. Lookup values that share non-key
  metadata column names are coalesced back into those columns, with
  non-missing lookup values taking precedence.

- material_hierarchy:

  hierarchy table or csv path used when non-`NULL`. It is joined with
  `join_material_hierarchy()` using the default `"material"` metadata
  key.

- metadata_name_lookup:

  a data.frame or data.table with `canonical_name`, `source_name`, and
  optional `regex` columns. The default is returned by
  [`lib_metadata_name_lookup()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/lib_metadata_name_lookup.md);
  use `NULL` to clean names without coalescing aliases.

- clean_metadata_values:

  logical; whether `build_lib()` should also lowercase, trim,
  ASCII-normalize, and normalize blank/unknown character metadata values
  in source metadata and lookup tables before joining.

- convert_intensity:

  logical; whether to infer reflectance, transmittance, or absorbance
  units from each source and convert known non-absorbance spectra with
  [`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md)
  before merging. Object attribute `intensity_unit` is authoritative
  when supplied; otherwise metadata column `intensity_units` is
  evaluated per spectrum.

- restrict_range_args:

  optional named list of arguments passed to
  [`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)
  after unit conversion and source merging. Supplying the list triggers
  restriction; `make_rel = FALSE` is used unless explicitly overridden.

- signal_noise:

  logical; whether to append the default
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
  result as metadata column `sn`.

- assess:

  logical; whether to run
  [`assess_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/assess_spec.md)
  on each output library and append assessment summaries to its
  metadata.

- progress:

  logical; whether `build_lib()` reports named processing stages and
  elapsed time.

- columns:

  metadata columns to deduplicate into a template.

- add:

  blank columns to add to a template.

- path:

  optional csv path. If `NULL`, template helpers return a data.table.

- lookup:

  a data.frame, data.table, or csv file path used as a metadata lookup
  table.

- by:

  named character vector mapping metadata columns to lookup columns, or
  an unnamed character vector when the names are the same in both
  tables.

- require_complete:

  logical; if `TRUE`, incomplete joins fail.

- return:

  whether to return an updated `OpenSpecy` object, joined table, report
  list, or selected ids depending on the helper.

- suffixes:

  suffixes used when joined metadata and lookup tables share non-key
  column names.

- hierarchy:

  a data.frame, data.table, or csv file path with hierarchical material
  metadata.

- key_col:

  metadata column containing material labels to match.

- levels:

  hierarchy columns ordered from most-specific to most-general.

- output_names:

  names to use for hierarchy columns added to metadata.

- duplicate:

  how duplicated generated identifiers should be handled.

- scale:

  numeric multiplier used before hashing intensity values.

- algo:

  hash algorithm passed to
  [`digest()`](https://eddelbuettel.github.io/digest/man/digest.html).

- group_cols:

  metadata columns defining groups for reduction.

- k:

  maximum representatives to keep for groups larger than `min_n`.

- min_n:

  groups with `min_n` or fewer spectra are kept whole.

- class_col, type_col:

  metadata columns used for model labels.

- alpha:

  alpha value passed to
  [`glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html).

- seed:

  random seed used before model training.

- grouped:

  logical; whether multinomial coefficients use grouped penalties.

- weights:

  logical; whether to use inverse class-frequency weights.

- make_relative:

  logical; whether to normalize model inputs with
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- complete_cases:

  logical; whether to remove spectra with any missing training values.

- nearest:

  logical; if `TRUE`, `assess_lib()` compares each spectrum with its
  highest-correlation neighbor and reports the fraction where that
  neighbor has the same `class_col` value.

- ...:

  further arguments passed to the underlying operation.

## Value

`build_lib()` returns a named list of `OpenSpecy` libraries.
`join_lib_metadata()`, `join_material_hierarchy()`, `dedupe_spec()`, and
`reduce_lib()` return an updated spectral object unless `return`
requests a table, report, or ids. `make_lib_lookup_template()` returns a
data.table unless `path` is supplied, in which case it writes the csv
and invisibly returns the table. `build_model_lib()` returns a list
suitable for AI classification with
[`match_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md).
`assess_lib()` returns a data.table summary.

## Details

`build_lib()` combines sources over their full wavenumber range,
optionally adds ordinary and hierarchical metadata, removes requested
identifiers, optionally generates stable source-stage duplicate IDs, and
applies named processing recipes. Source-stage IDs follow the reference
library's legacy hash recipe: each source spectrum is trimmed with
[`manage_na`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_na.md)`(type = "remove")`,
conformed at resolution 8, smoothed, and hashed from the resulting
wavenumber/intensity vectors before later merging and range restriction.
The older 100–4000 cm-1 hash is kept in `sample_name_old` when
`id_col = "sample_name"` so `exclude_ids` can remove both current and
legacy curated bad IDs. Metadata column names are first converted to
lowercase underscore names and known aliases are coalesced using
`metadata_name_lookup`; see
[`lib_clean_metadata()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/lib_metadata_name_lookup.md)
for automatic and regular-expression matching. Metadata values can
optionally be normalized to lowercase trimmed character values before
lookup joins. By default, each source is also converted to absorbance
before merging when its intensity units are known. A nonempty
`intensity_unit` object attribute takes precedence over the per-spectrum
`intensity_units` metadata column. Each recipe is either a named list of
arguments passed to
[`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md)
or a function accepting one `OpenSpecy` object. An empty recipe returns
an unprocessed copy. Signal-to-noise is added by default, and optional
[`assess_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/assess_spec.md)
results are summarized into one metadata row per spectrum. Progress
messages report named stages and elapsed time by default so long-running
builds remain observable.

`make_lib_lookup_template()` creates a deduplicated table of metadata
values from an `OpenSpecy` or `Specs` object. Users can fill the added
columns in R or write the template to CSV and curate it elsewhere.

`join_lib_metadata()` left-joins lookup columns onto object metadata and
reports unmatched metadata keys, duplicate lookup keys, and missing
joined values. Joins are exact; clean or harmonize values before calling
this helper.

`join_material_hierarchy()` joins user-defined hierarchical material
metadata. The supplied `levels` are tried from most-specific to
most-general so a material label can match any level in the hierarchy.

`dedupe_spec()` hashes the current spectra and wavenumber axis to create
stable IDs and remove duplicated spectra. Process or conform spectra
before this step when that should affect duplicate detection.

`reduce_lib()` uses PAM medoids to keep representative spectra within
each metadata group. It uses OpenSpecy's optimized correlation routine
on relative, mean-filled spectra.

`build_model_lib()` trains the multinomial `glmnet` model structure used
by OpenSpecy model libraries. Filter, smooth, or otherwise preprocess
spectra before calling this helper.

`assess_lib()` returns a compact summary of object validity, library
size, class balance, and optionally nearest-neighbor class consistency.

## Author

Win Cowger

## Examples

``` r
wavenumber <- seq(100, 6100, by = 100)
base_a <- dnorm(seq(-3, 3, length.out = length(wavenumber)))
base_b <- rev(cumsum(seq_along(wavenumber)))
spectra <- cbind(base_a, base_a + 0.1, base_a + 0.2,
                 base_b, base_b + 0.1, base_b + 0.2)
colnames(spectra) <- paste0("s", seq_len(ncol(spectra)))
mini <- as_OpenSpecy(
  wavenumber,
  spectra = spectra,
  metadata = data.table::data.table(
    sample_name = colnames(spectra),
    source = rep(c("A", "B"), each = 3),
    label = c("nylon 6", "polyamides", "nylon 6",
              "pet", "polyesters", "pet"),
    material_class = rep(c("polyamides", "polyesters"), each = 3),
    spectrum_type = rep("ftir", 6),
    intensity_units = rep("absorbance", 6)
  ),
  attributes = list(intensity_unit = "absorbance")
)

name_lookup <- lib_metadata_name_lookup()
name_lookup[name_lookup$canonical_name == "material_color", ]
#>    canonical_name    source_name  regex
#>            <char>         <char> <char>
#> 1: material_color material_color   <NA>
#> 2: material_color          color   <NA>
#> 3: material_color         colour   <NA>

make_lib_lookup_template(mini, columns = "source", add = "library_type")
#>    source library_type
#>    <char>       <char>
#> 1:      A         <NA>
#> 2:      B         <NA>

source_lookup <- data.frame(
  source = c("A", "B"),
  library_type = c("lab", "field"),
  material = c("nylon 6", "pet")
)
joined <- join_lib_metadata(mini, source_lookup, by = "source",
                            require_complete = TRUE)

hierarchy <- data.frame(
  material = c("nylon 6", "pet"),
  material_class = c("polyamides", "polyesters"),
  material_type = c("plastic", "plastic")
)
joined <- join_material_hierarchy(joined, hierarchy, key_col = "label",
                                  require_complete = TRUE)

deduped <- dedupe_spec(joined)
reduced <- reduce_lib(deduped, group_cols = "material_class",
                      k = 1, min_n = 1)
libs <- build_lib(
  mini,
  recipes = list(
    raw = list(),
    derivative = list(
      conform_spec = FALSE,
      smooth_intens = TRUE,
      smooth_intens_args = list(window = 15, derivative = 1),
      make_rel = TRUE
    )
  ),
  metadata_lookups = source_lookup,
  material_hierarchy = hierarchy,
  restrict_range_args = list(min = 100, max = 6000),
  assess = TRUE,
  dedupe = FALSE
)
#> build_lib [0.0s]: starting
#> build_lib [0.0s]: using one in-memory OpenSpecy source
#> build_lib [0.0s]: preparing 1 source object(s)
#> build_lib [0.0s]: restricting the wavenumber range
#> build_lib [0.0s]: joining metadata lookup 1/1
#> build_lib [0.0s]: joining the material hierarchy
#> build_lib [0.0s]: processing recipe 1/2 (raw)
#> build_lib [0.0s]: calculating signal-to-noise (raw)
#> build_lib [0.0s]: assessing spectra (raw)
#> build_lib [0.0s]: processing recipe 2/2 (derivative)
#> build_lib [0.0s]: calculating signal-to-noise (derivative)
#> build_lib [0.0s]: assessing spectra (derivative)
#> build_lib [0.0s]: complete

model <- suppressWarnings(build_model_lib(
  joined, class_col = "material_class", type_col = NULL, min_n = 2,
  nlambda = 3
))
assess_lib(libs$raw, class_col = "material_class", nearest = FALSE)
#>             metric  value
#>             <char> <char>
#> 1: valid_OpenSpecy   TRUE
#> 2:         spectra      6
#> 3:     wavenumbers     60
#> 4:         classes      2
#> 5:  smallest_class      3
```
