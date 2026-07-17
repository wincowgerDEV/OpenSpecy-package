# Build a Mini Reference Library

``` r

library(OpenSpecy)
```

This example combines a few files bundled with OpenSpecy into a small
reference library. Real library builds usually need larger lookup tables
and more curation, but the same helper functions apply.

## Read And Combine Spectra

[`c_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_spec.md)
and
[`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
default to the widest range represented by the source spectra at
resolution 6. Values outside each source’s original range are kept as
`NA`, so useful spectral regions are not discarded. This compact
vignette example uses the shared overlapping range to keep the rendered
output small.

``` r

mini_files <- c(
  read_extdata("raman_hdpe.csv"),
  read_extdata("ftir_ldpe_soil.asp"),
  read_extdata("raman_atacamit.spc")
)

mini_sources <- lapply(mini_files, read_any)
mini_sources <- lapply(mini_sources, function(x) {
  x$metadata$intensity_units <- "absorbance"
  attr(x, "intensity_unit") <- "absorbance"
  x
})
mini_raw <- c_spec(mini_sources, range = "common", res = 6)

check_OpenSpecy(mini_raw)
#> [1] TRUE
dim(mini_raw$spectra)
#> [1] 67  3
mini_raw$metadata[, "file_name", with = FALSE]
#>             file_name
#>                <char>
#> 1:     raman_hdpe.csv
#> 2: ftir_ldpe_soil.asp
#> 3: raman_atacamit.spc
```

## Create And Fill A Lookup

Lookup templates help users see which metadata values need curation.
When `path` is not supplied, the template is returned as a `data.table`;
users can write it to CSV by supplying `path`. Lookup joins use exact
values, so edit the template until the key column matches the object
metadata.

``` r

template <- make_lib_lookup_template(
  mini_raw,
  columns = "file_name",
  add = c("material", "material_type")
)

template
#>             file_name material material_type
#>                <char>   <char>        <char>
#> 1:     raman_hdpe.csv     <NA>          <NA>
#> 2: ftir_ldpe_soil.asp     <NA>          <NA>
#> 3: raman_atacamit.spc     <NA>          <NA>
```

For this small example, create the lookup table directly in R. The same
shape could come from a CSV edited outside R.

``` r

lookup <- data.table::data.table(
  file_name = basename(mini_files),
  library_type = c("example", "example", "example"),
  material = c("hdpe", "ldpe in soil", "atacamite")
)

hierarchy <- data.table::data.table(
  material = c("hdpe", "ldpe in soil", "atacamite"),
  material_class = c("polyethylene", "polyethylene", "copper mineral"),
  material_type = c("plastic", "plastic", "mineral")
)

join_lib_metadata(mini_raw, lookup, by = "file_name",
                  require_complete = TRUE)$metadata[
  , c("file_name", "library_type", "material"), with = FALSE
]
#>             file_name library_type     material
#>                <char>       <char>       <char>
#> 1:     raman_hdpe.csv      example         hdpe
#> 2: ftir_ldpe_soil.asp      example ldpe in soil
#> 3: raman_atacamit.spc      example    atacamite
```

## Build A Mini Library

[`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
can run the ordinary lookup and material hierarchy joins before applying
named recipes. Recipe names become names in the returned list. Empty
recipes keep the merged spectra unchanged; other recipe lists are passed
to
[`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md).
Missing values and processing attributes are handled automatically.
Metadata column names are also cleaned to lowercase underscore names.
Known aliases are coalesced using an editable lookup table. Variants
that differ only by underscores or one terminal plural `s` match
automatically.

Before sources are merged,
[`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
also converts declared reflectance and transmittance spectra to
absorbance by default. A nonempty `attr(x, "intensity_unit")` is the
primary truth for the whole object; otherwise,
`metadata$intensity_units` is evaluated spectrum by spectrum. Unknown or
missing units are left unchanged with a warning. Use
`convert_intensity = FALSE` when unit handling has already been
completed outside the builder.

The normal input is one or more file paths; one `OpenSpecy` or a list of
`OpenSpecy` objects supports sources already loaded in memory. An RDS
path may store either one object or a list of objects. Other formats are
read with
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md).
Large same-axis source lists are prepared in bulk, which is much faster
for legacy RDS files that store many one-spectrum objects. Named stages
and elapsed time are reported while the library is built; use
`progress = FALSE` when quiet output is preferred. Supplying
`restrict_range_args` triggers the existing
[`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)
operation before deduplication and recipes; multiple retained ranges can
exclude a known silent region without custom workflow code.

``` r

name_lookup <- lib_metadata_name_lookup(
  project_code = c("campaign id", "study code"),
  regex = list(instrument_mode = "^method_[0-9]+$")
)
name_lookup[
  canonical_name %in% c("material_color", "number_of_accumulations")
]
#>             canonical_name             source_name  regex
#>                     <char>                  <char> <char>
#> 1:          material_color          material_color   <NA>
#> 2:          material_color                   color   <NA>
#> 3:          material_color                  colour   <NA>
#> 4: number_of_accumulations number_of_accumulations   <NA>
#> 5: number_of_accumulations  number_of_sample_scans   <NA>
#> 6: number_of_accumulations           coadded_scans   <NA>
lib_clean_name(c("User Name", "Laser (%)", "Method...3"))
#> [1] "user_name"  "laser_perc" "method_3"
```

Named arguments add exact aliases to the defaults, while `regex` adds
patterns evaluated against cleaned names. Overlapping regex patterns
produce an error that identifies the source column and matching rules.
Pass the result as `metadata_name_lookup`. Set
`clean_metadata_values = TRUE` in
[`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
or `clean_values = TRUE` in
[`lib_clean_metadata()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/lib_metadata_name_lookup.md)
to lowercase, trim, and ASCII-normalize character metadata values before
joins. Ordinary and hierarchical joins run whenever their corresponding
lookup input is non-`NULL`. Automatic ordinary lookups use the single
shared column with overlapping values and unique lookup keys. Lookups
with no usable shared key are skipped with a message; lookups with
multiple usable shared keys are treated as ambiguous, so call
[`join_lib_metadata()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
directly when a custom join is needed. Automatic
[`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
lookups coalesce curated lookup values back into existing metadata
columns, so lookup tables can correct fields such as `spectrum_type`.

``` r

mini_libs <- build_lib(
  mini_files,
  recipes = list(
    raw = list(),
    derivative = list(
      conform_spec = FALSE,
      smooth_intens = TRUE,
      smooth_intens_args = list(window = 15, derivative = 1),
      make_rel = TRUE
    ),
    nobaseline = list(
      conform_spec = FALSE,
      smooth_intens = FALSE,
      subtr_baseline = TRUE,
      make_rel = TRUE
    )
  ),
  metadata_lookups = lookup,
  material_hierarchy = hierarchy,
  clean_metadata_values = TRUE,
  convert_intensity = FALSE,
  assess = TRUE,
  dedupe = FALSE
)
#> build_lib [0.0s]: starting
#> build_lib [0.0s]: reading path 1/3 (raman_hdpe.csv)
#> build_lib [0.0s]: reading path 2/3 (ftir_ldpe_soil.asp)
#> build_lib [0.0s]: reading path 3/3 (raman_atacamit.spc)
#> build_lib [0.1s]: preparing 3 source object(s)
#> build_lib [0.1s]: merging 3 source object(s) with c_spec()
#> build_lib [0.1s]: joining metadata lookup 1/1
#> build_lib [0.1s]: joining the material hierarchy
#> build_lib [0.1s]: processing recipe 1/3 (raw)
#> build_lib [0.1s]: calculating signal-to-noise (raw)
#> build_lib [0.1s]: assessing spectra (raw)
#> build_lib [0.2s]: processing recipe 2/3 (derivative)
#> build_lib [0.2s]: calculating signal-to-noise (derivative)
#> build_lib [0.2s]: assessing spectra (derivative)
#> build_lib [0.2s]: processing recipe 3/3 (nobaseline)
#> build_lib [0.3s]: calculating signal-to-noise (nobaseline)
#> build_lib [0.3s]: assessing spectra (nobaseline)
#> build_lib [0.3s]: complete

names(mini_libs)
#> [1] "raw"        "derivative" "nobaseline"
check_OpenSpecy(mini_libs$raw)
#> [1] TRUE
check_OpenSpecy(mini_libs$derivative)
#> [1] TRUE
attr(mini_libs$derivative, "derivative_order")
#> [1] "1"
attr(mini_libs$nobaseline, "baseline")
#> [1] "nobaseline"
mini_libs$raw$metadata[
  , .(file_name, material, material_class, material_type, sn,
      assessment_flag, assessment_checks)
]
#>             file_name     material material_class material_type        sn
#>                <char>       <char>         <char>        <char>     <num>
#> 1:     raman_hdpe.csv         hdpe   polyethylene       plastic  5.542373
#> 2: ftir_ldpe_soil.asp ldpe in soil   polyethylene       plastic 43.439904
#> 3: raman_atacamit.spc    atacamite copper mineral       mineral  5.220850
#>    assessment_flag          assessment_checks
#>             <lgcl>                     <char>
#> 1:            TRUE co2_region; missing_values
#> 2:            TRUE             missing_values
#> 3:            TRUE  high_tail; missing_values
```

## Official Reference-Library Workflow

The version-controlled
[`workflows/OpenSpecy_reference_library.R`](https://github.com/wincowgerDEV/OpenSpecy-package/blob/main/workflows/OpenSpecy_reference_library.R)
script is a straight-line composition of OpenSpecy functions.
Canonically named class, library-type, material-hierarchy, and
known-bad-ID tables live under `workflows/data/`. Raw-data corrections
are completed externally before this workflow runs. The script is
excluded from package builds but remains available in the GitHub
repository so library releases can be reviewed and reproduced.
