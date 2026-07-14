# Changelog

## OpenSpecy 1.7.1

- Added source manifests, app configuration, and GitHub Actions for
  building a hosted Shinylive/WebAssembly app from `inst/shiny/`. The
  hosted app is pinned to a versioned wasm CRAN-like repository
  containing `OpenSpecy` and the app dependency closure, stages the
  small medoid/model libraries, and keeps full library support available
  in the local bundled app.
- Bundled the action-built, commit-pinned wasm library image into
  Shinylive so the app loads the package version in `DESCRIPTION`
  without waiting for the floating webR package repository. Deployment
  now smoke-tests the package version, upload, identification, download,
  and public GitHub Pages endpoint.
- Bundled the Shiny app in `inst/shiny/` from
  `wincowgerDEV/OpenSpecy-shiny` commit
  `60d1bdefff90affcda3353d7c389ea8f3748ca56`;
  [`run_app()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/run_app.md)
  now launches the installed app by default instead of downloading app
  files from GitHub.
- Added bundled-app path, asset, source-parse, YAML-removal, and app
  helper regression tests; optimized/pruned Shiny app static assets and
  fixed app sample-data loading for the current matrix-backed
  `OpenSpecy` spectra format.
- Fixed bundled Shiny app smoke-test issues: startup no longer opens a
  blocking donation modal, bundled UI no longer auto-loads remote image
  assets, and identification uses existing package/app cached reference
  libraries before attempting a download.
- Removed built-in YAML read/write support and the YAML example fixture;
  [`read_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/io_spec.md)
  and
  [`write_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/io_spec.md)
  now support JSON, RDS, and CSV formats.
- Removed runtime `signal` and `cluster` dependencies by using internal
  Savitzky-Golay filtering and PAM medoid selection in package
  workflows.
- Made internal PAM medoid return order deterministic in tied cases so
  `reduce_lib(return = "ids")` is stable across platforms.
- Aligned
  [`automate_particle_analysis()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/automate_particle_analysis.md)
  collapse exports with legacy `analyze_features()` particle details,
  summaries, raw maps, and processed particle objects; returned list
  item names now mirror export filenames and formats.
- Added
  [`automate_particle_analysis()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/automate_particle_analysis.md)
  image return/export support for particle heatmaps, thresholded
  particle heatmaps, and correlation heatmaps. Requested image outputs
  are returned as recorded base-graphics plots, and are written to
  matching image files when `output_dir` is supplied.
- Fixed
  `automate_particle_analysis(particle_id_strategy = "all_cell_id")` so
  cell-level match joins preserve `x`/`y` map coordinates, collapsed
  particle spectra are processed to the library wavenumber axis before
  final matching, H5 mosaic coregistration can drive complete
  edge-tolerant particle color extraction, and single-class character
  feature labels define one class instead of erroring.
- [`particle_image()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/particle_image.md)
  now leaves particle labels off by default and uses the attached visual
  image’s full map extent when overlaying collapsed particle results.
  Particle maps are now drawn as categorical rasters with transparent
  background cells rather than point markers.
- Added a signal/noise heatmap legend, enlarged the correlation heatmap
  legend, and made `automate_particle_analysis(spectral_smooth = TRUE)`
  smooth already-loaded `OpenSpecy`/`Specs` maps as well as file-backed
  maps.
- Fixed visual-image BMP reading without relying on the unavailable
  `grDevices::readbitmap()` helper.
- Fixed `.xyz` text-map reading so coordinate metadata and spectra are
  aligned.

## OpenSpecy 1.7.0

- Improved run_app functionality to allow for version control.
- Added
  [`automate_particle_analysis()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/automate_particle_analysis.md)
  for package-native batch particle detection, matching, summaries, and
  optional file output based on `OpenSpecy`/`Specs` workflows.
- Added visual-image helpers
  ([`add_visual_image()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/visual_image.md),
  [`visual_image()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/visual_image.md),
  and
  [`detect_image_origin()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/visual_image.md))
  so spectral maps can carry aligned visual imagery for feature color
  extraction and base graphics overlays.
- Added
  [`particle_image()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/particle_image.md)
  for dependency-light particle map plotting with the package material
  color defaults.
- Added
  [`crowd_lookup()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/validation_metrics.md),
  [`recovery_rate()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/validation_metrics.md),
  [`minimum_detectable_amount()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/validation_metrics.md),
  and
  [`batch_detection_limit()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/validation_metrics.md)
  for generalized particle-size crowding, spike recovery, MDA, and
  single-blank BDL summaries.
- [`read_h5()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
  now defaults to raw per-region/pixel spectra instead of collapsing by
  particle, preserves region and stage-position metadata, parses scalar
  H5 metadata where possible, and attaches mosaic imagery when present.
- Faster ENVI file reading.
- Add area under band calculation.
- Added library-builder helpers for creating lookup templates, auditing
  metadata joins, reducing libraries with PAM medoids, and training
  model libraries.
- Expanded
  [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
  into the standard end-to-end library workflow with full-range
  resolution-6 merging, lookup-triggered metadata and material hierarchy
  joins, editable metadata-name cleanup, automatic NA-aware recipes,
  signal-to-noise, processing attributes, and optional
  [`assess_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/assess_spec.md)
  metadata summaries.
- [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
  now converts declared reflectance and transmittance sources to
  absorbance before merging. The `intensity_unit` object attribute takes
  precedence over per-spectrum `intensity_units` metadata, and
  conversion can be disabled with `convert_intensity = FALSE`.
- [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
  now accepts file paths, one `OpenSpecy`, or a list of `OpenSpecy`
  objects. Each RDS path may contain either one object or a list, while
  other formats continue through
  [`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md).
  Named progress stages and elapsed time are reported by default and can
  be disabled with `progress = FALSE`. It also accepts optional
  `restrict_range_args` before library recipes. Large same-axis source
  lists are bulk-prepared to avoid repeated legacy object coercion.
- Automatic
  [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
  metadata lookups now infer the single shared column with overlapping
  values and unique lookup keys, skip lookups with no usable shared key,
  remain strict when multiple usable keys are ambiguous, and coalesce
  curated lookup values back into existing metadata columns.
- Added optional metadata value normalization with
  `build_lib(clean_metadata_values = TRUE)` and
  `lib_clean_metadata(clean_values = TRUE)`, used by the reference
  workflow to trim/lowercase metadata values before joins.
- Fixed NA-aware
  [`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md)
  dispatch so downstream arguments such as baseline or intensity `type`
  reach the intended processing function. NA-aware processing now groups
  leading/trailing missing-value ranges and bulk-processes complete
  spectra where possible.
- Optimized
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
  for matrix-native signal/noise summaries, including the default run
  signal-to-noise calculation used by
  [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md).
- [`build_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/build_lib.md)
  now generates reference-library `sample_name` hashes at the source
  stage using the legacy cleanup recipe and removes `exclude_ids`
  against both `sample_name` and `sample_name_old`, preserving
  compatibility with the curated bad-ID hash list.
- [`filter_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
  now treats `NA` values in logical filters as `FALSE` and checks
  logical filter length, preventing spectra/metadata misalignment when
  filtering metadata columns that contain missing values.
- Added a tracked, package-build-excluded
  `workflows/OpenSpecy_reference_library.R` workflow composed only from
  existing package operations, with canonical lookup and exclusion CSVs
  under `workflows/data/`. Repeated filtering, reduction, assessment,
  model building, and artifact writing are applied across named library
  lists.
- The reference workflow now prunes legacy raw-source technical metadata
  using a versioned metadata-drop CSV while retaining modern canonical
  metadata names.
- Exported metadata-name cleaning helpers with automatic underscore and
  terminal-`s` matching, extensible exact aliases, and ambiguity-checked
  regular expression rules.
- [`as_Specs()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/Specs.md)
  now supports an end-to-end compressed `Specs` workflow. By default it
  fits PCA and then Hilbert-encodes the scores into exact high/low
  64-bit code rows; K-means can be placed before, between, or after
  those steps. Hilbert `Specs` objects can be decoded, decompressed back
  to approximate `OpenSpecy` spectra, subset-decompressed by numeric
  index for plotting, and matched with fast Hilbert-code distance.

## OpenSpecy 1.5.0

### Major

- Update to vignettes for new functionality.
- Improved plots
- Improved tests for Open Specy format.
- Improved reading of csv files.
- Improved reading of spa files.
- Extended options for library version downloads.
- Simpler function calling
- Extended baseline fitting options.

## OpenSpecy 1.3.0

### Major

- added 2 new libraries a nobaseline and derivative version of medioid
  and model
- Created new function for spatial smooth without reading envi files
- Allow adj_intens to work on vectors or Open Specy objects

### Minor

- fixed bug with mac reading libraries

## OpenSpecy 1.2.0

CRAN release: 2024-09-14

### Potentially Breaking

- Removed share data options in all functions. They just weren’t useful
  to users at all and were more of an administrative thing. Keeping them
  forced us to be incompatible with webR.

### Major

- added support for siMPle files.
- added support for xyz files.
- added support for img files.
- improved interactive plot popups.
- changed how libraries are downloaded to avoid osfr pacakage.
- increased support for options when collapsing maps.
- avoid forcing min-max relative plots in interactive mode.
- create static map option.

## OpenSpecy 1.1.0

CRAN release: 2024-06-13

### Minor Improvements

- updated links

## OpenSpecy 1.0.9

### Minor Improvements

- more closing and flexibility options

## OpenSpecy 1.0.8

CRAN release: 2024-03-14

### Minor Improvements

- updated `manage_na`, `spec_res`, `read_any` for easier flow with the
  app

## OpenSpecy 1.0.7

CRAN release: 2024-03-11

### Minor Improvements

- Modified `manage_na.R`
- Added to NAMESPACE

## OpenSpecy 1.0.6

CRAN release: 2023-11-25

### Minor Improvements

- Add attributes to `OpenSpecy` objects
- More flexible
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
- Simpler matching

## OpenSpecy 1.0.5

CRAN release: 2023-10-31

### Minor Improvements

- Support .tsv files

### Bug Fixes

- Flip xy coordinates in ENVI files

## OpenSpecy 1.0.4

CRAN release: 2023-10-02

### Minor Improvements

- More contributors
- `showlegend` argument for interactive plots

### Bug Fixes

- Fixes a fatal error in
  [`match_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
  probably causing incorrect identifications

## OpenSpecy 1.0.3

CRAN release: 2023-09-13

### Minor Improvements

- Simplify
  [`check_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
- Improve unit tests
- Improve interactive plots

## OpenSpecy 1.0.2

CRAN release: 2023-09-05

### Bug Fixes

- Set data.table threads to 2 for (CRAN) checks

## OpenSpecy 1.0.1

### Bug Fixes

- Fixed spelling mistakes
- Reduced example and test run times for CRAN

## OpenSpecy 1.0.0

### New Features

- Complete package, app, and SOP overhaul!
- The Shiny app has been outsourced to an own GitHub repository:
  <https://github.com/wincowgerDEV/OpenSpecy-shiny>
- Spectra are now stored in dedicated `OpenSpecy` objects, which can be
  managed with a set of new functions including
  [`c_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_spec.md)
  for concatenating spectra or converting them back to tables
- Various functions have been renamed and improved, for instance, to
  facilitate reading (and writing) spectral files
- New functions include
  [`def_features()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md)
  to identify microplastics in spectral maps and
  [`ai_classify()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
  to use AI for matching/identifying spectra

### Minor Improvements

- Added pkgdown documentation
- Added code coverage tests

## OpenSpecy 0.9.5

CRAN release: 2022-07-06

### Bug Fixes

- Fixed outdated links and redirects

## OpenSpecy 0.9.4

### Minor Improvements

- UI improvements
- Gitter support

### Bug Fixes

- Fixed invalid regex failing CRAN checks

## OpenSpecy 0.9.3

CRAN release: 2021-10-13

### Minor Improvements

- Better error handling for .csv formats
- Add funders and goals
- Updated package citation
- CI testing for Mac

### Bug Fixes

- Fixed testthat routines occasionally failing CRAN checks

## OpenSpecy 0.9.2

CRAN release: 2021-05-20

### New Features

- Manual baseline corrections
- Citable technical note

### Minor Improvements

- More generic .spa file reading
- Added funding

### Bug Fixes

- UI improvements

## OpenSpecy 0.9.1

CRAN release: 2021-04-11

### Bug Fixes

- Checks fail gracefully if api.osf.io is not reachable
- Adjust UI selectors to comply with inverse axis and not exceed ranges

## OpenSpecy 0.9.0

CRAN release: 2021-04-09

### New Features

- UI overhaul
- Give more control to the user when starting via
  [`run_app()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/run_app.md)

### Minor Improvements

- Reverse spectral axes to comply with most wavenumber scales
- Let users select metadata license
- Improved data sharing and logging capabilities
- Google Analytics removed

### Bug Fixes

- Use tempdir for unit tests and examples

## OpenSpecy 0.8.2

CRAN release: 2021-03-31

### Minor Improvements

- Compliance with CRAN style guide
- More references with DOIs
- Better error/warning messages during Shiny file input

### Bug Fixes

- Fixed bug with Shiny reactive values

## OpenSpecy 0.8.1

### Bug Fixes

- Fix redirecting URLs

## OpenSpecy 0.8.0

### New Features

- Use external Open Specy libraries from OSF
- [`read_asp()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
  for reading Agilent .asp files
- GUI overhaul
- Comprehensive package vignette and function documentation
- Unit testing for main functions

### Minor Improvements

- Better error handling
- Stripped down dependencies

## OpenSpecy 0.7.0

- Transferred code base from openspecy.org to this R package
