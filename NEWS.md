# OpenSpecy 1.7.1

- Fixed bundled Shiny app startup when another attached package caused R to
  resolve dashboard `box()` calls to `graphics::box()`.
- Restored the empty spectrum canvas and made uploaded spectra render before
  reference matching completes. Replaced redundant native progress popups with
  one central status display showing the active phase, elapsed time, and a
  staged progress bar without fragile completion-time estimates. Spectral,
  heatmap, and diagnostic plots now use a cohesive bordered dark theme.
- Added ratio-based CO2 and high-tail quality checks that avoid flagging
  unstructured noise. `flatten_range()` and `restrict_range()` can now assess
  and correct those issues automatically, with guarded batch-wide tail cropping.
  The bundled app enables both corrections and identification by default, gates
  reference results on an uploaded spectrum, and prioritizes downloads according
  to the current upload and identification state. In the app, ordinary
  preprocessing now runs before range/CO2 assessment, and an automatic
  correction is retained only when it strictly increases the number of passing
  spectra; the bundled Test Map exercises both corrections.
- Fixed Test Data, Test Map, Processed Spectra, and Top Matches downloads by
  restoring the native Shiny download link and validating every generated
  payload. Added an always-available, timestamped User Metadata CSV containing
  the current analysis inputs for manual reproducibility, without adding a
  settings-import compatibility contract. Top Match options are collapsed by
  default.
- Refined the bundled app workspace with collapsed-by-default settings and
  download cards, tab-triggered settings expansion, selection-specific download
  labels, responsive gap-free summary layouts, and one dark navy/cyan theme for
  the app chrome, cards, controls, tables, progress widgets, and plots.
- Added named area-under-band ratio indices, explicit custom area-ratio
  composition, `peak_ratio()` for nearest-point or linearly interpolated point
  ratios, and 4S Fill Peaks baseline correction. The app's Quantification tab
  now defaults off and lets users name and save any number of area-band or
  point-ratio definitions from final-processed-axis sliders. It calculates saved
  ratios from the exact final processed spectra displayed in the app, uses
  integer wavenumber sliders, and includes exact definitions, values, and
  processed-spectrum provenance in Processed Spectra and Top Matches downloads.
- Reimplemented 4S Fill Peaks smoothing and suppression in base R, removing the
  compiled `baseline` runtime dependency so the same correction works in local
  R and the hosted WebAssembly app.
- Widened the contextual download action, changed uploaded spectrum traces to
  white, standardized enabled switches to green and white, validated all
  informational disclosures, and restored the historical donation choices in
  an on-demand right-side header dialog. Removed the inactive help and dark-mode
  header toggles, aligned the full-width Spectra and Summary cards, and kept
  disabled child settings inert until their owning analysis switch is enabled.
  Automatic tail mode now visibly disables its manual bounds and explains that
  assessment uses the full processed axis.
- Streamlined the app to one analysis workspace with Preprocessing,
  Identification, and Advanced tabs; moved independent thresholds and map
  controls to Advanced, removed Google Translate and the informational sidebar,
  and moved community, partner, and contract information to the pkgdown source.
- Embedded the hosted Shinylive app immediately below the pkgdown title with
  real Shiny readiness feedback and a viewport full-screen mode that persists
  through upload/download dialogs. GitHub retains a normal README, and brief
  reactive updates no longer flash the app's processing overlay. The standalone
  app is published at `/app/` so it resolves to
  `openanalysis.org/openspecy/app/` through the repository-relative Moore
  Institute hosting fork.
- Added source manifests, app configuration, and GitHub Actions for building a
  hosted Shinylive/WebAssembly app from `inst/shiny/`. The hosted app is pinned
  to a versioned wasm CRAN-like repository containing `OpenSpecy` and the app
  dependency closure, stages the small medoid/model libraries, and keeps full
  library support available in the local bundled app.
- Bundled the action-built, commit-pinned wasm library image into Shinylive so
  the app loads the package version in `DESCRIPTION` without waiting for the
  floating webR package repository. Deployment now smoke-tests the package
  version, upload, identification, download, and public GitHub Pages endpoint.
- Fixed hosted-app startup by including hard dependencies from R's recommended
  packages (including `Matrix`, `survival`, and their closure), skipping the
  unavailable Google Translate connectivity probe in WebAssembly mode, and
  exercising the Shinylive iframe/selectize controls in the browser smoke test.
- Consolidated GitHub Pages publication into one native deployment containing
  the pkgdown site and the self-contained Shinylive app. The complete wasm
  package repository is now retained as a pinned Actions build artifact and
  embedded in the app instead of accumulating public `wasm/<commit>` trees.
- Bundled the Shiny app in `inst/shiny/` from
  `wincowgerDEV/OpenSpecy-shiny` commit
  `60d1bdefff90affcda3353d7c389ea8f3748ca56`; `run_app()` now launches the
  installed app by default instead of downloading app files from GitHub.
- Added bundled-app path, asset, source-parse, YAML-removal, and app helper
  regression tests; optimized/pruned Shiny app static assets and fixed app
  sample-data loading for the current matrix-backed `OpenSpecy` spectra format.
- Fixed bundled Shiny app smoke-test issues: startup no longer opens a blocking
  donation modal, bundled UI no longer auto-loads remote image assets, and
  identification uses existing package/app cached reference libraries before
  attempting a download.
- Removed built-in YAML read/write support and the YAML example fixture;
  `read_spec()` and `write_spec()` now support JSON, RDS, and CSV formats.
- Removed runtime `signal` and `cluster` dependencies by using internal
  Savitzky-Golay filtering and PAM medoid selection in package workflows.
- Made internal PAM medoid return order deterministic in tied cases so
  `reduce_lib(return = "ids")` is stable across platforms.
- Aligned `automate_particle_analysis()` collapse exports with legacy
  `analyze_features()` particle details, summaries, raw maps, and processed
  particle objects; returned list item names now mirror export filenames and
  formats.
- Added `automate_particle_analysis()` image return/export support for particle
  heatmaps, thresholded particle heatmaps, and correlation heatmaps. Requested
  image outputs are returned as recorded base-graphics plots, and are written to
  matching image files when `output_dir` is supplied.
- Fixed `automate_particle_analysis(particle_id_strategy = "all_cell_id")`
  so cell-level match joins preserve `x`/`y` map coordinates, collapsed
  particle spectra are processed to the library wavenumber axis before final
  matching, H5 mosaic coregistration can drive complete edge-tolerant particle
  color extraction, and single-class character feature labels define one class
  instead of erroring.
- `particle_image()` now leaves particle labels off by default and uses the
  attached visual image's full map extent when overlaying collapsed particle
  results. Particle maps are now drawn as categorical rasters with transparent
  background cells rather than point markers.
- Added a signal/noise heatmap legend, enlarged the correlation heatmap legend,
  and made `automate_particle_analysis(spectral_smooth = TRUE)` smooth
  already-loaded `OpenSpecy`/`Specs` maps as well as file-backed maps.
- Fixed visual-image BMP reading without relying on the unavailable
  `grDevices::readbitmap()` helper.
- Fixed `.xyz` text-map reading so coordinate metadata and spectra are aligned.

# OpenSpecy 1.7.0

- Improved run_app functionality to allow for version control. 
- Added `automate_particle_analysis()` for package-native batch particle
  detection, matching, summaries, and optional file output based on
  `OpenSpecy`/`Specs` workflows.
- Added visual-image helpers (`add_visual_image()`, `visual_image()`, and
  `detect_image_origin()`) so spectral maps can carry aligned visual imagery
  for feature color extraction and base graphics overlays.
- Added `particle_image()` for dependency-light particle map plotting with the
  package material color defaults.
- Added `crowd_lookup()`, `recovery_rate()`,
  `minimum_detectable_amount()`, and `batch_detection_limit()` for generalized
  particle-size crowding, spike recovery, MDA, and single-blank BDL summaries.
- `read_h5()` now defaults to raw per-region/pixel spectra instead of
  collapsing by particle, preserves region and stage-position metadata, parses
  scalar H5 metadata where possible, and attaches mosaic imagery when present.
- Faster ENVI file reading. 
- Add area under band calculation. 
- Added library-builder helpers for creating lookup templates, auditing metadata
  joins, reducing libraries with PAM medoids, and training model libraries.
- Expanded `build_lib()` into the standard end-to-end library workflow with
  full-range resolution-6 merging, lookup-triggered metadata and material
  hierarchy joins, editable metadata-name cleanup, automatic NA-aware recipes,
  signal-to-noise, processing attributes, and optional `assess_spec()` metadata
  summaries.
- `build_lib()` now converts declared reflectance and transmittance sources to
  absorbance before merging. The `intensity_unit` object attribute takes
  precedence over per-spectrum `intensity_units` metadata, and conversion can
  be disabled with `convert_intensity = FALSE`.
- `build_lib()` now accepts file paths, one `OpenSpecy`, or a list of
  `OpenSpecy` objects. Each RDS path may contain either one object or a list,
  while other formats continue through `read_any()`. Named progress stages and
  elapsed time are reported by default and can be disabled with
  `progress = FALSE`. It also accepts optional `restrict_range_args` before
  library recipes. Large same-axis source lists are bulk-prepared to avoid
  repeated legacy object coercion.
- Automatic `build_lib()` metadata lookups now infer the single shared column
  with overlapping values and unique lookup keys, skip lookups with no usable
  shared key, remain strict when multiple usable keys are ambiguous, and
  coalesce curated lookup values back into existing metadata columns.
- Added optional metadata value normalization with
  `build_lib(clean_metadata_values = TRUE)` and
  `lib_clean_metadata(clean_values = TRUE)`, used by the reference workflow to
  trim/lowercase metadata values before joins.
- Fixed NA-aware `process_spec()` dispatch so downstream arguments such as
  baseline or intensity `type` reach the intended processing function. NA-aware
  processing now groups leading/trailing missing-value ranges and bulk-processes
  complete spectra where possible.
- Optimized `sig_noise()` for matrix-native signal/noise summaries, including
  the default run signal-to-noise calculation used by `build_lib()`.
- `build_lib()` now generates reference-library `sample_name` hashes at the
  source stage using the legacy cleanup recipe and removes `exclude_ids`
  against both `sample_name` and `sample_name_old`, preserving compatibility
  with the curated bad-ID hash list.
- `filter_spec()` now treats `NA` values in logical filters as `FALSE` and
  checks logical filter length, preventing spectra/metadata misalignment when
  filtering metadata columns that contain missing values.
- Added a tracked, package-build-excluded
  `workflows/OpenSpecy_reference_library.R` workflow composed only from
  existing package operations, with canonical lookup and exclusion CSVs under
  `workflows/data/`. Repeated filtering, reduction, assessment, model building,
  and artifact writing are applied across named library lists.
- The reference workflow now prunes legacy raw-source technical metadata using
  a versioned metadata-drop CSV while retaining modern canonical metadata names.
- Exported metadata-name cleaning helpers with automatic underscore and
  terminal-`s` matching, extensible exact aliases, and ambiguity-checked regular
  expression rules.
- `as_Specs()` now supports an end-to-end compressed `Specs` workflow. By
  default it fits PCA and then Hilbert-encodes the scores into exact high/low
  64-bit code rows; K-means can be placed before, between, or after those steps.
  Hilbert `Specs` objects can be decoded, decompressed back to approximate
  `OpenSpecy` spectra, subset-decompressed by numeric index for plotting, and
  matched with fast Hilbert-code distance.

# OpenSpecy 1.5.0

## Major
- Update to vignettes for new functionality. 
- Improved plots
- Improved tests for Open Specy format. 
- Improved reading of csv files. 
- Improved reading of spa files. 
- Extended options for library version downloads. 
- Simpler function calling
- Extended baseline fitting options. 


# OpenSpecy 1.3.0

## Major
- added 2 new libraries a nobaseline and derivative version of medioid and model
- Created new function for spatial smooth without reading envi files
- Allow adj_intens to work on vectors or Open Specy objects

## Minor
- fixed bug with mac reading libraries

# OpenSpecy 1.2.0

## Potentially Breaking

- Removed share data options in all functions. They just weren't useful to users at all and were more of an administrative thing. Keeping them forced us to be incompatible with webR. 

## Major

- added support for siMPle files. 
- added support for xyz files. 
- added support for img files. 
- improved interactive plot popups. 
- changed how libraries are downloaded to avoid osfr pacakage. 
- increased support for options when collapsing maps. 
- avoid forcing min-max relative plots in interactive mode. 
- create static map option. 


# OpenSpecy 1.1.0

## Minor Improvements

- updated links


# OpenSpecy 1.0.9

## Minor Improvements

- more closing and flexibility options


# OpenSpecy 1.0.8

## Minor Improvements

- updated `manage_na`, `spec_res`, `read_any` for easier flow with the app


# OpenSpecy 1.0.7

## Minor Improvements

- Modified `manage_na.R`
- Added to NAMESPACE


# OpenSpecy 1.0.6

## Minor Improvements

- Add attributes to `OpenSpecy` objects
- More flexible `sig_noise()`
- Simpler matching


# OpenSpecy 1.0.5

## Minor Improvements

- Support .tsv files

## Bug Fixes

- Flip xy coordinates in ENVI files


# OpenSpecy 1.0.4

## Minor Improvements

- More contributors
- `showlegend` argument for interactive plots

## Bug Fixes

- Fixes a fatal error in `match_spec()` probably causing incorrect
  identifications


# OpenSpecy 1.0.3

## Minor Improvements

- Simplify `check_OpenSpecy()`
- Improve unit tests
- Improve interactive plots


# OpenSpecy 1.0.2

## Bug Fixes

- Set data.table threads to 2 for (CRAN) checks


# OpenSpecy 1.0.1

## Bug Fixes

- Fixed spelling mistakes
- Reduced example and test run times for CRAN


# OpenSpecy 1.0.0

## New Features

- Complete package, app, and SOP overhaul!
- The Shiny app has been outsourced to an own GitHub repository:
  https://github.com/wincowgerDEV/OpenSpecy-shiny
- Spectra are now stored in dedicated `OpenSpecy` objects, which can be managed
  with a set of new functions including `c_spec()` for concatenating spectra or
  converting them back to tables
- Various functions have been renamed and improved, for instance, to facilitate
  reading (and writing) spectral files
- New functions include `def_features()` to identify microplastics in spectral
  maps and `ai_classify()` to use AI for matching/identifying spectra

## Minor Improvements

- Added pkgdown documentation
- Added code coverage tests


# OpenSpecy 0.9.5

## Bug Fixes

- Fixed outdated links and redirects


# OpenSpecy 0.9.4

## Minor Improvements

- UI improvements
- Gitter support

## Bug Fixes

- Fixed invalid regex failing CRAN checks 


# OpenSpecy 0.9.3

## Minor Improvements

- Better error handling for .csv formats
- Add funders and goals
- Updated package citation
- CI testing for Mac

## Bug Fixes

- Fixed testthat routines occasionally failing CRAN checks


# OpenSpecy 0.9.2

## New Features

- Manual baseline corrections
- Citable technical note

## Minor Improvements

- More generic .spa file reading
- Added funding

## Bug Fixes

- UI improvements


# OpenSpecy 0.9.1

## Bug Fixes

- Checks fail gracefully if api.osf.io is not reachable
- Adjust UI selectors to comply with inverse axis and not exceed ranges


# OpenSpecy 0.9.0

## New Features

- UI overhaul
- Give more control to the user when starting via `run_app()`

## Minor Improvements

- Reverse spectral axes to comply with most wavenumber scales
- Let users select metadata license
- Improved data sharing and logging capabilities
- Google Analytics removed

## Bug Fixes

- Use tempdir for unit tests and examples


# OpenSpecy 0.8.2

## Minor Improvements

- Compliance with CRAN style guide
- More references with DOIs
- Better error/warning messages during Shiny file input

## Bug Fixes

- Fixed bug with Shiny reactive values


# OpenSpecy 0.8.1

## Bug Fixes

- Fix redirecting URLs


# OpenSpecy 0.8.0

## New Features

- Use external Open Specy libraries from OSF
- `read_asp()` for reading Agilent .asp files
- GUI overhaul
- Comprehensive package vignette and function documentation
- Unit testing for main functions

## Minor Improvements

- Better error handling
- Stripped down dependencies


# OpenSpecy 0.7.0

- Transferred code base from openspecy.org to this R package
