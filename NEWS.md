# OpenSpecy 1.7.1

- Added a single **Run** button as the sole trigger for the app's analysis
  tranche, replacing the four per-tab owner switches; the button turns bright
  green whenever a new dataset is uploaded or a setting changes, and returns
  to the app's normal accent color once Run has produced current results.
  Uploading a new dataset now also resets the heatmap, spectrum plot, and
  quality/automatic-correction reports back to a "click Run" state instead of
  continuing to show the previous dataset's results. "Collapse Particle
  Spectra" and "Spatial Smooth" are silently ignored for a single uploaded
  spectrum instead of erroring. Fixed a crash ("wasn't able to determine
  range of domain") when a heatmap's selected color metric has no finite
  values for any pixel (for example, when no uploaded spectrum clears the
  correlation threshold).
- Replaced the **Preserve Uploaded Wavenumbers** advanced switch with a
  **Mean Up** conformation technique (the new default). Mean Up only
  resamples the uploaded spectra to the selected Wavenumber Resolution when
  that resolution is finer than what was actually uploaded; otherwise it
  leaves the uploaded axis untouched and conforms the reference library onto
  it instead, exactly as the removed switch did.
- Rebuilt the bundled app around one in-memory `OpenSpecy` workflow with a
  unified 10 GiB upload ceiling and best-effort resident/peak-memory guidance.
  Identification now ranks bounded query blocks and retains only a shared Top N
  result (10 by default) for the match table and download. Particle analysis
  calculates signal/noise after optional spatial smoothing but before other
  processing. Spectral cluster modes now fit source-scoped PCA/K-means first,
  identify collapsed clusters once, and either retain them as non-spatial
  particles or project their identities into a second connected same-material
  spatial collapse without re-identification. Correlation thresholds reuse that
  first pass. All heatmaps black out rejected pixels, omit inline legends, and
  expose a formatted legend modal (or a >30-category explanation); rejected
  clicks return no match and a flat processed trace. Threshold histograms remain
  on-theme, and only caught errors open alert dialogs.
  The default-on uploaded-axis option conforms the reference library onto the
  exact uploaded axis with memory-bounded `mean_up` averaging/interpolation,
  and particle ZIPs restore the summary table, both
  histograms, every heatmap, material summary, and size distribution.
- Corrected package `automate_particle_analysis()` partitioning so connected
  units and source-scoped PCA/K-means clusters never cross
  source maps or H5 regions; return stable pixel-to-unit membership and aligned
  unit IDs/metadata; and apply the minimum pixel area inclusively. Connected
  units retain recomputed shape and signal summaries, while `specs_centers`
  remains the public K policy and non-default `specs_steps` now fails clearly
  instead of being silently ignored.
- Added experimental, package-only `FileSpecs` descriptors for read-only H5 and
  ENVI maps. They fingerprint immutable sources, keep derived generations in a
  separate atomic cache, provide bounded `decompress_spec()` selections and
  lightweight region views, stream complete rectangular views to new atomic
  float64 ENVI pairs without wavelength-axis truncation, and fail early for
  unsupported matrix-only
  operations while preserving legacy matrix-backed `Specs` behavior. The first
  direct large-map workflow streams region-wise S/N and exact particle means
  through `automate_particle_analysis()`, retains one exact best match, and
  lazily caches registered regional H5 mosaics
  for particle images; it intentionally requires the
  collapse strategy, `mean`, and non-entropy S/N. `spectral_smooth = TRUE` now
  streams a halo-padded 3-D Gaussian smooth (matching `mmand::gaussianSmooth()`
  exactly) instead of erroring, without ever materializing a full region.
  H5 mosaics retain region, local and stage coordinates, unique pixels, and
  intersecting image tiles. These APIs remain available to package users but
  are no longer routed through the app.
- `automate_particle_analysis()`/`automate_particle_filespecs()` now return
  queryable plot **data** (`particle_image`, `particle_heatmap`,
  `particle_heatmap_thresholded`, `cor_heatmap`, `sn_histogram`,
  `cor_histogram`; each a list with grid/histogram values and a `type`, or
  `type = "empty"` with a `reason` when nothing passed filtering) instead of
  stored `recordedplot` objects; this is a breaking change to the field names
  and shape of `automate_particle_analysis()`'s per-sample result. `plot()`
  still draws any of these with base graphics, and the app renders them with
  Plotly for on-theme, interactive maps. Advanced no longer disables its own
  controls while off, matching the other top-level switches. The Thresholded
  Particles download drops the duplicative Raw Map object choice, defaults to
  itself once a particle result exists, and now zips every selected content
  type including an explanatory details/summary when no particles passed
  filtering. The redundant "No regions passing threshold" popup is removed in
  favor of the existing quality warning/success indicators.
- Unified the app's numeric, categorical, and particle heatmaps into one Plotly
  renderer with hover tooltips, an on-demand modal legend, and a selection marker kept
  in sync via a cheap trace restyle; this replaces the separate base-graphics
  heatmap, its click/brush handlers, and the metadata popover. Material-class
  colors are resolved from one shared palette across the heatmap, particle
  summary, and `particle_image()`. The Advanced switch and its correlation
  threshold default on. The
  Uploaded Metadata tab moves x/y/z and other per-pixel columns to the front
  for every source, and for sources over 100,000 spectra shows only those
  columns, dropping duplicated file-level metadata. `automate_particle_analysis()`
  now accepts a character vector of file paths, reading and processing each
  one in turn. Base-graphics particle-plot legends (`plot()`, `particle_image()`)
  now draw in the margin outside the plotted data instead of overlapping it.
  The `as_OpenSpecy()` data.table-to-matrix conversion notice is silent when
  called internally.
- Added `correct_spike()` with a conservative wavenumber-aware residual method
  and the manual and automated prominence/FWHM methods described by Coca-Lopez
  (2024). Corrections are transactional, preserve the `OpenSpecy` axis and
  metadata alignment, avoid boundary extrapolation, and retain auditable
  accepted/rejected-region diagnostics. Safe correction now repeats while the
  correctable count decreases, retaining successful passes when later
  candidates are newly exposed and leaving no-progress candidates unchanged
  with their safeguard reason.
- Added opt-in spike and saturation checks plus `report = "all"` status output
  to `assess_spec()`, exact sorted-amplitude `breakpoint_snr` support to
  `sig_noise()`, and optional spike correction at the start of `process_spec()`.
  `restrict_range()` can now remove one guarded union of hard saturation
  intervals from a whole batch, with irregular-axis coverage accounting and a
  conservative rollback when the proposed loss exceeds 70% or leaves too few
  points.
- Added a default-on app control for isolated spikes and an opt-in saturation
  control, separated
  automatic-correction details from warning/success results for the active
  spectrum, an external adaptive spectrum legend, and bright
  colorblind-accessible heatmap palettes. Numeric map legends sit horizontally
  above the plot, default Match Name maps no longer flash a numeric metric,
  categorical Match Name colors are shared with the material summary, and map
  selection updates its marker without rebuilding the heatmap. Hosted
  WebAssembly downloads now use a same-frame validated Blob handoff while
  local Shiny retains its native download handler; browser smoke tests require
  genuine CSV and ZIP files from real clicks.
- Added a dark, accessible static landing page at the hosted-site root with the
  embedded app, navigation guidance, search and social metadata, tutorial,
  publications, contacts, and funding context. Conventional README-driven
  pkgdown documentation now lives at `/pkgdown/`; the app remains at `/app/`.
  Pew-Gerstner Fellowship in Ocean Plastics Research and Walking Softer are
  credited as Thriving monetary partners.
- Added a reusable workflow for compressing hyperspectral images with PCA and
  K-means (`k = 100`) and plotting pixel cluster groups with `heatmap_spec()`.
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
  ratios, `point_intensity()` for non-ratio point measurements, and 4S Fill
  Peaks baseline correction. The app's Quantification tab now defaults off and
  lets users save ratios, individual band areas, and individual point
  intensities from precise numeric inputs. Custom Ratios and Single
  Measurements now share the single Quantification owner without a redundant
  child switch. The app calculates any combination from
  the exact final processed spectra displayed in the app and includes exact
  definitions, values, and processed-spectrum provenance in Processed Spectra
  and Top Matches downloads.
- Made the representative medoid library the interactive app default and cache
  reference-library preparation by the final processed axis. The complete
  library remains an explicit local-app option for users who accept its longer
  initial calculation.
- Reimplemented 4S Fill Peaks smoothing and suppression in base R, removing the
  compiled `baseline` runtime dependency so the same correction works in local
  R and the hosted WebAssembly app.
- Made the contextual download action fill its card, changed uploaded spectrum traces to
  white, standardized enabled switches to green and white, validated all
  informational disclosures, and restored the historical donation choices in
  an on-demand right-side header dialog. Removed the inactive help and dark-mode
  header toggles, aligned the full-width Spectra and Summary cards, and kept
  disabled child settings inert until their owning analysis switch is enabled.
  Automatic tail mode now visibly disables its manual bounds and explains that
  assessment uses the full processed axis. Processing disclosures now explain
  each spike and saturation input, success findings omit empty interpretation
  and action fields, and automatic details report the ranges actually corrected
  by spike, saturation, CO2, and high-tail operations.
- Streamlined the app to one analysis workspace with Preprocessing,
  Identification, and Advanced tabs; moved independent thresholds and map
  controls to Advanced, removed Google Translate and the informational sidebar,
  and moved community, partner, and contract information to the hosted landing
  source.
- Embedded the hosted Shinylive app on the static landing page with real Shiny
  readiness feedback and a viewport app mode that persists through
  upload/download dialogs. GitHub retains a normal README, and brief reactive
  updates no longer flash the app's processing overlay. Relative `app/` and
  `pkgdown/` routes keep GitHub project and hosting-fork deployments portable.
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
  the static landing page, conventional pkgdown docs, and the self-contained
  Shinylive app. The complete wasm
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
