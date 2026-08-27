# OpenSpecy 1.7.1

- `build_lib()` now provides a no-argument, end-to-end official workflow that
  returns libraries, medoids, models, and named assessment tables in one
  object. Completed components and full old/new assessment stages are exported
  with input manifests as they finish; `reuse = TRUE` resumes only compatible
  checkpoints and validated artifacts are promoted to a versioned release
  directory.
- Full reference assessment now uses a stable-identity-grouped ten-percent
  holdout across the complete candidate and legacy artifacts, prevents exact
  reference leakage, records model and reference identification metrics, and
  reports per-check shifts from `assess_spec()`. Model evaluation rows now live
  in one tidy `tests` table rather than scattered accuracy/confusion fields.
- Canonical source metadata is coalesced before external joins, and
  `fallback_by` is deprecated. Literal-only anchored class patterns moved from
  `classes_regex.csv` to exact entries in `classes_reference.csv`.
- Added auditable `prune_lib()` and recipe-selective `build_lib(prune = ...)`
  support for reference-library QA/QC. Generic classes are reassigned only to
  eligible same-technique candidates, then classes are processed largest first
  with bounded correlation blocks, deterministic ties, and protected minimum
  sizes. The official workflow prunes derivative and nobaseline libraries
  before medoid/model creation while leaving raw unpruned.
- Harmonized reviewed metadata aliases and made `build_lib()` lookup keys
  explicitly selectable, with optional fallback-key merging and fill-only
  lookup values. The official workflow coalesces username into a missing
  organization before one type join and verifies complete library/spectrum
  types. The curated reference tables now separate polyamides
  from polyacrylamides, classify adipate polymers as polyesters, correct PA,
  aramid, Nomex, duplicate, and common-name mappings, and cover reviewed
  organization plus exact user-source fallbacks.
- Added `predict_class_reference()` for reviewable class-table curation.
  Flexible patterns now live in a separate regex reference, run only after the
  exact lookup, and fill only blank materials when every match agrees. Exact
  overlaps are allowed and reported; distinct-material clashes stay blank.
- `build_lib()` now removes recognizable paths and every `read_any()`-supported
  trailing file extension from `spectrum_identity` before exact metadata
  lookup. Numeric OPUS suffixes include any terminal period followed only by
  digits, such as `.10`. It records an audit attribute and normalizes exact
  lookup keys the same way. The compressed exact class table no longer carries
  extension-only aliases, and the source table records all observed spectrum
  techniques, including MBARI as Raman.
- Accelerated reference-library Savitzky-Golay derivatives with compiled
  convolution and polynomial baseline subtraction with reusable QR fits. The
  retained benchmark compares the former implementations and enforces tight
  same-output tolerances.
- Added the opt-in compact map `Specs` 0.2 format for ENVI/H5/ZIP inputs.
  Regular coordinates and repeated metadata use validated descriptors, while
  optional S/N background suppression retains foreground values and maps every
  rejected source to an exact virtual zero spectrum with auditable reasons.
  Weighted PCA/K-means and foreground Hilbert transforms preserve full-source
  multiplicity without expanding compact pixels. The bundled app now stages
  one local direct path or one hosted WORKERFS mount, reads only after **Run**,
  and offers the compact transformed map as an RDS download.
- `automate_particle_analysis()` now treats both S/N threshold extremes as
  valid outcomes. Removing every map pixel emits a message and returns an empty
  analysis before library matching; retaining every pixel emits a message and
  continues, allowing connected collapse to identify and measure the full map
  as one particle per source.
- Reduced default `read_envi()` peak memory without changing its public API or
  returned `OpenSpecy` format. BIP, BIL, and BSQ files are now read in bounded
  blocks directly into the final band-by-pixel matrix instead of constructing
  and permuting multiple complete arrays; `spectral_smooth = TRUE` retains its
  existing three-dimensional smoothing path.
- Reduced `read_zip()` peak memory for two-member ENVI HDR+DAT archives by
  streaming the compressed binary directly into the same blockwise
  band-by-pixel reader. This avoids retaining a complete extracted DAT beside
  the final matrix in WebAssembly while preserving the returned `OpenSpecy`
  data and the existing extraction path for other ZIP layouts and smoothed ENVI
  reads.
- Fixed collapsed analysis settings requiring a separate maximize click: the
  Preprocessing, Identification, Advanced, and Quantification tabs now expand
  the card and activate the chosen tab with the same click. Run, Recalculate
  Preview, and download actions again schedule the central loading overlay
  directly from the browser click, before a blocking local or WebAssembly R
  task can delay server phase messages. The overlay now follows Shiny's real
  idle lifecycle instead of being dismissed after the first reactive flush,
  which could precede lazy identification and rendering work.
- Changed the default `assess_spec()` silent region to 2420--2550 cm^-1^ and
  the high-tail/CO2 detection and automatic-correction ratio from 3x to 2x.
  Explicit caller values remain unchanged.
- Removed speculative RAM forecasting from the Shiny app. Jobs now proceed
  until the real read, allocation, or processing operation succeeds or fails,
  with elapsed-phase recovery guidance while retaining the 10 GiB input limit.
- The hosted Shinylive app can mount browser-selected files into webR WORKERFS
  and pass their paths to the ordinary `read_any()` pipeline, avoiding the
  copying multipart/R-raw upload bridge while still fully materializing an
  in-memory `OpenSpecy` object. Shinylive now presents only that mounted-file
  picker, while local Shiny presents only its native upload. Hosted mount and
  read/materialization status appears in the central progress popup instead of
  explanatory/status text below the picker. Mounted text spectra are read
  through `fread()`'s text parser to avoid its unsupported 32-bit WORKERFS file
  memory map while retaining delimiter/type inference and output structure.
- WebAssembly repository builds now reuse a verified dependency-only CRAN-like
  cache locally and in GitHub Actions. Every reuse evicts and rebuilds
  `OpenSpecy`, refreshes changed dependency versions, regenerates the VFS image,
  and retains exact commit/artifact checks.
- Fixed active-spectrum quality findings for collapsed maps: retained units and
  rejected clicked pixels now use the same one-spectrum object as the plotted
  trace, and SNR is calculated directly from that object instead of indexing a
  dataset/heatmap vector. Rejected pixels are labeled and no longer assessed as
  synthetic zero spectra.
- Fixed Spatial Smooth running its (potentially expensive) convolution
  immediately on every toggle/Spatial Standard Deviation change, before Run
  was ever clicked. An always-on observer that keeps the heatmap's selection
  marker in sync with clicks was reading the spatially-smoothed object purely
  for pixel x/y coordinates, which smoothing never changes, and that
  incidentally forced the real computation to run live.
- Changed Remove Isolated Spikes, Flatten Region, and Range Selection to
  default off. Whether their toggles are on or off, the viewed spectrum's
  Warnings/Successes now always include a spike/CO2-region/high-tail/
  saturation check (previously these four were only ever reported as part
  of "Automatic Corrections Made," which stayed silent when the matching
  toggle was off, and a leftover filter separately hid them from Warnings/
  Successes even after being computed), so turning automatic correction off
  never hides whether the spectrum actually has the issue. Every one of
  these checks now also has its own specific success message (e.g. "No
  isolated single-point spikes were detected") instead of a generic "check
  passed" placeholder. A Low Signal/Noise check was considered but left out
  as redundant with the app's existing separate SNR Threshold finding.
- Filled the Warnings/Successes buttons with their semantic color (amber/
  green) instead of a thin border on a neutral background, so they read as
  clickable like the app's other buttons; Automatic Corrections Made keeps
  its rainbow identity as a permanent fill (previously only a border shown
  when something had actually been applied), with a glow ring added to
  still flag when a correction was actually applied.
- Fixed the Thresholded Particles download's Particle Unit and Match ID
  heatmap images always drawing a legend, even though both are per-particle
  identifiers with too many categories for a legend to be useful.
- Fixed a selection feedback loop that snapped a manual heatmap click on a
  multi-pixel collapsed particle back to that particle's first/representative
  pixel instead of staying on the pixel actually clicked: syncing the
  sidebar metadata table's selection to match a heatmap click echoed back
  through the table's own selection-change handler, which was
  indistinguishable from a genuine row click.
- Fixed the Top Matches table staying empty whenever Library type = AI model,
  even though the Top Matches download and the Selection Metadata table
  already showed AI predictions. AI mode has one prediction per spectrum
  rather than a ranked candidate list, so the table now shows that single
  prediction for the selected spectrum instead of erroring/staying blank.
- Clicking Run, Recalculate Preview, or a download now shows busy feedback
  immediately instead of after a multi-second delay (dominated, for Run, by
  an unannounced whole-map signal-to-noise scan that ran ahead of the first
  progress message; Recalculate Preview previously had no progress signal at
  all in its default configuration). Run and Recalculate Preview also get an
  instant client-side busy indicator on click, and downloads show the same
  indicator consistently in both the local Shiny app and the hosted
  Shinylive build.
- Fixed clicking a row in the Uploaded Metadata table jumping to an unrelated
  or unchanged map location instead of that particle's first (lowest raw
  pixel index) location: the handler treated the table row's particle/unit
  index as if it were a raw pixel index, and separately skipped updating the
  selection whenever the clicked unit happened to already equal the current
  selection's default -- most visibly on the very first row click, since the
  app's initial selection defaults to unit 1. It now resolves both the
  selected pixel and unit directly, unconditionally, matching the heatmap
  click handler.
- Changed the Summary panel's "Good Signal"/"Good Match Values"/"Good
  Identifications" bars to show the underlying pixel counts (e.g.
  "142 / 331,180") alongside the percentage: `shinyWidgets::progressBar()`
  rounds its displayed percentage to the nearest whole number, so a real but
  small share of passing pixels on a large, sparse map could read as a
  misleading "0%".
- Moved the Signal/Noise **Recalculate Preview** button out of the histogram
  card it previously shared with the plot: that card dims when the preview
  is stale, which was dimming the one control needed to un-dim it. The
  button now uses the same green ("would change the result")/dark navy
  ("already current") convention as the main Run button, and the histogram
  itself now visibly resets to blank (instead of freezing on the previous
  dataset's chart) when a new file is uploaded.
- Fixed a bug in `canonical_state_gate`'s Run-gated result where `return()`
  inside `tryCatch()` exits the enclosing reactive directly, silently
  skipping the settings snapshot the previous entry's fixes attached after
  the `tryCatch()` call -- on every code path except one (collapse with
  Threshold Correlation on and a successful result), `canonical_state()`'s
  settings were `NULL`, so the heatmap/plot/download fixes below were
  silently inert whenever Threshold Correlation was off. Settings are now
  attached at every actual return point instead. This also fixes the
  particle-size histogram never rendering when collapsed (its `req()` on
  the missing settings blocked it silently) and the Map Color selector/
  particle-summary gating for the same reason.
- Added a **Signal/Noise Basis** choice (Raw / Spatially Smoothed, the
  previous default; or Fully Processed, which also applies every other
  enabled preprocessing step to each pixel before scoring it) that decides
  which pixels are eligible for particle collapsing. The Signal/Noise
  histogram preview no longer recomputes live on every settings change
  (which could re-run spatial smoothing or, with Fully Processed, full
  preprocessing, before Run was ever clicked); it now only updates on Run
  or a new **Recalculate Preview** button in the Threshold Signal/Noise
  box, and dims when the basis, Spatial Smooth, or thresholding settings
  have changed since its last computation. The memory preflight advisory
  no longer runs a live spatial smooth either (uses the raw upload's
  dimensions only, which is all it ever needed).
- Fixed a `filter_spec()` "zero spectra" error when clicking a
  collapse-rejected/background pixel: the raw-spectrum overlay reactive had
  no fallback for an invalid selection (unlike the processed-spectrum
  reactive, which already flat-lines correctly); it now does the same.
- Fixed the Run-gated reactivity the previous entry introduced: the heatmap,
  particle/material plots, correlation and signal/noise histograms, download
  type list, and progress-bar summaries now read only the settings captured
  at the last Run instead of live checkboxes, so toggling Collapse Particle
  Spectra, Spatial Smooth, Threshold Signal/Noise, or Threshold Correlation
  no longer recomputes or re-renders anything before Run is clicked. Added an
  **on/off switch to the Identification Strategy** box (default on) that
  fully skips identification, and one **Turn All On/Off** button per settings
  tab that has switches. Processed spectra now flat-line below the enabled
  signal/noise threshold whether or not Collapse Particle Spectra is on
  (previously only when it was on). Fixed a race between the six
  Run-triggered result caches that could leave quantification, quality
  reports, and other Run-gated results silently stuck at their pre-Run value;
  results are now populated in an explicit, deterministic order. Fixed the
  Map Color selector defaulting to Signal/Noise and never updating once
  Material Class/Match ID/Match Value became available. The Run button's
  default (nothing-to-run) color is now the app's dark background color
  instead of light blue, and the Spectra card has visible space above it.
  Vectorized residual spike detection across every spectrum in a map/batch
  upload at once instead of one small allocation per spectrum per
  correction pass (same output; see `benchmarks/spike_correction.R`), and
  raised the identification blockwise match size from 100 to 1,000 query
  spectra per block (same output, less chunking overhead). The memory
  preflight estimate no longer runs the actual spatial smooth as a side
  effect of estimating memory. Fixed a duplicate `id="columns_selected"`
  between the Top Matches column-choice `uiOutput` wrapper and its inner
  `selectInput`.
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
