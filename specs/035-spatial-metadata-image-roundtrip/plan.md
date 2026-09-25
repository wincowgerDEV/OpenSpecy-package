# Feature Plan: Spatial Metadata And Image Round Trips

**Feature dir**: `specs/035-spatial-metadata-image-roundtrip`  
**Date**: 2026-09-25  
**Review budget**: Under 100 nonblank lines and approximately 1,500 words.  
**Current tranche**: Source-native map coordinates, restorable app settings, optional visual overlays, H5 mosaic-preserving splits, continuous legend ticks, and reliable replacement uploads.  
**Change class**: Mixed; highest class is hosted/release because shared package readers and bundled/hosted app upload and visualization behavior change.

## Goal

- Make supported H5 and ENVI maps use their validated source coordinate system and visual imagery throughout the app, while allowing a downloaded settings snapshot and a replacement dataset to be loaded cleanly in the same session.
- Preserve spectral values, source ordering, integer map topology, metadata alignment, visual registration, and bounded file-backed behavior.

## Scope

- **In**: H5 and common ENVI origin/step parsing; physical app coordinates; Advanced-tab settings CSV import; embedded-H5 and companion-JPG/PNG overlays with adjustable transparency; split-H5 mosaic retention; five-tick continuous legends; themed numeric metadata filters; repeat local selection.
- **Out**: arbitrary image registration without a red boundary, general GIS reprojection, automatic analysis after settings import, and hand edits to generated docs or hosted artifacts.
- **Users**: local and Shinylive map users, plus package users of `read_h5()`, `read_envi()`, `open_specs()`, `split_h5()`, and `automate_particle_analysis()`.

## Requirements

- R1. Dense, compact, and file-backed H5/ENVI reads derive one validated spatial calibration (`x_origin`, `y_origin`, `x_step`, `y_step`, unit, axis direction) from supported source metadata. Package-native integer `x/y` plus `row/col` remain topology-safe; the app projects display/export `x/y = origin + zero-based index * step` without mutating adjacency coordinates.
- R2. H5 prefers region stage metadata already read from `-StagePosXYZ`/file metadata. ENVI supports the common headers accepted by the package, including standard map/start fields and Thermo-style description/pixel-size fields; ambiguous, incomplete, or non-finite calibration warns and falls back to pixels without guessing units.
- R3. The app uses physical `x/y` and source units for heatmap axes, hover, selection metadata, collapsed-particle location/size exports, and image registration when calibration exists; manual Pixel edge length/unit remains the documented fallback and is never applied twice.
- R4. A separate **Load Settings** CSV control in the Advanced tab accepts the app's one-row **User Metadata** download, validates a schema version plus recognized columns, restores all supported controls and saved ratio/measurement definitions with correct types, leaves provenance fields read-only, resets omitted settings to app defaults, and invalidates the current result until Run. Invalid files change nothing and report actionable fields.
- R5. Restored owner toggles continue to gate child inputs; inactive child values may be restored but remain scientifically inert. Import does not replace spectral data or trigger Run.
- R6. Live **Visual Image Overlay** and **Overlay Transparency** controls appear only when a registered image is available. H5 uses its embedded registered mosaic; an ENVI DAT/IMG+HDR upload may include one basename-matched JPG/PNG, attached via `detect_image_origin()` and `add_visual_image()`. Ambiguous pairing or failed red-boundary detection warns and leaves analysis usable without overlay.
- R7. The image is the background and the classified particle heatmap uses the same colored, alpha-adjustable overlay style as `particle_image()`/`automate_particle_analysis()`; rejected masks and selection stay above it. Toggle/transparency changes are live and do not recompute science.
- R8. Native `split_h5(..., format = "h5")` copies only mosaic tiles intersecting retained regions, writes corresponding `Centers` rows with consistent image renumbering, and retains required mosaic metadata. Every output must yield a registered `visual_image()` and an `automate_particle_analysis()` visual overlay equivalent to its source region. RDS splits preserve the materialized attribute.
- R9. Every continuous app heatmap legend shows five ordered tick values spanning the finite range, including endpoints, formatted with `signif(..., 3)`; constant and all-nonfinite data have explicit stable labels. Categorical legends are unchanged.
- R10. In local mode, **Choose spectra...** opens on every click after prior selection/Run; a second accepted selection fully replaces the source and clears stale heatmap clicks, overlay, S/N preview, matches, quantification outputs, and errors while leaving the app ready to Run.
- R11. Numeric Uploaded Metadata filter bars use centralized theme colors for background, track, handles, labels, and contrast in normal, hover, focus, and disabled states; nonnumeric filters and table behavior remain unchanged.

## Technical Decisions

- **Geometry**: add one internal calibration model shared by `read_envi()`, `read_h5()`, and `open_specs()`. Store source calibration/provenance as an object attribute and retain any vendor stage columns, then route app plotting/export through one coordinate projection helper. No new public flag/export; input metadata presence triggers the behavior.
- **OpenSpecy contract**: preserve `wavenumber`, spectra columns, metadata rows, IDs, source order, and visual/calibration attributes across dense/`Specs`/`FileSpecs` conversion, filtering, splitting, collapse, processing, and RDS round trips. Tests cover negative axis steps and multi-region H5.
- **Settings contract**: add a versioned, allowlisted CSV parser/serializer; use normal Shiny `update*Input()` calls and dedicated parsers for multi-select and quantification definitions. Unknown future columns warn; missing required/version-incompatible schemas fail atomically.
- **Images**: reuse `visual_image()`, `detect_image_origin()`, `add_visual_image()`, and particle-image palettes. Add the lightweight `png` decoder dependency because the existing JPEG-only reader cannot safely decode requested PNG companions. App pairing is basename-first for one JPG/PNG. For path inputs, `automate_particle_analysis()` automatically discovers an unambiguous same-directory, same-basename JPG/PNG when no explicit image is supplied. Plotly receives only the active registered raster.
- **Generated artifacts**: update roxygen source and regenerate with configured roxygen2 8.0.0; inspect `NAMESPACE`/`man/*.Rd` immediately and do not hand-edit them.
- **Performance**: new behavior, so no old/new benchmark is required. Add allocation assertions/subset timing for image extraction and split fixtures; stop and isolate if a split/overlay probe exceeds 2x source-copy time or unexpectedly materializes spectra.
- **Bundled app**: `canonical_state()`/`pixel_projection()` remain the only final state feeding heatmap, metadata, summaries, and downloads. Owner/child gating is preserved. Affected states are no-upload, staged replacement, processed/identified/collapsed map, restored quantification definitions, muted children, overlay on/off, and settings/download round trip.
- **Pipeline diagram**: update Upload/source validation, H5/ENVI materialization, Advanced calibration, pixel projection/heatmap, metadata/download, and replacement-reset boxes in `.specify/memory/pipeline-diagram.html` in the same implementation.
- **Hosted app**: shared `R/` and `inst/shiny/` inputs change. Preserve root `site/`, relative `/app/`, `/pkgdown/`, hardcoded package/dependency pins, small-library staging, and generated-artifact boundaries. Run `-HostedAppStatic` and exact matching-artifact startup/upload/overlay/settings smoke; no clean wasm rebuild unless dependency, image, driver, pin, or release scope changes.

## Package Surfaces

- `R/{read_envi,read_ext,Specs_file,Specs_compact,visual_image,split_h5,automate_particle_analysis}.R`: calibration/image preservation and split mosaic subset copying; update roxygen where contracts change.
- `inst/shiny/{global,ui,server}.R` and `inst/shiny/www/parent-frame.js`: image/settings inputs, replacement lifecycle, coordinate projection, particle-style overlay/transparency, themed numeric filters, and legend ticks; no bundled static image added.
- `tests/testthat/{test-read_envi,test-read_h5,test-h5-registration,test-FileSpecs,test-visual_image,test-split_h5,test-automate_particle_analysis,test-app-in-memory-helpers,test-run_app}.R`: focused contract and regression coverage with genuine written files.
- `benchmarks/`: N/A - new behavior, not a same-output optimization. `DESCRIPTION`/`NAMESPACE`: add/regenerate the `png` import required by requested PNG companions. `NEWS.md`: add user-visible features/fixes.
- `site/vignettes/README/pkgdown`: update the app vignette and pipeline diagram; README/site routes and generated pkgdown HTML otherwise unchanged. `.github/workflows/` and `workflows/`: unchanged unless a hosted smoke fixture needs explicit JPG/settings inputs.

## Work Checklist

- [x] Implement and propagate spatial calibration across package readers/conversions and topology-safe app projection (`R/`, `inst/shiny/global.R`, `inst/shiny/server.R`).
- [x] Add Advanced-tab atomic settings import/export and reliable second-selection reset (`inst/shiny/{ui,server,global}.R`, `www/parent-frame.js`).
- [x] Add automatic JPG/PNG discovery, H5/ENVI particle-style overlay/transparency, and mosaic-preserving `split_h5()` behavior (`R/`, `inst/shiny/`).
- [x] Add five-value three-significant-figure legend ticks, themed numeric metadata filters, guidance, `NEWS.md`, app vignette, roxygen, and the pipeline diagram.
- [x] Add focused helper/server/package tests, genuine H5/ENVI/JPG/CSV round trips, and a local no-upload browser/layout journey.
- [ ] Run the matching-artifact browser journey after a clean commit produces the required action-built wasm artifact.
- [x] Run local proportional gates, audit package/app size, inspect generated diffs, reconcile checkboxes, stop owned processes, inspect `git status`, and remove task scratch.

## Verification

- Focused order: parse/static checks -> reader/calibration/split/image tests -> app helper and `testServer()` restoration/reset tests -> genuine `split_h5()` then `automate_particle_analysis()` overlay -> targeted browser journeys -> documentation/full tests -> hosted gates.
- Browser acceptance: load H5, Run, toggle overlay/change transparency, inspect particle-style colors and themed numeric filters, click mapped coordinates; replace locally with ENVI DAT+HDR+JPG/PNG and Run; round-trip User Metadata through the Advanced uploader and confirm restored controls/definitions and stale-result gating. Repeat desktop/mobile and inspect screenshots plus console/server errors.
- Run configured Windows R preflight, focused `testthat::test_file()` calls, `devtools::document()`, full `devtools::test()`, and the fast `-HostedAppStatic` gate. Defer `devtools::check()` to a release candidate unless implementation changes dependencies/package metadata or exposes a broader package failure.
- Exact-artifact preflight must preserve the 118-package pinned closure and exercise startup, ENVI multi-file assembly including JPG, settings CSV restore, H5 overlay, and one typed library match. Full clean-commit wasm rebuild is proportionally deferred unless its trigger changes.
- Reuse existing app/library-match evidence only while package pin, dependencies, staged libraries, and covered source contracts are unchanged. No production-scale stage is introduced; fixture probes target seconds and bounded image memory.

## Risks And Open Questions

- Real vendor ENVI coordinate spellings and H5 mosaic variants may exceed current fixtures; implementation must capture sanitized representative headers/schema before extending the allowlist, never infer an undocumented unit.
- H5 tiles can intersect multiple split regions; duplication across output files is acceptable for self-contained overlays, but irrelevant tiles and complete spectra must not be copied/materialized.
- If browser evidence shows the second-selection failure is a native-dialog platform defect rather than app state, retain the single-button contract and route that platform through the existing `shinyFiles` no-copy picker.

## Approval Notes

- Approved by: maintainer implementation request, 2026-09-25.
- Evidence: focused and full tests pass (4,035 assertions, two opt-in AWS skips); HostedAppStatic passes 361 checks; staged R CMD check has only the pre-existing marked-UTF-8 data NOTE; installed app source grows 20,555 bytes with no new static asset.
- Follow-up: the exact matching-artifact browser tier and clean wasm dependency rebuild require a clean committed SHA/action artifact and remain deferred. Push/pull remains unauthorized.
