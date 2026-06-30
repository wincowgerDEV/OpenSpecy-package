# Feature Plan: Automated Particle Analysis And H5 Images

**Feature dir**: `specs/004-automated-particle-analysis`
**Date**: 2026-06-30
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Generalize the external `analyze_features()` workflow into package-native, composable particle analysis, image-overlay, crowding, recovery, MDA, and BDL functions.
- Preserve `OpenSpecy`/`Specs` spectral alignment while adding visual-image metadata and faster map readers without new package dependencies.

## Scope

- **In**: `automate_particle_analysis()`, `particle_image()`, red-box image-origin detection, visual-image attachment/access by `def_features()` and plotting helpers, `crowd_lookup()`, recovery/MDA/BDL helpers, faster `as_OpenSpecy()`, `read_envi()`, and raw-region `read_h5()`.
- **Out**: `adj_map_baseline`, `k`/`k_weighting`, particle-cell voting, `median_spec_plot`, `median_spec`, new `sgolay` use, max-derivative wavenumber indexing in readers, ggplot2/ggrepel/dplyr/tidyr/magick dependencies, Shiny app code, and direct edits to generated files.
- **Users**: Analysts processing batches of hyperspectral particle maps who need reproducible particle tables, summaries, image overlays, and method validation from spike/blank samples.

## Source Notes

- External script: `H:\My Drive\Work\Projects\OpenSpecy\OpenSpecyDev\automated_steel_pipeline_Validation.R`.
- H5 probe file: `C:\Users\winco\OneDrive\Documents\RamanMap\tinytest.h5`; observed `/Regions/Region1/Dataset` dims `1738x188x109`, UTF-16-like XML in `/FileInfo/MetaData`, stage-position data, and BMP-like mosaic bytes in `/Mosaic/Image*`.

## Requirements

- R1. `automate_particle_analysis()` accepts file paths or `OpenSpecy`/`Specs` objects plus a user-supplied library and returns a structured list of per-sample raw maps, processed particles, details, summaries, optional plots, and timings; file writes occur only when `output_dir` is supplied.
- R2. Supported particle strategies are `collapse`, `partial_collapse`, `nonspatial_collapse`, `all_cell_id`, and `raw`; removed modes/arguments error with migration guidance instead of silently doing old behavior.
- R3. `collapse` keeps the current `def_features()` -> `collapse_spec()` workflow, thresholds by SNR/correlation/area, and preserves feature metadata, spectra columns, and object attributes.
- R4. `partial_collapse` and `nonspatial_collapse` use `Specs` compression state (`as_Specs()`, `collapse_spec.Specs()`, and coords/value metadata) instead of script-only `wave_id`.
- R5. Visual images can be attached to `OpenSpecy` and `Specs` through object attributes containing image data/source, image extent, map-to-image transform, and detection method; constructors/checkers preserve the canonical top-level object names unless this contract is explicitly expanded.
- R6. `detect_image_origin()` identifies Thermo Fisher iN10 red bounding boxes from JPEG/BMP-compatible arrays or paths and returns `bottom_left`, `top_right`, and diagnostics usable by `def_features()` and overlays.
- R7. `def_features()` keeps existing `img`, `bottom_left`, and `top_right` compatibility but can infer image and origin from attached visual-image attributes.
- R8. `particle_image()` uses base R graphics and the script's material color defaults; it plots particle classes by map coordinates and can overlay onto attached or supplied visual images with transparency.
- R9. Hyperspectral overlays support a user-supplied metadata vector/column, material colors or continuous colors, and `alpha`, without adding image-processing dependencies.
- R10. `crowd_lookup()` summarizes potential particle crowding from user-specified sample, area, size, and optional material columns; material classes of interest are never hard-coded.
- R11. Recovery helpers calculate recovery rates from spike/expected and observed counts using user-specified material/size/sample columns; no plastic/mineral/unknown assumptions are built in.
- R12. `minimum_detectable_amount()` calculates MDA from blank samples by user-specified grouping; `batch_detection_limit()` implements the single-blank BDL case and is called by MDA only when one blank is present.
- R13. `read_h5(collapse = FALSE)` is the default and returns rawest per-region/pixel spectra with region, subpixel, row/col/x/y, stage position, source file, parsed H5 metadata, and visual mosaic attributes when present.
- R14. `read_envi()` and `read_h5()` build spectra matrices and metadata in bulk rather than column-by-column loops; output must remain equivalent except for intentional `read_h5()` default and metadata improvements.

## Technical Decisions

- **Approach**: Split the monolithic script into composable functions; keep batch orchestration thin and route processing through existing `read_any()`, `sig_noise()`, `process_spec()`, `match_spec()`, `def_features()`, `collapse_spec()`, and `Specs` primitives.
- **Public API**: Export only reusable user workflows: `automate_particle_analysis()`, `particle_image()`, `detect_image_origin()`, `add_visual_image()`/`visual_image()`, `crowd_lookup()`, `recovery_rate()`, `minimum_detectable_amount()`, and `batch_detection_limit()`; keep one-caller parsing helpers internal.
- **Dependencies**: No new dependencies; use existing `data.table`, `jpeg`, `hdf5r`, `mmand`, `matrixStats`, `plotly`, and base graphics.
- **OpenSpecy contract**: Keep `wavenumber`, `spectra`, and `metadata` aligned; visual-image data lives in attributes copied by `as_OpenSpecy()`, `as_Specs()`, `decompress_spec()`, `filter_spec()`, `c_spec()` when compatible, and read/plot helpers.
- **Generated artifacts**: Update roxygen in `R/*.R`; regenerate `NAMESPACE`/`man/*.Rd` only with configured roxygen2 8.0.0 and inspect generated diffs immediately.
- **External resources**: `tinytest.h5` remains outside the package; benchmarks use an env var such as `OPENSPECY_H5_BENCH_FILE` and skip with a clear message when absent.
- **Reference workflow compatibility**: No reference-library rebuild; if library matching defaults change, run staged representative `OpenSpecy` match comparisons before release.

## Package Surfaces

- `R/`: Add `R/automate_particle_analysis.R`, `R/particle_image.R`, `R/validation_metrics.R`, and visual-image helpers; update `R/as_OpenSpecy.R`, `R/def_features.R`, `R/Specs.R`, `R/Specs_methods.R`, `R/read_ext.R`, `R/read_envi.R`, `R/read_multi.R`, and attribute-preserving helpers as needed.
- `tests/testthat/`: Add focused tests for API errors, image-origin detection, visual-image attribute preservation, base plotting side effects, crowd/recovery/MDA/BDL formulas, H5 raw-region behavior from a tiny generated H5 fixture, and removed argument errors.
- `benchmarks/`: Add legacy/current comparisons for `as_OpenSpecy()`, `read_envi()`, and `read_h5()` using repeated timings, output equivalence, and `OPENSPECY_H5_BENCH_FILE` for the external H5 probe.
- `workflows/`: Unchanged except optional example notes; do not move the external analysis script into the package.
- `vignettes/README/pkgdown`: Update the SOP or advanced vignette with concise particle automation and image-overlay examples; no direct pkgdown HTML edits.
- `DESCRIPTION`: Unchanged unless implementation proves an existing imported package is missing from metadata.
- `NEWS.md`: Add entries for new particle-analysis helpers, image support, validation metrics, and the `read_h5()` default change.
- External Shiny compatibility: Consider map/image metadata and `read_h5(collapse = FALSE)` downstream impact; no Shiny app code in this repository.

## Work Checklist

- [ ] Design final public signatures and error messages in `R/automate_particle_analysis.R`, `R/particle_image.R`, and `R/validation_metrics.R`.
- [ ] Implement visual-image attributes, red-box origin detection, and base overlay plotting with preservation through `OpenSpecy`/`Specs` flows.
- [ ] Port the trimmed particle workflow with `Specs`-based partial/nonspatial collapse and no removed script modes.
- [ ] Rework `read_h5()` metadata/image parsing and default raw-region behavior; bulk-optimize `as_OpenSpecy()` and `read_envi()`.
- [ ] Add focused tests, roxygen, NEWS, and benchmark scripts before full quality gates.

## Verification

- Focused tests: `devtools::test(filter = "def_features|Specs|read_envi|read_ext|automate_particle_analysis|particle_image|validation_metrics")`.
- Toolchain/version preflight: confirm installed roxygen2 matches `Config/roxygen2/version: 8.0.0`.
- `devtools::document()`: required after roxygen/export changes; inspect generated `NAMESPACE` and `man/*.Rd` diffs immediately.
- Full `devtools::test()`: required after focused tests pass.
- `devtools::check()` or CI/R CMD check: required before release-facing merge.
- Benchmarks: run new reader/constructor benchmark scripts; fail on inequivalent same-output results or material slowdown.
- Reference-library/long workflow staging: N/A unless matching/library defaults are changed; H5 full-file benchmark is manual/env-var guarded.

## Risks And Open Questions

- Visual images as attributes preserve the current object contract; a top-level `image` list element would require an explicit contract change across checkers, converters, and serializers.
- Exact MDA/BDL constants and rounding should match the maintained method defaults, with arguments only where users have real alternative policies.
- H5 XML metadata parsing must be robust to Thermo format variation; implementation should start with small path/dimension probes before large-file reads.
- `read_h5(collapse = FALSE)` is a behavior change for current callers; document migration to explicit `collapse_spec()` or `as_Specs()` workflows.

## Approval Notes

- Approved by:
- Follow-up: Consider later exporting lower-level H5 metadata/image parsers only if they gain independent user value.
