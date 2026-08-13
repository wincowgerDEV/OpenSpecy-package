# Feature Plan: Particle Analysis App Refinement

**Feature dir**: `specs/010-particle-app-refinement`
**Date**: 2026-08-12
**Current tranche**: Fix eight concrete defects/polish items in the Advanced/FileSpecs particle-analysis pipeline shipped in `specs/009-large-hyperspectral-workflows` (status: implemented), plus one user-requested addition (R8: halo-based streaming `spectral_smooth` for FileSpecs). This tranche does not reopen 009's checklist; it is a follow-up bug-fix/polish pass on the same surface.
**Status**: Implemented and verified (focused + full `devtools::test()`: 2292 passed, 0 failed; live Playwright smoke against `test_1um.h5` and the bundled ordinary map). Remaining: maintainer `R CMD check`/release gates and any post-push CI.
**Change class**: Mixed, highest is package/scientific (return-contract change to `automate_particle_analysis()`/`automate_particle_filespecs()`) plus bundled-app behavior.

## Goal

- Make the Advanced particle pipeline behave correctly and consistently for large local sources (verify against `C:\Users\winco\OneDrive\Documents\EWG\test_1um.h5`) and ordinary uploads.
- Make particle-result plots, popups, downloads, and theming match the rest of the app and the pre-existing ordinary-map experience.

## Scope

- **In**: Advanced master-switch gating parity; default-requested particle plots actually rendering and matching local `automate_particle_analysis()` output; small inline click popover instead of full modal; default download-type selection; complete/matching Thresholded Particles zip contents; removing "Raw Map object" from downloadable content; Plotly-based, on-theme particle plots fed by queryable plot data instead of stored `recordedplot`s; removing the redundant "No regions passing threshold" alert; halo-padded streaming `spectral_smooth` support for `automate_particle_analysis.FileSpecs()`.
- **Out**: Changing S/N or matching algorithms, redesigning the Advanced tab layout, hosted/Shinylive-specific work (no wasm/package-repo changes expected; revisit only if plotly reintroduction changes the app dependency closure). FileSpecs must never materialize a full region (often the whole file) in memory, including for smoothing.
- **Users**: App users running large local H5/ENVI particle analyses and ordinary uploaded-map particle analyses; package users calling `automate_particle_analysis()` directly for custom plotting.

## Requirements

- R1. Turning Advanced off must not `shinyjs::disable` any input (its own child controls or any other tab's). It must only gate server-side computation via `isTRUE(input$active_advanced)`/`req()`, matching how `active_identification` already behaves, so users can edit Advanced/particle settings while off without triggering analysis. Remove `set_advanced_child_state`/`advanced_child_ids` disabling; keep existing `req()`/`isTRUE()` guards.
- R2. `Particle Image` and `Correlation` (cor_heatmap) must render whenever they are in the requested `outputs` (default) and matches exist, for both `automate_particle_analysis()` and `automate_particle_filespecs()`. Confirmed root cause via reproduction against `test_1um.h5`: (a) raw streamed S/N is numerically identical (max abs diff 0) between `automate_particle_analysis.FileSpecs()` and the eager `read_h5()`/`sig_noise()` path, so the join/plot mechanics — not scientific parity — are at fault; (b) when zero particles survive the area/threshold filter, `.empty_particle_result()` correctly omits `particle_image`/`cor_heatmap`/`cor_histogram` (they have no data), but `plot.OpenSpecyParticleAnalysis()` then reports the generic "not requested" `stop()` even though the output WAS requested — this is a messaging bug, not a join bug; (c) `particle_image` additionally requires `material_col` to survive `.append_particle_matches()`, which silently no-ops when the library metadata lacks the configured `material_col`/`library_id_col`. Fix: give `plot()`/the app a clear "no particles passed filtering" or "library has no `<material_col>` column" state instead of "not requested", and keep the join as-is (it is already correct).
- R3. The heatmap click popover must show only `x`, `y`, and the currently displayed z-value/label (plus its axis title), rendered as a small element docked inside the plot viewer (e.g., an overlay panel or side card next to the plot), not a page-covering `modalDialog`. Applies to both the FileSpecs raw-preview click path and the particle/ordinary-map click path in `server.R` (`particle_metadata_modal`, `observeEvent(input$heatmap_click, ...)`).
- R4. When a large/FileSpecs source is loaded and Collapse Particles is enabled, `download_selection` must default/auto-switch to `"Thresholded Particles"` instead of the first alphabetical/default choice.
- R5. The Thresholded Particles zip must contain exactly the files matching every checked box in `particle_outputs_selected`, including `details`, `summary`, `processed`, `particle_image`, `particle_heatmap`, `particle_heatmap_thresholded`, `cor_heatmap`, `sn_histogram`, `cor_histogram`, and `time`. Remove `"Raw Map object" = "raw"` from `app_particle_output_choices()` (and stop passing `"raw"` into `outputs`); fix whatever currently causes only 3 of the selected outputs to be written (likely `outputs` not reaching `automate_particle_analysis()`/`automate_particle_filespecs()` unfiltered, or `app_particle_output_files()` patterns not matching actual filenames).
- R6. Change `automate_particle_analysis()`/`automate_particle_filespecs()` to return queryable plot **data** per sample (grid `x`/`y`/`z` + labels for heatmaps, image-plot inputs, histogram values + threshold lines) instead of `recordedplot` objects; keep `plot.OpenSpecyParticleAnalysis()` working via base graphics from that data for script users. The Shiny app renders these via Plotly (`app_style_plotly()`), matching the pre-FileSpecs `heatmapA`/`MyPlotC` theme, for `particle_image`, `particle_heatmap`, `particle_heatmap_thresholded`, `cor_heatmap`, `sn_histogram`, `cor_histogram`. Keep the FileSpecs raw-preview (`app_draw_filespec_preview`) on base graphics — it intentionally avoids serializing full large-map matrices to the browser.
- R7. Remove the `show_alert("No regions passing threshold", ...)` block (`server.R` ~1637-1654); the existing quality warning/success indicator (`quality_counts()`/`quality_findings()`) already communicates zero-passing-region states.
- R8 (user-requested addition). `automate_particle_analysis.FileSpecs(..., spectral_smooth = TRUE)` must work instead of erroring. FileSpecs must never materialize a full region in memory to do it — regions are frequently the entire source file. Implement halo-padded streaming: for a target column chunk, read all rows for `[chunk_cols - halo, chunk_cols + halo]` (halo sized to `mmand::gaussianSmooth()`'s own kernel radius for `sigma1`) plus the full wavenumber axis, run the same `mmand::gaussianSmooth()` call the eager reader uses, then trim back to the requested columns before computing S/N or feature means. Applies to both `.filespec_particle_snr()` and `.filespec_mean_features()`.

## Technical Decisions

- **Approach**: Fix R1-R5 and R7 as targeted app/server bugs first (independently testable); R6 is the one contract change and should land last since R2/R3's plot rendering will already be exercised against it.
- **Public API**: `automate_particle_analysis()`/`automate_particle_filespecs()` return shape changes (`*_png`/`*_jpg` recordedplot fields become structured data fields). This is a breaking return-contract change — apply `openspecy-design-public-api` before touching `R/automate_particle_analysis.R`. Keep `plot()` as the stable script-facing entry point so casual callers are unaffected; document the new field names in `NEWS.md` as a breaking change.
- **Dependencies**: `plotly` is already an Imports/Suggests dependency (used by `MyPlotC`); no new dependency for R6.
- **OpenSpecy contract**: No change to `wavenumber`/`spectra`/`metadata`; only the auxiliary plot-data fields on `OpenSpecyParticleAnalysis` sample entries change shape.
- **Generated artifacts**: Regenerate `man/automate_particle_analysis.Rd`, `man/plot.OpenSpecyParticleAnalysis.Rd`, `NAMESPACE` via roxygen2 8.0.0 after signature/return-doc changes; inspect diffs.
- **Bundled Shiny app**: Canonical reactive stays `particle_analysis()`/`particle_sample()`. Owner gating fixed per R1. Click popover (R3) and default download selection (R4) change reactive/UI wiring in `server.R`; theme/rendering changes (R6) touch `output$heatmapA`, `output$particle_plot`/`material_plot` stay ggplot/unchanged. Verify no-upload, FileSpecs-loaded, ordinary-map, and particle-pipeline-enabled states.
- **Hosted Shinylive/WebAssembly app**: N/A expected (plotly already in the app's dependency closure); confirm no closure delta if plotly usage expands to more outputs.
- **R8 halo smoothing**: Halo radius replicates `mmand::gaussianKernel()`'s own size formula (`ceiling(6*sigma)`, forced odd) so the padded-and-trimmed block is numerically identical to smoothing the full region at once, without ever reading more than one halo-padded column slab. Requires a complete rectangular row/col grid per region (already assumed elsewhere in the FileSpecs particle path); an incomplete/irregular region raises a clear error rather than silently falling back to a full-region read, since a region may be the entire source file. `mmand` is already an Imports dependency.

## Package Surfaces

- `R/automate_particle_analysis.R`, `R/automate_particle_filespecs.R`: plot-data return contract (R6), clearer empty-plot messaging (R2), halo-padded `spectral_smooth` streaming (R8).
- `tests/testthat/test-automate_particle_analysis.R`, `test-FileSpecs-particle.R`, `test-particle_image.R`: cover new plot-data fields, join fix, removed `"raw"` output default.
- `benchmarks/file_specs_particle_analysis.R`: update for new return shape (not a same-output change, but the benchmark asserts on result structure).
- `inst/shiny/{global.R,server.R,ui.R,www/}`: R1, R3-R6 app wiring; `app_particle_output_choices()`, `app_particle_output_files()`, `app_write_particle_archive()`.
- Roxygen/`NEWS.md`: document breaking return-shape change.

## Work Checklist

- [x] Remove Advanced child-input disabling (`server.R` `set_advanced_child_state`/`advanced_child_ids`); confirm all downstream Advanced/particle reactives already `req()`/`isTRUE()`-gate on `active_advanced`. Verified live: MinSNR stays editable/unchanged in DOM regardless of Advanced state.
- [x] Root-caused: the join was already correct; `.empty_particle_result()` correctly omits post-match plots when nothing passes filtering, but `plot()` reported the generic "not requested" instead of explaining why. Fixed via the plot-data `type = "empty"`/`reason` contract below.
- [x] Replaced `particle_metadata_modal()`/`showModal` heatmap-click handling with `heatmap_popover_info` + `output$heatmap_popover`, a small in-viewer popover (x/y/z only). Applies to the FileSpecs-preview, ordinary-map, and Plotly particle-click paths. Verified live for both an ordinary map (x/y/Match Name) and FileSpecs (x/y).
- [x] `download_ui` + a dedicated `observeEvent(particle_analysis(), ...)` now force `download_selection` to `"Thresholded Particles"` once a particle result exists (the render-time "preserve current selection" logic alone never overrode the initial default). Verified live.
- [x] Removed `"Raw Map object"` from `app_particle_output_choices()`; fixed the empty-collapse path so `details`/`summary`/`time` are still written (previously skipped entirely). Verified live: downloaded zip contains all 12 expected files instead of 3.
- [x] Redesigned `automate_particle_analysis()`/`automate_particle_filespecs()` plot outputs as structured data (`.particle_*_data()`/`.draw_particle_plot_data()` families); `plot.OpenSpecyParticleAnalysis()` now distinguishes not-requested (stop) from requested-but-empty (informative blank plot). Docs regenerated.
- [x] Added `output$heatmapB` (Plotly, `app_particle_plotly()` in `global.R`) for particle results; `heatmapA` keeps ordinary maps and the FileSpecs raw preview on base graphics. Verified live: on-theme continuous/categorical/binary heatmaps and histograms.
- [x] Removed the "No regions passing threshold" `show_alert` block.
- [x] Implemented halo-padded `spectral_smooth` streaming (`.filespec_smoothed_values()`, `.gaussian_kernel_half_width()`) for both H5 and ENVI backends; verified bit-identical to eager `mmand::gaussianSmooth()` on `test_1um.h5` across chunk seams, and via a new ENVI-fixture test.
- [x] Ran focused + full package tests (2292 passed, 0 failed) and a genuine local Playwright smoke against `test_1um.h5` (large FileSpecs + real medoid library) and the bundled ordinary map, inspecting console and screenshots for each changed state.

## Verification

- **Focused**: `devtools::test(filter = "automate_particle_analysis|FileSpecs-particle|particle_image")`.
- **Scientific**: Compare S/N heatmap values/coloring and particle counts from the app run against a local `automate_particle_analysis()` call with identical arguments on `test_1um.h5` (raw S/N parity already confirmed bit-identical). For R8, compare smoothed per-pixel S/N from `automate_particle_analysis.FileSpecs(..., spectral_smooth = TRUE)` against eager `read_h5(spectral_smooth = TRUE)` + `sig_noise()` on `test_1um.h5`, including chunk-seam and region-edge pixels.
- **App/manual**: Local `run_app()` smoke: toggle Advanced on/off while editing its fields (no disabling, no spurious recompute); open `test_1um.h5`, confirm Particle Image/Correlation/Signal-Noise render; click heatmap, confirm small x/y/z popover; confirm default download is Thresholded Particles; download with all boxes checked and unzip to confirm every selected file is present; confirm no "No regions passing threshold" popup when quality indicators already show zero passing.
- **Broad**: `devtools::document()` (roxygen2 8.0.0), inspect diff; full `devtools::test()`; `R CMD check` before calling this tranche release-ready.
- **Reusable evidence**: None carried over — this tranche touches the exact files 009 last verified.

## Risks And Open Questions

- Rendering Plotly per-sample particle grids must stay responsive for `test_1um.h5`-scale regions; if a region's pixel count makes Plotly sluggish, downsample the served grid (not the underlying data) rather than reverting to base graphics.
- R8's halo padding roughly doubles-to-triples per-chunk disk reads near chunk boundaries (halo overlap read twice by neighboring chunks); acceptable for correctness but worth noting if streaming throughput regresses materially on very wide regions.
