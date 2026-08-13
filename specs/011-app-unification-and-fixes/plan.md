# Feature Plan: App Heatmap Unification, Upload Unification, Package Fixes

**Feature dir**: `specs/011-app-unification-and-fixes`
**Date**: 2026-08-13
**Current tranche**: Replace the base-graphics/Plotly split (`heatmapA`/`heatmapB`) shipped in `specs/010-particle-app-refinement` with one Plotly heatmap+histogram format for every map state; unify the two file-upload paths into one with automatic FileSpecs fallback for large local files; fix `automate_particle_analysis()`'s file-vector handling and legend placement; silence a diagnostic message; fix a Shiny map-color legend/artifact bug; fix the failing Shinylive GitHub Action. 010 is implemented/verified; this is a new, unrelated tranche per user follow-up review.
**Change class**: Mixed, highest is bundled-app behavior (map/upload architecture) plus package/scientific (`automate_particle_analysis` fixes) plus hosted/release (Shinylive Action fix).

## Goal

- One on-theme, interactive Plotly heatmap+histogram format for every map state (ordinary upload, FileSpecs, particle results), with click-to-select spectrum sync, a moving selection marker, and hover-only metadata (no click popover, no base-graphics variant).
- One upload control: local files over the browser cap route transparently through FileSpecs; output/behavior is otherwise identical to an in-memory upload.

## Scope

- **In**: Restore/generalize the pre-`e706165` Plotly heatmap mechanism (`heatmap_spec`-equivalent) via the already-existing, already-themed `app_particle_plotly()`/`heatmapB`, extended to cover ordinary maps and the FileSpecs preview; reconnect the orphaned `app_heatmap_legend_layout()`; restyle-based marker sync (`plotlyProxyInvoke("restyle", ...)`) fixing the "spectrum doesn't update when collapse is on" bug; hover tooltips replacing the click popover added in 010; consistent material-class colors (heatmap = Summary box = particle image) via one canonical palette resolver; single `fileInput` with local-only automatic FileSpecs routing above the existing 2 GiB cap, removing the separate "Local H5/ENVI source" panel; FileSpecs chunk reads sized to ~100 MB instead of per-spectrum streaming; Uploaded Metadata tab shows x/y/z-selectable columns first (prepended for small files, the only columns for >100k-spectra files); `automate_particle_analysis()` accepts a character vector of file paths; base-graphics particle-plot legends moved outside the plot area; default `active_advanced` to `TRUE`; silence the `as_OpenSpecy.OpenSpecy()` data.table-conversion message; fix the Match Name legend/"pink square" artifact; fix the failing Shinylive Action.
- **Out**: Redesigning preprocessing/identification/quantification logic; changing S/N or matching algorithms; wasm/hosted upload-size policy (stays capped; only the *local* app gets automatic FileSpecs fallback).
- **Users**: All app users (one consistent map experience regardless of file size/backend); script users calling `automate_particle_analysis()` with multiple files.

## Requirements

- R1. `output$heatmapA`/`heatmapB` collapse into one Plotly output covering ordinary maps, particle results, and the FileSpecs region preview. Base-graphics `app_draw_server_heatmap()`/`app_draw_filespec_preview()` and the `.openspecy-heatmap-popover` mechanism (reactiveVal, renderUI, CSS, click handlers built in 010) are removed.
- R2. Clicking the heatmap resolves the nearest point (coordinate match, falling back to `pointNumber`) and sets `data_click$plot`; a non-isolated `observeEvent(data_click$plot, ...)` moves a dedicated marker trace via `plotlyProxyInvoke("restyle", ...)` (no full redraw) and updates the spectrum view. This must work identically whether the current map is an ordinary upload, a FileSpecs region, or a collapsed particle result — selecting a particle's row index must resolve within the *particle* object, not `preprocessed$data`.
- R3. Hover (`hoverinfo = "text"`) shows x/y/z (+ file/row identity) on the point under the cursor; no click-triggered info panel.
- R4. Continuous and categorical legends/colorbars sit outside the plot area (reconnect `app_heatmap_legend_layout()`'s horizontal-above-plot colorbar; extend the same treatment to categorical tick-labeled colorbars). Root-caused "pink square"/legend-over-map artifacts to `app_draw_server_heatmap()`'s in-plot `graphics::legend("topright", ...)` and its always-on default-selection marker; both go away with R1.
- R5. One material-class color resolver (`.particle_material_palette()` from `R/particle_image.R` as the primary source, falling back to the existing cycling palette for unrecognized labels) used by the heatmap, the Summary material bar chart, and `particle_image()`'s static export — same material, same color, everywhere.
- R6. Remove the separate "Local H5/ENVI source" panel/inputs. The standard `fileInput` validates size; when running locally (not wasm) and a file exceeds the existing 2 GiB cap and is H5/ENVI, open it via `open_specs()` instead of rejecting it; non-H5/ENVI oversize files still get a clear rejection. Downstream analysis, downloads, and metadata must be indistinguishable in content between the two backends (FileSpecs may be slower).
- R7. `load_filespec_selection()` and related reads move from per-spectrum streaming to ~100 MB chunked reads.
- R8. Uploaded Metadata tab: for >100k-spectra sources show only x, y, and the metadata columns selectable as heatmap "Map Color" values; for smaller sources, prepend those same columns to the front of the existing full table. Both should be able to seed the heatmap and metadata table from one shared cache.
- R9. `active_advanced` defaults to `TRUE` (matching `cor_threshold_decision`'s existing default).
- R10. `automate_particle_analysis(x, ...)` accepts a character vector of file paths (currently errors: `.normalize_particle_samples()` wraps the whole vector as one sample instead of one-per-path) — **fixed** in `R/automate_particle_analysis.R`. Confirm `particle_details_all_csv`/`particle_summary_all_csv` (already unconditional concatenation across samples) satisfy the "concatenated outputs" ask; document if so rather than adding new API surface.
- R11. Base-graphics particle-plot legends (`.draw_particle_categorical_heatmap()` et al.) render outside the plot's data area (`par(xpd = NA)` + margin, not an in-plot corner).
- R12. `as_OpenSpecy.OpenSpecy()`'s forced data.table-conversion message is silent — **fixed** (`message_conversion = FALSE`).
- R13. Fix the failing "Build and deploy Shinylive app" GitHub Action; reproduce locally via `openspecy-verify-hosted-app` first.

## Technical Decisions

- **Approach**: `app_particle_plotly()` (already on-theme, already used by `heatmapB`) is the base to generalize, not a re-creation of the historical `heatmap_spec()` — add a marker trace + `select`/`text` hover support + reconnect `app_heatmap_legend_layout()`, then point every map state at it. Land in order: R1-R4 (single heatmap) before R6-R8 (upload/metadata unification), since the unified metadata cache in R8 is easiest to build once there is one heatmap consumer.
- **Public API**: `automate_particle_analysis()`'s accepted `x` shapes expand (character vector of paths); no other signature change. Apply `openspecy-design-public-api` only if this reveals a need for more surface than the `.normalize_particle_samples()` fix.
- **OpenSpecy contract**: Unchanged.
- **Bundled Shiny app**: Canonical reactive stays the current selection (ordinary `preprocessed$data`, FileSpecs `final_selection()`, or the active particle sample) feeding the heatmap, spectrum view, metadata, and downloads. Owner gating: `active_advanced` continues to only gate computation, never disable inputs (010). Verify no-upload, ordinary-map, FileSpecs (small/large), particle-pipeline, and >100k-row metadata states with real files and browser screenshots (`openspecy-develop-shiny-app`).
- **Hosted Shinylive/WebAssembly app**: R13 only; local mode's large-file FileSpecs auto-routing does not apply to wasm (its existing cap/guidance is unchanged). Verify via `openspecy-verify-hosted-app` after the Action fix.
- **Reference workflow**: Re-verify R2/R7 against `test_1um.h5` for scientific/behavioral parity (already-established S/N/particle equivalence from 010 is reusable evidence; only the read/click mechanics change here).

## Package Surfaces

- `R/automate_particle_analysis.R`: `.normalize_particle_samples()` fix (R10); legend-outside-plot for `.draw_particle_categorical_heatmap()`/`.draw_particle_heatmap()`/`.draw_particle_binary_heatmap()` (R11).
- `R/as_OpenSpecy.R`: silence conversion message (R12) — done.
- `tests/testthat/test-automate_particle_analysis.R`: character-vector-of-paths coverage; legend-position assertion if practical.
- `inst/shiny/{global.R,server.R,ui.R,www/}`: unified heatmap/histograms, marker restyle sync, hover, legend reconnect, material palette unification, single upload + auto-FileSpecs routing, chunked local reads, metadata tab restructuring, `active_advanced` default. Asset audit if any base-graphics-only helpers become dead code.
- `.github/workflows/deploy-shinylive.yml` or its inputs: R13, exact fix TBD after reproducing the failure.
- `NEWS.md`: entry covering the heatmap/upload unification and the two package fixes (R10, R12).

## Work Checklist

- [x] Generalize `app_particle_plotly()`: add a `select` marker trace, hover `text`, reconnect `app_heatmap_legend_layout()` for continuous and categorical colorbars (`inst/shiny/global.R`).
- [x] Replace `heatmapA`/`heatmapB`/`app_draw_server_heatmap`/`app_draw_filespec_preview` with one Plotly output; remove the 010 popover mechanism (`inst/shiny/server.R`, `ui.R`).
- [x] Add the `plotlyProxyInvoke("restyle", ...)` marker-sync observer on `data_click$plot`, resolving selection correctly for ordinary/FileSpecs/particle (collapsed) states.
- [x] Unify material-class color resolution (`app_category_palette()` consults `.particle_material_palette()` first) — `inst/shiny/global.R`, `R/particle_image.R`.
- [x] Remove the "Local H5/ENVI source" panel; extend `observeEvent(input$file, ...)` to auto-route local oversize H5/ENVI uploads through `open_specs()`.
- [x] Convert `load_filespec_selection()`/related reads to ~100 MB chunk sizing via a read-through block cache (`.filespec_chunk_size_for_bytes()`, `.filespec_read_block()` in `R/Specs_file.R`); repeat clicks in the same neighborhood reuse the cached block instead of re-reading.
- [x] Rebuild the Uploaded Metadata tab/cache for the x/y/z-first and >100k-row-reduced views (`app_uploaded_metadata_display()`/`app_metadata_variable_columns()` in `inst/shiny/global.R`).
- [x] Default `active_advanced` to `TRUE` (`ui.R`).
- [x] `.normalize_particle_samples()` character-vector fix (`R/automate_particle_analysis.R`).
- [x] Move base-graphics particle-plot legends outside the plot area (`.draw_particle_heatmap()`/`.draw_particle_categorical_heatmap()` in `R/automate_particle_analysis.R`; `particle_image()`'s legend in `R/particle_image.R`).
- [x] Silence `as_OpenSpecy.OpenSpecy()`'s conversion message (`R/as_OpenSpecy.R`).
- [x] Reproduce and fix the Shinylive Action failure (`download_ui`'s selection never displaced "User Metadata" once identification completed; fixed with a dedicated `observeEvent(max_cor_settled(), ...)` default-jump observer).
- [x] Add character-vector-of-paths test coverage for `automate_particle_analysis()` (`tests/testthat/test-automate_particle_analysis.R`); confirmed `particle_details_all_csv`/`particle_summary_all_csv` already satisfy the "concatenated outputs" ask (R10) — documented here rather than adding new API surface.
- [x] Fix `tests/testthat/test-run_app.R`/`test-run_app-filespec.R` structural assertions left referencing deleted functions/UI (`app_draw_server_heatmap`, `heatmapB`, `heatmap_brush`, the manual `filespec_path`/`filespec_open` inputs, `app_upload_limit_bytes()` as the transport cap).
- [x] Run focused + full package tests, then a genuine local smoke (`test_1um.h5`, an ordinary map) with console/screenshot review for every changed state.

### Regressions found and fixed during implementation

- **Stuck busy overlay**: the Shinylive-Action fix (`observeEvent(max_cor(), ...)`) subscribed directly to `max_cor()`, which re-invalidates several times while identification/library-loading settle; each invalidation re-armed the busy overlay's client-side grace timer, so it never cleared. Fixed by debouncing (`max_cor_settled <- shiny::debounce(reactive(max_cor()), 1000)`) so the default-selection jump fires once, after the result actually settles. Verified via a 60s Playwright poll of `#openspecy_busy_overlay`'s `aria-hidden` state, before and after.
- **FileSpecs selection marker used the wrong coordinate space**: `current_select_xy()`'s FileSpecs branch read plain grid `x`/`y` from the index row, but the heatmap itself plots `app_filespec_coordinates()`'s space (absolute `stage_x_nm`/`stage_y_nm` when present). This placed the marker at the wrong point and badly distorted Plotly's axis autorange (observed as values spanning millions of nm off-window). Fixed by routing the marker through `app_filespec_coordinates()` too. Found while live-testing R7's click flow against `test_1um.h5`.

## Verification

- **Focused**: `devtools::test(filter = "automate_particle_analysis|FileSpecs")`.
- **App/manual**: local `run_app()` smoke — single upload control behaves identically for a small map and for `test_1um.h5` over the cap; heatmap click moves the marker and updates the spectrum for ordinary, FileSpecs, and collapsed-particle states; hover shows x/y/z; legends never overlap the map; material colors match between heatmap/particle image/Summary bar chart; Uploaded Metadata shows the reduced x/y/z view above 100k rows and the prepended view below it.
- **Broad**: full `devtools::test()`; `devtools::document()` only if any roxygen changes; `R CMD check` before calling this tranche release-ready.
- **Hosted**: `openspecy-verify-hosted-app` reproduction of the Shinylive Action locally before/after the fix.
- **Reusable evidence**: 010's `test_1um.h5` scientific-parity results (raw and smoothed S/N, halo streaming) are unaffected by this tranche's UI-only changes and do not need to be rerun.

## Risks And Open Questions

- ~~The exact Shinylive Action failure is unconfirmed~~ — resolved: found via GitHub's REST API + nightly.link artifact download (no `gh` CLI/token available), root-caused to a `download_ui` selection-preservation bug, and fixed (see checklist).
- ">100k spectra" is taken as the literal threshold from the request (`app_uploaded_metadata_large_threshold`); no existing constant found to match against.
- Chunk sizing "up to 100 MB" is translated into a spectra count via `.filespec_chunk_size_for_bytes()` (double precision, 8 bytes/value, per the source's band count); exact spectra-per-chunk varies by file. Verified against `test_1um.h5` (1738 bands): ~7,541 spectra/chunk ≈ 100MB.
- `automate_particle_filespecs.R`'s own `chunk_size` option (`OpenSpecy.filespec.chunk_size`, default 8192 spectra, used for the bulk S/N/particle-mean streaming) was left untouched — it already reads in bounded chunks (not per-spectrum), and for typical hyperspectral band counts already lands close to 100MB. Only the app's per-click `load_filespec_selection()` path (R7) was streaming truly one spectrum at a time and needed the new block-cache.

## Status

Implemented and verified. Focused suites (`automate_particle_analysis`, `FileSpecs-particle`, `particle_image`, `run_app`, `run_app-filespec`) pass with 0 failures; full `devtools::test()` run as the final gate. Live Playwright smoke covered: ordinary small-map upload (busy-overlay lifecycle, Uploaded Metadata x/y/z-front ordering), FileSpecs auto-routing at a temporarily-lowered threshold (heatmap axis/marker correctness, click-to-select with the new block cache, cache-hit vs cache-miss reads verified numerically identical to direct single-spectrum reads), and R11's legend repositioning (continuous, categorical, and `particle_image()` legends all render outside the plotted data). The real 2 GiB `app_upload_limit_bytes()` threshold is restored in `inst/shiny/global.R` (no leftover test values).
