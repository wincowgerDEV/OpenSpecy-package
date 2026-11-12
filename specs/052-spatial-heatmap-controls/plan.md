# Feature Plan: Spatial Heatmap Controls and Particle Centroid Views

**Feature dir**: `specs/052-spatial-heatmap-controls`
**Date**: 2026-11-10
**Current tranche**: Add Run-gated XY cropping, quantified heatmap layers, centroid-aware processed-particle plotting, a red scope selection marker, and remove the landing timeline badge.
**Change class**: mixed; package/scientific is highest, with bundled-app behavior and hosted presentation changes.

## Goal

- Let users crop a spatial analysis from its Plotly map, color that map by their named quantitative results, and inspect uploaded processed particles as selectable centroids rather than synthetic pixels.
- Keep every visible/exported result aligned to one cropped canonical spectral state while preserving the `OpenSpecy` object contract.

## Scope

- **In**: Advanced-tab Plotly/manual XY crop; saved/restored crop controls; quantitative map-color choices with globally unique names; typed processed-particle RDS objects; `heatmap_spec()` centroid-point behavior; red scope marker; TOPSCINET badge/link/CSS removal; tests, docs, NEWS, and pipeline diagram.
- **Out**: changing ratio/measurement calculations, reconstructing particle footprints from centroid-only RDS files, changing reference libraries, redesigning the remaining timeline, hand-editing generated docs/web output, deployment, or remote synchronization.
- **Users**: local and hosted app users working with maps or `particles_*.rds`, and package users calling `heatmap_spec()` on particle-level `OpenSpecy` objects.

## Requirements

- R1. Add **Crop XY Range** near the bottom of Advanced, before Load Settings. Its manual X minimum/maximum and Y minimum/maximum inputs accept known pre-upload bounds; otherwise successful materialization seeds the full displayed extent. Keep Plotly Box Select active on every current map and write each normalized selection to the inputs whether cropping is on or off. The owner toggle alone decides whether Run applies them; reset restores the full source extent.
- R2. The next Run includes coordinate centers on all four box edges and excludes spectra outside the box before preprocessing, identification, quantification, summaries, and downloads. Reject missing, degenerate, or empty bounds without replacing the last current result; no-input is disabled, running uses the committed snapshot, success publishes it, and failure retains the draft for retry.
- R3. Ratio and measurement display names must be unique together after trimming and case folding; any duplicate is rejected before saving the definition. After Run applies saved definitions, Map Color offers each finite numeric result under the exact user-provided name while using its collision-safe metadata column as the value. Selection is run-gated, projects unit results back to member pixels (or particle points), and disappears when its definition/result is absent.
- R4. Every newly created processed `particles_*.rds` carries `attr(x, "openspecy_object_type") == "particle_centroids"`; upload preserves/uses that type and renders one Plotly `scattergl` point per aligned particle centroid, not an inferred rectangular heatmap. Hover, click, keyboard/table synchronization, map coloring, axis units, and nearest-point selection use the same particle index; no particle footprint is invented. Legacy centroid-bearing particle objects remain inferable for compatibility.
- R5. `heatmap_spec()` preserves regular-grid behavior but infers point rendering for particle-level `OpenSpecy` metadata with finite aligned centroids. `readRDS("particles_sample.rds") |> heatmap_spec(z = ...)` returns a selectable Plotly object without changing spectra, metadata, identifiers, or attributes.
- R6. Replace the selected-location white circle with a high-contrast red scope sight (ring plus centered crosshairs) above heatmap, point, rejection, and registered-image layers at representative map scales.
- R7. Remove the TOPSCINET Top 2% badge/link from the 2026 landing timeline, delete its orphaned CSS/assertions and stale NEWS claim, and retain the 2026 development text and the rest of the timeline.

## Technical Decisions

- **Approach**: seed full-range numeric child controls after upload, capture persistent Plotly `select2d`/`plotly_brushed` ranges independently of `xy_crop_decision`, and retain the red scope as the only custom map shape. Apply enabled bounds by aligned coordinate index for in-memory `OpenSpecy`/`Specs` and by bounded ROI access for `FileSpecs`, without dense full-map decompression.
- **Public API**: extend the existing `heatmap_spec()` behavior rather than add a competing export or mode flag. Point geometry is selected by the persisted `openspecy_object_type = "particle_centroids"` attribute, with aligned centroid metadata as a legacy fallback; `x`, `z`, thresholds, `select`, and `...` retain their documented uses. The app delegates centroid trace construction to this package behavior and adds only app-owned theme, rejection, image, crop, and scope layers.
- **Quantification**: validate trimmed case-insensitive name uniqueness across both definition tables, join active definition `name` to its generated `column`, read values only from `quantified_data()`, align by canonical spectrum/unit identifiers, and extend `pixel_projection()`; never recalculate quantification in the heatmap reactive.
- **Dependencies**: none; reuse Plotly, Shiny, and existing representation-aware accessors.
- **OpenSpecy contract**: spatial cropping changes only the retained spectra columns and matching metadata rows; it preserves wavenumbers, unique aligned IDs, valid attributes, source order within the box, and `NA` semantics. Processed particle outputs persist the `openspecy_object_type = "particle_centroids"` attribute through RDS serialization; plotting is read-only, and quantitative layers remain numeric metadata.
- **Generated artifacts**: update roxygen in `R/interactive_plots.R`, confirm roxygen2 8.0.0, run `devtools::document()`, and inspect `NAMESPACE`/`man/*.Rd` immediately; do not edit them directly.
- **Performance and observability**: centroid plotting must allocate O(particles), not an `n_unique_x * n_unique_y` matrix; add a representative sparse-centroid assertion. Cropping reports source/retained counts and bounds in the existing analysis phase; no production-scale or same-output benchmark is triggered.
- **Bundled Shiny app**: `canonical_state()` remains the single owner after crop, then feeds `quantified_data()` and `pixel_projection()` for plots, metadata, summaries, and genuine downloads. `xy_crop_decision` gates applying/editing bounds but not Box Select updates; adjacent help states default extents, coordinate units, inclusive edges, Run behavior, empty-box rejection, and reset consequences. Affected states are no-upload, uploaded defaults, muted selection, dirty enabled draft, running, current cropped grid/point map, quantified/identified, and error/retry; assets are unchanged.
- **Pipeline diagram**: add the committed Spatial ROI Crop between Read/Materialize and processing, and update Run Snapshot & Gate, Canonical Final, Quantification, Pixel Projection, Heatmap, Active Spectrum Inspection, and processed-download tips in `.specify/memory/pipeline-diagram.html`.
- **Reference workflows**: intake, official build, frozen validation, quarantine, and long-stage contracts are N/A.
- **Hosted Shinylive/WebAssembly app**: `R/`, `inst/shiny/`, and `site/` are shared inputs, so run fast `-HostedAppStatic` plus exact matching-artifact interaction/route preflight when available. Preserve `/`, relative `/app/`, `/pkgdown/`, hardcoded package/dependency pins, closure evidence, small-library staging, and generated boundaries; no clean wasm rebuild unless dependencies, image, driver, pin, or release scope changes.

## Package Surfaces

- `R/interactive_plots.R`, roxygen-generated `man/interactive_plots.Rd`: inferred centroid-point rendering and selection scope; `DESCRIPTION`/`NAMESPACE` unchanged unless regeneration proves otherwise.
- `inst/shiny/{ui,server,global}.R`: Advanced owner/children, Plotly relayout state, representation-aware crop, quantified color projection, point/grid rendering, shape composition, and guidance; `inst/shiny/www/` assets unchanged.
- `tests/testthat/{test-interactive_plots,test-app-in-memory-helpers,test-run_app,test-app-onboarding,test-shinylive_wasm}.R`: package invariants, crop/state/settings, quant labels, centroid clicks, scope geometry, landing removal, and hosted contracts with genuine temporary RDS/downloads.
- `site/index.html`, `site/assets/site.css`, `NEWS.md`, `.specify/memory/pipeline-diagram.html`, and the relevant heatmap/particle vignette example: user-facing cleanup and documentation.
- `benchmarks/`: N/A—intended output changes; add a bounded sparse-point allocation assertion instead. `workflows/` and `.github/workflows/`: unchanged. Generated pkgdown/wasm artifacts remain untouched.

## Work Checklist

- [x] Implement and document typed processed-particle outputs, aligned spatial ROI helpers, and centroid-aware `heatmap_spec()` in `R/{automate_particle_analysis,interactive_plots}.R`; add focused package tests.
- [x] Add Advanced crop UI/state/settings and apply its committed snapshot through dense, compact, and file-backed app paths in `inst/shiny/{ui,server,global}.R`.
- [x] Project named quantitative metadata into grid/point map choices and render selectable processed-particle centroids with the shared package behavior.
- [x] Compose the red scope and crop selection, update help/NEWS/vignette/pipeline diagram, and remove the landing badge plus orphaned CSS/tests.
- [x] Run focused tests, documentation generation, full tests, genuine local browser/upload/download acceptance, and `-HostedAppStatic`; exact-artifact preflight was unavailable for the current commit.
- [x] Reconcile every checkbox with evidence; report deferred gates, inspect/stop owned processes, inspect `git status`, and remove task-created scratch files.
- [x] Refine crop defaults and interaction: seed full uploaded extents, keep Box Select active without draw shapes, update bounds while muted, synchronize guidance/diagram/tests, and rerun the affected app/hosted gates.

## Verification

- Focused: `test-interactive_plots.R`, app helper/state files, hosted landing assertions, source parsing, regular-grid non-regression, centroid O(n) trace count/alignment, inclusive/negative-axis crop, invalid/empty box, duplicate/missing quant values, and scope shape tests.
- Browser: with a genuine map, Run, confirm full-range defaults and persistent Box Select, select while crop is off, verify bounds update without invalidation, enable crop and rerun, then click grid/centroid points and inspect the red scope, console, and screenshot. Existing quantification and download coverage proves named colors and only in-box aligned exports; upload a genuine `particles_*.rds` and select multiple centroids.
- Toolchain: resolve configured Windows R, confirm roxygen2 8.0.0, run `devtools::document()`, inspect generated diffs, then one final `devtools::test()`. R CMD check is deferred because this is not release/CRAN-facing and adds no dependency; escalate if package metadata or broader failures appear.
- Hosted: run `-HostedAppStatic`; with an exact action artifact, verify `/`, `/app/`, `/pkgdown/`, startup, upload, crop/rerun, quant color, centroid selection, download, one pinned-library match, console, and screenshots. Full clean rebuild is proportionally deferred.
- Reusable evidence: none for changed package/app/landing contracts; unchanged dependency pins, closure, and small staged libraries may reuse their latest passing evidence.
- Closure: report `inst/shiny` asset count/bytes (expected unchanged), removed external badge/CSS, deferred artifact/check gates, owned-process status, clean scratch state, and all unrelated worktree files left untouched.

## Risks And Open Questions

- No blocking questions remain: the maintainer confirmed a Run-gated crop affecting all analysis/outputs, manual pre-upload bounds, the persisted particle-centroid object attribute, and globally unique ratio/measurement names.

## Approval Notes

- Approved by: user planning request and clarified implementation decisions, 2026-11-10.
- Follow-up: implementation must apply `openspecy-develop-shiny-app`; remote push/pull/publish remains maintainer-owned.
