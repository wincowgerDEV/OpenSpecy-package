# Feature Plan: Uploaded Crop Range Button

**Feature dir**: `specs/054-uploaded-crop-range-button`  
**Date**: 2026-11-12  
**Current tranche**: Keep one Crop XY Range action that reuses materialized upload state without rereading.  
**Change class**: bundled-app behavior

## Goal And Scope

- Add an explicit Advanced-tab action that fills all four Crop XY Range inputs from the current upload before analysis.
- **In**: local and hosted staged uploads, already materialized uploads, button availability/progress, adjacent guidance, pipeline diagram, focused/static/browser regressions.
- **Out**: changing crop inclusion rules, coordinate calibration, canonical Run publication, package APIs, downloads, dependencies, deployment, or generated hosted artifacts.

## Requirements

- R1. Crop XY Range includes one clearly labeled **Use Uploaded Range** button; the redundant **Reset to Full Range** action is absent.
- R2. With no upload the action is disabled. With a staged upload it works whether Crop XY Range is on or off, reads/materializes the existing source through the shared upload reader, and fills X/Y minimum and maximum without triggering canonical analysis.
- R3. If the upload is already materialized, derive the extent directly from `preprocessed$data` using the current display calibration; do not reread the file or replace current analysis until Run is required by an effective enabled-crop change.
- R4. A newly staged upload remains Run-dirty after the action. With crop off, filling bounds is muted and does not add an invalidation. With crop on, replacing a prior custom range on current data makes Run pending. Run remains the only owner that applies the crop to plots, analysis, and outputs.
- R5. Long reads use the existing central phase/progress display. Read/check failures retain the existing upload error/status behavior and never publish canonical analysis.

## Technical Decisions

- **Single data path**: reuse `read_uploaded_files()` for an unmaterialized source, preserving dense/`Specs`/`FileSpecs`, particle-centroid attributes, calibration, validation, and bounded readers. Do not add a second extent-only import path.
- **State owner**: `active_file_info()` determines no-input availability, `preprocessed$data` is checked first and supplies an already-read extent without a reader/progress phase, `analysis_running()` prevents competing use, and existing crop policy/signature state determines muted versus Run-dirty behavior.
- **OpenSpecy flow**: the action may populate the existing preprocessed source but does not publish `canonical_state()`, `quantified_data()`, `pixel_projection()`, plots, summaries, or downloads. Object structure and attributes are unchanged.
- **Guidance**: explain that the action reads a staged source only when needed, otherwise uses the materialized upload, fills displayed-axis extents, works while Crop XY Range is off, and still requires Run to apply an enabled crop.
- **Pipeline diagram**: add the explicit pre-Run range action as another materialization trigger and state that only the four draft bounds are updated.
- **Assets/dependencies**: no new or changed asset or dependency; only a staged-only read starts the server-owned central progress phase.
- **Hosted impact**: `inst/shiny/` is shared local/hosted source, so run `-HostedAppStatic`. Routes `/`, `/app/`, `/pkgdown/`, package/dependency pins, staged libraries, workflow assembly, and generated outputs are unchanged; matching-artifact and clean-rebuild tiers are not triggered.

## Package Surfaces

- `inst/shiny/ui.R`: single button and adjacent guidance.
- `inst/shiny/server.R`: availability, materialized fast path, staged shared-reader action, extent update, and muted/dirty semantics; `canonical_state()` remains the sole final analysis source.
- `tools/shiny-local-smoke.spec.js`: genuine processed-particle upload, pre-Run range population, and retained Run-dirty state.
- `tests/testthat/test-run_app.R`: static ownership and integration assertions.
- `.specify/memory/pipeline-diagram.html`: synchronized action/data-flow description.
- `R/`, generated docs, `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, downloads, benchmarks, workflows, and bundled assets: unchanged. Same-output benchmark is N/A because this is new UI orchestration.

## Work Checklist

- [x] Keep only **Use Uploaded Range** and put the materialized-upload fast path before the staged reader/progress path.
- [x] Synchronize guidance/static assertions and retain the genuine pre-Run browser regression.
- [x] Run focused `run_app` static tests, the named browser journey, and `-HostedAppStatic`; inspect the changed screenshot and console.
- [x] Reconcile evidence, run `git diff --check`, report unchanged asset inventory, inspect owned processes/status, and clean task scratch.

## Verification

- Focused: parse all bundled app sources and JS; run `run_app` assertions for the single control, independent enablement, materialized fast path before the shared reader, and no competing canonical trigger.
- Browser: stage the genuine `particles_*.rds` fixture, open Advanced before Run, click **Use Uploaded Range** while crop is off, confirm `0..2` bounds, heatmap still awaits Run, and Run remains visible/dirty; retain the existing crop/centroid journey as regression coverage and inspect the changed crop-card screenshot plus severe console errors.
- Hosted: run fast `-HostedAppStatic`; exact artifact and clean wasm rebuild are proportionally deferred because runtime assembly, routes, dependencies, pins, and release inputs are unchanged.
- Broad package tests, documentation, and R CMD check are not triggered because no package/scientific or generated-document surface changes.

## Risks And Open Questions

- No blocking question. Reading is unavoidable when a staged format does not expose its complete spatial extent in its header; the existing reader preserves bounded/file-backed behavior where supported.

## Approval Notes

- Approved by the user's implementation request on 2026-11-12. Remote synchronization remains maintainer-owned.
