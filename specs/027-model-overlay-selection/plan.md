# Feature Plan: Model Overlay Selection

**Feature dir**: `specs/027-model-overlay-selection`
**Date**: 2026-09-04
**Status**: Complete
**Current tranche**: Restore the logistic-model weight overlay and make Top Matches row selection update it for single spectra and selected spectra within batches/maps.
**Change class**: bundled-app behavior

## Goal And Scope

- Show the quantitative logistic-regression weight background whenever a user views a model-identified spectrum, and update it to the selected Top Matches class.
- Keep raw/processed/reference plotting, model prediction, quantification, and downloads unchanged.
- Out of scope: random-forest interpretability, model retraining, model/library artifact changes, new controls, dependency changes, or remote synchronization.

## Requirements

- R1. A logistic-model Top Matches selection resolves against the rows for the currently selected spectrum, not the unfiltered predictions for the whole upload.
- R2. The overlay works for a single-spectrum upload and for one selected spectrum in a batch/map; changing the selected spectrum resets the candidate row to rank 1 and changing the Top Matches row updates the class/weights.
- R3. Typed FTIR/Raman/NIR model sets select the member that produced the chosen prediction. Missing classes, unsupported random-forest models, and unavailable coefficients safely omit the overlay without breaking the spectrum plot.
- R4. Automated tests prove row-to-class/model resolution and that two candidate selections produce distinct quantitative heatmap weights; the targeted browser journey verifies the visible selection transition when feasible.
- R5. Spectrum trace toggles remain in a horizontal legend above the axes at desktop and mobile widths; when logistic weights are present, their vertical quantitative scale occupies a separately reserved right margin and does not cover the spectra or trace legend.

## Technical Decisions

- **Approach**: move candidate scoping and typed-model resolution into a pure app helper. The server supplies `selected_unit_index()` and the table row, removing the incorrect whole-upload `source_count() == 1` gate. Use an explicit row-selection observer and reset only when the viewed spectrum/predictions change.
- **Public API / dependencies**: no package export, argument, or dependency change; this is internal bundled-app reactivity around the existing exported `model_class_weights()` plotting contract.
- **OpenSpecy contract**: no spectral or metadata mutation. The selected unit continues to come from canonical `DataR()`/`DataR_plot()` and only chooses explanatory model coefficients.
- **Generated artifacts**: no roxygen/NAMESPACE/man generation. Add a NEWS fix entry.
- **Performance**: candidate filtering is at most the compact Top-N output and model weights are computed only for the viewed class; no benchmark is needed for this corrected behavior.
- **Bundled Shiny app**: `inst/shiny/server.R` and `global.R` change; no assets or downloads change. Verify single-spectrum and batch selected-spectrum model states plus non-model/no-identification safety.
- **Pipeline diagram**: update the model-identification/Top Matches-to-spectrum-overlay branch in `.specify/memory/pipeline-diagram.html` because the selected-unit and selected-candidate reactive relationship changes.
- **Hosted Shinylive/WebAssembly**: shared `inst/shiny/` input changes, triggering fast `-HostedAppStatic`. This is an interaction change, so use an exact matching-artifact preflight only if an artifact matching the dirty candidate exists; no dependency/image/driver/pin change or clean wasm rebuild is required.

## Package Surfaces And Work Checklist

- [x] `inst/shiny/global.R` and `server.R`: scope candidate selection to the current spectrum, resolve typed logistic models, remove the batch gate, and make row/spectrum selection reactive.
- [x] `tests/testthat/test-run_app_reactivity.R` and app helper tests: cover batch candidate selection, typed models, selection changes, unsupported models, and distinct heatmap overlays.
- [x] `.specify/memory/pipeline-diagram.html` and `NEWS.md`: document the repaired interactive branch and user-visible fix.
- [x] Parse app sources, run focused tests, exercise the targeted browser journey, and run fast `-HostedAppStatic`; reuse broader passing package evidence because package/scientific contracts are unchanged.
- [x] Reconcile every checkbox with evidence; record deferred gates, inspect owned processes and `git status`, and remove task-created scratch files.
- [x] `inst/shiny/global.R`, focused layout assertions, and the named browser journey: separate the top trace legend from the right-side logistic scale and verify non-overlap.

## Verification

- Focused: compact `testthat` filters for run-app/helpers and interactive plots, plus source parsing.
- Browser: upload a genuine multi-spectrum fixture, enable logistic model identification, select another spectrum and second Top Matches row, and confirm the heatmap trace/class changes without severe console/server errors. If the exact local app cannot access a matching model artifact, record the limitation and use the real saved model in an R integration probe.
- Hosted: run fast `-HostedAppStatic`; exact-artifact preflight is conditional on an artifact matching this candidate and otherwise deferred to Actions. Full `devtools::test()`, documentation, R CMD check, and clean wasm rebuild are not triggered by this bounded app-only fix.
- Closure: inspect app asset inventory (unchanged expected), owned processes, root scratch, plan checkboxes, and git status.

Verification result: app sources parsed; focused in-memory helper, reactivity, layout, and interactive-plot tests passed with no failures. Fast `-HostedAppStatic` passed 304 assertions with zero failures/warnings/skips. A real local-browser run loaded the bundled derivative logistic models for two Raman spectra: rank 1 rendered one heatmap, selecting rank 2 changed the active DT row and heatmap/colorbar, and selecting the other uploaded spectrum reset rank 1 and changed the overlay again. A second named real-browser journey passed and geometrically confirmed that the horizontal raw/active trace legend is above the axes, the vertical logistic-weight scale is beyond the right plot edge, and their rendered boxes do not intersect. The current-page console had no errors. App assets remain 13 files/419,694 bytes (`www`: 6 files/103,931 bytes). The matching-artifact preflight, full suite, documentation, R CMD check, and clean wasm rebuild are not triggered and remain deferred. No owned R process or task scratch remains; existing Node runtimes and `.claude/settings.local.json` are user/environment-owned.

## Risks And Open Questions

- DT briefly clears selection while rerendering; the reset must not overwrite a genuine row click or preserve an invalid rank after the selected spectrum changes.
- Legacy logistic artifacts have no explicit `model_type` but do contain coefficients; retain the existing compatibility default.

## Approval Notes

- Approved by user bug report, 2026-09-04.
