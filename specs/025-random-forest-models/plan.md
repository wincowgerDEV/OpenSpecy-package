# Feature Plan: Full-Library Random-Forest Models

**Feature dir**: `specs/025-random-forest-models`
**Date**: 2026-09-03
**Status**: Complete
**Current tranche**: Add reusable logistic/RF training, experimental full-library RF builds, logistic model explanations in the bundled app, and a level hosted hero video.
**Change class**: mixed (package/scientific, bundled-app behavior, dependency/hosted-source, and hosted presentation)

## Goal And Scope

- Train, serialize, identify with, and assess probability random forests for raw, derivative, and nobaseline FTIR/Raman/NIR libraries without adding them to the Shiny identification UI yet.
- Rename the existing model family to `logistic_regression`, preserve its calibrated fitting behavior, and compare both families with interpretable class-balanced and overall accuracy outputs.
- Expose reusable model training and quantitative logistic coefficient interpretation; show ranked model scores and the selected class's red-yellow-green coefficient background in the bundled app.
- Level the hosted homepage video card. Out of scope: publishing RF files, adding RF as a Shiny strategy, altering preprocessing/class taxonomy, or pushing/deploying.

## Requirements

- R1. Add exported `train_spec_model(method = c("logistic_regression", "random_forest"))` with documented, tested scientific defaults. `build_model_lib()` remains a compatible linked wrapper and `build_lib()` uses the new trainer. RF requires `ranger`; both return the common deployable model/fill/class/test contract.
- R2. Official `models` are nested by `logistic_regression` and `random_forest`; logistic models retain derivative/nobaseline medoid training, while RF models use the complete eligible raw/derivative/nobaseline libraries after spectrum-type range restriction, 10% observed-support filtering, relative normalization, and training-wavenumber mean filling.
- R3. RF uses a reproducible multiclass probability forest, inverse-frequency balanced case sampling, permutation importance, at least 500 trees, high-dimensional `mtry`, OOB class/overall diagnostics, and available CPU threads. User-supplied named `...` may override ranger tuning owned by ranger.
- R4. Accuracy tables, class tables, confusion tables, assessment/error correlations, summaries, warnings, checkpoints, progress, and release filenames identify the algorithm explicitly. Current/published models are labeled `logistic_regression`; legacy `model_derivative.rds` and `model_nobaseline.rds` remain readable and are retained as compatibility exports.
- R5. RF production fits use all eligible full-library spectra. RF accuracy uses the same deterministic 10% group-stratified source-local test cohort as the corresponding recipe/type comparison, fits a separate assessment forest to only the remaining 90%, and never evaluates a spectrum used to train that assessment fit. Model queries always come from full libraries, never medoids.
- R6. The hero video card has no rotation at desktop or mobile widths; autoplay, accessibility, privacy-enhanced URL, responsive 16:9 layout, and all unrelated landing content remain unchanged.
- R7. Model prediction can return deterministic top-N class probabilities per spectrum. The app's logistic Top Matches table shows those ranked scores and row selection controls the explanation; ordinary reference matching and map winner selection remain unchanged.
- R8. Extend the package spectral plotting function with an optional logistic model/class explanation. Align coefficients to the plotted axis and draw a continuous, symmetric, quantitatively scaled red (negative)-yellow (near zero)-green (positive) background with a labeled color bar beneath readable spectrum traces. Reject unsupported models/classes clearly and leave plots unchanged when omitted.
- R9. App explanations are derived from the loaded logistic artifact and selected spectrum/match only; they do not alter canonical data, processing, identification, quantification, metadata, or downloads. Hide the overlay for library matching, no identification, heatmaps/multiple spectra, and unavailable coefficients.

## Technical Decisions

- **Public API**: `train_spec_model()` owns fitting; `build_model_lib()` delegates without changing its arguments/default result. Existing `weights`, `seed`, `min_n`, fill, and `...` remain applicable. Add `model_type` to artifacts and dispatch internally from it; one-caller engine helpers remain internal. Add `top_n` model probability ranking and optional `model`/`model_class` explanation arguments to `plotly_spec()`.
- **Dependency**: add `ranger` to `Suggests`, not `Imports`, because RF is experimental and absent from Shiny; fail with an installation instruction only when requested. This avoids forcing ranger into the hosted wasm closure before UI adoption.
- **RF design**: `ranger::ranger(x, y, probability = TRUE, num.trees = 500, case.weights = inverse frequency, importance = "permutation", num.threads = 0)` with a reproducible seed. A four-way Raman holdout benchmark selected balanced sampling alone (macro 0.533, total 0.994) over class weighting alone (macro 0.402, total 0.989), combined sampling/weighting (0.510/0.993), and square-root sampling plus weighting (0.455/0.991). Use `mtry = max(floor(sqrt(p)), floor(p / 20))` for correlated high-dimensional spectra unless overridden; record resolved arguments, OOB macro/overall accuracy, class accuracy, and importance.
- **Validation design**: group by stable legacy/sample identity before stratification to prevent replicate leakage. Refit only assessment RFs on train partitions; cache each production and assessment model independently. Published old artifacts have logistic evidence only; RF is assessed on the candidate cohort without inventing an old comparator.
- **OpenSpecy contract**: preserve axes, aligned spectra/metadata, attributes, and IDs; model inputs are copied matrices. Stored filler and exact training axis make partial-spectrum prediction deterministic.
- **Performance/observability**: benchmark 1,000 spectra at the real per-type axis before production. Expect each 500-tree type fit under 30 minutes and <12 GB peak; report recipe/type, n, p, classes, trees, mtry, threads, elapsed, and OOB macro accuracy. Checkpoint every fit; stop/investigate any silent fit over 30 minutes, fit over 60 minutes, or memory pressure that threatens the 32 GB host, then resume from checkpoints. Keep the historical combined FTIR/Raman logistic member, but do not refit redundant combined RFs after the typed forests already cover those spectra.
- **Evidence basis**: ranger is designed for high-dimensional, memory-efficient forests; its probability forests average tree probabilities and support class weights. Spectroscopy literature supports normalization/baseline/derivative alternatives, sample-identity splits, inner tuning/OOB diagnostics, and independent tests. The experiment retains all three recipes so preprocessing value is measured rather than assumed.
- **Generated artifacts**: update roxygen and vignette, add `ranger` metadata, confirm configured roxygen2, run `devtools::document()`, and inspect generated diffs; never edit `NAMESPACE` or `man` directly.
- **Interpretation/app**: logistic top-N scores come from one prediction pass; coefficient values are tied to the selected class label through `dimension_conversion`. The overlay is explanatory model weight, not peak attribution or causal evidence. Update `.specify/memory/pipeline-diagram.html` for prediction ranking → selected class → coefficient overlay; the canonical final spectrum remains the sole analysis source.
- **Hosted**: `site/`, `R/`, `DESCRIPTION`, and `inst/shiny/` trigger `-HostedAppStatic`. Inspect landing desktop/mobile, verify bundled no-upload/single-spectrum model/ordinary-library/multi-spectrum states, and run exact-artifact preflight plus nested-frame smoke if a matching wasm artifact exists. Ranger remains Suggests and outside app roots, so no wasm closure/pin change unless resolution proves otherwise.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`, `R/match_spec.R`: add `train_spec_model()`, compatible wrapper, unified dispatch, model top-N, full-library RF production/holdout training, algorithm-aware layouts, progress, checkpoints, assessments, and compatibility handling.
- [x] `R/interactive_plots.R`, `inst/shiny/global.R`, `inst/shiny/server.R`, and app tests: implement reusable logistic coefficient overlay, ranked Top Matches selection, and correct single/multi/no-identification gating without RF UI exposure.
- [x] `tests/testthat/test-build_lib.R`, `tests/testthat/test-match_spec.R`: cover RF fit/prediction, weights/fill/OOB schema, leakage-free full-library holdout, algorithm labels/layout, logistic compatibility, and release files with small deterministic fixtures.
- [x] `DESCRIPTION`, roxygen/generated help, `vignettes/library-builder.Rmd`, `NEWS.md`, workflow example, build diagram, and Shiny pipeline diagram: document dependency, training API, score/weight interpretation, assessments, resource behavior, and migration.
- [x] `site/assets/site.css` and hosted landing assertions: remove the hero rotation and inspect desktop/mobile presentation without changing media behavior.
- [x] Benchmark a representative 1,000-spectrum fit, then reuse completed libraries to checkpoint and run all production RF/logistic models plus assessment-only holdouts; report model sizes, elapsed time, OOB/holdout macro and total accuracy, coverage, and top confusion rows.
- [x] Run focused package/app tests, documentation, vignette validation, full tests, bundled/static parsing, and `-HostedAppStatic`; run exact-artifact hosted preflight/browser smoke when available; defer R CMD check unless release/CRAN is requested.
- [x] Reconcile every checkbox with evidence; record deferred gates, inspect/stop owned processes, inspect `git status`, and remove task scratch.

Verification completed with focused tests, 3,163 full-suite passes (zero failures), 304 bundled/hosted static passes, successful vignette knit, and the focused bundled-app Playwright journey. Exact-artifact hosted preflight was unavailable because this uncommitted candidate has no matching action-built wasm artifact; R CMD check remains deferred by the approved plan.

## Risks And Open Questions

- Full-resolution forests can be large and expensive; the representative benchmark and stop thresholds govern production rather than allowing an unbounded run.
- Inverse-frequency balanced sampling improves minority representation but can reduce calibration and make OOB results optimistic; grouped holdout macro accuracy is primary, while total accuracy, class tables, confusion counts, and OOB-versus-holdout differences must also be reported.
- Exact old-vs-new RF comparison is unavailable because no published RF exists. Logistic legacy comparison remains intact; RF value is judged against candidate logistic on equivalent candidate full-library holdouts.
- Logistic coefficients are signed model weights, sensitive to correlated wavenumbers and preprocessing; the UI must label them as model influence and not imply causal peak importance.

## Approval Notes

- Approved and expanded by user request, 2026-09-03. Remote push/deploy and Shiny RF adoption remain maintainer-owned follow-ups.
