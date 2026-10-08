# Feature Plan: Hierarchical Spectral Model Selection

**Feature dir**: `specs/047-hierarchical-model-selection`  
**Date**: 2026-10-08  
**Current tranche**: Add a deployable two-stage logistic model and guardrailed multi-metric lambda selection, then validate frozen candidates with the existing positive-control workflow.  
**Change class**: Package/scientific behavior plus long-running external validation.

## Goal

- Train broad material type before material class so natural spectra do not compete directly with every polymer subtype, while preserving the final labels and `match_spec()`/`automate_particle_analysis()` workflow.
- Select regularization without trading away common-class accuracy solely to maximize macro accuracy, and retain reproducible legacy flat models for comparison.

## Scope

- **In**: hierarchical logistic training from `material_type` to `material_class`; joint path probabilities; flat and hierarchical model artifacts; grouped out-of-fold selection diagnostics; positive-control comparison against OS1 and current flat models.
- **Out**: hierarchical random forests, probability-threshold abstention, taxonomy edits, tuning from Positive Controls, replacing published/staged model artifacts, app controls, or remote synchronization.
- **Users**: maintainers rebuilding libraries and callers passing trained models to `match_spec()` or `automate_particle_analysis()`.

## Requirements

- R1. Supplying `hierarchy_col` to `train_spec_model()`/`build_model_lib()` trains a root over `(type_col, hierarchy_col)` and conditional leaf models over `class_col`; `NULL` retains flat topology and legacy saved artifacts remain deployable.
- R2. Filter leaf support first, require complete hierarchy values for retained spectra, derive one leak-free stable-group fold assignment stratified by final leaf, and reuse it at every node. A one-leaf branch is deterministic; a branch without deployable leaves is invalid.
- R3. Hierarchical inference returns normalized leaf probabilities `P(broad) * P(leaf | broad)` and the existing ranked columns and final label spelling. `top_n`, stored fill values, predictor order, typed FTIR/Raman labels, and streamed/in-memory particle paths remain compatible.
- R4. For each fitted node, compute out-of-fold overall accuracy, macro accuracy, multiclass log loss, Brier score, and per-class recall. The default `guardrailed` rule keeps lambdas within 0.01 absolute accuracy of the best overall result, then within 0.01 of the best eligible macro result, chooses minimum log loss, and breaks ties with the largest lambda. Preserve `macro` and `overall` rules for controlled comparison and reproducibility.
- R5. Store node topology, selected lambdas, policy constants, metrics, support, folds, and end-to-end out-of-fold diagnostics in assessment/checkpoint state; slim release artifacts retain only prediction state and scientific provenance needed to audit the topology and selection rule.
- R6. The official builder produces flat and hierarchical logistic collections side by side; historical `model_derivative.rds` aliases remain flat throughout this tranche, with acceptance only authorizing a later promotion decision. No holdout outcome may alter classes, weights, alpha, tolerances, or thresholds here.
- R7. Stage 1 must reproduce OS1 medoid and model saved recovery and accuracy mean/RSD within 1.0 percentage point at overall, sample, material-type, and size levels after excluding `PMMA_15Nov223_control` and `RedPETFibers_15Nov223_control`; any prediction-path change returns both gates to Stage 1.
- R8. Stage 2 uses identical `automate_particle_analysis()` settings to compare current flat-macro, flat-guardrailed, hierarchical-macro, and hierarchical-guardrailed models. Advance the final hierarchy only if overall/broad, plastic/non-plastic, size-stratum, and polyethylene recall are no worse than OS1 by more than 1.0 point; also report non-plastic-to-plastic errors, PE-to-PVA transitions, calibration, coverage, runtime, and uncertainty.

## Technical Decisions

- **Architecture**: keep node fitting and probability composition internal. `model_type = "hierarchical_logistic_regression"` owns a shared filler/axis, one broad model, branch models, leaf conversion, and deterministic branches; existing `logistic_regression` bundles are unchanged.
- **Public API**: add only `hierarchy_col = NULL` (input presence triggers hierarchy) and `selection_rule = c("guardrailed", "macro", "overall")` (a demonstrated policy choice). Fixed guardrails are recorded, not exposed as speculative tuning arguments; existing glmnet controls remain in `...`.
- **Official artifacts**: add `models$hierarchical_logistic_regression` and explicit `model_hierarchical_logistic_regression_<recipe>.rds` files while retaining flat collections and legacy aliases. Update checkpoint/signature versions so incompatible cached models cannot be reused.
- **OpenSpecy contract**: training accepts one aligned `OpenSpecy`; `wavenumber`, `spectra`, `metadata`, identifiers, processing attributes, and the one-spectrum fill object stay aligned. Prediction returns the current table contract, so particle code needs compatibility tests rather than a parallel pathway.
- **Dependencies/generated files**: reuse `glmnet` and current imports. Update roxygen and regenerate `man/build_lib.Rd`/`man/match_spec.Rd` with the configured roxygen version; inspect `NAMESPACE` and attribution diffs immediately.
- **Performance/observability**: benchmark about 6,500 spectra × 400 predictors × 41 leaves with five grouped folds. Target <10 minutes and <80% physical memory per candidate fit, <5 minutes per map, checkpoints after each fitted node/map, and progress with dimensions/elapsed time. Stop at 2× the probe projection, 80% memory, or 10 minutes without progress and isolate the node/map kernel.
- **Holdout discipline**: choose/freeze candidates using grouped builder development evidence only. Write final confirmation to `Positive_Controls/hierarchical_model_validation`; a failed Stage 2 is reported, not tuned against this cohort.
- **Bundled app/pipeline diagram**: `inst/` and `.specify/memory/pipeline-diagram.html` are unchanged because no app route, control, staged model, or `match_spec()` stage changes; coefficient overlays remain flat-model-only in this tranche.
- **Hosted impact**: `R/` is shared hosted input, so run fast `-HostedAppStatic`. Exact-artifact preflight becomes required only if a hierarchical model is staged; no dependency/pin/driver change triggers a clean wasm rebuild now.

## Package Surfaces

- `R/build_lib.R`: node trainer, selection metrics/policy, official dual builds, checkpoints, assessments, slimming, release/load manifests, and roxygen.
- `R/match_spec.R`: hierarchical prediction and ranked joint probabilities with flat-model backward compatibility.
- `tests/testthat/test-build_lib.R`, `test-match_spec.R`, `test-automate_particle_analysis.R`: topology, folds, selection, probabilities, serialization/slimming, legacy artifacts, and dense/streamed workflow parity.
- `benchmarks/hierarchical_model_validation.R`: frozen Stage 1/2 runner that reuses plan-044 settings and writes restartable external evidence; retain old comparison code in `benchmarks/`. Summarize results and recommendations in `specs/047-hierarchical-model-selection/report.md`.
- `vignettes/library-builder.Rmd`, `NEWS.md`: model construction, selection semantics, interpretation, and compatibility; `workflows/OpenSpecy_reference_library.R` changes only if an explicit reproducibility setting is needed.
- `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README, assets: unchanged. Generated Rd changes only through roxygen.

## Work Checklist

- [ ] Implement internal node fitting, guardrailed selection, hierarchy artifacts, diagnostics, slimming, and cache invalidation in `R/build_lib.R`.
- [ ] Add hierarchical deployment to `R/match_spec.R` without changing flat model or particle-result contracts.
- [ ] Add focused synthetic tests, including hierarchy errors, one-leaf branches, joint probability/ranking, grouped folds, metric selection, old artifact loading, and in-memory/streamed particle parity.
- [ ] Update roxygen, vignette, NEWS, and generated documentation with reviewed diffs.
- [ ] Add the restartable benchmark; fit/freeze the four current-source candidates from identical inputs and run the three-map time/memory probe.
- [ ] Pass both complete Stage 1 gates before Stage 2; run the frozen 20-map comparisons once and produce paired accuracy, calibration, confusion, runtime, and recommendation outputs.
- [ ] Run focused tests, benchmark assertions, documentation, full tests, and fast hosted-source verification; reconcile evidence, processes, status, and scratch cleanup.

## Verification

- Focused: compact `devtools::test(filter = "build_lib|match_spec|automate_particle_analysis", reporter = "summary")`; assert probabilities sum to one, stable groups never cross folds, legacy predictions are unchanged, and flat/hierarchical artifacts survive release slimming/reload.
- Model-development gate: on the builder grouped development split, hierarchy must be within 1 point of flat for overall, broad, macro, and supported-class recall, with no worse log loss/Brier beyond bootstrap uncertainty; retain full per-node metrics even when it fails.
- External Stage 1/2: use `true_values_2`, the two fixed exclusions, exact plan-044 processing, hashes/configuration, per-map checkpoints, bootstrap paired intervals, and the R7/R8 non-inferiority gates. Recovery count/area/Feret outputs must remain invariant across model candidates.
- Broad gates: verify configured roxygen, run `devtools::document()` once and inspect generated diffs, then full `devtools::test()` and `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -HostedAppStatic`. R CMD check and hosted matching-artifact preflight are deferred until release/staging.
- Reusable evidence: plan-044 truth/scorer definitions and legacy saved targets remain reusable; all runtime evidence is invalidated by changes to `build_lib.R`, `match_spec.R`, particle analysis, model inputs, or benchmark configuration.

## Risks And Open Questions

- Root errors can propagate and joint probabilities may be less calibrated than flat scores; report path-level diagnostics before considering abstention or calibration in a later plan.
- Passing the holdout authorizes a later publication decision; it does not automatically replace the historical model alias or hosted staged library.

## Approval Notes

- Approved by the maintainer's 2026-10-08 request for hierarchical training, multi-metric selection, and validation through the frozen positive-control workflow.
