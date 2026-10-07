# Feature Plan: Positive-Control Medoid And Model Validation

**Feature dir**: `specs/044-positive-control-medoid-model-validation`  
**Date**: 2026-10-07  
**Current tranche**: Extend the frozen positive-control benchmark to the corresponding OS1, pre-0.90-closure, and current FTIR derivative medoid libraries and classification models.  
**Change class**: Package/scientific benchmark and external validation; Stage 1 compatibility fixes are permitted only when directly required.

## Goal And Scope

- Reproduce the supplied OS1 medoid and model outputs, then compare three medoid artifacts and three FTIR derivative models using the exact frozen 20-map workflow and `automate_particle_analysis()`.
- **In**: six complete result sets, independent saved-output scorers, two Stage 1 gates, paired material/size/particle diagnostics, runtime/memory/warnings, and an updated report combining all nine library representations.
- **Out**: tuning on this holdout; rebuilding/replacing libraries or models; app/deployment changes; PMMA and Red PET Fiber controls; remote synchronization.
- **Users**: maintainers choosing among full, medoid, and model representations and deciding future library/model development.

## Requirements

- R1. Reuse the exact 20-map cohort, exclusions, ordering, crosswalk, scoring, S/N bands, area semantics, processing, outputs, and in-memory mode frozen by plan 043; only the supplied library/model artifact may change.
- R2. Independently score `OS1_Results/mediod/particle_details_all.csv` and `OS1_Results/model/particle_details_all.csv`. Locked mean/RSD (%) targets are medoid: count `91.003/41.538`, area `109.716/53.575`, Feret `97.659/35.940`, Specific `92.228/12.162`, Broad `94.067/11.199`; model: the same recovery plus Specific `93.998/7.570`, Broad `95.496/6.098`.
- R3. Stage 1 runs `automate_particle_analysis()` with OS1 `medoid_derivative.rds` and `model_derivative.rds[["ftir"]]`. For each representation, every finite overall, material-type, and size-stratum mean/RSD delta must be <=1.0 percentage point, every per-sample metric must be within 1.0 point, and paired missing values must agree.
- R4. Medoid Stage 2 is locked until the complete OS1 medoid gate passes; model Stage 2 is independently locked until the complete OS1 model gate passes. Any package analysis-code change invalidates affected evidence and returns both routes to Stage 1.
- R5. Every map must complete in memory in <5 minutes with <80% physical memory and zero unexplained warnings. Record hashes, axes/predictors, classes, configuration, elapsed time, memory, warnings, and restartable per-map checkpoints.
- R6. Report within-representation paired deltas versus OS1 and current versus pre-closure, plus all-nine descriptive accuracy, material/size strata, particle class transitions, score/margin and fixed-threshold sensitivity where semantically valid, and coverage/provenance limitations.

## Technical Decisions

- **Artifacts**: OS1 medoid/model SHA-256 `E588F690...B5E7`/`6F4E5D20...E811`; pre-closure `34048398...4B2D`/`87C8656E...AD2`; current `6CB03C87...05CC`/`02A4E968...E955`. Select only `ftir` from combined artifacts. Medoids are `OpenSpecy`; models are multinomial glmnet bundles with `all_variables` and `dimension_conversion`.
- **Frozen processing**: `sn_range` 800--2200 and 2420--3200 cm^-1; smoothing, thresholds, collapse, normalization, derivative processing, truth, and outputs exactly match plan 043. Conform queries to each medoid wavenumber axis or model predictor axis; model scores are probabilities, not correlations.
- **Output**: add `04_os1_medoid`, `05_pre_0.9_medoid`, `06_current_medoid`, `07_os1_model`, `08_pre_0.9_model`, and `09_current_model` under `Positive_Controls/library_validation_3_libraries`, with representation-specific comparison CSVs/report sections under `comparison`.
- **OpenSpecy contract**: dense maps remain canonical `OpenSpecy`; spectra/metadata alignment, IDs, processing attributes, medoid axes, model predictor order, and class conversion tables are asserted before use.
- **Performance**: probe red beads, Red Brick, and Clear Silicone first. Expect medoid/model matching to be no slower than full libraries and each map <5 minutes; checkpoint each map, stop at >=5 minutes, >=80% memory, an artifact/gate mismatch, or 10 minutes without progress.
- **Public API/dependencies/generated docs**: no API or dependency change planned; no roxygen generation unless Stage 1 exposes a package defect. Use existing glmnet/model matching. Long validation remains outside routine tests.
- **Bundled app/pipeline diagram/hosted app**: N/A; benchmark and external outputs only. If `R/` changes, update this decision and run focused/full tests plus `-HostedAppStatic`; matching-artifact and wasm rebuild remain untriggered.

## Package Surfaces

- `benchmarks/positive_control_medoid_model_validation.R`: new restartable six-artifact runner/scorer/comparison; reuse plan-043 helpers without changing their frozen semantics.
- `specs/044-positive-control-medoid-model-validation/report.md`: combined result interpretation and recommendations; update plan 043 report only with a pointer if useful.
- External authorized result root: six new folders plus comparison artifacts; source maps and OS1 saved outputs remain read-only.
- `R/`, tests, `NEWS.md`, generated docs: unchanged unless a focused Stage 1 defect requires a tested package fix. `inst/`, site, workflows, DESCRIPTION, and deployment surfaces remain unchanged.

## Work Checklist

- [x] Implement the six-artifact benchmark, independent saved scorers, manifests, compatibility gates, and Stage 2 locks.
- [x] Run representative OS1 medoid/model probes; diagnose first divergence and verify runtime/memory before full Stage 1.
- [x] Pass both complete 20-map OS1 gates within 1%; only then run the four Stage 2 artifact sets.
- [x] Produce paired overall/material/size/particle diagnostics and the combined full/medoid/model comparison.
- [x] Write the report and run only verification triggered by actual code surfaces.
- [x] Reconcile evidence, inspect processes/status, and remove task scratch while retaining authorized results.

## Verification And Risks

- Direct checks: exact saved scorer; artifact hashes/types/axes/classes; deterministic cohort/config; Stage 1 <=1 point at every requested level; identical recovery across representations; per-map runtime/memory/warnings; current-source hash agreement.
- Benchmark staging: score saved outputs, run two three-map probes, then OS1 medoid/model sequentially; Stage 2 remains locked by gate files and runs sequentially to stay below the memory threshold.
- Package gates: benchmark parse and evidence audit always. Reuse plan-043 package tests/docs/HostedAppStatic while `R/` is unchanged; if package code changes, rerun focused tests, documentation if triggered, full `devtools::test()`, and `-HostedAppStatic`. R CMD check is deferred unless scope becomes release-facing.
- Risks: historical model prediction/tie behavior may differ from current `match_spec`; OS1 and new predictor axes differ (363 vs 400); medoid axes differ (1,983 vs 400); probabilities and correlations are not directly comparable; small strata cannot establish superiority.

## Approval Notes

- Approved by maintainer request on 2026-10-07. Library/model rebuilding and publication remain out of scope.
