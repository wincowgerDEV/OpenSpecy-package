# Feature Plan: Spectrum-Type Reference Quality Rebuild

**Feature dir**: `specs/022-spectrum-type-reference-quality`  
**Date**: 2026-09-01  
**Review budget**: Under 100 nonblank lines and about 1,500 words.  
**Current tranche**: Add post-processing artifact/SNR gates, type-specific library ranges including NIR, fair macro-accuracy assessment, stable schemas, complete-library Shiny routing/preview polish, and complete a checkpointed production rebuild.  
**Change class**: mixed package/scientific and bundled-app behavior

## Goal

- Produce smaller, type-specific reference artifacts with explicit FTIR CO2, high-tail, and running-SNR quality control before medoid/model construction.
- Make old/new accuracy scientifically comparable by evaluating both on identical held-out spectra and treating mean class-wise accuracy as the primary metric.

## Scope

- **In**: NA-aware artifact assessment; FTIR CO2 flattening; high-tail correction/drop; SNR filtering; FTIR/Raman/NIR library partitions and ranges; complete/all-type app identification; pre-Run SNR preview; readable metadata/quality outcomes; shared holdouts; macro metrics; warning/numeric schemas; diagrams, docs, tests, benchmark, and one full reusable rebuild.
- **Out**: Publishing artifacts, changing `get_lib()` download endpoints, changing processing algorithms outside the named quality gates, and retraining downloaded legacy models.
- **Users**: Maintainers run `build_lib()` with explicit source/output paths; package and Shiny users identify FTIR, Raman, or NIR spectra with compatible artifacts.

## Requirements

- R1. After derivative and baseline removal, assess each spectrum on its finite support. For FTIR, flatten the CO2 interval only when `max(CO2 region) / max(silent region) > 2`; record before/after ratios and correction status.
- R2. High-tail detection ignores leading/trailing `NA` values. Apply the same ratio and tail-width defaults exposed by the bundled app to derivative and nobaseline spectra; retain a corrected spectrum only when its reassessment passes, otherwise remove it and audit the reason.
- R3. Remove derivative and nobaseline spectra with finite running SNR below 2; undefined/non-finite SNR is an explicit removal or unavailable status, never silently retained. Finish CO2, high-tail, and SNR gates before pruning, medoids, models, or comparisons.
- R4. Store full libraries by spectrum type, restrict Raman to 200–4000, FTIR to 400–4000, and NIR to 4000–12000, and independently drop metadata columns that are entirely missing/blank while preserving aligned identifiers and required `OpenSpecy` fields.
- R5. Build Raman and FTIR medoids/models on 800–3200. Build a separate NIR medoid/model on a deterministic finite-coverage interval derived within 4000–12000 and record that interval and coverage. Preserve legacy filenames as containers and compatibility access where practical.
- R6. Identification assessments use `match_spec()`/the package classification path with NA handling, not direct base/model prediction. Missing values inside 800–3200 remain eligible and prediction coverage is explicit.
- R7. For every old/new comparison, derive one reproducible cohort from stable identities present on both sides and use exactly the same held-out spectrum IDs for both. Keep new-only/old-only coverage separate from the paired accuracy result and never retrain legacy models.
- R8. Report macro class accuracy first, with evaluated coverage, overall accuracy, class counts, and confusion details. Any training-time model selection uses only the training partition and maximizes macro class accuracy; the shared 10% holdout remains untouched.
- R9. Every assessment table has a stable typed empty schema. Counts and measurements are integer/double/logical as appropriate; no numeric metric is coerced to character merely to combine rows.
- R10. Default the bundled app’s Spectrum Type to `all`, routing full/medoid identification across complete FTIR, Raman, and NIR libraries and model identification across every overlapping type-specific model; preserve explicit single-type selection, raw/processed viewing, optional identification, quantification, downloads, and heatmap ownership rules.
- R11. Report progress with dimensions, elapsed time, removals, and checkpoint writes/reuse at each quality gate, type partition, medoid/model, and assessment. Export each completed component before dependent work begins.
- R12. The app rounds displayed signal-to-noise metadata to two significant figures without mutating stored values, reports excluded CO2/silent assessment regions as successful no-ops, and lets Recalculate Preview materialize a staged upload and calculate S/N before Run.

## Technical Decisions

- **Approach**: Keep `build_lib()` as the public orchestrator and add focused internal helpers for finite-support metrics, per-spectrum correction, typed partitioning, shared-cohort construction, and typed assessment schemas. `libraries`, `medoids`, and `models` remain recipe-first and become spectrum-type keyed where applicable; `both` is retained only as a compatibility view over the shared FTIR/Raman 800–3200 interval.
- **Public API**: Avoid speculative flags. Existing `build_lib()` stage argument lists carry thresholds; defaults encode CO2 ratio 2, SNR 2, and declared ranges. Add only the minimum Shiny choice/state needed for NIR. Preserve exported helper signatures unless an independently useful argument is required.
- **Dependencies/OpenSpecy**: Add no dependency. Preserve `wavenumber`, spectra columns, metadata rows, names, processing attributes, and audit attributes through every split/filter/recombine operation.
- **Generated artifacts**: Update roxygen and regenerate with the configured R/roxygen toolchain; never edit `NAMESPACE` or `man/*.Rd` directly. Update `NEWS.md` and builder vignette/help.
- **Reference compatibility**: Compare axes, IDs, per-type counts, metadata, warnings, representative matches, and identical paired holdouts against all seven legacy `get_lib()` artifacts. Old models are inference-only comparators with unknown training membership.
- **Performance/observability**: Probe representative FTIR/Raman/NIR subsets first. Use matrix operations and one finite mask per aligned matrix where possible; no repeated `split_spec()` hot loop. Expect each full correlation/PAM kernel under 10 minutes and the complete rebuild below the prior 7h24m baseline; stop and isolate any silent stage over 15 minutes or any stage exceeding 2x its probe projection. Reuse checksum-compatible checkpoints.
- **Bundled/hosted app**: Keep one canonical analysis state. Type-specific real libraries are selected directly or combined on their union axis for `all`; typed models run only where their axes overlap and the best confidence is retained per query. Preview may materialize staged input but never publishes canonical Run results. Shared `R/` and `inst/shiny/` changes require fast `-HostedAppStatic`; staged artifact routing requires exact-artifact preflight. A full wasm rebuild is deferred unless pins/dependencies/images change.
- **Pipeline diagrams**: Update `.specify/memory/build-lib-diagram.html` for both processed branches entering type splits/medoids/models and `.specify/memory/pipeline-diagram.html` for all-type/NIR identification and pre-Run preview materialization. Keep the established HTML/constitution style.

## Package Surfaces And Work Checklist

- [ ] `R/assess_spec.R`, `R/adj_range.R`, and tests: finite-support high-tail/CO2 metrics, selective correction, and typed stable results.
- [ ] `R/build_lib.R` and tests: quality gates, typed ranges/metadata, NIR artifacts, shared paired cohorts, macro-first numeric assessments, warnings schema, progress, checkpoints, and compatibility.
- [ ] `R/match_spec.R`, `inst/shiny`, and app tests: NA-tolerant package identification path plus inferred/explicit NIR selection without breaking independent processing/identification/quantification states.
- [ ] Roxygen/vignette/NEWS and both canonical HTML diagrams: document ranges, thresholds, artifact shape, metrics, and workflow.
- [ ] Benchmark and staged probe: demonstrate mask/matrix behavior, representative accuracy/coverage, runtime projection, checkpoint restart, and stop thresholds before production scale.
- [ ] Focused tests, documentation, full tests, fast hosted static gate, and matching-artifact app preflight pass on the final candidate.
- [ ] Run the complete build from explicit workflow paths, inspect every assessment/output manifest table, compare against legacy artifacts, and reconcile processes/checklist/root scratch cleanup.

## Verification

- Focused: Windows Rscript preflight; `devtools::test(filter = "assess_spec|adj_range|build_lib|match_spec|shiny", reporter = "check", stop_on_failure = TRUE)` plus exact schema/type, same-ID cohort, finite-tail, correction/drop, SNR, ranges, missing-value inference, and NIR routing assertions.
- Benchmark/staging: fixed-seed representative type subsets in ignored/temp output; record dimensions, elapsed/RSS, removals, medoid/model coverage, macro/overall accuracy, and reuse evidence. Compare same-output kernels to retained benchmark code and flag >10% regressions.
- Final: configured `devtools::document()` with generated-diff inspection, vignette render, one full `devtools::test()`, fast `-HostedAppStatic`, exact staged-artifact preflight, then the complete checkpointed build and artifact reload/assessment review. Release-only R CMD check and wasm rebuild remain deferred unless their triggers change.
- Closure: confirm no owned process remains, `git status` is understood, all checklist boxes have evidence or explicit deferral, and task-created root scratch files are removed.

## Risks And Open Questions

- The useful NIR common interval depends on actual finite coverage; derive and record it during the probe instead of hard-coding an unsupported subrange.
- Legacy artifacts are not type-nested, so compatibility adapters/tests must prevent ambiguous callers from treating the new containers as a single `OpenSpecy` object.

## Approval Notes

- Approved by: user request, 2026-09-01.
- Follow-up: publishing/replacing hosted reference artifacts remains maintainer-owned.
