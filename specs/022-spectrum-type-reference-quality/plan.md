# Feature Plan: Spectrum-Type Medoid and Model Completion

**Feature dir**: `specs/022-spectrum-type-reference-quality`
**Date**: 2026-09-02
**Current tranche**: Reuse completed derivative/nobaseline libraries to rebuild NA-tolerant medoids, macro-selected models, and source-local assessments without rerunning ingestion or processing.
**Change class**: package/scientific

## Goal

- Retain partially covered spectra in every spectrum-type medoid/model workflow when at least 10% of that type's identification interval is observed.
- Make medoid selection computationally complete without erasing genuine missingness from the published medoids, then use explicit wavenumber-wise training fills for models.
- Assess candidate artifacts on candidate data and legacy artifacts on legacy data so taxonomy evolution does not require fuzzy cross-version class matching.

## Scope

- **In**: downstream rebuild API; support filtering; spectrum-wise PAM fill; original-value medoid restoration; wavenumber-wise model fill; macro-accuracy lambda selection; source-local 10% tests; typed support/coverage audits; progress/checkpoints; tests, docs, NEWS, benchmark, and a downstream production rerun from the completed 2026-09-01 libraries.
- **Out**: source ingestion, derivative/baseline processing, quality gates, class reassignment, Shiny changes, legacy-model retraining, forced inclusion of sparse classes, fuzzy class matching, and changes to calibrated alpha/intercept/grouped/weight settings.
- **Users**: maintainers pass an existing completed build/checkpoint and an explicit new output directory; package consumers receive the unchanged four-part build shape.

## Requirements

- R1. For each derivative/nobaseline spectrum type, restrict to its identification range and retain spectra with finite values at no fewer than `ceiling(0.10 * n_wavenumbers)` positions. Record observed positions/fraction and retained/dropped status in assessments.
- R2. PAM receives one same-axis matrix per group. Fill each spectrum's missing values with that spectrum's finite mean using `mean_replace()`, normalize it, represent a genuinely flat normalized spectrum as zero, and use optimized `cor_spec()` plus deterministic `cluster::pam(variant = "faster")` without block/chunk paths when the complete matrix fits.
- R3. After PAM chooses identifiers, construct the published medoid object by selecting those identifiers from the original unfilled range-restricted library. Preserve original `NA` positions, identifiers, metadata alignment, range, and support audit attributes.
- R4. Before model fitting, replace each missing training value with the finite mean at that wavenumber across restored medoids. Remove complete-case filtering; fail clearly only when a wavenumber has no finite training values or fewer than two classes meet `min_n = 10` after the 10% support gate.
- R5. Keep the calibrated model policy unchanged: `alpha = 0.1`, `intercept = FALSE`, grouped multinomial coefficients, inverse-frequency class weights, relative normalization, five-or-fewer stratified folds, and existing class/type labels. Do not add data-poor classes solely to improve coverage.
- R6. Select lambda from out-of-fold predictions by maximum mean class-wise accuracy. Report the per-lambda macro/overall accuracy and tie rule; never select on the untouched artifact assessment sample.
- R7. Candidate full libraries, medoids, and models use a reproducible approximately 10% class/type-stratified sample from the candidate full library; legacy equivalents independently use the legacy full library. For reference/medoid matching, remove selected query identifiers from the corresponding reference before identification to prevent exact self-matches.
- R8. Compare expected and predicted classes exactly within each source's own taxonomy; do not fuzzy-map classes across versions. Report candidate and legacy metrics as source-local results with sample counts and provenance, not as paired causal deltas or equal-cohort claims.
- R9. Add exported `rebuild_lib_artifacts(x, output_dir, previous_library_dir = "system", reuse = TRUE, seed = 123, holdout = 0.1, progress = TRUE)`. `x` accepts a completed build object, its RDS file, or an output/checkpoint directory; it reuses `libraries` and upstream assessments, checkpoints each downstream artifact, writes a versioned release, and returns `libraries`, `medoids`, `models`, and `assessments`.
- R10. Progress reports type/range dimensions, finite-support removals, fill counts, PAM/model elapsed time, lambda result, source-local train/test counts, checkpoint writes/reuse, and release output. Stable typed empty assessment schemas remain valid.
- R11. Benchmark representative Raman, FTIR, and NIR NA patterns. Report complete-case versus supported/imputed class coverage, macro/overall accuracy, runtime, and memory-relevant dimensions; propose but do not adopt alternate calibrated model settings without maintainer approval.

## Technical Decisions

- **Object flow**: completed build/path -> validated nested `OpenSpecy` libraries -> 10%-support type slices -> spectrum-mean-filled PAM working copy -> IDs -> original-NA medoids -> wavenumber-mean-filled training matrix -> unchanged glmnet policy -> source-local assessments -> checkpointed four-part build.
- **Public API review**: `x` and `output_dir` are required inputs; legacy location, restart behavior, deterministic split, assessment fraction, and progress are demonstrated policies. Support/fill/range/model calibration are inferred from the official workflow. No downstream tuning is exposed and the return type/side effects mirror `build_lib()` official mode.
- **Assessment interpretation**: independent source-local cohorts improve robustness to changing taxonomies but do not isolate library-version effects. Each summary must expose its denominator and class support; compatibility tables still report IDs, axes, and metadata differences.
- **Performance envelope**: first probe representative type/group kernels. Full correlation/PAM kernels should remain under 10 minutes each; stop at a checkpoint and isolate any silent stage over 15 minutes or over 2x its probe projection. Do not chunk absent measured memory pressure.
- **Generated/docs**: update roxygen and regenerate `NAMESPACE`/`man` only with configured R 4.3.3 tooling; inspect generated diffs immediately. Update `vignettes/library-builder.Rmd` and `NEWS.md`.
- **Hosted impact**: shared `R/` source changes, so run the fast `-HostedAppStatic` gate. No app/runtime/route/assembly, dependency, image, driver, or pin change; matching-artifact preflight and clean wasm rebuild are N/A.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`: implement the downstream rebuild API, 10% support gate, spectrum-wise PAM working fill with original-NA restoration, wavenumber-wise model fill, and macro-accuracy lambda selection.
- [x] `R/build_lib.R`: replace paired/fuzzy comparison behavior with independently stratified candidate/legacy tests, medoid self-match exclusion, typed coverage audits, progress, and checkpoint versioning.
- [x] `tests/testthat/test-build_lib.R`: cover input resolution, reuse, support threshold, fill orientation, restored NAs, calibrated-setting invariants, lambda selection, source-local cohorts, exact labels, and stable schemas.
- [x] `benchmarks/`: compare complete-case/current selection with supported/imputed macro selection on representative typed NA patterns and flag material runtime regressions.
- [x] Roxygen, generated docs, `vignettes/library-builder.Rmd`, `NEWS.md`: document the downstream workflow and scientific interpretation; inspect generated diffs.
- [x] Run subset probes and the downstream production rebuild from the completed derivative/nobaseline checkpoints; review medoid/model coverage, macro/overall accuracy, compatibility, warnings, ranges, IDs, metadata, checkpoint restart, and output reload.
- [x] Focused tests, benchmark, documentation/vignette validation, full tests, and fast hosted static gate pass on the final candidate; reconcile processes, scratch files, status, and every checkbox.

## Verification

- Focused: configured Windows Rscript preflight and `devtools::test(filter = "build_lib|match_spec", reporter = "check", stop_on_failure = TRUE)` with direct object/alignment/schema assertions.
- Staged: fixed-seed Raman/FTIR/NIR probes in a task-specific ignored/temp evidence directory; record matrices, NA/support counts, elapsed time, chosen lambda metrics, and checkpoint reuse before production scale.
- Final: run benchmark; `devtools::document()` and generated-diff inspection; vignette render; one full `devtools::test()`; fast `-HostedAppStatic`; downstream full rebuild/reload and legacy compatibility review. R CMD check, hosted artifact preflight, and wasm rebuild are deferred because this tranche is not release/runtime/dependency facing.

## Risks And Open Questions

- Classes below `min_n = 10` remain deliberately unsupported until more training data arrive; coverage tables must distinguish this from prediction failure.
- A spectrum with 10% finite support can still have weak identification evidence; retain score/support together so users can interpret such predictions.

## Approval Notes

- Approved by user request, 2026-09-02. Publishing or replacing hosted/downloaded artifacts remains maintainer-owned.
