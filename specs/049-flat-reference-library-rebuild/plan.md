# Feature Plan: Flat Reference-Library Rebuild

**Feature dir**: `specs/049-flat-reference-library-rebuild`  
**Date**: 2026-10-15  
**Current tranche**: Rebuild the official library from the latest local reference sources without cross-class 0.9 closure, fit the flat classifier, and return the standard `build_lib()` assessment.  
**Change class**: Package/scientific behavior plus a long-running external reference-library workflow.

## Goal

- Produce a staged, reproducible `build_lib()` release from all 36 current processed sources plus `library_raw.rds`.
- Retain ordinary within-class pruning and generic reassignment, but disable the cross-class correlation exclusion/closure policy and fit only the flat guardrailed logistic classifier.

## Scope

- **In**: explicit official model selection; method-keyed signatures/checkpoints; driver configuration; subset/preflight evidence; full staged rebuild; standard cleanup/reference/medoid/model/comparison assessment; old/new artifact compatibility.
- **Out**: positive-control map tests; taxonomy edits; classifier tuning; artifact publication or package-library staging; remote synchronization; app changes; R CMD check.
- **Users**: maintainers running `workflows/OpenSpecy_reference_library.R` and reviewers consuming the saved release and `assessments.rds`.

## Requirements

- R1. `build_lib(..., output_dir=)` accepts one official `model_method`: the compatibility default remains strict gated hierarchical logistic regression, while `"logistic_regression"` creates the flat model and historical flat aliases.
- R2. Model choice participates in build/artifact signatures, checkpoint names, validation, release manifests, and standard assessment so incompatible model artifacts cannot be reused.
- R3. The workflow passes `cross_class = FALSE` for derivative and no-baseline pruning. This disables the current strict `> 0.9` cross-class conflict removal and final closure while preserving other pruning/reassignment behavior.
- R4. The production run uses the newest locally available configured source set (36 processed RDS files; newest modified 2026-10-07, plus raw) and writes to a new staged output without replacing or publishing an existing release.
- R5. Return the standard `build_lib()` assessment and compatibility evidence; do not run either positive-control benchmark.

## Technical Decisions

- **Approach/API**: `model_method` is a demonstrated official-build policy with two current uses, not a speculative flag. It is ignored nowhere, accepts one supported artifact method, and does not change composable source-only builds. Existing `prune` owns the correlation policy.
- **OpenSpecy contract**: every saved library/medoid retains aligned `wavenumber`, `spectra`, `metadata`, unique IDs, attributes, and typed partitions; flat model inputs use unchanged medoids and identification ranges.
- **Generated artifacts**: update roxygen/vignette/NEWS, regenerate with configured roxygen2 8.0.0, and inspect generated diffs; never edit `man/` or `NAMESPACE` directly.
- **Reference compatibility**: compare IDs, axes, spectra, metadata names/counts, warnings, retention/quarantine, model schema, and representative matches against the prior/system artifacts. The changed closure policy intentionally changes survivor counts.
- **Performance/observability**: probe the driver and a bounded source subset first. Production progress boundaries are core build, pruning, typed closure, medoids, flat fits, assessments, and promotion; checkpoints are reusable only for an identical signature. Use the measured ~42,096 x 600 FTIR closure kernel as the risk envelope, target <=2 hours correlation work and <=8 hours total, review at 4 hours, and stop/restart at >80% available memory, >2x a measured projection, or 10 minutes without stage progress.
- **Bundled Shiny/pipeline**: N/A; `inst/` and `.specify/memory/pipeline-diagram.html` are untouched.
- **Hosted impact**: `R/build_lib.R` is a shared hosted input, so run fast `-HostedAppStatic`. No runtime, route, assembly, dependency, image, driver, pin, or staged hosted-library change; matching-artifact and clean wasm tiers are N/A.

## Package Surfaces

- `R/build_lib.R`, `tests/testthat/test-build_lib.R`: official model policy, signatures, checkpoints, validation, and focused regressions.
- `workflows/OpenSpecy_reference_library.R`: explicit flat method, cross-class closure disabled, staged output supplied by environment; source discovery remains unchanged.
- `vignettes/library-builder.Rmd`, `NEWS.md`, generated `man/build_lib.Rd`: document the official policy. `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README/pkgdown: unchanged.
- `benchmarks/`: no same-output performance implementation; use the existing standard post-build audit only. Positive-control scripts are explicitly deferred.

## Work Checklist

- [x] Implement and test method selection/signature/checkpoint handling in `R/build_lib.R` and `tests/testthat/test-build_lib.R`.
- [x] Configure and preflight `workflows/OpenSpecy_reference_library.R` for flat-only modeling and `cross_class = FALSE`; update source docs and generated help.
- [x] Run focused tests and `-HostedAppStatic`, then a bounded source/reuse probe with recorded dimensions, time, memory, and checkpoint behavior.
- [x] Run the full build to a new staged external output and monitor the declared stop/restart thresholds through completion.
- [x] Run the standard saved-release assessment/audit, summarize compatibility, retention, warnings, class/model results, and representative matches; leave positive controls deferred.
- [x] Reconcile evidence, owned processes, `git status`, and scratch/output retention before handoff.

## Verification

- Focused: compact `devtools::test(filter = "build_lib", reporter = "summary")`; driver parse/single-call assertions; method-specific checkpoint rejection and flat release-alias checks.
- Documentation: confirm roxygen2 8.0.0, run `devtools::document()`, inspect only expected usage/argument changes; one full `devtools::test()` follows package code changes.
- Long workflow: latest-source inventory, bounded probe, full run, `assessments.rds`, release manifest, and `benchmarks/reference_release_postbuild_audit.R` where its topology assertions support the selected flat release; otherwise use the equivalent standard assessment tables without altering the benchmark solely to force gated expectations.
- Hosted: run `quality-gates.ps1 -HostedAppStatic`; R CMD check and all browser/wasm rebuild tiers are deferred by scope.
- Reusable evidence: 048 correlation-kernel equivalence and package gates remain valid until their covered code/contracts change; this tranche invalidates official model orchestration/docs and release-output evidence only.

## Risks And Open Questions

- The user described the “< 0.9 exclusion rule”; the owning implementation removes cross-class conflicts strictly above 0.9. This plan disables that entire cross-class rule, which is the only build-library 0.9 exclusion policy found.
- External H: source latency and the extra 2026-10-07 processed file may change projections; the bounded probe must re-baseline before production.

## Result Evidence

- Build signature `a1b961ef21a5a612bebc88607f40f5165617d40457d849785c17368a0fd23d48`; staged release `reference-library-build-flat-no-closure-20261015/releases/a1b961ef21a5`; total runtime 20,259.5 seconds (5h 37m 39.5s), within the eight-hour threshold and healthy at the four-hour review.
- All 15 raw/processed/medoid partitions validate as `OpenSpecy`; the empty quarantine contains zero spectra and conflicts. Cross-class conflicts, cross-class removals, threshold removals, and correlation-closure records are all zero.
- All eight flat logistic models trained with complete lambda paths and converged selections: derivative/nobaseline Raman 17 classes, FTIR 45, NIR 4, and combined 62. The compatibility aliases are checksum-identical to their flat model files.
- Standard assessment is saved in `assessments.rds`: 18 reference, 12 medoid, and 20 model accuracy rows; 70 functionality comparisons; 15 old/new compatibility rows; zero saved assessment warnings. Positive-control workflows were not run.
- Package evidence: focused `build_lib` tests 830 passes; full tests 4,813 passes, 30 expected warning assertions, two guarded AWS skips; HostedAppStatic 393 passes; documentation regenerated with roxygen2 8.0.0 and expected `man/build_lib.Rd` changes only.

## Approval Notes

- Approved by: maintainer request on 2026-10-15 for the flat, no-0.9-closure rebuild and standard assessment only.
- Follow-up: artifact publication/staging remains a separate maintainer decision.

## Maintainer Follow-up: Positive Controls

- **Authorized 2026-10-16**: run the frozen 20-map positive-control cohort against only the new derivative FTIR full library, medoid library, and flat model; do not tune or publish from these holdout outcomes.
- [x] Verify frozen input/truth hashes and run all three artifacts through a probe of the three predeclared maps.
- [x] Complete all 20 eligible maps per artifact with restartable checkpoints, under five minutes per map and below 80% physical memory.
- [x] Compare specific and plastic/non-plastic accuracy with the frozen OS1 and prior-current counterparts; retain manifests/results externally and reconcile processes/workspace.
- **Evidence**: `Positive_Controls/flat_no_closure_validation_20261016`; 60/60 maps, zero warnings, 99.73-second slowest map, 40.08% maximum system-memory use. Mean specific/broad accuracy was 93.945/95.352% full, 91.657/94.169% medoid, and 83.653/92.876% model. The paired model-specific delta versus OS1 was -10.345 points (-19.565 to -3.335); no tuning or publication followed.
