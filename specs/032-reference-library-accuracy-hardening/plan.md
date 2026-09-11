# Feature Plan: Leakage-Free Reference-Library Accuracy Hardening

**Feature dir**: `specs/032-reference-library-accuracy-hardening`  
**Date**: 2026-09-10  
**Status**: Implemented and under final checkpointed production verification.
**Current tranche**: Make official `build_lib()` accuracy evidence independent, interpretable, complete, and reproducible before tuning or publishing another library.  
**Change class**: package/scientific.
**Audit target**: `C:\Users\winco\OneDrive\Documents\OpenSpecy_offline\reference-library-build\releases\75b72a79d864\reference_library_build.rds` (SHA-256 `c4e80dc4778e0a839c41d6692a84279f81b3b63b863b741bae19eab7e29b24ed`).

## Current Assessment

- Baseline `75b72a79d864` is a 381,470,649-byte aggregate with 15 aligned full/medoid `OpenSpecy` artifacts.
- Derivative/nobaseline full-library matching reports 93.75-100% macro and 99.84-100% overall accuracy, but only as closed-set `material_class` survivor consistency: splitting follows label-informed `prune_lib()` removal. This is not external identification accuracy.
- Typed logistic macro accuracy is 95.49/92.85/98.86% for derivative FTIR/Raman/NIR and 88.81/97.84/99.55% for nobaseline, but production models include 594/58/11 and 611/62/20 exact test spectra. Removing overlap leaves both Raman tests mineral-only.
- RF failed: `ranger` was unavailable, all nine fits were skipped, and three promoted files are empty. Fifteen `glmnet` convergence notices remain; several FTIR/combined fits select the final lambda.
- Of 68,241 deduplicated rows, 7,057 remained unmatched and 8,467 missing-identity/generic rows were removed. `classes_reference.csv` contains the Excel mutation `4-5` -> `5-Apr`.
- Evaluation support is thin: 12/16 processed Raman and 5/42 FTIR classes have fewer than five test groups; raw Raman leaves 16/37 untested. Current `coverage` means non-missing predictions only.
- ID grouping misses transformed duplicates: train contains an exact spectral duplicate for 299/2,056 raw and 133/2,026 derivative Raman test queries (plus five FTIR/nobaseline queries); several processed/medoid spectra are exactly flat zero.
- A hard-coded unanchored `(7_b1)` exclusion also matched `7_b10`-`7_b12`, silently removing 31 valid FTIR plastic spectra; the saved filter audit is empty. CO2 flattening leaves 1,189 derivative and 300 nobaseline FTIR postcondition failures in final libraries.
- Compatibility/provenance is incomplete: NIR uses 1,334 points versus the legacy 1,333, only 451/891 raw IDs are shared, and manifests lack payload hashes, full code/dependency provenance, and immutability.
- `assessments` currently exposes 31 flat entries with empty placeholders, repeated diagnostics, and old/new rows stacked vertically, making the process sequence and comparisons unnecessarily difficult to review.

## Goal And Scope

- Produce reproducible accuracy and coverage evidence for references, medoids, logistic models, and random forests before changing scientific policy.
- **In**: train-only evaluation; grouped holdouts; compact process-nested assessments; uncertainty/coverage; completeness, lookup, and provenance gates; staged comparison.
- **Out**: fuzzy or automatic chemical assignments, app model selection, changing CO2/SNR/pruning thresholds or model tuning before the evaluator is valid, publishing/replacing external artifacts, and a full source rebuild when unchanged core checkpoints remain demonstrably reusable.
- **Users**: maintainers run the unchanged official builder/rebuilder interfaces and receive a release that either passes explicit acceptance checks or fails before promotion.

## Requirements

- R1. Freeze physical/content groups and evaluation membership before cross-spectrum or label-informed pruning, reduction, medoid selection, or fit. Every learned/selected artifact uses training groups only; assert zero spectrum/group overlap for every test.
- R2. Refit logistic and RF assessments on identical training folds with grouped inner tuning. Persist row-level tests, group sizes, hashes, seeds, grouped 95% intervals, coverage/calibration/rejection, and a locked contributor/instrument or expert-labelled final test; mark fewer than five independent groups insufficient.
- R3. Replace 31 flat entries with five ordered, nonempty process lists and at most 10 leaf tables: `cleanup(summary, dropped_spectrum_identities)`, `ref_lib(accuracy, confusion)`, `medoid(accuracy, confusion)`, `model(accuracy, confusion, diagnostics)`, and `functionality(comparison)`. The dropped table contains only sorted distinct `spectrum_identity` values; omit empty/orphaned leaves.
- R4. Present every old/new comparison on one row with adjacent `{metric}_old`, `{metric}_new`, and `{metric}_shift` columns. Sort accuracy by `macro_class_accuracy_new`, misidentified confusion rows by `spectra_new`, and model diagnostics by `absolute_correlation`, all descending with old-value fallbacks and missing values last.
- R5. In `functionality$comparison`, merge `assess_spec` error and warning counts/rates, exclude passes, and sort by `rate_shift` descending. Pivot `old_new_compatibility` to one wide row per artifact with old/new metric columns adjacent.
- R6. Official mode must fail promotion for missing/empty models, invalid objects/axes/fills/schema, flat spectra, leakage, non-convergence, failed QC, or rows without one terminal conservation reason. Approved warnings remain explicit.
- R7. Apply exact mappings before regex; reject duplicate/blank keys, clashes, date coercions, broad known-bad-ID patterns, and unreviewed changes. Rank unmatched/below-`min_n` queues and leave ambiguous identities unmatched.
- R8. Compare old/new on shared-ID/shared-class queries or label cohorts composition-only; explain NIR changes and round-trip every standalone/aggregate model and `OpenSpecy` artifact through `match_spec()`.
- R9. Make releases immutable with SHA-256 for inputs, curated tables, code/package/dependencies, arguments, checkpoints, and payloads; existing paths must match the manifest or fail.
- R10. Compare typed full-reference, medoid, logistic, and RF routes on common leakage-free cohorts. Keep combined models compatibility-only unless they beat typed routes under paired criteria; correct clustered data/label errors before tuning.

## Technical Decisions

- **Object flow**: normalized/deduplicated core `OpenSpecy` -> frozen group/domain splits -> train-only prune/reduce/medoid/model -> untouched query evaluation -> final production fit -> fail-closed promotion. Preserve axes, attributes, unique spectrum columns, and metadata row order at each boundary.
- **Public API**: keep function signatures unchanged, but intentionally replace the flat assessment return with schema-versioned process nesting. Normalize legacy builds when read; do not retain duplicate legacy aliases or empty compatibility placeholders.
- **Review versus evidence**: the 10-table assessment is the human surface. Row-level tests, split manifests, and release manifests remain hash-addressed machine evidence referenced from `functionality$comparison`, not extra review elements.
- **Functionality consolidation**: `functionality$comparison` uses `assessment_kind` to separate wide quality-shift, compatibility, integrity, and release rows without more leaves.
- **Model policy**: do not tune alpha, thresholds, or default routes against `75b72a79d864`. First reproduce all algorithms with identical folds; select logistic medoids independently inside each training fold so assessment matches the deployed model route, while RF retains full training spectra. Typed derivative FTIR and typed nobaseline Raman are provisional comparison leaders, not validated recommendations.
- **Dependencies/generated files**: keep `ranger` in `DESCRIPTION` `Suggests` but require it for official RF builds. Update roxygen/vignette/`NEWS.md` and regenerate `NAMESPACE`/`man/*.Rd` only with roxygen2 8.0.0; `.github/workflows/` is N/A unless CI enforcement is approved.
- **Performance/observability**: preflight <2 minutes/<2 GiB; probe 1,000 groups/type. Batch artifact/SNR/pruning kernels, stage full recipes on disk, consume partitions in place, and use exact PAM through 3,000 spectra or five deterministic 1,000-spectrum PAM samples scored on the full group above that bound. Checkpoint each fold/recipe/type/algorithm; enforce a 12 GiB resident ceiling and stop any non-checkpointed stage at 60 minutes or >2x projection.
- **Checkpoint/release lifecycle**: materialize only the current old/new artifact or model partition, retain compact metrics, and release memory between stages. Keep scientific checkpoints independently reusable, but derive the promoted release signature from the assessment signature plus current package source, git state, runtime, and dependencies. Existing release bytes are reusable only through a completed same-signature manifest whose recorded size and SHA-256 still match.
- **Bundled Shiny / pipeline diagram**: this scientific tranche does not alter the app pipeline. A later maintainer-requested maintenance extension removes the guided walkthrough, updates its diagram annotation, and fixes the hosted busy-state smoke assertion without changing spectral processing.
- **Hosted Shinylive/WebAssembly**: `R/` and docs trigger fast `-HostedAppStatic`; exact-artifact preflight applies only to staged replacements, and clean wasm is N/A unless dependency/pin/release scope changes.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`: implement stable splits, fold-local builders, the five-process/10-table assessment assembler and sort/pivot rules, exact exclusions, QC/conservation, fail-closed gates, and cryptographic manifests.
- [x] `tests/testthat/test-build_lib.R`: prove hierarchy/leaf-count/order, no empty leaves, dropped-identity schema, wide old/new adjacency, required sorts, no leakage, grouped folds, failure gates, immutability, and round trips.
- [x] `workflows/data/classes_reference.csv`, `classes_regex.csv`, and new `known_bad_ids.csv`: restore `4-5`, guard date coercion, replace broad exclusions with reviewed exact IDs, and report all mapping/removal denominators.
- [x] `benchmarks/reference_library_validation.R` and `reference_library_raman_alpha.R`: consume the new paths, verify sort/pivot invariants, machine-readable bounds, common cohorts, calibration, and staged resource guards.
- [x] `R/build_lib.R`, `vignettes/library-builder.Rmd`, `NEWS.md`, and `.specify/memory/build-lib-diagram.html`: document schema migration, meanings, limitations, provenance, failure policy, and review sequence; regenerate expected help only.
- [ ] Run focused checks, probe folds, full tests, hosted static, then one checkpointed downstream/full candidate into a new external output root; compare IDs, axes, metadata, warnings, hashes, and representative matches to `75b72a79d864` without publishing.
- [ ] Reconcile every checkbox with evidence; record deferred external/release gates, stop or record owned processes, inspect `git status`, and remove task scratch.

## Verification

- Focused: configured Windows R 4.3.3 x64 `devtools::test(filter = "build_lib|match_spec", reporter = "check", stop_on_failure = TRUE)` plus CSV duplicate/blank/date-pattern and `predict_class_reference(..., return = "report")` audits.
- Statistical acceptance: zero train/test ID/group overlap; every route present; no headline for <5 groups; deterministic folds; intervals/calibration recompute from retained rows; paired results share queries and labels.
- Review-schema acceptance: exactly five ordered process groups and no more than 10 nonempty leaves; one-column dropped identities; paired suffix columns adjacent; accuracy, confusion, correlation, and rate-shift orders remain stable after serialization; no pass rows in quality shifts.
- Staged workflow: audit current RDS read-only, run split-only and 1,000-group probes to temporary output, then reuse only hash-verified core checkpoints. A fresh full run is triggered by changed labels, preprocessing, source inputs, or unverifiable checkpoints.
- Final candidate: benchmark, full `devtools::test()`, roxygen2 8.0.0 documentation/diff review, vignette render, and fast `-HostedAppStatic`; defer `devtools::check()` unless release/CRAN-facing.
- CI maintenance: make seeded ranger fitting deterministic and verify completed release bytes by signed manifest so R-devel does not treat backend serialization state as scientific drift. Collapse only verified case-only pkgdown redirect aliases during offline staging; all substantive portable-path collisions remain fatal. Offline browser acceptance validates server-owned selection metadata rather than a theme-specific DT row class.
- Release acceptance: hashes/objects agree, models predict after reload, NIR differences are explained, and no unmanifested payload exists. External and hosted release gates wait for their inputs.
- Reusable evidence: this read-only audit covers only `75b72a79d864` and current assessment code; any artifact, lookup, dependency, builder, or validation-contract change invalidates it.

## Risks And Open Questions

- A genuinely independent labelled corpus may not yet exist; contributor-held-out validation is the minimum fallback, not a substitute for external field spectra and open-set negatives.
- Decide promotion thresholds only after the leakage-free baseline exposes attainable intervals; until then, structural completeness is a hard gate and accuracy is reported without an unsupported pass/fail cutoff.
- Exact class labels are assumed ground truth in current data. Maintainer/domain review is required for changed mappings and the highest-volume confusion/removal groups.

## Approval Notes

- Approved by the maintainer for implementation and a local external rebuild on 2026-09-10. Publishing remains maintainer-owned and out of scope.
