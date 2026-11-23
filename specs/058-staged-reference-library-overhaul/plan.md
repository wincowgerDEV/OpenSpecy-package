# Feature Plan: Staged Reference-Library Overhaul

**Feature dir**: `specs/058-staged-reference-library-overhaul`  
**Date**: 2026-11-21  
**Current tranche**: Validate the updatedv1 cohort, build each admitted dataset independently through complete raw/derivative/nobaseline libraries, and join only those finished artifacts into one unified library bundle.  
**Change class**: hosted/release (package/scientific builder behavior plus official external artifacts)

## Goal And Scope

- Produce complete unified raw, derivative, and no-baseline reference libraries from the 68 admitted `rawrefdata/updatedv1/datasets.csv` sources, including 19 non-legacy additions and 49 legacy sources.
- Build and checkpoint one dataset at a time so source-local cleaning/removal is paid once and no all-source spectral object exists before the final bounded join.
- Out of scope: the two planned, one rejected, and two reviewed-but-not-admitted manifest rows; medoid selection; model fitting; positive-control tuning; hosted library pinning; publication/upload; remote synchronization.

## Requirements

- R1. Admit only exact manifest-selected `<dataset>/Processed/CleanedOS_raw.rds` files whose cleaner/output hashes, type/units/transmission semantics, approved diagnostic, warning-free `check_OpenSpecy()`, serialization recheck, provenance/license distinction, and class coverage pass.
- R2. `build_lib()` supports a checkpointed library-only artifact scope that returns/promotes raw, derivative, nobaseline, assessments, and quarantine without executing medoid, model, or legacy-comparison stages; the existing complete default stays compatible.
- R3. Each dataset writes immutable recipe/type RDS artifacts plus a hash/count manifest under its existing `Processed/` folder; retries reuse only signature-compatible completed work.
- R4. Source-local policy is exact-content hashes followed by deterministic same-source/type/identity correlation deduplication strictly `> 0.99`; derivative/nobaseline ordinary pruning and generic reassignment are dataset-local with `cross_class = FALSE`, `min_n = 10`, while raw is unpruned.
- R5. The final join retains every surviving per-dataset record: no cross-dataset correlation pruning, class closure, generic reassignment, model-range filtering, medoid, or model work is allowed during assembly.
- R6. All outputs remain valid aligned `OpenSpecy` objects with unique IDs, preserved axes/values/metadata/attributes, and every removal balanced to quarantine with stable source, stage, rule, reason, and comparison evidence.

## Technical Decisions

- **API**: add `artifact_scope = c("complete", "libraries")` to `build_lib()`. It is a current policy choice, defaults to the existing complete workflow, and keeps downstream medoid/model creation composable through `rebuild_lib_artifacts()`; no new export is needed.
- **Per-dataset flow**: `CleanedOS_raw.rds |> build_lib(output_dir, artifact_scope = "libraries", previous_library_dir = NULL, reuse = TRUE, remove_other = TRUE, prune = list(derivative = list(cross_class = FALSE, min_n = 10L), nobaseline = list(cross_class = FALSE, min_n = 10L)))`; promote one recipe/type at a time and release resident matrices between datasets.
- **Final flow**: read only manifested per-dataset recipe/type paths; assemble each recipe/type independently in source order using retained-only 5,000-column file-backed blocks; preserve every survivor and resolve only cross-dataset ID collisions by qualifying the colliding `sample_name` with its dataset slug while retaining `source_dataset` and `source_sample_name`; validate and serialize one final artifact at a time, then write a signed aggregate bundle/index under `rawrefdata/updatedv1/Processed/`.
- **Freeze boundary**: hash the manifest, 68 cleaner/raw/output files, curated lookup CSVs, consumed R/workflow code, comparator release `../reference-library-build-bounded-prune-reassignment-20261119-r1/releases/57da878d87b2`, arguments, runtime, and output target before production; any changed consumed hash invalidates reuse.
- **Bounded contract**: hash blocks 20,000; correlation blocks at most 256 queries by 5,000 candidates; source/final merge and recipe blocks 5,000; metadata joins 100,000 rows; diagnostics at most 1,000 representative values with exact aggregate/collapsed counts; GC every 10 blocks and each source boundary; staged completion trigger 250,000 spectra.
- **Storage**: source-prefilter quarantine first, retained spectra only, global exclusions planned before any metadata union, retained-row-only binding, by-reference blank removal, lossless metadata compaction/public expansion, no full metadata copies, and direct temporary HDF5 merged targets; remove temporary cores after durable recipe closure or failure.
- **Exact pruning fast paths**: after generic reassignment, skip the bounded best-match scan when every active candidate in a pool already has the target class. For sparse opposing classes, find exact different-class leaders first, discharge queries as soon as a same/unknown-class witness wins beyond tie tolerance, preserve deterministic candidate-order ties, and rescore only winning conflicts in the legacy bounded tile; never repeat the full all-candidate scan. Record blocks, early retention, conflicts, and avoided allocation; retain the legacy all-pairs kernel in the benchmark.
- **Stop rules**: stop at the next durable dataset/checkpoint above 80% RAM, below 75 GiB free disk, above twice projected kernel runtime, after 10 minutes without progress, on a changed frozen hash, an unbounded square correlation allocation, a full metadata copy, or any disposition imbalance; diagnose and restart into a fresh target.
- **Compatibility**: compare overlapping IDs, axes, values (bit-exact or predeclared machine tolerance), metadata names/classes/counts, warnings, attributes, and representative `c_spec()`/matching with release `57da878d87b2`; expected additions/removals remain explicit rather than treated as failures.
- **Generated/hosted impact**: update roxygen/vignette/NEWS and regenerate with configured R 4.3.3/roxygen2; update `.specify/memory/build-lib-diagram.html`. `R/` is shared hosted input, so run fast `-HostedAppStatic`; no hosted runtime/route/pin/dependency artifact changes, matching-artifact preflight, or wasm rebuild.

## Package Surfaces

- `R/build_lib.R`, `R/build_lib_staging.R`: library-only completion/promotion, signatures, return validation, and no-medoid/no-model enforcement.
- `tests/testthat/test-build_lib-library-only.R`, `tests/testthat/test-reference-library-staging.R`, `tests/testthat/test-reference-intake.R`: default compatibility, library-only ordinary and forced-file-backed paths, UTF-8 class keys, warning-free intake, exact outputs, quarantine balance, no downstream checkpoints, and final-join fixtures.
- `workflows/OpenSpecy_reference_library.R` plus a bounded updatedv1 staging driver: per-dataset resume, manifests, final join, telemetry, audit; no arbitrary cleaner discovery.
- `benchmarks/`: allocation/runtime probe for per-dataset staging and final bounded join; N/A to old/new algorithm equivalence because this is a new artifact scope, while library output equivalence is asserted in tests.
- `vignettes/library-builder.Rmd`, `NEWS.md`, generated `man/build_lib.Rd`, `.specify/memory/build-lib-diagram.html`; `DESCRIPTION`, `NAMESPACE`, app/site/deployment sources unchanged except generated documentation if required.

## Work Checklist

- [x] Revalidate all admitted inputs and class coverage; record source failures and keep non-admitted rows excluded.
- [x] Implement and document `artifact_scope = "libraries"`, library-only promotion, and no-downstream checkpoints in `R/build_lib*.R`.
- [x] Add focused ordinary/file-backed/final-join tests and the production allocation probe; update workflow, vignette, NEWS, diagram, and generated docs.
- [x] Run focused tests/benchmark, `devtools::document()`, full tests, fast hosted static, and release-facing R CMD check once on the final package candidate; the pre-production broad evidence must be refreshed after the NA-gap correction.
- [x] Freeze the production contract; run representative measured/computed and largest-source probes; process all 68 datasets with checkpoint/log/resource monitoring.
- [x] Join the manifested per-dataset artifacts into unified raw/derivative/nobaseline outputs and run compatibility, disposition/quarantine, checksum, serialization, and postbuild audits.
- [x] Reconcile every checkbox with evidence; record publication as maintainer-owned, stop or record owned processes, inspect `git status`, and clean task scratch.

## Verification And Open Questions

- Direct behavior: library-only output equals the complete workflow's library checkpoint on identical fixtures, including forced HDF5 completion, while no medoid/model/legacy-comparison checkpoint or payload exists.
- Intake evidence: per dataset raw/clean counts/hashes, FTIR/Raman type, intensity/transmission semantics, approved plot, warning-free re-read, license distinction, class exact/regex/unmatched coverage, and source failures.
- Production evidence: per-dataset input/removed/retained counts; largest hash/correlation/merge/join blocks; modeled/staged bytes; pre-bind/retained rows; no-copy flags; final axes/NA envelopes/unique names; recipe/type counts and hashes; exact quarantine shard count/bytes/SHA-256 balance.
- Package gates: focused `build_lib` first, benchmark, configured documentation and diff review, full `devtools::test()`, `-HostedAppStatic`, then staged R CMD check because public builder behavior is release-facing. Reuse passing evidence until a covered file/input/contract changes.
- Intake/class evidence: 68/68 inputs valid, 2,038,474 spectra, zero source failures; 1,557,195 identities resolve to 81,478 exact plus 1,956,513 regex spectrum matches, 483 exclusions, zero unmatched, and zero clashes.
- Probe evidence: measured 237/237 retained (489.7 MiB peak), computed 112/115 retained (464.7 MiB peak), and largest-source 41/41 retained after bounded sampling (5.58 GiB observed sampling peak); all prohibited downstream-artifact checks passed.
- Pre-production package evidence: the affected `build_lib`, library-only, `process_spec`, intake, and staging tests pass after the NA-gap correction; the earlier deterministic benchmark, full-test, hosted-static, and staged R CMD check evidence is retained only as a baseline and will be refreshed on the final source candidate.
- Threshold/refreeze evidence: preserved r4 after QM9's output-inert 129,795-by-129,795 same-class scan exceeded the frozen progress/runtime stop. The zero-block short-circuit matches `prune_lib()` and reduced a 1,000-spectrum legacy scan from 0.78--0.98 seconds to zero while avoiding 7.63 MiB. Preserved r5 after six reassigned generic rows created a sparse opposing-class pool and defeated that proof. The replacement returns the same winners and bit-exact staged audit scores, retains 994/997 benchmark queries early with zero fallback rescans, and reduced the 256-feature median from 3.61 to 3.40 seconds; the complete focused `build_lib` suite passes. Production restarts in fresh r6; r4/r5 remain immutable evidence.
- Final production evidence: r7 contract `3385f9ba8269ec4db5e599c30ae7bd443c29c1959ad6e834ffa7929287e6d8ab` consumed the complete r6 dataset-local release and promoted signature `66b66ec8e00e95f674170de44cffd5af3752e9f1caffb60640e3e5a8e939a059`. Final counts are raw 2,009,733, derivative 1,973,851, and nobaseline 1,477,496 spectra; all six typed artifacts and three aggregate bundles serialized and re-read successfully, with no medoid/model artifact.
- Final audit evidence: all 68 manifests balance exactly, quarantine v2/v3 shards balance every removal, typed/bundled objects are identical, IDs and axes are aligned/unique, and colliding legacy RRUFF IDs alone are dataset-qualified with original provenance retained. All six legacy comparator probes pass `c_spec()` and matching with compatible axes/attributes; expected updated-cleaning value and metadata differences are recorded under the r7 `review/` directory.
- Final package evidence: focused tests pass 1,240/1,240; the deterministic staging benchmark passes with a 1.53 MiB maximum merge block and output-identical sparse-conflict kernel; generated documentation changes only `man/build_lib.Rd`; hosted static passes 391/391; full tests pass 5,340 with two guarded AWS skips; staged R CMD check passes installation, tests, examples, docs, and vignettes with zero errors/warnings and two unchanged package-wide NOTEs (existing data.table visible bindings and marked UTF-8 data strings).
- Production authorization: maintainer request on 2026-11-20 authorizes the validated admitted cohort and local staged/full builds; publication, upload, hosted pins, and remote sync remain deferred.

## Approval Notes

- Approved by: maintainer request on 2026-11-20.
- Follow-up: medoid/model builds may be requested later from the completed unified libraries via `rebuild_lib_artifacts()`; they are prohibited in this tranche.
