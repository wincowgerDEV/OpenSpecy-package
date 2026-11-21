# Feature Plan: Preserve Small Reference Classes

**Feature dir**: `specs/057-preserve-small-reference-classes`  
**Date**: 2026-11-16  
**Current tranche**: Correct the large-file regression by restoring bounded generic reassignment and ordinary pruning without restoring support-count class mutation; rebuild and compare the October 15 cohort.
**Change class**: hosted/release (package/scientific behavior plus an official external library artifact)

## Goal And Scope

- Preserve every reviewed non-generic `material_class` regardless of class size; only exact class-reference or hierarchy joins may rename it.
- Keep generic `other`/`other plastic`/`other material` correlation reassignment and explicit cross-class conflict removal independently composable.
- Rebuild the exact recoverable October 15 source cohort with the maintained production contract; publication/upload remains maintainer-owned.
- Out of scope: taxonomy CSV edits, new datasets, model-method tuning, hosted generated artifacts, or remote synchronization.

## Requirements

- R1. `prune_lib()` never changes or removes a resolved class solely because its support is below `min_n`; groups at or below the floor remain whole.
- R2. `min_n` remains the lower retention floor for ordinary correlation pruning and model/reduction consumers; generic labels retain their existing audited reassignment behavior.
- R3. Cross-class conflict removal may remove conflicting spectra only under its explicit policy, but no post-closure support floor may delete the remaining small class.
- R4. Report names/schemas remain compatible; small-class `excluded_classes`, reassignment, and threshold-removal outputs are empty/zero unless populated by a distinct retained policy.
- R5. The rebuilt Raman medoid retains Horiba `polydimethylsiloxane` as `polysiloxanes`, with aligned `OpenSpecy` spectra/metadata and valid attributes.
- R6. Repair the latest-push CI failures: make the staged-dedupe fixture invariant across supported R versions, preserve the frozen SpecyPro schema bytes on Windows checkout, and make malformed metadata-byte cleanup deterministic on Windows locales.
- R7. Keep app spatial-unit suffixes and JCAMP-DX parsing deterministic when Windows R cannot activate the requested UTF-8 locale.
- R8. Keep repeated ingested occurrences independently quarantined when distinct source records share the same content hash and local row.
- R9. Large staged derivative/no-baseline recipes must reproduce in-memory generic reassignment and ordinary active-set pruning, quarantine every removal, and never leave a retained target generic row with reassignment unattempted.

## Technical Decisions

- **Approach/API**: remove the one-caller small-class reassignment helper and support-only final drops. Do not add an argument or change signatures: reviewed-class preservation is the conservative default; explicit renames remain data-driven in `classes_reference.csv`/`material_hierarchy.csv`.
- **OpenSpecy contract**: preserve axes, spectral values, stable IDs, metadata row/column alignment, and attributes; only prevent unsupported mutations of `material_class`/`material_type` and support-only filtering.
- **Generated artifacts**: update roxygen/vignette source and regenerate `man/build_lib.Rd` with the `DESCRIPTION`-pinned roxygen2; never edit generated files directly.
- **Reference compatibility**: compare the fresh build with release `a1b961ef21a5`, including IDs, axes, metadata names/counts, warnings, class changes, representative joins/matches, model schema, and the PDMS row.
- **Official-build contract**: freeze the recovered October 15 source paths and SHA-256 hashes, lookup/code/prior-artifact hashes, and launch clean with `reuse = FALSE`; preserve prior failed/completed targets. Supersede the incorrect `prune = list()` contract: select derivative and nobaseline ordinary pruning with `cross_class = FALSE`, generic reassignment before removals, default 2200--2420 exclusion and `min_n = 10`; raw remains unpruned. Strict source-local identity-cohort correlation `> 0.99` after exact hashes remains independent. Use flat guardrailed logistic regression, seed 123, holdout 0.1, current hierarchy, and the October 15 artifact as comparator.
- **Bounded build**: retain the frozen hash/merge/join/recipe limits. Use the standard bounded `prune_lib()` path below the 250,000-spectrum completion threshold; only larger selected completed recipes use temporary HDF5 with at most 256 queries by 5,000 candidates, recorded avoided square bytes, cleanup, and streamed balanced v3 quarantine.
- **Performance/stop rule**: probe the highest-risk source hash/merge/recipe kernels first. Expect no worse than the October 15 cohort's roughly 17-hour wall time and keep modeled dense target plus baseline below the current frozen threshold; stop at the next durable checkpoint on any modeled-threshold breach, transient memory breach, source hash change, unbounded square correlation allocation, or materially worse measured scaling, then isolate and restart fresh. Run one stopped after staging 39,726 objects when per-record collection projected two 9.5-hour metadata passes; cadence 10 reduced a same-output 200-shard scan from 53.73 to 5.50 seconds. Run two completed staging in 3h57m and scan/bind in 4h44m, then stopped when two collections per merge source projected 31.9 hours; cadence reduced the exact merge kernel from 113.555 to 6.520 seconds. Run three completed staging in 3h55m, scan in 2h42m, and bind in 68m, then stopped when one compressed HDF5 write per source projected 8.9 hours; the buffered exact-value benchmark reduced the 200-shard median from 105.570 to 5.895 seconds, GC calls from 400 to 20, and HDF5 writes from 200 to 1. Run four proved the bounded scan/bind/merge/recipe path in about 9 hours, then failed post-partition because 30 distinct Rachel Kozloski FTIR removals shared content-hash/row keys across two source records; the frozen replacement adds source-record identity to both quarantine keys and payload IDs.
- **Pipeline diagram**: update `.specify/memory/build-lib-diagram.html` to state that support count cannot rename/drop reviewed classes. The app analysis `pipeline-diagram.html` is unaffected.
- **Hosted impact**: `R/` is shared hosted input, so run fast `-HostedAppStatic`. No hosted routes/runtime/assembly, dependency pin, wasm image, or generated app changes; matching-artifact preflight and clean wasm rebuild are not triggered.

## Package Surfaces

- `R/build_lib.R`, `R/build_lib_staging.R`: `prune_lib()`, small-class helper removal, typed closure, source-record quarantine identity, roxygen.
- `tests/testthat/test-build_lib.R`: regressions for small-class preservation, generic reassignment, cross-class closure, schemas, and PDMS-like metadata.
- `.gitattributes`, `tests/testthat/test-run_app.R`: preserve the LF-pinned SpecyPro schema hash across platforms; no app behavior change.
- `R/read_ext.R`, `inst/shiny/global.R`, focused reader/app tests: avoid locale-dependent non-ASCII source parsing and retry a structurally valid JCAMP file when its requested decoder fails.
- `vignettes/library-builder.Rmd`, `NEWS.md`, generated `man/build_lib.Rd`, `.specify/memory/build-lib-diagram.html`: policy documentation.
- `workflows/OpenSpecy_reference_library.R`: add a minimal prior-library comparator-path environment override; production policy and defaults remain unchanged.
- `benchmarks/`: repeated same-output GC-cadence and buffered-HDF5-write comparisons for the production bottlenecks.
- `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README/pkgdown source: unchanged.

## Work Checklist

- [x] Update `R/build_lib.R` so low support alone cannot rename or remove reviewed classes while report schemas remain compatible.
- [x] Replace historical small-class expectations with focused preservation and cross-class regressions in `tests/testthat/test-build_lib.R`.
- [x] Update roxygen, vignette, NEWS, and the canonical build-library diagram; regenerate and inspect documentation.
- [x] Run focused `build_lib` tests, `-HostedAppStatic`, full tests, and the staged R CMD check once on the final package candidate.
- [x] Reproduce and fix the latest GitHub Actions test failures, then run the focused build/app checks invalidated by those fixes.
- [x] Recover/freeze the October 15 inputs and hashes, run bounded probes, launch one clean fresh build, monitor thresholds/checkpoints, and run postbuild/disposition audits.
- [x] Reconcile every checkbox with evidence; record deferred publication, stop or record owned processes, inspect `git status`, and remove task scratch.
- [x] Freeze and run the 20-map positive-control holdout against release `7c5b4c82d666` full, medoid, and model artifacts after fresh OS1 compatibility gates; report paired accuracy without tuning.
- [x] Prove staged best-match, generic reassignment, active-set removal, audit, and quarantine equivalence; update docs/diagram and run invalidated package gates.
- [x] Freeze a fresh corrective production contract/target, run probes, launch and monitor one full October 15-source rebuild, and complete postbuild/disposition audits.
- [x] Rerun the same exposed holdout strictly as a regression comparison (not an independent promotion estimate), without changing the candidate from its outcomes.

## Verification And Open Questions

- Direct regression: a one-spectrum reviewed class remains unchanged with `min_n = 10`; generic labels can still reassign; support-only reports stay empty; cross-class removal does not trigger a final low-support purge.
- Toolchain: confirm R 4.3.3 and installed roxygen2 exactly match `Config/roxygen2/version`; run `devtools::document()` once and reject unexpected authorship/export/alias/reference diffs.
- Gates: focused `build_lib`; fast hosted-source static; full `devtools::test()` once; staged `R CMD check` because the tranche is release-facing. No browser gate is triggered.
- Build audit: validate source fingerprint stability, same/variable-axis equivalence, bounded hash/merge/recipe and metadata-copy telemetry, storage-mode cleanup, object validity, disposition/quarantine balance and shard hashes, compatibility summaries, and the exact Horiba PDMS class.
- Holdout follow-up: pin truth, DAT/HDR inputs, exclusions, code, artifact hashes, runners, and controls in a fresh external result root; probe three maps per route, use restartable per-map checkpoints, and stop on an error, ten minutes without progress, or a resource threshold breach.
- Corrective verification: compare staged and in-memory pruning on representative deterministic ties and actual removals; require no target-recipe `retained_reassignment_not_attempted`/`reassignment_audit_missing`, bounded telemetry, balanced quarantine, and exact retained spectra/metadata with correlation evidence within floating-point tolerance.
- Open question resolved by the corrective request: “rerun October 15” means the same recoverable source cohort and comparator, with the pre-large-file derivative/no-baseline generic reassignment and ordinary pruning semantics restored through the new bounded staged implementation.
- Package evidence: focused builder and locale regressions pass; staged/in-memory pruning equivalence passes with exact retained spectra, metadata, assignments, and removals (correlations within floating-point tolerance); the bounded 2,000-by-2,000 kernel holds its largest correlation block to 3.9 MiB while avoiding the 30.5 MiB square; the full repository suite passes with three known locale warnings and two opt-in AWS skips; hosted static passes 391/391; the 598-file, 21,262,066-byte staged R CMD check passes with the two known NOTES under Windows’ supported UTF-8 locale. The retained check log is under `openspecy-staged-check-8cceb6c5554f485da11dd4eb8ee52817`.
- Build evidence: the 2,050-spectrum staged probe retained 1,420 spectra and balanced 630 removals with 630 unique source-record-qualified quarantine keys. The unchanged 36 processed plus one raw October 15 inputs total 1,403,791,165 bytes; the production contract is `125de02625ab2bf754bcbdf3f84face7daecc8f9d17faf487024455ed215b105`. Release `7c5b4c82d666` completed in 50,903 seconds (14h08m): source staging 3h55m, exclusion/hash scan 3h01m, metadata assembly 65.6m, merge 83.7m, and bounded recipe staging 29.5m. The warning-free postbuild audit passed 12/12 release hashes, all 15 aligned `OpenSpecy` partitions, identical model top-level schemas, a balanced 40,326-row v3 quarantine, the 20,000/5,000/100,000 block limits with zero metadata copies, zero support-only exclusions/reassignments/removals, and the exact Horiba PDMS derivative/Raman medoid as `polysiloxanes`. Driver warnings were the known Windows `C.UTF-8` worker startup messages, one native-to-UTF-8 translation notice, and empty successful intake-report notices.
- Holdout evidence: frozen contract `b93ee9fc52092828c87296985c1e22eb2d6a382b26339ba4ede7a01fdb210947` ran 20 eligible maps for fresh OS1 controls and release `7c5b4c82d666` full/medoid/model routes. All Stage 1 gates reproduced exactly, recovery stayed identical, and analysis warnings were zero, but paired specific-ID deltas were -7.63 pp (95% bootstrap CI -12.32 to -3.88), -12.62 pp (-22.47 to -5.31), and -38.63 pp (-54.75 to -23.51), respectively; do not promote this artifact as the accuracy candidate. The external frozen report and hashed decision files are under `Positive_Controls/library_validation_7c5b4c82d666/assessment`.
- Corrective build evidence: contract `7d0c632085047540e11cf7b3ff108481dbabe0c7f51bd98cfc646c6970710b0e` produced release `57da878d87b2` in 46,666 seconds from the pinned 36 processed plus one raw October 15 inputs. The merged library stayed below the 250,000-spectrum trigger and used standard bounded `prune_lib()`: derivative reassigned 6,946 generics and removed 1,547 ordinary redundancies; nobaseline reassigned 7,629 and removed 1,744; support-only exclusions were zero and cross-class closure was disabled. Candidate-specific audit passed 33/33, including 15/15 October axes, 14/15 bit-exact shared spectra plus raw-FTIR machine-epsilon equivalence (maximum `2.22e-16`), balanced 43,617-row/2,041-shard quarantine, and exact Horiba PDMS taxonomy. The standard audit's three failures were stale-comparator/schema checks: `bb8cd82c7ecb` axes instead of the October comparator, bitwise raw-FTIR tolerance, and a vinyl evidence gate requiring absent `material_group`/`gate_2_name` columns.
- Corrective holdout evidence: exposed-regression contract `4b3db307cce80217deb91122b25e62f426c0816b392ccd4f83d81cc43eac974a` completed all six 20-map routes with 15/15 OS1 gates and zero analysis warnings. Full paired deltas were -1.17 pp specific (95% bootstrap CI -2.48 to +0.01) and -1.70 pp plastic (-3.96 to +0.06); medoid deltas were -0.97 pp (-4.60 to +3.48) and -0.08 pp (-3.50 to +4.45). The reported full/medoid vinyl-halide-to-`other` regressions fell from 185/10 to 0/0. The refit model remains blocked at -13.00 pp specific (-23.22 to -4.58) and -6.36 pp plastic (-13.75 to -0.87), led by 112 polyethylene particles routed to PVA; this defect predates the pruning correction (107 in the prior model) and is outside the no-tuning scope. Hashed outputs are under `Positive_Controls/library_validation_57da878d87b2_exposed_retest/assessment`.

## Approval Notes

- Approved by: maintainer request on 2026-11-16.
- Follow-up: artifact publication/upload and any hosted pin change remain maintainer-owned.
