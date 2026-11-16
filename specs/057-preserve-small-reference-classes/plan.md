# Feature Plan: Preserve Small Reference Classes

**Feature dir**: `specs/057-preserve-small-reference-classes`  
**Date**: 2026-11-16  
**Current tranche**: Remove support-count-based material-class reassignment/removal, verify the public builder contract, and rebuild the October 15 reference cohort into a fresh audited target.  
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

## Technical Decisions

- **Approach/API**: remove the one-caller small-class reassignment helper and support-only final drops. Do not add an argument or change signatures: reviewed-class preservation is the conservative default; explicit renames remain data-driven in `classes_reference.csv`/`material_hierarchy.csv`.
- **OpenSpecy contract**: preserve axes, spectral values, stable IDs, metadata row/column alignment, and attributes; only prevent unsupported mutations of `material_class`/`material_type` and support-only filtering.
- **Generated artifacts**: update roxygen/vignette source and regenerate `man/build_lib.Rd` with the `DESCRIPTION`-pinned roxygen2; never edit generated files directly.
- **Reference compatibility**: compare the fresh build with release `a1b961ef21a5`, including IDs, axes, metadata names/counts, warnings, class changes, representative joins/matches, model schema, and the PDMS row.
- **Official-build contract**: freeze the recovered October 15 source paths and SHA-256 hashes, lookup/code/prior-artifact hashes, and launch only into `../reference-library-build-preserve-small-classes-20261116`; clean `reuse = FALSE`; global `prune_lib()` disabled (`prune = list()`), so generic and small-class correlation reassignment are both disabled in production; cross-class closure disabled (threshold/operator N/A) independently of strict source-local identity-cohort correlation `> 0.99` after exact hashes with deterministic survivor order. Use flat guardrailed logistic regression, seed 123, holdout 0.1, current hierarchy, and the October 15 artifact as comparator.
- **Bounded build**: source hash blocks 20,000; path-backed two-pass retained-only staging and HDF5 target blocks 5,000; plan global exclusions/hashes before retained-row metadata binding; capture removals from source-local shards; drop blanks by reference; losslessly compact/expand metadata; release source references early; GC every 10 merge chunks; joins 100,000 rows with no full copy, in-pass coalescing, and 1,000 representative diagnostics plus exact/collapsed totals; swap only recipe metadata columns; file-backed recipe trigger 250,000/block 5,000; hashed v3 quarantine shards must balance the ledger.
- **Performance/stop rule**: probe the highest-risk source hash/merge/recipe kernels first. Expect no worse than the October 15 cohort’s roughly 17-hour wall time and keep modeled dense target plus baseline below the current frozen threshold; stop at the next durable checkpoint on any modeled-threshold breach, transient memory breach, source hash change, unbounded square correlation allocation, or materially worse measured scaling, then isolate and restart fresh.
- **Pipeline diagram**: update `.specify/memory/build-lib-diagram.html` to state that support count cannot rename/drop reviewed classes. The app analysis `pipeline-diagram.html` is unaffected.
- **Hosted impact**: `R/` is shared hosted input, so run fast `-HostedAppStatic`. No hosted routes/runtime/assembly, dependency pin, wasm image, or generated app changes; matching-artifact preflight and clean wasm rebuild are not triggered.

## Package Surfaces

- `R/build_lib.R`: `prune_lib()`, small-class helper removal, typed closure, roxygen.
- `tests/testthat/test-build_lib.R`: regressions for small-class preservation, generic reassignment, cross-class closure, schemas, and PDMS-like metadata.
- `.gitattributes`, `tests/testthat/test-run_app.R`: preserve the LF-pinned SpecyPro schema hash across platforms; no app behavior change.
- `vignettes/library-builder.Rmd`, `NEWS.md`, generated `man/build_lib.Rd`, `.specify/memory/build-lib-diagram.html`: policy documentation.
- `workflows/OpenSpecy_reference_library.R`: add a minimal prior-library comparator-path environment override; production policy and defaults remain unchanged.
- `benchmarks/`: N/A—intentional scientific behavior change, not a same-output optimization.
- `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README/pkgdown source: unchanged.

## Work Checklist

- [x] Update `R/build_lib.R` so low support alone cannot rename or remove reviewed classes while report schemas remain compatible.
- [x] Replace historical small-class expectations with focused preservation and cross-class regressions in `tests/testthat/test-build_lib.R`.
- [x] Update roxygen, vignette, NEWS, and the canonical build-library diagram; regenerate and inspect documentation.
- [ ] Run focused `build_lib` tests, `-HostedAppStatic`, full tests, and the staged R CMD check once on the final package candidate.
- [x] Reproduce and fix the latest GitHub Actions test failures, then run the focused build/app checks invalidated by those fixes.
- [ ] Recover/freeze the October 15 inputs and hashes, run bounded probes, launch one clean fresh build, monitor thresholds/checkpoints, and run postbuild/disposition audits.
- [ ] Reconcile every checkbox with evidence; record deferred publication, stop or record owned processes, inspect `git status`, and remove task scratch.

## Verification And Open Questions

- Direct regression: a one-spectrum reviewed class remains unchanged with `min_n = 10`; generic labels can still reassign; support-only reports stay empty; cross-class removal does not trigger a final low-support purge.
- Toolchain: confirm R 4.3.3 and installed roxygen2 exactly match `Config/roxygen2/version`; run `devtools::document()` once and reject unexpected authorship/export/alias/reference diffs.
- Gates: focused `build_lib`; fast hosted-source static; full `devtools::test()` once; staged `R CMD check` because the tranche is release-facing. No browser gate is triggered.
- Build audit: validate source fingerprint stability, same/variable-axis equivalence, bounded hash/merge/recipe and metadata-copy telemetry, storage-mode cleanup, object validity, disposition/quarantine balance and shard hashes, compatibility summaries, and the exact Horiba PDMS class.
- Open question resolved conservatively: “rerun October 15” means the same recoverable source cohort and prior artifact comparison under today’s maintained bounded production contract, not restoration of the deprecated global class-pruning pass.

## Approval Notes

- Approved by: maintainer request on 2026-11-16.
- Follow-up: artifact publication/upload and any hosted pin change remain maintainer-owned.
