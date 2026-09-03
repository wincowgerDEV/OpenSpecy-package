# Feature Plan: Reference Taxonomy, Review Filtering, and Wasm Recovery

**Feature dir**: `specs/023-reference-taxonomy-and-wasm`
**Date**: 2026-09-02
**Current tranche**: Refine official material classes and review filtering, rank assessment confusions, run a fresh full reference build, and restore the failing pinned wasm repository workflow.
**Change class**: hosted/release (package/scientific behavior plus deployment build infrastructure)

## Goal

- Publish reference libraries, medoids, and models whose identities are reviewable and whose polymer class names are concise and useful for identification.
- Remove unresolved/generic spectra by default without losing their audit trail, while retaining an opt-in semisupervised reassignment workflow.
- Make the pinned WebAssembly package-repository Action reliably rebuild after dependency metadata changes.

## Scope

- **In**: official `build_lib()` filtering policy; typed review assessments; exact hierarchy edits; polyethylene/polypropylene separation; ranked class confusions; docs/NEWS/diagram; GitHub wasm cache/build recovery; focused, full-package, R CMD, hosted, subset, and fresh production-build verification.
- **Out**: filling missing identities by inference, changing calibrated model hyperparameters, fuzzy old/new taxonomy matching, changing bundled Shiny behavior, publishing artifacts, or pushing commits.
- **Users**: maintainers supply explicit source/output paths and call `build_lib(..., remove_other = TRUE)`; reviewers use the returned assessment tables to repair excluded metadata and inspect dominant errors.

## Requirements

- R1. Add `remove_other = TRUE` to `build_lib()`. In official end-to-end mode, `TRUE` removes every spectrum with blank `spectrum_identity` or a generic `material`, `material_class`, or `material_type` of `other`, `other plastic`, or `other material` before quality control, pruning, partitioning, medoids, models, and comparisons.
- R2. Store a stable typed `assessments$other_review` row for every affected artifact/spectrum with identifiers, available source metadata, prior labels, reason, and action; store numeric per-artifact before/after/removal counts. Missing identity takes precedence as the review reason.
- R3. With `remove_other = FALSE`, retain generic spectra for the existing `prune_lib()` nearest eligible class policy: unrestricted `other`, plastic-only `other plastic`, and organic-matter/mineral-only `other material`. Report unresolved reassignments rather than silently deleting them.
- R4. Replace verbose polymer class aliases with exact concise labels in `workflows/data/material_hierarchy.csv`; preserve `polyhydroxy(meth)acrylates`, the conventional exceptions `silicones`, `cellulose derivatives`, and `paint`, and make other polymer/copolymer families start with `poly` where chemically appropriate.
- R5. Map `poly(ethylene)` to `polyethylene` and `poly(propylene)` to `polypropylene`; retain the concise `polyolefins` class for the other olefin rows. Validate one hierarchy row per material and no blank class/type values.
- R6. Confusion assessments expose a typed `misidentified` flag and within-expected-class error proportion, with misidentifications sorted by descending spectrum count before correct cells so the largest model/library failure modes are immediately visible.
- R7. The full production run uses the supplied official sources, a fresh explicit output directory, `reuse = FALSE`, `remove_other = TRUE`, the entire candidate data, and entire legacy artifacts for source-local assessment. The prior 1,000-spectrum sample remains probe-only.
- R8. Diagnose the current `deploy-cran-repo.yml` failure from GitHub evidence. A dependency-metadata cache miss must be able to seed from the latest compatible successful repository, evict/rebuild OpenSpecy, refresh requested roots, and validate root packages, hard dependencies, pinned SHA/version, binaries, and image.

## Technical Decisions

- **Object flow**: sources -> canonical metadata/exact+regex class resolution -> hierarchy -> generic review/removal policy -> derivative/nobaseline quality -> prune -> type-specific full libraries -> 10%-support medoids -> filled model training -> source-local assessments -> checkpointed release.
- **Public API**: `remove_other` is a meaningful official-build policy with two current uses. It defaults to the maintainer's high-confidence library; generic-label detection, review schema, propagation, and matching constraints are inferred/internal. Source-only builds do not perform official taxonomy filtering.
- **Taxonomy**: edit only exact hierarchy values; do not add regex or infer missing chemical identities. Class-renaming tests enumerate the reviewed old-to-new mapping and the polyethylene/polypropylene split.
- **OpenSpecy contract**: filtering always uses aligned `sample_name` identifiers through `filter_spec()` and preserves axes, metadata order, object attributes, and original NA intensities.
- **Performance/observability**: removal is one vectorized metadata pass per recipe. Probe 1,000 raw spectra plus representative FTIR/Raman/NIR kernels; expect each PAM/correlation kernel under 10 minutes and abort an uncheckpointed silent stage at 15 minutes or >2x probe projection. Full stages report dimensions, elapsed time, and checkpoint writes.
- **Generated/docs**: update roxygen, vignette, NEWS, and `.specify/memory/build-lib-diagram.html`; regenerate with configured R 4.3.3/roxygen only and inspect generated diffs. `.specify/memory/pipeline-diagram.html` is N/A because the Shiny analysis reactive is unchanged.
- **Hosted**: `R/` and `.github/workflows/deploy-cran-repo.yml` change. Run `-HostedAppStatic`, focused wasm tests, and a clean-commit-equivalent repository rebuild; run exact-artifact app preflight only if a matching successful artifact exists after the fix. No app source/routes/pins/library staging changes.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`: implement/propagate `remove_other`, typed review/count schemas, progress, checkpoint signatures, recovery, and ranked confusion fields/order.
- [x] `workflows/data/material_hierarchy.csv`: apply the reviewed concise polymer mapping and polyolefin split; audit exact uniqueness/coverage.
- [x] `tests/testthat/test-build_lib.R`: cover default/opt-out behavior, NA-safe aligned filtering, review schemas, reassignment compatibility, hierarchy mapping, and confusion ordering/rates.
- [x] `.github/workflows/deploy-cran-repo.yml`, `tools/wasm/build-wasm-repo.ps1`, and wasm tests: restore a compatible cache seed across dependency hashes and prevent empty-root artifacts.
- [x] Roxygen/generated help, `vignettes/library-builder.Rmd`, `NEWS.md`, `workflows/OpenSpecy_reference_library.R`, and `.specify/memory/build-lib-diagram.html`: document the behavior and explicit full-run invocation.
- [x] Run focused tests/audits, 1,000-spectrum and representative kernel probes, documentation/vignette validation, benchmarks, full tests, `-HostedAppStatic`, wasm recovery fixture, and R CMD check. Docker rehearsal is unavailable because its daemon is stopped; post-push CI remains the exact environment gate.
- [x] Run/monitor fresh `build_lib(..., remove_other = TRUE, reuse = FALSE)`; reload artifacts and compare IDs, axes, metadata, warnings, exclusions, dominant confusions, and source-local legacy/candidate metrics.
- [x] Reconcile every checkbox with evidence; record unavailable post-push Action/artifact gates, stop owned processes, inspect status, export the assessment report, and remove task scratch from the repository root.

## Verification And Risks

- Direct: compact `testthat` filters for `build_lib|shinylive_wasm`; CSV duplicate/blank/expected-mapping audit; PowerShell parse/static checks; fixture repository proving cross-key seed discovery and OpenSpecy eviction.
- Final: configured-toolchain `devtools::document()`, vignette render, one `devtools::test()`, fast hosted-source gate, maintained staged `R CMD check`, and full reference build/reload report. The prior passing CMD check is invalidated by public R/docs/workflow changes.
- Wasm: current run `33694702636` failed after 23 minutes because `OpenSpecy`, `DT`, `plotly`, `hyperSpec`, and `hdf5r` were absent; the last success is `33099690227`. Local Docker is initially unavailable, so a true image rebuild requires restoring Docker or post-push CI; keep that checkbox/evidence explicit until satisfied.
- Taxonomy changes intentionally break exact comparability of class labels across versions; source-local cohorts and exact labels remain the approved assessment contract.
- Removing generic rows may reduce rare-class support; the review/count and model-class-support tables must make that loss visible before release.

## Completion Evidence

- Focused tests: 639 passed. Full tests: 3,097 passed with 31 expected test warnings and 3 guarded skips. Hosted static: 297 passed. R CMD check: 0 errors, 0 warnings, 2 pre-existing data/global-variable notes.
- Fresh release `17b419a919a3` completed in 18,200.6 seconds. All nine type-specific full libraries reload as aligned, duplicate-free `OpenSpecy` objects; 8,307 generic/missing-identity spectra are excluded and reviewable; source-local splits have no group leakage.
- Wasm recovery fixture proves cross-hash seed selection, while focused tests verify the builder wires that seed to OpenSpecy eviction. Live Action confirmation requires a maintainer push; no matching post-fix artifact exists and Docker is not running locally.

## Approval Notes

- Approved by user request, 2026-09-02. Remote push/publish remains maintainer-owned.
