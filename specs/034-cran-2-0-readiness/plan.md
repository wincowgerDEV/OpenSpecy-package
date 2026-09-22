# Feature Plan: CRAN 2.0.0 Readiness

**Feature dir**: `specs/034-cran-2-0-readiness`  
**Date**: 2026-09-21  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Slim reference artifacts and rebuild first; then AWS-only distribution, package/vignette hardening, and CRAN release gates.  
**Change class**: hosted/release (package/scientific artifact contracts are primary).

## Goal And Scope

- Submit OpenSpecy 2.0.0 with reproducible, fast-loading runtime libraries/models, a separate canonical assessment artifact, no errors or warnings, no unexplained significant notes, and tested package/vignette workflows.
- Preserve spectra, identifiers, axes, metadata alignment, predictions, and scientifically meaningful runtime attributes while removing build/audit payloads from deployable artifacts.
- **In**: release artifact schema, full rebuild, overall-only accuracy review, AWS migration, release metadata, tests/docs/vignettes, licensing, URLs, reverse dependencies, and hosted release gates.
- **Out**: new algorithms, app UI/pipeline changes, and hosted route redesign.

## Current Assessment

- Live CRAN 1.5.3 results (2026-09-19) show five r-devel ERROR flavors from two `read_opus()` warning failures; current source explicitly aligns `end[-length(end)]`, but exact-tarball r-devel evidence remains. The invalid historical Gitter URL remains a Debian NOTE.
- Baseline build `reference-library-build/releases/4a32e68349ba` was finalized 2026-09-12. It has no standalone assessment artifact; `reference_library_build.rds` is 688 MB and duplicates deployable content.
- Clean candidate `reference-library-build-2.0.0/releases/1085b9176a1d` completed in 11.0 hours from all 36 Processed sources plus `library_raw.rds`. Checksums, runtime loading/prediction/matching, and legacy comparisons pass; all 15 partitions preserve axes, IDs, counts, metadata names, and representative intensities exactly. Its medoids are 4.26/2.67 MB, assessments are 29.48 MB, and the release index is 415 bytes.
- Global cleanup, quality, pruning, and support reports survive filtering/partitioning as attributes on each Raman/FTIR/NIR object. Removing only component attributes changes medoid derivative 12.74→4.26 MB and no-baseline 9.09→2.67 MB; the spectra themselves are not the inflation source.
- Model payloads include prediction state plus training tests, lambda/OOB/class metrics, support, feature importance, and warnings; consumers must be mapped before defining the minimal runtime schema. Reviewed accuracy currently binds overall and per-class rows.
- `get_lib()` still exposes OSF/AWS selection; AWS omits `raw.rds`, and app/wasm pins are duplicated.

## Requirements

- R1. Set version/date, lead `NEWS.md` with the 2.0.0 migration summary, and generate `cran-comments.md` from final evidence.
- R2. Eliminate code-analysis findings by declaring legitimate data-table/NSE symbols and namespace-qualifying/importing utilities; add no speculative exports.
- R3. Correct two mojibaked `test_lib` strings, preserve valid UTF-8, and establish licenses/attribution; `raman_hdpe` follows the package CC BY 4.0 license because no supported dataset exception exists.
- R4. Keep representative CRAN tests; CI-guard exhaustive app/hosted/network cases. Target tests ≤3 minutes and check ≤5 minutes on current R.
- R5. Clean-build both CRAN vignettes; validate disabled snippets; fix stale claims, links, booleans, and README's invalid plural `wavenumbers` access.
- R6. Run online URL/DOI/ORCID checks, verify canonical URLs, re-query reverse dependencies, and prove R-patched/release/devel plus multi-platform checks.
- R7. Preserve the OPUS alignment fix and prove warning-free single/multiple reads with output parity on exact-tarball r-devel Linux GCC/clang and Windows.
- R8. Make `get_lib()` AWS-only: remove its `aws` argument and OSF storage logic/prose; add `raw.rds`; keep `revision` as S3 `versionId`; retain justified historical attribution.
- R9. Publish `assessments.rds` as the sole release payload for global cleanup, quality-control, pruning, compatibility, accuracy/confusion, model diagnostics/warnings, and manifest reports. Include it in the release manifest.
- R10. Libraries and medoids contain only `wavenumber`, `spectra`, row-aligned `metadata`, and required scientific/runtime provenance attributes; no global report table may remain on a component or metadata object.
- R11. Model artifacts retain only fields proven necessary to load and predict. Move training tests, per-class/OOB assessment tables, tuning paths, warnings, feature importance, and support diagnostics to `assessments.rds` unless a runtime consumer proves a field necessary.
- R12. `ref_lib$accuracy`, `medoid$accuracy`, and `model$accuracy` contain only overall aggregate rows and metrics (including overall and macro accuracy); remove `scope = "class"`/`expected_class` accuracy rows. Distinct confusion/support diagnostics may remain only in `assessments.rds`.
- R13. Prediction and matching results from slim artifacts equal the pre-slim build within existing tolerances; preserve type/axis/ID/class mappings and required baseline/derivative/transformation/range attributes.
- R14. Implement and subset-test the slim schema, then perform a clean full rebuild before AWS upload or remaining CRAN finalization. Compare it with `4a32e68349ba`; do not mutate that immutable baseline.
- R15. After upload, record version IDs/SHA-256 for the runtime artifacts plus `assessments.rds`, prove byte identity, load every artifact, and exercise representative full/medoid/model matches. Keep this integration off CRAN.
- R16. Derive `library_name` from organization first and user name second; retain reviewed broad `other plastic`/`other material` sources, and publish per-recipe source-library retention counts plus a clear first-stage reason for every complete drop.
- R17. Keep README onboarding short. Put compact/file-backed Specs and app pipeline details only in the advanced, SOP, and app vignettes.
- R18. Reassign every resolved class below pruning `min_n` as a whole to its most-correlated established class within the same technique/material constraints; drop only when no valid destination exists and audit the outcome.

## Technical Decisions

- **Assessment boundary**: gather reports while building, consume them for validation, then strip them through one internal release-sanitization step. Do not rely on incidental attribute loss in `filter_spec()`. A slim index may replace or accompany `reference_library_build.rds`, but it must not duplicate full artifacts or assessments.
- **Attribute contract**: define/test an allowlist of scientific/runtime attributes (for example units, derivative/baseline state, transformations, spectrum type, and identification range); everything report-shaped is assessment data.
- **Model contract**: map `match_spec()`, app, serialization, and backward-compatible loaders before trimming. Keep prediction inputs/state and compact schema metadata; extract diagnostics before promotion and test round-trip predictions.
- **Accuracy contract**: stop generating/publishing class-wise accuracy review rows rather than filtering only at display time; keep aggregate denominators, coverage, old/new provenance, and shifts interpretable.
- **Rebuild staging**: probe all three techniques and recipes with ≤100 spectra per class, then benchmark medoid reduction/model fit at representative largest-class dimensions. Set expected full-stage time from the probe; abort/restart from checkpoints at >2× that estimate, >80% available RAM, or 30 minutes without progress/checkpoint evidence.
- **Performance**: extend `benchmarks/library_builder.R` with old-versus-slim serialized bytes, in-memory bytes, and five repeated `readRDS()` timings plus prediction equivalence. Target medoid derivative ≤5 MB and no-baseline ≤3 MB; every other runtime artifact must shrink materially or document irreducible model/spectral state.
- **Generated docs**: edit roxygen/package metadata, run configured roxygen2 8.0.0, and inspect `NAMESPACE`/`man/*.Rd`; never hand-edit generated output.
- **Bundled app/pipeline**: no analysis routing change and no diagram update. Synchronize immutable AWS pins only; test genuine full, medoid, and model identification from the rebuilt set.
- **Hosted impact**: shared `R/` and staged libraries change. Run `-HostedAppStatic`, exact-artifact `/`, `/app/`, `/pkgdown/` preflight with library matching, and the release-triggered clean wasm rebuild; stage only slim medoid/model artifacts.

## Package Surfaces

- `R/build_lib.R`: assessment collection/review, overall-only accuracy, release sanitizer, model slimming, standalone assessment promotion, aggregate/manifest behavior.
- `R/raman_hdpe.R`, package/extdata objects, `README.md`, and vignettes: correct license metadata and separate beginner from advanced guidance.
- `R/manage_lib.R`, `R/zzz.R`: AWS-only mappings and version semantics; regenerate affected Rd files.
- `tests/testthat/test-build_lib.R` and model/matching/app tests: artifact absence/presence contracts, overall-only rows, prediction parity, manifests, loading, and guarded integration.
- `benchmarks/library_builder.R`: serialized size, memory, read latency, and equivalent predictions.
- `workflows/OpenSpecy_reference_library.R`: rebuild-first budgets, checkpoints, reporting, and new artifact inventory.
- `DESCRIPTION`, `NEWS.md`, `cran-comments.md`, `README.md`, `vignettes/*.Rmd`, `_pkgdown.yml`, `site/`: 2.0.0, AWS workflow, links, and release evidence.
- `inst/shiny/global.R`, `tools/wasm/stage-shinylive-libraries.R`, `.github/workflows/`: new immutable pins, slim staging, and release verification; no direct generated-site edits.

## Work Checklist

- [x] Define the runtime attribute/model-field allowlists and standalone `assessments.rds`/manifest schema; add focused tests for duplicated reports and class-wise accuracy.
- [x] Extract reports before sanitization, publish overall-only accuracy, slim library/medoid/model artifacts, and replace the aggregate with a nonduplicating index.
- [x] Run the subset probe and `benchmarks/library_builder.R`; verify object invariants, exact/tolerant prediction parity, size targets, read latency, budgets, and checkpoint restart.
- [x] Run the clean full rebuild first; compare IDs, axes, counts, metadata names, warnings, joins/matches, model predictions, assessment completeness, sizes, memory, and read timings with `4a32e68349ba`.
- [x] Make `get_lib()` AWS-only, remove OSF storage guidance/call sites, regenerate documentation, and update 2.0.0 release prose.
- [ ] Add canonical `library_name`, retain reviewed broad source categories, reassign undersupported classes, audit every source library, and rebuild with explicit complete-drop assessments.
- [x] Correct `raman_hdpe` licensing and shorten README while preserving advanced guidance in vignettes.
- [ ] After maintainer upload, pin all version IDs/hashes and validate byte-identical downloads plus local and hosted type-specific matches.
- [ ] Complete OPUS, code-analysis, encoding/licensing, test-runtime, vignette, URL, reverse-dependency, exact-tarball, and multi-platform CRAN gates.
- [ ] Run `-HostedAppStatic`, matching-artifact preflight, and clean wasm build once on the final rebuilt/pinned candidate; reconcile evidence, processes, `git status`, and scratch cleanup.

## Verification And Open Questions

- Focused order: sanitizer/assessment/model tests → payload benchmark/subset probe → full rebuild → AWS integration → docs/vignettes → full tests/check → hosted/release gates. Reuse evidence only while covered code, artifacts, pins, and contracts are unchanged.
- Upload/publishing remains maintainer-owned. Capture S3 version IDs before pin changes and do not validate only through unversioned cached CloudFront responses.
- Confirm whether any external maintainer workflow consumes `reference_library_build.rds`; absent a consumer, omit the 688 MB aggregate and retain the manifest plus canonical `assessments.rds`.
- Confirm canonical 2.0 public/support URLs and third-party redistribution rights; remove unsupported links/assets rather than guessing replacements or relicensing.
- 2026-09-22: all seven runtime AWS objects are pinned and pass byte/SHA/load/full-medoid-model matching on R 4.3.3 and R 4.6.1; `assessments.rds` and release index/manifest are not yet public, so R15 remains open.
- Current R 4.3.3 candidate: 3,884 assertions passed with 30 expected validation warnings and two guarded AWS skips; 353 hosted-static assertions passed. The staged check rebuilt both CRAN vignettes and has 0 errors, 0 warnings, and one explained valid-UTF-8 NOTE in 12m16s. R 4.6.1 remains to be rerun after the current source changes; current CRAN reverse dependencies: none.
- Hosted static gate passes 353 assertions. Commit `3b92b15` now pins the candidate, but exact-artifact preflight/clean wasm awaits a running Docker engine or fresh CI artifact; R-patched/devel Linux/macOS checks also remain, so the last two checklist items stay open.

## Approval Notes

- Approved by:
- Follow-up: implementation uses package, generated-doc, Windows-R, and quality-gate skills; push/pull remains unauthorized.
