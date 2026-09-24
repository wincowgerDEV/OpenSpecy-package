# Feature Plan: CRAN 2.0.0 Readiness

**Feature dir**: `specs/034-cran-2-0-readiness`  
**Date**: 2026-09-21  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Reuse the completed 2.0 libraries/models for revised assessments while completing bounded matching and bundled-app usability fixes.
**Change class**: hosted/release (package/scientific artifact contracts are primary).

## Goal And Scope

- Submit OpenSpecy 2.0.0 with reproducible, fast-loading runtime libraries/models, a separate canonical assessment artifact, no errors or warnings, no unexplained significant notes, and tested package/vignette workflows.
- Preserve spectra, identifiers, axes, metadata alignment, predictions, and scientifically meaningful runtime attributes while removing build/audit payloads from deployable artifacts.
- **In**: release artifact schema, compact comparative accuracy review, bounded matching, bundled-app UI/pipeline fixes, AWS migration, tests/docs/vignettes, and hosted release gates.
- **Out**: new identification algorithms and hosted route redesign.

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
- R12. `ref_lib$accuracy`, `medoid$accuracy`, and `model$accuracy` contain overall accuracy, macro class accuracy, evaluated class count, and old/new source context without per-class accuracy rows. Distinct confusion/support diagnostics may remain only in `assessments.rds`.
- R13. Prediction and matching results from slim artifacts equal the pre-slim build within existing tolerances; preserve type/axis/ID/class mappings and required baseline/derivative/transformation/range attributes.
- R14. Implement and subset-test the slim schema, then perform a clean full rebuild before AWS upload or remaining CRAN finalization. Compare it with `4a32e68349ba`; do not mutate that immutable baseline.
- R15. Routine package tests, bundled-app fallback, and Shinylive staging download the current unversioned AWS objects. Preserve optional `get_lib(revision=)` only for explicit historical comparisons; record the actual staged SHA-256/bytes in the action manifest and keep the live integration off CRAN.
- R16. Derive `library_name` from organization first and user name second; retain reviewed broad `other plastic`/`other material` sources, and publish per-recipe source-library retention counts plus a clear first-stage reason for every complete drop.
- R17. Keep README onboarding short. Put compact/file-backed Specs and app pipeline details only in the advanced, SOP, and app vignettes.
- R18. Reassign every resolved class below pruning `min_n` as a whole to its most-correlated established class within the same technique/material constraints; drop only when no valid destination exists and audit the outcome.
- R19. Keep random-forest training available only through explicit experimental calls; the production reference rebuild defaults to logistic models and does not publish random-forest artifacts.
- R20. Keep review tables coherent and compact: do not row-bind unrelated schemas, reject review columns with more than 10% missing values, retain detailed evidence separately, and report model/test-error association as with-error versus without-error accuracy percentages.
- R21. Route H5 paths in `automate_particle_analysis()` through bounded `FileSpecs`; support streamed `all_cell_id` matching and connected Mean collapse without materializing the complete spectral matrix.
- R22. Add optional `batch_size` to `match_spec()`; when supplied for spectral libraries require finite `top_n`, bound each library-by-query correlation matrix, and preserve dense results/order. Use `top_n = 1` for in-memory `all_cell_id` and expose the batch size in the app.
- R23. Expose one local direct-path picker: use the native Windows/macOS dialog when available and the no-copy `shinyFiles` browser elsewhere; keep Shinylive on WORKERFS. Repair the landing contact action, reorder Advanced controls, and nest pixel calibration under particle collapse.
- R24. Min-Max Normalize controls the displayed raw, active, and identification traces after all processing; keep peak markers aligned. Simple Metadata always exposes Column ID and exposes X/Y when available.
- R25. Make the logistic-weight overlay a legend-controlled trace group. Rename the memory control to Load Entire File into Memory and place its identification batch-size setting inside that box.
- R26. Let users add valid quantification ratios and measurements before upload or Run; defer only axis-dependent calculation to the committed analysis. Mark any settings tab green while it has an active feature; Quantification uses saved definitions and provides Remove All.

## Technical Decisions

- **Assessment boundary**: gather reports while building, consume them for validation, then strip them through one internal release-sanitization step. Do not rely on incidental attribute loss in `filter_spec()`. A slim index may replace or accompany `reference_library_build.rds`, but it must not duplicate full artifacts or assessments.
- **Attribute contract**: define/test an allowlist of scientific/runtime attributes (for example units, derivative/baseline state, transformations, spectrum type, and identification range); everything report-shaped is assessment data.
- **Model contract**: map `match_spec()`, app, serialization, and backward-compatible loaders before trimming. Keep prediction inputs/state and compact schema metadata; extract diagnostics before promotion, reduce released glmnet fits to the selected lambda, and test round-trip predictions. The exact historical derivative model (`Wk7H...`) is 12,546,653 bytes; the replacement must not exceed it.
- **Accuracy contract**: publish overall and macro class accuracy plus evaluated class count with adjacent old/new source rows; keep per-class accuracy, coverage, score, and training diagnostics out of the review table.
- **Deployed-artifact assessment**: apply each medoid to its complete corresponding processed library and each existing model to its complete corresponding source dataset once. Do not split, reduce medoids, or retrain models for these assessments; retain grouped holdouts only for full-reference nearest-neighbor evaluation.
- **Rebuild staging**: probe all three techniques and recipes with ≤100 spectra per class, then benchmark medoid reduction/model fit at representative largest-class dimensions. Set expected full-stage time from the probe; abort/restart from checkpoints at >2× that estimate, >80% available RAM, or 30 minutes without progress/checkpoint evidence.
- **Performance**: extend `benchmarks/library_builder.R` with old-versus-slim serialized bytes, in-memory bytes, and five repeated `readRDS()` timings plus prediction equivalence. Target medoid derivative ≤5 MB and no-baseline ≤3 MB; every other runtime artifact must shrink materially or document irreducible model/spectral state.
- **Generated docs**: edit roxygen/package metadata, run configured roxygen2 8.0.0, and inspect `NAMESPACE`/`man/*.Rd`; never hand-edit generated output.
- **Bundled app/pipeline**: update the Upload/source-routing, Run/settings snapshot, Identification, Simple Metadata, and Spectrum Plot boxes in `pipeline-diagram.html`. Standard upload paths converge on the same reader; dense matching uses public bounded `match_spec()`, and plots apply display-only normalization without changing canonical data.
- **Hosted impact**: shared `R/`, app fallback, staged libraries, and canonical URLs change. Publish the landing page, app, pkgdown, sitemap, and social metadata under `https://www.openanalysis.org/OpenSpecyV2/`. Keep the package/WASM dependency closure pinned, but fetch latest slim medoid/model data at action time and record their resolved hashes; run `-HostedAppStatic`, exact-artifact preflight, and the release-triggered clean wasm rebuild.

## Package Surfaces

- `R/build_lib.R`: assessment collection/review, overall-only accuracy, release sanitizer, model slimming, standalone assessment promotion, aggregate/manifest behavior.
- `R/raman_hdpe.R`, package/extdata objects, `README.md`, and vignettes: correct license metadata and separate beginner from advanced guidance.
- `R/manage_lib.R`, `R/zzz.R`, `R/match_spec.R`, `R/automate_particle_analysis.R`: AWS semantics and bounded public matching; regenerate affected Rd files.
- `inst/shiny/{global,ui,server}.R`, `site/`, and `pipeline-diagram.html`: native upload, control placement/order, metadata, normalization, and legend interaction.
- `tests/testthat/test-build_lib.R` and model/matching/app tests: compact accuracy rows, bounded parity/memory behavior, app-state behavior, and guarded integration.
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
- [x] Add canonical `library_name`, retain reviewed broad source categories, reassign undersupported classes, audit every source library, and rebuild with explicit complete-drop assessments.
- [x] Remove captured glmnet training calls, publish selected-lambda-only logistic fits at or below the 12.55 MB historical derivative model, disable random forests in routine builds, assess deployed medoids/models once without retraining, and resume the completed build from model checkpoints.
- [x] Correct `raman_hdpe` licensing and shorten README while preserving advanced guidance in vignettes.
- [x] After maintainer upload, switch routine consumers to latest AWS objects, retain opt-in historical revision tests, and validate current downloads plus staged hashes and type-specific matches.
- [x] Replace sparse mixed assessment unions with coherent long-form review tables, restore compact error-mode accuracy comparisons without retraining, enforce the 10% missingness ceiling, and retain detailed evidence separately.
- [x] Route H5 batch paths and `all_cell_id` through bounded FileSpecs chunks; reproduce the 4.94 GB eager-allocation cause and verify a real 2,384-spectrum slab without `bad_alloc`.
- [x] Restore overall/macro/class-count old/new assessment rows and rerun assessments from completed release `2c1b3ce60210` without rebuilding libraries/models.
- [x] Expose bounded `match_spec(batch_size=)` and route in-memory `all_cell_id` plus the app through it; document, benchmark, and prove dense-result parity.
- [x] Complete the requested upload, Advanced-control, metadata, display-normalization, contact, logistic-overlay, and pre-Run quantification interactions; update the pipeline diagram and targeted browser coverage.
- [ ] Complete OPUS, code-analysis, encoding/licensing, test-runtime, vignette, URL, reverse-dependency, exact-tarball, and multi-platform CRAN gates.
- [ ] Run `-HostedAppStatic`, matching-artifact preflight, and clean wasm build once on the final rebuilt/pinned candidate; reconcile evidence, processes, `git status`, and scratch cleanup.

## Verification And Open Questions

- Focused order: sanitizer/assessment/model tests → payload benchmark/subset probe → full rebuild → AWS integration → docs/vignettes → full tests/check → hosted/release gates. Reuse evidence only while covered code, artifacts, pins, and contracts are unchanged.
- Upload/publishing remains maintainer-owned. Routine consumers deliberately follow current CloudFront objects; exact comparison tests must supply an explicit S3 version ID, while action manifests preserve the resolved content hashes.
- Confirm whether any external maintainer workflow consumes `reference_library_build.rds`; absent a consumer, omit the 688 MB aggregate and retain the manifest plus canonical `assessments.rds`.
- Confirm canonical 2.0 public/support URLs and third-party redistribution rights; remove unsupported links/assets rather than guessing replacements or relicensing.
- 2026-09-24: all seven latest runtime AWS objects pass download/load/type-specific matching; the served derivative model is 800,310 bytes. Historical comparisons retain optional explicit revision IDs.
- 2026-09-24 assessment release `fa1ea1d52eb5`: all runtime RDS files are byte-identical to `2c1b3ce60210`; assessments are 25.88 MB, compact review tables have no column over 10% missing, Nicolas Coca is retained, and no complete source library is dropped.
- Current R 4.3.3 candidate: full tests pass 3,958 assertions (two guarded AWS tests separately pass live), all 11 bundled browser scenarios and 358 hosted-static assertions pass, and the staged check rebuilds both CRAN vignettes with 0 errors, 0 warnings, and one explained valid-UTF-8 data NOTE in 10m. R 4.6.1 remains to be rerun after the current source changes; current CRAN reverse dependencies: none.
- Hosted static gate passes 357 assertions. The exact `41722b5` GitHub wasm artifact passes its 118-package closure, staged AWS library match (0.9715), 269 MB site assembly, and full desktop/mobile browser workflow after selecting the typed Raman medoid partition and accepting the 2.0 library label/quoted CSV contract. A clean post-fix CI rerun and R-patched/devel Linux/macOS checks remain, so the last two checklist items stay open.

## Approval Notes

- Approved by:
- Follow-up: implementation uses package, generated-doc, Windows-R, and quality-gate skills; push/pull remains unauthorized.
