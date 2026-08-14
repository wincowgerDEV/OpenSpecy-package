# Feature Plan: Scientific Integrity Hardening (Identification, I/O, Reference Library)

**Feature dir**: `specs/013-scientific-integrity-hardening`  
**Date**: 2026-08-14  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Fix correctness/integrity gaps a full-repo scientific-accuracy recon surfaced after 012 closed: unvalidated AI-classification model selection, unverified reference-library downloads, non-reproducible `Specs` compression, silent binary-reader failure paths, and two doc bugs.  
**Change class**: package/scientific (highest); minor presentation/docs alongside.

## Goal

- Close correctness gaps in identification, reference-library integrity, and binary readers found during recon.
- Make `ai_classify()`/`build_model_lib()`'s model selection defensible (cross-validated) instead of always the least-regularized fit.

## Scope

- **In**: README/roxygen doc bugs (R1-R2); `read_opus_raw()`/`read_envi()` silent-failure paths (R3-R4); `build_model_lib()`/`ai_classify()` lambda selection (R5); `get_lib()`/`check_lib()` download validation and staleness signal (R6); `.kmeans_specs()` reproducibility (R7); `NEWS.md` v1.5.1 gap (R8).
- **Out**: `automate_particle_analysis()`'s `partial_collapse`/`nonspatial_collapse` divergence (see Risks — NOT actually fixed by 012 despite its checklist); `correct_spike()`/`assess_spec()`/`sig_noise()` threshold calibration; `Specs`'s narrow S3 method contract; performance/architecture (no compiled/parallel path); CRAN-readiness items (non-OSI license, `.Rbuildignore` gaps, pkgdown grouping); CI/governance hygiene (`release-file-system-image.yml`, `quality-gates.ps1` gaps). Each is a candidate for its own future concise plan.
- **Users**: Package users calling `read_opus()`, `read_envi()`, `ai_classify()`, `get_lib()`, `as_Specs()` directly; README readers following the quick-start example.

## Requirements

- R1. README.md's single-spectrum example uses `spec_lib$wavenumber` (not `$wavenumbers`) so the conform-to-library-axis step actually runs.
- R2. `conform_spec()`'s roxygen `@return` (`R/conform_spec.R`) documents its own `OpenSpecy` return value, not `adj_intens()`'s.
- R3. `read_opus_raw()` raises a clear, file-named error on parse failure instead of returning an unhandled `"try-error"` object that produces a confusing downstream error.
- R4. `read_envi()` hard-errors (not warns) when no `wavelength` field is found, instead of silently substituting band indices as wavenumbers.
- R5. `build_model_lib()` selects lambda via `cv.glmnet()` instead of always `min(model$lambda)`; `ai_classify()` consumes the cross-validated selection. Document the expected shift in classification output.
- R6. `get_lib()` checks `download.file()`'s return status and validates the downloaded artifact before treating a fetch as successful; `check_lib()` can report whether a cached library matches the requested revision.
- R7. `.kmeans_specs()` (`R/Specs.R`) has a documented, reproducible default so `as_Specs(..., steps = c("kmeans", ...))` is deterministic without the caller supplying `nstart`.
- R8. `NEWS.md` includes an entry for tagged release `v1.5.1`.

## Technical Decisions

- **Approach**: Eight small, independent fixes; no wide refactor. Each ships with its own focused test.
- **Public API**: No new exported arguments. R5-R7 change internal defaults/behavior, not signatures; if `check_lib()`'s return shape changes for R6, document it as a minor breaking change in `NEWS.md`.
- **Dependencies**: `glmnet::cv.glmnet()` is already available via the existing `glmnet` Import; no new dependency.
- **OpenSpecy contract**: N/A, except that `read_opus_raw()`/`read_envi()` error paths must fail before any malformed `OpenSpecy` object is constructed.
- **Generated artifacts**: `devtools::document()` after the `conform_spec()` roxygen fix; confirm installed roxygen2 matches `Config/roxygen2/version` first; inspect the `man/`/`NAMESPACE` diff.
- **External resources**: `get_lib()` changes must keep the existing host guard and offline-skip behavior for network tests.
- **Bundled Shiny app**: N/A — `ai_classify()` is not called anywhere in `inst/shiny` (confirmed by grep); the other fixes are package-internal I/O and library-management paths the app doesn't call directly.
- **Hosted Shinylive/WebAssembly app**: N/A — no `inst/shiny` or wasm-repo surface touched.

## Package Surfaces

- `R/`: `read_opus_raw.R`, `read_envi.R`, `build_lib.R`, `match_spec.R` (`ai_classify`), `manage_lib.R`, `Specs.R`, `conform_spec.R` (roxygen only).
- `tests/testthat/`: `test-read_opus.R` (or new `test-read_opus_raw.R`), `test-read_envi.R`, `test-build_lib.R`, `test-match_spec.R` (restore/replace the disabled numeric assertion at line 150), `test-manage_lib.R` (add an offline-safe unit test, not just network-gated ones), `test-Specs.R`.
- `benchmarks/`: N/A — these are behavior/correctness changes, not same-output performance work.
- `workflows/`: N/A.
- `.github/workflows/`: unchanged.
- `inst/`: unchanged.
- `site/vignettes/README/pkgdown`: `README.md` (R1); `man/conform_spec.Rd` regenerated (R2).
- `DESCRIPTION`: unchanged.
- `NEWS.md`: entries for R3-R8 (breaking-ish behavior changes called out explicitly), plus the v1.5.1 backfill.

## Work Checklist

- [ ] Fix `README.md`'s `spec_lib$wavenumbers` typo.
- [ ] Fix `conform_spec()`'s roxygen `@return` in `R/conform_spec.R`; regenerate docs.
- [ ] Replace `read_opus_raw()`'s bare `try()` with a clear-erroring `tryCatch()`; add a corrupt-file test.
- [ ] Escalate `read_envi()`'s missing-wavelength fallback from warning to error; update its test.
- [ ] Switch `build_model_lib()`/`ai_classify()` to `cv.glmnet()`-selected lambda; update tests and the disabled assertion in `test-match_spec.R:150`.
- [ ] Add download-status and artifact validation to `get_lib()`; add a cached-revision signal to `check_lib()`; add an offline-safe unit test.
- [ ] Give `.kmeans_specs()` a reproducible default; assert determinism in `test-Specs.R`.
- [ ] Add the missing `NEWS.md` entry for `v1.5.1`.

## Verification

- Direct regression: one behavioral test per requirement above (see Work Checklist).
- Focused tests: `devtools::test(filter = "read_opus|read_envi|build_lib|match_spec|manage_lib|Specs|conform_spec")`.
- Toolchain/version preflight: confirm installed roxygen2 vs `Config/roxygen2/version` before `devtools::document()`.
- `devtools::document()` trigger: yes, for the `conform_spec()` roxygen fix; inspect `man/`/`NAMESPACE` diff.
- Full `devtools::test()` trigger: once, on the final candidate.
- `devtools::check()`: deferred to release-facing work, not required for this tranche.
- Benchmarks: N/A (behavior changes, not same-output improvements).
- Shiny/Shinylive: N/A, per Technical Decisions.
- Reusable evidence: none yet; this is new work.

## Risks And Open Questions

- `partial_collapse`/`nonspatial_collapse` still diverge at the package level (`R/automate_particle_analysis.R:658-702`, `.partition_particle_map()`) even though spec 012's checklist marks the equivalent item complete — the corrected two-stage cluster to collapse flow exists only inside `inst/shiny/server.R:1157-1263`. Not fixed here; flagging so it isn't mistaken for resolved.
- R5's lambda-selection change will shift `ai_classify()`'s numeric output for any previously-downloaded/cached model library — document prominently in `NEWS.md` even though no exported signature changes.
- R4's warning-to-error escalation is a breaking change for pipelines currently tolerating index-fallback wavenumbers — needs a clear `NEWS.md` breaking-change note.
- A larger backlog was intentionally deferred (see Scope: Out) — performance/architecture, `Specs` method-contract gaps, uncited scientific thresholds, CRAN-readiness (notably the non-OSI CC BY 4.0 license, never flagged elsewhere as a submission risk), and CI/governance hygiene (`release-file-system-image.yml` confirmed dead, `quality-gates.ps1` has no automatic tranche detection). Each is a candidate for its own future concise plan.

## Approval Notes

- Approved by: Pending
- Follow-up: This recon also surfaced CRAN-readiness and CI-governance backlogs not included here; raise with maintainer for separate tranches.
