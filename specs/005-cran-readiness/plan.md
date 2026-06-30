# Feature Plan: CRAN Readiness Review And Cleanup

**Feature dir**: `specs/005-cran-readiness`
**Date**: 2026-06-30
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Prepare OpenSpecy for a low-risk CRAN submission by resolving release blockers, stale generated artifacts, dependency bloat, and confusing public surfaces.
- Preserve spectral integrity, `OpenSpecy`/`Specs` object contracts, current user workflows, and downstream Shiny compatibility while avoiding unnecessary pre-CRAN breaking changes.

## Scope

- **In**: Public API audit, dependency audit, roxygen/source docs cleanup, release metadata, network/cache behavior, generated artifact regeneration, focused tests, benchmarks for same-output changes, and full CRAN checks.
- **Out**: Removing established exports without a deprecation path, broad numerical algorithm rewrites without benchmarks, full reference-library rebuild unless touched, pkgdown HTML edits, and Shiny app repository changes.
- **Users**: CRAN users installing OpenSpecy and analysts using `read_any()`, `process_spec()`, `match_spec()`, map/particle workflows, and library management.

## Review Findings To Address

- Generated artifacts are stale: `NAMESPACE` is missing new roxygen exports from `R/automate_particle_analysis.R`, `R/particle_image.R`, `R/validation_metrics.R`, `R/visual_image.R`, and `R/Specs.R`; `man/read_ext.Rd` still shows `read_h5(collapse = T)` and `man/Specs.Rd` shows older `as_Specs()` arguments.
- Release metadata is inconsistent: `DESCRIPTION` is `1.7.1` with `Date: 2025-09-29`, `NEWS.md` starts at `1.7.0`, `README.md` title says `Open Specy 1.0`, the README DOI badge differs from the current package DOI, and `cran-comments.md` lists old check environments.
- Cache/download defaults may write into the installed package tree: `get_lib(path = "system")`, `rm_lib(path = "system")`, and `run_app(path = "system")` resolve to `system.file(...)`.
- Network tests need exact-host guards: `tests/testthat/test-manage_lib.R` skips on `osf.io` even when `get_lib(..., aws = TRUE)` downloads from `d2jrxerjcsjhs7.cloudfront.net`.
- Narrow dependency candidates need triage: `shiny`, `plotly`, `hdf5r`, `glmnet`, `cluster`, `hyperSpec`, `caTools`, `jpeg`, `mmand`, and `signal`; keep core/performance packages such as `data.table`, `matrixStats`, `digest`, `jsonlite`, and `yaml` unless evidence says otherwise.
- Roxygen/source doc issues found statically: mojibake references in `R/OpenSpecy-package.R` and `R/manage_lib.R`, README typo `spec_lib$wavenumbers`, `Web Assemply`, old `T`/`F` examples, and unclear low-level helper guidance.
- Focused code risks found statically: `R/visual_image.R` calls `grDevices::readbitmap()`, `R/read_ext.R` has an `.xyz` branch assigning `colnames(xy)` before `xy` exists, and `R/run_app.R` hard-codes `.rs.invokeShinyWindowExternal`.

## Requirements

- R1. CRAN-installed package exports must match intended roxygen exports after confirming which new helpers are public and which should remain internal.
- R2. Package examples, README, vignettes, and Rd signatures must describe current behavior and use `wavenumber`, `TRUE`/`FALSE`, and runnable primary workflows.
- R3. Downloading libraries or the Shiny app must default to a user-writable cache or explicit user path, never the installed package directory.
- R4. Optional external features must fail with clear install guidance when a Suggested package is absent; core package load must not require heavy optional UI/H5/model dependencies unnecessarily.
- R5. API simplification must favor documented primary workflows (`read_any()`, `process_spec()`, `match_spec()`, `automate_particle_analysis()`) while preserving composable helpers and deprecating rather than abruptly removing user-facing exports.
- R6. Network tests must skip on CRAN/offline and guard the actual host/resource used, including CloudFront/AWS variants.
- R7. Same-output code changes must include benchmark comparisons under `benchmarks/` with output equivalence and material-regression signaling.
- R8. Final release candidate must pass focused tests, full tests, documentation regeneration, vignettes, package build checks, and `R CMD check --as-cran` or equivalent CI matrix.

## Technical Decisions

- **Approach**: Fix release blockers first, then make conservative dependency/API cleanups only when tests and docs can prove no user workflow is broken.
- **Public API**: Keep existing exports before CRAN unless they are unexported in the current CRAN release or clearly accidental; mark future consolidation/deprecation in NEWS rather than silently dropping functions.
- **Dependency policy**: Prefer moving narrow UI/file/model dependencies to `Suggests` with `requireNamespace()` guards; port only small, stable readers/helpers when license, numerical equivalence, and benchmarks are clear.
- **OpenSpecy contract**: Any reader, processor, matcher, filter, collapse, image, or `Specs` change must preserve `wavenumber`, `spectra`, `metadata`, column/row alignment, identifiers, and object attributes.
- **Generated artifacts**: Do not edit `NAMESPACE`, `man/*.Rd`, or `docs/`; resolve roxygen2 version mismatch (`Config/roxygen2/version: 8.0.0` vs `RoxygenNote: 7.3.2`) before `devtools::document()`.
- **External resources**: OSF, CloudFront, and GitHub downloads must be explicit, guarded, cached outside the package tree, and skipped during CRAN checks.
- **Reference workflow compatibility**: N/A unless library-building, matching defaults, IDs, or wavenumber axes change; then run staged subset/temp-output comparisons against legacy artifacts.

## Package Surfaces

- `R/`: Audit exports and docs; likely touch `R/manage_lib.R`, `R/run_app.R`, `R/visual_image.R`, `R/read_ext.R`, `R/Specs.R`, `R/OpenSpecy-package.R`, and roxygen for primary workflow pages.
- `tests/testthat/`: Add focused tests for cache paths, optional dependency guards, exact network skips, generated-export availability, image BMP fallback, `.xyz` reading, and `run_app()` launch argument handling.
- `benchmarks/`: Add/update only for same-output dependency ports or performance refactors; keep legacy comparison code here, not in `tests/`.
- `workflows/`: Unchanged unless reference-library functions or IDs change; if changed, run staged compatibility checks.
- `vignettes/README/pkgdown`: Update README and included vignettes from source; do not patch pkgdown HTML.
- `DESCRIPTION`: Reconcile `Version`, `Date`, dependency fields, `RoxygenNote`, URLs, and optional dependency classification.
- `NEWS.md`: Add a `1.7.1`/release entry covering CRAN cleanup, dependency moves, API deprecations, bug fixes, and doc updates.
- External Shiny compatibility: Keep `run_app()` package-side compatible with the separate `wincowgerDEV/OpenSpecy-shiny`; no Shiny app code in this repo.

## Work Checklist

- [ ] Confirm intended public exports against current CRAN exports and roxygen tags; decide accidental vs deliberate new APIs.
- [ ] Fix release-blocking behavior in `R/manage_lib.R`, `R/run_app.R`, `R/visual_image.R`, and `R/read_ext.R`.
- [ ] Triage dependencies in `DESCRIPTION`; move narrow optional packages to `Suggests` only with guards, tests, and examples updated.
- [ ] Refresh roxygen docs, README, included vignettes, NEWS, and cran-comments from source.
- [ ] Regenerate generated artifacts with the configured roxygen2 version and inspect `NAMESPACE`/`man/*.Rd` diffs immediately.
- [ ] Run focused tests, relevant benchmarks, full package tests, vignettes, build/package-size checks, and CRAN checks.

## Verification

- Focused tests: `devtools::test(filter = "manage_lib|run_app|visual_image|read_ext|read_h5|Specs|read_multi|process_spec|match_spec")`.
- Dependency checks: `_R_CHECK_DEPENDS_ONLY_=true`, `_R_CHECK_SUGGESTS_ONLY_=true`, and explicit tests with optional packages absent where feasible.
- Toolchain/version preflight: verify R, roxygen2, pandoc, and package dependency versions before regeneration.
- `devtools::document()`: run once with configured roxygen2; inspect generated diffs for aliases, exports, authors, references, and stale signatures.
- Full checks: `devtools::test()`, `devtools::build_vignettes()`, `R CMD build`, `R CMD check --as-cran`, and GitHub Actions matrix on Windows/macOS/Linux R release/devel/oldrel.
- Benchmarks: required for any same-output reader, processor, matcher, or dependency-port change; compare output equivalence and flag >10% slowdown.
- CRAN release checks: inspect package tarball contents/size, URLs/DOIs, examples, vignettes, `cran-comments.md`, reverse dependencies, and no unexpected files.

## Risks And Open Questions

- Should `hdf5r`, `plotly`, `shiny`, `glmnet`, `cluster`, `hyperSpec`, `caTools`, and `jpeg` become optional `Suggests`, or are any considered core enough to remain hard Imports?
- Is any new roxygen-exported API from the particle/image/validation/Specs work intentionally public for this CRAN release, or should some helpers stay internal until the next release?
- Should `path = "system"` remain as a backward-compatible alias with a warning, or should defaults change directly to `tools::R_user_dir("OpenSpecy", "cache")`?

## Approval Notes

- Approved by:
- Follow-up: After CRAN submission, consider a separate lifecycle/deprecation plan for low-level helper exports and visualization/read-function consolidation.
