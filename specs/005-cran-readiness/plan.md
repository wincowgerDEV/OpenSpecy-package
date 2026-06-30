# Feature Plan: CRAN Readiness Review And Cleanup

**Feature dir**: `specs/005-cran-readiness`
**Date**: 2026-06-30
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Prepare OpenSpecy for a low-risk CRAN submission by resolving release blockers, stale generated artifacts, dependency cleanup, and confusing public surfaces.
- Preserve spectral integrity, `OpenSpecy`/`Specs` object contracts, current user workflows, and downstream Shiny compatibility while avoiding unnecessary pre-CRAN breaking changes.

## Scope

- **In**: Public API audit, dependency audit, YAML support removal, roxygen/source docs cleanup, release metadata, generated artifact regeneration, focused tests, benchmarks for same-output changes, and full CRAN checks.
- **Out**: Removing new particle/image/validation/Specs functions from this release, changing `run_app(path = "system")` cache behavior, broad numerical algorithm rewrites without benchmarks, full reference-library rebuild unless touched, pkgdown HTML edits, and Shiny app repository changes.
- **Users**: CRAN users installing OpenSpecy and analysts using `read_any()`, `process_spec()`, `match_spec()`, map/particle workflows, and library management.

## Review Findings To Address

- `devtools::document()` was not run: installed roxygen2 is `7.3.2`, but `DESCRIPTION` requires `Config/roxygen2/version: 8.0.0`; resolve the generator version before regenerating docs.
- `devtools::test()` passed locally on 2026-06-30: 844 pass, 30 warnings from expected warning tests, 3 skips (`manage_lib` CI-only and OSF model unavailable).
- `devtools::check(document = FALSE)` ran `--as-cran` and failed release criteria with 0 errors, 2 warnings, 3 notes.
- Generated artifacts are stale: check reports codoc mismatches for `Specs`, `as_OpenSpecy`, and `read_h5`; `NAMESPACE` is missing new roxygen exports from `R/automate_particle_analysis.R`, `R/particle_image.R`, `R/validation_metrics.R`, `R/visual_image.R`, and `R/Specs.R`.
- Release metadata is inconsistent: `DESCRIPTION` is `1.7.1` with `Date: 2025-09-29`, `NEWS.md` starts at `1.7.0`, `README.md` title says `Open Specy 1.0`, the README DOI badge differs from the current package DOI, and `cran-comments.md` lists old check environments.
- Network tests need exact-host guards: `tests/testthat/test-manage_lib.R` skips on `osf.io` even when `get_lib(..., aws = TRUE)` downloads from `d2jrxerjcsjhs7.cloudfront.net`.
- YAML is isolated to `R/io_spec.R`, `inst/extdata/raman_hdpe.yml`, `tests/testthat/test-io_spec.R`, `tests/testthat/test-manage_spec.R`, and vignettes; remove YAML read/write support, fixture, docs, and dependency.
- `cluster` is only used for `cluster::pam()` inside `.pam_group_ids()`/`reduce_lib()`; port or replace that medoid selection in-package with tests and a benchmark against current output.
- `signal` is still used by `.sgfilt()`/`.sgfilt_matrix()` and tests/benchmarks; remove it by replacing Savitzky-Golay coefficient/filter code in-package and benchmark equivalent output.
- Roxygen/source doc issues found statically: mojibake references in `R/OpenSpecy-package.R` and `R/manage_lib.R`, README typo `spec_lib$wavenumbers`, `Web Assemply`, old `T`/`F` examples, and unclear low-level helper guidance.
- Check/probes confirmed code risks: `grDevices::readbitmap()` is missing, `.xyz` reading errors with `object 'xy' not found`, and new code needs imports/globals for `.row_id`, `y`, `tail`, `png`, and `dev.off`.

## Requirements

- R1. CRAN-installed package exports must include the new particle, image, validation, and Specs functions intended for this release.
- R2. Package examples, README, vignettes, and Rd signatures must describe current behavior and use `wavenumber`, `TRUE`/`FALSE`, and runnable primary workflows.
- R3. Remove YAML as a read/write/package-data format and keep JSON, CSV, and RDS support.
- R4. Packages explicitly called by package functions stay in `Imports` unless the call is removed or ported; packages used only by examples, tests, vignettes, workflows, or the external Shiny app stay in `Suggests`.
- R5. API simplification must favor documented primary workflows (`read_any()`, `process_spec()`, `match_spec()`, `automate_particle_analysis()`) while preserving composable helpers needed for this release.
- R6. Network tests must skip on CRAN/offline and guard the actual host/resource used, including CloudFront/AWS variants.
- R7. Same-output code changes must include benchmark comparisons under `benchmarks/` with output equivalence and material-regression signaling.
- R8. Final release candidate must pass focused tests, full tests, documentation regeneration, vignettes, package build checks, and `R CMD check --as-cran` or equivalent CI matrix.

## Technical Decisions

- **Approach**: Fix release blockers first, then make conservative dependency/API cleanups only when tests and docs can prove no user workflow is broken.
- **Public API**: Export all new particle/image/validation/Specs functions intended by roxygen for this release; leave any broader consolidation/deprecation for a post-CRAN lifecycle pass.
- **Dependency policy**: Remove YAML support entirely; port `cluster::pam()` and `signal` Savitzky-Golay usage; keep explicit package-function calls in `Imports` and non-called support packages in `Suggests`.
- **OpenSpecy contract**: Any reader, processor, matcher, filter, collapse, image, or `Specs` change must preserve `wavenumber`, `spectra`, `metadata`, column/row alignment, identifiers, and object attributes.
- **Generated artifacts**: Do not edit `NAMESPACE`, `man/*.Rd`, or `docs/`; resolve roxygen2 version mismatch (`Config/roxygen2/version: 8.0.0` vs `RoxygenNote: 7.3.2`) before `devtools::document()`.
- **External resources**: OSF, CloudFront, and GitHub downloads must be explicit, guarded, cached outside the package tree, and skipped during CRAN checks.
- **Reference workflow compatibility**: N/A unless library-building, matching defaults, IDs, or wavenumber axes change; then run staged subset/temp-output comparisons against legacy artifacts.

## Package Surfaces

- `R/`: Audit exports and docs; likely touch `R/io_spec.R`, `R/build_lib.R`, `R/smooth_intens.R`, `R/visual_image.R`, `R/read_ext.R`, `R/Specs.R`, `R/automate_particle_analysis.R`, `R/zzz.R`, `R/OpenSpecy-package.R`, and primary workflow roxygen pages.
- `tests/testthat/`: Update YAML tests, add focused tests for exact network skips, generated-export availability, image BMP fallback, `.xyz` reading, SG filtering, PAM/medoid reduction, and check-note globals/imports.
- `benchmarks/`: Add same-output comparisons for cluster/PAM removal and signal/Savitzky-Golay removal; keep legacy comparison code here, not in `tests/`.
- `workflows/`: Unchanged unless reference-library functions or IDs change; if changed, run staged compatibility checks.
- `vignettes/README/pkgdown`: Update README and included vignettes from source; do not patch pkgdown HTML.
- `DESCRIPTION`: Reconcile `Version`, `Date`, remove `yaml`, remove `cluster`/`signal` after ports, dependency fields, `RoxygenNote`, URLs, and optional dependency classification.
- `NEWS.md`: Add a `1.7.1`/release entry covering CRAN cleanup, dependency moves, API deprecations, bug fixes, and doc updates.
- External Shiny compatibility: Keep `run_app()` package-side compatible with the separate `wincowgerDEV/OpenSpecy-shiny`; no Shiny app code in this repo.

## Work Checklist

- [ ] Confirm intended public exports and ensure new particle/image/validation/Specs APIs are generated into `NAMESPACE`.
- [ ] Remove YAML support and fixture from `R/io_spec.R`, `inst/extdata/`, tests, vignettes, `README.md`, `DESCRIPTION`, and roxygen.
- [ ] Port `cluster::pam()` reduction and `signal` Savitzky-Golay filtering with tests plus same-output benchmarks.
- [ ] Fix release-blocking behavior in `R/visual_image.R`, `R/read_ext.R`, `R/automate_particle_analysis.R`, `R/Specs.R`, and `R/zzz.R`.
- [ ] Refresh roxygen docs, README, included vignettes, NEWS, and cran-comments from source.
- [ ] Regenerate generated artifacts with the configured roxygen2 version and inspect `NAMESPACE`/`man/*.Rd` diffs immediately.
- [ ] Run focused tests, relevant benchmarks, full package tests, vignettes, build/package-size checks, and CRAN checks.

## Verification

- Pre-plan evidence: `devtools::test()` passed; `devtools::check(document = FALSE)` found 2 warnings and 3 notes; `.codex-logs/` was a temporary check artifact and should be removed before final checks.
- Focused tests: `devtools::test(filter = "io_spec|build_lib|smooth_intens|visual_image|read_ext|Specs|automate_particle_analysis|read_multi|process_spec|match_spec")`.
- Dependency checks: `_R_CHECK_DEPENDS_ONLY_=true`, `_R_CHECK_SUGGESTS_ONLY_=true`, and explicit checks after removing YAML/cluster/signal.
- Toolchain/version preflight: resolve roxygen2 `8.0.0` vs installed `7.3.2` before regeneration.
- `devtools::document()`: run once with configured roxygen2; inspect generated diffs for aliases, exports, authors, references, and stale signatures.
- Full checks: `devtools::test()`, `devtools::build_vignettes()`, `R CMD build`, `R CMD check --as-cran`, and GitHub Actions matrix on Windows/macOS/Linux R release/devel/oldrel.
- Benchmarks: required for any same-output reader, processor, matcher, or dependency-port change; compare output equivalence and flag >10% slowdown.
- CRAN release checks: inspect package tarball contents/size, URLs/DOIs, examples, vignettes, `cran-comments.md`, reverse dependencies, and no unexpected files.

## Risks And Open Questions

- Should `Config/roxygen2/version` be changed to the installed/release generator or should roxygen2 8.0.0 be installed before documentation regeneration?
- What exact compatibility tolerance is acceptable for the in-package Savitzky-Golay replacement compared with current `signal` output?
- For the PAM port, should the implementation match `cluster::pam(pamonce = 6)` exactly, or is deterministic medoid equivalence on representative library groups sufficient?

## Approval Notes

- Approved by:
- Follow-up: After CRAN submission, consider a separate lifecycle/deprecation plan for low-level helper exports and visualization/read-function consolidation.
