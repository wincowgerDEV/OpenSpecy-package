# Feature Plan: CRAN Readiness Review, Cleanup, And Bundled Shiny App Port

**Feature dir**: `specs/005-cran-readiness`
**Date**: 2026-07-10
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Prepare OpenSpecy for a low-risk CRAN submission by resolving release blockers, dependency cleanup, generated artifacts, and bundled Shiny app packaging.
- Preserve spectral integrity, `OpenSpecy`/`Specs` object contracts, current user workflows, CRAN readiness, and a maintainable in-package app.

## Scope

- **In**: Prior CRAN cleanup, YAML removal, dependency ports, docs/check fixes, and porting `wincowgerDEV/OpenSpecy-shiny` into `inst/shiny/` with optimized assets, tests, and `run_app()` support.
- **Out**: Removing new particle/image/validation/Specs functions, broad numerical rewrites without benchmarks, full reference-library rebuild unless touched, direct pkgdown HTML edits, or bundling avoidable large/raw/orphaned Shiny assets.
- **Users**: CRAN users installing OpenSpecy, analysts using spectral workflows, and users launching the bundled app through `run_app()`.

## Review Findings To Address

- Previous CRAN cleanup is mostly implemented: YAML removed, `cluster`/`signal` runtime use ported, new particle/image/validation/Specs exports generated, and package checks recently passed except expected time-verification notes.
- `run_app(path = "system")` currently resolves to `system.file(package = "OpenSpecy")` and searches recursively for `app.R` or `server.R`/`ui.R`; this can support `inst/shiny/` but should prefer that installed app path explicitly after the port.
- Upstream app source is `https://github.com/wincowgerDEV/OpenSpecy-shiny`; current repo root exposes `global.R`, `server.R`, `ui.R`, `config.yml`, and a `www/` asset directory.
- Bundled app assets are the primary CRAN risk: images must be compressed/downsampled/deduplicated, orphaned/raw/generated files removed, and source/installed package size reported.
- Shiny testing must avoid routine long interactive launches while still checking app helpers, server/module logic where feasible, installed paths, required assets, and a CI/manual smoke test.

## Requirements

- R1. CRAN-installed package exports include all new particle/image/validation/Specs functions intended for this release.
- R2. Package examples, README, vignettes, and Rd signatures describe current behavior and use `wavenumber`, `TRUE`/`FALSE`, and runnable primary workflows.
- R3. YAML read/write/package-data support remains removed; JSON, CSV, and RDS support remain.
- R4. Packages explicitly called by package functions stay in `Imports`; app-only packages stay in `Suggests` with clear `run_app()` runtime messaging unless package functions call them directly.
- R5. `run_app()` launches the bundled installed app by default, still supports explicit local paths for development/testing, and does not silently download during CRAN checks.
- R6. The Shiny app port preserves app behavior without weakening `OpenSpecy` object contracts or moving app-specific logic into public package APIs without review.
- R7. Shiny assets bundled under `inst/shiny/` are optimized, deduplicated, and audited so no orphaned, raw source, obsolete generated, or excessive files ship.
- R8. Shiny changes include focused current-behavior tests: installed path/assets, sourceable helpers, headless server/module tests where feasible, and manual or CI-guarded smoke tests.
- R9. Network tests/downloads skip on CRAN/offline and guard the actual host/resource used, including CloudFront/AWS/GitHub variants.
- R10. Same-output code changes include benchmarks under `benchmarks/` with output equivalence and material-regression signaling.
- R11. Final release candidate passes focused tests, full tests, documentation regeneration, vignettes, package build/package-size checks, Shiny smoke validation, and `R CMD check --as-cran` or equivalent CI.

## Technical Decisions

- **Approach**: Preserve completed CRAN cleanup, then import the upstream app into `inst/shiny/`, prune/optimize assets, and adapt `run_app()`/tests to prefer installed app files without relying on network download.
- **Public API**: Keep `run_app()` as the user entrypoint; avoid new Shiny-specific public functions unless helper extraction has package value beyond the app.
- **Dependency policy**: App-only packages remain `Suggests` checked by `run_app()`; compression tooling is development-only unless already available in package dependencies.
- **OpenSpecy contract**: App code may call package workflows but must preserve `wavenumber`, `spectra`, `metadata`, identifiers, and object attributes.
- **Generated artifacts**: Do not edit `NAMESPACE`, `man/*.Rd`, or `docs/`; regenerate only from roxygen/metadata with the configured roxygen2 version and inspect generated diffs.
- **External resources**: GitHub source import is development-time only; runtime app launch from installed package must work offline except explicitly documented downloads with guards.
- **Reference workflow compatibility**: N/A unless app port changes library-building, matching defaults, IDs, or wavenumber axes.
- **Bundled Shiny app**: Use `inst/shiny/`; audit `www/` references, compress/downsample images, remove unused files, record size deltas, and add headless/installed-path/smoke tests.

## Package Surfaces

- `R/`: Finalize CRAN fixes and update `R/run_app.R` to prefer `system.file("shiny", package = "OpenSpecy")`, with explicit local/dev path support.
- `tests/testthat/`: Keep existing CRAN tests; add Shiny installed-path, required-asset, helper, and feasible `shiny::testServer()` tests.
- `benchmarks/`: Existing PAM/SG and particle legacy benchmarks remain; add only if the Shiny port changes same-output package functions.
- `workflows/`: Add or update an app asset-audit/compression helper if useful; keep generated assets out of source unless intentionally bundled.
- `inst/`: Add `inst/shiny/` from `OpenSpecy-shiny`; optimize `www/`; remove orphaned/duplicate/raw/generated files before final checks.
- `vignettes/README/pkgdown`: Update source docs to describe the bundled app; do not patch pkgdown HTML directly.
- `DESCRIPTION`: Reconcile app dependencies as `Imports` vs `Suggests`, package metadata, `RoxygenNote`, and release date/version.
- `NEWS.md`: Add entries for CRAN cleanup, dependency ports, bug fixes, docs, and the bundled Shiny app.
- Bundled Shiny app: Include asset audit, image compression report, installed-size/source-size impact, headless tests, and manual/CI smoke test.

## Work Checklist

- [x] Confirm intended public exports and ensure new particle/image/validation/Specs APIs are generated into `NAMESPACE`.
- [x] Remove YAML support and fixture from package code, tests, docs, data, and dependencies.
- [x] Port `cluster::pam()` reduction and `signal` Savitzky-Golay filtering with tests plus same-output benchmarks.
- [x] Fix release-blocking behavior in readers, visual-image helpers, Specs, particle automation, and startup/check code.
- [x] Add exact-settings `analyze_features()` export benchmark for `tiny_map`/`test_lib` and align `automate_particle_analysis()` return/export names.
- [ ] Import `wincowgerDEV/OpenSpecy-shiny` into `inst/shiny/` at a recorded commit; keep top-level app files and `www/` structure installable.
- [ ] Audit, compress/downsample, deduplicate, and prune Shiny assets; report source tarball and installed package size before/after.
- [ ] Update `run_app()` and docs so default launch uses bundled `inst/shiny/`, while explicit local paths and guarded remote/dev workflows remain clear.
- [ ] Add focused Shiny tests for installed app path, required static assets, sourceable helpers, feasible server/module logic, and skipped CI/manual smoke coverage.
- [ ] Refresh roxygen docs, README, included vignettes, NEWS, and cran-comments from source; regenerate generated artifacts with the configured roxygen2 version and inspect diffs.
- [ ] Run focused tests, relevant benchmarks, full package tests, vignettes, build/package-size checks, Shiny smoke validation, and CRAN checks.

## Verification

- Focused tests: `devtools::test(filter = "run_app|io_spec|build_lib|smooth_intens|visual_image|read_ext|Specs|automate_particle_analysis|read_multi|process_spec|match_spec")`.
- Shiny app tests/asset audit/smoke test: installed path via `system.file("shiny", package = "OpenSpecy")`; required `global.R`/`server.R`/`ui.R`/`config.yml`/`www`; orphan reference scan; image size/compression report; `shiny::testServer()` where feasible; manual or CI-guarded app launch.
- Dependency checks: `_R_CHECK_DEPENDS_ONLY_=true`, `_R_CHECK_SUGGESTS_ONLY_=true`, explicit checks after YAML/cluster/signal removal, and app-only dependency availability messaging.
- Toolchain/version preflight: confirm installed roxygen2 matches `Config/roxygen2/version` before regeneration.
- `devtools::document()`: run once with configured roxygen2; inspect generated diffs for aliases, exports, authors, references, and stale signatures.
- Full checks: `devtools::test()`, `devtools::build_vignettes()`, `R CMD build`, `R CMD check --as-cran`, and GitHub Actions matrix on Windows/macOS/Linux R release/devel/oldrel.
- Benchmarks: required for same-output reader, processor, matcher, dependency-port, or run_app behavior changes; compare output equivalence and flag >10% slowdown.
- Reference-library/long workflow staging: N/A unless app port changes library artifacts or core matching/library-generation behavior.
- CRAN release checks: inspect package tarball contents/size, installed package size, URLs/DOIs, examples, vignettes, `cran-comments.md`, reverse dependencies, and no unexpected files.

## Risks And Follow-Up

- Shiny app images may push CRAN package size over acceptable limits; reduce assets first and externalize optional media only with offline-safe behavior.
- Server code may be hard to test headlessly if it is not modular; extract sourceable helpers only when it improves testability without creating speculative public APIs.
- Historical app remote-download behavior may still be useful for development, but CRAN default should favor the bundled offline app.

## Approval Notes

- Approved by:
- Follow-up: After CRAN submission, consider a separate lifecycle/deprecation plan for low-level helper exports and visualization/read-function consolidation.
