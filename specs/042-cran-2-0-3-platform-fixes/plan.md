# Feature Plan: CRAN 2.0.3 Platform Fixes

**Feature dir**: `specs/042-cran-2-0-3-platform-fixes`  
**Date**: 2026-10-06  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Repair the platform-dependent CRAN 2.0.1 test failures reported for OPUS reading, default library construction, and particle companion-image discovery, then export a verified 2.0.3 source tarball.  
**Change class**: Hosted/release (package/scientific fixes plus a CRAN resubmission).

## Goal

- Make the affected tests portable across Windows, macOS arm64, Linux arm64, BLIS, and MKL checks without weakening scientific object validation.
- Deliver a new 2.0.3 source package that passes focused, full-suite, hosted-static, and exact-tarball CRAN checks.

## Scope

- **In**: Diagnose all supplied live logs; accept documented non-fatal `read_opus()` warnings while still validating returned objects; make default `build_lib()` outputs satisfy the `OpenSpecy` contract under BLAS/SVD variation; canonicalize discovered image paths consistently on macOS; version, NEWS, tests, and release artifact.
- **Out**: New public APIs, reference-library regeneration, bundled Shiny behavior, hosted UI changes, dependency/pin changes, deployment, and remote synchronization.
- **Users**: CRAN users on all supported R platforms and maintainers preparing the corrected submission.

## Requirements

- R1. Single- and multi-file OPUS fixtures may warn on platform parser details, but must return valid `OpenSpecy` objects with the existing expected dimensions, ranges, and metadata.
- R2. Every default `build_lib()` recipe must return a valid `OpenSpecy` object with finite usable spectra; numerical linear-algebra variation must not create all-missing columns.
- R3. Companion-image discovery and stored visual-image source paths must use the same canonical absolute path convention on macOS and other supported systems.
- R4. Package metadata and NEWS identify version 2.0.3, and the exported tarball contains no repository or scratch debris.

## Technical Decisions

- **Approach**: Preserve strict output assertions while replacing only the inappropriate silence assertion for OPUS. Reproduce and isolate the library failure at the numerical kernel before choosing a narrow stable implementation or tolerance. Normalize the discovered companion path at the production boundary, not only in tests.
- **Public API/dependencies**: No signature, export, or dependency changes.
- **OpenSpecy contract**: Preserve `wavenumber`, spectra/metadata alignment, unique IDs, and processing attributes; explicitly test each recipe with `check_OpenSpecy()` and finite-value counts.
- **Generated artifacts**: DESCRIPTION and NEWS only unless roxygen source must change; never edit generated Rd/NAMESPACE directly. If triggered, require roxygen2 8.0.0 and run `devtools::document()`.
- **Reference workflow compatibility**: No production rebuild. Use the existing eight-spectrum kernel; default recipe outputs and metadata/attributes must remain equivalent within numerical tolerance.
- **Performance and observability**: Subsecond eight-spectrum kernel; repeat benchmark only if production computation changes. Abort broader checks until the focused reproducer passes.
- **Bundled Shiny/pipeline diagram**: N/A; no `inst/shiny/` or analysis-pipeline change.
- **Hosted impact**: `R/` and `DESCRIPTION` are shared inputs. Run `-HostedAppStatic`; because this is release-facing, verify the exact package tar/check. No wasm rebuild because dependency closure, package image driver, pins, and hosted app source are unchanged.

## Package Surfaces

- `R/build_lib.R` or its numerical helper, `R/automate_particle_analysis.R`: narrow portability fixes as diagnosis requires; `R/read_opus.R` unchanged unless warnings reveal a real parser defect.
- `tests/testthat/test-build_lib.R`, `test-automate_particle_analysis.R`, `test-read_opus.R`: cross-platform regressions with strict object/content validation.
- `benchmarks/`: run/update the existing library-builder benchmark only if computation changes.
- `DESCRIPTION`, `NEWS.md`: version 2.0.3 and platform-fix entry.
- Generated docs, workflows, `.github/workflows/`, `inst/`, site/vignettes/README/pkgdown, wasm repo/pins/generated output: unchanged unless inspection proves otherwise.

## Work Checklist

- [x] Reproduce or reduce each linked platform failure and identify the production/test cause.
- [x] Implement focused OPUS, library-builder, and companion-path regressions and minimal fixes.
- [x] Bump `DESCRIPTION`/`NEWS.md` to 2.0.3; regenerate docs only if source documentation changes.
- [x] Run focused tests, any triggered benchmark, full tests, and `-HostedAppStatic`.
- [x] Stage, inspect, build, and run an exact-tarball `R CMD check --as-cran`; export and hash `OpenSpecy_2.0.3.tar.gz`.
- [x] Reconcile evidence, processes, status, and task scratch while preserving `.claude/settings.local.json`.

## Verification

- Focused: affected readers, normalization, particle analysis, library building, matrix processing, and Hamming similarity passed; the constant-spectrum pre-fix reproducer returned `NaN` and the corrected path returns finite zeros while preserving `NA` positions.
- Full: 4,286 passes, 30 expected warning-contract observations, and 2 guarded AWS integration skips; 0 failures.
- Hosted: `-HostedAppStatic` passed 370 assertions plus JS/R/PowerShell parsing; matching-artifact/browser N/A because hosted runtime/routes/interactions are unchanged.
- Documentation: configured and installed roxygen2 were 8.0.0; `devtools::document()` generated only `man/make_rel.Rd`, with `NAMESPACE` unchanged.
- Release: 549-file/19,671,634-byte isolated source staged successfully. Exact-tar `R CMD check --as-cran --no-manual` completed in 621.9 seconds with 0 errors, 0 warnings, and 2 environmental notes (one-day update age and unavailable time verification).
- Artifact: `OpenSpecy_2.0.3.tar.gz` has 250 entries, version/date 2.0.3/2026-10-06, no repository/scratch debris, 1,963,120 bytes, and SHA-256 `76857FBCAAB6271AC06907A31710558122192AE296646D3F1B6C8A83CD99A3E8`.
- Benchmarks: `benchmarks/library_builder.R` passed all equivalence and 10% regression guards; grouped NA processing was 0.38s versus 0.53s and the no-NA path 0.18s versus 0.19s in this run.
- Reference/long workflows: N/A; no official library artifact rebuild.
- Reusable evidence: final full, hosted-static, benchmark, documentation, staged-check, and exact-tar checks cover the unchanged package candidate; the later plan-only evidence update is excluded from the source tar.
- Closure: no owned R/Node/check processes or test scratch remain; `git diff --check` passes; `.claude/settings.local.json` is untouched; `.codex-cran-2.0.3/` intentionally retains the tarball and check evidence.

## Risks And Open Questions

- Resolved: alternate BLAS can make an exactly fitted polynomial baseline identically zero; flat relative normalization now returns finite zeros and the library test reports invalid/non-finite recipes explicitly.
- Resolved: the Windows summary omits warning text, so only fixture parser warnings are suppressed while returned class, validity, dimensions, ranges, axes, and metadata remain strictly asserted.
