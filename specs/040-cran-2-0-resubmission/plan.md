# Feature Plan: CRAN 2.0.1 Resubmission

**Feature dir**: `specs/040-cran-2-0-resubmission`  
**Date**: 2026-10-05  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Correct the CRAN 2026-10-02 incoming-pretest findings, apply the maintainer-required new version, and export one verified source tarball.  
**Change class**: Hosted/release; package behavior and scientific results are unchanged.

## Goal

- Produce an `OpenSpecy_2.0.1.tar.gz` that removes every reported WARNING/NOTE cause and is ready for the CRAN webform.
- Preserve all public APIs, `OpenSpecy` object behavior, and scientific outputs.

## Scope

- **In**: Undeclared test dependency, unavailable documentation URLs, slow interactive examples, overall check-time contributors, historical archived CRAN test failure audit, generated help, release note, staged build/check, and exact-tarball handoff.
- **Out**: New package features, reference-library rebuilding, bundled/hosted app behavior changes, remote synchronization, and CRAN submission itself.
- **Users**: The maintainer uploading the checked source package; package users receive unchanged runtime behavior.

## Requirements

- R1. Shipped tests contain no direct call to an undeclared package; the quarantine-review test still creates and cleans an isolated temporary directory.
- R2. The USDA, UCL, and NASA source credits remain without unavailable external URLs that CRAN must probe.
- R3. Focused `build_lib` and `read_opus` tests pass, including the formerly failing silent single/multiple OPUS reads from CRAN 1.5.3.
- R4. Full tests, the fast hosted-source gate, dependency inspection, URL inspection, and exact-source package checks complete without errors or warnings attributable to the package; interactive examples are skipped only in noninteractive sessions and remain unit-tested. Two measured end-to-end integration tests run locally/CI but skip on CRAN to keep check time below ten minutes.
- R5. The handed-off tarball is the same artifact checked, excludes repository/development debris, and has recorded size and SHA-256.
- R6. `DESCRIPTION` and `NEWS.md` identify version 2.0.1, with the package date updated to 2026-10-05; schema/reference-build versions remain unchanged.

## Technical Decisions

- **Approach**: Replace the one `withr::local_tempdir()` test call with base-R temporary-directory setup/cleanup instead of adding a package needed by one test. Remove unavailable URL markup while retaining USDA/UCL/NASA credits, gate genuinely interactive Plotly examples with `interactive()`, and CRAN-skip only the two profiled 35-second integration tests while keeping them active locally and in CI.
- **Public API/dependencies**: No exports, arguments, defaults, runtime code, or dependencies change; `DESCRIPTION` changes only package version/date.
- **OpenSpecy contract**: `wavenumber`, `spectra`, `metadata`, identifiers, and object attributes are unaffected.
- **Generated artifacts**: Run `devtools::document()` only with configured roxygen2 8.0.0; inspect `NAMESPACE` and `man/` diffs immediately.
- **External resources**: Confirm the USDA, UCL, and NASA credits have no unavailable URL to probe; no package tests download from those sources.
- **Reference workflow/performance**: No library build algorithm or same-output implementation changes. Profile first, then CRAN-skip the complete artifact-discovery/reference-build integration test while retaining its local/CI coverage; record exact-tarball live-check timing.
- **Bundled Shiny/pipeline diagram**: N/A; `inst/shiny/` and `.specify/memory/pipeline-diagram.html` are unchanged.
- **Hosted Shinylive/WebAssembly app**: Runtime, routes, dependency closure, staged libraries, and pinned commit are unchanged, but `DESCRIPTION` now reports 2.0.1. Run `-HostedAppStatic`; defer a wasm image rebuild to a future hosted publish because this request changes only the CRAN package version/date and does not authorize deployment.

## Package Surfaces

- `R/manage_lib.R`, `R/temperature_emissivity.R`, and generated help: USDA/UCL/NASA credits without unavailable URLs; `R/interactive_plots.R` and generated help: interactive-only examples.
- `tests/testthat/test-build_lib.R`: base-R temporary directory with cleanup and CRAN-only skip for the complete reference-build integration; `tests/testthat/test-app-onboarding.R`: CRAN-only skip for the file-backed Shiny integration; `tests/testthat/test-read_opus.R`: verification only.
- `DESCRIPTION`/`NEWS.md`: version 2.0.1 and date/release heading; `NAMESPACE`, benchmarks, workflows, `.github/workflows/`, `inst/`, site, vignettes, README, pkgdown: unchanged unless a gate exposes a necessary correction.
- Bundled and hosted apps: no behavior or asset impact; only the required fast hosted-static contract check runs.

## Work Checklist

- [x] Patch the isolated test dependency and remove the timed-out roxygen URL; update `NEWS.md`.
- [x] Run focused `build_lib`/`read_opus` tests and verify dependency/URL findings directly.
- [x] Regenerate documentation with roxygen2 8.0.0 and audit generated diffs.
- [x] Regenerate/audit documentation and rerun `-HostedAppStatic`, full tests, staged preparation, and exact package checks for version 2.0.1.
- [x] Export and independently inspect the exact checked 2.0.1 tarball, hash it, and reconcile status/processes/scratch files.

## Verification

- Focused: `build_lib|read_opus` passed 625 assertions; interactive plots passed 15; no shipped `withr::` remains. The two profiled integration files then passed 733 assertions with 0 skips locally, confirming their bodies remain active outside CRAN.
- Documentation/URL: roxygen2 8.0.0 changed only the three intended Rd files; `NAMESPACE` is unchanged. The final network-enabled incoming check has no invalid URL finding; USDA/UCL/NASA citations remain without their timed-out links.
- Broad: full tests passed 4,279 assertions (30 expected warning assertions, 2 skips), and the final `-HostedAppStatic` gate passed 364 assertions/contracts with the 2.0.1 wasm-version contract.
- Manual: the first exact `--as-cran` run passed through tests/vignettes but local MiKTeX, reported as a fresh unfinished installation, stalled at PDF generation and was stopped with its owned process tree. The submitted CRAN Windows/Debian pretests had already passed PDF-manual generation; final documentation structure passed all Rd checks.
- Historical evidence: archived 1.5.3 errors were OPUS warning expectations; the current focused test reconfirmed silent single/multiple reads.
- Exact artifact: the final network-enabled `R CMD check --as-cran --no-manual` completed in 491.9 seconds (8:12) with 0 errors, 0 warnings, and 2 environmental notes (archived/new-submission status and unavailable time verification). Incoming feasibility fell to 36 seconds, examples passed in 18 seconds, and tests passed in 158 seconds with no unstated dependencies.
- Artifact: `OpenSpecy_2.0.1.tar.gz` contains 250 entries and no repository/development debris; size 1,961,510 bytes; SHA-256 `87CFFD81D9A2EA98DD876F589666FEDB3F3F6699B13D507651E8E2714E359C45`.
- Closure audit: preserve unrelated `.claude/settings.local.json`; retain ignored evidence under `.codex-cran-resubmission/` until submission is accepted.

## Risks And Open Questions

- R 4.3.3 cannot exactly emulate CRAN's 2026 R-devel; dependency and URL checks therefore need static/CRAN-style inspection in addition to local `R CMD check`.
- The maintainer requires a unique submission version, so the replacement must be 2.0.1 even though 2.0.0 was rejected before release.

## Approval Notes

- Approved by: maintainer request on 2026-10-05 to correct all CRAN findings, use version 2.0.1, and export an uploadable tarball.
- Follow-up: Webform upload and remote synchronization remain maintainer-owned; configuring local MiKTeX is optional because CRAN's manual checks already passed.
