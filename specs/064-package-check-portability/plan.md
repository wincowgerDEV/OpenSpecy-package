# Package Check Portability

**Tranche** Fix the package and coverage check failures for pushed commit `6193548`.
**Change class** Package verification and test fixtures.

## Goal and requirements

- Metadata staging tests must use the checkout workflow path they resolved even when executed from a built package. Retain the existing skip when the checkout helpers are unavailable.
- File-backed pruning must preserve the exact decisions, IDs, and audit fields other than correlation. Compare computed correlations at a tight tolerance to allow floating-point rounding across matrix kernels.
- Restore temporary fixture options with base R. Render both local upload controls and the hosted control in the UI fixture without opening a native GUI.

## Technical decisions

- Anchor the metadata patch's code hashes to the same absolute directory that supplied the scripts. Keep workflow sources excluded from the package archive.
- Keep the portable comparison in the tests. The pruning thresholds and spectral data are unaffected by the assertion. No package interfaces, dependencies, generated docs, workflows, or hosted inputs change in this tranche. Prior verification of the console-progress changes remains applicable.
- CI checks inspect the built package with the checkout path supplied via `GITHUB_WORKSPACE`. Recreate that layout for focused tests and run a staged package check as the broad gate.

## Package surfaces

- `tests/testthat/test-reference-library-staging.R`, `test-build_lib-library-only.R`, `test-build_lib.R`, and `test-run_app.R`. No same-output runtime refactor, so no new benchmark.
- Hosted impact is N/A. The app files, analysis pipeline diagram, and deployment workflows are outside this test-only change.

## Work checklist

- [x] Fix the built-package test path, score assertions, option scopes, and headless picker fixture.
- [x] Verify the focused fixtures and test the checkout fallback from a separate built-package layout.
- [x] Verify staged R CMD check, inspect status and processes, and clean scratch.

## Verification and open questions

- Compare the staged check to the previous GitHub Actions findings. Record any remaining check notes and whether the GitHub matrix has exercised the new commit.
- Focused Windows checks passed 1,147 assertions. The separate R 4.6.1 built-layout probe passed 48 staging and 19 pruning assertions. Absolute tolerance is 1e-12 for correlation scores only, with exact missingness and other audit fields.
- Staged Windows R CMD check completed with zero errors and warnings and three notes (installed size, static R code analysis, and marked UTF-8 data strings). The tarball includes the staging test and excludes `workflows/`. Dependency checks were OK, and installed-package tests reported 4,993 passes, 30 skips, zero warnings, and zero failures.
- The staged source consisted of 608 files and 21,476,906 bytes. The manifest and full check logs remain in the OS temporary evidence directory `openspecy-staged-check-6f95cb29e58c48c7a1ec59238239fc03`. The repo-local probe was removed. The GitHub matrix awaits a pushed commit containing the fix.

## Codecov upload follow-up

- Coverage run 45506457475 at `d45789a` passed its tests with 5,135 assertions and zero failures. Codecov rejected the legacy anonymous upload on the protected branch.
- Generate a Cobertura report in the existing test job and upload it with the version-pinned official Codecov action. Authenticate pushes and same-repository pull requests with short-lived GitHub OIDC. Public fork pull requests retain their tokenless upload path. Preserve upload failures and test diagnostics.
- Use covr 4's `cobertura()` when exported, with a `to_cobertura()` fallback for the locally installed covr 3.x.
- Verification scope is the coverage workflow YAML and a local execution of its R report-generation step against a fixture package. Hosted and package source inputs are unchanged in this follow-up. The authenticated upload requires the next GitHub run.
- The YAML checks and exact report-generation step passed with covr 3.6.5 and GitHub's covr 4.0.0, producing Cobertura XML with covered lines and a relative source filename. All five R-CMD-check jobs passed on the preceding commit `d45789a`.
