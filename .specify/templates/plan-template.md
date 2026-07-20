# Feature Plan: [FEATURE]

**Feature dir**: `specs/[###-feature-name]`  
**Date**: [DATE]  
**Review budget**: Keep this file under 100 nonblank lines. If it grows, summarize or move detail into code comments after implementation.

## Goal

- [One sentence describing the user-visible outcome.]
- [One sentence describing the package/scientific constraint that matters most.]

## Scope

- **In**: [Concrete behavior to add or change.]
- **Out**: [Concrete behavior intentionally deferred.]
- **Users**: [Who benefits and how they will call or experience it.]

## Requirements

- R1. [Testable behavior or contract.]
- R2. [Testable behavior or contract.]
- R3. [Testable behavior or contract.]

## Technical Decisions

- **Approach**: [Implementation strategy in one short paragraph.]
- **Public API**: [Why each new argument/export is necessary; what is inferred, internal, or passed through `...`.]
- **Dependencies**: [Reuse existing dependencies, add guarded optional dependency, or none.]
- **OpenSpecy contract**: [How `wavenumber`, `spectra`, `metadata`, identifiers, and `attr()` metadata are preserved or intentionally changed.]
- **Generated artifacts**: [Whether documentation generation is needed; required configured tool version; no direct generated-file edits.]
- **External resources**: [Network hosts, large files, offline guards, or N/A.]
- **Reference workflow compatibility**: [For library/large workflow changes, old/new artifact comparison plan or N/A.]
- **Bundled Shiny app**: [impact on `inst/` app code/assets, image compression, orphan-file audit, package-size impact, headless app tests, smoke test, or N/A.]
- **Hosted Shinylive/WebAssembly app**: [impact on `.github/workflows/`, wasm package repository, hardcoded package/dependency pins, dependency closure, small medoid/model library staging, GitHub README vs `pkgdown/index.md`, generated app artifacts, action-equivalent preflight, interaction/visual smoke tests, or N/A.]

## Package Surfaces

- `R/`: [files/functions]
- `tests/testthat/`: [focused current-behavior tests]
- `benchmarks/`: [required only for same-output function improvements; otherwise state "N/A - new behavior" or reason]
- `workflows/`: [changed, unchanged, or N/A]
- `.github/workflows/`: [changed, unchanged, or N/A]
- `inst/`: [bundled Shiny app/assets, examples/data, unchanged, or N/A]
- `vignettes/README/pkgdown`: [changed, unchanged, or deferred; keep interactive app markup pkgdown-only]
- `DESCRIPTION`: [changed or unchanged]
- `NEWS.md`: [entry or reason none]
- Bundled Shiny app: [impact, asset audit, headless tests, smoke test, or N/A]
- Hosted Shinylive/WebAssembly app: [wasm repo workflow, package and dependency pins, dependency closure, library staging, generated output, smoke test, or N/A]

## Work Checklist

- [ ] [First implementation task with path]
- [ ] [First focused test task with path]
- [ ] [Documentation or metadata task with path]
- [ ] [Validation command or manual/CI-guarded check]

## Verification

- Focused tests:
- Toolchain/version preflight:
- `devtools::document()`:
- Full `devtools::test()`:
- `devtools::check()` or CI/R CMD check:
- Benchmarks:
- Reference-library/long workflow staging:
- Shiny app tests/asset audit/smoke test:
- Shinylive/WebAssembly package repo, dependency closure, pins, libraries, action-equivalent preflight, and nested-frame interaction/visual smoke test:

## Risks And Open Questions

- [Risk, assumption, or question. Keep only items that can change implementation.]

## Approval Notes

- Approved by:
- Follow-up:
