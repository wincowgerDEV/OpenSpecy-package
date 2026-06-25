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

## Package Surfaces

- `R/`: [files/functions]
- `tests/testthat/`: [focused current-behavior tests]
- `benchmarks/`: [required only for same-output function improvements; otherwise state "N/A - new behavior" or reason]
- `workflows/`: [changed, unchanged, or N/A]
- `vignettes/README/pkgdown`: [changed, unchanged, or deferred]
- `DESCRIPTION`: [changed or unchanged]
- `NEWS.md`: [entry or reason none]
- External Shiny compatibility: [impact or N/A; no Shiny app code in this repo]

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

## Risks And Open Questions

- [Risk, assumption, or question. Keep only items that can change implementation.]

## Approval Notes

- Approved by:
- Follow-up:
