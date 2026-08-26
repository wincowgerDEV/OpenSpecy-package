# Feature Plan: [FEATURE]

**Feature dir**: `specs/[###-feature-name]`  
**Date**: [DATE]  
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words. If it grows, summarize completed history or move implementation detail into code comments.
**Current tranche**: [The bounded set of changes being implemented now; do not inherit unrelated historical gates.]
**Change class**: [presentation-only | bundled-app behavior | package/scientific | hosted/release | mixed, with the highest class named]

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
- **Bundled Shiny app**: [impact on `inst/`; canonical final `OpenSpecy` or explicitly planned compact `Specs` reactive feeding visible/exported results through bounded accessors; owner/child gating; substantive adjacent guidance for non-obvious scientific/processing inputs; contextual downloads/progress; assets/size; headless and browser tests; or N/A.]
- **Pipeline diagram**: [`.specify/memory/pipeline-diagram.html` box(es)/decision(s) this plan changes, and whether the diagram is updated in this plan; or N/A if the analysis pipeline is untouched.]
- **Hosted Shinylive/WebAssembly app**: [Always classify impact, including explicit N/A. Name changed shared inputs and the triggered tier: fast `-HostedAppStatic`; exact matching-artifact preflight for hosted runtime/routes/interactions/assembly; full clean-commit rebuild for dependency/image/driver/pin or release-facing changes.]

## Package Surfaces

- `R/`: [files/functions]
- `tests/testthat/`: [focused current-behavior tests]
- `benchmarks/`: [required only for same-output function improvements; otherwise state "N/A - new behavior" or reason]
- `workflows/`: [changed, unchanged, or N/A]
- `.github/workflows/`: [changed, unchanged, or N/A]
- `inst/`: [bundled Shiny app/assets, examples/data, unchanged, or N/A]
- `site/vignettes/README/pkgdown`: [changed, unchanged, or deferred; keep the interactive app shell in `site/`, README iframe-free, and pkgdown conventional]
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

- Direct regression or acceptance check for each changed behavior:
- Focused tests and parse/static checks:
- Targeted browser journey and genuine downloads, when triggered:
- Toolchain/version preflight:
- `devtools::document()` trigger or N/A:
- Full `devtools::test()` trigger or N/A:
- `devtools::check()` or CI/R CMD check trigger or N/A:
- Benchmarks:
- Reference-library/long workflow staging:
- Shiny affected states or N/A: [Only changed no-upload/processed/identified/batch/quantification/muted/download/progress/visual states; asset inventory when assets change.]
- Shinylive/WebAssembly impact: [Always state changed/unchanged/N/A; fast hosted-source gate for shared inputs; matching-artifact and full-rebuild triggers or explicit proportional deferral.]
- Reusable evidence: [gate, covered files/contracts, candidate state; invalidate only when one changes.]

## Risks And Open Questions

- [Risk, assumption, or question. Keep only items that can change implementation.]

## Approval Notes

- Approved by:
- Follow-up:
