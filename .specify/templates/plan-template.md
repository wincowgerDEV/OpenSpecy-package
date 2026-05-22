# Implementation Plan: [FEATURE]

**Branch**: `[###-feature-name]` | **Date**: [DATE] | **Spec**: [link]

**Input**: Feature specification from `/specs/[###-feature-name]/spec.md`

**Note**: This template is filled in by the `/speckit-plan` command. See `.specify/templates/plan-template.md` for the execution workflow.

## Summary

[Extract from feature spec: primary requirement + technical approach from research]

## Technical Context

<!--
  ACTION REQUIRED: Replace the content in this section with the technical details
  for this feature. Defaults below reflect the OpenSpecy R package.
-->

**Language/Version**: R >= 4.3.0 or NEEDS CLARIFICATION

**Primary Dependencies**: Existing package dependencies in `DESCRIPTION`, plus any new dependency justification or NEEDS CLARIFICATION

**Storage**: Package `data/`, `inst/extdata`, and downloaded spectral libraries

**Testing**: testthat edition 3 via `devtools::test()`; long-running tests manual or GitHub Actions guarded; R CMD check via `devtools::check()` or GitHub Actions

**Target Platform**: CRAN/GitHub R package across Windows, macOS, and Linux; external OpenSpecy-shiny and webR compatibility impacts if relevant

**Project Type**: R package with pkgdown site and vignettes; Shiny application lives in `wincowgerDEV/OpenSpecy-shiny`

**Performance Goals**: Spectral workflows remain accurate and computationally efficient for representative individual, batch, and map data; same-output function improvements avoid >10% slowdown in benchmarks

**Constraints**: Preserve scientific meaning, function compatibility, CRAN checks, generated-doc workflow, external Shiny boundary, and dependency discipline

**Scale/Scope**: Affected functions, OpenSpecy object flows, classes, tests, benchmarks, vignettes, docs, data files, and release notes

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- Scientific integrity: plan states how spectral values, wavenumbers, metadata,
  classes, and tolerances are preserved or intentionally changed.
- OpenSpecy object contract: plan states whether `wavenumber`, `spectra`, and
  `metadata` structure, object attributes via `attr()`, alignment, coercion,
  examples, or `Specs` conversion boundaries are affected.
- R package contract: affected functions, exported objects, dependencies, `DESCRIPTION`, and
  CRAN/R CMD check impact are identified.
- Tests: required `tests/testthat/` additions or updates are listed; any
  untestable area has a written justification; tests cover current behavior only;
  long-running tests are manual or GitHub Actions guarded.
- Benchmarks: same-output function improvements list required `benchmarks/`
  additions or updates, retain old implementations there, verify output
  equivalence, and check for substantial runtime regression (~>10%).
- Documentation: roxygen, vignettes, README/pkgdown, and `NEWS.md` impact are
  listed as changed, unchanged, or not applicable, with examples centered on
  `OpenSpecy` objects when relevant.
- Shiny boundary: external OpenSpecy-shiny compatibility is considered when
  relevant, but package correctness and CRAN readiness take precedence; Shiny
  application code is not added to this repository.
- Generated artifacts: `NAMESPACE`, `man/*.Rd`, and pkgdown files are not
  planned for direct edits; required updates are generated from roxygen,
  `DESCRIPTION`, `devtools::document()`, or the pkgdown build process.

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
|-- plan.md              # This file (/speckit-plan command output)
|-- research.md          # Phase 0 output (/speckit-plan command)
|-- data-model.md        # Phase 1 output (/speckit-plan command)
|-- quickstart.md        # Phase 1 output (/speckit-plan command)
|-- contracts/           # Phase 1 output (/speckit-plan command)
`-- tasks.md             # Phase 2 output (/speckit-tasks command)
```

### Source Code (repository root)

<!--
  ACTION REQUIRED: Expand the concrete paths affected by this feature and delete
  paths that are not relevant.
-->

```text
R/                      # Source and roxygen comments
tests/testthat/         # testthat coverage
vignettes/              # User workflows and long-form examples
inst/                   # Bundled examples and package resources; not Shiny application code
data/                   # Datasets loaded with the package that are easily callable
benchmarks/             # Benchmark old/current implementations for same-output speed work; keep legacy comparison code here, not in tests/
DESCRIPTION             # Package metadata and dependency declarations
NEWS.md                 # User-visible release notes
README.md               # Github viewable introductory information
LICENSE.md              # The license for the open source package
.Rbuildignore           # Files and folders that are not essential to R packages
cran-comments.md        # Generated; do not edit
man/                    # Generated by devtools::document(); do not edit directly
NAMESPACE               # Generated by devtools::document(); do not edit directly
docs/                   # Generated pkgdown site; do not edit HTML directly
```

**Structure Decision**: [Document the selected structure and reference the real
directories captured above]

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., direct generated-file edit] | [current need] | [why source roxygen or generation workflow is insufficient] |
| [e.g., new dependency] | [specific problem] | [why existing dependencies/base R are insufficient] |
