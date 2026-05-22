<!--
Sync Impact Report
Version change: template/unratified -> 1.0.0
Modified principles:
- Template principle 1 -> Scientific Spectral Integrity
- Template principle 2 -> R Package API and CRAN Readiness
- Template principle 3 -> Tests Track Behavior
- Template principle 4 -> Documentation Is Part of the Change
- Template principle 5 -> Generated Artifacts Stay Generated
Added sections:
- R Package Standards
- Development Workflow and Quality Gates
Removed sections:
- None; template placeholders were replaced.
Templates requiring updates:
- .specify/templates/plan-template.md: updated
- .specify/templates/spec-template.md: updated
- .specify/templates/tasks-template.md: updated
- .specify/templates/commands/*.md: not present
- AGENTS.md: updated
Follow-up TODOs: none
-->

# OpenSpecy Constitution

## Core Principles

### I. Scientific Spectral Integrity
OpenSpecy changes MUST preserve the scientific meaning of Raman and FTIR spectral
data. Public functions MUST keep wavenumber axes, intensity values, metadata,
identifiers, object classes, and units coherent through reading, processing,
matching, plotting, and export workflows. Any algorithmic change that can alter
spectral interpretation MUST document the intended effect, expected numerical
tolerance, and user-visible consequences before implementation.

Rationale: This package supports spectroscopy workflows where
small processing or metadata errors can change material identification results and lead to incorrect scientific conclusions.

### II. R Package API and CRAN Readiness
The package MUST remain a maintainable R package centered on `R/`,
`tests/testthat/`, `vignettes/`, `inst/`, `DESCRIPTION`, `NEWS.md`, and generated
documentation. User-facing APIs MUST remain stable unless a breaking change is
explicitly specified, tested, documented, and recorded in `NEWS.md`.

New dependencies, R version requirements, authorship changes, URLs, package
metadata, and roxygen configuration changes MUST be reflected in `DESCRIPTION`.
Features MUST be compatible with the current package baseline: R >= 4.3.0,
testthat edition 3, roxygen markdown, knitr/rmarkdown vignettes, pkgdown docs,
and multi-platform R CMD check.

Rationale: OpenSpecy is distributed as an R package and is used through CRAN,
GitHub, vignettes, examples, and the Shiny app.

### III. Tests Track Behavior
Every behavior change MUST include or update tests in `tests/testthat/` unless
the plan documents why automated testing is impossible. Tests MUST cover the
public contract, important edge cases, error handling, and representative
spectral data paths. Bug fixes MUST add a test that fails without the
fix. Changes that touch examples, data readers, object methods, processing,
matching, or Shiny-facing behavior MUST include tests that exercise the affected
workflow.

The expected verification command for local feature work is `devtools::test()`.
Release-sensitive work MUST also pass `devtools::check()` or equivalent R CMD
check coverage before release.

Rationale: The package already has broad testthat coverage, and tests are the
main protection against silent spectral-processing errors.

### IV. Documentation Is Part of the Change
Every user-visible change MUST update the documentation surface it affects.
Roxygen comments in `R/*.R` MUST be updated with the code they describe.
Vignettes in `vignettes/` MUST be updated when workflows, examples, recommended
parameters, app behavior, or scientific interpretation change. `README.md` and
pkgdown-oriented content MUST stay consistent with the package's current
installation, getting-started, citation, and workflow guidance. `NEWS.md` MUST
record user-visible features, fixes, breaking changes, dependency changes, and
documentation-only updates that matter to users.

Rationale: OpenSpecy users rely on examples, vignettes, help pages, and release
notes to reproduce scientific workflows.

### V. Generated Artifacts Stay Generated
`NAMESPACE`, `man/*.Rd`, and other roxygen-generated package artifacts MUST NOT
be edited directly. Update the source roxygen comments, package metadata, and
`DESCRIPTION`, then regenerate generated documentation with
`devtools::document()`. Generated pkgdown output in `docs/` MUST be produced by
the appropriate pkgdown build process rather than manual HTML edits.

Generated files may be inspected and committed when appropriate, but direct hand
edits are prohibited because they disconnect package behavior from source
documentation.

Rationale: Hand-editing generated files makes documentation drift likely and
breaks the normal R package maintenance workflow.

## R Package Standards

The repository is governed as the OpenSpecy R package. Plans and pull requests
MUST identify the affected package surfaces:

- R source files in `R/`
- testthat tests in `tests/testthat/`
- vignettes and supporting assets in `vignettes/`
- package metadata in `DESCRIPTION`
- release notes in `NEWS.md`
- generated roxygen outputs from `devtools::document()`
- optional pkgdown output from the package website build process
- bundled data and examples in `inst/` and package data files

Specs and implementation plans MUST call out whether each surface is changed,
unchanged, or intentionally not applicable. A change that alters public behavior
without tests, roxygen documentation, and NEWS consideration is non-compliant.

## Development Workflow and Quality Gates

Feature work MUST start from a Spec Kit specification and plan that state the
user impact, affected APIs, expected tests, documentation updates, and generated
artifact strategy. Tasks MUST include explicit work items for tests,
documentation, `DESCRIPTION`, `NEWS.md`, and `devtools::document()` whenever the
feature touches those surfaces.

Before implementation is complete:

- `devtools::test()` MUST pass for relevant tests.
- `devtools::document()` MUST be run after roxygen, export, S3/S4 method, or
  package metadata changes that affect help pages or `NAMESPACE`.
- `devtools::check()` or the GitHub Actions R CMD check matrix MUST pass before
  release or CRAN-facing work is considered ready.
- Vignettes MUST build or be validated when their examples or dependencies
  change.
- `NEWS.md` MUST include the change whenever users, downstream packages, or
  maintainers need to know about it.

Complexity MUST be justified in the plan when a simpler R package pattern would
work. New abstractions MUST protect repeated spectral workflows, package API
clarity, or testability.

## Governance

This constitution supersedes ad hoc practices for Spec Kit feature work in this
repository. Specs, plans, tasks, code review, and release preparation MUST check
for compliance with the principles above.

Amendments require a change to this file, an updated Sync Impact Report, and
alignment updates to dependent Spec Kit templates. Versioning follows semantic
versioning:

- MAJOR: Removes or redefines a core principle or governance requirement.
- MINOR: Adds a new principle, required section, or materially expands
  compliance expectations.
- PATCH: Clarifies wording without changing obligations.

Reviewers MUST block changes that directly edit locked generated files, skip
required tests without justification, or omit required documentation
updates. Temporary exceptions MUST be documented in the feature plan with the
reason, risk, and follow-up task.

**Version**: 1.0.0 | **Ratified**: 2026-05-21 | **Last Amended**: 2026-05-21
