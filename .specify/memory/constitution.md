<!--
Sync Impact Report
Version change: 2.1.0 -> 2.2.0
Modified principles:
- OpenSpecy Object Contract: clarified filter/join alignment and NA-logical filter expectations
- Tests Track Current Behavior: added reference-library compatibility comparison expectations
- Development Workflow and Quality Gates: added staged temp-output verification for long-running external workflows
Added sections:
- Reference Library and External Workflow Verification
Removed sections:
- None
Templates requiring updates:
- .specify/templates/plan-template.md: add workflows surface and reference-library compatibility verification
- .agents/skills/speckit-implement/SKILL.md: add long-running workflow staging guidance
- AGENTS.md: summarize long-running workflow and reference-library comparison rules
Follow-up TODOs:
- Update OSF-dependent tests so offline guards check the actual download host.
-->

# OpenSpecy Constitution

## Core Principles

### I. Scientific Spectral Integrity
OpenSpecy changes MUST preserve the scientific meaning of Raman and FTIR spectral
data. Public functions MUST keep wavenumber axes, intensity values, metadata,
identifiers, object classes, object attributes, and units coherent through
reading, processing, matching, plotting, and export workflows. Any algorithmic
change that can alter spectral interpretation MUST document the intended effect,
expected numerical tolerance, and user-visible consequences before
implementation.

Rationale: This package supports spectroscopy workflows where small processing
or metadata errors can change material identification results and lead to
incorrect scientific conclusions.

### II. OpenSpecy Object Contract
The `OpenSpecy` object structure MUST stay central to package design, function
interfaces, tests, and examples. Canonical `OpenSpecy` objects are three-part
lists with names `wavenumber`, `spectra`, and `metadata`; `wavenumber` is the
shared spectral axis, `spectra` is a two-dimensional matrix with one row per
wavenumber and one column per spectrum, and `metadata` is a `data.table` with
one row per spectrum. Column names in `spectra` MUST remain unique and aligned
with rows in `metadata`; function changes MUST preserve or deliberately update
that alignment through `as_OpenSpecy()`, `OpenSpecy()`, or documented conversion
helpers.

Filtering, joining, splitting, reducing, and model-preparation code MUST keep
`spectra` columns and `metadata` rows in the same order and length. Logical
filters derived from metadata MUST handle `NA` values deliberately, usually by
treating them as `FALSE`, and tests MUST cover this path when missing metadata
values are plausible.

Object attributes attached through `attr()` or constructor attributes MUST be
treated as part of the long-term object contract. Attributes such as intensity
unit, derivative order, baseline state, spectra type, processing history, and
future compatibility fields MUST be preserved when valid, updated when a
function changes the object's properties, and checked when they can prevent
misuse. When attributes indicate incompatibility, functions MUST provide helpful
warnings or errors if users attempt operations that are incompatible with an
object's format, prior processing, units, or spectral type.

Examples, vignettes, and public workflows MUST demonstrate use with `OpenSpecy`
objects unless a lower-level vector, matrix, data frame, `Specs`, or helper
interface is the explicit subject of the function. Compressed `Specs` workflows
MUST explain their relationship to `OpenSpecy` conversion, matching, and
decompression boundaries.

Rationale: The object structure is the spine of package workflows. Centering it
keeps reading, processing, matching, plotting, external application
compatibility, and documentation behavior consistent for users.

### III. R Package Interface and CRAN Readiness
The package MUST remain a maintainable R package centered on `R/`,
`tests/testthat/`, `vignettes/`, `inst/`, `DESCRIPTION`, `NEWS.md`, and generated
documentation. User-facing functions and objects MUST remain stable unless a
breaking change is explicitly specified, tested, documented, and recorded in
`NEWS.md`.

New dependencies, R version requirements, authorship changes, URLs, package
metadata, and roxygen configuration changes MUST be reflected in `DESCRIPTION`.
Features MUST be compatible with the current package baseline: R >= 4.3.0,
testthat edition 3, roxygen markdown, knitr/rmarkdown vignettes, pkgdown docs,
and multi-platform R CMD check.

Commands that generate package artifacts MUST use the tool versions configured
in `DESCRIPTION`, such as `Config/roxygen2/version`. A version mismatch MUST be
resolved before regeneration rather than accepted as incidental generated-file
churn.

Rationale: OpenSpecy is distributed as an R package and is used through CRAN,
GitHub, vignettes, examples, and downstream tools.

### IV. Tests Track Current Behavior
Every behavior change MUST include or update tests in `tests/testthat/` unless
the plan documents why automated testing is impossible. Tests MUST cover the
public function contract, important edge cases, error handling, and
representative spectral data paths. Bug fixes MUST add a test that fails without
the fix. Changes that touch examples, data readers, object methods, processing,
matching, external Shiny compatibility, or object attributes MUST include tests
that exercise the affected workflow.

`tests/` MUST test current package functionality only. Previous implementations
kept for comparison MUST live in `benchmarks/`, not in `tests/`, because they
are not part of the package functions or CRAN submission surface.

Long-running tests MUST be opt-in locally or run through GitHub Actions
automation. They MUST NOT make routine local `devtools::test()` runs
substantially slower unless explicitly requested for the current task. Long
tests that depend on network resources, large libraries, or heavy computation
MUST use clear testthat skips or CI-only guards.

Network-dependent tests MUST guard the actual host and resource used by the
download, including redirects when relevant. An offline environment MUST cause
a clear skip, not a routine test or package-check failure.

The expected verification command for local feature work is `devtools::test()`.
Release-sensitive work MUST also pass `devtools::check()` or equivalent R CMD
check coverage before release.

Rationale: The package already has broad testthat coverage, and tests are the
main protection against silent spectral-processing errors.

### V. Documentation Is Part of the Change
Every user-visible change MUST update the documentation surface it affects.
Roxygen comments in `R/*.R` MUST be updated with the code they describe.
Vignettes in `vignettes/` MUST be updated when workflows, examples, recommended
parameters, external application compatibility, or scientific interpretation
change. `README.md` and pkgdown-oriented content MUST stay consistent with the
package's current installation, getting-started, citation, and workflow
guidance. `NEWS.md` MUST record user-visible features, fixes, breaking changes,
dependency changes, and documentation-only updates that matter to users.

Examples and workflow documentation MUST prefer representative `OpenSpecy`
objects and MUST show how the object structure and meaningful attributes move
through function flows when that helps users understand package behavior.

Rationale: OpenSpecy users rely on examples, vignettes, help pages, and release
notes to reproduce scientific workflows.

### VI. Benchmark-Governed Performance Work
Performance improvement is a standing priority. Any function update described as
an improvement, refactor, cleanup, vectorization, memory optimization, or speed
change that is intended to keep the same output MUST add or update a benchmark
under `benchmarks/`. The benchmark MUST keep the relevant previous
implementation in `benchmarks/`, compare it with the current package function on
representative data, verify identical or tolerance-defined equivalent output,
and flag a substantial slowdown. A change that keeps the same output MUST NOT be
accepted when the new implementation is more than approximately 10 percent
slower on representative benchmark cases unless the plan documents an explicit
scientific, correctness, or maintainability reason.

Benchmarks with subsecond timings MUST use warmup and repeated measurements or
another stable timing method. Setup, file loading, documentation, and unrelated
work SHOULD be kept outside the measured expression. Benchmark scripts MUST
signal material regressions instead of only printing timings.

Speed improvement recommendations are welcome when they preserve package scope,
avoid unnecessary dependency growth, and include benchmark evidence. Benchmarks
MUST remain outside formal package tests and CRAN obligations; they are
development evidence for comparing old and current implementations.

Rationale: OpenSpecy often works with large spectral datasets. Faster analysis
matters, but speed work must preserve identical scientific output.

### VII. Generated Artifacts Stay Generated
`NAMESPACE`, `man/*.Rd`, and other roxygen-generated package artifacts MUST NOT
be edited directly. Update the source roxygen comments, package metadata, and
`DESCRIPTION`, then regenerate generated documentation with
`devtools::document()`. Generated pkgdown output in `docs/` MUST be produced by
the appropriate pkgdown build process rather than manual HTML edits.

Generated files may be inspected and committed when appropriate, but direct hand
edits are prohibited because they disconnect package behavior from source
documentation.

Authorship, contributor roles, references, aliases, and exports MUST NOT change
as incidental generator output. After regeneration, their diffs MUST be
reviewed. Unexpected changes MUST be corrected in `DESCRIPTION`, roxygen source,
or the configured toolchain and regenerated; generated files MUST NOT be
restored or patched as a substitute for fixing the source.

Rationale: Hand-editing generated files makes documentation drift likely and
breaks the normal R package maintenance workflow.

### VIII. Public API Restraint and Composability
Public functions MUST expose arguments only for required inputs, demonstrated
common policy choices, or stable advanced options owned by an underlying
function. Derived state SHOULD be inferred, and speculative switches MUST NOT be
added without a current workflow that needs them. Input presence SHOULD trigger
optional operations when that is unambiguous, rather than pairing the input with
a second boolean flag.

Standard workflows SHOULD have concise defaults that reproduce the maintainer's
normal package use. Advanced operations SHOULD remain independently composable
with base `|>` unless they are integral to the standard workflow. Helpers used by
one caller SHOULD remain internal until they have a stable, reusable contract;
broadly reusable helpers SHOULD be exported, documented, and tested directly.
Public interfaces MUST prefer `OpenSpecy` objects and domain-general terminology
over table-only pathways or one dataset's taxonomy.

Rationale: Small, evidence-based APIs reduce maintenance burden, documentation
churn, and repeated breaking redesign while preserving advanced composition.

## R Package Standards

The repository is governed as the OpenSpecy R package. Plans and pull requests
MUST identify the affected package surfaces:

- R source files in `R/`
- testthat tests in `tests/testthat/`
- benchmark comparisons in `benchmarks/`
- reference-library and other reproducible maintenance workflows in `workflows/`
- vignettes and supporting assets in `vignettes/`
- package metadata in `DESCRIPTION`
- release notes in `NEWS.md`
- generated roxygen outputs from `devtools::document()`
- optional pkgdown output from the package website build process
- bundled data and examples in `inst/` and package data files

Concise feature plans and pull requests MUST call out whether each surface is
changed, unchanged, or intentionally not applicable. A change that alters public
behavior without tests, roxygen documentation, and NEWS consideration is
non-compliant. A same-output function improvement without benchmark
consideration is non-compliant.

## Reference Library and External Workflow Verification

The official reference-library workflow and other long-running external-data
workflows SHOULD be straight-line, version-controlled scripts that use package
functions and small curated data files. Large source data and generated library
artifacts SHOULD remain external to the package unless a feature plan explicitly
scopes them into `inst/`, package data, or release artifacts.

Changes that can affect official library identifiers, wavenumber axes, metadata
joins, filtering, range restriction, reduction, model libraries, or artifact
shape MUST include compatibility evidence against the current or legacy library
artifact when one is available. The evidence SHOULD report spectrum counts,
shared and missing identifiers, wavenumber-axis compatibility, metadata column
count/name deltas, important warnings, and at least one representative
`OpenSpecy` operation such as `c_spec()` or matching across old and new
artifacts.

Long-running external workflows SHOULD be verified in stages before a full
rerun: first a small representative or subset probe, then isolated expensive
stages with logs and temporary outputs, and only then the full workflow. Full
local rebuilds SHOULD write to a temporary output directory first unless the
user explicitly authorizes publishing or replacing generated artifacts.
Successful intermediate artifacts MAY be reused for diagnosis and comparison,
but temporary diagnostics and generated outputs MUST be cleaned up or clearly
reported before handoff.

## Shiny Compatibility Boundary

The Shiny application MUST NOT be reintroduced into this package repository. It
lives separately at `https://github.com/wincowgerDEV/OpenSpecy-shiny`.
Compatibility with that external application MUST be considered when changing
functions, objects, metadata, examples, or exported behavior, but package
correctness, scientific integrity, maintainability, and CRAN readiness MUST take
precedence. The package is the functional foundation for the external Shiny
application, not a subordinate implementation detail of it.

## Development Workflow and Quality Gates

Feature work SHOULD start from one concise Spec Kit `plan.md` that combines the
former specification, planning, and task-review content. The plan MUST be short
enough for a maintainer to review in about five minutes, with a hard target of
no more than 100 nonblank lines. It MUST state the user impact, affected
functions and objects, test expectations, documentation updates, benchmark
impact, generated artifact strategy, and a short implementation checklist.

Separate `spec.md`, `tasks.md`, research, data-model, contract, quickstart, or
checklist files MUST NOT be required by default. They MAY be created only when
the user explicitly asks for a deeper artifact or when the plan documents why a
separate artifact is necessary for high-risk work. Detail that does not affect
review or implementation SHOULD live in code, tests, roxygen, vignettes, or
benchmarks instead of duplicated planning documents.

Before implementation is complete:

- Focused tests MUST pass before broader verification begins.
- Relevant benchmarks MUST run before full tests for same-output changes.
- `devtools::document()` MUST be run after roxygen, export, S3/S4 method, or
  package metadata changes that affect help pages or `NAMESPACE`, but only
  after configured generator versions are confirmed.
- Generated documentation diffs MUST be inspected immediately after
  regeneration, with attribution and export changes treated as blocking.
- `devtools::test()` MUST then pass for relevant local tests.
- `devtools::check()` or the GitHub Actions R CMD check matrix MUST pass before
  release or CRAN-facing work is considered ready.
- Vignettes MUST build or be validated when their examples or dependencies
  change.
- `NEWS.md` MUST include the change whenever users, downstream packages, or
  maintainers need to know about it.
- Benchmarks in `benchmarks/` MUST be added or updated for same-output function
  improvements and reviewed for output equivalence plus runtime regression.
- OpenSpecy object invariants and attributes MUST be checked when a change
  touches object creation, coercion, processing, matching, plotting, metadata,
  or examples.
- Long-running tests MUST be manual or GitHub Actions guarded, not a surprise
  cost in routine local test runs.
- Official reference-library or other long-running external workflow changes
  MUST use staged subset/temp-output verification and report compatibility
  counts against available legacy artifacts before being treated as complete.
- External Shiny compatibility MUST be considered when relevant, but package
  functionality MUST take precedence.

On Windows, maintained project skills or scripts SHOULD resolve real executable
paths once and reuse them. Process-scoped PowerShell execution-policy bypasses
MAY be used for repository scripts; machine-wide policy changes and Store-stub
executables MUST NOT be used as workflow shortcuts. Expensive full tests,
documentation, and checks SHOULD each run once per final candidate unless a
failure requires another run.

Complexity MUST be justified in the plan when a simpler R package pattern would
work. New abstractions MUST protect repeated spectral workflows, package
function clarity, `OpenSpecy` object consistency, performance, or testability.
Plans MUST avoid repeating the same requirement in multiple sections.

## Governance

This constitution supersedes ad hoc practices for Spec Kit feature work in this
repository. Concise feature plans, code review, and release preparation MUST
check for compliance with the principles above.

Amendments require a change to this file, an updated Sync Impact Report, and
alignment updates to dependent Spec Kit templates. Versioning follows semantic
versioning:

- MAJOR: Removes or redefines a core principle or governance requirement.
- MINOR: Adds a new principle, required section, or materially expands
  compliance expectations.
- PATCH: Clarifies wording without changing obligations.

Reviewers MUST block changes that directly edit locked generated files, skip
required tests without justification, omit required documentation updates,
ignore `OpenSpecy` object invariants or attributes, reintroduce Shiny
application code into this repository, or omit required benchmarks for
same-output function improvements. Temporary exceptions MUST be documented in
the feature plan with the reason, risk, and follow-up task.

**Version**: 2.2.0 | **Ratified**: 2026-05-21 | **Last Amended**: 2026-06-24
