<!--
Sync Impact Report
Version change: 3.4.1 -> 3.5.0
Modified principles:
- Tests Track Current Behavior: require state-transition, genuine-download, and visual/console evidence proportional to bundled Shiny changes
- Bundled Shiny Application Boundary: establish one canonical processed-data flow, owner-gated reactivity, native downloads, and explicit progress phases
- Development Workflow and Quality Gates: add a reusable local-app gate and reserve R CMD check for explicit full-check or release-facing work
Added sections:
- None
Removed sections:
- None
Templates requiring updates:
- .specify/templates/plan-template.md and .agents/skills/speckit-plan/: plan canonical app state, owner gating, downloads, and browser evidence
- .agents/skills/openspecy-develop-shiny-app/: new bundled-app implementation and verification workflow
- .agents/skills/speckit-implement/ and openspecy-run-quality-gates/: route app work through the staged local browser gate
- .agents/skills/openspecy-test-hosted-app-browser/ and openspecy-verify-hosted-app/: link local/hosted parity to the new bundled-app skill
- AGENTS.md: synchronize concise durable guidance
Follow-up TODOs:
- None
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

Bundled Shiny application changes MUST preserve CRAN readiness. App code and
assets under `inst/` MUST be kept small, portable, and installable without
network access. Large media assets, especially images, MUST be compressed,
downsampled, deduplicated, or moved out of the package with a documented
download/cache strategy before they are accepted into the release surface.

Hosted Shinylive/WebAssembly application work MUST preserve package release
quality while treating generated web artifacts and wasm package repositories as
deployment output. The source package SHOULD NOT carry generated Shinylive build
products or wasm package repository artifacts unless a feature plan explicitly
scopes them into the package and accounts for CRAN size, portability, dependency
reproducibility, and generated-file review.

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

Bundled Shiny app changes MUST include tests proportional to the changed app
surface. Testable app helpers SHOULD live in package functions or sourceable app
helper files and be covered by normal `testthat` tests. Shiny module and server
logic SHOULD use headless tests such as `shiny::testServer()` when feasible.
Tests that verify installed app paths, static assets, and launchability SHOULD
load the app through `system.file()` or the same package helper users call, so
missing `inst/` files are caught before release. User-visible interaction work
MUST exercise the affected transitions among no-upload, uploaded/processed,
identification-enabled, batch/map, and optional quantification states. Changes
to downloads MUST create and inspect genuine files; changes to reactivity MUST
prove muted child settings remain inert; and layout/theme changes MUST include
visual inspection at representative desktop and mobile sizes. Browser,
snapshot, or long-running end-to-end app tests MUST be manual, optional, or
CI-guarded and MUST skip clearly when optional test backends or network
resources are missing. Browser evidence MUST capture severe console/server
errors and inspect screenshots rather than relying only on DOM assertions.

Hosted Shinylive/WebAssembly changes MUST verify the generated app and the
CRAN-like wasm package repository together. Verification SHOULD check that the
package repository index is produced, the repository contains the intended
OpenSpecy package plus the hosted app's required non-base runtime dependency
closure, the hosted app points to the intended pinned package version or commit,
required small reference libraries are available to the app, and browser smoke
tests cover startup plus at least one library-matching path. These checks MAY
run in GitHub Actions or guarded manual smoke tests when local WebAssembly
tooling is impractical.

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

`README.md` SHOULD remain directly readable on GitHub and MUST NOT carry the
hosted app iframe or its interactive controls unless a feature plan explicitly
reopens that presentation decision. Pkgdown-only homepage markup and behavior
belong in `pkgdown/index.md`, `pkgdown/extra.css`, and `pkgdown/extra.js`.
Package prose duplicated across README and pkgdown sources MUST stay aligned.

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
- GitHub Actions workflows and deployment metadata under `.github/workflows/`
- bundled data and examples in `inst/` and package data files
- bundled Shiny application code and optimized assets under `inst/` when
  affected
- generated Shinylive application, wasm CRAN-like repository, and pinned hosted
  app dependency deployment outputs when their source workflows are affected

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

## Bundled Shiny Application Boundary

The Shiny application SHOULD be managed in this package repository under
`inst/` once ported from `https://github.com/wincowgerDEV/OpenSpecy-shiny`.
During the transition, that repository is the source to import from; after the
port, the package copy is the canonical application surface unless a feature
plan explicitly states otherwise.

Shiny application code MUST live under `inst/` and MUST remain separable from
core package functions. Package correctness, scientific integrity,
maintainability, and CRAN readiness MUST take precedence over app convenience.
App code MAY call exported or internal package functions as appropriate, but it
MUST NOT require weakening `OpenSpecy` object contracts, generated-file policy,
testing expectations, or public API restraint.

For one app analysis state, the visible spectrum, summaries, identification,
quantification, metadata, and downloads MUST derive from the same final
processed `OpenSpecy` reactive object unless a deliberately different source is
named in the interface and plan. Hidden duplicate preprocessing pipelines are
non-compliant because they make visible and exported results irreproducible.
Collision-prone UI functions MUST be namespace-qualified, and dynamically
rendered inputs MUST be handled safely while absent.

Owner controls MUST gate their dependent inputs before server code reads them.
Changing a disabled, hidden, or otherwise muted child setting MUST NOT
invalidate analysis, start network or reference-library work, or display the
analysis overlay. Long-running feedback MUST come from explicit analysis phases
in one accessible central display with actual elapsed time and staged progress;
brief reactive flushes and configuration-only changes MUST remain quiet.

Contextual downloads MUST preserve the native Shiny download binding, expose a
clear current label, validate nonempty server output, and be browser-tested with
representative file content when changed. Download configuration, plotting,
tables, sidebars, dialogs, and dynamic controls MUST participate in the same
theme and reactive state model as the main interface rather than being treated
as detached features.

Shiny app updates MUST include an asset audit before release-facing checks:
remove orphaned files, duplicate assets, raw source images, obsolete generated
outputs, and unused dependencies; compress or downsample images; and report
source-package and installed-package size impact when app assets change. Large
optional assets SHOULD be generated, cached, downloaded with offline guards, or
kept outside the package rather than bundled.

Routine package tests MUST NOT launch a long-running interactive app. App
changes SHOULD include focused noninteractive tests for package-side helpers,
server/module logic, installed paths, and required static assets, plus a manual
or CI-guarded Shiny smoke test that verifies the bundled app starts, loads
required assets, and does not rely on files outside the installed package.

## Hosted Shinylive/WebAssembly Application Boundary

The public WebAssembly app SHOULD be generated and published with this
repository's pkgdown site, with pkgdown at the site root and the standalone app
at `/app/`. The pkgdown homepage MAY embed that route immediately below
the OpenSpecy title, while GitHub's README remains a normal document with a link
to the site. The bundled Shiny app under `inst/` is the canonical source for the
local and hosted app experience. The
`Moore-Institute-4-Plastic-Pollution-Res/openspecy` repository is the active
hosting bridge for `openanalysis.org/openspecy/`; deployment workflows MUST
remain fork-portable and SHOULD consume this package repository's canonical
source or generated artifact without maintaining a divergent app implementation.

The Shinylive application MUST be generated from package app source by GitHub
Actions or an equivalent reproducible command. Generated Shinylive site files,
WebAssembly package repository contents, and other deployment artifacts MUST NOT
be hand-edited; changes belong in the package source, `inst/` app source,
library staging code, workflow configuration, or pinned deployment metadata.

The `.github/workflows/deploy-cran-repo.yml` workflow named "Build and deploy
wasm R package repository" is required infrastructure for the hosted app. It
MUST build a CRAN-like wasm repository from the checked-out package source and
the hosted app's complete non-base R runtime dependency closure, including
direct app dependencies and transitive dependencies not supplied by the
WebAssembly runtime. The repository MUST record package names, versions, and
available build identifiers through `PACKAGES`, lock metadata, manifest files,
or equivalent generated metadata so the app bundle remains a permanent artifact
that resolves to the same functionality at that time. The workflow MUST remain
green before a hosted Shinylive release is treated as ready. If this workflow
fails, fixing it is part of the hosted-app work rather than optional cleanup.

The Shinylive app MUST point to a hardcoded/pinned wasm package repository and
package version, commit, or equivalent immutable build identifier for the app
release. The pin SHOULD be refreshed from the most recently pushed package
source when the app is intentionally rebuilt, but it MUST NOT float to future
package or dependency versions automatically. The hosted app MUST NOT resolve
required app packages from floating external repositories at runtime. A package
or dependency update that affects the app requires an explicit Shinylive rebuild
or a documented decision to leave the hosted app on its previous pin.

Library spectra for the Shinylive app SHOULD be staged by GitHub Actions using
package functions such as `get_lib()` where feasible, with host guards,
reproducible cache paths, and size reporting. The WebAssembly app MAY use only
the smaller medoid and model libraries; the local bundled Shiny app MAY expose
full libraries as well. This small-library restriction SHOULD be the only major
functional difference between local and WebAssembly apps unless a plan documents
the reason, user impact, tests, and documentation update.

An expanded app view that is expected to survive file pickers, uploads, and
downloads MUST use page-owned viewport state rather than the browser Fullscreen
API, which browsers may dismiss for native UI. Browser verification MUST cover
entry, `Escape`, a file chooser, upload, download, and exit through the app's
explicit control. Global Shiny busy feedback SHOULD appear only for sustained
work; brief reactive flushes after results render MUST NOT flash a blocking
overlay. Shared local/hosted UI code MUST be tested as shared behavior.

Hosted app work MUST include verification proportional to the change: action
syntax and permissions, wasm package repository index and package availability,
dependency-closure availability, library artifact availability, pinned package
and dependency metadata, generated app startup, static asset loading, and a
browser or CI-guarded smoke path that exercises library matching. Size impact
and generated output location MUST be reported before handoff.

When a matching action-built wasm artifact is available, local handoff SHOULD
run `tools/wasm/test-shinylive-action.ps1` with that artifact's exact package
commit. The preflight MUST assemble pkgdown plus `/app/`, bundle the
pinned library image, run repository/export checks, and exercise startup,
upload, identification, download, console diagnostics, and desktop/mobile
screenshots. `_wasm/` outputs MUST remain ignored and MUST NOT be mistaken for
source changes.

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
- Routine bundled-app iteration MUST NOT repeat R CMD check. Run it only when
  the maintainer explicitly requests a full/package check or the active plan is
  release/CRAN-facing; record intentional deferral in the handoff.
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
- Bundled Shiny app changes MUST apply the
  `openspecy-develop-shiny-app` workflow: parse the canonical app sources, run
  focused noninteractive tests, verify the affected app-state matrix, create
  genuine changed downloads, inspect browser console/screenshots when relevant,
  and report the `inst/shiny` asset inventory. Perform the full orphan,
  dependency, compression, and package-size audit when assets change or the work
  is release-facing.
- Hosted Shinylive/WebAssembly changes MUST verify the wasm package repository
  workflow, package and dependency pins, dependency closure, small-library
  staging, generated app startup, asset loading, and at least one
  library-matching smoke path before release-facing handoff.
- Hosted homepage or interaction changes MUST use the action-equivalent
  preflight when a matching wasm artifact is available and MUST verify app-mode
  persistence plus absence of post-result busy-overlay flashes.

On Windows, maintained project skills or scripts SHOULD resolve real executable
paths once and reuse them. Process-scoped PowerShell execution-policy bypasses
MAY be used for repository scripts; machine-wide policy changes and Store-stub
executables MUST NOT be used as workflow shortcuts. Expensive full tests,
documentation, and checks SHOULD each run once per final candidate unless a
failure requires another run.

Remote synchronization is maintainer-owned by default. Automated agents MUST
NOT run `git push`, `git pull`, or `git pull --rebase` unless the user explicitly
authorizes that specific remote operation in the current request. Earlier or
standing permission MUST NOT be treated as authorization for a later operation.
Agents SHOULD leave local changes, commits, and verification results ready for
maintainer review and synchronization.

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
ignore `OpenSpecy` object invariants or attributes, add Shiny application code
outside `inst/`, bundle avoidable large or orphaned Shiny assets, or omit
required benchmarks for same-output function improvements. Reviewers MUST also
block bundled-app changes that split visible and exported results across
unexplained processing pipelines, let muted controls trigger analysis, replace
native download bindings without necessity, or claim changed downloads work
without genuine file evidence. Reviewers MUST also
block hosted Shinylive/WebAssembly changes that float to unpinned package or
dependency versions, bypass the wasm package repository workflow without
justification, resolve required app packages from floating external
repositories, or silently diverge from the local app beyond the documented
small-library constraint. They MUST also block hosted homepage changes that put
interactive app markup back into GitHub's README, commit generated `_wasm/`
outputs, or claim browser readiness without the required interaction evidence.
Temporary exceptions MUST be documented in the feature plan with the reason,
risk, and follow-up task.

**Version**: 3.5.0 | **Ratified**: 2026-05-21 | **Last Amended**: 2026-07-23
