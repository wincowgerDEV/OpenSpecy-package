---
name: "speckit-implement"
description: "Implement the current concise Spec Kit plan by executing the Work Checklist in plan.md."
---

# Implement From Plan

Use `plan.md` as the source of truth. Separate `tasks.md` files are legacy and
are not required.

## Workflow

1. Run `.specify/scripts/powershell/check-prerequisites.ps1 -Json` and read the
   active `plan.md`, `.specify/memory/constitution.md`, and `AGENTS.md`.
2. Extract the `Work Checklist`, package surfaces, verification commands, and
   open questions from the plan.
3. If an open question blocks correctness, ask it before editing. Otherwise make
   conservative assumptions and proceed.
4. Implement checklist items in dependency order. Update the checkbox in
   `plan.md` as each item is completed.
5. Keep changes scoped to files named or implied by the plan and constitution.
6. For public API changes, apply `openspecy-design-public-api` before editing
   signatures.
7. Run staged verification with `openspecy-run-quality-gates`: focused tests,
   relevant benchmarks, toolchain preflight and documentation, full tests, then
   package check. Report skipped long-running, external-resource, optional
   backend, or CI-only checks precisely.
8. For reference-library or other long-running external workflows, run a small
   representative probe first, then isolated expensive stages with logs and
   temporary outputs before the full workflow. Compare rebuilt artifacts against
   available legacy identifiers, wavenumber axes, metadata shape, warnings, and
   representative `OpenSpecy` joins or matches before marking the work complete.
9. For bundled Shiny app work, keep app code/assets under `inst/`, audit and
   remove orphaned files, compress or downsample images, report package-size
   impact, test helpers and server/module logic headlessly where feasible,
   verify installed app paths/assets, and run a manual or CI-guarded app smoke
   test when relevant.
10. For hosted Shinylive/WebAssembly work, treat the bundled `inst/` app and
    package source as canonical, fix and verify the "Build and deploy wasm R
    package repository" workflow when affected, keep the app on a hardcoded
    package version/commit pin plus pinned app dependency closure, stage only
    the small medoid/model libraries with guarded `get_lib()` workflows unless
    the plan says otherwise, avoid hand-editing generated web artifacts, and run
    a CI-guarded or manual smoke test that covers startup, assets, dependency
    resolution, and library matching.

## Rules

- Tests cover current package behavior; old implementations used for speed
  comparisons belong in `benchmarks/`.
- Same-output function improvements need a benchmark update or a written reason
  in the plan. Short benchmarks need repeated measurements and must flag
  material regressions.
- Do not edit `NAMESPACE`, `man/*.Rd`, or generated pkgdown HTML directly.
- Do not regenerate documentation with a roxygen2 version that differs from the
  version configured in `DESCRIPTION`.
- Preserve `OpenSpecy` object structure, identifiers, metadata alignment, and
  relevant attributes through function flows.
- Treat logical metadata filters with possible `NA` values deliberately and
  verify spectra/metadata row-column alignment after filters, joins, reductions,
  and model-preparation steps.
- Shiny application code belongs under `inst/`; avoidable large, duplicate, raw,
  generated, or orphaned Shiny assets must not be bundled.
- Hosted Shinylive/WebAssembly app output and wasm package repository contents
  are generated deployment artifacts; update source app code, package code,
  workflow config, library staging, dependency closure, or pinned metadata
  instead of editing them directly.
