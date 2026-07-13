---
name: "speckit-plan"
description: "Create or update one concise feature plan for this repository. Use when the user wants Spec Kit planning, a new feature brief, or a combined plan/spec/tasks artifact without the old separate specify-plan-tasks workflow."
---

# Concise Feature Plan

Create a single `plan.md` that a maintainer can review in about five minutes.
The plan replaces separate `spec.md`, `research.md`, `data-model.md`,
`contracts/`, `quickstart.md`, and `tasks.md` artifacts by default.

## Workflow

1. Read `.specify/memory/constitution.md` and `AGENTS.md`.
2. If the user is starting a new feature, create or select one directory under
   `specs/` using the existing numeric prefix convention and update
   `.specify/feature.json`.
3. Copy `.specify/templates/plan-template.md` to `plan.md` if the file does not
   already exist.
4. Fill the plan from the user request and repository context.
5. Keep the final plan under 100 nonblank lines. Prefer bullets and concrete
   file paths over narrative.
6. Ask at most three clarification questions, and only when the answer changes
   scope, package contracts, scientific behavior, dependencies, or validation.
7. Update the Spec Kit block in `AGENTS.md` only when the active plan path
   changes or the durable guidance changes.

## Required Content

- Goal and scope, including explicit out-of-scope items.
- Testable requirements.
- Technical decisions that matter for implementation.
- OpenSpecy object flow and object attributes when affected.
- Package surfaces: `R/`, `tests/testthat/`, `benchmarks/`, docs,
  `DESCRIPTION`, `NEWS.md`, generated artifacts, `.github/workflows/`, bundled
  Shiny app impact when `inst/` app code or assets are affected, and hosted
  Shinylive/WebAssembly impact when the wasm package repository, package pin,
  app dependency closure, generated app, or staged medoid/model libraries are
  affected.
- Shiny planning must include headless module/server tests, installed-asset
  checks, and manual or CI-guarded smoke tests when relevant; hosted
  Shinylive/WebAssembly planning must include action verification, hardcoded
  package and dependency pinning, dependency-closure evidence, small-library
  staging, generated-artifact boundaries, and at least one
  startup/library-matching smoke path when relevant.
- A short work checklist with exact paths.
- Verification commands and any manual or CI-guarded checks.
- Risks or open questions that can change implementation.

## Rules

- Do not create separate spec, task, checklist, research, data model, contract,
  or quickstart files unless the user explicitly asks for a deeper artifact.
- Do not duplicate the same requirement across multiple sections.
- Mark unknowns as short open questions instead of expanding the plan.
- Same-output improvements must mention the benchmark requirement.
- Generated files stay generated: update roxygen/package metadata and regenerate.
