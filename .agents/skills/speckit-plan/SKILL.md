---
name: "speckit-plan"
description: "Create or update one concise feature plan for this repository. Use when the user wants Spec Kit planning, a new feature brief, or a combined plan/spec/tasks artifact without the old separate specify-plan-tasks workflow."
---

# Concise Feature Plan

Create a single `plan.md` that a maintainer can review in about five minutes.
The plan replaces separate `spec.md`, `research.md`, `data-model.md`,
`contracts/`, `quickstart.md`, and `tasks.md` artifacts by default.

## Workflow

1. Read `AGENTS.md`, then locate and read the constitution sections governing
   the affected surfaces. Read the full constitution only for governance work,
   cross-cutting plans, or unresolved routing conflicts.
2. If the active plan is implemented except for maintainer, CI, or post-push
   work, start a new concise plan for an unrelated refinement tranche instead
   of appending to the historical release scope.
3. If the user is starting a new feature, create or select one directory under
   `specs/` using the existing numeric prefix convention and update
   `.specify/feature.json`.
4. Copy `.specify/templates/plan-template.md` to `plan.md` if the file does not
   already exist.
5. Name the bounded current tranche and classify its highest affected surface
   as presentation-only, bundled-app behavior, package/scientific,
   hosted/release, or mixed. Derive gates from this tranche, not old completed
   scope.
6. Fill the plan from the user request and repository context.
7. Keep the final plan under 100 nonblank lines and target at most 1,500 words.
   Prefer bullets and concrete file paths over narrative; condense completed
   evidence instead of accumulating a running transcript.
8. Ask at most three clarification questions, and only when the answer changes
   scope, package contracts, scientific behavior, dependencies, or validation.
9. Update the Spec Kit block in `AGENTS.md` only when the active plan path
   changes or the durable guidance changes. `CLAUDE.md` imports `AGENTS.md`
   directly, so it never needs a separate edit.

## Required Content

- Goal and scope, including explicit out-of-scope items.
- Current tranche, highest change class, and the smallest triggered verification
  ladder that directly covers it.
- Testable requirements.
- Technical decisions that matter for implementation.
- OpenSpecy object flow and object attributes when affected.
- Package surfaces: `R/`, `tests/testthat/`, `benchmarks/`, docs,
  `DESCRIPTION`, `NEWS.md`, generated artifacts, `.github/workflows/`, bundled
  Shiny app impact when `inst/` app code or assets are affected, and hosted
  Shinylive/WebAssembly impact when the wasm package repository, package pin,
  app dependency closure, generated app, or staged medoid/model libraries are
  affected.
- Bundled Shiny planning must name the canonical final reactive feeding visible
  and exported results, owner/child input gating, affected no-upload/processed/
  identified/batch/quantification states, changed genuine downloads, progress,
  console/screenshot evidence, asset impact, and substantive adjacent guidance
  for every non-obvious scientific or processing input. Apply
  `openspecy-develop-shiny-app` during implementation.
- Hosted Shinylive/WebAssembly planning must include the root `site/`, `/app/`,
  and `/pkgdown/` route contract, action verification, hardcoded package and
  dependency pinning, dependency-closure evidence, small-library staging,
  generated-artifact boundaries, and at least one startup/library-matching
  smoke path when relevant.
- Every plan must classify hosted impact, including explicit `N/A`. Changes to
  `R/`, `DESCRIPTION`, `inst/shiny/`, `site/`, README/pkgdown inputs,
  `tools/wasm/`, or deployment workflows schedule `-HostedAppStatic`; add the
  matching-artifact tier for hosted runtime/routes/interactions/assembly, and
  the clean-rebuild tier only for dependency/image/driver/pin or release-facing
  triggers.
- A short work checklist with exact paths.
- Verification commands and any manual or CI-guarded checks.
- Reusable evidence whose covered files, dependencies, inputs, and contracts
  have not changed; do not schedule an expensive gate again without naming its
  invalidating change.
- Risks or open questions that can change implementation.

## Rules

- Do not create separate spec, task, checklist, research, data model, contract,
  or quickstart files unless the user explicitly asks for a deeper artifact.
- Do not duplicate the same requirement across multiple sections.
- Mark unknowns as short open questions instead of expanding the plan.
- Same-output improvements must mention the benchmark requirement.
- Generated files stay generated: update roxygen/package metadata and regenerate.
- Select only skills required by the highest affected surface. Do not add
  package, generated-doc, hosted, or deployment skills "just in case."
