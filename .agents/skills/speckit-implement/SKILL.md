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
- Do not add Shiny application code to this package repository.
