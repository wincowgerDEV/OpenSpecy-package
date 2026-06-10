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
6. Run the verification listed in the plan where feasible. Report any skipped
   long-running, optional-backend, or CI-only checks.

## Rules

- Tests cover current package behavior; old implementations used for speed
  comparisons belong in `benchmarks/`.
- Same-output function improvements need a benchmark update or a written reason
  in the plan.
- Do not edit `NAMESPACE`, `man/*.Rd`, or generated pkgdown HTML directly.
- Preserve `OpenSpecy` object structure, identifiers, metadata alignment, and
  relevant attributes through function flows.
- Do not add Shiny application code to this package repository.
