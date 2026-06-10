---
name: "speckit-constitution"
description: "Create or update the durable project constitution and synchronize concise planning guidance."
---

# Constitution Maintenance

The constitution is long-term project memory: scientific rules, package
contracts, generated-file policy, testing expectations, benchmark policy, and
workflow preferences that should survive individual features.

## Workflow

1. Read `.specify/memory/constitution.md`.
2. Apply the user's governance change with the smallest wording change that
   preserves clear MUST/SHOULD rules.
3. Update the Sync Impact Report, semantic version, and amendment date.
4. Synchronize dependent guidance only where needed:
   `.specify/templates/plan-template.md`, `.agents/skills/`, workflow metadata,
   and `AGENTS.md`.
5. Keep the planning workflow concise: one default `plan.md`, under 100
   nonblank lines, with embedded requirements and tasks.

## Versioning

- MAJOR: Removes or redefines a core principle or governance requirement.
- MINOR: Adds a principle, required section, or materially expands compliance.
- PATCH: Clarifies wording without changing obligations.

## Rules

- Do not reintroduce mandatory separate spec/task/research/checklist artifacts.
- Do not weaken generated-file, test, benchmark, OpenSpecy, or Shiny-boundary
  requirements unless the user explicitly asks to amend those principles.
- Leave no unexplained `[PLACEHOLDER]` tokens in the constitution.
