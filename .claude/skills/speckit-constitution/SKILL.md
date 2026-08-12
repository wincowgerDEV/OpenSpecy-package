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
   `.specify/templates/plan-template.md`, `.agents/skills/`,
   `.claude/skills/` (keep the shared `speckit-*` skills identical across both
   agent directories; `openspecy-*` skills live only under `.agents/skills/`
   and reach Claude through a git-ignored local junction, recreated with
   `.specify/scripts/powershell/link-claude-skills.ps1`, so one edit there
   covers both agents), workflow metadata, and `AGENTS.md` (`CLAUDE.md`
   imports `AGENTS.md`, so it never needs its own edit).
5. When reviewing an implementation log, separate durable obligations from
   repeatable procedures. Put obligations in the constitution; update an
   existing skill or create one concise project skill for commands, failure
   triage, or interaction patterns. Reuse maintained scripts instead of copying
   their implementation into skills.
6. Use the system `skill-creator` workflow for new skills. If its Python
   initializer is unavailable after executable discovery, create only the
   required `SKILL.md` (add `agents/openai.yaml` for Codex display metadata
   only when the skill lives under `.agents/skills/`), validate their YAML and
   naming constraints with an available parser, and report the fallback. A new
   project skill (`openspecy-*` naming) belongs under `.agents/skills/`; rerun
   `link-claude-skills.ps1` so its junction appears under `.claude/skills/`
   without a second copy.
7. Keep the planning workflow concise: one default `plan.md`, under 100
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
