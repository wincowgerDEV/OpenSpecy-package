<!-- SPECKIT START -->
@AGENTS.md
<!-- SPECKIT END -->

## Working alongside Codex in this repo

This project uses Spec Kit with two installed agent integrations, Codex and
Claude Code, sharing one set of durable instructions and skills:

- The paragraph above is imported live from `AGENTS.md`, the same file Codex
  reads, so both agents follow identical durable rules with no duplication.
- `speckit-constitution`, `speckit-plan`, and `speckit-implement` are
  hand-customized for this repo's single-`plan.md` workflow (see
  `.specify/memory/constitution.md`). Each agent keeps its own physical copy
  under `.agents/skills/` (Codex) and `.claude/skills/` (Claude); the
  `speckit-constitution` skill's workflow step keeps them in sync when the
  constitution changes. The stock multi-artifact Spec Kit skills
  (`speckit-specify`, `speckit-clarify`, `speckit-tasks`, `speckit-analyze`,
  `speckit-checklist`, `speckit-taskstoissues`) were removed from both agents'
  skill sets because this repo intentionally uses one concise plan instead.
- The `openspecy-*` skills (public API design, Shiny app development, quality
  gates, hosted-app verification) are authored once under `.agents/skills/`
  and reach Claude through git-ignored local directory junctions under
  `.claude/skills/`. If they're missing after a fresh clone, run
  `powershell.exe -ExecutionPolicy Bypass -File .specify\scripts\powershell\link-claude-skills.ps1`.
- Skills alone aren't invocable as `/slash` commands in Claude Code (unlike
  Codex, where `speckit-*` skills are directly invocable). `.claude/commands/`
  has thin `speckit-plan.md`, `speckit-implement.md`, and
  `speckit-constitution.md` wrappers that just point at the matching skill, so
  `/speckit-plan`, `/speckit-implement`, and `/speckit-constitution` work the
  same way Codex's equivalents do without duplicating skill content.
