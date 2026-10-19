# Feature Plan: Session-Informed Workflow Governance

**Feature dir**: `specs/050-session-informed-workflow-governance`
**Date**: 2026-10-19
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.
**Current tranche**: Reconcile the five latest substantive OpenSpecy development tranches into durable governance and streamlined project skills.
**Change class**: Workflow/governance only; package, scientific runtime, bundled app runtime, and hosted runtime are unchanged.

## Goal

- Convert repeated problems from app-state fixes, hierarchical/reference rebuilds, post-build assessment, the flat no-closure rebuild, and frozen positive controls into clear durable contracts.
- Keep operational detail in concise skills so the constitution states obligations without becoming a command manual.

## Scope

- **In**: constitution 3.13.0; official-build provenance and spectrum-disposition rules; frozen holdout discipline; Shiny action-state ownership; a reference-rebuild skill; synchronized planning/implementation guidance.
- **Out**: package/app behavior changes, another library build, artifact publication, positive-control reruns, generated R documentation, remote synchronization.
- **Users**: maintainers and coding agents planning, running, monitoring, auditing, or explaining OpenSpecy work.

## Requirements

- R1. Base amendments on the five latest substantive project tranches, not child-agent traces or unrelated sessions, and retain no copied session logs in the repository.
- R2. Official rebuild evidence states and freezes source/code/artifact identity, exact pruning threshold/operator and enablement, model method, signature/reuse policy, stage budgets, and output target.
- R3. Assessments balance retained, reassigned, and removed spectra; quarantine preserves every removed ingested spectrum with stable ID, source, removal stage, rule, reason, and relevant threshold/comparison evidence.
- R4. Frozen external validation pins inputs, truth, artifacts, and output-determining controls; a changed implementation or setting invalidates downstream evidence and returns through compatibility/probe gates before full comparison.
- R5. User-facing analysis actions have one explicit owner for no-input, dirty, running, current, and error/retry states, with direct transition tests.
- R6. Add and validate one concise `openspecy-rebuild-reference-library` skill; update the existing Shiny and shared Spec Kit skills without duplicating maintained scripts.

## Technical Decisions

- **Approach**: amend durable obligations in the constitution and route repeatable commands, launch checks, monitoring, assessment, and handoff structure through skills.
- **Public API/OpenSpecy contract**: unchanged; governance reinforces existing object, identifier, metadata, provenance, and artifact contracts.
- **Generated artifacts**: no R generated artifacts change. Shared `speckit-*` skill copies remain byte-identical; `openspecy-*` Claude discovery uses the maintained junction script.
- **External resources**: read local Codex session logs as evidence only; do not modify, copy, or commit them.
- **Reference workflow compatibility**: no rebuild; require future build contracts, complete disposition accounting, and comprehensive quarantine around existing signatures, manifests, assessments, and audit scripts.
- **Bundled Shiny app/pipeline**: runtime and diagram are unchanged; only future action-state workflow guidance changes.
- **Hosted impact**: N/A. No `R/`, `DESCRIPTION`, `inst/shiny/`, `site/`, README/pkgdown input, `tools/wasm/`, or deployment workflow changes; `-HostedAppStatic` is not triggered.

## Package Surfaces

- `.specify/memory/constitution.md`, `.specify/templates/plan-template.md`, `.specify/feature.json`: amend governance and activate this tranche.
- `.agents/skills/`, `.claude/skills/`: add the rebuild skill and synchronize affected workflows.
- `AGENTS.md`: point to plan 050 and summarize the new durable routing rules.
- `R/`, `tests/testthat/`, `benchmarks/`, `workflows/`, `inst/`, `DESCRIPTION`, `NEWS.md`, generated docs, hosted sources: unchanged.

## Work Checklist

- [x] Amend the constitution, version/date, and Sync Impact Report with the four evidence-backed contracts.
- [x] Synchronize `AGENTS.md`, the plan template, and shared Spec Kit planning/implementation skills.
- [x] Create and validate `openspecy-rebuild-reference-library`; strengthen the Shiny skill's action-state workflow; recreate its Claude junction.
- [x] Verify plan limits, YAML/name constraints, shared-skill equality, Markdown/diff hygiene, owned processes, status, and scratch cleanup.

## Verification

- Evidence basis: top-level project session logs covering the five named tranches; exclude spawned-agent and unrelated workspace sessions.
- Skill validation: parse each changed frontmatter and `agents/openai.yaml`; enforce lowercase hyphenated folder/name equality and required metadata.
- Synchronization: compare affected `.agents/skills/speckit-*` files byte-for-byte with `.claude/skills/speckit-*`; confirm the new OpenSpecy junction target.
- Static review: plan nonblank lines <=100; no unexplained placeholders; `git diff --check`; inspect changed hunks and `git status`.
- Package/app/hosted gates: N/A because runtime-consumed sources do not change.

## Risks And Open Questions

- Session files aggregate many turns; the review unit is the five latest substantive development tranches, explicitly documented above, rather than treating spawned agents as sessions.

## Approval Notes

- Approved by: maintainer request on 2026-10-19.
- Follow-up: implement comprehensive quarantine in `build_lib()` and its audits before the next official library run; no current artifact is republished by this governance tranche.

## Result Evidence

- Reviewed the five named top-level development tranches; spawned-agent traces and the unrelated presentation session were excluded, and no session logs were copied.
- Constitution advanced 3.12.0 -> 3.13.0; active plan, template, AGENTS guidance, shared Spec Kit skills, Shiny skill, and rebuild skill are synchronized.
- R/YAML validation passed for 10 project skill frontmatters and seven OpenAI metadata files; all three shared Spec Kit skill pairs are identical and the new Claude junction resolves correctly.
- Final plan is 56 nonblank lines/840 words; JSON, placeholder, trailing-whitespace, and `git diff --check` checks passed with no runtime source changes.
- No owned R process or task-created scratch remains. Pre-existing `.codex-*` evidence and `.claude/settings.local.json` were preserved unchanged.
- Maintainer correction: quarantine is the complete removal archive; cross-class removals are one reason category, not its exclusive purpose.
