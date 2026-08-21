---
name: "speckit-implement"
description: "Implement the current concise Spec Kit plan by executing the Work Checklist in plan.md."
---

# Implement From Plan

Use `plan.md` as the source of truth. Separate `tasks.md` files are legacy and
are not required.

## Workflow

1. Run `.specify/scripts/powershell/check-prerequisites.ps1 -Json`, read the
   active `plan.md` and `AGENTS.md`, and read the constitution sections for the
   affected surfaces. Do not reread identical injected guidance or the full
   constitution unless governance, cross-cutting behavior, or a conflict
   requires it.
2. Extract the `Work Checklist`, package surfaces, verification commands, and
   open questions from the plan.
3. Identify the bounded current tranche, classify its highest affected surface,
   and record the smallest verification ladder that covers every changed
   behavior. Confirm the plan also classifies hosted impact. Shared hosted
   inputs trigger `-HostedAppStatic`; use matching-artifact and clean-rebuild
   tiers only when their runtime/assembly or dependency/build inputs change.
   Passing evidence remains valid until a covered file, dependency, input, or
   contract changes.
4. If an open question blocks correctness, ask it before editing. Otherwise make
   conservative assumptions and proceed.
5. Locate owning symbols and consumers with `rg`, then read bounded source and
   test regions. Read whole large files only for cross-cutting ownership or
   pipeline work.
6. Implement checklist items in dependency order. Update the checkbox in
   `plan.md` as each item is completed.
7. Keep changes scoped to files named or implied by the plan and constitution.
8. For public API changes, apply `openspecy-design-public-api` before editing
   signatures.
9. Run staged verification with `openspecy-run-quality-gates`. Iterate only on
   the directly affected focused gate; run full browser, full tests,
   documentation, and package check once for the final candidate and only when
   their class trigger is present. Report skipped long-running,
   external-resource, optional-backend, or CI-only checks precisely.
10. If routine app work exceeds 30 minutes, reveals a higher change class, or
    the same broad gate fails twice, stop broad reruns, isolate a focused
    reproducer, and tell the user what expanded before continuing.
11. For reference-library or other long-running external workflows, run a small
   representative probe first, then isolated expensive stages with logs and
   temporary outputs before the full workflow. Compare rebuilt artifacts against
   available legacy identifiers, wavenumber axes, metadata shape, warnings, and
   representative `OpenSpecy` joins or matches before marking the work complete.
12. For bundled Shiny app work, apply `openspecy-develop-shiny-app`. Keep one
   final processed `OpenSpecy` reactive behind plots, summaries, identification,
   quantification, metadata, and downloads; gate child inputs behind their owner
   controls; preserve native downloads; and verify the affected app-state matrix
   with focused tests plus a manual or CI-guarded browser smoke when relevant.
13. For hosted Shinylive/WebAssembly work, treat the bundled `inst/` app and
    package source as canonical, fix and verify the "Build and deploy wasm R
    package repository" workflow when affected, keep the app on a hardcoded
    package version/commit pin plus pinned app dependency closure, stage only
    the small medoid/model libraries with guarded `get_lib()` workflows unless
    the plan says otherwise, avoid hand-editing generated web artifacts, and run
    a CI-guarded or manual smoke test that covers startup, assets, dependency
    resolution, and library matching. Apply `openspecy-verify-hosted-app` for
    action-equivalent assembly and `openspecy-test-hosted-app-browser` for
    landing/pkgdown/Shinylive interaction or presentation changes.
14. Hosted browser fixtures must set output-determining controls explicitly.
    Do not encode a UI default as fixture setup unless that default is the
    behavior the regression is intended to test.

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
- Do not run `git push`, `git pull`, or `git pull --rebase` unless the user
  explicitly authorizes that specific remote operation in the current request.
  Prior or standing permission does not carry forward; prepare a local handoff
  for maintainer synchronization by default.
- Load the minimal skill set for the current tranche: app skills for app work;
  package/Rscript/API/generated-doc skills only when their surfaces change; and
  hosted skills only for hosted source, runtime, workflow, pin, or public-route
  changes.
- Keep tool output decision-sized: prefer `git diff --stat`/`--name-only`,
  changed hunks, compact reporters, and short failure excerpts. Retain complete
  verbose logs outside conversational context when filtering them.
- Parallelize only independent surfaces with one owner each and concise task
  packets. Do not request redundant reviews of a contract already covered by a
  direct regression unless its scientific or release risk warrants it.
