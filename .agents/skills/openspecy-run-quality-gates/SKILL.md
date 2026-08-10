---
name: openspecy-run-quality-gates
description: Run staged OpenSpecy R package and bundled Shiny verification on Windows. Use after package code, tests, benchmarks, roxygen, DESCRIPTION, NAMESPACE, man pages, vignettes, app source, browser smoke, or network-dependent tests change, and before claiming a feature or release-facing change is complete.
---

# Run OpenSpecy Quality Gates

Use `scripts/quality-gates.ps1` for repeatable local verification.

## Gate Selection

Classify the current tranche by its highest affected surface and run only gates
invalidated by the files or contracts that changed:

- Presentation/help/order/CSS: app parse/static checks, focused `run_app`
  assertions, and the changed-state screenshot when visual evidence matters.
- App reactivity/plot/selection/download: focused app tests plus a targeted
  Playwright journey; inspect genuine files when a download changed.
- Package/scientific behavior: focused invariant tests, a benchmark for
  same-output work, documentation only when its source changed, then one full
  package test run for the final candidate.
- Hosted/release behavior: add the hosted verification skills, matching wasm
  artifact/action-equivalent checks, or R CMD check only when those surfaces or
  release obligations are triggered.

A passed gate remains current until one of its covered files, dependencies,
inputs, or contracts changes. Do not use full browser, full package, or package
check as a debugging loop.

## Sequence

1. Inspect `git status --short` and preserve unrelated user changes.
2. Run the smallest focused test filter first.
3. For bundled app work, use `-BundledAppStatic` for R parsing, browser-test
   syntax, and asset inventory while iterating; it does not render or inspect
   the UI. Add
   `-BundledAppBrowser -BrowserGrep "<journey>"` after focused tests pass when
   browser evidence is needed. Run the complete browser spec once for the final
   cross-cutting or release-facing candidate.
4. Run affected benchmarks before the full suite. Subsecond comparisons need
   repeated timings; equivalent-output regressions over 10 percent must fail or
   be justified in the active plan.
5. Before `devtools::document()`, compare installed roxygen2 with
   `Config/roxygen2/version` in `DESCRIPTION`. Stop on mismatch.
6. Run documentation once, then inspect generated diffs. Author, contributor,
   reference, alias, and export changes require corresponding source changes.
   Never repair generated files manually.
7. Run the full local tests once after focused and app-browser tests pass when
   package/scientific code changed or the plan's verification class triggers it.
8. Add `-Check` only when the maintainer explicitly requests a full package
   check or the plan is release/CRAN-facing.

If the same broad stage fails twice, stop rerunning it and create a reduced
reproducer for the failing selector, reactive state, test, build copy, or
external dependency. Preserve the full log in a temporary or ignored evidence
path and return only the gate summary plus relevant failure excerpt to the
conversation.

## Windows Rules

- Use `C:\Program Files\R\R-4.3.3\bin\Rscript.exe` when present.
- If absent, discover `Rscript.exe` under `C:\Program Files\R`.
- Run Spec Kit PowerShell scripts with
  `powershell.exe -ExecutionPolicy Bypass -File ...`; do not change the user's
  machine execution policy.
- Ignore Windows Store executable aliases. Resolve real executables once and
  reuse their absolute paths.

## External Tests

Network-dependent tests must skip against the actual download host and should
not be part of routine local success criteria. When a full test/check fails:

1. Confirm focused tests passed.
2. Identify the exact failing URL and host.
3. Verify the test's offline guard covers that host and resource.
4. Report the external blocker precisely; do not describe the full suite as
   passing.

## Package Check Staging

This OneDrive workspace may contain ignored `_wasm`, `node_modules`, nested
repositories, and browser results. Before a release-facing package check,
inspect for those trees. Use the maintained staged-check helper, which copies
the current working versions of tracked files, applies tracked deletions,
requires every untracked file to be explicitly included or excluded, and
rejects nested `.git`, `_wasm`, `node_modules`, and test-result trees. Do not
retry a broad-copy `devtools::check()` path already known to ingest them. Record
the candidate manifest, staged source size, and retained check-log path.

`quality-gates.ps1 -Check` routes through this helper. When unrelated or
intended new untracked files exist, classify them with
`-CheckExcludeUntracked "path1;path2"` or
`-CheckIncludeUntracked "path1;path2"`; semicolon-delimited scalar values work
with the standard `powershell.exe -File` entry point. An unclassified file
stops the check before R build work begins.
Use `-Check -CheckPrepareOnly` to validate and retain the exact manifest without
running R CMD build/check.

## Commands

Package-focused example:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter build_lib -Benchmark benchmarks\library_builder.R -Document
```

Final routine bundled-app candidate without full package tests or R CMD check:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -BrowserGrep "<changed journey>"
```

Final mixed app/package or release-facing candidate without R CMD check:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -FullTests
```

Target one named Playwright journey during iteration:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -BrowserGrep "map-scale Top Matches"
```
