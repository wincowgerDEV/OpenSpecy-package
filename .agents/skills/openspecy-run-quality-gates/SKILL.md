---
name: openspecy-run-quality-gates
description: Run staged OpenSpecy R package and bundled Shiny verification on Windows. Use after package code, tests, benchmarks, roxygen, DESCRIPTION, NAMESPACE, man pages, vignettes, app source, browser smoke, or network-dependent tests change, and before claiming a feature or release-facing change is complete.
---

# Run OpenSpecy Quality Gates

Use `scripts/quality-gates.ps1` for repeatable local verification.

## Sequence

1. Inspect `git status --short` and preserve unrelated user changes.
2. Run the smallest focused test filter first.
3. For bundled app work, add `-BundledAppBrowser` after focused tests pass. The
   script parses `global.R`, `ui.R`, and `server.R`, checks the Playwright test
   syntax, reports the app asset inventory, and runs the real local browser
   smoke with one worker.
4. Run affected benchmarks before the full suite. Subsecond comparisons need
   repeated timings; equivalent-output regressions over 10 percent must fail or
   be justified in the active plan.
5. Before `devtools::document()`, compare installed roxygen2 with
   `Config/roxygen2/version` in `DESCRIPTION`. Stop on mismatch.
6. Run documentation once, then inspect generated diffs. Author, contributor,
   reference, alias, and export changes require corresponding source changes.
   Never repair generated files manually.
7. Run the full local tests once after focused and app-browser tests pass.
8. Add `-Check` only when the maintainer explicitly requests a full package
   check or the plan is release/CRAN-facing.

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

## Commands

Package-focused example:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter build_lib -Benchmark benchmarks\library_builder.R -Document
```

Final bundled-app candidate without R CMD check:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -FullTests
```
