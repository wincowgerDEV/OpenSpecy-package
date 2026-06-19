---
name: openspecy-run-quality-gates
description: Run staged OpenSpecy R package verification on Windows. Use after package code, tests, benchmarks, roxygen, DESCRIPTION, NAMESPACE, man pages, vignettes, or network-dependent tests change, and before claiming a feature or release-facing change is complete.
---

# Run OpenSpecy Quality Gates

Use `scripts/quality-gates.ps1` for repeatable local verification.

## Sequence

1. Inspect `git status --short` and preserve unrelated user changes.
2. Run the smallest focused test filter first.
3. Run affected benchmarks before the full suite. Subsecond comparisons need
   repeated timings; equivalent-output regressions over 10 percent must fail or
   be justified in the active plan.
4. Before `devtools::document()`, compare installed roxygen2 with
   `Config/roxygen2/version` in `DESCRIPTION`. Stop on mismatch.
5. Run documentation once, then inspect generated diffs. Author, contributor,
   reference, alias, and export changes require corresponding source changes.
   Never repair generated files manually.
6. Run the full local tests once after focused tests pass.
7. Run `devtools::check(document = FALSE)` once for release-facing work.

## Windows Rules

- Use `C:\Program Files\R\R-4.3.3\bin\Rscript.exe` when present.
- If absent, discover `Rscript.exe` under `C:\Program Files\R`.
- Run Spec Kit PowerShell scripts with
  `powershell.exe -ExecutionPolicy Bypass -File ...`; do not change the user's
  machine execution policy.
- Ignore Windows Store executable aliases. Resolve a real executable once and
  reuse its absolute path.

## External Tests

Network-dependent tests must skip against the actual download host and should
not be part of routine local success criteria. When a full test/check fails:

1. Confirm focused tests passed.
2. Identify the exact failing URL and host.
3. Verify the test's offline guard covers that host and resource.
4. Report the external blocker precisely; do not describe the full suite as
   passing.

## Script

Run from the repository root:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter build_lib -Benchmark benchmarks\library_builder.R -Document
```

Add `-FullTests` and `-Check` only after earlier gates pass.
