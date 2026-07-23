---
name: openspecy-develop-shiny-app
description: Implement and verify OpenSpecy's bundled Shiny application under inst/shiny. Use for local app UI, server, reactivity, preprocessing or identification orchestration, plots, tables, downloads, progress, themes, metadata, responsive layout, or local-browser regressions.
---

# Develop The Bundled OpenSpecy App

Keep `inst/shiny/` as the canonical source shared by the local package app and
hosted Shinylive build. Use the active plan for feature behavior and this skill
for integration and verification procedure.

## Workflow

1. Read the active plan, constitution, `AGENTS.md`, `inst/shiny/global.R`,
   `ui.R`, `server.R`, `tests/testthat/test-run_app.R`, and
   `tools/shiny-local-smoke.spec.js`.
2. Map each changed user action through its owner control, dependent inputs,
   canonical reactive data, plots/tables, identification or quantification,
   metadata, and downloads. Resolve divergent consumers before editing.
3. Put pure/sourceable app helpers in `global.R`, declarative controls and
   theme tokens in `ui.R`, and orchestration in `server.R`. Move logic into
   `R/` only when it has a stable package-level contract.
4. Implement the smallest coherent change, then verify the affected app-state
   matrix before broad package tests.
5. Apply `openspecy-test-hosted-app-browser` and
   `openspecy-verify-hosted-app` only when hosted presentation, wasm runtime,
   workflows, pins, dependency closure, or staged libraries are affected.

## Integration Contracts

- Feed the visible spectrum, summary, identification, quantification,
  metadata, and downloads from the same final processed `OpenSpecy` reactive.
  Do not create a hidden second preprocessing pipeline.
- Branch on every owner toggle before reading child inputs. Disabled, hidden,
  or otherwise muted child controls must not invalidate analysis, run network
  work, or show the busy overlay.
- Treat dynamically rendered inputs as temporarily absent. Use explicit
  defaults or `req()` only after the owning state makes the input relevant.
- Qualify collision-prone UI functions such as `bs4Dash::box`; parse all three
  app source files before starting a browser run.
- Drive the central progress overlay only from explicit analysis phases. Show
  stage progress plus actual elapsed time; do not add duplicate native progress
  notifications or speculative completion-time estimates.
- Preserve the native `downloadButton()`/`downloadHandler()` binding. Keep
  context ordering and visible labels in tested helpers, validate that the
  server wrote a nonempty file, and browser-test changed downloads for filename
  and representative content.
- Keep empty, uploaded, processed, identified, batch, and responsive states
  useful. A table appearing does not substitute for a missing spectrum or match
  overlay.
- Extend the centralized theme to dynamic controls, sidebars, tables, dialogs,
  and plots. Informational disclosures must contain substantive details.
- Keep settings exports human-readable, but do not add an importer or imply a
  compatibility contract unless the active plan explicitly accepts that API.

## App-State Matrix

Cover the states affected by the change:

1. No upload: defaults, disabled/muted controls, empty plot, contextual test
   downloads, and no identification or network work.
2. Uploaded/processed: active spectrum, raw overlay when applicable, summary,
   correction diagnostics, and Processed Spectra download.
3. Identification active: match overlay, results table, Top Matches options,
   and genuine download content.
4. Batch/map or quantification when affected: shared-axis behavior, responsive
   summaries, saved definitions, and aligned metadata.
5. Muted configuration: changing a child while its owner is off leaves the
   analysis result and explicit progress overlay unchanged.
6. Long-running work: one accessible central phase message, monotonic staged
   progress, elapsed time, completion reset, and no severe console/server error.

## Verification

Use the maintained gate script from the repository root. For a final local app
candidate, run:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -FullTests
```

This parses the app, runs focused tests, checks the browser test syntax, runs
the real local Playwright workflow with one worker, and then runs full package
tests. Use `-Check` only when the maintainer explicitly requests a full package
check or the plan is release/CRAN-facing.

Inspect every emitted screenshot, browser console/server diagnostics, and each
downloaded file rather than relying on exit status alone. Run `git diff
--check`, confirm no generated package or hosted output was hand-edited, and
report `inst/shiny/www` file count/bytes; perform a detailed orphan/compression
audit when assets or dependencies changed.

## Handoff

Report changed app states, the canonical reactive feeding all consumers,
focused/full/browser results, download evidence, screenshot review, asset-size
impact, hosted checks run or not applicable, and whether `R CMD check` was
explicitly requested. Never claim public deployment success from local smoke.
