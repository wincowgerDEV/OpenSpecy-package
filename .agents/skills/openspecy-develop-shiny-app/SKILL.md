---
name: openspecy-develop-shiny-app
description: Implement and verify OpenSpecy's bundled Shiny application under inst/shiny. Use for local app UI, server, reactivity, preprocessing or identification orchestration, plots, tables, downloads, progress, themes, metadata, responsive layout, or local-browser regressions.
---

# Develop The Bundled OpenSpecy App

Keep `inst/shiny/` as the canonical source shared by the local package app and
hosted Shinylive build. Use the active plan for feature behavior and this skill
for integration and verification procedure.

## Workflow

1. Read the active tranche and `AGENTS.md`, then locate relevant constitution
   rules, app symbols, consumers, tests, and browser journeys with `rg`. Read
   bounded regions first; read complete large app/test files only when a
   cross-cutting reactive pipeline or ownership question requires it.
2. Map each changed user action through its owner control, dependent inputs,
   canonical reactive data, plots/tables, identification or quantification,
   metadata, and downloads. Resolve divergent consumers before editing.
   Cross-check the mapped flow against `.specify/memory/pipeline-diagram.html`
   and update the diagram in the same change when a stage, branch, or function
   call moves (Constitution Principle X).
3. Put pure/sourceable app helpers in `global.R`, declarative controls and
   theme tokens in `ui.R`, and orchestration in `server.R`. Move logic into
   `R/` only when it has a stable package-level contract.
4. Implement the smallest coherent change, then iterate with source parsing,
   the narrowest `run_app` assertions, and one browser journey selected by
   name when visual or interactive evidence is required.
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
  and plots. For every non-obvious scientific or processing control group,
  informational disclosures must name each input, its units or scale, how its
  modes or higher/lower values affect analysis, and material rejection, no-op,
  or interpretation consequences.
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

Use the maintained gate script from the repository root. During presentation
or help-text iteration, run static parsing plus focused assertions:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppStatic
```

For changed reactivity, plotting, selection, or downloads, add one targeted
browser journey before the final broad pass:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser -BrowserGrep "<changed journey>"
```

Run the complete browser workflow once on a coherent final app candidate when
the change is cross-cutting or release-facing. Add `-FullTests` only when
package/scientific code changed or the plan's class requires it:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  .agents\skills\openspecy-run-quality-gates\scripts\quality-gates.ps1 `
  -Filter run_app -BundledAppBrowser
```

This parses the app, runs focused tests, checks the browser test syntax, and
runs the real local Playwright workflow with one worker. It runs full package
tests only when `-FullTests` is supplied. Use `-Check` only when the maintainer
explicitly requests a full package check or the plan is release/CRAN-facing.

On the first browser failure, retain its trace, screenshot, console, and server
diagnostics and inspect them before rerunning. After two failures in the same
stage, reduce the journey or add a direct state probe before another full
browser attempt. Prefer semantic Shiny acknowledgements and stable-result
conditions to fixed waits, and reuse one local app process when practical.

Inspect screenshots and downloaded files for the changed states rather than
loading every unchanged artifact. Run `git diff
--check`, confirm no generated package or hosted output was hand-edited, and
report `inst/shiny/www` file count/bytes; perform a detailed orphan/compression
audit when assets or dependencies changed.

## Handoff

Report changed app states, the canonical reactive feeding all consumers,
focused/targeted/final gate results, download evidence, changed screenshot
review, asset-size impact, hosted checks run or not applicable, and whether
full package tests or `R CMD check` were triggered. Never claim public
deployment success from local smoke.
