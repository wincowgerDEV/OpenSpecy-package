---
name: openspecy-test-hosted-app-browser
description: Exercise OpenSpecy's pkgdown-embedded Shinylive app through nested browser frames. Use for hosted upload, identification, download, app-mode, loading, busy-overlay, responsive-layout, console, or screenshot regressions.
---

# Test The Hosted App In A Browser

Prefer extending `tools/wasm/shinylive-smoke.spec.js` so local and CI behavior
stay aligned.

## Frame Model

The hosted interaction crosses three documents:

1. pkgdown page;
2. `#openspecy-app-frame` Shinylive shell;
3. Shinylive's `iframe.app-frame` containing Shiny.

Wait for the app's `openspecy:ready` handshake and pinned package-version text.
An outer iframe load event alone does not mean Shiny is ready.

## Interaction Workflow

1. Build or reuse an action-equivalent assembled Pages tree with
   `openspecy-verify-hosted-app`.
2. Start the static server and Playwright in one PowerShell `try/finally`; stop
   the server before returning. Do not leave an untracked background process.
   Keep the generous overall WebAssembly startup timeout, but use short
   per-action timeouts while stabilizing selectors so a geometry mistake does
   not consume the full smoke-test budget.
3. Capture console errors, page errors, failed requests, and HTTP responses of
   400 or higher before navigation.
4. Verify the loading view, readiness message, pinned OpenSpecy version, and
   enabled app-mode control.
5. Enter app mode and assert the shell class plus document scroll lock. Press
   `Escape`; the mode must remain active until the explicit Exit control runs.
6. Verify a genuine browser file chooser while app mode is active. Nested
   Shiny file inputs can have unreliable hit geometry in Playwright; use a
   temporary visible parent-page chooser probe for this browser behavior, then
   use `setInputFiles()` on Shiny's real input for the actual upload workflow.
7. Upload `inst/extdata/raman_hdpe.csv`, wait for completion, enable
   identification, and require a representative polyethylene result.
8. Observe `openspecy-busy-visible` class transitions around result rendering.
   After the first result is visible, wait past the debounce interval and fail
   if a blocking busy class appears again.
9. Download Top Matches, verify a nonempty CSV and representative result text,
   and confirm app mode remains active after chooser, upload, and download.
10. Exit only through the toolbar control. Capture loading, embedded desktop,
    expanded, and mobile screenshots and inspect them for clipping, overlap,
    blank frames, stale overlays, and unreadable controls.

## Local App Parity

Busy-state and other shared UI fixes belong under `inst/shiny/`, not only in
the generated Shinylive app. Apply `openspecy-develop-shiny-app` first for
shared-source changes and reuse its no-upload/processed/identified state,
genuine-download, console, and screenshot evidence before testing nested-frame
behavior. Runtime-specific differences still require hosted coverage here.

## Browser Rules

- Use page-owned fixed viewport state when Browse/Download must not exit app
  mode; browser-native Fullscreen is allowed to close for native browser UI.
- Keep long browser tests manual or CI-guarded.
- Diagnose selector, frame, and hit-geometry failures as harness failures before
  changing application code.
- Inspect screenshots, not only DOM assertions.
- Treat severe package/runtime console errors as failures; report benign browser
  noise separately.
- Do not claim public deployment success from a local server alone.
