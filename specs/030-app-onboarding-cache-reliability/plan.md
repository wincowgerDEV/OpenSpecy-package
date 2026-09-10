# Feature Plan: App Onboarding, Cache, and First-Run Reliability

**Feature dir**: `specs/030-app-onboarding-cache-reliability`  
**Date**: 2026-09-09  
**Status**: Implemented locally; fresh Action-artifact gate pending  
**Current tranche**: Guided workflows, runtime caching, offline desktop bundles, compatibility warnings, all-off controls, and fresh-session fixes.  
**Change class**: mixed; highest class is hosted/release because the generated service-worker runtime and Pages artifact contract change.

## Goal And Scope

- Make the apps approachable on a first visit, reliable on the first Run, and faster after their WebAssembly assets have been fetched once.
- Preserve one Run-gated canonical `OpenSpecy` result and the package's pinned hosted build; tutorial actions must exercise the real controls rather than a duplicate analysis path.
- **In**: an Action producing offline desktop archives; no-R documentation; versioned caching; Process/Identify/Quantify walkthroughs; library warnings; all-off actions; startup/Plotly and first-Run selection repairs.
- **Out**: `file://` execution, mobile/unsupported CPU launchers, bundling a browser, new scientific algorithms or defaults, new reference/model artifacts, public R APIs, and remote synchronization.

## Requirements

- R1. A new GitHub Action consumes an exact successful `openspecy-pages-<SHA>` artifact and publishes ready-to-unzip archives for Windows x64, macOS Intel/Apple Silicon, and Linux x64/ARM64. Each contains `/`, `/app/`, `/pkgdown/`, and a native loopback-only launcher; runtime needs no R, Python, Node, additional download, installation, or internet. The Action validates artifact/SHA identity and never floats the pinned closure.
- R2. An offline archive starts from a spaced path, binds only `127.0.0.1` on an available port, and completes startup, test-data processing, medoid identification, Top Matches selection/download, and quantification with non-loopback requests blocked. `vignettes/app.Rmd` documents download, identity/checksums, launch/stop, supported targets/browser, local data handling, and why `file://` is unsupported.
- R3. After one successful hosted/static load, browsers reuse same-origin `app.json`, webR, pinned package image, and Shinylive assets. Cache/API/quota failure falls back to network. A SHA-derived worker/cache key activates new deployments and removes only obsolete OpenSpecy caches automatically.
- R4. A prominent **Walk me through** button opens large accessible Process, Identify, and Quantify choices. Each session path discloses upload replacement, loads packaged data through the normal input/reset boundary, highlights controls, applies a named comparison, and reuses ordinary Run. Back, Next, Repeat, and Exit work on keyboard/mobile; Exit keeps the latest result.
- R5. Walkthrough and existing "What this changes"/workflow disclosures share one guidance source. Text names inputs, units/scales, higher/lower effects, owner-off no-ops, and interpretation/rejection consequences.
- R6. On an identification Run, a nonblocking warning appears when Derivative lacks enabled absolute first derivative processing, or No Baseline lacks baseline correction/retains an active derivative transform. It names the exact corrective controls and notes that deliberately preprocessed uploads may proceed; configuration changes alone do not pop warnings.
- R7. The top action on Preprocessing, Identification, and Advanced always turns every switch off; it never offers or performs "all on." Muted child values remain inert until their owner is enabled.
- R8. A fresh `run_app()` session reaches a quiet no-analysis state without evaluating `ncol(DataR()$spectra)` or unregistered `heat_plot` click data. The first successful identification of `inst/extdata/raman_hdpe.csv` immediately renders Selection Metadata, selects Top Matches rank 1, and lets rank changes update the reference/metadata without a settings change or second Run.

## Technical Decisions

- **App controller**: keep tutorial definitions/state internal to `global.R`/`server.R`; `ui.R` renders the modal/CSS. Flush input updates before clicking existing `run_analysis`, preserving gates, progress, downloads, and dirty state. Add no dependency or persistent data.
- **Guidance**: define stable control/topic IDs with display text once and consume them from both `app_control_box()` footers and tutorial steps; tests fail on missing/empty topic mappings.
- **Readiness/selection**: selection accessors inspect nullable `canonical_state()$object` and return `NA` before results. Initialize plot/pixel/rank, `meta_cache`, DT proxies, and Top Matches at one result-ready boundary; transient clears cannot erase valid selection. Read Plotly clicks only after a multi-spectrum heatmap is renderable and keep `event_register()`.
- **Compatibility warning**: a pure helper evaluates the Run snapshot, and one Run observer displays its messages; it does not block intentional expert workflows or mutate settings.
- **Caching**: add a deterministic `tools/wasm/` post-export configurator that validates worker markers, uses cache-first only for successful same-origin app/immutable payloads, stamps the SHA into worker/cache names, and makes writes best-effort. Keep `registration.update()`. Workflow/preflight commands augment generated output; never hand-edit `_site/` or `_wasm/`.
- **Offline archives**: cross-compile a standard-library Go launcher from `tools/offline/` after the Pages build succeeds. Package the exact Pages tree per target with instructions/checksums and validate it in CI. A modern browser is the only prerequisite; document one-time macOS approval when signing credentials are unavailable.
- **OpenSpecy contract**: tutorial data becomes the same canonical three-part `OpenSpecy` input (`wavenumber`, aligned `spectra`/`metadata`, valid attributes) and then uses `canonical_state_gate()`/`quantified_data_gate()`. Visible plots, selection metadata, matches, quantification, and downloads remain consumers of that one Run result.
- **Pipeline diagram**: update `.specify/memory/pipeline-diagram.html` at **Click Run**, **Top Matches Table**, and **Selection Metadata Table** to show tutorial input convergence and post-result selection readiness; no scientific processing stage or branch changes.
- **Hosted contract**: `/`, `/app/`, and `/pkgdown/` remain siblings; `site/` stays source-only, README iframe-free, and external videos click-to-load. Preserve pins, wasm closure, compact-library staging, and generated boundaries. Run `-HostedAppStatic`, fresh exact-artifact preflight/nested-frame smoke, and a clean-commit wasm rebuild before release.

## Package Surfaces And Work Checklist

- [x] `inst/shiny/{global.R,ui.R,server.R,www/parent-frame.js}`: shared guidance, walkthrough controller/UI, all-off actions, Run warnings, readiness/selection repair, and guarded heatmap events; add no large assets.
- [x] `tests/testthat/{test-app-onboarding.R,test-run_app.R,test-run_app_reactivity.R,test-app-in-memory-helpers.R}`: guidance/tutorial presets, owner gating, warning matrix, no-analysis startup, Plotly registration timing, and fresh-session first-identification selection regressions.
- [x] `tools/wasm/` configurator/check/preflight scripts, `.github/workflows/deploy-shinylive.yml`, and `tests/testthat/test-shinylive_wasm.R`: versioned best-effort cache generation, restricted eviction, route/pin preservation, and cold/warm/update harness evidence.
- [x] `tools/offline/`, `.github/workflows/build-offline-shinylive.yml`, `site/`, and hosted contract tests: exact Pages-artifact lookup, five native launchers/archives, manifests/checksums, lazy external media, loopback/path-with-spaces launch, and internet-blocked full app smoke.
- [x] `vignettes/app.Rmd` and `NEWS.md`: no-R static-bundle tutorial and user-visible behavior/fix notes; render the vignette/pkgdown, never edit generated HTML.
- [x] `.specify/memory/pipeline-diagram.html`: synchronize tutorial-to-Run convergence and first-result selection readiness.
- [x] Apply `openspecy-develop-shiny-app`, `openspecy-rscript-windows`, `openspecy-run-quality-gates`, and `openspecy-verify-hosted-app` during implementation/verification; reconcile every checkbox with evidence and clean owned processes/scratch before handoff.

## Verification

- Focused: parse app/R/JavaScript/PowerShell sources; configured Windows Rscript runs compact `run_app|app-in-memory-helpers|shinylive_wasm` tests, including a `shiny::testServer()` first-Run fixture and identical tutorial/manual Run results.
- Bundled browser: from a fresh session, run each walkthrough with genuine packaged data; verify A/B reruns, guidance, modal focus/mobile layout, nonblocking compatibility warnings, all-off behavior, immediate metadata/rank-1 selection, rank switching, downloads unaffected, and no severe console/server warnings. Inspect `inst/shiny` count/bytes; no material asset growth expected.
- Hosted: run `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -HostedAppStatic`; configure and validate the generated worker in the action-equivalent build; with a fresh matching artifact run `tools/wasm/test-shinylive-action.ps1` and nested-frame startup/upload/identification/download smoke across `/`, `/app/`, `/pkgdown/`.
- Cache/offline acceptance: record cold versus retained-profile reload requests/bytes and startup milestones; confirm cache hits, SHA rotation, scoped eviction, and safe Cache Storage failure. Extract a produced archive under a spaced path and run its native launcher with non-loopback requests blocked through startup, processing, identification, Top Matches download, and quantification; verify no missing resource/package fetch. Report that caching removes repeat transfer latency but cannot eliminate webR initialization/decompression time.
- Docs/toolchain: render `vignettes/app.Rmd` and pkgdown with configured tools; `devtools::document()` is N/A (no roxygen/export changes). Full `devtools::test()` runs once because hosted assembly and multiple app states change; R CMD check is deferred unless release/CRAN-facing review requests it. Benchmarks and long reference workflows are N/A: this adds UI/runtime behavior, not package algorithms or library rebuilds.
- Reusable evidence: retain only results whose covered files, pins, dependencies, inputs, and browser contract are unchanged. Final cache readiness requires one clean-commit wasm rebuild/fresh artifact; older `_wasm/` artifacts may inform diagnostics but cannot close that gate.

## Risks And Open Questions

- Browser storage quotas and eviction vary; cache failures must remain invisible and safe, and measured warm-start benefit may be bounded by CPU-side webR startup rather than transfer.
- Native launchers are unsigned unless release signing credentials exist; Windows reputation and macOS Gatekeeper messaging must be documented honestly, and the archives are portable rather than installed applications.
- Programmatic tutorial updates can race Shiny input flushes or replace user work; explicit replacement disclosure, flush-before-Run sequencing, and fresh-session/browser tests are release blockers.

## Approval Notes

- Approved by: maintainer on 2026-09-09, with self-contained offline Action amendment.
- Follow-up: implementation is authorized; push/pull remains unauthorized.
