# Feature Plan: Run-Gating Correctness, App Polish, and Hosted Deploy Fix

**Feature dir**: `specs/014-run-gating-reactivity-and-deploy-fix`
**Date**: 2026-08-17
**Review budget**: Under 100 nonblank lines and 1,500 words.
**Current tranche**: The "run button add"/"new run" commits (46b0778, ea6ce89) reworked `inst/shiny` around a Run-gated `run_gated_reactive()` cache but left several outputs reading raw/ungated inputs, broke the hosted Shinylive deploy's Playwright smoke test (5 straight CI failures since fa13649f), and regressed/never-restored an identification on/off switch. Fix all of it, add app-polish requests, and hit a performance target.
**Change class**: hosted/release (highest, via the CI-blocking deploy fix); bundled-app behavior throughout.

## Goal

- Make "Run" the single source of truth for the heatmap, tables, spectra state, and downloads, and get `deploy-shinylive.yml` green again.
- Correct particle-collapse/clustering/threshold behavior so results match documented intent, and get the full pipeline on a real large ENVI map under 4 minutes.

## Scope

- **In**: hosted deploy CI fix (R1); run-button styling/spacing (R2); Identification on/off switch (R3); per-tab All On/Off button (R4); heatmap/tables/downloads recompute only on Run (R5); spectra plot updates only on click or Run (R6); flat-line-below-threshold applies with collapse off too (R7); `#analysis_settings` tab-box closes on re-clicking its open tab (R8); verify/fix signal-noise-as-collapse-prefilter (R9); verify/fix spatial vs. non-spatial cluster divergence (R10); `<4 min` full-pipeline benchmark on `Roto-Mold Dust_27Nov23_control.dat` (436x421px, 427 bands) with defaults + collapse + spatial smooth + S/N threshold (signal x noise, min 0.01) (R11); heatmap-click-to-spectra latency check (R12).
- **Out**: `R/automate_particle_analysis.R`'s package-level `partial_collapse`/`nonspatial_collapse` contract (already explicitly deferred by spec 013's Risks) unless the benchmark proves the app-level fix in scope needs a package-level change; `ai_classify()`/reference-library work from spec 013 (separate, still-pending tranche).
- **Users**: App users running the hosted or local Shiny app; CI/maintainer relying on a green Pages deploy.

## Requirements

- R1. `deploy-shinylive.yml`'s Playwright smoke (`tools/wasm/shinylive-smoke.spec.js`) passes; its local mirror (`tools/shiny-local-smoke.spec.js`) is updated to match corrected semantics.
- R2. `.btn.openspecy-run-button`'s clean state uses `var(--openspecy-canvas)` background (matches `.content-wrapper`) with a visible border/text color; dirty (green) state unchanged. `#spectra_box` gets `margin-top` so it's visibly separated from the upload/Run row.
- R3. Identification Strategy box gets an on/off switch (`identification_active`, default on); off skips `identify_blockwise`/`ai_output_gate` and all identification-derived outputs (Top Matches, match coloring, correlation threshold, which requires identification and must itself be disabled/hidden when off).
- R4. Preprocessing, Identification, and Advanced tabs each get a header button that flips every `app_control_box` switch in that tab together; label reflects the action ("Turn All On"/"Turn All Off") based on current state.
- R5. `map_color_choices`, `current_heatmap_data`, `output$choice_names`, `output$download_ui`, `output$particle_plot`/`material_plot`, and `output$particle_partition_status` stop reading `particle_pipeline_enabled()`/raw `input$` directly; they derive only from the Run-gated `pixel_projection()`/`canonical_state()` payload. `memory_preflight_status` may stay live (advisory text only, runs no analysis).
- R6. `RawR_plot()` and `matches_to_single()` stop reading `particle_pipeline_enabled()`/`input$cor_threshold_decision` directly; spectra selection reruns only on `data_click` or a changed gated result.
- R7. Apply `signal_eligible()` masking in `canonical_state_gate`'s `!collapse` branch too (currently `identity_pixel_mapping()` hardcodes every pixel kept), so a rejected pixel flat-lines to zero whether or not Collapse is on.
- R8. DROPPED. Two independent click-handler implementations (raw class check, then a `shown.bs.tab`-tracked version) both caused `#analysis_settings_box` to collapse unexpectedly mid-journey in unrelated tests (Quantification tab content going invisible). Reverted; left for a future tranche with real DOM/JS investigation time.
- R9. CONFIRMED CORRECT by code read + real-map run: `canonical_state_gate`'s collapse branches already filter by `signal_eligible()` on `spatial_data()` (raw + optional spatial smooth) before `.partition_particle_map()`/`ordinary_process()`. No change made.
- R10. CONFIRMED CORRECT: on a real multi-particle CA_tiny_map run, `nonspatial_collapse` produced 4 particles and `partial_collapse` produced 1 (merged same-material connected region) from identical inputs -- genuine divergence. `particle_partition_status` now derives its `strategy`/`collapse` flags from `canonical_state()$settings` (a new run-time snapshot) instead of live inputs, fixing a display bug (R5) rather than the settings-source theory in the original plan (`cluster_partition$settings`'s PCA/K numbers are legitimately shared between modes; no change needed there).
- R11. Full pipeline on the specified file, default settings + Collapse + Spatial Smooth + Threshold S/N (signal x noise, min 0.01): measured 386s (real browser, upload 100.5s + Run 285.5s), down from ~500s baseline. STILL OVER the 4-minute (240s) target after the R9/R11 optimizations below; see Risks.
- R12. Heatmap click -> spectra plot update: no code-level lag found (click handlers write directly to `data_click`, no debounce); not independently re-timed in a real browser after R11's fixes given time spent on R11 itself.

## Technical Decisions

- **Approach**: Audit every consumer of `particle_pipeline_enabled()`/raw settings `input$` outside `run_gated_reactive()` and reroute onto the gated payload; the click-driven spectra path and advisory memory-preflight text are the only intentional live exceptions. Add one new gate input (`identification_active`) threaded through `canonical_state_gate`. All-On/Off and tab-collapse are additive UI/server features using `shinyWidgets::updatePrettySwitch()` and a small `shinyjs::runjs`/`tags$script` handler, no DOM state hacks.
- **Public API**: No package (`R/`) export changes expected; app-only `input$`/`output$` additions.
- **Dependencies**: None new (`shinyjs`, `shinyWidgets` already used).
- **OpenSpecy contract**: Unaffected; `wavenumber`/`spectra`/`metadata` flow through `canonical_state_gate` unchanged except R7's masking fix.
- **Generated artifacts**: N/A (no roxygen/NAMESPACE surface touched).
- **External resources**: Benchmark input is a local, non-repo file (`C:\...\Positive_Controls\Roto-Mold Dust_27Nov23_control.dat`); no network calls, no repo fixture added.
- **Bundled Shiny app**: Canonical reactive stays `canonical_state_gate` -> `canonical_final`/`DataR` feeding heatmap, spectra, tables, quantification, and downloads. Owner/child gating: `identification_active` gates `lib_type`/`top_n_input`/correlation-threshold children. Affected states: no-upload (unchanged), processed (R7 threshold masking), identified (R3 off-switch), map/batch (R5/R6/R10 heatmap+spectra gating, clustering), download (R5). Verify with genuine uploads (`raman_hdpe.csv`, `CA_tiny_map.zip`, the large ENVI file) plus console/screenshot review.
- **Hosted Shinylive/WebAssembly app**: R1 only; no wasm package/dependency pin, route, or library-staging change expected — the fix is app-behavior/test-expectation correctness, verified against the existing pinned wasm workflow contract.

## Package Surfaces

- `R/`: `correct_spike.R` -- added `.local_residual_metrics_matrix()`, a batched-across-columns sibling of `.local_residual_metrics_complete()` (identical output, verified against the per-column loop); `.detect_residual_spikes()` calls it once per correction pass instead of looping per spectrum. Same-output improvement per AGENTS.md.
- `tests/testthat/`: `test-run_app.R` (metadata-snapshot input-id list updated for `identification_active`); `test-correct_spike.R` unchanged and still passing (105/105).
- `benchmarks/`: `spike_correction.R` (pre-existing) reran clean; no new script added (large-ENVI timing used a temporary, non-repo Playwright test removed after measurement -- see Risks).
- `.github/workflows/`: unchanged; root cause was app-level, not workflow-level.
- `inst/`: `inst/shiny/global.R`, `server.R`, `ui.R`.
- `site/vignettes/README/pkgdown`: unchanged.
- `DESCRIPTION`: unchanged.
- `NEWS.md`: entry added under the unreleased 1.7.1 section.
- Bundled Shiny app: full state-matrix pass; headless `run_app` assertions (607 pass) plus the full local Playwright journey (4/4 pass) on the final candidate.
- Hosted Shinylive/WebAssembly app: local mirror (`tools/shiny-local-smoke.spec.js`) green; genuine hosted confirmation happens on the next CI run (no local wasm artifact download without a GitHub token -- see Risks).

## Work Checklist

- [x] Reproduce the CI Playwright failure locally and identify root cause: an observer-priority race between `run_gated_reactive()`'s independent `observeEvent(input$run_analysis, ...)` hooks let `quantified_data_gate` read `canonical_final()` before `canonical_state_gate` (and the dirty/reset-clearing observer) had run, silently leaving `quantified_data()` at `NULL` and crashing `#eventmetadata`.
- [x] Fix: `run_gated_reactive(compute, priority)` + explicit `RUN_GATE_PRIORITY_RESET`/`_CANONICAL` constants make ordering deterministic.
- [x] Fix `inst/shiny/server.R` reactive gating (R5-R7) and add `identification_active` (R3), incl. a `run_settings` snapshot on `canonical_state_gate`'s result (collapse/strategy/threshold/correlation/min-snr/max-snr/min-cor at Run time) so every consumer reads that instead of live inputs.
- [x] `inst/shiny/ui.R`: run-button CSS (R2), spectra-box spacing (R2), identification switch (R3), All-On/Off buttons (R4).
- [x] R8 attempted and reverted (see Risks).
- [x] Investigated R9-R10 with the real ENVI map: both already correct; fixed `particle_partition_status`'s live-input reads instead.
- [x] Updated `tools/wasm/shinylive-smoke.spec.js` and `tools/shiny-local-smoke.spec.js` for corrected gating semantics (R1) -- both now click Run after changing `threshold_decision`/`MinSNR` before expecting the download list to reflect it.
- [x] Fixed two unrelated pre-existing bugs surfaced once the above unblocked test progress: `map_color_choices()` rendering before the first Run (stale "Signal/Noise" default that never got replaced) and a duplicate `id="columns_selected"` between a `uiOutput` wrapper and its inner `selectInput`.
- [x] Vectorized spike detection across pixels (`R/correct_spike.R`) and raised `identify_blockwise`'s match block size 100->1,000; measured before/after on the large ENVI file.
- [x] `NEWS.md` entry.

## Verification

- Direct regression: one behavioral check per R1-R12 above; the metadata-crash root cause was confirmed via `shiny::testServer()` with debug instrumentation (temporary, removed) before the general priority fix.
- Focused tests: full `devtools::test()` (touched `R/correct_spike.R`) -- 0 failures, only pre-existing unrelated warnings in `test-as_OpenSpecy.R`.
- Full local Playwright journey (`tools/shiny-local-smoke.spec.js`): 4/4 pass on the final candidate (down from ~15-17 min to ~3 min wall-clock once the metadata-crash and premature-render bugs were fixed).
- `devtools::document()`/`devtools::check()`: N/A, no exported/roxygen surface changed.
- Benchmarks: `Rscript benchmarks/spike_correction.R` clean (105/105 `test-correct_spike.R` pass); large-ENVI timing measured via a temporary Playwright probe (added to, then removed from, `tools/shiny-local-smoke.spec.js`) since the file is local-only and not a repo fixture.
- Shiny affected states: no-upload, processed, identified-off, map/collapse/spatial/threshold, download -- all covered by the full local Playwright run.
- Shinylive/WebAssembly trigger: local mirror green; the actual `deploy-shinylive.yml` CI run is the authoritative confirmation and is maintainer-triggered (push not authorized in this session).
- Reusable evidence: the full local Playwright pass and `devtools::test()` above cover every changed file in this tranche; re-run only if `inst/shiny/*` or `R/correct_spike.R` change again.

## Risks And Open Questions

- **R11 target not met.** Measured 386s (real browser) vs. the 240s target, down from ~500s. Remaining cost is dominated by `tcrossprod` correlation matching (~92s, `R/match_spec.R`, scales with query-pixel count x library size -- inherent to the correlation-threshold strategy's per-pixel identification pass) and file upload/read/correction overhead (~100s for a 313MB/183k-pixel file). Block-size tuning (100->1,000) did not measurably help, confirming the cost is FLOP-bound, not chunking-overhead-bound. Further reduction would need either a scientific-behavior change (e.g., skipping some preprocessing step in the per-pixel identification pass) that needs explicit sign-off, or more package-level vectorization in a follow-on tranche.
- R8 (tab-box collapse on re-click) is reverted, not shipped; the DOM/JS interaction with bs4Dash's own tab-switch handling needs dedicated investigation time this tranche didn't have left.
- No GitHub token/`gh` CLI available in this environment, so CI logs and wasm artifacts can't be pulled directly; the actual `deploy-shinylive.yml` run on push is the first real confirmation this fix is sufficient.
- Raw `& playwright.cmd test <file>` invocations intermittently failed with a `test.beforeAll()` "did not expect" error unrelated to file content (reproduced even on a trivial file); routing through `quality-gates.ps1` (which the working test suite already used successfully all session) avoided it every time. Cause not root-caused; flagging so a future session doesn't lose time on it.
- R12 (click-to-spectra latency) was not independently re-timed after the R11 fixes in a real browser; the `CLICK_TO_SETTLE_MS_APPROX` probe from the temporary timing test read ~5.1s, but that measurement's methodology (a `waitForFunction` polling a nonexistent property) was not rigorous enough to trust -- worth a dedicated, real timing check.

## Approval Notes

- Approved by: Win Cowger (auto-accepted per explicit request to auto-accept and implement)
- Follow-up: (1) R8 tab-collapse-on-reclick needs dedicated DOM investigation. (2) R11's remaining ~146s gap needs either a product decision on preprocessing scope for the per-pixel identification pass, or more `R/` vectorization work, scoped as its own tranche. (3) R12 needs a rigorous real-browser click-latency measurement.
