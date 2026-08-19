# Feature Plan: Diagram-Conformance Fix and Instant Run/Preview/Download Feedback

**Feature dir**: `specs/015-diagram-conformance-instant-feedback`
**Date**: 2026-08-19
**Review budget**: Under 100 nonblank lines and 1,500 words.
**Current tranche**: An audit of `inst/shiny/server.R` against the newly built `.specify/memory/pipeline-diagram.html` found one genuine app-side conformance gap (AI-mode Top Matches Table stays blank though AI results already reach two sibling outputs), plus a separately requested UX gap: Run, Recalculate Preview, and downloads give no visible feedback for several seconds, risking double-clicks. This tranche fixes both.
**Change class**: bundled-app behavior.

## Goal

- AI-mode ("model" library) users see a populated Top Matches table, matching the AI results already shown in Selection Metadata and the Top Matches CSV.
- Clicking Run, Recalculate Preview, or a download shows a busy indicator within a perceived instant, not after a multi-second silent computation.

## Scope

- **In**: R1 Top Matches Table AI-mode branch; R2 an announce-first observer on `input$run_analysis`; R3 an immediate `analysis_phase()` call at the top of `recalculate_snr_preview()`; R4 client-side (zero-round-trip) busy/disable feedback on Run/Recalculate Preview click; R5 client-side busy feedback for the Top Matches and Thresholded Particles downloads.
- **Out**: raw computation speed (spec 014's still-open ~146s gap); relaxing the deliberate AI-mode-blocks-correlation-threshold restriction (`server.R:1349-1354`); Progress Bars' correlation/match bars in AI mode (confirmed structurally blocked upstream by design, not a bug).
- **Users**: app users on large maps who currently double-click Run/downloads; AI-mode identification users.

## Requirements

- R1. `output$event`/`top_matches()` gets an AI-mode branch. AI mode has one prediction per spectrum, not a ranked candidate list, so (discovered during implementation, correcting this plan's original draft) it mirrors `match_metadata()`'s existing AI branch -- `matches_to_single()[selected_unit_index(), ]` -- rather than the download handler's whole-map `bind_cols()` export shape, which is a different concept (every spectrum, not just the selected one). Uses `dplyr::any_of()` (as `app_selected_metadata()` already does) instead of the current literal `dplyr::select(...)`, which errors on AI mode's narrower `match_val`/`material_class` shape. `output$event`'s own `req(!grepl("^model$", ...))` guard and its `organization`/`material_class` factor `mutate()` (which would error on AI mode's missing `organization` column) are also fixed.
- R2. A new observer fires on `input$run_analysis` at a priority above `RUN_GATE_PRIORITY_RESET` (20) and sends one `analysis_phase()` message before any other Run-triggered observer runs, so the client's busy overlay can start counting from click time rather than from whenever the first currently-instrumented call happens to land. A `testServer()` regression test reproduces spec 014's original priority-race failure mode (a consumer observer reading `canonical_state()`/`canonical_final()` before `canonical_state_gate` has populated it on the same click) to prove the new observer cannot reintroduce it.
- R3. `recalculate_snr_preview()`'s first statement becomes an `analysis_phase()` call, before `signal_to_noise_basis()`/`spatial_data()`/`sig_noise()` -- today this path has zero progress signals in the default configuration.
- R4. `#run_analysis` and `#recalculate_snr` get an immediate client-side (plain JS, no server round trip) busy/disabled visual on click, via one shared helper function in `parent-frame.js` -- re-enabled on the existing completion signal or `shiny:idle`. A second click while active is inert. 650ms client debounce is confirmed acceptable and left unchanged.
- R5. The Top Matches and Thresholded Particles download links use that same shared helper on click, in BOTH wasm and local (non-wasm) modes, so click feedback is consistent across hosting modes: wasm reuses `downloadInCurrentFrame()`'s existing fetch+blob completion signal to re-enable; non-wasm keeps its native Shiny `<a>` download binding untouched (per the Bundled Shiny Application Boundary's native-download-binding requirement) and re-enables on a fixed fallback timeout, since a same-tab anchor download has no reliable JS completion event.

## Technical Decisions

- **Approach**: server-side changes are additive (new/reordered `analysis_phase()` calls only); client-side changes extend `parent-frame.js`'s existing click-handler pattern to non-wasm Run/Recalculate/download buttons instead of introducing a new mechanism.
- **Public API**: none; app-only. **Dependencies**: none new (`shinyjs` already used). **OpenSpecy contract**: unaffected.
- **Bundled Shiny app**: canonical reactive chain (`canonical_state_gate` -> `canonical_final`/`DataR`) is unchanged; R1 only changes what an existing Run-gated output renders, not its source object. R2/R4 touch observer registration/priority, the exact area spec 014 already had one real race regression in (an earlier priority conflict let `quantified_data_gate` read `canonical_final()` before `canonical_state_gate` had run) -- the new observer MUST be additive and message-only (no state mutation, no read of anything `canonical_state_gate` writes) so it cannot reintroduce that class of bug.
- **Pipeline diagram**: `.specify/memory/pipeline-diagram.html` "Top Matches Table" box/depends-line to update once R1 lands. Already corrected ahead of this plan (Correlation Histogram's existing AI-mode fallback tap, found during the audit that produced R1) -- no other diagram box changes expected from R2-R5, which are timing/UX only and don't change the pipeline shown.
- **Generated artifacts / External resources**: N/A.

## Package Surfaces

- `inst/`: `server.R` (`top_matches()`/`output$event`, new announce observer, `recalculate_snr_preview()`), `www/parent-frame.js` (click handlers), `ui.R` only if a new element id is needed for R5.
- `tests/testthat/`: R1 gets a `shiny::testServer()` assertion with a model-library fixture proving `top_matches()` returns a populated table instead of empty. R2-R5 are interaction-timing changes without a stable headless assertion; covered by manual/CI-guarded browser timing checks.
- `R/`, `DESCRIPTION`: unchanged. `NEWS.md`: entry for the AI-mode Top Matches fix (user-visible); feedback-latency work folded into the same entry or a second minor line.
- Bundled Shiny app: identified/AI-mode state exercised for R1; no-upload unchanged; processed/identified/batch states exercised for R2-R5 (click feedback only -- no scientific-output change, so existing download content is confirmed unchanged, not re-verified from scratch).

## Work Checklist

- [x] `global.R`/`server.R`: AI-mode branch for `top_matches()`, extracted to a testable `app_top_matches_table()` helper (matches the codebase's `sys.source()`-tested-helper convention; `shiny::testServer()` isn't used anywhere in this app's test suite). `output$event`'s own guard/factor-mutate fixed to match.
- [x] `server.R`: `RUN_GATE_PRIORITY_ANNOUNCE` (25, above `RESET`'s 20) + message-only announce-first observer on `input$run_analysis`.
- [x] `server.R`: `recalculate_snr_preview()`'s first statement is now `analysis_phase(...)`.
- [x] `www/parent-frame.js`: shared `markBusy()`/`clearBusy()` helpers; wired to `#run_analysis`, `#recalculate_snr` (cleared via `hideBusy()`, the real completion signal), and `#download_data` in non-wasm mode (fixed 4s fallback, no reliable completion event) -- `#download_data` is the single button that serves every download type, so no second download link was needed. Wasm mode's existing `downloadInCurrentFrame()` now reuses the same helpers.
- [x] `.specify/memory/pipeline-diagram.html`: Top Matches Table box, AI Model Classification tooltip, and footer updated for R1.
- [x] `NEWS.md` entry.
- [x] Regression test for the observer-priority failure mode (`tests/testthat/test-run_app_reactivity.R`): an isolated reactive graph using the real priority constants, proving `announce -> reset -> canonical -> default` ordering and that the default-priority consumer never sees a stale/missing canonical-priority value.

## Verification

- Direct regression: R1 via `testServer()` (AI-mode fixture, non-empty populated table, correct columns via `any_of()`). R2-R5 via a manual or CI-guarded browser check measuring time-to-first-visible-indicator on Run, Recalculate Preview, and each download, before vs. after.
- Focused tests: `devtools::test()` limited to touched app/R files. Full `devtools::test()`/`devtools::check()`: not triggered by this app-only, non-exported-surface tranche.
- Shiny affected states: identified (AI mode, R1); processed/identified/batch (R2-R5, click-feedback only). No-upload and download-content are unchanged and reused, not re-verified from scratch.
- Reusable evidence: N/A, new tranche; the touched files (`server.R` observers, `parent-frame.js`) changed again since spec 014.
- **Run**: focused gate (`quality-gates.ps1 -Filter run_app -BundledAppStatic`) -- 630/630 pass, sources parse. Targeted browser journey ("local app renders spectra, matches, and one informative progress overlay") -- 1/1 pass, confirming Run + the busy overlay still work end to end with the reordered/added observers. Full local Playwright suite and `devtools::check()` not run: not triggered by this app-only, non-release-facing tranche.

## Risks And Open Questions

- Observer-priority changes are the highest-risk part of this tranche given spec 014's real regression in this exact area; resolved by a dedicated `testServer()` regression test reproducing that exact failure mode (see R2), not just "the button still works."
- 650ms client debounce: confirmed acceptable by the maintainer; not changed in this tranche.
- Download consistency: resolved by sharing one client-side busy-decoration helper across wasm and non-wasm, per maintainer direction to keep the two modes consistent where possible; the underlying download transport stays per-mode (wasm fetch+blob, non-wasm native `<a>` binding, per the constitution's native-binding requirement) since unifying transport is out of scope and riskier than unifying the visual feedback layer.

## Approval Notes

- Approved by: (pending)
- Follow-up: none yet.
