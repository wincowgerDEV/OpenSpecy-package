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

- R1. `output$event`/`top_matches()` gets an AI-mode branch mirroring the Top Matches download's `bind_cols(quantified_data()$metadata, matches_to_single())` shape, using `dplyr::any_of()` (as `app_selected_metadata()` already does) instead of the current literal `dplyr::select("match_val", "material_class", "spectrum_identity", "organization", "sample_name")`, which errors on AI mode's 3-column shape. Table shows one row per spectrum with its AI prediction instead of staying empty.
- R2. A new observer fires on `input$run_analysis` at a priority above `RUN_GATE_PRIORITY_RESET` (20) and sends one `analysis_phase()` message before any other Run-triggered observer runs, so the client's busy overlay can start counting from click time rather than from whenever the first currently-instrumented call happens to land.
- R3. `recalculate_snr_preview()`'s first statement becomes an `analysis_phase()` call, before `signal_to_noise_basis()`/`spatial_data()`/`sig_noise()` -- today this path has zero progress signals in the default configuration.
- R4. `#run_analysis` and `#recalculate_snr` get an immediate client-side (plain JS, no server round trip) busy/disabled visual on click -- mirroring the existing `downloadInCurrentFrame()` pattern in `parent-frame.js` that already does this for wasm downloads -- re-enabled on the existing completion signal or `shiny:idle`. A second click while active is inert.
- R5. The Top Matches and Thresholded Particles download links show a visible "generating..." state from click, since plain `<a href>` downloads never fire the `shiny:busy`/`shiny:idle` events the current overlay depends on.

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

- [ ] `server.R`: AI-mode branch for `top_matches()`; verify with `testServer()`.
- [ ] `server.R`: announce-first observer on `input$run_analysis` (priority above `RUN_GATE_PRIORITY_RESET`), message-only.
- [ ] `server.R`: move `recalculate_snr_preview()`'s first `analysis_phase()` call to its top.
- [ ] `www/parent-frame.js`: click handlers for `#run_analysis`, `#recalculate_snr`, and the two download links; disable + visual state on click, re-enable on completion/idle.
- [ ] `.specify/memory/pipeline-diagram.html`: update Top Matches Table box once R1 lands.
- [ ] `NEWS.md` entry.

## Verification

- Direct regression: R1 via `testServer()` (AI-mode fixture, non-empty populated table, correct columns via `any_of()`). R2-R5 via a manual or CI-guarded browser check measuring time-to-first-visible-indicator on Run, Recalculate Preview, and each download, before vs. after.
- Focused tests: `devtools::test()` limited to touched app/R files. Full `devtools::test()`/`devtools::check()`: not triggered by this app-only, non-exported-surface tranche.
- Shiny affected states: identified (AI mode, R1); processed/identified/batch (R2-R5, click-feedback only). No-upload and download-content are unchanged and reused, not re-verified from scratch.
- Reusable evidence: N/A, new tranche; the touched files (`server.R` observers, `parent-frame.js`) changed again since spec 014.

## Risks And Open Questions

- Observer-priority changes are the highest-risk part of this tranche given spec 014's real regression in this exact area; R2 needs a test/manual check specifically reproducing that failure mode (a stale/half-updated read), not just "the button still works."
- The client's fixed 650ms busy-overlay debounce (`parent-frame.js`) may still be worth shortening once server signals arrive promptly at click time -- left as an implementer judgment call after R2/R3 land, not pre-decided.
- R5 has no natural "download finished" signal for a plain browser download in non-wasm mode; whether to reveal for a fixed duration or switch non-wasm downloads to the wasm-style fetch+blob pattern (which does have a real completion event) is an open implementation choice, not resolved here.

## Approval Notes

- Approved by: (pending)
- Follow-up: none yet.
