# Feature Plan: Identification Block Progress

**Feature dir**: `specs/029-identification-block-progress`
**Date**: 2026-09-04
**Status**: Implemented; fresh hosted artifact verification pending
**Current tranche**: Report completed-block percentages and repair the hosted busy-overlay deployment regression.
**Change class**: hosted/release, bundled-app behavior, and a small internal package callback

## Goal And Requirements

- While full or medoid library identification runs in bounded query blocks, show the completed block count, total blocks, and percentage in the existing central progress overlay.
- Begin at 0%, update after every successfully reduced block, and reach 100% for the identification stage without estimating remaining time.
- Preserve the existing 1,000-spectrum block size, optimized correlation/ranking path, Top-N output order and values, memory bound, elapsed timer, and downstream canonical result. Inputs below that boundary report one total block.
- Keep progress monotonic in the central analysis bar by mapping block completion into the existing identification portion of the overall workflow.
- Do not let output-only reactive flushes reopen the hosted busy overlay after a completed Run has returned visible results.

## Technical Decisions

- Add an optional internal callback to `.match_spec_blockwise()` and invoke it only after each block's Top-N rows are stored. This avoids duplicating scientific matching code in the app and does not expand the public API.
- Put count/percentage formatting and overall-progress mapping in a pure `global.R` helper; `server.R::identify_blockwise()` forwards updates through the existing `analysis_phase()` custom message.
- Update the Library Identification node in `.specify/memory/pipeline-diagram.html`; no spectral, metadata, download, model, or quantification state changes.
- The browser bridge accepts phase updates only while a user-owned busy action is active. Once that action reaches idle and is cleared, late output rendering cannot create a new overlay lifecycle.
- Hosted impact: shared app and busy-overlay behavior change. Run fast `-HostedAppStatic`, the action-equivalent exact-artifact preflight, and the nested-frame smoke; no route, dependency, package pin, or artifact format changes.

## Work Checklist

- [x] Add/test the internal block-completion callback with identical matching results.
- [x] Add/test app block progress formatting and wire it to `identify_blockwise()`.
- [x] Update the pipeline diagram and NEWS entry.
- [x] Prevent late result-rendering flushes from reopening the hosted busy overlay and cover the lifecycle contract.
- [x] Run focused/package tests, the progress and complete local browser journeys, and hosted static checks.
- [ ] Run the exact-artifact nested-frame smoke after a fresh matching wasm artifact exists; local Docker is unavailable, so the next action run owns this final verification.
- [x] Reconcile evidence, processes, status, assets, and task scratch before handoff.

## Verification And Risks

- Unit: callback reports `(1, total)` through `(total, total)`, malformed callbacks fail early, and callback/no-callback results are identical.
- App: 2,501 queries at block size 1,000 format 0/3 through 3/3 and map overall progress monotonically from 76 to 88.
- Browser: upload a genuine one-spectrum Raman file, use the Raman medoid reference, and observe 0/1 followed by 1/1 (100%); package integration separately verifies all callbacks for a multi-block input.
- Hosted browser: after a genuine identification result appears and the Run reaches idle, no subsequent output-only busy transition is allowed; reproduce the workflow check that blocked run 33888049974.
- Full tests are triggered because `R/match_spec.R` changes; documentation and R CMD check are not triggered because the package helper is internal and has no generated documentation surface.

## Approval Notes

- Approved by maintainer request, 2026-09-04.
