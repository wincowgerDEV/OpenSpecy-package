# Feature Plan: Hosted Deployment Reliability

**Feature dir**: `specs/016-hosted-deployment-reliability`
**Date**: 2026-08-21
**Review budget**: Under 100 nonblank lines and 1,500 words.
**Current tranche**: Fix the repeatedly failing Shinylive browser smoke and make hosted-impact planning plus a fast hosted-source gate routine for future changes.
**Change class**: hosted/release.

## Goal

- Restore a deployable `/app/` by making the map Top Matches smoke configure its required `Top N` value instead of relying on a moved control's old default.
- Catch hosted workflow/test drift during ordinary development without requiring a wasm rebuild and full nested-frame run for every minor change.

## Scope

- **In**: hosted smoke fixture setup; a fast hosted-source quality-gate tier; constitution, plan template, AGENTS, and relevant project-skill guidance.
- **Out**: changing app behavior or scientific outputs; dependency/package pins; rebuilding the wasm package repository; pushing or re-running Actions.
- **Users**: maintainers changing package/app/site source that ultimately feeds the hosted app.

## Requirements

- R1. The hosted map fixture explicitly sets `top_n_input` to 1 before asserting the 209-line Top Matches CSV; it must not depend on the UI default.
- R1a. The thresholded-particle smoke verifies the app's current ZIP download contract and its summary/detail CSV members.
- R2. A fast `-HostedAppStatic` gate validates hosted workflow contracts and JavaScript/R/PowerShell source syntax without needing Docker, a wasm artifact, downloads, or a browser runtime.
- R3. Every concise development plan explicitly classifies hosted impact and schedules the fast gate whenever shared hosted inputs change; full action-equivalent/browser verification remains proportional and artifact-driven.
- R4. Durable governance and hosted/implementation/quality-gate skills distinguish three tiers: fast hosted-source gate, matching-artifact preflight, and full clean-commit rebuild/rehearsal.

## Technical Decisions

- **Approach**: fix test preconditions at the test; add `-HostedAppStatic` to the maintained quality-gate script and keep full wasm verification in existing tools.
- **Public API / OpenSpecy contract / dependencies**: unchanged; no scientific or package runtime behavior changes.
- **Generated artifacts**: none committed; `_wasm/` remains ignored.
- **Bundled Shiny app / pipeline diagram**: source and canonical reactive pipeline unchanged; diagram N/A.
- **Hosted Shinylive/WebAssembly app**: `/`, `/app/`, `/pkgdown/`, pins, dependency closure, and staged libraries are unchanged. The browser smoke contract and planning/verification workflow change.

## Package Surfaces

- `R/`, `DESCRIPTION`, `NEWS.md`, `benchmarks/`, `workflows/`, `inst/`, `site/`: unchanged.
- `tests/testthat/`: hosted contract assertions updated for explicit fixture setup and the fast gate.
- `.github/workflows/`: unchanged unless diagnosis finds the action invocation itself deficient.
- `tools/wasm/`: hosted smoke fixture setup.
- `.agents/skills/`, `.specify/`, `AGENTS.md`: tiered hosted planning and verification guidance.
- Hosted output: generated only in ignored `_wasm/`; no hand edits.

## Work Checklist

- [x] `tools/wasm/shinylive-smoke.spec.js`: explicitly set map `Top N` to 1 and verify the thresholded ZIP contract.
- [x] Quality gate and `tests/testthat/test-shinylive_wasm.R`: add/cover `-HostedAppStatic` syntax and source-contract checks.
- [x] Constitution, plan template, AGENTS, and hosted/Spec Kit skills: require hosted-impact classification and tiered gates.
- [x] Run focused hosted contracts, fast hosted-source gate, and matching-artifact preflight against the exact reusable artifact.

## Verification

- Direct regression: focused `shinylive_wasm` contracts pass with 237 assertions, including explicit Top N setup and ZIP members.
- Fast gate: `quality-gates.ps1 -Filter shinylive_wasm -HostedAppStatic` passed (`FAIL 0`, `WARN 0`, `SKIP 0`, `PASS 237`).
- Toolchain/source checks: hosted JS syntax, wasm R parse, PowerShell parser, and workflow contract tests passed.
- Matching artifact: exact artifact `a34ce9e6e00898471d19a9ae0bb6790c002a861c` passed the 117-package closure check and staged-library probe (top medoid score 0.9686); its assembled 587-file site passed the nested-frame Playwright smoke (1/1, 3.3 minutes) with desktop/mobile/fullscreen screenshots reviewed.
- Full pre-push rebuild: not triggered because package/dependency closure and wasm build inputs are unchanged.
- Documentation/package/full tests/benchmarks: N/A for hosted harness/governance-only changes.
- Reusable evidence: green wasm run 32315243842 covers the unchanged package repository artifact at the exact SHA; invalidated only by package/dependency/build-input changes.

## Risks And Open Questions

- A source-only gate cannot prove WebAssembly runtime behavior; it is an early gate, not a replacement for matching-artifact verification when hosted runtime behavior changes.

## Approval Notes

- Approved by: user request, 2026-08-21.
- Follow-up: maintainer push is required to obtain the final Actions deployment result; remote synchronization was not authorized.
