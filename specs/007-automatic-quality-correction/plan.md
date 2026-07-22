# Feature Plan: Automatic Spectral Quality Correction

**Feature dir**: `specs/007-automatic-quality-correction`
**Updated**: 2026-07-22
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Keep artifact correction inside `flatten_range()` and `restrict_range()`, while making the app apply those operations only after all other preprocessing.
- Make the bundled/hosted app dependable and focused: working contextual downloads, useful progress feedback, streamlined controls/content, and cohesive dark-theme plots.

## Scope

- **In**: Existing ratio-based CO2/high-tail automation, app-side staged correction acceptance, all three download paths, progress/UI/navigation changes, and pkgdown migration of informational content.
- **Out**: `correct_spec()`, per-spectrum batch cropping, correction of raw data followed by recursive preprocessing, exact completion-time prediction, Google Translate, new dependencies, and hand edits to generated pkgdown/Shinylive output.
- **Users**: R users composing range functions and app users wanting safe automated defaults with clear, reliable interaction feedback.

## Requirements

- R1. `assess_spec()` uses transient per-spectrum min-max normalization and flags CO2/high-tail artifacts when `candidate_max / control_max >= 3`; the control excludes both regions and unstructured noise is not flagged.
- R2. `flatten_range(automate = TRUE)` checks CO2 first and flattens only when at least one spectrum fails; a passing batch is an exact no-op. Manual behavior remains the default package API.
- R3. `restrict_range(automate = TRUE)` checks high tails and finds the equivalent shared-axis result of cropping one problematic edge point at a time until all spectra pass.
- R4. Automated cropping is transactional: combined removal beyond 20% of the full original wavenumber span returns the original object with a diagnostic. Per-spectrum batch cropping remains unsupported.
- R5. Both functions preserve `OpenSpecy` class, spectra/metadata alignment, identifiers, compatible attributes, and auditable automation diagnostics.
- R6. `correct_spec()` and the former global correction workflow remain removed.
- R7. App preprocessing, identification, range restriction, flattening, and both automation options default on; identification remains inert until upload.
- R8. The app calls `process_spec()` without range restriction or flattening, then applies enabled range/flatten operations to that processed `OpenSpecy` object.
- R9. An automated correction is skipped when its `assess_spec()` check has no failures and is accepted only when the relevant batch pass count strictly increases; otherwise the pre-correction result is retained. Sequential accepted corrections are evaluated from the latest accepted object.
- R10. Download choices remain context ordered and all produce nonempty files: no upload starts with Test Data/Test Map, upload without identification starts with Processed Spectra, and upload with identification starts with Top Matches.
- R11. One accessible central overlay reports the current phase and elapsed time with a monotonic workload-based progress bar; remove exact ETA text and native bottom-right progress notifications.
- R12. Remove Google Translate and the app sidebar. Move About, Partner With Us, and Contract Us information into `pkgdown/index.md`, never generated HTML.
- R13. Present Preprocessing, Identification, and Advanced as adjacent analysis tabs; move signal/noise threshold, correlation threshold, spatial smoothing, XY-grid conformity, and particle-spectrum collapse into Advanced.
- R14. Top Match download options are collapsed by default but expandable; the native Shiny download binding must remain intact.
- R15. Empty, processed, match-overlay, heatmap, and diagnostic plots use a bordered, cohesive dark palette with readable axes/gridlines and no pasted-on appearance.
- R16. Local Shiny and hosted Shinylive use the same source behavior; wasm pins, dependency closure, staged libraries, and generated deployment artifacts remain unchanged.

## Technical Decisions

- **Package API**: No new export or signature change in this follow-up. Existing `automate` policies and detector parameters remain owned by `flatten_range()`/`restrict_range()`.
- **App pipeline**: Use an app-local, sourceable assessment/acceptance helper. Run ordinary `process_spec()` steps first, then flattening followed by shared-axis restriction; gate automated candidates by the relevant strict pass-count improvement. Explicit manual bounds also run after ordinary preprocessing.
- **Downloads**: Use an unnested native `shiny::downloadButton()` plus state-aware choice helpers and server-side validation; browser tests must consume and inspect each download.
- **Progress**: Publish fixed stage percentages with phase messages, track real elapsed time in JavaScript, finish at 100%, and reset between runs. Percentages express workflow stage, not a completion-time forecast.
- **UI/content**: Use one analysis body without dashboard navigation, a three-panel tabset, collapsed top-match details, and app-local plot/CSS theming. Port existing informational prose/links to pkgdown source.
- **OpenSpecy contract**: Each candidate retains a shared wavenumber axis and aligned spectra/metadata; rejected candidates cannot leak altered axes, values, or attributes.
- **Generated/hosted boundaries**: Do not hand-edit `NAMESPACE`, `man/*.Rd`, `docs/`, or generated Shinylive output. No roxygen regeneration is expected unless source documentation changes.

## Package Surfaces

- `R/`, `benchmarks/`, generated help: Existing detector/range implementation retained unless focused correction tests expose a defect; no new API or benchmark expected for app-only orchestration.
- `inst/shiny/global.R`, `server.R`, `ui.R`, `www/`: Pipeline helper, download binding, progress protocol, tab/content cleanup, and visual styling; remove orphaned Translate asset if present.
- `tests/testthat/`: Headless helper/source assertions for ordering, strict acceptance/rollback, controls, progress, content migration, downloads, and installed assets.
- `tools/shiny-local-smoke.spec.js`: Real browser coverage for initial/processed/matched plots, Test Data/Processed/Top Match downloads, correction phases, progress, console errors, and desktop/mobile presentation.
- `pkgdown/index.md` and optional source CSS: Port information and links; `README.md` stays free of the interactive embed. `NEWS.md`: record user-visible fixes.
- `DESCRIPTION`, workflows, pins, reference/model libraries: Unchanged unless verification reveals a direct source-contract defect.

## Work Checklist

- [x] Establish detector/range automation, remove `correct_spec()`, and default app preprocessing/identification automation on.
- [x] Move range/flatten app orchestration after ordinary preprocessing and add strict assessment-improvement acceptance tests using representative map spectra.
- [x] Repair and test all contextual downloads, including browser-level file creation/content checks.
- [x] Replace ETA feedback with elapsed time plus staged progress and test lifecycle/accessibility.
- [x] Remove Translate/sidebar, migrate informational content to pkgdown, reorganize Advanced controls, and collapse Top Match options.
- [x] Apply and visually inspect cohesive dark plot styling across empty, spectral, heatmap, and diagnostic plots.
- [x] Update NEWS/tests, audit app assets/size, run focused then full tests, and run local/available hosted browser smoke without R CMD check.

## Verification

- Focused tests: `devtools::test(filter = "assess_spec|adj_range|process_spec|run_app|shinylive")`, including exact no-ops, staged app ordering, strict improvement/rejection, controls, progress, and download state/content.
- App probe: process representative Test Map data, record before/candidate/accepted pass counts for CO2 and tail checks, and verify all `OpenSpecy` invariants.
- Browser: run local Playwright smoke; require three genuine nonempty downloads, processed trace before match readiness, reference overlay/table, progress lifecycle, screenshots, and no severe console/server errors.
- Broader gates: inspect git/generated diffs, run `devtools::test()` once, static hosted-source tests, size/asset audit, and matching-artifact preflight only if available. Do not run R CMD check unless the maintainer explicitly requests it.
- Result: focused and full tests passed with three expected external/CI skips; local Playwright passed in 56.9 seconds with four downloads and inspected desktop/mobile screenshots. The Test Map improved CO2 passes from 207/208 to 208/208 and tail passes from 206/208 to 208/208. App assets fell from 782,619 to 168,736 bytes; no matching wasm artifact was available for action-equivalent preflight.

## Risks And Open Questions

- Strict per-check improvement prevents harmful automatic corrections but can leave a known artifact visible; surface that outcome in progress/diagnostic text without blocking identification.
- Shared-axis tail restriction can change which CO2 points remain; sequential assessment must always use the latest accepted object and preserve rejection rollback exactly.

## Approval Notes

- Approved by: maintainer follow-up on 2026-07-22
- Follow-up: R CMD check intentionally deferred until explicitly requested.
