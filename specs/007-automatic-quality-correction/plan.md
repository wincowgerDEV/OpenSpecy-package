# Feature Plan: Automatic Spectral Quality Correction

**Feature dir**: `specs/007-automatic-quality-correction`
**Updated**: 2026-07-22
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Keep artifact correction inside `flatten_range()` and `restrict_range()`, while making the app apply those operations only after all other preprocessing.
- Make the bundled/hosted app dependable and focused: working contextual downloads, useful progress feedback, streamlined controls/content, and cohesive dark-theme plots.
- Add reproducible user-defined area and peak ratios without applying them to derivative spectra.

## Scope

- **In**: Existing artifact automation, dependable app interaction/downloads, centralized theming, user-defined area/peak ratios, Fill Peaks baseline correction, and exported quantification metadata.
- **Out**: `correct_spec()`, per-spectrum batch cropping, recursive preprocessing, exact completion-time prediction, Google Translate, preloaded ratio catalogs, concentration calibration, and hand edits to generated output.
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
- R17. The tabbed settings card and a separate equal-width download card start collapsed; settings tabs stay visible and selecting any tab expands the settings card, while the native download action stays usable in the collapsed download header.
- R18. Keep download configuration to ordinary dropdowns with no popovers. The download action label names the currently selected artifact and follows context-driven default-selection changes without replacing the native Shiny download binding.
- R19. The Summary card occupies the same full row width as the spectra/identification card. Its active progress bars and plots are conditionally laid out with fluid rows and equal responsive columns, without empty placeholder columns or floating gaps.
- R20. One app theme token set governs the page, header/footer, cards, tab headers, dropdowns, tables, progress widgets, and plots using the dark navy surface and cyan border/accent treatment.
- R21. The download action is roughly twice its former width without wrapping; uploaded spectra render white, enabled switches render green with white knobs, and every informational disclosure has substantive content.
- R22. A header action labelled “Support Open Source Software” with a donate icon opens the former donation-link choices on demand and never blocks startup.
- R23. Build app area ratios compositionally from two explicit `area_under_band()` ranges; validate full uploaded-axis coverage and return named `NA` values with a diagnostic for invalid denominators. Existing package-only named presets remain compatible but are not surfaced in the app.
- R24. `subtr_baseline(type = "fill_peaks")` exposes canonical 4S Fill Peaks correction while retaining polynomial/manual defaults and preserving complete `OpenSpecy` alignment.
- R25. Export `peak_ratio()` for explicit numerator/denominator spectral positions using a documented deterministic shared-axis lookup and the same named-vector/invalid-value contract as compositional area ratios.
- R26. Quantification defaults off and provides a name/type builder with two point sliders for peak ratios or two range sliders for area ratios, bounded to the uploaded axis; users can add/remove unlimited definitions and receive one concrete scenario in the help text.
- R27. Calculate saved definitions only from an explicit non-derivative raw/min-max/Fill Peaks/modified-polynomial treatment, and append treatment, definitions, and values to processed-spectrum and simple/full top-match exports.
- R28. Remove inactive help/dark-mode header toggles, right-align donation, space the download icon, and make Summary and Spectra equal full-row widths on desktop/mobile.
- R29. Muted child settings never invalidate analysis or show the busy overlay; only enabled owners read their child inputs, draft ratios run only after Add plus Quantification enablement, and explicit analysis phases alone authorize the overlay.

## Technical Decisions

- **Package API**: Preserve `area_under_band()` legacy custom sums/presets and add only `peak_ratio()` with explicit positions, nearest/linear lookup policy, and named numeric-vector output. Compose area ratios from two band calls. Keep Fill Peaks backed by CRAN `baseline`.
- **App pipeline**: Use an app-local, sourceable assessment/acceptance helper. Run ordinary `process_spec()` steps first, then flattening followed by shared-axis restriction; gate automated candidates by the relevant strict pass-count improvement. Explicit manual bounds also run after ordinary preprocessing.
- **Downloads**: Keep an unnested native `shiny::downloadButton()` in the collapsed card header, update only its visible/accessibility label from a tested choice-to-label helper, and retain state-aware choices plus server-side file validation.
- **Progress**: Publish fixed stage percentages with phase messages, track real elapsed time in JavaScript, finish at 100%, and reset between runs. Percentages express workflow stage, not a completion-time forecast.
- **UI/content**: Use one analysis body without dashboard navigation, equal-width collapsed settings/download cards, a four-panel tabset, collapsed top-match details, validated disclosures, a right-aligned donation modal action, equal full-row Spectra/Summary cards, and app-wide theme tokens.
- **Quantification**: Store only definitions committed with Add; derive slider limits from the uploaded shared axis, calculate saved definitions against a dedicated non-derivative copy, and attach values plus exact definition/treatment provenance to final processed metadata.
- **Reactivity**: Branch on each owner toggle before reading child inputs. Gate the custom overlay on server analysis-phase messages rather than generic Shiny busy events so configuration-only renders remain quiet.
- **OpenSpecy contract**: Each candidate retains a shared wavenumber axis and aligned spectra/metadata; rejected candidates cannot leak altered axes, values, or attributes.
- **Generated/hosted boundaries**: Do not hand-edit `NAMESPACE`, `man/*.Rd`, `docs/`, or generated Shinylive output. Regenerate roxygen output once with the configured version after public API documentation changes.

## Package Surfaces

- `R/area_under_band.R`, new peak-ratio source, `R/subtr_baseline.R`, `DESCRIPTION`, generated help, benchmarks: Composable area ratios, explicit peak ratios, and canonical Fill Peaks behavior.
- `inst/shiny/global.R`, `server.R`, `ui.R`, `www/`: Pipeline helper, download binding, progress protocol, tab/content cleanup, and visual styling; remove orphaned Translate asset if present.
- `tests/testthat/`: Headless helper/source assertions for ordering, strict acceptance/rollback, controls, progress, content migration, downloads, and installed assets.
- `tools/shiny-local-smoke.spec.js`: Real browser coverage for initial/processed/matched plots, Test Data/Processed/Top Match downloads, correction phases, progress, console errors, and desktop/mobile presentation.
- `pkgdown/index.md` and optional source CSS: Port information and links; `README.md` stays free of the interactive embed. `NEWS.md`: record user-visible fixes.
- Workflows, pins, and reference/model libraries remain unchanged; `DESCRIPTION` adds the Fill Peaks dependency and the hosted resolver absorbs its transitive closure.

## Work Checklist

- [x] Establish detector/range automation, remove `correct_spec()`, and default app preprocessing/identification automation on.
- [x] Move range/flatten app orchestration after ordinary preprocessing and add strict assessment-improvement acceptance tests using representative map spectra.
- [x] Repair and test all contextual downloads, including browser-level file creation/content checks.
- [x] Replace ETA feedback with elapsed time plus staged progress and test lifecycle/accessibility.
- [x] Remove Translate/sidebar, migrate informational content to pkgdown, reorganize Advanced controls, and collapse Top Match options.
- [x] Apply and visually inspect cohesive dark plot styling across empty, spectral, heatmap, and diagnostic plots.
- [x] Update NEWS/tests, audit app assets/size, run focused then full tests, and run local/available hosted browser smoke without R CMD check.
- [x] Collapse settings/download cards by default, auto-expand settings tabs, keep the download action exposed, remove popovers, and make its label state-aware.
- [x] Reflow the Summary card and active widgets with responsive fluid rows/columns so every state uses the available width.
- [x] Apply centralized dark/cyan theme tokens to the full app chrome, inputs, cards, tables, and footer/header; update tests, NEWS, and browser screenshots.
- [x] Add and document canonical Fill Peaks baseline correction with focused tests and a batch benchmark.
- [x] Establish the Quantification tab/non-derivative treatment path and propagate aligned metadata through processed and top-match downloads.
- [x] Finish the requested button/plot/switch/disclosure/donation UI touches and verify them headlessly and in a real browser.
- [x] Regenerate and inspect documentation, run focused/full tests and local browser smoke, and report hosted dependency/preflight status without R CMD check.
- [x] Replace predefined app controls with explicit band composition plus documented `peak_ratio()` and a dynamic saved-definition builder that defaults off.
- [x] Gate every muted preprocessing/quantification child dependency and the busy overlay; add regression coverage proving configuration changes stay quiet until enabled.
- [x] Apply header/icon/full-width layout changes, regenerate only intended help, and rerun focused/full/browser/hosted-source verification without R CMD check.

## Verification

- Focused tests: `devtools::test(filter = "area_under_band|peak_ratio|subtr_baseline|process_spec|run_app|shinylive")`, including legacy sums/presets, explicit area composition/peak ratios, Fill Peaks invariants, muted dependencies, non-derivative quantification, metadata alignment, controls, and download content.
- App probe: process representative Test Map data, record before/candidate/accepted pass counts for CO2 and tail checks, and verify all `OpenSpecy` invariants.
- Browser: run local Playwright smoke; require four genuine nonempty downloads, collapsed/expanding card behavior, reactive download labels, gap-free summary states, theme coverage, processed/reference plots, progress lifecycle, desktop/mobile screenshots, and no severe console/server errors.
- Broader gates: inspect git/generated diffs, run `devtools::test()` once, static hosted-source tests, size/asset audit, and matching-artifact preflight only if available. Do not run R CMD check unless the maintainer explicitly requests it.
- Result: focused checks passed 345 assertions, and all 1,288 package tests passed with 31 expected warning assertions and three external/CI skips. Local Playwright passed in 112.8 seconds with muted-setting checks, two saved ratio types, metadata exports, donation/header controls, switch colors, white spectrum traces, four downloads, and inspected desktop/mobile screenshots. Roxygen 8.0.0 added only the intended peak help/exports beyond prior area/Fill Peaks help. Static hosted-source tests passed; no artifact can match uncommitted source for action-equivalent preflight. R CMD check was not run.

## Risks And Open Questions

- Strict per-check improvement prevents harmful automatic corrections but can leave a known artifact visible; surface that outcome in progress/diagnostic text without blocking identification.
- Shared-axis tail restriction can change which CO2 points remain; sequential assessment must always use the latest accepted object and preserve rejection rollback exactly.
- Published ratios are material/method specific and preprocessing materially changes their scale; metadata must retain treatment provenance and unsupported spectral coverage must remain explicit `NA`.
- Peak positions selected between sampled wavenumbers need one transparent lookup rule; tests and help must make the chosen behavior reproducible.

## Approval Notes

- Approved by: maintainer follow-up on 2026-07-22
- Follow-up: R CMD check intentionally deferred until explicitly requested.
