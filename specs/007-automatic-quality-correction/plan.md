# Feature Plan: Automatic Spectral Quality Correction

**Feature dir**: `specs/007-automatic-quality-correction`  
**Date**: 2026-07-21  
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Make artifact correction an optional capability of `flatten_range()` and `restrict_range()` instead of a separate workflow function.
- Default the app to automated preprocessing and identification while requiring an uploaded spectrum for identification and prioritizing context-relevant downloads.

## Scope

- **In**: Ratio-based CO2/high-tail assessment, function-owned conditional correction, shared-axis guarded tail restriction, app defaults, upload-gated identification, and contextual download ordering.
- **Out**: `correct_spec()`, global post-processing correction/rollback, per-spectrum batch cropping, reference-library changes, new dependencies, and hand edits to generated docs or Shinylive output.
- **Users**: R users composing range functions and app users wanting safe automated defaults without irrelevant library/download states.

## Requirements

- R1. `assess_spec()` uses transient per-spectrum min-max normalization and flags CO2/high-tail artifacts when `candidate_max / control_max >= 3`; the control excludes both regions so simultaneous artifacts do not mask each other and unstructured noise is not flagged.
- R2. `flatten_range(automate = TRUE)` first runs the CO2 check with its target region and flattens only when at least one spectrum fails; a passing batch returns exactly unchanged. Manual behavior remains the default package API.
- R3. `restrict_range(automate = TRUE)` first runs the high-tail check and crops shared batch bounds one problematic edge point at a time until every spectrum passes; an equivalent optimized bound search is allowed.
- R4. Automated cropping is transactional: if combined removal from both ends exceeds 20% of the full original wavenumber span, return the original object unchanged with a diagnostic. Per-spectrum batch cropping is unsupported; users may `split_spec()` explicitly.
- R5. Both range functions preserve the `OpenSpecy` class, spectra-column/metadata-row alignment, identifiers, compatible attributes, and auditable automation diagnostics when an issue is found.
- R6. Remove the `correct_spec()` generic, methods, documentation, tests, vignette workflow, namespace entries, and prior app-wide correction helper/notification logic.
- R7. App preprocessing, identification, range selection, flattening, and each range function's automation option default on; range/flatten controls remain visible so users can disable automation or set manual bounds.
- R8. With no uploaded spectrum, identification must not load or display a reference library even though its switch defaults on; upload activates normal identification behavior.
- R9. Download choices are ordered by state: no upload starts with Test Data/Test Map; upload without identification starts with processed spectra; upload with identification starts with Top Matches. Remove Library Spectra and keep other applicable choices after the priority item.
- R10. Local Shiny and hosted Shinylive share source behavior; generated web artifacts, wasm pins, dependency closure, and staged small libraries remain unchanged.
- R11. The spectrum panel renders an empty plot before upload, then renders the processed upload independently of match readiness and overlays the selected reference once available.
- R12. Replace native bottom-right progress notifications with one accessible central status overlay that reports the current phase, elapsed time, and an honest workload-based ETA range until the app becomes idle.

## Technical Decisions

- **Approach**: Keep `.artifact_ratio_metrics()` internal and reuse it through `assess_spec()`. Add one consistent `automate = FALSE` policy argument to `flatten_range()` and `restrict_range()`; correction state is inferred, and existing parameters own thresholds/regions.
- **Public API**: No new export. `automate` is a demonstrated policy choice; ratio, tail length, CO2 region, and crop guard are advanced tuning arguments on the owning functions. Remove `correct_spec()` completely.
- **Primary pipelines**: `x |> flatten_range(automate = TRUE)` and `x |> restrict_range(automate = TRUE)`; the app passes these modes through `process_spec()`.
- **Dependencies**: None; reuse `matrixStats`, `data.table`, processing helpers, and existing Shiny packages.
- **OpenSpecy contract**: Flattening preserves the axis; tail restriction changes one shared axis only when all batch spectra can be retained within the 20% guard. Clean automated calls are exact no-ops.
- **Generated artifacts**: Update roxygen source and regenerate `NAMESPACE`/`man/*.Rd` with configured roxygen2; inspect removal of `correct_spec` and preservation of authorship/aliases.
- **External/reference workflows**: No network/library rebuild; compare representative matching inputs when automated cropping changes axes.
- **Bundled Shiny app**: Update `inst/shiny/ui.R`/`server.R`, remove obsolete global helpers, add sourceable download-order helper/tests, verify installed app/assets, and report unchanged asset/package size.
- **Hosted Shinylive/WebAssembly app**: Shared app source only; run static hosted tests and matching-artifact preflight/browser smoke when available, without changing pins, dependency closure, libraries, or generated output.

## Package Surfaces

- `R/`: Keep detector updates in `R/assess_spec.R`; revise `R/adj_range.R`; remove `R/correct_spec.R`; `R/process_spec.R` uses existing named-args pass-through unchanged.
- `tests/testthat/`: Extend `test-assess_spec.R`/`test-adj_range.R`; remove `test-correct_spec.R`; update `test-process_spec.R` and `test-run_app.R` for defaults, upload gating, and download order.
- `benchmarks/`: Retain/update repeated literal-loop versus optimized automatic restriction benchmark with equivalent output and regression guard.
- `workflows/` and `.github/workflows/`: Unchanged unless verification exposes a source contract issue. `DESCRIPTION`: unchanged.
- `inst/`: App source only; no assets. `vignettes/README/pkgdown`: document function-owned automation and remove `correct_spec()` guidance; no generated HTML edits.
- `NEWS.md`: Record detector semantics, function automation, removal of the uncommitted correction API, and app identification/download defaults.

## Work Checklist

- [x] Replace `correct_spec()` with exact-no-op automation in `R/adj_range.R`; update detector/range/process tests and benchmark.
- [x] Remove prior global app automation; default both processing functions and their automation controls on in `inst/shiny/ui.R`/`server.R`.
- [x] Gate library/identification behavior on upload and implement/test contextual download ordering without Library Spectra.
- [x] Update roxygen, vignette, NEWS, generated docs, focused/full tests, package check, size audit, and guarded hosted verification.
- [x] Namespace dashboard boxes and regression-test startup against package search-path collisions.
- [x] Restore empty/uploaded/match plot rendering and decouple the primary trace from reference-match readiness.
- [x] Replace redundant Shiny progress popups with dynamic central phase, elapsed, and ETA feedback.
- [x] Add headless helper coverage and a real local browser smoke covering initial plot, upload, match overlay/table, progress lifecycle, console errors, and screenshots.

## Verification

- Focused tests: `devtools::test(filter = "assess_spec|adj_range|process_spec|run_app")` including clean no-op, ratio boundary, CO2-only flattening, minimal shared crop, 20% rollback, app defaults, upload gating, and all download states.
- Benchmark: optimized versus literal one-point restriction, repeated with identical bounds/output and >10% regression failure.
- Toolchain/docs/full gates: Windows Rscript; roxygen2 version preflight; `devtools::document()` and generated diff review; `devtools::test()`; `devtools::check(document = FALSE)`.
- App/hosted: Source parsing/helper tests, installed paths/assets, size audit, local guarded smoke; static wasm tests and matching-artifact action preflight/nested browser smoke when available.
- Result: focused app/hosted-source 131 passed; full 1038 passed/3 expected skips; local Playwright smoke passed in 59s with real full-library matching, two traces, progress lifecycle, off/on state, screenshots, and no severe console/server errors. No app assets changed. R CMD check was intentionally not rerun per maintainer direction; matching hosted artifact remains unavailable.

## Risks And Open Questions

- An enabled identification switch without upload is inert by design; all library-loading reactives must require uploaded data to avoid hidden startup downloads.
- Download priority is implemented as both ordering and the initial selected value whenever state changes; preserve a still-valid explicit user selection only if that does not defeat the requested top default.

## Approval Notes

- Approved by:
- Follow-up:
