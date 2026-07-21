# Feature Plan: Automatic Spectral Quality Correction

**Feature dir**: `specs/007-automatic-quality-correction`  
**Date**: 2026-07-21  
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Make the Open Specy app open in automation mode, with preprocessing and identification enabled while their advanced controls are hidden by default.
- Prevent noise-only spectra from false CO2/high-tail flags and automatically correct genuine issues in final processed spectra without violating spectral or batch alignment.

## Scope

- **In**: Ratio-based CO2/high-tail assessment, batch-wide automatic tail restriction, an exported assessment-driven correction function, and shared local/hosted app automation behavior.
- **Out**: Per-spectrum cropping within a batch (users may explicitly `split_spec()` first), fixes for other `assess_spec()` checks, new quality models/dependencies, reference-library changes, and hand edits to generated docs or Shinylive output.
- **Users**: R users correcting `OpenSpecy` batches and app users wanting a default hands-off process with optional advanced controls.

## Requirements

- R1. `assess_spec()` evaluates a transient per-spectrum min-max normalization and flags CO2 or a high tail when `candidate_max / control_max >= 3`; the control excludes both CO2 and tail regions so simultaneous artifacts do not mask each other, and noise-like spectra with similarly sized maxima across regions are not flagged.
- R2. Comparative checks handle zero/negative control maxima, missing/non-finite values, absent/overlapping regions, ascending/descending axes, short spectra, batches, and ties deterministically; diagnostics report normalized candidate/control maxima, their ratio, and threshold.
- R3. Existing `silent_region` and non-structural checks retain their current meaning; compatibility of `high_prob` is documented or deprecated deliberately rather than silently ignored.
- R4. `restrict_range()` gains an automatic-tail mode equivalent to removing one point at a time from each problematic outer edge until every spectrum has `tail_max / control_max < 3`; implementation may precompute candidate depths/maxima and select the first passing shared bounds instead of rebuilding the object iteratively.
- R5. Cropping is transactional and batch-wide: if the smallest passing shared bounds would remove more than 20% of the full original wavenumber span, counting both ends together, leave the original axis/tails unchanged and return a diagnostic; otherwise crop all spectra to the same passing axis. Never expose per-spectrum cropping on a batched `OpenSpecy`.
- R6. Exported `correct_spec()` (final name subject to API review) accepts an `OpenSpecy`, assesses CO2/high-tail issues by default, flattens CO2 and crops tails batch-wide, and returns one valid `OpenSpecy` with preserved spectrum/metadata order, identifiers, valid attributes, and auditable correction diagnostics/history.
- R7. Corrections are idempotent within numerical tolerance: reassessment of corrected output has no correctable CO2/high-tail findings, and a second correction pass does not further change a passing object.
- R8. The app defaults automation mode on, preprocessing on, and identification on; automation mode collapses/hides both control panels without disabling their active values, and users can reveal advanced controls or turn either stage off.
- R9. The app preprocesses the batch once, records how many final processed spectra pass both CO2 and tail checks, then makes exactly one correction attempt directly on that processed batch and reassesses without rerunning preprocessing. Keep the corrected output only when strictly more spectra pass both checks; otherwise revert to the original processed output. Never recursively correct.
- R10. Local Shiny and hosted Shinylive use the same source behavior; no generated web artifact is edited directly and no package/dependency/library pin changes are needed.

## Technical Decisions

- **Approach**: Min-max normalize temporary assessment values, compare each artifact maximum with the maximum outside both artifact regions at a default ratio of 3, and centralize correction of final processed objects in package code. For speed, precompute or scan crop-depth maxima and select the minimal bounds satisfying every column, proving equivalence to a one-point loop in tests.
- **Public API**: `correct_spec(x, checks = c("co2_region", "high_tail"), ratio = 3, ...)` exposes the reusable correction policy and meaningful threshold; batch behavior and correction order are inferred. Keep normalization, crop-depth search, guards, diagnostics assembly, and app adapters internal; automatic-tail tuning belongs to `restrict_range()` or a named args list rather than duplicate flags.
- **Primary pipeline**: `x |> process_spec(...) |> correct_spec() |> assess_spec() |> match_spec(library)`; callers wanting independent axes use `split_spec()` before correction.
- **Dependencies**: None; reuse `matrixStats`, `data.table`, existing processing helpers, and Shiny dependencies.
- **OpenSpecy contract**: Preserve the shared `wavenumber`, spectra-column/metadata-row alignment, dimnames, class, and compatible attributes; update correction history. Tail restriction changes the shared axis only through `restrict_range()` and must retain enough points/range for identification.
- **Generated artifacts**: Update roxygen source and regenerate `NAMESPACE`/`man/*.Rd` only with `Config/roxygen2/version`; inspect exports, authorship, aliases, and references immediately.
- **External/reference workflows**: No network/library rebuild. Probe Raman, FTIR, pure-noise, simultaneous CO2+tail, and mixed-quality batches; compare representative matching inputs/results where axes change.
- **Bundled Shiny app**: Update `inst/shiny/ui.R` and `inst/shiny/server.R`; no asset additions. Add helper/server tests, installed-path/asset checks, package-size confirmation, and a guarded app smoke test.
- **Hosted Shinylive/WebAssembly app**: Shared app source only; pins, dependency closure, wasm repository workflow, and small-library staging remain unchanged. Validate workflow/preflight and nested-frame startup, upload, correction, identification, download, layout, console, and busy-overlay behavior when a matching artifact exists.

## Package Surfaces

- `R/`: Update `R/assess_spec.R` and `R/adj_range.R`; add `R/correct_spec.R` unless colocation keeps the public contract clearer.
- `tests/testthat/`: Extend `test-assess_spec.R`, `test-adj_range.R`, add `test-correct_spec.R`, and update app/server and wasm source-contract tests.
- `benchmarks/`: Add repeated one-point-loop versus optimized shared-bound search covering equivalent bounds/output and flagging a material regression; document intentional detector-result changes separately.
- `workflows/`: Unchanged. `.github/workflows/`: unchanged unless hosted verification exposes a source-contract update.
- `inst/`: Modify bundled app UI/server only; no new assets. `DESCRIPTION`: unchanged.
- `vignettes/README/pkgdown`: Add an `OpenSpecy` correction pipeline to relevant help/vignette; keep GitHub README embed-free and rebuild pkgdown rather than editing HTML.
- `NEWS.md`: Record detector semantics, automatic tail correction/API, 20% guard, batch-only cropping, final-processed app correction, and default automation mode.

## Work Checklist

- [ ] Implement the normalized ratio-3 detector in `R/assess_spec.R`, including pure-noise, CO2+tail, zero-control, and mixed-batch fixtures.
- [ ] Implement transactional batch tail cropping and the 20%-span guard in `R/adj_range.R`, plus correction orchestration/history and exact-equivalence benchmark.
- [ ] Wire one post-processing correction attempt, reassessment, pass-count comparison, rollback, and identification into `inst/shiny/server.R`; add the default-on automation control and hidden/collapsed panels in `inst/shiny/ui.R`.
- [ ] Add focused package/headless app tests, roxygen, vignette, NEWS, generated-doc review, and hosted smoke coverage.

## Verification

- Focused tests: `devtools::test(filter = "assess_spec|adj_range|correct_spec|run_app")` including ratio boundary (`2.999`/`3`), pure noise, simultaneous CO2+tail, both edges, mixed batches, exact minimal crop, combined full-span 20% pass/fail, no partial crop on guard failure, idempotence, one retry, strict improvement, rollback on equal/worse results, and attributes.
- Toolchain/docs: Resolve Windows Rscript, confirm configured roxygen2, run `devtools::document()`, and inspect generated diffs immediately.
- Full gates: Run the repeated optimized-versus-loop benchmark first, then `devtools::test()` and `devtools::check()`/CI R CMD check; compare representative pre/post correction matching inputs.
- App: `shiny::testServer()` where feasible, installed app/path/static-asset tests, asset/package-size audit, and manual or CI-guarded local upload-to-identification smoke.
- Hosted: Validate unchanged wasm repo/pins/dependency closure/small libraries; with a matching artifact run `tools/wasm/test-shinylive-action.ps1` plus nested-frame correction/identification/download and console/layout/busy-overlay smoke.

## Risks And Open Questions

- Define zero-control behavior conservatively: a positive candidate with zero control has infinite contrast, but an all-zero/flat spectrum must remain noise/flat rather than an artifact finding.
- App comparison counts a spectrum as passing only when it has neither CO2 nor high-tail findings; this prevents improvement in one check from hiding a new failure in the other, but may reject a partially improved batch.

## Approval Notes

- Approved by:
- Follow-up:
