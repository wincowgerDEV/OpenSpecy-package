# Feature Plan: Spectral Quality and Hosted App Reliability

**Feature dir**: `specs/008-hosted-app-polish`
**Date**: 2026-07-31
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Add auditable spike correction, saturation-aware shared range restriction, and complete quality reporting that drive the bundled/local and hosted app from one canonical final `OpenSpecy` state.
- Repair real Shinylive downloads and deliver an accessible, dark, funder-ready welcome experience with clearer plots and acknowledgements.

## Scope

- **In**: `correct_spike()`; paper-backed and local-residual spike detection; `assess_spec()` spike/saturation/full-status reporting; conservative saturation handling in `restrict_range()`; exact breakpoint SNR; default app corrections and quality modals; hosted download repair; homepage, partner, legend, and heatmap polish.
- **Out**: Per-spectrum matching masks, saturated-peak reconstruction/imputation, exposure merging, a general `correct_spec()` wrapper, deleting individual spectra, changing library intensities, generated-site hand edits, or full-library staging in WebAssembly.
- **Users**: Package analysts, local/hosted app users, educators, contributors, and prospective funders.

## Requirements

- R1. Export one pipeable `correct_spike()` generic/method that preserves `OpenSpecy` dimensions, shared axis, spectrum/metadata alignment, IDs, and compatible attributes; it changes only accepted spike intervals, is idempotent, and returns an exact no-op when no correction is accepted.
- R2. A shared internal spike detector supports the chat's wavenumber-aware local-interpolation residual/MAD method (positive, negative, or both; conservative one-point default) and Coca-Lopez's prominence/FWHM method (DOI `10.1016/j.aca.2024.342312`, pp. 2-4): manual prominence/width thresholds plus automated prominence/FWHM upper outliers at `z > 3.5` when enough peaks exist.
- R3. Paper mode measures width in sample/CCD-pixel units, flags a configurable prominence-width interval (paper reference `rel_height = 0.8`), and replaces only flagged samples from unflagged neighbors (paper reference `m = 10`, linear default); adjacent spikes, boundaries, spikes on true bands, and narrow calcite/polystyrene bands must not be silently damaged. Do not copy the tutorial's boundary-wrapping behavior.
- R4. Correction is transactional: correctable-spike count decreases; no new non-finite values, boundary spikes, axis/dimension changes, or edits outside flagged intervals occur; store standardized `automatic_spike` diagnostics including method, parameters, corrected/rejected regions, affected spectra, and reason.
- R5. `assess_spec()` adds opt-in `"spike"` and `"saturation"` checks using the same detectors. `report = "issues"` remains the silent, default, clean-empty compatibility path; `report = "all"` returns one explicit `pass`/`warning`/`error` result per requested check/scope with stable IDs, evidence, counts/regions, likely cause/fix, and correction history for the UI.
- R6. `restrict_range()` keeps positional/manual `min`, `max`, `make_rel` and current `automate = TRUE` high-tail semantics, then adds independently triggered saturation input: `NULL`, `"auto"` hard-plateau detection, or one finite numeric detector ceiling; unsupported/ambiguous inputs fail clearly and ordinary maxima/broad real peaks are not auto-classified.
- R7. Saturation detections are unioned, guard-expanded, merged, and removed identically from every unknown spectrum and the reference library. Loss uses wavenumber cell coverage on irregular axes; accept at or below `max_saturation_loss = 0.70` only if enough matching points remain, otherwise make an exact no-op and warn with loss, limit, interpretation risk, and recollection guidance.
- R8. Accepted/rejected saturation operations store detected/excluded/retained ranges, affected spectra, thresholds, loss, counts, reason, and a retained-axis signature; never replace saturated values with NA, means, zeroes, or inferred peaks.
- R9. Add `sig_noise(metric = "breakpoint_snr")` using exact sorted amplitudes (`sort.int()` plus linear breakpoint evaluation, no histogram bins); let `assess_spec()` select its SNR metric without changing current package-wide defaults before blank/known-spectrum/identification validation.
- R10. The app enables owner controls for isolated-spike correction and saturated-range removal by default, gates all child controls, and processes raw upload -> spike correction -> saturation detection/shared restriction -> ordinary preprocessing/current CO2/high-tail stages -> identical library-axis restriction -> identification. Rejected saturation stays visible and is exported in processing metadata.
- R11. Three always-visible, non-color-only controls above the active spectrum show unique error, warning, and pass counts from cached `assess_spec(displayed_object, report = "all")`; each opens a keyboard-accessible filtered modal with finding, evidence, interpretation, action, and automatic-correction outcome without rerunning identification.
- R12. Every hosted download choice produces its intended suggested filename, MIME/disposition, nonempty bytes, and format signature through a real user click; neither a blank HTML fallback nor a canceled native download may pass because an in-frame `fetch()` succeeds.
- R13. The dark pkgdown welcome page has a responsive hero, novice/advanced/funder entry paths, site/app navigation, scientific/value explanation, clear calls to action, the full [YouTube tutorial](https://www.youtube.com/watch?v=y2F4Fu6A4aA&list=PLqdH8O1nalYa4a8JXQ6GbNsH3YQV_aY7g), and accessible contrast/motion. Add Pew-Gerstner Fellowship in Ocean Plastics Research and Walking Softer to Thriving monetary partners.
- R14. Move the spectrum legend outside the data region at desktop/mobile widths and use a colorblind-accessible heatmap palette whose full range remains distinct from the dark canvas without changing plotted values or categories.

## Technical Decisions

- **Public API**: Keep one exported correction operation; one method argument is justified by the two requested, scientifically distinct detectors, while one-caller peak/plateau/range/diagnostic helpers remain internal. Append new formals to preserve positional calls; input presence triggers saturation rather than another boolean.
- **Paper evidence**: Retain a literal/reference implementation under `benchmarks/`; validate paper defaults rather than universalizing graphene tuning (`P0 = 40`, `FWHM0 = 4`). Fig. 6 is a required regression case because linear interpolation at `rel_height = 0.8` can truncate a real band.
- **Download diagnosis**: The deployed OpenSpecy 1.7.1 app reached ready state, but the native dynamic `session/.../download/download_data` click produced no artifact. Investigate the Shinylive 0.5.0 service-worker/`target=_blank` handoff and compare the last working export; if upstream attachment routing is unreliable, add a wasm-only in-frame fetch -> validated Blob/object-URL bridge while retaining normal local Shiny `downloadHandler` behavior.
- **Generated/dependency boundaries**: Prefer base/current imports and preserve wasm dependency closure/pins/small-library staging. Update roxygen and regenerate with configured roxygen 8.0.0; never hand-edit `NAMESPACE`, `man/`, `docs/`, `_wasm/`, or generated Shinylive output.

## Package Surfaces

- `R/`: `correct_spike.R` (new), `assess_spec.R`, `adj_range.R`, `sig_noise.R`, and named integration in `process_spec.R`; preserve object attributes and base-pipe composition.
- `tests/testthat/`: new `test-correct_spike.R`; expand `test-assess_spec.R`, `test-adj_range.R`, `test-sig_noise.R`, `test-run_app.R`, and `test-shinylive_wasm.R` with paper fixtures, compatibility/invariant cases, app state, and strict native-download contracts.
- `benchmarks/`: repeated reference-vs-current spike detection/correction, saturation detection/union, full assessment, and representative single/100-spectrum/map cases; flag output differences and >10% same-output regressions.
- `inst/shiny/`: canonical pipeline, owner/child controls, status controls/modals, diagnostics/export, spectrum legend, heatmap palette; no new raw/generated media.
- `tools/wasm/`, `.github/workflows/`: strengthen `shinylive-smoke.spec.js` and action-equivalent preflight; change deployment only if service-worker/export diagnosis requires it, without floating package/dependency pins.
- `pkgdown/index.md`, `pkgdown/extra.css`, `pkgdown/extra.js`, `_pkgdown.yml`: welcome/navigation/tutorial/funder design and partner credits; keep the interactive embed out of GitHub `README.md`.
- `vignettes/sop.Rmd`, roxygen/man, `NEWS.md`: scientific limitations, processing order, paper citation, warnings, UI/download changes; `DESCRIPTION` only if an unavoidable dependency is approved; reference-library workflow unchanged except subset matching compatibility evidence.

## Work Checklist

- [x] Implement and benchmark internal detectors plus `R/correct_spike.R`; add paper/local-residual fixtures and transactional invariant tests.
- [x] Extend `R/assess_spec.R`, `R/adj_range.R`, `R/sig_noise.R`, and `R/process_spec.R` with backward-compatible schemas/defaults, shared diagnostics, shared-axis restriction, and focused tests.
- [x] Implement the default ordered pipeline and quality controls in `inst/shiny/`; verify owner gating, active-spectrum counts/modals, matching-axis equality, legend, palette, and genuine local downloads.
- [x] Remove the unconditional canceled-download allowance, add the wasm-only validated Blob bridge, and require real click-to-disk evidence for all six choices.
- [x] Build the dark pkgdown welcome source, tutorial/navigation/audience paths, and acknowledgements; update SOP/roxygen/NEWS and regenerate only owned generated artifacts.
- [x] Run focused tests/benchmarks, documentation diff review, full tests/check, action-equivalent assembly, and desktop/mobile local/hosted visual/console review.
- [ ] After a maintainer commit produces a matching current-SHA wasm artifact, run the canonical upload/identification and six-download hosted smoke.

## Verification

- Resolve the real Windows Rscript first; run focused `testthat` files, paper regression fixtures, object/axis/attribute invariants, 70% inclusive boundary and rollback, muted controls, selected-spectrum status changes, and real local files before `devtools::test()`.
- Run repeated benchmarks before full tests; run `devtools::document()` with roxygen 8.0.0 and immediately inspect exports/authorship/aliases; build the SOP/pkgdown source and run `devtools::check()` because the new public APIs are release-facing.
- Apply `openspecy-develop-shiny-app`: exercise no-upload, raw/processed, identified, batch/map, quantification, correction accepted/rejected, all downloads, progress, console, screenshots, and asset inventory.
- Validate wasm repository/package/dependency pins and medoid/model libraries; run `tools/wasm/test-shinylive-action.ps1` with the exact action artifact and nested-frame startup/upload/identification/library-match/app-mode/chooser/all-download/desktop/mobile smoke. Click-to-disk is authoritative; attach status, MIME/disposition, first bytes, console, failed requests, and screenshots on failure.

## Risks And Open Questions

- Paper modes were validated primarily on positive Raman cosmic-ray spikes and require enough peaks for the 3.5-SD LoD; calibrate on representative Raman/FTIR, negative-spike, edge, NA, heterogeneous-noise, and narrow-band cases before choosing the app's detector default.
- This is a large release-facing sequence: implement package detection/reporting contracts before app automation, then hosted/browser and presentation work, so UI does not duplicate unstable scientific policy.

## Approval Notes

- Approved by: user, 2026-07-31 ("go for it").
- Completed evidence: package/API review, local native downloads, browser layout/accessibility, action-equivalent assembly, and three real hosted clicks passed; the stale `d2899d9` artifact then stopped at the new `correct_spike()` call as expected.
- Follow-up: publish a fresh wasm artifact from the eventual maintainer commit and run the remaining canonical processed/identified/thresholded click-to-disk checks before deployment sign-off.
