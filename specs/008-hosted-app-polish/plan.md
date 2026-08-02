# Feature Plan: Spectral Quality and Hosted App Reliability

**Feature dir**: `specs/008-hosted-app-polish`
**Date**: 2026-07-31
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Add auditable spike correction, saturation-aware shared range restriction, and complete quality reporting that drive the bundled/local and hosted app from one canonical final `OpenSpecy` state.
- Repair real Shinylive downloads and deliver an accessible, dark, funder-ready static landing site with clearer routes, tutorials, project evidence, and acknowledgements.
- Restore responsive single-spectrum/map interaction and add composable non-ratio point/area quantification without duplicating the canonical processed state.

## Scope

- **In**: `correct_spike()`; paper-backed and local-residual spike detection; `assess_spec()` reporting; conservative saturation handling; exact breakpoint SNR; point/area and ratio quantification; app corrections, quality/automatic modals, latency/reactivity repair, hosted downloads, static landing/SEO/routes, partner, legend, and heatmap polish.
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
- R11. Three always-visible controls above the active spectrum show automatic corrections, warnings, and successes. Automatic lists spike, saturation, CO2 flattening, and high-tail outcomes and counts only applied corrections; those checks are excluded from warning/success. Warning and success modals contain only their exact failed/passed results, including selected-spectrum SNR and correlation relative to the app thresholds; remove the separate Cor/SNR badge. Use a rainbow automatic icon/outline when any correction applied, yellow warning icon/outline, and green success icon/outline.
- R12. Every hosted download choice produces its intended suggested filename, MIME/disposition, nonempty bytes, and format signature through a real user click; neither a blank HTML fallback nor a canceled native download may pass because an in-frame `fetch()` succeeds.
- R13. Publish a dependency-free HTML/CSS/JS landing page at `/` with the `/app/` iframe as its primary action, responsive novice/analyst/funder guidance, the full [YouTube tutorial](https://www.youtube.com/watch?v=y2F4Fu6A4aA&list=PLqdH8O1nalYa4a8JXQ6GbNsH3YQV_aY7g), publications, contact/support paths, funder acknowledgements, accessible dark presentation, descriptive metadata, canonical/social tags, JSON-LD, `robots.txt`, and a sitemap. Put ordinary README-driven pkgdown documentation at `/pkgdown/` without marketing/embed duplication; keep the README iframe-free.
- R14. Move plot legends outside data regions; numeric heatmaps show a non-overlapping value legend, categorical heatmaps may omit it. Reuse the exact Match Name category-to-color mapping in the material-class summary plot and retain the bright colorblind-accessible palette.
- R15. A map-cell selection updates only selection-owned outputs, leaves a persistent orange marker at the selected location, and must not rerun preprocessing, identification, or redraw the heatmap twice. Add a browser regression for the marker and returned double-load/blink.
- R16. Benchmark the default single-spectrum analysis path and its expensive stages with repeated measurements. Cache or reuse correction/quality evidence, remove redundant checks/reactive calls, preserve identical scientific output, and flag a >10% same-output regression.
- R17. Export a pipe-friendly `point_intensity()` for one nearest or linearly interpolated wavenumber; keep single-region measurement in `area_under_band()`. The app adds an independent Measurements box after Custom Ratios, permits ratios and measurements together, uses numeric inputs rather than sliders for every quantification value, and exports definitions/values from the displayed processed spectra.
- R18. A push recommendation requires a clean-commit rehearsal that builds the current wasm repository with the same digest-pinned Docker driver as CI, validates both artifact manifests/SHA and image checksums, uses isolated host dependencies plus freshly staged libraries, and passes the complete nested-frame browser workflow including `/`, `/app/`, `/pkgdown/`, a fresh map generation, and six genuine downloads. Docker readiness fails early with actionable read-only diagnostics; a separately named non-Docker site-shell gate must not claim action equivalence.

## Technical Decisions

- **Public API**: Keep one exported correction operation; one method argument is justified by the two requested, scientifically distinct detectors, while one-caller peak/plateau/range/diagnostic helpers remain internal. Append new formals to preserve positional calls; input presence triggers saturation rather than another boolean.
- **Quantification API**: Add only `point_intensity(x, wavenumber, method)`; reuse an internal point lookup with `peak_ratio()`, retain `area_under_band()` for areas, and keep app definition builders internal. Numeric input presence triggers saved measurements without a second package wrapper.
- **Reactive policy**: Compute canonical processed/correlation/map data independently of the selected cell; assess only the selected spectrum because whole-map quality is slower, exclude correction-owned checks, reuse correction histories, and separate applied automation from post-processing quality results.
- **Paper evidence**: Retain a literal/reference implementation under `benchmarks/`; validate paper defaults rather than universalizing graphene tuning (`P0 = 40`, `FWHM0 = 4`). Fig. 6 is a required regression case because linear interpolation at `rel_height = 0.8` can truncate a real band.
- **Download diagnosis**: The deployed OpenSpecy 1.7.1 app reached ready state, but the native dynamic `session/.../download/download_data` click produced no artifact. Investigate the Shinylive 0.5.0 service-worker/`target=_blank` handoff and compare the last working export; if upstream attachment routing is unreliable, add a wasm-only in-frame fetch -> validated Blob/object-URL bridge while retaining normal local Shiny `downloadHandler` behavior.
- **Generated/dependency boundaries**: Keep root landing source under `site/`, build pkgdown into `/pkgdown/`, and generate `/app/` from `inst/shiny/`; assemble all three with one shared script and never hand-edit `_site/`, `docs/`, `_wasm/`, or generated Shinylive output. Preserve wasm closure/pins/small-library staging and configured roxygen 8.0.0.

## Package Surfaces

- `R/`: correction/assessment/range/SNR integration plus `point_intensity.R`; preserve object attributes and base-pipe composition.
- `tests/testthat/`: correction/quality tests plus `test-point_intensity.R`, `test-run_app.R`, and hosted contracts covering quantification, exact modal buckets, selection-only reactivity, legends, colors, and downloads.
- `benchmarks/`: repeated reference-vs-current correction, assessment, point lookup/ratio, and default single/100-spectrum/map app-stage cases; flag output differences and >10% same-output regressions.
- `inst/shiny/`: canonical pipeline, owner/child controls, status controls/modals, diagnostics/export, spectrum legend, heatmap palette; no new raw/generated media.
- `site/`, `tools/wasm/`, `.github/workflows/`: root landing source, three-route assembly/static checks/browser smoke, clearer Docker readiness, and unchanged immutable wasm/package pins.
- `README.md`, `_pkgdown.yml`, `vignettes/`: README-driven conventional `/pkgdown/` content and corrected tutorial links; remove the obsolete pkgdown marketing/embed source and keep README iframe-free.
- `vignettes/sop.Rmd`, roxygen/man, `NEWS.md`: scientific limitations, processing order, paper citation, warnings, UI/download changes; `DESCRIPTION` only if an unavoidable dependency is approved; reference-library workflow unchanged except subset matching compatibility evidence.

## Work Checklist

- [x] Implement and benchmark internal detectors plus `R/correct_spike.R`; add paper/local-residual fixtures and transactional invariant tests.
- [x] Extend `R/assess_spec.R`, `R/adj_range.R`, `R/sig_noise.R`, and `R/process_spec.R` with backward-compatible schemas/defaults, shared diagnostics, shared-axis restriction, and focused tests.
- [x] Implement the default ordered pipeline and quality controls in `inst/shiny/`; verify owner gating, active-spectrum counts/modals, matching-axis equality, legend, palette, and genuine local downloads.
- [x] Remove the unconditional canceled-download allowance, add the wasm-only validated Blob bridge, and require real click-to-disk evidence for all six choices.
- [x] Build the dark pkgdown welcome source, tutorial/navigation/audience paths, and acknowledgements; update SOP/roxygen/NEWS and regenerate only owned generated artifacts.
- [x] Run focused tests/benchmarks, documentation diff review, full tests/check, action-equivalent assembly, and desktop/mobile local/hosted visual/console review.
- [x] Replace app errors with automatic/warning/success details, move all four applied-correction diagnostics into Automatic, and apply the requested icon/outline colors.
- [x] Remove selection-driven heatmap/preprocessing/identification invalidation and benchmark/optimize the default single-spectrum path without changing scientific output.
- [x] Add `point_intensity()`, numeric-only ratio inputs, simultaneous saved point/area Measurements, metadata/download integration, docs, NEWS, tests, and benchmarks.
- [x] Add an outside numeric heatmap legend and share Match Name colors with the material summary; verify categorical behavior and desktop/mobile layout.
- [x] Run focused package/app tests and benchmarks, regenerate owned docs, run full tests, and inspect genuine local browser state/download/screenshot/console evidence.
- [x] Make success green, route configured SNR/correlation thresholds into exact pass/fail modal buckets, replace stale modal observers after each upload, remove the badge, and restore the persistent orange marker from real map clicks.
- [x] Fix the hosted map-download race by waiting for a fresh multi-spectrum heatmap and stable server-acknowledged selection; add the same thresholded download to native browser coverage.
- [x] Add exact artifact/repository validation, a shared digest-pinned Docker wasm builder, fresh isolated tools, and a clean-commit pre-push entry point; validate the downloaded current-HEAD action artifact.
- [x] Move the marketing/app shell into pure `site/` source, stage `/`, build README-driven docs at `/pkgdown/`, retain `/app/`, and update workflow/static/browser route contracts plus SEO evidence.
- [x] Add a read-only Docker engine preflight and a clearly labeled non-Docker landing/pkgdown shell test; run the latter locally and preserve the full Docker gate as mandatory before push.
- [x] On a clean candidate commit with a healthy Docker/WSL engine, run `tools/wasm/test-shinylive-prepush.ps1` and retain its JSON evidence.
- [ ] After a maintainer commit produces a matching current-SHA wasm artifact, run the canonical upload/identification and six-download hosted smoke.

## Verification

- Resolve the real Windows Rscript first; run focused `testthat` files, paper regression fixtures, object/axis/attribute invariants, 70% inclusive boundary and rollback, muted controls, selected-spectrum status changes, and real local files before `devtools::test()`.
- Run repeated benchmarks before full tests; run `devtools::document()` with roxygen 8.0.0 and immediately inspect exports/authorship/aliases; build the SOP/pkgdown source and run `devtools::check()` because the new public APIs are release-facing.
- Apply `openspecy-develop-shiny-app`: exercise no-upload, raw/processed, identified, batch/map, quantification, correction accepted/rejected, all downloads, progress, console, screenshots, and asset inventory.
- Validate the source landing page's metadata/JSON-LD/routes/mobile layout without Docker, then validate wasm repository/package/dependency pins and medoid/model libraries with the exact action artifact and nested-frame startup/upload/identification/library-match/app-mode/chooser/all-download/desktop/mobile smoke. Click-to-disk remains authoritative.

## Risks And Open Questions

- Paper modes were validated primarily on positive Raman cosmic-ray spikes and require enough peaks for the 3.5-SD LoD; calibrate on representative Raman/FTIR, negative-spike, edge, NA, heterogeneous-noise, and narrow-band cases before choosing the app's detector default.
- This is a large release-facing sequence: implement package detection/reporting contracts before app automation, then hosted/browser and presentation work, so UI does not duplicate unstable scientific policy.
- Docker Desktop 4.84.0/Engine 29.6.2 now passes the Linux action-equivalent build; keep the early engine diagnostic and portable-path guard so Windows-only checks cannot mask runner failures.

## Approval Notes

- Approved by: user, 2026-07-31 ("go for it").
- Minor extension approved by: user, 2026-08-01 (`speckit-implement` request in this chat).
- Hosted-site architecture extension approved by: user, 2026-08-02 (`speckit-implement`; static root, `/app/`, `/pkgdown/`, SEO).
- Completed evidence: clean candidate `ddf94d0e` passed the full Docker pre-push gate on 2026-08-02: exact 116-package repository, pinned checksums, three routes, nested-frame startup/upload/identification/map selection, and six genuine downloads; retained report records `status = passed` and 184,025,215 site bytes.
- Follow-up: after the maintainer commits and pushes the portable-path fix, verify the matching public wasm artifact and Pages publication; these are the only post-push checks still pending.
