# Feature Plan: Shared-Range Library Identification

**Feature dir**: `specs/046-shared-range-identification`  
**Date**: 2026-10-07  
**Current tranche**: Correct bundled-app spectral-library correlations by cropping temporary query/reference inputs to their typed shared support before normalization, missing-value filling, and correlation, and show that exact correlation interval in the spectrum plot.  
**Change class**: Bundled-app behavior with scientific identification impact.

## Goal And Scope

- Restore scientifically comparable library scores for uploaded spectra whose axes extend beyond the selected reference data, including the three supplied SPC spectra whose current scores are about 0.04--0.26 instead of about 0.9.
- **In**: ordinary dense identification, file-backed streaming and selected-spectrum Top-N, correlation-threshold matching, Cluster Buster's temporary background reference, and display-only plot cropping while a real-library match is selected.
- **Out**: public R API changes, changing canonical uploaded/processed spectra or downloads, rebuilding reference artifacts, changing model classification, or deploying the hosted app.
- **Users**: local and hosted app users selecting full or medoid spectral libraries, especially the default All spectrum-type search.

## Requirements

- R1. Before normalization or mean filling, form temporary query/reference pairs only on wavenumbers within both the processed query axis and an applicable typed-library support envelope; retain the committed canonical processed object unchanged.
- R2. Treat independently ranged FTIR/Raman/NIR partitions as applicable support groups, merging only identical envelopes. Skip groups with fewer than three shared points so a non-overlapping NIR range cannot widen or damp Raman/FTIR correlation.
- R3. After cropping, retain reference/query `NA`s inside the typed envelope and use the established mean-fill and Pearson calculation. Exclude candidates with fewer than three finite reference values in the overlap.
- R4. Preserve global and per-organization Top-N ranking, deterministic library order, block-size memory bounds, progress reporting, metadata joins, thresholds, and selected-reference display. When a real-library match is selected, crop display copies of raw, active, reference, and peak traces to that match's correlation interval.
- R5. Apply identical alignment semantics to dense, prepared/streamed, selected-file, Cluster Buster, and selected-match plot paths; the three supplied SPC files must keep their expected PE/PS/PET winners and score above 0.88 with the current derivative medoid library.

## Technical Decisions

- **Approach**: add internal app helpers that derive each `spectrum_type` partition's finite envelope, merge partitions sharing one range, crop a copy of the query, conform a copy of each reference group onto that cropped query axis, then call existing package matching. Candidate NAs within a typed envelope remain for established post-crop mean filling. Combine group candidates and rerank globally. A lightweight per-library source/target/final interval map replays the exact selected conformation for plotting without rescanning Full; prepared file-backed matching stores the same cropped groups and subsets every later chunk before fill/correlation.
- **Cluster Buster**: its processed background remains a separate full-query support candidate; typed library partitions are still cropped to their shared envelope before bounded comparison.
- **Public API review**: no new export, argument, flag, default, or return type. Package `cor_spec()` already intersects different axes before filling; the regression is the app manufacturing equal padded axes. Keep helpers internal to `inst/shiny/global.R`.
- **OpenSpecy contract**: all temporary pairs retain aligned `wavenumber`, spectrum columns, metadata rows, and attributes. Identification and display cropping do not further mutate the committed canonical processed axis used for metadata, quantification, and downloads; the spectrum plot uses temporary cropped display copies only while a real-library match is active.
- **Numerics**: crop first, then established `make_rel()`, `.matrix_mean_replace()`, scaling, and Pearson correlation. Expected acceptance tolerance is score `> 0.88`; synthetic dense/prepared results must agree within `1e-12`.
- **Performance**: typed range groups are few (normally one applicable technique; FTIR/Raman currently share a range), existing query/library blocks remain bounded, and live plot changes reuse recorded intervals rather than rescanning the library. This is a correctness change, not a same-output optimization, so no benchmark artifact is required.
- **Pipeline diagram**: update `Library Identification`, `Active Spectrum Inspection`, and `Spectrum Plot` to name typed shared-support alignment, pre-fill cropping, skipped non-overlapping groups, unchanged canonical state, and the display-only correlation-range crop.
- **Hosted impact**: `inst/shiny/` is shared hosted source. Run fast `-HostedAppStatic`; exact-artifact preflight is deferred until an action-built wasm artifact contains this unpushed change. No dependency, image, build-driver, or pin change triggers a clean wasm rebuild rehearsal.

## Package Surfaces

- `inst/shiny/global.R`: typed-support grouping/alignment, grouped Top-N reranking, prepared/bounded matching, and correlation-range plot inputs.
- `inst/shiny/server.R`: route dense, streaming, selected-file, Cluster Buster, and selected-match plotting through the shared helpers.
- `inst/shiny/ui.R`: explain typed shared-support cropping, skipped non-overlapping groups, and display-only plot cropping beside the affected controls.
- `tests/testthat/test-app-in-memory-helpers.R`, `tests/testthat/test-run_app.R`: synthetic regression, edge cases, path equivalence, and orchestration assertions.
- `tools/shiny-local-smoke.spec.js`: targeted genuine-file browser journey for the extended-axis identification state.
- `.specify/memory/pipeline-diagram.html`: synchronize the Library Identification, Active Spectrum Inspection, and Spectrum Plot contracts.
- `NEWS.md`: record the restored correlation semantics.
- `R/`, generated docs, `DESCRIPTION`, assets, site sources, workflows, and reference artifacts: unchanged.

## Work Checklist

- [x] Add temporary typed shared-support alignment and deterministic grouped ranking helpers.
- [x] Use them in dense, streamed, selected-file, Cluster Buster, and selected-match plot paths.
- [x] Add focused regressions for extended tails, typed non-overlap, internal `NA`s, insufficient overlap, Top-N grouping, prepared/eager equivalence, exact selected-match display range/peaks, and an unchanged canonical axis.
- [x] Update NEWS and the canonical Library Identification, Active Spectrum Inspection, and Spectrum Plot diagram nodes.
- [x] Verify supplied SPC files, focused/full tests, fast hosted-source checks, and a targeted genuine-file browser journey with console/screenshot inspection.
- [x] Reconcile evidence, inspect owned processes/status, and remove task-created scratch artifacts.

## Verification And Risks

- Reproduction baseline: current All/derivative-medoid scores are `0.120009`, `0.040360`, and `0.255534`; Raman-support crops give `0.912290`, `0.903038`, and `0.916979` with unchanged PE/PS/PET winners.
- Focused tests must prove unsupported tails cannot affect a score, internal holes are filled only after cropping, disjoint groups are skipped, fewer than three shared points errors clearly, and dense/prepared/blockwise results agree.
- Browser fixture controls must explicitly select All, Derivative, and Medoid. Inspect the Top Matches score, all three displayed traces cropped to the selected correlation interval, busy state, server output, and severe console errors.
- State impact: no-upload and model-identification states are unchanged; real-library dense/batch/streamed scores and the identified plot change; canonical quantification, metadata, and downloads remain unchanged by identification/display cropping.
- Focused commands: `Rscript -e "devtools::test(filter='app-in-memory-helpers|run_app', reporter='summary')"`; targeted browser: `quality-gates.ps1 -Filter run_app -BundledAppBrowser -BrowserGrep 'extended spectrum axes use shared reference ranges'`; broad gates: `quality-gates.ps1 -HostedAppStatic` and `quality-gates.ps1 -FullTests`.
- Risk: combined All libraries use an NA-padded union axis. Range grouping must use typed metadata and each partition's finite envelope rather than that union, retain within-partition NAs for post-crop filling, and preserve original candidate and organization order during final reranking.
- Broad gates: run the full package test suite once on the final scientific candidate plus `-HostedAppStatic`; routine R CMD check remains deferred because this is not release-facing and the public package API is unchanged.
- Final evidence: supplied scores/winners are PE `0.912289`, PS `0.903038`, and PET/polyesters `0.916979`, all over `805.923706--3196.427734 cm^-1`; focused helper/server tests and the genuine Green-bottle browser journey passed, with screenshot/console review confirming all plotted traces share that interval.
- Full-library probe: the legacy derivative artifact (`1,983 x 43,171`, data-table-backed) formed three typed groups in 2.62 seconds and matched the Green-bottle spectrum in 26.22 seconds without storage or subsetting errors; the winner remained in the Raman polyester family.
- Gate evidence: `-HostedAppStatic` passed 370 assertions and source/workflow checks; full tests passed 4,388 assertions with 30 expected warning assertions and two guarded large-AWS skips. Exact-artifact preflight awaits a matching pushed wasm artifact; R CMD check is not triggered for this bundled-app-only change.
- Footprint: no `inst/shiny/www` asset changed; bundled-app source increased by 13,278 bytes.

## Approval Notes

- Approved by the maintainer's correlation-regression request on 2026-10-07; remote synchronization and hosted publication remain out of scope.
