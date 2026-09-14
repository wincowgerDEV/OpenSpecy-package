# Feature Plan: Peak Labels, Per-Organization Matching, And Bounded Hosted Analysis

**Feature dir**: `specs/033-shinylive-identification-peaks-memory`  
**Date**: 2026-09-14  
**Status**: Implemented locally; clean-commit hosted/offline workflows await maintainer push.  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Add live peak labels and per-organization library matches while repairing first-run hosted identification, genuine large-ENVI memory behavior, and the currently red CMD/Shinylive/offline CI chain.  
**Change class**: mixed; highest is hosted/release, with package/scientific and bundled-app behavior changes.

## Current Assessment

- At `930b164`, only Ubuntu R-devel CMD fails; Shinylive assembles then fails its Thresholded Particles download smoke with `$table.DataTable is not a function`, missing tables, an invalid Plotly trace, and Shiny client errors. Offline is consequently skipped; its last executed failure was the internet-blocked bundle smoke.
- `read_envi(..., representation = "Specs", background_filter = ...)` reads the supplied BIP map in blocks, but retains every eligible pixel in `value_chunks` and `cbind`s them before app collapse. The 511 x 580 x 427 float32 fixture is 506,217,040 bytes; a full R double matrix is about 966 MiB, so a 600 MiB allocation is credible even when connected mean collapse later yields fewer than 1,000 units.
- Metadata fixture `os_metadata_20260914-114832.csv` records threshold `sig_times_noise` 0.01, connected Mean collapse, medoid derivative identification, Top 1, no spatial smoothing, and enabled spectral smoothing/absolute first derivative (polynomial 3, window 90). The matching supplied BlueSphere BIP pair is 436 x 445 x 427/331,386,160 bytes (DAT SHA-256 `b1bccf03...e23f`; HDR `7302dcd5...50c6`); its dense R-double payload is about 632 MiB and directly explains the hosted allocation error.

## Goal And Scope

- Show ranked derivative-zero peak positions on only the active retained processed spectrum, and return Top X candidates from each library organization without changing the winning global identity.
- Make the exact large-file workflow collapse before dense foreground materialization and make first identification, rank selection, metadata, plots, and downloads reliable in local, Shinylive, and offline builds.
- **Out**: exporting a peak-finding R API, changing model-library ranking, approximate median collapse, reference-library rebuild/publishing, generated web edits, or push/pull.
- **Users**: app users get annotations and broader source coverage; R users opt in with `query |> match_spec(library = lib, top_n = 1, top_n_by = "organization")`.

## Requirements

- R1. Inside the Active retained spectrum frame, add **Show Peak Positions** (default on) and a gated 1-20 slider (default 7). It is live after a retained selection changes and never needs Run; off/no-upload/no-retained-selection/rejected-pixel states perform no peak work and show no markers.
- R2. An app-internal pure helper accepts exactly one processed spectrum, finds finite interior local maxima where the discrete first derivative crosses positive-to-negative (including a zero plateau bracketed by those signs), chooses one deterministic sample per plateau, then ranks by displayed intensity descending with wavenumber/source-index tie breaks. Plot the requested available ranks as points and non-overlapping wavenumber labels; fewer/no candidates remain valid.
- R3. Add `top_n_by = NULL` to spectral-library `match_spec()` methods. `NULL` preserves global Top-N and schema; one valid library-metadata column (initial use `organization`) returns up to `top_n` rows per nonblank group and query, globally score-sorted per query with deterministic library-order ties. Invalid/missing groups, grouped model matching, and nonpositive/missing `top_n` fail clearly; `ident_spec()` stays unchanged.
- R4. Add an Identification-owned **Top N per organization** switch, default on, muted when Identification/model mode is off. App matching passes `top_n_by = "organization"`; filter-library mode groups only selected organizations. Rank 1 across all returned candidates alone feeds canonical material/max-correlation metadata, while every group winner feeds Top Matches, metadata selection, and download.
- R5. Grouped blockwise matching loops by organization and by at most 1,000 query spectra, releasing each correlation block after reduction. Progress reports organization plus query-block completion; retained-capacity checks use `queries x top_n x groups`. Ungrouped outputs remain tolerance-identical and no slower by more than 10% on repeated representative benchmarks.
- R6. For BIP ENVI + raw/spatial S/N threshold + connected Mean collapse, keep the source as `FileSpecs`, stream S/N and connected membership, then read and accumulate retained spectra by particle before materializing only collapsed `OpenSpecy` units; never `cbind` the retained full map. Preserve source mapping, coordinates, metadata, thresholds, mean spectra, processing order/attributes, active inspection, heatmap, summaries, and downloads. Unsupported large-file modes preflight and explain their bound instead of attempting a huge allocation.
- R7. A fresh hosted session with genuine packaged spectra immediately shows Top Matches rank 1 and Selection Metadata, supports rank changes, and completes all native downloads without DT/Plotly/Shiny client errors. Fix the result-ready/trace/table cause in shared source, not a smoke-only exception.
- R8. Diagnose and fix the exact Ubuntu R-devel check error; require CMD, Shinylive, and an actually executed (not skipped) offline bundle workflow to pass on one candidate. Offline remains a consumer of the exact successful Pages SHA.

## Technical Decisions

- **API**: `top_n_by` is a meaningful, general metadata-group policy; its presence triggers grouping, so no package boolean or new export is added. Implement one internal ranking primitive shared by `match_spec.OpenSpecy()`, `match_spec.Specs()`, and `.match_spec_blockwise()`; model matching rejects it.
- **Object flow**: mounted ENVI pair -> descriptor-only `FileSpecs` -> bounded S/N/mask -> connected unit mapping -> streaming Mean `OpenSpecy` -> ordinary processing -> organization/query-block matches -> canonical final `OpenSpecy` -> plots/tables/downloads. Keep unique spectra names, aligned metadata, coordinates, selection status, processing/correction attributes, and the original file read-only.
- **Memory/observability**: primary reproduction is BlueSphere 194,020 x 427 and stress fixture is MIPPR 296,380 x 427; chunk <=8,192 (about 27 MiB raw double payload). Log source/eligible/unit counts, bytes and elapsed time at mount, S/N, components, collapse, processing, each organization, and completion. Target no single spectral allocation above 64 MiB and incremental webR heap below 512 MiB; stop before allocation if projected live state exceeds 768 MiB or measured stage growth exceeds 2x its projection.
- **Dependencies/generated files**: add none. Update roxygen and regenerate/inspect `man/match_spec.Rd` with configured roxygen2 8.0.0; never hand-edit `NAMESPACE`, Rd, pkgdown, or `_wasm/` output.
- **Bundled app/diagram**: canonical visible/exported source remains `canonical_state_gate()$object`; peak annotations read only `active_spectrum_view()`. Gate peak count behind peak owner and retained status, and grouping behind Identification/non-model mode. Update Pipeline boxes **Read & Combine/Base Specs Materialization**, **Collapse**, **Library Identification**, **Active Spectrum Inspection**, **Spectrum Plot**, **Top Matches**, and **Selection Metadata**.
- **Hosted impact**: shared `R/`, `inst/shiny/`, tests, docs, and diagram change. Preserve `/`, `/app/`, `/pkgdown/`, hard pins, closure, compact libraries, and generated boundaries; run `-HostedAppStatic`, an exact matching-artifact preflight/nested-frame smoke, then a clean-commit rebuild because this tranche is release-facing.
- **Surface classification**: `R/`, `tests/testthat/`, `benchmarks/`, `inst/shiny/`, app docs, and `NEWS.md` change; `tools/{wasm,offline}` and `.github/workflows/` change only when root-cause repair requires it. `workflows/`, `site/`, README, `DESCRIPTION`, dependencies, and app assets stay unchanged; reference-artifact compatibility is N/A.

## Package Surfaces And Work Checklist

- [x] `R/match_spec.R`, `R/Specs_methods.R`: add validated grouped ranking and two-axis block iteration; `tests/testthat/test-match_spec.R`, `test-Specs.R`: groups, ties, NA/blank/error/model cases, schema, capacity, callbacks, and legacy equivalence.
- [x] `R/Specs_file.R`, `R/automate_particle_filespecs.R`, `inst/shiny/{global.R,server.R}`: reuse/extract file-backed S/N, connected mapping, and mean reducers at the app boundary; add fixture-scale telemetry and no-large-allocation regression tests.
- [x] `inst/shiny/{ui.R,global.R,server.R}`: internal peak helper, retained-state owner/slider, live marker/text traces, and per-organization switch/guidance; update headless app tests for every gated state.
- [x] Repair first-result DT/Plotly selection and native-download behavior in shared app/wasm sources; update local, hosted, and offline browser fixtures only to express the corrected contract.
- [x] Update roxygen/example, `vignettes/{sop.Rmd,app.Rmd}`, `NEWS.md`, and `.specify/memory/pipeline-diagram.html`; regenerate expected docs and inspect generated diffs.
- [x] Add repeated `benchmarks/match_spec_grouped.R` and `benchmarks/shinylive_filespec_collapse.R`, retaining old kernels and checking output equivalence, runtime, allocation projection, and abort thresholds.
- [ ] Isolate R-devel failure, run focused/full/package/hosted gates once on the final candidate, then reconcile evidence, processes, status, and scratch; local gates pass, while clean-commit Shinylive/deploy/offline Actions await maintainer push.

## Verification

- Focused: configured Windows R runs `devtools::test(filter = "match_spec|Specs|FileSpecs-particle|run_app|app-in-memory|shinylive_wasm", reporter = "check", stop_on_failure = TRUE)` plus app/R/JS/PowerShell parse/static checks and both benchmarks before broad tests.
- Scientific/UI acceptance: hand-built spectra cover sharp, plateau, tied, boundary, NA, fewer-than-X, and no-peak cases; grouped results equal independently split-and-ranked organization oracles; the ungrouped route is unchanged.
- Genuine large path: reproduce the metadata settings on BlueSphere DAT/HDR `b1bccf03...e23f` / `7302dcd5...50c6`, then stress the supplied MIPPR `fb93e1b8...a6f3` / `2901a1e8...b61`; compare collapsed spectra/mapping/counts to bounded eager oracles within `1e-10`, verify source hashes unchanged, and inspect stage telemetry.
- Browser: fresh local and exact nested hosted frames cover no-upload, retained/rejected selection, live peak on/off/count, grouped Top Matches and rank changes, Selection Metadata, 300-500 MB mounted ENVI Run, genuine Processed/Top Matches/Thresholded Particles downloads, desktop/mobile screenshots, and severe console/server errors.
- Broad/release: configured documentation diff, full `devtools::test()`, `devtools::check()`/all R-CMD matrix legs, `-HostedAppStatic`, exact-artifact preflight, clean wasm rebuild, successful deploy smoke, then internet-blocked offline archive smoke for the same SHA. Reuse only evidence whose source, fixture, dependency, pin, and contract remain unchanged.

## Risks And Open Questions

- Exact streaming median is intentionally unsupported for oversized maps; small-map legacy behavior stays, and large-map Median must fail preflight unless a later plan approves a disk-backed exact algorithm.
- Gaussian spatial smoothing remains the scientific default. Mean/median-neighbor smoothing is a potential separately benchmarked optimization, not required for the memory and identification fixes in this tranche.

## Approval Notes

- Approved by:
- Follow-up: implementation requires `openspecy-develop-shiny-app`, `openspecy-rscript-windows`, `openspecy-run-quality-gates`, and `openspecy-verify-hosted-app`; push/pull remains unauthorized.
