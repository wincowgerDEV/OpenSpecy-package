# Feature Plan: In-Memory Particle Workflow And Memory Preflight

**Feature dir**: `specs/012-in-memory-particle-workflow`  
**Date**: 2026-08-13  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Finalize staged spectral clustering, legend/export/selection behavior, axis-preserving identification, and 1 GiB-class performance; retain package FileSpecs.
**Change class**: Mixed; highest hosted/release, plus package/scientific and bundled-app behavior.  
**Status**: Follow-up implementation complete; hosted smoke awaits a matching action-built artifact.

## Goal

- Give app users one in-memory `OpenSpecy` workflow that forecasts memory, reduces spectra before expensive work, and explains how to recover when a job will not fit.
- Make threshold and PCA/K-means collapse coherent while preserving package `FileSpecs` APIs.

## Scope

- **In**: 10 GiB upload ceiling; RAM/peak forecasts; app FileSpecs purge; corrected partition order; blockwise matching; map masks; compact legends; themed histograms; condition routing; docs.
- **Out**: Removing package `FileSpecs`/`open_specs()`/FileSpecs particle methods; guaranteeing sub-10-GiB jobs fit; editing generated output; changing `/`, `/app/`, or `/pkgdown/`.
- **Users**: Browser and `run_app()` map users; package file-backed workflows remain.

## Requirements

- R1. App source, smoke code, and app-facing docs contain no FileSpecs/file-backed/local-H5 route, state, control, or guidance; package implementations, tests, and benchmarks remain.
- R2. Client selection, server validation, and `shiny.maxRequestSize` use one `10 * 1024^3`-byte total cap in local and hosted modes. Exactly 10 GiB passes; 10 GiB + 1 byte and multi-file totals above it fail inline before reading.
- R3. Pre-read and operation forecasts report resident/peak bytes, available RAM and probe source (or "unknown"), reserve, and remedies. Stop known-unsafe work inline; never call unknown RAM safe.
- R4. Default order: upload -> XY conform -> optional spatial smoothing -> S/N/cluster/collapse -> preprocessing -> identification -> quantification. When correlation thresholds collapse pixels, calculate S/N from the spatial-only object, fully process eligible pixels, identify once, then filter/collapse using that pass.
- R5. One final `OpenSpecy` reactive feeds spectra, summaries, identification, quantification, metadata, downloads, and selection. A pixel-to-unit table only projects final numeric particle IDs/scores/masks/clicks to the map; rejected clicks return no match and a flat white processed trace.
- R6. S/N-only connected regions use its spatial-only pass mask. Correlation-only/both reuse one fully processed blockwise identification result: filter below correlation (and S/N for both), then form connected same-material regions. Failures become background, not unknown particles.
- R7. Cluster strategies fit source-scoped PCA/K-means to eligible spatial-only pixels before other processing, collapse raw/spatial-only spectra by cluster, then process and identify those clusters once. Nonspatial outputs those cluster particles; spatial reclusters original pixels into connected equal-material particles, reprocesses them without re-identification, and reuses the first pass identities. UI components/K start at 10, clamp/show effective limits, and geometric mean requires positive values.
- R8. Identification retains only top N matches per spectrum (Identification control, default 10). The match table and Top Matches download use this same dataset and never exceed current N. Collapse prevents other consumers from triggering correlation; matching ranks/discards bounded query blocks.
- R9. An explicit mask paints rejected pixels black for S/N, correlation, or both on every map; missing cells remain distinct and hover names the reason.
- R10. Heatmaps have no inline legend; a button opens a themed legend modal, while categorical maps above 30 categories show only an explanatory message. Always show themed S/N, and use the same interactive histogram renderer in all states; draw only enabled threshold lines.
- R11. Only caught errors open alerts. Warnings go to inline diagnostics, messages to progress/logs, and prerequisites, memory/size estimates, unsupported inputs, and validation stay inline.
- R12. Help/progress state pixel versus unit level, order, metric scale, PCA/K bounds, correlation cost, effective settings, and remedies: S/N-first collapse, fewer references/clusters/components, smaller data, or more RAM.
- R13. Ingestion reads each member once, combines/conforms once, preallocates H5/ENVI outputs where practical, skips no-op NA/zero work, releases intermediates, and preserves axes/metadata/attributes; benchmark peak load memory.
- R14. Particle downloads include both histograms, every available heatmap, material and particle-size plots, and the final-particle summary table; current top-N limits still govern match exports.
- R15. A default-on advanced control preserves the uploaded wavenumber axis and conforms the identification library to it with `mean_up`; the memory forecast reflects the conformed reference shape.
- R16. Collapse mode emits no data.table scope warning, implicit histogram-bin message, or out-of-bounds quality alert. The supplied ~833 MiB RDS loads and completes a representative app-equivalent pass without avoidable full-data copies and with timing recorded.

## Technical Decisions

- **Approach**: The app calls `read_any(c_spec = FALSE)` and combines once. Particle internals return `analysis_units` plus complete/stable `pixel_to_unit`; corrected units (or pixels without collapse) become `canonical_final()` with no second identification path.
- **Correlation/memory**: Show `8*M*N` as avoided full-matrix cost, but gate actual block peak. Attempt a RAM probe/adaptive `Q` once; if unavailable or unreliable, use 100 queries. Rank each block immediately into the shared top-N table and discard its matrix. Treat more than 10% runtime overhead as a reported regression; accept at most 50% for fixed bounded blocks because avoiding the unbounded matrix is the controlling contract.
- **Cluster collapse**: PCA/K-means is the first reduction on spatial-only pixels and is source-scoped, never repeated per connected region. Identify processed cluster spectra once. Nonspatial keeps those clusters; spatial projects their identities to pixels, forms connected equal-material particles from the spatial-only source, processes those final particles, and carries the strongest first-pass match evidence forward without a second correlation.
- **Public API review**: Add no export, flag, formal, or hidden `...` behavior. `specs_centers` remains public K policy; app PCA components pass only to an internal partition helper. Base-pipe composition and optional file output remain.
- **Dependencies**: None unless portable RAM probing proves impossible. Use injectable native/browser probes with explicit unknown; OS output is not a scientific dependency.
- **OpenSpecy contract**: Preserve aligned core fields, IDs, correction/visual/scientific attributes. Final metadata stores stable unit IDs/counts; clicks/exports preserve membership.
- **Generated artifacts**: Update roxygen for changed particle behavior/`...`, regenerate with the configured R/roxygen2 8.0.0 toolchain, and inspect `NAMESPACE`/`man/*.Rd` immediately; no direct generated-file edits.
- **Bundled app**: Apply `openspecy-develop-shiny-app`; owners gate bounds/area/PCA/K computation. Verify no-upload, processed, identified, muted, three collapse, batch, quantification, blocked-memory, download, progress, console, and screenshot states; report unchanged assets/size.
- **Hosted app**: Keep root `site/`, relative iframe, generated `/app/`, and `/pkgdown/`. Build only through the action and final package pin; keep dependency/library staging pinned. Run action preflight and nested-frame startup/upload/identify/download; do not edit generated output.
- **External resources**: Guard existing library downloads. Optional large H5 evidence cannot be required or replace synthetic 10 GiB boundary tests.

## Package Surfaces

- `R/automate_particle_analysis.R`, `R/match_spec.R`: correct partition/mapping and add internal bounded matching; `R/Specs_file.R` and `R/automate_particle_filespecs.R` remain behaviorally unchanged.
- `R/read_multi.R`, `R/read_ext.R`, `R/read_envi.R`, `R/manage_na.R`: same-output in-memory load/copy fast paths, only where benchmarks show lower peak allocation.
- `R/run_app.R`: remove the app-only local-files option lifecycle; preserve launch and dependency checks.
- `tests/testthat/`: extend particle tests; add mapping/block-memory coverage; rewrite `test-run_app.R`; migrate useful assertions, then remove/rename `test-run_app-filespec.R`.
- `benchmarks/particle_in_memory_workflow.R`: retain full-matrix/old-flow references; repeat equivalence, time, allocation/peak, and regression checks.
- `inst/shiny/{global.R,server.R,ui.R,www/parent-frame.js}`, `tools/shiny-local-smoke.spec.js`: implement the graph/display/validation; remove stale FileSpecs/modal/tolerated-alert paths.
- `README.md`, `vignettes/sop.Rmd`, `NEWS.md`: retain package FileSpecs docs, remove app claims, and document cap, forecast, threshold/cluster order, controls, and recovery.
- `DESCRIPTION`: unchanged unless the no-new-dependency decision is invalidated; `NAMESPACE`/`man/`: generated only; `workflows/`: package FileSpecs workflow unchanged.
- `.github/workflows/`: source unchanged unless preflight exposes a real action defect; rerun the existing Shinylive build/deploy verification because shared app/package inputs change.

## Work Checklist

- [x] Purge app FileSpecs, unify 10 GiB/memory helpers, and remove redundant load copies (`inst/shiny/`, readers, `R/run_app.R`, tests/smoke).
- [x] Implement corrected connected/PCA-K-means partitions, stable pixel mapping, and blockwise best/top-N matching (`R/automate_particle_analysis.R`, `R/match_spec.R`).
- [x] Rebuild server reactivity around `canonical_final()` and rewire plots, tables, summaries, selection, quantification, and native downloads (`inst/shiny/server.R`).
- [x] Implement black masks, title-free compact legends, always-visible themed histograms, owner/child controls, progress, inline guidance, and condition routing (`inst/shiny/global.R`, `ui.R`, `server.R`).
- [x] Add scientific/app regressions and the repeated memory/runtime benchmark (`tests/testthat/`, `benchmarks/particle_in_memory_workflow.R`).
- [x] Update source docs/NEWS, regenerate roxygen output, and inspect generated diffs (`R/`, `README.md`, `vignettes/sop.Rmd`, `NEWS.md`).
- [x] Run final focused/package/local-browser/download gates; audit hosted artifacts and defer nested-frame smoke because none matches this candidate.
- [x] Correct the staged global cluster -> identification -> optional spatial material-collapse flow, final particle IDs/counts, rejected-pixel selection, and quality indexing.
- [x] Add modal legends, restored summary/table and figure exports, deterministic histograms, and warning-free mapping updates.
- [x] Add default-on uploaded-axis preservation with reference `mean_up`, validate forecasts, and profile/fix the supplied ~833 MiB RDS path.
- [x] Run focused scientific/app tests, genuine local browser/download smoke, the external large-file timing gate, then the smallest triggered broad gates.

## Verification

- **Direct/scientific**: Test reader/load equivalence, threshold truth table, spatial-only S/N, exact region/cluster membership, area/clamping, pixel projection, block/full top-N equality with ties/NA, and `OpenSpecy` attributes.
- **Focused**: Resolved Windows `Rscript` runs `devtools::test(filter = "automate_particle_analysis|match_spec|run_app")`; parse changed sources; scan proves FileSpecs absent from app but present in package.
- **Benchmark**: Repeat load and analysis cases; report equivalence, reduction, median time, allocation/peak RSS, forecast error, and flags before broad tests.
- **Local browser**: Use `CA_tiny_map.zip` and the supplied `dropRegion1.rds`; test S/N-off/correlation-on, three collapse modes, modal legends (including >30), histograms, rejected clicks, summaries/exports, progress, console, and screenshots.
- **Downloads/states**: Inspect genuine Processed, Top Matches, Thresholded Particles, and metadata outputs across processed/identified/batch/quantification; muted child edits cause no recompute/busy/error UI.
- **Boundary/memory**: Inject sizes/RAM for 10 GiB, +1 byte, multi-file, safe, unsafe, unknown, and minimum-block cases; allocate no boundary fixture.
- **Docs/broad**: Confirm versions; run `devtools::document()`, inspect Rd/NAMESPACE, render the vignette, then full `devtools::test()` and `devtools::check()` once.
- **Hosted**: Verify final pin, dependency closure, staged libraries, action preflight, and nested-frame upload -> collapse -> identify -> download while preserving all routes.
- **Reusable evidence**: Reuse only 010 FileSpecs parity while its package files/dependencies/fixtures stay unchanged; all 011 app/browser/full-suite evidence is invalidated.

## Risks And Open Questions

- 10 GiB is a transport ceiling; browser/WebAssembly practical limits may be lower, but preflight must not silently restore a fixed smaller cap.
- Correlation-only and combined thresholds require one blockwise pixel identification pass; combined mode first removes spatial-only S/N failures, then applies correlation/material grouping to that same pass and never correlates collapsed units again.
- PCA/K-means corrects broken public outputs; document the change instead of preserving erroneous clusters.
- RAM probing is best-effort; abandon it after the first failed implementation pass and keep the 10 GiB ceiling plus fixed 100-query blocks.

## Approval Notes

- Approved by: User, 2026-08-13
- Follow-up: Package FileSpecs remains available for future research but is not presented by the app.
- Follow-up approved by user, 2026-08-14: staged spatial/nonspatial cluster semantics, modal legends, complete figure/table exports, uploaded-axis preservation, and the supplied large RDS acceptance test.
- Acceptance: the 872,949,677-byte RDS loaded in 23.37 s and completed a calibrated 1,807-particle workflow in 142.29 s total; peak ingestion Vcells were about 2.13 GiB. Focused particle/Top-Matches browser downloads, full package tests, vignette, and final staged R CMD check passed (one pre-existing UTF-8 data NOTE). The complete browser sweep passed 3/4 journeys; its general journey reproducibly timed out in the harness after a completed metadata download, while the affected identification and download paths pass separately. Hosted smoke remains artifact-gated.
