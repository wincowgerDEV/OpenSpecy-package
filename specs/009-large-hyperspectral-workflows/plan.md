# Feature Plan: File-Backed Specs And Integrated Particle Workflows

**Feature dir**: `specs/009-large-hyperspectral-workflows`
**Date**: 2026-08-11
**Current tranche**: Integrate the approved FileSpecs particle pipeline into the bundled app, simplify its public controls, and repair post-push CI.
**Change class**: Hosted/release because package APIs, scientific processing, bundled Shiny, generated documentation, and GitHub checks change.
**Status**: FileSpecs Option A is implemented. The maintainer approved this app/API integration follow-up.

## Goal

- Keep `OpenSpecy` stable for ordinary in-memory work while making H5/ENVI-backed `FileSpecs` the bounded-memory path for large maps.
- Give local large sources and ordinary uploaded maps one understandable particle-analysis journey, scalable maps, progress, diagnostics, and complete configurable downloads.

## Scope And Compatibility

- **In**: Linux/macOS cache containment fix; 2 GB browser upload cap; Advanced-tab local source controls; Advanced master gating; particle-analysis settings/results; scalable maps and click metadata; threshold histograms; configurable result exports; removal of ineffective particle API arguments.
- **Out**: Rewriting unrelated `OpenSpecy` methods, Plotly map compatibility, hover payloads for every pixel, arbitrary FileSpecs pipelines, in-place source mutation, or matching raw pixels.
- Existing matrix-backed `Specs`, `.rds` serialization, ordinary preprocessing/identification, and non-particle `spatial_smooth`/`top_n` APIs retain their contracts.

## Requirements

- R1. Cache paths reject absolute, empty, dot, and parent-traversal components before platform normalization; the failing GitHub coverage test passes on all runners.
- R2. Browser and Shinylive uploads allow up to 2 GiB. No persistent size note appears; an oversize selection is stopped client-side and opens a message explaining the limit and local H5/ENVI route.
- R3. Move local H5/ENVI controls into Advanced. A top-level Advanced switch gates every child control and computation.
- R4. Run `automate_particle_analysis()` only when Advanced and Collapse Particle Spectra are enabled. Use the same orchestrator for local `FileSpecs` and compatible ordinary map uploads.
- R5. Feed collapse function, particle ID strategy, preprocessing, correlation, S/N metric, minimum, and maximum thresholds from visible app settings. Omit `raw` from ID strategies; collapse-off is the raw path.
- R6. Remove `spatial_smooth` and `top_n` from the particle-analysis public methods and workflow. Keep one exact best match internally and reject named legacy arguments clearly.
- R7. Emit useful package-stage messages and translate them into app progress for source indexing, S/N, collapse, matching, plots, outputs, and completion.
- R8. Replace the Plotly map with one scalable base/server plot. Particle results select `particle_image`, `particle_heatmap_thresholded`, `particle_heatmap`, or `cor_heatmap`; ordinary maps retain relevant options. A click opens concise nearest-pixel/particle metadata.
- R9. Show result S/N and correlation histograms in their threshold boxes, including lines at current thresholds, with styling consistent with the map.
- R10. Show Thresholded Particles download only while collapse analysis is enabled. Offer all pipeline artifacts selected by default and zip the chosen details, summaries, objects, maps, histograms, and timing.
- R11. Source files remain read-only; FileSpecs outputs use immutable versioned cache or session-owned export directories. Session cleanup never deletes user sources.
- R12. Preserve Uploaded Metadata behavior, selected-spectrum Plotly, native downloads, and hosted/local mode boundaries.

## Technical Decisions

- Keep `automate_particle_analysis()` as the single scientific orchestrator. Add no public Shiny callback; stage `message()` conditions work in scripts and are captured by the app for progress.
- The Advanced master controls UI availability and server guards. Collapse owns the particle pipeline, so no duplicate legacy collapse computation runs in that state.
- Maintain one session result and output directory keyed by source identity plus analysis settings. Local FileSpecs stays region-sequential; an ordinary map uses the default method.
- Render recorded particle plots with base Shiny rather than serializing full matrices to Plotly. Resolve clicks against retained result metadata and show a modal; do not ship per-cell hover data.
- Generate downloads from the completed session output directory and filter files by explicit content groups, avoiding scientific recomputation during download.
- The browser cap is a product limit, not a promise that every 2 GiB input fits WebAssembly memory; the popup directs larger or memory-constrained maps to the local path workflow.

## Package Surfaces

- `R/Specs_file.R`, `tests/testthat/test-FileSpecs.R`: cross-platform cache-path validation and regression coverage.
- `R/automate_particle_analysis.R`, `R/automate_particle_filespecs.R`, particle tests/benchmarks/workflow: simplified API, progress, exact single match, result/output integrity.
- `inst/shiny/{global.R,ui.R,server.R,www/}` and app tests: cap/popup, Advanced ownership, pipeline orchestration, maps/clicks, histograms, downloads, and metadata states.
- Roxygen, `NEWS.md`, vignette/SOP, and generated docs: update sources, regenerate with roxygen2 8.0.0, and inspect generated diffs rather than editing generated files.

## Work Checklist

- [x] Repair cross-platform FileSpecs cache containment and reproduce the former GitHub coverage failure locally.
- [x] Simplify particle-analysis API, add progress messages, update callers/tests/benchmarks/guidance, and regenerate documentation.
- [x] Rebuild Advanced UI ownership, 2 GiB popup contract, source controls, threshold/strategy settings, and dynamic download choices.
- [x] Integrate one guarded particle reactive for FileSpecs and ordinary maps with progress, scalable maps, click metadata, threshold histograms, and session-safe result exports.
- [x] Run focused package/app tests and a genuine tiny H5/ENVI plus ordinary-map smoke; inspect no-upload, disabled, processed, identified, and metadata states.
- [x] Run documentation, full tests, package check, coverage-equivalent test command, app gates, and hosted preflight/browser smoke when a matching wasm artifact is available.

## Verification

- **Focused**: cache traversal variants; removed-argument failures; result classes/plots/exports; FileSpecs region equivalence; master/collapse guards; 2 GiB boundary; map selectors/click modal; S/N/correlation thresholds and histograms; download contents.
- **Scientific**: retained pixels, particles, collapsed spectra, best match/correlation, summaries, maps, thresholds, and output files agree with the established eager pipeline and prior FileSpecs oracle.
- **Broad**: Windows R 4.3.3 focused/full tests, roxygen2 8.0.0 documentation, `R CMD check`, coverage-equivalent tests, installed app smoke, and only the hosted gates invalidated by shared app code.
- **Manual large**: use environment-configured `drop.h5`/`dropRegion1.rds` with temp cache/output; confirm three regions, progress, memory bounds, maps, click metadata, exports, and unchanged source hashes.

## Risks And Open Questions

- A nominal 2 GiB browser upload can still exceed practical Shinylive memory; fail clearly without claiming that every such file is analyzable in-browser.
- Replaying full-resolution recorded plots may need raster downsampling for response time, while thresholds and click coordinates must still refer to original data.
- FileSpecs supports a narrower collapse strategy contract than eager maps; unsupported selections must be disabled or rejected explicitly rather than silently changed.
