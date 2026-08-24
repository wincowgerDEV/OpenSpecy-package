# Feature Plan: Active-Spectrum Quality And Large-Map Hosted Support

**Feature dir**: `specs/017-active-spectrum-large-map`
**Date**: 2026-08-21
**Review budget**: Under 100 nonblank lines and 1,500 words.
**Current tranche**: Remove speculative memory blocking, make quality findings follow the plotted active spectrum, update scientific defaults, and bypass Shinylive's copying upload transport by mounting the supplied 500 MB ENVI input before fully materializing it in R memory.
**Change class**: Mixed; highest hosted/release, with package/scientific and bundled-app behavior changes.

## Goal

- Let real runtime behavior, not a RAM forecast, decide whether analysis can proceed; large jobs show measured stage progress and actionable failures.
- Make every quality finding describe the exact active spectrum shown in the spectrum plot, including collapsed/rejected selections, with scientifically reviewed defaults.

## Scope

- **In**: remove `.app_memory_preflight()` and its UI/blocking branch; active-spectrum inspection lane; SNR/flat/silent-region fixes; 2x artifact defaults; a browser-File-to-WORKERFS Shinylive ingestion path; measured full in-memory ENVI analysis; synchronized pipeline diagram.
- **Out**: `FileSpecs`/chunked downstream analysis; promising that every upload below 10 GiB fits WebAssembly; changing identification libraries; committing the 500 MB fixture; silently reducing scientific options.
- **Users**: local and hosted app users inspecting individual spectra or analyzing hyperspectral ENVI maps.

## Requirements

- R1. Delete the platform RAM probes, forecast helper/status UI, and known-unsafe Run short-circuit. Preserve the 10 GiB upload ceiling and surface actual read/allocation/runtime failures with phase, elapsed time, and recovery guidance.
- R2. Define one `active_spectrum_view()`-equivalent single-spectrum object used byte-for-byte by the Active Spectrum plot trace and `assess_spec()`. A retained selection uses its canonical unit; a rejected collapsed pixel uses that clicked source pixel processed with the committed Run settings and is labeled as a rejected-pixel inspection, never replaced by a synthetic zero spectrum.
- R3. Compute the viewed spectrum's SNR directly from that single-spectrum object and metric selection. Quality findings must not index `canonical_signal_noise()`, `max_cor()`, the heatmap projection, or a full-dataset position to obtain active-spectrum evidence; correlation may reuse an already-computed selected-unit match but must not trigger matching.
- R4. Changing the threshold switch alone must not change the plotted active-spectrum intensities or the flat/silent/high-tail/CO2 result for the same selection. Finite non-flat spectra must not report `finite_intensity_range = 0`; finite active spectra must always yield an evaluable SNR finding.
- R5. Change `assess_spec()`'s silent-region default to `c(2420, 2550)` cm^-1. Change the high-tail/CO2 detection and automatic-correction default artifact ratio from 3x to 2x consistently in `assess_spec()`, `restrict_range()`, `flatten_range()`, internal fallbacks, and app controls; explicit caller values remain authoritative.
- R6. Use `C:/Users/winco/Downloads/drive-download-20260818T215633Z-1-001.zip` only as opt-in external evidence. It contains a 571 x 580 x 427 BIP float32 map (331,180 spectra; 565,655,440 raw bytes; about 1.054 GiB as one R double matrix). Record upload/read/preprocess/collapse timings and peak/copy evidence without extracting into the repository.
- R7. In hosted mode, select files with a browser `File` input and mount those `File` objects into webR WORKERFS without sending their bytes through Shinylive's multipart/httpuv/R-raw upload bridge. Pass only mounted paths and file metadata into Shiny. Local Shiny retains native `fileInput()` behavior. Both paths must converge before `read_any()` and fully materialize the same in-memory `OpenSpecy` object before any processing, collapse, identification, quantification, plot, or download runs.
- R8. The mount is an ingestion optimization, not a file-backed analysis mode. It must support multiple companion files and ZIP input, use a session-scoped unguessable read-only mountpoint, reject traversal/name collisions, and unmount on replacement/session end. Small files retain native-upload fallback; large files with no mount capability get explicit guidance rather than entering the copying bridge. Preserve axes, spectra/metadata alignment, IDs, attributes, particles, SNR, matches, and exports exactly.

## Technical Decisions

- **Approach**: keep `canonical_final()` as the sole source for summaries, identification, quantification, metadata, and downloads. Add one deliberately named view-only active-spectrum lane for inspection/quality, sourced from the canonical selected unit or a one-column processed clicked-pixel fallback. Hosted mounting changes only how source paths enter `read_any()`; the complete object then follows the ordinary pipeline with no `FileSpecs` branch or processing-mode flag.
- **Public API**: no new arguments or exports. The reviewed changes are defaults for existing meaningful-policy arguments (`silent_region`, `artifact_ratio`); explicit values and base-pipe composition remain unchanged. Document scientific/user-visible consequences and boundary behavior.
- **Dependencies**: no new R dependency. The pinned webR 0.6.0 WORKERFS wrapper accepts package metadata, not its documented direct-file form: preserve the selected browser `File` objects through structured cloning, expose them as one composite Blob plus member offsets, and call `WebR.FS.mount("WORKERFS", {packages}, mountpoint)`. The deterministic adapter must fail closed on a Shinylive asset/hash mismatch rather than editing generated output by hand.
- **OpenSpecy contract**: the inspection object is one aligned `OpenSpecy`; canonical/export objects remain unchanged. Mounted and native inputs must produce equal fully materialized objects before downstream functions run.
- **Bundled Shiny app**: owner controls remain Run-gated. Quality follows selection without recomputing the full map or library. Verify no-upload, processed, threshold on/off, collapsed retained/rejected, identified, batch, and large-map progress/error states; assets unchanged.
- **Pipeline diagram**: `.specify/memory/pipeline-diagram.html` shows local temporary files and hosted WORKERFS mounts converging at “Read & Materialize”; it omits the removed memory-prediction/block branch, routes Heatmap selection through “Active Spectrum Inspection” and “Quality Findings,” and retains dataset “Signal/Noise Compute” only for thresholding/histogram.
- **Hosted Shinylive/WebAssembly app**: add a small inner-frame picker/handshake and a reproducible outer-shell mount adapter owned by `tools/wasm/`; do not hand-edit `_site`. Current evidence shows the pinned bridge repeatedly concatenates the entire HTTP body and creates an R raw copy, so mounted files must bypass that request. Preserve `/`, `/app/`, `/pkgdown/`, package/dependency pins, closure, and staged libraries; exact-artifact tests must prove mount, read, replacement cleanup, ordinary small upload, and all existing app journeys.

## Package Surfaces

- `R/`: remove `app_memory.R`; update `assess_spec.R`, `adj_range.R`; readers change only if mounted ZIP/path handling exposes a format defect. `FileSpecs` code is unchanged and unused by the app.
- `tests/testthat/`: replace memory-forecast tests; add active-spectrum/collapse regressions, default/boundary tests, native-vs-mounted object invariants, and app source/reactivity contracts.
- `benchmarks/`: extend `particle_in_memory_workflow.R` with repeated upload-copy/read/materialization timing and an opt-in external-file case; do not benchmark a different scientific path.
- `inst/`: `global.R`, `server.R`, `ui.R`, and a small `www/wasm-file-mount.js`-equivalent adapter; local browser smoke confirms the capability-absent native path.
- `.specify/memory/pipeline-diagram.html`: target flow updated now and finalized with source locations during implementation. `tools/wasm/{prepare-shinylive-app.R,shinylive-smoke.spec.js,check-shinylive-export.R}` own reproducible injection and pin checks; workflows change only if required to assemble that owned source.
- `vignettes/sop.Rmd`, roxygen, `NEWS.md`: explain new defaults, active-spectrum QC semantics, large-map support boundary, and removal of forecasts; regenerate `man/*.Rd` with roxygen2 8.0.0. `DESCRIPTION`: unchanged.

## Work Checklist

- [x] Prototype WORKERFS mounting in an ignored export built from the exact pinned Shinylive/webR artifact; prove browser `File` bytes bypass the multipart/httpuv/R-raw bridge before changing app source.
- [x] Baseline the genuine ENVI ZIP, its direct HDR+DAT pair, and a small synthetic equivalent by mount/read/materialize/analysis phase; retain ignored hashes, timing, and peak/copy evidence.
- [x] Remove memory prediction/gating and replace its tests/UI/diagram branch with actual-failure guidance (`R/app_memory.R`, `inst/shiny/`, `tests/testthat/`).
- [x] Implement the single-spectrum inspection/QC lane and collapsed rejected-pixel regression (`inst/shiny/{global.R,server.R}`, app tests/smoke).
- [x] Apply 2420-2550 cm^-1 and 2x package/app defaults; update focused scientific tests, roxygen, SOP, and NEWS.
- [x] Implement the session-scoped hosted mount handshake and deterministic pinned-asset adapter; converge on ordinary `read_any()` and a fully in-memory `OpenSpecy`, with cleanup and small-file fallback.
- [x] Reconcile the already-updated target diagram with final function names/source locations, remove its target-status note, then run focused, benchmark, package, app-browser, and hosted tiers once.

## Verification

- **Direct/scientific**: selected retained/rejected collapsed pixels; threshold on/off identical active trace and non-threshold QC; finite SNR; non-flat range; silent-region boundaries; artifact ratios below/equal/above 2; explicit 3x override unchanged.
- **Performance/parity**: verify zero full-file HTTP/R-raw upload copy on the mounted route; repeated small native-vs-mounted object/output equality with <=10% downstream regression; genuine ZIP completes or records the actual browser/runtime limit within 30 minutes; direct HDR+DAT is a diagnostic comparator, not a different analysis mode.
- **App/browser**: focused `run_app|assess_spec|adj_range` tests, bundled static gate, mounted synthetic multi-file/ZIP journey plus native small upload, selection/collapse/QC/progress, console and desktop/mobile screenshots. The 500 MB journey is manual/CI-guarded and never routine.
- **Docs/package**: confirm roxygen2 8.0.0, regenerate/inspect generated diffs, run full `devtools::test()` and staged `R CMD check` once because exported scientific defaults change.
- **Hosted**: `-HostedAppStatic`; clean-commit wasm build because package image changes; exact matching-artifact action preflight and nested-frame smoke. Reuse current green workflow syntax/permissions only; R/app runtime evidence is invalidated.

## Risks And Open Questions

- WORKERFS avoids copying selected bytes through Shinylive's request bridge but does not reduce the 1.054 GiB R matrix or later processing copies; ZIP extraction may still add the 0.527 GiB DAT. If the exact mounted ZIP still cannot finish, report the measured phase and recommend selecting the extracted HDR+DAT pair, without restoring a forecast or changing downstream functions.
- Local evidence for SHA-256 `4931f855…d59b6d50`: the 521,106,125-byte ZIP materialized to 1,157.239 MiB (427 x 331,180) in 30.15 s; direct HDR+DAT extraction/read took 7.17 + 22.48 s and produced an equal compact signature. Small repeated mounted-path reads were 0.09-0.12 s versus 0.13-0.14 s with a native-copy proxy.
- Exact hosted evidence: clean candidate `4860abf` built the pinned 117-package WebAssembly closure in 50.1 minutes; action assembly, four guarded compact libraries, route/static checks, and the complete nested-frame mount/map/download smoke passed in 8.4 minutes (browser journey 3.2 minutes). The genuine ZIP stayed mounted without a crash or rejection but did not emit the post-`read_any()`/`manage_na()` validation marker within 30 minutes, so it exceeds the current supported browser evidence limit; routine smoke remains bounded to representative mounted HDR/DAT and ZIP fixtures.
- Shinylive does not expose a public app-level mount hook. The exact pinned asset must first prove a narrow message bridge can safely reach its webR proxy. If that prototype fails, stop for an upstream/fork decision rather than silently returning to FileSpecs or shipping a brittle generated-file edit.

## Approval Notes

- Approved by: user request, 2026-08-21.
- Follow-up: implementation must apply `openspecy-develop-shiny-app`, `openspecy-design-public-api`, generated-doc, quality-gate, and hosted verification skills; no push is authorized by this plan.
