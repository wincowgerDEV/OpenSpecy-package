# Feature Plan: Active-Spectrum Quality And Large-Map Hosted Support

**Feature dir**: `specs/017-active-spectrum-large-map`
**Date**: 2026-08-21
**Review budget**: Under 100 nonblank lines and 1,500 words.
**Current tranche**: Post-push hosted-memory follow-up; safe reader improvements are verified, while the genuine ZIP is blocked on a custom webR runtime/fork decision.
**Change class**: Mixed; highest hosted/release, with package/scientific and bundled-app behavior changes.

## Goal

- Let real runtime behavior, not a RAM forecast, decide whether analysis can proceed; large jobs show measured stage progress and actionable failures.
- Make every quality finding describe the exact active spectrum shown in the spectrum plot, including collapsed/rejected selections, with scientifically reviewed defaults.

## Scope

- **In**: completed active-spectrum/default/WORKERFS work; settings-tab expansion; Run/Recalculate/download overlay startup; blockwise ENVI binary materialization into the final spectra matrix; direct ENVI-pair streaming from ZIP to avoid wasm extraction copies; synchronized pipeline diagram.
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
- R8. The mount is an ingestion optimization, not a file-backed analysis mode. It must support multiple companion files and ZIP input, use a session-scoped unguessable read-only mountpoint, reject traversal/name collisions, and unmount on replacement/session end. Render exactly one mode-appropriate picker: native `fileInput()` locally and WORKERFS for every hosted selection, with no hosted native fallback. Keep hosted guidance/status text out of the upload column; report Mount and Read/Materialize stages in the central progress popup and capability/runtime failures as persistent alerts. Preserve axes, spectra/metadata alignment, IDs, attributes, particles, SNR, matches, and exports exactly.
- R9. Clicking any visible analysis tab expands the settings card and activates that tab in one action. Run, Recalculate Preview, and downloads schedule the accessible central overlay from the browser click, retain the 650 ms anti-flash delay, and stay synchronized with real phase/idle or download completion.
- R10. Default `read_envi()` reads BIP/BIL/BSQ blocks directly into the final band-by-pixel matrix. Its signature and output dimensions, ordering, values, metadata, coordinates, names, IDs, attributes, and spectral-smoothing behavior remain unchanged.
- R11. Cache the verified wasm dependency repository across local rehearsals and GitHub Actions using dependency-defining inputs plus the pinned webR image as the compatibility key. Evict the local `OpenSpecy` package before every reuse, refresh changed dependency versions, rebuild the VFS image, and retain exact commit/artifact validation so a same-version new commit cannot be skipped.
- R12. Treat the 10 GiB picker ceiling separately from the pinned webR runtime's compiled 2 GiB heap. For an ENVI-only ZIP, stream the HDR and binary members into the same blockwise reader without extracting the full DAT into MEMFS; preserve ordinary `read_zip()` output and cleanup behavior for every other archive layout.

## Technical Decisions

- **Approach**: keep `canonical_final()` as the sole source for summaries, identification, quantification, metadata, and downloads. Add one deliberately named view-only active-spectrum lane for inspection/quality, sourced from the canonical selected unit or a one-column processed clicked-pixel fallback. Hosted mounting changes only how source paths enter `read_any()`; the complete object then follows the ordinary pipeline with no `FileSpecs` branch or processing-mode flag.
- **ENVI memory path**: retain the legacy caTools array route only when `spectral_smooth=TRUE`; the default path allocates the final spectra matrix once and fills bounded BIP/BIL/BSQ blocks, avoiding two whole-array permutations while producing the same object.
- **Hosted ZIP memory path**: the pinned wasm heap limit is compile-time, not an app upload option. Prefer removing the avoidable uncompressed-DAT MEMFS copy by reading ENVI ZIP members through sequential connections; do not patch generated webR assets or claim the 10 GiB picker cap is allocatable analysis memory.
- **Public API**: no new arguments or exports. The reviewed changes are defaults for existing meaningful-policy arguments (`silent_region`, `artifact_ratio`); explicit values and base-pipe composition remain unchanged. Document scientific/user-visible consequences and boundary behavior.
- **Dependencies**: no new R dependency. The pinned webR 0.6.0 WORKERFS wrapper accepts package metadata, not its documented direct-file form: preserve the selected browser `File` objects through structured cloning, expose them as one composite Blob plus member offsets, and call `WebR.FS.mount("WORKERFS", {packages}, mountpoint)`. The deterministic adapter must fail closed on a Shinylive asset/hash mismatch rather than editing generated output by hand.
- **Mounted text compatibility**: webR is a 32-bit process and `fread(filename)` cannot mmap WORKERFS files. Direct mounted CSV/TSV/TXT/XYZ inputs therefore enter the same `read_text()` parser through `fread(text=...)`; binary/ENVI paths retain their existing readers and the returned `OpenSpecy` structure is unchanged.
- **OpenSpecy contract**: the inspection object is one aligned `OpenSpecy`; canonical/export objects remain unchanged. Mounted and native inputs must produce equal fully materialized objects before downstream functions run.
- **Bundled Shiny app**: owner controls remain Run-gated. Upload UI is mode-exclusive, while both server paths still converge on the same reader. Quality follows selection without recomputing the full map or library. Verify no-upload, processed, threshold on/off, collapsed retained/rejected, identified, batch, and large-map progress/error states; assets unchanged.
- **Pipeline diagram**: `.specify/memory/pipeline-diagram.html` shows local temporary files and hosted WORKERFS mounts converging at “Read & Materialize”; it omits the removed memory-prediction/block branch, routes Heatmap selection through “Active Spectrum Inspection” and “Quality Findings,” and retains dataset “Signal/Noise Compute” only for thresholding/histogram.
- **Hosted Shinylive/WebAssembly app**: use the inner-frame WORKERFS picker/handshake and reproducible outer-shell mount adapter owned by `tools/wasm/`; do not render `fileInput()` or hand-edit `_site`. Current evidence shows the pinned native bridge repeatedly concatenates the entire HTTP body and creates an R raw copy, so every hosted selection must bypass that request. Preserve `/`, `/app/`, `/pkgdown/`, package/dependency pins, closure, and staged libraries; exact-artifact tests must prove single-control rendering, popup progress, mount, read, replacement cleanup, an ordinary small file, and all existing app journeys.

## Package Surfaces

- `R/`: `read_envi.R` changes internally because the mounted large-file run exposed avoidable array/permutation copies; public arguments and returned format are unchanged. `FileSpecs` remains unchanged and unused by the app.
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
- [x] Implement the session-scoped hosted mount handshake and deterministic pinned-asset adapter; converge on ordinary `read_any()` and a fully in-memory `OpenSpecy`, with cleanup and no hosted native fallback.
- [x] Reconcile the already-updated target diagram with final function names/source locations, remove its target-status note, then run focused, benchmark, package, app-browser, and hosted tiers once.
- [x] Make analysis tab clicks expand before tab activation and start sustained-work feedback client-side for Run, Recalculate, and local/hosted downloads.
- [x] Replace default ENVI array permutations with bounded direct-to-final-matrix reads; add all-interleave invariants and the former path to the repeated benchmark.
- [x] Update the Read & Materialize diagram detail; run focused tests/benchmark, full package tests, bundled browser smoke, fast hosted gate, and matching-artifact preflight.
- [x] Seed and verify the wasm dependency-repository cache; compare an uncached closure build with a cached exact-artifact rerun and retain the clean SHA checks.
- [x] Reproduce the genuine mounted ZIP locally against the action-equivalent hosted app and capture the exact failure phase/message.
- [ ] Prove the extracted HDR+DAT pair can materialize in the same pinned runtime and add multi-file genuine-map smoke support.
- [ ] Stream ENVI-only ZIP members without full extraction; prove small ZIP parity, package tests/benchmark, and genuine hosted materialization.

## Verification

- **Direct/scientific**: selected retained/rejected collapsed pixels; threshold on/off identical active trace and non-threshold QC; finite SNR; non-flat range; silent-region boundaries; artifact ratios below/equal/above 2; explicit 3x override unchanged.
- **Performance/parity**: verify zero full-file HTTP/R-raw upload copy on the mounted route; repeated small native-vs-mounted object/output equality with <=10% downstream regression; genuine ZIP completes or records the actual browser/runtime limit within 30 minutes; direct HDR+DAT is a diagnostic comparator, not a different analysis mode.
- **Follow-up parity**: compare the blockwise reader byte-for-byte with the former caTools/aperm result for BIP/BIL/BSQ fixtures and the genuine ZIP signature; browser assertions must observe card expansion and the overlay itself for all three action types.
- **Hosted-memory follow-up**: the action-equivalent browser must mount the genuine ZIP, fully materialize 427 x 331,180 spectra, and enable Run without `cannot allocate vector`; the extracted pair is the same-runtime diagnostic control.
- **App/browser**: focused `run_app|assess_spec|adj_range` tests, bundled static gate, local native upload plus hosted WORKERFS small/multi-file/ZIP journeys, selection/collapse/QC/progress, console and desktop/mobile screenshots. The 500 MB journey is manual/CI-guarded and never routine.
- **Docs/package**: confirm roxygen2 8.0.0, regenerate/inspect generated diffs, run full `devtools::test()` and staged `R CMD check` once because exported scientific defaults change.
- **Hosted**: `-HostedAppStatic`; clean-commit wasm build because package image changes; exact matching-artifact action preflight and nested-frame smoke. Reuse current green workflow syntax/permissions only; R/app runtime evidence is invalidated.

## Risks And Open Questions

- WORKERFS avoids copying selected bytes through Shinylive's request bridge but does not reduce the 1.054 GiB R matrix or later processing copies; ZIP extraction may still add the 0.527 GiB DAT. If the exact mounted ZIP still cannot finish, report the measured phase and recommend selecting the extracted HDR+DAT pair, without restoring a forecast or changing downstream functions.
- Post-push reproduction: the prior matching app mounted the genuine ZIP then failed after 110.8 seconds with `cannot allocate vector of size 1.1 Gb`; the streamed-reader wasm candidate `0a57691` preserved exact desktop output (25.84 seconds) but the same hosted allocation failed after 1.4 seconds, while mounted HDR+DAT stayed busy and timed out at 30 minutes. The pinned `R.js` hardcodes a 2,147,483,648-byte heap and R's vector-size default is already unlimited, so a higher limit requires rebuilding/forking webR in 4 GiB or memory64 mode rather than changing the 10 GiB picker or an app option.
- Local evidence for SHA-256 `4931f855…d59b6d50`: the 521,106,125-byte ZIP materialized to 1,157.239 MiB (427 x 331,180) in 30.15 s; direct HDR+DAT extraction/read took 7.17 + 22.48 s and produced an equal compact signature. Small repeated mounted-path reads were 0.09-0.12 s versus 0.13-0.14 s with a native-copy proxy.
- Exact hosted evidence: clean candidate `600f72c` restored and verified the pinned 117-package repository in 522.6 seconds after an app-only update (uncached baseline 3,372.4 seconds; first cached sample 598.7 seconds). Its action-equivalent assembly, library smoke, routes, mounted CSV/ENVI/ZIP-map analysis, progress overlay, and downloads passed; the nested-frame browser journey took 3.1 minutes. The mounted-text regression found during preflight was `fread(filename)` attempting an unsupported 32-bit WORKERFS mmap and is now routed through the equivalent text parser.
- Shinylive does not expose a public app-level mount hook. The exact pinned asset must first prove a narrow message bridge can safely reach its webR proxy. If that prototype fails, stop for an upstream/fork decision rather than silently returning to FileSpecs or shipping a brittle generated-file edit.

## Approval Notes

- Approved by: user request, 2026-08-21.
- Follow-up: implementation must apply `openspecy-develop-shiny-app`, `openspecy-design-public-api`, generated-doc, quality-gate, and hosted verification skills; no push is authorized by this plan.
