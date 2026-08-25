# Feature Plan: Compact Map Specs With Background Suppression

**Feature dir**: `specs/018-lossless-map-specs`  
**Date**: 2026-08-25  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Prove compact-map feasibility, add exact `Specs` deduplication plus optional batch-smoothed background suppression, then route H5/ENVI maps through it in the bundled and hosted app.  
**Change class**: Mixed; highest is hosted/release because public scientific APIs, object flow, app behavior, and the action-built wasm package change.

## Goal

- Keep large maps fully in memory by storing repeated spectra/metadata/coordinates once and, when explicitly committed, mapping batch-smoothed background pixels to one zero spectrum before they consume dense R memory.
- Preserve canonical `OpenSpecy`; make `Specs` either exactly reconstructable or clearly marked as background-suppressed, with indexed processing, inspection, provenance, and exports.

## Scope

- **In**: Genuine-map profiling; exact spectral deduplication; compact grids/metadata; halo-aware batch smoothing; configured S/N background suppression; `Specs` version/accessors; direct H5/ENVI/ZIP readers; app-required methods; Run-gated local/Shinylive routing; docs, benchmarks, and hosted verification.
- **Out**: Changing the three-part `OpenSpecy` contract; FileSpecs; approximate foreground deduplication; custom webR heaps; early suppression for Fully Processed S/N basis; silent deletion of source pixels; non-map formats by default.
- **Users**: Package users request a compact representation/filter explicitly; the app derives it from committed H5/ENVI map settings without another upload control.

## Requirements

- R1. Profile the private 500 MB ZIP before structural work: exact unique spectra, background/foreground counts under representative configured filters, metadata cardinalities, grid regularity, projected compact bytes, read time, and peak RSS. Never copy, commit, or upload it. Stop before app rewrites if neither exact nor filtered projection can credibly fit with the pinned runtime below 2 GiB.
- R2. Exact `deduplicate` remains independently available: store each bit-identical column once with an integer source-to-value mapping. Hashes only index candidates; verified equality makes collisions harmless. Exact mode reconstructs ordering, IDs, NA/NaN, values, metadata, wavenumbers, and attributes.
- R3. Add `background` as an explicit optional `Specs` step. A non-`NULL` filter policy supplies its inputs: stream full-band spatial tiles/regions, apply configured 3-D Gaussian smoothing, compute the chosen `sig_noise()` metric, then suppress non-finite or strict-min/max failures. It must precede PCA, K-means, or Hilbert because those remove original per-pixel spectral/coordinate meaning; supported later steps can stack in their existing legal order.
- R4. Every suppressed source remains but points to one correctly sized zero sentinel. Exclude that sentinel from PCA fitting and K-means assignment so indexed decompression always returns exact zero; apply later transforms only to retained smoothed foreground, then deduplicate. Preserve source S/N, eligibility/reason, and identity.
- R5. Tiles include spatial halos derived from the actual Gaussian kernel support; discard halos only after smoothing. Small BIP/BIL/BSQ and H5 fixtures must match whole-cube `mmand::gaussianSmooth()` and `sig_noise()` at borders and tile seams within a stated floating tolerance, with identical masks.
- R6. Regular complete grids use per-region dimensions, origin, spacing/axes, and traversal order; irregular grids fall back to compact explicit vectors. Repeated metadata uses constants or dictionaries. Indexed coordinate/metadata access must not materialize all rows.
- R7. `Specs` accepts legacy dense 0.1 objects and compact exact/filtered objects, bumps its format version, validates mappings/scopes/descriptors/provenance, round-trips through `write_specs()`/`read_specs()`, and never silently upgrades a lossless object to lossy.
- R8. Add `representation = c("OpenSpecy", "Specs")`, ordered `steps`, and `background_filter = NULL` to H5/ENVI routes through `read_zip()`/`read_any()`. Defaults remain `OpenSpecy`. `specs_background_filter(metric, minimum, maximum, sigma, step)` creates validated policy; input presence replaces a boolean. PCA/K-means tuning passes to the existing `as_Specs()` owners rather than duplicating reader arguments.
- R9. Compact readers build `Specs` incrementally without dense `OpenSpecy`, then use the shared ordered-step engine. Exact mode preserves ordinary-reader output; filtered mode records algorithm/version, basis, metric, bounds, step, sigma, boundary policy, counts, source fingerprint, and subsequent transforms.
- R10. Extend only demonstrated app operations to `Specs`: NA handling, processing/corrections, ranges, S/N, features/collapse, matching/AI, quantification, map projection, metadata, and native downloads. Columnwise work runs per unique value; coordinate-dependent work uses indexes and re-deduplicates.
- R11. Upload/mount performs path, header, dimensions, and readability validation only. Click Run snapshots settings and owns batch smoothing/filtering/compaction. The early suppression path requires existing Threshold S/N and Spatial Smooth owners on with Raw / Spatially Smoothed basis; otherwise use exact compact reading and existing later threshold semantics. Muted settings do no work.
- R12. Recalculate Preview uses the same tile/halo/S/N algorithm to produce a histogram without retaining a second map; it shows central staged progress and marks the committed object stale. Changing filter metric, bounds, sigma, or basis requires another Run from the still-mounted/native source.
- R13. One Run-gated canonical object feeds all results: compact `Specs` for an uncollapsed map and `OpenSpecy` for a single/collapsed result. Indexed `decompress_spec(index=)` produces Active Spectrum. A suppressed click shows the zero line plus “background-suppressed” provenance and bypasses misleading flat-spectrum/SNR-unavailable warnings.
- R14. Existing processed, collapsed, identified/model, quantified, map, metadata, and download states remain aligned. Large exports stream or retain compact RDS; filtered exports are labeled transformed data and never presented as raw measurements.
- R15. Acceptance requires the exact action-built app to load the genuine ZIP, report suppression/unique counts, select foreground/background pixels, complete representative Run/identification, and download a verified result without severe console errors or wasm allocation failure.

## Technical Decisions

- **Approach**: Evolve `Specs(variables, values, coords, metadata)` with compact mappings/dictionaries, a protected zero sentinel, and ordered transformation records. Dense components remain valid. Exact deduplication and lossy `background` are distinct steps; PCA/K-means/Hilbert operate only on foreground afterward.
- **Public API**: `representation`, ordered `steps`, and optional `background_filter` are demonstrated policies. Export its validated builder and indexed coordinate/metadata accessors; keep hashing/tiling/halo/dictionaries internal. Example: `read_envi(..., representation="Specs", steps=c("background","deduplicate","pca","kmeans"), background_filter=specs_background_filter(...), ...)`.
- **Scientific behavior**: Suppression is never default in package readers. In the app, the existing Threshold S/N owner, metric/bounds, Raw/Smoothed basis, and Spatial Smooth owner/sigma visibly define it. The foreground stored in filtered `Specs` is smoothed once; downstream spatial smoothing must recognize provenance and not run twice.
- **Constitution/generated artifacts**: Before the app switch, use `speckit-constitution` to permit one canonical spectral object (`OpenSpecy` or `Specs`) while retaining the single-pipeline rule. Update roxygen first, verify configured roxygen2, regenerate/inspect `NAMESPACE` and `man`, and never edit generated web/wasm output.
- **Bundled app/pipeline diagram**: Keep one picker, owner/child gating, central elapsed progress, native downloads, and no new assets. Update “Read & Lossless Compact” into upload-time source staging, insert Run-gated “Batch Smooth → S/N → Suppress → Deduplicate,” and revise Spatial Smooth, preview, S/N, canonical state, inspection, metadata, and download dependencies.
- **Hosted app**: Shared `R/` and `inst/shiny/` inputs require fast `-HostedAppStatic`; changed runtime/interactions require exact-artifact preflight. The release-facing genuine-file target requires one final clean-commit wasm rebuild. Preserve `/`, `/app/`, `/pkgdown/`, pins, closure, staged small libraries, and generated boundaries.

## Package Surfaces

- `R/`: `Specs.R`, `Specs_methods.R`, readers, `sig_noise.R`, and only required processing/accessor/writer methods; `OpenSpecy` unchanged.
- `tests/testthat/`: Exact/filtered invariants, legal/illegal step orders, zero-sentinel exclusion from PCA/K-means, legacy migration, halo seams, masks/provenance, indexed reconstruction, app states/downloads, and hosted contracts.
- `benchmarks/`: Repeated dense versus exact versus filtered read/process/match/write; equivalence/tolerance, suppression accuracy, object/RDS bytes, peak RSS, and slowdown guards; genuine path opt-in.
- `inst/`: `inst/shiny/{global.R,server.R,ui.R}` guidance and browser fixtures; asset inventory/size unchanged.
- Docs/metadata: Update roxygen, vignette/README, `NEWS.md`, and generated docs; `DESCRIPTION` unchanged unless proven otherwise. `workflows/`, deployment workflow source, and `site/` routes remain unchanged.

## Work Checklist

- [ ] Profile exact duplicates and smoothed S/N suppression on the genuine map; record feasibility and false-background spot checks before APIs/app work.
- [ ] Amend constitution; implement/version compact components, filter policy, tiling/halo pipeline, accessors, provenance, and focused tests.
- [ ] Implement direct H5/ENVI/ZIP exact/filtered readers and compare whole-cube versus tiled values/masks plus dense exact signatures.
- [ ] Add demonstrated `Specs` methods; stage sources on upload and route Run/Preview/canonical outputs through one compact pipeline; synchronize the diagram.
- [ ] Update guidance, roxygen, NEWS, benchmarks, generated docs, and genuine transformed downloads.
- [ ] Run focused/package/app gates, exact-artifact preflight, and final clean wasm rebuild with the private ZIP acceptance journey.

## Verification

- Focused: `Specs|read_envi|read_ext|read_multi|sig_noise|spatial_smooth|process_spec|def_features|match_spec|run_app|shinylive_wasm`; parse app sources and run fast `-HostedAppStatic`.
- Scientific/performance: tile/full smoothing tolerance and identical masks; exact-mode equality; filtered foreground/mask/provenance; selected suppressed/retained pixels; false-background review; peak RSS, bytes, repeated runtime, and >10% same-output slowdown flag.
- Final: configured `devtools::document()`, full `devtools::test()`, release-facing `devtools::check()`, installed-app smoke, affected states/genuine downloads, console/screenshots, exact preflight, and clean rebuild. Prior hosted artifacts are invalidated by package/app flow changes.

## Risks And Open Questions

- The dense spectra are about 1,078.9 MiB versus 78.3 MiB of other object data; success depends on foreground/suppressed proportions. Unique foreground may still exceed webR memory.
- Batch smoothing must not create seam-dependent particle edges. False background is scientifically irreversible inside the compact object; recovery requires rerunning the untouched source with revised settings.
- Fully Processed S/N early suppression is deferred because it would move the entire configurable preprocessing pipeline into the reader; it retains exact compact reading and post-read thresholding in this tranche.

## Approval Notes

- Approved by:
- Follow-up: Reconsider a custom 4 GiB/memory64 runtime only if validated filtered compaction still cannot fit.
