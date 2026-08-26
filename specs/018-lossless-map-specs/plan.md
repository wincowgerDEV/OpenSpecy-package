# Feature Plan: Compact Map Specs With Background Suppression

**Feature dir**: `specs/018-lossless-map-specs`  
**Date**: 2026-08-25  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Prove compact-map feasibility; implement exact and background-suppressed `Specs`; keep PCA/K-means/Hilbert full-object transforms compact; route local direct paths and hosted WORKERFS paths through one Run-gated app pipeline.
**Change class**: Mixed; highest hosted/release because scientific APIs, dependencies, app ingestion, and the pinned wasm package change.

## Goal

- Keep large maps fully in memory without repeating metadata or coordinates; optionally map configured background to a virtual zero value after the user-selected smoothing/S/N workflow.
- Preserve canonical `OpenSpecy`, foreground scientific meaning, full-dataset PCA/K-means multiplicity, indexed inspection, provenance, exports, and one app pipeline without ever expanding compact pixels.

## Scope

- **In**: Genuine-map profiling; lossless compact grids/metadata; optional batch smoothing/S/N suppression; `Specs` version/accessors; base H5/ENVI/ZIP compact readers; weighted full-object PCA/K-means and foreground Hilbert; local direct-path picker; hosted WORKERFS; app methods/docs/benchmarks/gates.
- **Out**: Spectral deduplication; changing `OpenSpecy`; FileSpecs; reader arguments for PCA/K-means/Hilbert; custom webR heaps; silent source deletion; ordinary non-map compaction by default.
- **Users**: Package users explicitly choose compact/background operations; the app derives them from committed controls. Local and hosted users see exactly one mode-appropriate file picker.

## Requirements

- R1. Profile the private 500 MB ZIP before structural work: exact unique spectra, background/foreground counts under representative controls, metadata cardinalities, grid regularity, projected bytes, read time, and peak RSS. Never copy, commit, or upload it. Stop before app rewrites if neither exact nor filtered projection credibly fits below the pinned 2 GiB heap.
- R2. Compact grids and metadata are lossless: store region geometry/traversal, constants, dictionaries, and required explicit exceptions while preserving order, IDs, NA/NaN, values, axis, metadata, and attributes. The genuine map has zero exact spectral duplicates, so no spectral deduplication step or hashing cost is added.
- R3. `background` is an optional `as_Specs()` step and streaming reader transformation. A non-`NULL` filter policy applies configured 3-D Gaussian smoothing only when requested, computes the chosen `sig_noise()` metric, and suppresses non-finite or strict-min/max failures. If thresholding is off, do not suppress; if smoothing is off, classify unsmoothed values.
- R4. Reserve source/value index `0` as the authoritative background sentinel; foreground indices start at 1. Preserve source S/N, eligibility, below/above/non-finite reason, identity, and filter settings. Indexed raw decompression of index 0 is always an exact wavenumber-length zero line labeled transformed/background-suppressed.
- R5. Full-band spatial tiles/regions use halos derived from Gaussian kernel support. Small BIP/BIL/BSQ and H5 fixtures must match whole-cube `mmand::gaussianSmooth()` and `sig_noise()` at borders/seams within stated tolerance with identical masks.
- R6. Regular grids use per-region dimensions, origin, axes/spacing, and traversal; irregular grids use compact explicit vectors. Metadata columns use constants/dictionaries. Indexed access does not materialize unrelated rows.
- R7. `Specs` accepts legacy dense 0.1 plus compact exact/filtered objects, bumps its format version, validates mappings/descriptors/provenance, round-trips through `write_specs()`/`read_specs()`, and never silently makes a lossless object lossy.
- R8. H5/ENVI readers add only `representation = c("OpenSpecy","Specs")` and `background_filter = NULL`, forwarded by `read_zip()`/`read_any()`; defaults remain `OpenSpecy`. Do not expose `steps`, PCA, K-means, or Hilbert tuning on readers. `specs_background_filter(metric, minimum, maximum, sigma=NULL, step)` is the reusable validated policy.
- R9. Compact readers stream/build the complete base `Specs` without dense `OpenSpecy`, retaining every foreground spectrum exactly once. PCA/K-means/Hilbert run afterward only through `as_Specs()` on the fully read in-memory compact object and append ordered transformation records.
- R10. PCA excludes mapping 0 and uses foreground multiplicity weights from the source mapping to calculate weighted centering/scaling/covariance/SVD equivalent to repeated full pixels without expansion. Background PCA scores are exposed as all zero; inverse reconstruction checks the mask and returns exact spectral zero, not the PCA center.
- R11. K-means excludes background, uses source multiplicities in its objective/centroid updates, and reserves class 0 for background while foreground classes are 1..k. Hilbert fits limits/encodes foreground values only and exposes background code 0. A separate mask disambiguates legitimate foreground zero scores/codes.
- R12. Transformation order is validated: `background` must precede PCA/K-means/Hilbert while physical pixel spectra exist; supported PCA/K-means/Hilbert orders remain as currently documented. Mapping/weight composition, not row expansion, carries full-source multiplicity through stacked transforms.
- R13. Extend only demonstrated app operations to compact `Specs`: NA handling, corrections/ranges, S/N, features/collapse, matching/AI, quantification, map projection, metadata, and downloads. Coordinate-dependent work uses indexes and updates compact descriptors.
- R14. App staging snapshots actual controls: Threshold S/N off means no `background`; Spatial Smooth off means no smoothing; selected metric/bounds/basis/sigma are recorded and used once. Fully Processed basis runs the committed processing on compact foreground before classification without dense expansion. Muted controls do no work.
- R15. Upload/mount only stages source paths/header. Click Run owns materialization/transforms; Recalculate Preview scans the same tiles but retains only S/N results. Changed settings mark results stale and reread the untouched source. Suppressed clicks show explicit provenance and bypass misleading flat/SNR-unavailable warnings.
- R16. Replace local `fileInput()` with one local-only server-filesystem picker using `shinyFiles` (or an equivalently verified maintained API): multiple selections resolve to normalized read-only real paths and flow directly to readers without Shiny temp copies. Restrict roots to configured/local volumes, reject traversal/non-files, and preserve companion HDR/DAT selection. Shinylive renders only its existing WORKERFS picker and never loads this local dependency.
- R17. One canonical `Specs` map or `OpenSpecy` single/collapsed object feeds processed, identified/model, quantified, map, metadata, inspection, and download states. Large exports stream or remain compact RDS; transformed outputs are labeled and never presented as raw.
- R18. Acceptance requires the exact action-built app to process the genuine ZIP, report counts, select foreground/background, identify a representative spectrum, and download verified output without severe console or wasm allocation errors; local acceptance proves the selected source path is unchanged and no temporary upload copy was created.

## Technical Decisions

- **Approach**: `Specs(variables, values, coords, metadata)` stores every retained foreground value plus compact source mapping/grid/dictionaries; mapping 0 is virtual background. Weighted PCA/K-means consume `values` plus mapping counts; Hilbert is pointwise. Each transform returns a compact mapping/model without unraveling.
- **Public API**: Readers own only return representation and optional streaming background policy. `as_Specs(x, steps=..., background_filter=..., ...)` owns ordered full-object transforms and their existing tuning; export the filter builder and indexed coordinate/metadata accessors. Example: `read_envi(..., representation="Specs", background_filter=policy) |> as_Specs(steps=c("pca","kmeans","hilbert"), ...)`.
- **Scientific behavior**: Exact mode is lossless. Background suppression and PCA/K-means/Hilbert retain distinct provenance/tolerance. Small expanded oracles must show weighted PCA/K-means agree with full pixel multiplicity; sentinel zero is never fitted, clustered, or interpreted as measured flat data.
- **Constitution/generated artifacts**: Before app switching, use `speckit-constitution` to allow one canonical spectral object. Update roxygen/source metadata first, verify configured roxygen2, regenerate/inspect `NAMESPACE`/`man`, and never edit generated web/wasm output.
- **Bundled app/diagram**: Keep owner gating, central elapsed progress, native downloads, and no new data assets. Update source staging, local direct/hosted mount branches, batch formatting, full-object transforms, preview/S/N, canonical state, inspection, metadata, and download dependencies. Apply `openspecy-develop-shiny-app`.
- **Hosted impact**: `R/`, `DESCRIPTION`, and `inst/shiny/` change: run fast `-HostedAppStatic`, exact-artifact preflight, and one release-facing clean rebuild. Preserve `/`, `/app/`, `/pkgdown/`, pins, closure, small libraries, and generated boundaries; exclude local-only `shinyFiles` from hosted runtime roots.

## Package Surfaces

- `R/`: `Specs*.R`, readers, `sig_noise.R`, required methods, `run_app.R`; `OpenSpecy` unchanged.
- Tests/benchmarks: invariants/migration, tile seams, setting matrix, sentinel 0, legal orders, weighted compact-versus-expanded PCA/K-means, Hilbert/background, readers, local path security/no-copy, app states/downloads, hosted contracts; repeated bytes/RSS/runtime and >10% same-output slowdown flags.
- App/docs/metadata: `inst/shiny/{global,server,ui}.R`, browser fixtures, pipeline diagram, roxygen, vignette/README, `NEWS.md`, generated docs; add guarded `shinyFiles` metadata. No site-route or deployment-source change expected.

## Work Checklist

- [x] Profile genuine exact/background feasibility and false-background spot checks.
- [x] Amend constitution; implement/version compact mapping 0, dictionaries/accessors, filter policy, tile/halo reader, and invariants; omit spectral deduplication after the zero-duplicate profile.
- [x] Implement weighted no-expansion PCA/K-means, foreground Hilbert, stacked mapping/provenance, and expanded-oracle tests.
- [x] Replace local upload with secured direct-path selection; stage hosted/local sources and route Run/Preview/outputs through one pipeline; synchronize diagram.
- [x] Update dependencies/guidance/roxygen/NEWS/benchmarks/docs and genuine transformed downloads.
- [x] Run focused/full/check/app/static gates, exact preflight, and final clean rebuild with private ZIP acceptance.

## Verification And Risks

- Focused: `Specs|read_envi|read_ext|read_multi|sig_noise|spatial_smooth|process_spec|match_spec|run_app|shinylive_wasm`; app parse and `-HostedAppStatic`. Final: configured document, full test/check, installed/browser state matrix, genuine downloads, console/screenshots, exact preflight/rebuild.
- The genuine 331,180 x 427 map has no exact duplicate spectra: dense spectra are 1,099 MiB and exact compact projection is 1,080 MiB. S/N > 4 retains 20,155 raw or 23,001 sigma-1-smoothed pixels, projecting to 67/76 MiB; near-threshold spots include both retained/rejected cases. Dense profiling peaked at 10.7 GiB. Batch seams and weighted K-means equivalence remain risks.

## Approval Notes

- Approved by:
- Follow-up: Reconsider custom 4 GiB/memory64 only if validated compact workflows still cannot fit.
