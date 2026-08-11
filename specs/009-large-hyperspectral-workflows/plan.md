# Feature Plan: File-Backed Specs And Large Hyperspectral Particle Workflows

**Feature dir**: `specs/009-large-hyperspectral-workflows`
**Date**: 2026-08-10
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.
**Current tranche**: Establish the file-backed Specs contract, then implement one bounded particle-analysis pipeline and the related H5, app, metadata, and plot repairs.
**Change class**: Mixed; highest class is hosted/release because shared bundled-app code, package/scientific readers, public S3 APIs, and the generated Shinylive app are affected.
**Status**: Option A and the Specs-first boundary are approved; numerical and performance budgets remain provisional until measured.

## Goal

- Make `Specs` the experimental 1-10 GB analysis boundary while leaving established `OpenSpecy` behavior unchanged for ordinary in-memory work.
- Analyze large H5/ENVI maps with bounded memory, correct multi-region registration, immutable sources, and a deliberately limited first particle workflow.

## Scope

- **In**: File-backed Specs and versioned cache; local upload bypass/raster viewer; H5 mosaic repair; region-aware particle automation; Test Map metadata repair; durable plots and threshold diagnostics.
- **Out**: Rewriting every `OpenSpecy` function; arbitrary file-backed operation chains; full whole-map correlation matrices; in-place source edits; PCA/K-means fitting, median/custom collapse, or approximate science in the first contract; committing either external prototype.
- **Compatibility**: Existing four-component, matrix-backed `Specs` objects, `.rds` serialization, methods, and `OpenSpecy` inputs/outputs retain their current contracts.

## Requirements

- R1. Benchmark read-only `drop.h5` (3.10 GB, three regions) and `dropRegion1.rds` (873 MB) from environment-configured paths. Record open/index time, peak memory, I/O/cache bytes, preview/selection latency, pipeline passes, warm reuse, and eager-subset equivalence.
- R2. Add `FileSpecs`, an S3 `Specs` subtype whose serialized state contains source/layout, axis, coordinate/metadata, cache, recipe, generation, and fingerprint descriptors, never a live HDF5/file handle or a full values matrix.
- R3. H5 uses short-lived read-only hyperslabs. ENVI resolves its `.hdr` plus `.dat`/`.img` pair and reads binary windows from header-defined dimensions, offset, data type, byte order, and BSQ/BIL/BIP interleave; export atomically creates a new pair. No path may modify, replace, or append to an input.
- R4. Size/mtime and strong fingerprints for every source member identify a version. Copy-on-write generations use a separate cache root and atomic publication; errors/cancellation remove only owned partials. Changed/missing sources detach or error, never reuse stale results.
- R5. Small local upload remains. Large local inputs use a safe server-side path opener, avoiding a browser copy. Hosted mode rejects oversize files, explains its measured limit, and directs users locally; browser filesystems are out of scope.
- R6. Large spatial views use bounded server raster/Canvas tiles and exact region-of-interest selection; Plotly is retained only for bounded selected spectra. Progress reports indexing, analysis passes, bytes, elapsed time, cancellation, cleanup, and cache reuse.
- R7. Provisional gates: preview without full-cube read under 10 seconds; peak memory at most 2 GB above baseline; viewport at most 2 MB; cached selection under 200 ms; uncached spectrum under 1 second; documented numerical equivalence; no same-output regression over 10%.
- R8. `read_h5()` and the H5 adapter retain region, local row/column, stage coordinates/units, unique pixel IDs, and every intersecting mosaic tile. Tested region transforms align spectra, total-signal/material overlays, and visual pixels.
- R9. `automate_particle_analysis.FileSpecs()` initially supports region-sequential approved S/N, mean collapse, named exact preprocessing, and exact Top 1/Top N matching. Unsupported combinations fail before work without eager fallback.
- R10. Region iteration is inferred, requiring no manual loop. `split_spec.FileSpecs(by = "region")` returns lightweight views; bounded `decompress_spec()` creates `OpenSpecy` only for an explicit index, region, ROI, or export.
- R11. The genuine 208-spectrum Test Map Uploaded Metadata tab renders and selects matching spectra locally and hosted without DT/sidebar lifecycle, server, or console errors.
- R12. Particle results retain their list contract and replay through `plot()`. S/N and correlation histograms show threshold lines; `particle_heatmap` uses a continuous finite min-max signal legend while its mask remains binary.

## Technical Decisions

- **Selected architecture**: Option A is local-first, H5/ENVI-source-backed `FileSpecs`; `dropRegion1.rds` is an eager comparison oracle, not a new large-data backend.
- **S3 boundary**: Keep `Specs()` and `read_specs()` unchanged; add `open_specs(path, cache_dir)`. Make `check_Specs()` and `write_specs()` generic with legacy methods. Every existing `Specs` entry point gets a `FileSpecs` method or guarded error, preventing matrix fallback.
- **Connection lifetime**: Reopen read-only handles from durable descriptors per operation/session; close with `on.exit()` and Shiny cleanup. Saved objects reconnect after fingerprint validation.
- **Version safety**: Cache keys include source hash/schema, adapter and algorithm versions, canonical recipe, and library/model IDs. Temp-to-final commits, manifest-last completion, locking, and cache-root containment prevent partial/concurrent corruption; completed generations are immutable.
- **Pipeline**: (1) index regions/coordinates/images; (2) stream range and per-spectrum S/N blocks; (3) smooth only the small per-region scalar raster where requested; (4) label/filter complete regional masks; (5) reread retained pixels and compute exact running means; (6) materialize the small collapsed object; (7) reuse established in-memory preprocessing/matching; (8) join cached scalars/features into plots, tables, and downloads.
- **Algorithm limits**: Spectral per-spectrum smoothing is block-exact. Finite spatial smoothing requires region-bounded halos and an equivalence spike. Mean/sum are streaming-exact; median/custom functions need later disk-spill work. Full correlation is rejected; exact blockwise Top N or a small post-collapse matrix is allowed. PCA/K-means fitting and data-derived entropy breaks are deferred.
- **App state**: Large mode deliberately uses canonical `final_specs()` for the full recipe and `final_selection()` for its bounded `OpenSpecy` materialization; every selected plot, match, metadata table, quantification, and download derives from the latter, while full-map products derive from the former generation.
- **Hosted contract**: Preserve generated `/`, `/app/`, and `/pkgdown/`, immutable pins/closure, small libraries, and ignored artifacts. Hosted continues the small in-memory path plus Test Map fix and clearly reports the local-only large-data capability.

## Package Surfaces

- `R/Specs.R`, `R/Specs_methods.R`, new `R/Specs_file.R`: legacy compatibility, opener/descriptors, validation/write dispatch, fingerprints, generations, locks, bounded materialization, region views, and guarded S3 methods.
- `R/read_ext.R`, `R/read_envi.R`, `R/visual_image.R`, `R/match_spec.R`, `R/automate_particle_analysis.R`, `R/particle_image.R`: reusable H5/ENVI window readers, transforms, and the FileSpecs worker without whole-source decompression.
- `tests/testthat/`: Preserve legacy Specs/OpenSpecy/ENVI tests; add tiny H5 multi-region/multi-tile and equivalent ENVI interleave fixtures plus focused file-backed, automation, metadata, plotting, safety, cancellation, serialization, and concurrency regressions.
- `benchmarks/`: Retain eager comparison code; extend particle-reader benchmarks with repeated open/index/select/pass/cache measurements and guarded external-file runs.
- `workflows/automate_lab.R`: Demonstrate `open_specs("drop.h5") |> automate_particle_analysis(..., collapse_function = mean)`, threshold diagnostics, plots, automatic regions, and optional region views without generated large output.
- `inst/shiny/{global.R,server.R,ui.R,www/}`: Add local source opening, canonical large-mode state, bounded viewer/progress/cleanup, and metadata repair; report dependency, asset, and installed-package size impact.
- Roxygen/DESCRIPTION, `NEWS.md`, vignettes/SOP, `.github/workflows/`, and hosted checks: document API/file-format/capability changes, regenerate with roxygen2 8.0.0, inspect generated diffs, and never hand-edit generated documentation or web artifacts.

## Work Checklist

- [ ] Freeze legacy Specs/OpenSpecy and prototype descriptors, H5 hyperslabs, ENVI header/binary windows, fingerprints, cache commits/locking, reconnect, and bounded selection against tiny fixtures and guarded prototypes.
- [ ] Implement `FileSpecs` construction/validation/versioning plus explicit S3 dispatch and fail-fast coverage; prove source bytes, hashes, size, and mtime survive success, failure, and cancellation.
- [ ] Correct H5 region/global coordinates and all mosaic transforms; verify landmark correspondence for Region 1 and all three regions.
- [ ] Implement and benchmark the restricted two-pass mean-collapse particle pipeline, automatic region handling, Top N matching, warm-cache reuse, and eager equivalence without whole-source decompression.
- [ ] Add the local path workflow and tiled viewer, repair Uploaded Metadata locally/hosted, and add durable plots, continuous legends, and threshold histograms.
- [ ] Update workflow/guidance/NEWS/roxygen source, regenerate once with the configured toolchain, inspect generated and size diffs, then run the final verification ladder.

## Verification

- **Safety/compatibility**: Legacy Specs structural equality and RDS round trips; detached/moved/changed/same-name sources; dead-handle reload; cache-path collision; interrupted commit; concurrent session; owned cleanup; unsupported-method errors; and no implicit full decompression.
- **Scientific**: Region counts `120633/113460/126504`; axes, IDs, metadata, NAs, attributes, image landmarks, mask/feature membership, mean spectra, preprocessing, match names/values/ties, summaries, plots, and downloads agree with the eager oracle at several chunk sizes.
- **Focused/package**: Resolve Windows Rscript; run focused Specs/H5/image/particle/app tests and repeated benchmarks, then configured documentation, full tests, and `R CMD check` once because public/scientific contracts change.
- **Browser/hosted**: Exercise genuine small upload, Test Map metadata non-first-row selection, and local large path preview/pan/select/cancel/download with console and desktop/mobile screenshots; run action-equivalent preflight and matching-artifact smoke for all three routes and the hosted size message.
- **Large/manual**: Stage metadata/subset, one region, then full sequential `drop.h5` into temp caches/logs; use `dropRegion1.rds` for Region 1 equivalence. Never commit fixtures/results or overwrite either source.

## Risks And Open Questions

- Approve measured hardware budgets and numerical tolerances after the prototype; absolute latency varies, but memory must remain bounded rather than proportional to the cube.
- Decide whether `filelock` is justified or an atomic lock-directory protocol is sufficient on supported Windows/local deployments.
- ENVI header/binary pairing, interleave, byte order, offset, wavelength metadata, and moved-pair relinking must be validated without eager `caTools::read.ENVI()` fallback.
- Halo-based spatial smoothing, median/custom collapse, raw-pixel matching, disk-backed full correlations, and approximate fitted models require separate evidence and approval after the first pipeline.

## Approval Notes

- 2026-08-10: Maintainer selected Option A with H5/ENVI-backed Specs as the big-data boundary, legacy Specs/OpenSpecy preserved, source-safe versioned storage, S3 compartmentalization, and `automate_particle_analysis()` collapse workflow first.
- Implementation approval remains pending acceptance of prototype budgets and this revised public API boundary.
