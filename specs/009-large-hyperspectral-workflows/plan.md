# Feature Plan: File-Backed Specs And Large Hyperspectral Particle Workflows

**Feature dir**: `specs/009-large-hyperspectral-workflows`
**Date**: 2026-08-10
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.
**Current tranche**: Establish the file-backed Specs contract, then implement one bounded particle-analysis pipeline and the related H5, app, metadata, and plot repairs.
**Change class**: Mixed; highest class is hosted/release because shared bundled-app code, package/scientific readers, public S3 APIs, and the generated Shinylive app are affected.
**Status**: Option A was approved and implemented. Local package/app evidence is recorded below; startup tuning and a fresh matching hosted artifact remain follow-up work.

## Goal

- Make `Specs` the experimental 1-10 GB analysis boundary while leaving established `OpenSpecy` behavior unchanged for ordinary in-memory work.
- Analyze large H5/ENVI maps with bounded memory, correct multi-region registration, immutable sources, and a deliberately limited first particle workflow.

## Scope

- **In**: File-backed Specs and versioned cache; local upload bypass/raster viewer; H5 mosaic repair; region-aware particle automation; Test Map metadata repair; durable plots and threshold diagnostics.
- **Out**: Rewriting every `OpenSpecy` function; arbitrary file-backed operation chains; full whole-map correlation matrices; in-place source edits; PCA/K-means fitting, median/custom collapse, or approximate science in the first contract; committing either external prototype.
- **Compatibility**: Existing four-component, matrix-backed `Specs` objects, `.rds` serialization, methods, and `OpenSpecy` inputs/outputs retain their current contracts.

## Requirements

- R1. Benchmark read-only `drop.h5` (3.10 GB) and `dropRegion1.rds` (873 MB) from environment-configured paths, including time, memory, cache, warm reuse, and eager equivalence.
- R2. Add `FileSpecs`, an S3 `Specs` subtype containing durable source/layout, metadata, recipe, cache, and fingerprint descriptors, never a live handle or full values matrix.
- R3. H5 uses short-lived read-only hyperslabs. ENVI reads header-defined `.hdr` plus `.dat`/`.img` windows for supported interleaves and atomically exports a new pair. Inputs are immutable.
- R4. Size/mtime and strong member fingerprints identify each version. Separate copy-on-write cache generations publish atomically and clean owned partials. Changed sources error rather than reuse stale results; cooperative cancellation is deferred.
- R5. Small upload remains. Large local inputs use a safe server path opener; hosted mode rejects oversize files, explains its limit, and directs users locally.
- R6. The local app uses a bounded server raster with brush/pan/reset and click selection; Plotly is limited to the selected spectrum. Canvas tiling, a full-map Shiny action, and true cancellation are deferred.
- R7. Provisional gates: startup under 10 seconds; peak memory at most 2 GB above baseline; viewport at most 2 MB; cached selection under 200 ms; uncached spectrum under 1 second; documented numerical equivalence; no same-output regression over 10%. Record misses rather than weakening a gate.
- R8. H5 paths retain raw region names, local and stage coordinates, unique pixel IDs, and intersecting mosaic tiles; tested transforms align spectral and visual overlays.
- R9. `automate_particle_analysis.FileSpecs()` supports region-sequential approved S/N, mean collapse, named exact preprocessing, and exact Top N. Unsupported combinations fail without eager fallback.
- R10. Region iteration is automatic. `split_spec.FileSpecs(by = "region")` returns lightweight views; `decompress_spec()` requires an explicit bounded selection.
- R11. The 208-spectrum Test Map metadata tab renders and selects matching spectra locally and hosted without lifecycle or console errors.
- R12. Particle results retain their list contract and `plot()` replay. Threshold histograms and a continuous finite min-max signal legend accompany the binary mask.

## Technical Decisions

- **Selected architecture**: Option A is local-first, H5/ENVI-source-backed `FileSpecs`; `dropRegion1.rds` is an eager comparison oracle, not a new large-data backend.
- **S3 boundary**: Keep `Specs()` and `read_specs()` unchanged; add `open_specs(path, cache_dir)`. Make `check_Specs()` and `write_specs()` generic with legacy methods. Every existing `Specs` entry point gets a `FileSpecs` method or guarded error, preventing matrix fallback.
- **Connection lifetime**: Reopen read-only handles from durable descriptors per operation/session; close with `on.exit()` and Shiny cleanup. Saved objects reconnect after fingerprint validation.
- **Version safety**: Cache keys include source/schema, recipe, algorithm, and library identity. Atomic publication, locking, and containment prevent partial or concurrent corruption; completed generations are immutable.
- **Pipeline**: Index regions; stream S/N blocks; optionally smooth the small regional scalar raster; label/filter masks; reread retained pixels for exact means; materialize the collapsed object; then reuse established preprocessing, matching, and outputs.
- **Algorithm limits**: Mean collapse and post-collapse exact Top N are supported. Halo-dependent smoothing, median/custom collapse, full correlation, fitted PCA/K-means, and data-derived entropy breaks are deferred.
- **App state**: Large mode uses canonical `final_specs()` for the descriptor and `final_selection()` for a bounded `OpenSpecy` materialization. The delivered app feeds its selected spectrum, metadata, and bounded download from that selection; it does not expose a full-map analysis action.
- **Hosted contract**: Preserve the three routes, immutable pins, small libraries, and ignored artifacts. Hosted retains small in-memory inputs plus the Test Map fix and reports large mode as local-only.

## Package Surfaces

- `R/Specs.R`, `R/Specs_methods.R`, new `R/Specs_file.R`: legacy compatibility, opener/descriptors, validation/write dispatch, fingerprints, generations, locks, bounded materialization, region views, and guarded S3 methods.
- `R/read_ext.R`, `R/read_envi.R`, `R/visual_image.R`, `R/match_spec.R`, `R/automate_particle_analysis.R`, `R/particle_image.R`: reusable H5/ENVI window readers, transforms, and the FileSpecs worker without whole-source decompression.
- `tests/testthat/`: Preserve legacy Specs/OpenSpecy/ENVI tests; add tiny H5 multi-region/multi-tile and equivalent ENVI interleave fixtures plus focused file-backed, automation, metadata, plotting, safety, failed-publication cleanup, serialization, and concurrency regressions.
- `benchmarks/`: Retain eager comparison code; extend particle-reader benchmarks with repeated open/index/select/pass/cache measurements and guarded external-file runs.
- `workflows/automate_lab.R`: Demonstrate `open_specs("drop.h5") |> automate_particle_analysis(..., collapse_function = mean)`, threshold diagnostics, plots, automatic regions, and optional region views without generated large output.
- `inst/shiny/{global.R,server.R,ui.R,www/}`: Add local source opening, canonical large-mode state, bounded viewer/progress/cleanup, and metadata repair; report dependency, asset, and installed-package size impact.
- Roxygen/DESCRIPTION, `NEWS.md`, vignettes/SOP, `.github/workflows/`, and hosted checks: document API/file-format/capability changes, regenerate with roxygen2 8.0.0, inspect generated diffs, and never hand-edit generated documentation or web artifacts.

## Work Checklist

- [x] Freeze legacy Specs/OpenSpecy and implement descriptors, H5 hyperslabs, ENVI windows, strong fingerprints, cache commits/locking, reconnect, bounded reads, guarded errors, and source-safe atomic ENVI export.
- [x] Implement `FileSpecs` validation/versioning, explicit S3 dispatch, lightweight region views, fail-fast unsupported paths, raw collision-safe H5 region IDs, and focused safety/compatibility coverage.
- [x] Correct H5 region/stage coordinates and multi-tile mosaic transforms; verify the three real regions and Region 1 spectra against the eager oracle.
- [x] Implement and benchmark restricted two-pass mean-collapse automation, automatic regions, exact Top N, warm-cache reuse, diagnostics, replayable plots, and eager equivalence.
- [x] Add the local path opener and bounded raster brush/pan/click journey, repair Uploaded Metadata, and add continuous legends plus threshold histograms.
- [x] Update workflow/guidance/NEWS/roxygen source, regenerate with roxygen2 8.0.0, inspect generated diffs, and pass focused FileSpecs/H5/particle/app tests.
- [ ] Build a fresh post-change wasm artifact and run its matching action-equivalent hosted preflight/browser smoke; stale pre-change pins are not valid evidence.
- [ ] Consider Canvas tiling, cooperative cancellation, and a full-map Shiny action as separately scoped follow-up work.

## Verification

- **Safety/compatibility**: Legacy Specs structural equality and RDS round trips; detached/changed sources; dead-handle reload; cache containment/locking; failed commit cleanup; unsupported-method errors; no implicit full decompression; and byte-for-byte source immutability all pass. The external H5/RDS hashes remained unchanged.
- **Scientific**: Region counts `120633/113460/126504`; axes, IDs, metadata, NAs, attributes, image landmarks, mask/feature membership, mean spectra, preprocessing, match names/values/ties, summaries, plots, and downloads agree with the eager oracle at several chunk sizes.
- **Focused/package**: Resolve Windows Rscript; run focused Specs/H5/image/particle/app tests and repeated benchmarks, then configured documentation, full tests, and `R CMD check` once because public/scientific contracts change.
- **Measured large run**: Cold `drop.h5` open was about 26 seconds, missing the desired under-10-second startup gate because strong hashing dominates. An uncached spectrum was 0.70-0.89 seconds. The full three-region cold/warm particle runs were 210.77/29.36 seconds, peaked near 1.61 GB, created about 225 MB of cache, and returned 975 particles across Region1/2/3 (298/426/251).
- **Browser/hosted**: The maintained FileSpecs journey passes path open, region switching, bounded preview, brush/pan/reset, map click, selected spectrum, and bounded download without console errors. Small upload and Test Map metadata coverage pass. Canvas tiling, cancellation, full-map action, and fresh hosted-artifact validation were not performed.
- **Large/manual**: Stage metadata/subset, one region, then full sequential `drop.h5` into temp caches/logs; use `dropRegion1.rds` for Region 1 equivalence. Never commit fixtures/results or overwrite either source.

## Risks And Open Questions

- Cold strong hashing currently dominates startup and misses the provisional gate; investigate a durable trusted fingerprint/index strategy without weakening source identity. Memory remained within the provisional bound.
- Decide whether `filelock` is justified or an atomic lock-directory protocol is sufficient on supported Windows/local deployments.
- ENVI header/binary pairing, interleave, byte order, offset, wavelength metadata, and moved-pair relinking must be validated without eager `caTools::read.ENVI()` fallback.
- Halo-based spatial smoothing, median/custom collapse, raw-pixel matching, disk-backed full correlations, and approximate fitted models require separate evidence and approval after the first pipeline.

## Approval Notes

- 2026-08-10: Maintainer selected Option A with H5/ENVI-backed Specs as the big-data boundary, legacy Specs/OpenSpecy preserved, source-safe versioned storage, S3 compartmentalization, and `automate_particle_analysis()` collapse workflow first.
- 2026-08-10: Maintainer granted implementation approval. The initial bounded FileSpecs contract and restricted particle pipeline are implemented; the measured cold-open miss and explicitly deferred UI/hosted work above remain visible follow-ups.
