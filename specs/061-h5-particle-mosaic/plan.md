# Feature Plan: Registered H5 Particle Mosaic

**Date**: 2026-11-24  
**Tranche**: Preserve the microscope mosaic on automatically collapsed H5 particle imports.  
**Change class**: Package and bundled-app visualization, with shared hosted sources.

## Goal And Requirements

- Detected H5 particle acquisitions default to one arithmetic mean spectrum per region in Shiny, independent of the map-pixel collapse control or memory mode. Tile maps still use pixel geometry and their existing controls.
- With the existing default `read_visual = TRUE`, attach one registered optical overview using the embedded mosaic tiles. `FALSE` skips image I/O. Means, axes, IDs, attributes and physical centroids are identical in both paths except for the visual attribute.
- Place the image behind centroid markers using measured stage bounds in nm. Keep live overlay visibility and opacity and honor the committed XY crop. Omitted or unreadable visual tiles must leave the spectrum/centroid result usable.

## Technical Decisions

- **Package**: Reuse the HDF5 mosaic center layout and BMP header/format rules. Stream one encoded tile at a time and sample onto one shared raw RGB canvas capped at 2,048 pixels per side. Source tile payloads cap at 16 MiB encoded. No full-resolution mosaic, per-region rasters, new exports, dependencies, or change to the ordinary tile-map decoder output.
- **Registration**: Image x increases to the right and y decreases down the canvas. Preserve native stage edges and convert them along with particle coordinates using the file-declared spatial scale. Resolve tile names with center indices and paint overlaps in acquisition order. Record available, registered and unreadable tile counts. The image is a downsampled visual overview.
- **Performance probe**: User H5 has 748 spectra after one mean from each set of 9, 1,683 bands and 1,089 optical tiles. Prior spectra-only import was 15.8 s. Sample of 40 encoded tiles took 1.33 s to read. Expect a bounded canvas of at most 12 MiB and a single tile payload, with total import within about a minute on the probe machine. If more than two minutes, isolate read/sample throughput before retrying.
- **Shiny**: Existing Run owns analysis; existing Visual Image Overlay controls only presentation. No-input leaves Run unavailable, staged file is dirty, Run imports one centroid object plus its overview, success publishes the same canonical analysis for plot/metadata/download, and error uses the existing import recovery. The mosaic registers via explicit image bounds, not sparse-point spacing.
- **Pipeline diagram**: Update Materialize + Spatial Crop, Pixel Projection and Heatmap / Visual Overlay descriptions. The Collapse particles? branch still governs pixel grouping only.
- **Hosted impact**: Fast `-HostedAppStatic` for shared source; exact hosted runtime preflight if a matching action artifact exists. No dependency, driver, package pin, routes or deployment work changes.

## Work Checklist

- [x] Add the bounded mosaic reader and sampled BMP path; prove tile color/orientation, registration and spectra-only equivalence on fixtures and the real file.
- [x] Use registered stage bounds in the app; exercise default particle averaging, preview overlay, marker visibility and native download in the local browser.
- [x] Update reader/app guidance, NEWS, the pipeline diagram and regenerate reader help with the configured R tooling.
- [x] Run focused reader/app, hosted static, targeted browser, and full package tests, then reconcile deferred gates and clean scratch.

## Package Surfaces

- `R/read_ext.R`, `R/visual_image.R`, `tests/testthat/test-h5-registration.R`, and generated reader help.
- `inst/shiny/{server,global,ui}.R`, `tools/shiny-local-smoke.spec.js`, and relevant app-helper tests. App assets and download implementation are outside scope.
- NEWS, app/advanced vignettes and the canonical pipeline diagram. R CMD check is deferred to routine release verification.

## Verification Evidence

- Focused H5 tests passed 96 assertions after covering absent and unreadable mosaics. Hosted static checks passed 391 assertions. Roxygen2 8.0.0 regenerated the reader help; the generated diff is limited to the intended reader documentation.
- The source H5 imported in 57.19 seconds with a 2,048 by 1,324 overview (1,089 of 1,089 tiles, zero unreadable) and 748 centroids within its stage bounds. Particle intensities and coordinates matched the previous RDS exactly after ID alignment. The mosaic-enabled RDS reread and copy-hash checks passed (10,713,406 bytes); three local warm reads took 0.16–0.17 seconds.
- Focused app checks passed 1,065 assertions. The browser fixture proved default particle means with pixel collapse off, exact image stage bounds, markers above the image, live image visibility and one CSV row per particle. Reviewed the browser screenshot and real-file stage overlay. App assets total 6 files and 107,924 bytes.
- A direct Shiny plotting check on the delivered RDS verified 748 markers, stage-registered image placement and the plotted range from a committed crop.
- Exact hosted runtime preflight is deferred until a matching action-built artifact exists.
- Full package suite: 5,404 passed, zero failures or warnings, and two opt-in large AWS tests skipped. Diff and process checks passed. Removed task-created staging files, browser evidence and temporary logs, and restored the tracked test-generated PDF.
