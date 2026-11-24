# Feature Plan: H5 Particle Import

**Date**: 2026-11-24  
**Current tranche**: Generalize the RDS repair for particle-region H5 acquisitions into a package reader and the bundled Shiny import workflow.  
**Change class**: Package/scientific and bundled-app behavior.

## Goal And Requirements

- Import one spectrum per region for files where the acquisition assigned one region to each particle. Compute the arithmetic mean across that region's spectra at each original wavenumber, with no threshold or preprocessing at import.
- Use each region's recorded stage bounds to produce centroid x/y in nm, retaining region IDs and input spectrum counts. Align metadata rows and spectrum IDs and mark the result as an OpenSpecy particle-centroid object.
- Read one cube at a time and project only axis metadata, avoiding the pixel matrix, per-pixel vendor metadata replication, and mosaic decoding. Preserve ordinary map reads and return an actionable error for absent axis/stage information.
- Shiny detects H5/HDF5 acquisitions through the same reader on Run, Recalculate Preview, and Use Uploaded Range. Plot centroids in stage coordinates; grid conform, pixel smoothing, and pixel regrouping are inapplicable to the resulting sparse points.
- Scope excludes particle detection from continuous maps, stitched image reconstruction, reference-library work, and deployment.

## Technical Decisions

- Public workflow: `read_h5(file) |> process_spec()`. Default `collapse = NULL` derives particle mode from the explicit vendor collection mode or nonempty particle-label dataset names. A multi-region shape alone is insufficient. Explicit `FALSE` forces pixels; legacy `TRUE` retains its behavior. Reuse stage/axis readers and the explicit centroid attribute. Fail on unknown calibration; the region center is an acquisition centroid and does not estimate particle shape.
- One final analysis owner: Run's `canonical_state_gate()` feeds processed spectra, identification, quantification, projection, metadata and downloads. No new control or state owner. No input leaves Run hidden; file arrival makes Run dirty; Run materializes and processes; success publishes current results; import failure uses the existing upload error and reselection flow. Mixed centroid uploads require separate Runs to avoid flattening coordinates across sources.
- Update the diagram's **Materialize + Spatial Crop**, **Spatial Smooth**, **Collapse particles?**, and **Pixel Projection** stages. State that sparse particles skip pixel-only operations, retain physical positions and use centroid markers.
- Shared hosted sources trigger `-HostedAppStatic`. Run matching-artifact preflight if an action-built artifact for this source exists. No pin, dependencies, routes, or release work triggers a clean wasm rebuild.
- Scientific rule is new import behavior. Existing importer is unchanged, so a same-output regression benchmark is N/A. Compare the new output against direct means and the repaired real sample. Expected source dimensions 748 regions × 9 spectra × 1,683 bands; target one 748-column object and no mosaic buffer.

## Work Checklist

- [x] Implement the package reader and regression tests for exact means, real stage coordinates, source alignment, malformed inputs, and ordinary map compatibility.
- [x] Route the bundled import and centroid plot through existing gates. Update the pipeline diagram and test file selection, import and result transitions.
- [x] Document package and UI behavior in roxygen, NEWS and relevant vignettes; regenerate with the configured roxygen2 version and inspect the Rd and NAMESPACE diff.
- [x] Verify the original user file against the repaired RDS, run focused tests, inspect a local browser run and downloaded spectra, and run the fast hosted-source and full-package gates.
- [x] Reconcile evidence, processes, working tree, scratch and deferred gates.

## Verification And Scope Boundaries

- Package files: `R/read_ext.R`, `R/interactive_plots.R`, `tests/testthat/test-read_h5.R` and generated API docs. App files: `inst/shiny/{ui,global,server}.R`, focused helper tests and `tools/shiny-local-smoke.spec.js`. Documentation: NEWS, app and advanced vignettes.
- Local smoke uses a genuine H5 and explicit settings, confirms markers and stage positions, then inspects the native processed CSV download. Review console, server errors and desktop screenshot. No app assets, dependencies, workflows or library artifacts change.
- Full package tests follow focused gates. Exact hosted preflight is deferred until an action-built artifact matches this candidate. R CMD check is deferred to routine release verification.
- Focused H5 and plotting tests passed 268 assertions. The real H5 imported in 15.8 seconds and produced 748 stage centroids over 1,683 bands. Region-aligned intensities and x/y matched the repaired RDS exactly, and the spectral axes agreed within 1e-12 tolerance. Hosted static checks passed 391 assertions. Roxygen2 8.0.0 regenerated only the expected reader help diff.
- Focused app checks passed 1,065 assertions. The local browser journey imported a real HDF5 fixture, showed both stage centroids and the active spectrum, and downloaded exactly two processed CSV rows with the expected IDs, coordinates and counts. Screenshot and console/server error checks passed. App assets total 6 files and 107,924 bytes.
- Full suite: 5,391 passed, zero failures, three Windows encoding warnings while loading bundled test data, and two opt-in large AWS integration skips. Working-tree and process checks passed. Removed temporary gate and browser files and restored the tracked test-generated PDF.
