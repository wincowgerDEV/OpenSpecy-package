# Feature Plan: Masked Circular Autoencoder

**Feature dir**: `specs/001-masked-circular-ae`  
**Date**: 2026-05-22  
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Add an experimental masked circular autoencoder for spectra with missing wavenumber ranges.
- Keep fitting class-agnostic: spectral values and observed-value masks only, never polymer identity, class labels, material names, or supervised metadata.

## Scope

- **In**: Fit, encode, reconstruct, diagnose, and plot circular embeddings for OpenSpecy-style spectra.
- **In**: Optional training-time random contiguous block masking that hides observed ranges from encoder input while preserving them as reconstruction targets.
- **Out**: Bundled trained models, Shiny app code, keras/TensorFlow/reticulate integration, and claims that one circular coordinate is chemically correct for every dataset.
- **Users**: R package users working with Raman/FTIR spectra in canonical `OpenSpecy` objects or documented matrix-like inputs.

## Requirements

- R1. Fit a `MaskedCircularAEModel` from `OpenSpecy` or documented matrix-like spectra with explicit observed-value masks.
- R2. Treat missing values as unobserved, not zero intensity; reconstruction loss uses only observed positions and normalizes per spectrum.
- R3. Learn one circular angle plus a two-coordinate unit-circle representation, with wraparound-safe `circ_dist()` scaled 0 to 1.
- R4. Preserve class-free masked correlation and spectral-angle target distances; invalid low-overlap, zero-variance, or zero-norm pairs are excluded.
- R5. Encode compatible spectra, return finite `theta`, `z1`, `z2`, spectrum identifiers, and observed-range summaries.
- R6. Reconstruct from `theta`, `z`, or compatible spectra and return an OpenSpecy object aligned to the model grid.
- R7. Provide diagnostics for reconstruction quality, distance preservation, neighbor preservation, angular spread, and missingness/range effects.
- R8. Allow metadata only after training for diagnostic summaries or plotting.

## Technical Decisions

- **Approach**: Add standalone `masked_circular_ae*` R modules. Use explicit R helper code for masks, normalization, circular distances, target distances, diagnostics, and validation; use a guarded optional `torch` backend only for neural fitting/encoding/reconstruction.
- **Dependencies**: Reuse existing `data.table`, `matrixStats`, `digest`, base `stats`, and suggested `ggplot2`; add `torch` as guarded `Suggests`; do not add `keras`, `tensorflow`, or `reticulate`.
- **OpenSpecy contract**: Public workflows prefer canonical `OpenSpecy` lists with `wavenumber`, `spectra`, and `metadata`; preserve identifiers, metadata alignment, model grid, and relevant object attributes. Matrix-like inputs are conversion boundaries.
- **Generated artifacts**: Update roxygen and package metadata first; regenerate `NAMESPACE` and `man/*.Rd` with `devtools::document()`.

## Package Surfaces

- `R/`: `masked_circular_ae.R`, `masked_circular_ae_distances.R`, `masked_circular_ae_diagnostics.R`.
- `tests/testthat/`: `test-masked_circular_ae.R` for helper behavior, object contracts, no-label fitting, guarded tiny `torch` tests, diagnostics, plotting, and random block masks.
- `benchmarks/`: N/A for v1 because this is new behavior, not a same-output improvement.
- `inst/examples/`: add a small OpenSpecy-style example with artificial missing ranges.
- `vignettes/README/pkgdown`: optional vignette or README/pkgdown link only if the workflow becomes recommended public guidance.
- `DESCRIPTION`: add guarded optional backend dependency and any metadata needed for examples/docs.
- `NEWS.md`: add user-visible feature entry.
- External Shiny compatibility: keep return objects stable enough for future consumption, but do not add Shiny application code here.

## Work Checklist

- [ ] Add dependency metadata, NEWS stub, and source scaffolds in `DESCRIPTION`, `NEWS.md`, and `R/masked_circular_ae*.R`.
- [ ] Implement input preparation, mask-aware normalization, observed summaries, grid/model validation, backend guards, and validation splitting.
- [ ] Implement `circ_dist()`, `masked_reconstruction_loss()`, and `masked_spectral_distance()` with overlap and finite-pair handling.
- [ ] Implement `fit_masked_circular_ae()`, `encode_masked_circular_ae()`, and `reconstruct_masked_circular_ae()`.
- [ ] Implement diagnostics, `plot_circular_embedding()`, and optional random block masking.
- [ ] Add focused tests in `tests/testthat/test-masked_circular_ae.R`, keeping neural tests tiny and `skip_if_not_installed("torch")`.
- [ ] Add roxygen examples, optional `inst/examples/masked_circular_ae.R`, optional vignette/README/pkgdown references, and run documentation generation.
- [ ] Run `devtools::test()`; run `devtools::check()` or document equivalent CI/R CMD check before release-facing work.

## Verification

- `devtools::test()`: required for routine helper/object tests; optional backend tests must skip cleanly when `torch` is unavailable.
- `devtools::document()`: required after roxygen/export updates.
- `devtools::check()` or CI/R CMD check: required before release-facing work.
- Benchmarks: not required unless future work makes same-output performance claims.

## Risks And Open Questions

- `torch` availability may limit user adoption; all backend-dependent paths need clear skip/error behavior.
- A one-dimensional circle may not fit every spectral collection; diagnostics must make this visible before interpretation.
- Pairwise diagnostics need sampling controls to avoid unbounded O(n^2) work.

## Approval Notes

- Approved by:
- Follow-up:
