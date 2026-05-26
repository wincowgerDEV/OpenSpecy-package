# Implementation Plan: Masked Circular Autoencoder

**Branch**: `main` | **Date**: 2026-05-22 | **Spec**: [spec.md](spec.md)

**Input**: Feature specification from `/specs/001-masked-circular-ae/spec.md`

**Note**: This plan is filled in by the `/speckit-plan` workflow.

## Summary

Add a standalone experimental circular autoencoder workflow for OpenSpecy-style spectra with missing spectral ranges. The implementation will keep OpenSpecy as the primary input and reconstruction format, train only from spectral values plus observed-value masks, expose class-agnostic circular encoding and diagnostics, and avoid using metadata labels except in post-training plots or summaries.

The implementation uses an R-native backend based on `stats`: mask-aware target distances, classical multidimensional scaling for the training circle, masked spectral similarity for encoding, and periodic least-squares decoder terms for reconstruction. Keras, TensorFlow, reticulate, and torch are not selected for v1 because their runtime/install burden is too high for an experimental package feature.

## Technical Context

**Language/Version**: R >= 4.3.0.

**Primary Dependencies**: Reuse existing `data.table`, `matrixStats`, `digest`, base `stats`, and base graphics where possible. Do not add `torch`, `keras`, `tensorflow`, `reticulate`, or a plotting dependency for v1.

**Storage**: No trained model is bundled in `data/` for v1. Add a lightweight example script under `inst/examples/` if implementation needs a runnable package resource. Fitted model objects are ordinary R objects that users can save with `saveRDS()`.

**Testing**: testthat edition 3 via `devtools::test()`. Base-R helper tests and tiny R-native fit/encode/reconstruct/diagnose tests run routinely. Heavier validation is manual or GitHub Actions guarded. Release-sensitive work still requires `devtools::check()` or the GitHub Actions R CMD check matrix.

**Target Platform**: CRAN/GitHub R package across Windows, macOS, and Linux. Helper functions and fitting should work wherever base R and the package imports work. External OpenSpecy-shiny compatibility is considered through stable return objects, but no Shiny app code is added here.

**Project Type**: R package with pkgdown site and vignettes; the Shiny application remains in `wincowgerDEV/OpenSpecy-shiny`.

**Performance Goals**: Keep routine unit tests under normal package-test time by using helper-level tests and tiny synthetic model tests. Use vectorized matrix operations for masks, pairwise target distances, circular distances, and diagnostics where practical. Pairwise diagnostics should support sampling to avoid unbounded O(n^2) work on large spectral collections.

**Constraints**: Preserve spectral meaning, OpenSpecy object alignment, object attributes, generated-doc workflow, CRAN checks, external Shiny boundary, and dependency discipline. Never use polymer identity, material labels, class columns, or supervised metadata in fitting, validation splitting, encoder inputs, or losses.

**Scale/Scope**: Affects new R source for masked circular autoencoder functions, new tests, optional example/vignette documentation, `DESCRIPTION` dependency metadata, `NEWS.md`, and generated roxygen artifacts. Existing PCA/Specs code should remain functionally unchanged, with only optional compatibility helpers added if needed.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- Scientific integrity: PASS. The plan explicitly tracks wavenumbers, intensities, missing-value masks, normalization statistics, target-distance thresholds, and diagnostic limits. The circular coordinate is documented as an empirical embedding, not chemical truth.
- OpenSpecy object contract: PASS. OpenSpecy remains the primary input and reconstruction format. Internal training can transpose spectra to spectrum-by-wavenumber matrices, but user-facing objects preserve `wavenumber`, `spectra`, `metadata`, spectrum identifiers, and relevant attributes.
- R package contract: PASS. New exported functions, optional dependency metadata, tests, roxygen, examples, and NEWS are identified. The implementation is isolated from existing PCA digit-code functions.
- Tests: PASS. Routine tests cover current behavior for helper functions, object contracts, and a tiny R-native model workflow; long-running validations are manual or CI-guarded.
- Benchmarks: PASS. No same-output improvement to an existing function is planned. A benchmark is optional only if implementation makes performance claims for training or diagnostics.
- Documentation: PASS. Roxygen, example workflow, optional vignette/README/pkgdown entry, and NEWS are planned. Examples center OpenSpecy objects.
- Shiny boundary: PASS. Compatibility is considered through stable encoded and diagnostic outputs; no Shiny code enters this repository.
- Generated artifacts: PASS. `NAMESPACE`, `man/*.Rd`, and pkgdown HTML will be regenerated from source tooling and not edited directly.

## Project Structure

### Documentation (this feature)

```text
specs/001-masked-circular-ae/
|-- plan.md
|-- research.md
|-- data-model.md
|-- quickstart.md
|-- contracts/
|   `-- public-r-api.md
|-- checklists/
|   `-- requirements.md
`-- tasks.md
```

### Source Code (repository root)

```text
R/
|-- masked_circular_ae.R              # Fit, encode, reconstruct, model checks, roxygen
|-- masked_circular_ae_distances.R    # circ_dist(), masked spectral distances
`-- masked_circular_ae_diagnostics.R  # Reconstruction, distance, neighbor, range diagnostics, plotting

tests/testthat/
`-- test-masked_circular_ae.R         # Helper, no-label, OpenSpecy, and small synthetic tests

inst/examples/
`-- masked_circular_ae.R              # Small OpenSpecy-style example with artificial missing ranges

vignettes/
`-- masked-circular-autoencoder.Rmd   # Optional longer workflow if dependency/install burden is acceptable

DESCRIPTION                           # Add guarded optional backend dependency and metadata
NEWS.md                               # User-visible feature entry
NAMESPACE                             # Generated by devtools::document(); do not edit directly
man/                                  # Generated by devtools::document(); do not edit directly
docs/                                 # Generated pkgdown site; do not edit HTML directly
```

**Structure Decision**: Keep the feature as standalone `masked_circular_ae*` package modules. Reuse `OpenSpecy` and optional `Specs` compatibility patterns, but do not modify existing PCA compression behavior or matching behavior except through clearly named new functions.

## Phase 0 Research Summary

Research decisions are recorded in [research.md](research.md). The key outcomes are: use an R-native `stats` backend rather than torch/keras/TensorFlow, normalize spectra with observed-value masks, use a single degree-valued `theta` embedding, use class-free masked correlation and spectral-angle targets, and keep routine package checks fast.

## Phase 1 Design Summary

Design artifacts are recorded in [data-model.md](data-model.md), [contracts/public-r-api.md](contracts/public-r-api.md), and [quickstart.md](quickstart.md). The model object stores spectral grid, normalization statistics, mask-aware training configuration, backend state, and loss histories. Encoded outputs carry one latent value, `theta` in degrees on `[0, 360)`, plus spectrum identifiers and observed-count/range summaries. Reconstructions return OpenSpecy objects aligned to the model grid.

## Post-Design Constitution Check

- Scientific integrity: PASS. Data model and contracts specify mask-aware normalization, overlap validation, finite-distance handling, and diagnostic reporting.
- OpenSpecy object contract: PASS. Public contracts preserve OpenSpecy input, metadata alignment, and reconstruction outputs; optional `Specs` compatibility is additive.
- R package contract: PASS. Source, tests, docs, dependency metadata, generated artifacts, and NEWS are all called out.
- Tests: PASS. The contract keeps helper tests and tiny R-native model tests in routine package testing.
- Benchmarks: PASS. No same-output benchmark is required; optional diagnostic/training benchmarks are left for later if performance claims are introduced.
- Documentation: PASS. Roxygen and a runnable OpenSpecy-style example are mandatory implementation tasks; vignette/README/pkgdown updates are planned based on dependency burden.
- Shiny boundary: PASS. Return objects are stable enough for future Shiny consumption, with no app code added to this repo.
- Generated artifacts: PASS. Generated files remain generated.

## Complexity Tracking

No constitution violations are planned. The R-native backend avoids adding dependency complexity while preserving the requested mask-aware circular workflow.
