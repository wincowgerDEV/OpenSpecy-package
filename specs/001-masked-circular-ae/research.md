# Research: Masked Circular Autoencoder

## Decision: Use an R-native circular embedding backend for v1

**Decision**: Implement fitting, encoding, and reconstruction with base R and `stats`: mask-aware target distances, classical multidimensional scaling for the training circle, masked spectral similarity for encoding new spectra, and periodic least-squares decoder terms for reconstruction. Do not add `torch`, `keras`, `tensorflow`, or `reticulate` for v1.

**Rationale**: The package should avoid a native neural runtime for an experimental feature because binary downloads, SSL/proxy issues, and platform-specific setup complicate routine package development. A deterministic R-native backend preserves the no-label, mask-aware circular workflow while keeping installation and CRAN checks lightweight.

**Alternatives considered**:

- `torch`: provides a native R-facing neural training loop, but requires separate LibTorch/Lantern binaries and can fail behind SSL/proxy/firewall constraints.
- `keras` or TensorFlow: adds Python/TensorFlow environment concerns and complicates package checks.
- Base R manual backpropagation: avoids dependencies but would create brittle, hard-to-maintain neural code.
- Existing PCA/Specs compression only: insufficient because the requested model is mask-aware and circular.

## Decision: Keep the autoencoder standalone from PCA/Specs internals

**Decision**: Add new `masked_circular_ae*` modules and optional `Specs`-compatible encoded outputs. Do not reuse or modify `fit_specs_pca()` internals for this model.

**Rationale**: The existing `Specs` code provides useful object-shape conventions, but the circular autoencoder has different missing-value handling, loss functions, model state, and diagnostics. Keeping it standalone protects current PCA behavior and makes it easier to test the no-label training constraint.

**Alternatives considered**:

- Extend `fit_specs_pca()` with an autoencoder mode: rejected because it would tangle distinct models and risk regressions in PCA compression.
- Return only raw matrices: rejected because users need spectrum identifiers, metadata alignment, and diagnostics.

## Decision: Preserve OpenSpecy orientation externally and transpose internally

**Decision**: Accept OpenSpecy spectra as wavenumber rows by spectrum columns, then prepare an internal dense matrix with spectrum rows and wavenumber columns for training.

**Rationale**: This follows the OpenSpecy contract while matching the user requirement that wavenumbers are columns during training. Keeping the transposition in data preparation avoids leaking orientation surprises into user-facing APIs.

**Alternatives considered**:

- Require users to supply spectrum-row matrices only: rejected because OpenSpecy objects should be first-class.
- Store trained reconstructions in internal orientation: rejected because reconstruction should return an OpenSpecy object on the model grid.

## Decision: Use mask-aware normalization with neutral filling

**Decision**: Compute normalization statistics from finite observed training values per wavenumber, save those statistics on the model, normalize observed values, and fill unobserved positions with a neutral value only for dense fallback calculations.

**Rationale**: NA means unobserved or untrusted, not zero intensity. The observed-value mask carries which values are real and which values should contribute to reconstruction and target distances; filled values are used only where dense `stats` helpers require finite matrices.

**Alternatives considered**:

- Replace NA with zero without a mask: rejected because zero is a valid intensity value and would distort spectra.
- Drop spectra or wavenumbers with any NA: rejected because different spectral ranges are expected.
- Impute missing values and treat them as observed: rejected because the loss must be evaluated only where the original spectrum was observed.

## Decision: Retain random block masking only as compatibility metadata/helper

**Decision**: Remove training-time random block masking from the public API for v1.

**Rationale**: The selected backend has no stochastic mini-batch training loop. Removing unused tuning controls keeps the experimental API focused on the one-value circular embedding and avoids carrying implementation history into user-facing documentation.

**Alternatives considered**:

- Overwrite the observed mask during block masking: rejected because it would under-train reconstruction on intentionally hidden but valid values.
- Apply random point masking only: rejected for v1 because contiguous range gaps better match spectrometer coverage differences.

## Decision: Support masked correlation and masked spectral-angle target distances

**Decision**: Implement both target distances as class-free functions that operate only on shared finite observed wavenumbers and return invalid pairs as `NA`.

**Rationale**: The feature requires no polymer labels and two unsupervised spectral-distance options. Returning invalid pairs as `NA` makes it explicit when overlap thresholds, zero variance, or zero norm prevent reliable distance calculation.

**Alternatives considered**:

- Use material classes as distance targets: rejected by the core requirement.
- Fill invalid distances with zero or one: rejected because it would invent similarity or dissimilarity where overlap is insufficient.

## Decision: Use unlabeled random validation splitting by spectrum

**Decision**: Split train/validation spectra by seeded random spectrum index, independent of metadata columns.

**Rationale**: The validation split must not use class labels, material names, source labels, or other supervised metadata. A seeded split is reproducible and testable.

**Alternatives considered**:

- Stratify by polymer or dataset metadata: rejected because it uses supervised or diagnostic metadata during fitting.
- No validation history: rejected because the fitted model should report validation loss when requested.

## Decision: Keep diagnostics explicit and bounded

**Decision**: Provide reconstruction diagnostics per spectrum, sampled pairwise distance preservation, neighbor preservation for configurable `k`, and missingness/range diagnostics. Pairwise diagnostics default to sampling when data are large.

**Rationale**: A single circular coordinate can distort branching, clustered, or higher-dimensional spectral structure. Diagnostics must show whether the learned circle preserves spectral relationships and whether missing spectral range drives the embedding.

**Alternatives considered**:

- Report only training loss: rejected because it does not reveal circular-distance quality or missingness bias.
- Always compute all pairwise distances: rejected because large spectral libraries may make O(n^2) diagnostics too expensive.

## Decision: Keep all model tests routine and R-native

**Decision**: Always test circular distance, masked loss helpers, target distances, object preparation, no-label metadata exclusion, and a tiny R-native fit/encode/reconstruct/diagnose workflow. Longer validation remains manual or CI-only.

**Rationale**: This satisfies scientific behavior coverage without making `devtools::test()` unexpectedly slow or dependent on optional backend setup.

**Alternatives considered**:

- Require full neural training in every local test run: rejected because it violates dependency and long-running test guidance.
- Test only exported fit success: rejected because mask, distance, and no-label invariants need direct coverage.
