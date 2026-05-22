# Research: Masked Circular Autoencoder

## Decision: Use an optional native R neural backend for v1

**Decision**: Implement fitting, encoding, and reconstruction with `torch` as a guarded `Suggests` dependency. Do not add `keras`, `tensorflow`, or `reticulate` for v1.

**Rationale**: The package currently has no Python or neural-network dependency. The feature needs a custom training loop with masked reconstruction, within-batch pairwise distance preservation, circular normalization, optional angular spread, and random block masking. A native R training loop is easier to test, skip, and reason about in package checks than a TensorFlow/Python environment. Keeping the backend in `Suggests` avoids making routine package loading depend on a heavy neural stack.

**Alternatives considered**:

- `keras` or TensorFlow: matches the user's initial preference when feasible, but adds Python/TensorFlow environment concerns and complicates custom masked pairwise loss handling in this package.
- Base R manual backpropagation: avoids dependencies but would create brittle, hard-to-maintain neural code.
- Existing PCA/Specs compression only: insufficient because the requested model is nonlinear, mask-aware, and circular.

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

**Decision**: Compute normalization statistics from finite observed training values per wavenumber, save those statistics on the model, normalize observed values, and fill unobserved or hidden positions with a neutral value in normalized space while concatenating the visible mask into the encoder input.

**Rationale**: NA means unobserved or untrusted, not zero intensity. A filled matrix is required for dense neural input, but the observed-value mask carries which values are real and which values should contribute to reconstruction and target distances.

**Alternatives considered**:

- Replace NA with zero without a mask: rejected because zero is a valid intensity value and would distort spectra.
- Drop spectra or wavenumbers with any NA: rejected because different spectral ranges are expected.
- Impute missing values and treat them as observed: rejected because the loss must be evaluated only where the original spectrum was observed.

## Decision: Separate true missingness from random block masking

**Decision**: Maintain both an original observed-value mask and a per-batch visible mask. Random block masking modifies only the visible mask and encoder input; reconstruction loss and target distances continue to use the original observed mask.

**Rationale**: Training-time block masking is data augmentation, not evidence that the original value was unobserved. Keeping the two masks separate prevents hidden training blocks from disappearing from reconstruction targets.

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

## Decision: Split tests into routine helper tests and guarded neural tests

**Decision**: Always test circular distance, masked loss helpers, target distances, object preparation, and no-label metadata exclusion. Guard neural backend tests with package availability and keep synthetic training tiny; longer validation is manual or CI-only.

**Rationale**: This satisfies scientific behavior coverage without making `devtools::test()` unexpectedly slow or dependent on optional backend setup.

**Alternatives considered**:

- Require full neural training in every local test run: rejected because it violates the constitution's long-running test guidance.
- Test only exported fit success: rejected because mask, distance, and no-label invariants need direct coverage.
