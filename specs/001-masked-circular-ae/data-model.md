# Data Model: Masked Circular Autoencoder

## Spectral Input

Represents source spectra supplied by a user.

**Fields**

- `wavenumber`: numeric vector defining the spectral grid.
- `spectra`: numeric matrix with one row per wavenumber and one column per spectrum for OpenSpecy inputs.
- `metadata`: data.table with one row per spectrum when available.
- `attributes`: relevant OpenSpecy attributes such as units, processing state, baseline state, derivative order, and history.
- `spectrum_id`: unique identifier per spectrum, usually from `colnames(spectra)` or generated during preparation.

**Validation Rules**

- OpenSpecy inputs must pass `check_OpenSpecy()` or fail with a clear error.
- `length(wavenumber)` must equal `nrow(spectra)` for OpenSpecy inputs.
- Spectrum column names must be unique after preparation.
- Metadata rows must remain aligned with spectra.
- Matrix-like inputs require a supplied or inferable wavenumber grid.

## Training Matrix

Internal spectrum-row representation used by fitting and diagnostics.

**Fields**

- `x`: numeric matrix with one row per spectrum and one column per wavenumber.
- `mask_observed`: logical matrix matching `x`; `TRUE` means the original value is finite and eligible as a target.
- `x_filled`: numeric matrix matching `x`; missing values are filled only for dense fallback calculations.
- `wavenumber`: numeric vector matching columns of `x`.
- `spectrum_id`: character vector matching rows of `x`.
- `metadata`: optional data.table carried for outputs only, not for fitting decisions.

**Validation Rules**

- `dim(x)`, `dim(mask_observed)`, and `dim(x_filled)` must match.
- A spectrum with fewer than the minimum required observed points is excluded or reported according to the user-facing fitting rules.
- Non-finite observed values are treated as unobserved.
- Metadata columns are not included in `x`, `x_filled`, validation splitting, or loss calculation.

## Fitted Circular Model

Trained model object returned by `fit_masked_circular_ae()`.

**Fields**

- `model_type`: `"masked_circular_ae"`.
- `backend`: R backend name, version information, and method summary.
- `wavenumber`: numeric model grid.
- `normalization`: per-wavenumber center, scale, fill value, and observed-count summaries.
- `hyperparameters`: loss weights, overlap thresholds, decoder degree, validation fraction, seed, and verbosity settings.
- `target_distance`: selected target distance type.
- `state`: reference spectra/coordinates and periodic decoder state needed for encoding and reconstruction.
- `history`: training and validation loss summaries, including component losses when available.
- `preprocessing_assumptions`: statement that spectra were preprocessed before fitting.
- `model_id`: digest or stable identifier for compatibility checks.

**Validation Rules**

- `wavenumber` must be finite and match the reference spectra and decoder state.
- `normalization` vectors must match the wavenumber grid length.
- `lambda_rec`, `lambda_dist`, and `lambda_uniform` must be finite non-negative numbers, with at least one positive loss weight.
- Model state must be present before encoding or reconstruction.
- No metadata label fields are stored as model targets or fitting inputs.

## Encoded Circular Embedding

Per-spectrum circular coordinates returned by `encode_masked_circular_ae()`.

**Fields**

- `spectrum_id`: input spectrum identifier.
- `theta`: numeric angle in degrees on `[0, 360)`.
- `observed_points`: number of observed wavenumbers in the encoded input.
- `min_wavenumber`: lowest observed wavenumber for the spectrum.
- `max_wavenumber`: highest observed wavenumber for the spectrum.
- `metadata`: optional post-training join for diagnostics or plotting.

**Validation Rules**

- `theta` must be finite for spectra that meet minimum observation requirements.
- `theta` must be normalized to `[0, 360)`.
- Metadata joins must not alter model state or loss history.
- Optional `Specs` compatibility stores exactly one latent variable named `theta`.

## Reconstruction

Predicted spectra on the model's wavenumber grid.

**Fields**

- `wavenumber`: model grid.
- `spectra`: reconstructed matrix with one row per wavenumber and one column per requested spectrum or coordinate.
- `metadata`: source identifiers, theta coordinates, and optional source metadata.
- `attributes`: OpenSpecy-compatible attributes documenting reconstruction and model id.

**Validation Rules**

- Reconstruction output must be an OpenSpecy object when returned as spectra.
- Reconstruction columns must align with metadata rows.
- Evaluation against original spectra uses only original observed positions.

## Diagnostic Summary

Post-training metrics used to judge whether the circular embedding is useful.

**Fields**

- `reconstruction`: per-spectrum masked RMSE and masked correlation.
- `distance_preservation`: sampled pair records and Pearson/Spearman agreement.
- `neighbor_preservation`: overlap between nearest neighbors in spectral-distance and circular-distance spaces.
- `missingness_range`: theta versus observed count, minimum observed wavenumber, and maximum observed wavenumber.
- `angular_spread`: mean circular vector length and related collapse indicators.

**Validation Rules**

- Diagnostics may use metadata only after fitting.
- Diagnostics must report when insufficient valid pairs or observed points prevent a metric.
- Diagnostic text must avoid claiming chemical correctness from the circular coordinate alone.
