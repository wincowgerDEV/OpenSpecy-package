# Public R API Contract: Masked Circular Autoencoder

## `fit_masked_circular_ae()`

```r
fit_masked_circular_ae(
  x,
  wavenumber = NULL,
  target_distance = c("correlation", "spectral_angle"),
  lambda_rec = 1,
  lambda_dist = 1,
  lambda_uniform = 0.01,
  min_overlap_points = 100,
  min_overlap_fraction = 0.25,
  decoder_degree = 3,
  validation_fraction = 0.2,
  seed = 1,
  verbose = TRUE
)
```

**Accepts**

- An `OpenSpecy` object.
- A numeric matrix or data.frame with spectra in rows and wavenumbers in columns when `wavenumber` is supplied or column names are numeric.

**Returns**

- A `MaskedCircularAEModel` list with model state, wavenumber grid, normalization statistics, hyperparameters, target-distance settings, random masking settings retained for compatibility, training history, validation history, and model id.

**Required Behavior**

- Must not require labels or metadata.
- Must not use polymer identity, class labels, material names, or supervised metadata in fitting, splitting, encoder input, or losses.
- Must treat missing values as unobserved and use explicit masks.
- Must use an R-native backend based on `stats` rather than requiring a neural runtime.

## `encode_masked_circular_ae()`

```r
encode_masked_circular_ae(
  model,
  x,
  wavenumber = NULL,
  as_specs = FALSE
)
```

**Accepts**

- A `MaskedCircularAEModel`.
- Compatible OpenSpecy or matrix-like spectra.

**Returns**

- By default, a data.table-like encoded embedding with `spectrum_id`, `theta`, `observed_points`, `min_wavenumber`, and `max_wavenumber`.
- `theta` is the single latent value per spectrum and is expressed in degrees on `[0, 360)`.
- If `as_specs = TRUE`, a `Specs` object with one latent variable, `theta`, and model metadata attached for compatibility.

**Required Behavior**

- Must verify wavenumber compatibility with the model.
- Must return one finite `theta` value per spectrum within `[0, 360)` when sufficient observed data are available.
- Must not consume metadata labels except to preserve identifiers or optional output joins.

## `reconstruct_masked_circular_ae()`

```r
reconstruct_masked_circular_ae(
  model,
  theta = NULL,
  x = NULL,
  wavenumber = NULL
)
```

**Accepts**

- A fitted model plus one of `theta` or compatible spectra `x`.

**Returns**

- An OpenSpecy object with reconstructed spectra on the model's wavenumber grid and metadata identifying the requested coordinates or source spectra.

**Required Behavior**

- Must require exactly one reconstruction source among `theta` or `x`.
- Must normalize supplied `theta` values to `[0, 360)`.
- Must preserve model-grid alignment in the output.

## `circ_dist()`

```r
circ_dist(theta1, theta2)
```

**Accepts**

- Numeric angle vectors in degrees.

**Returns**

- Numeric circular distance values scaled from 0 to 1.

**Required Behavior**

- Same angles return 0.
- `0` and `2 * pi` return 0.
- `0` and `pi` return 1.
- Angles near wraparound, such as 359 degrees and 1 degree, return a small distance.

## `masked_spectral_distance()`

```r
masked_spectral_distance(
  x,
  mask = is.finite(x),
  method = c("correlation", "spectral_angle"),
  min_overlap_points = 100,
  min_overlap_fraction = 0.25,
  pairs = NULL,
  weights = c("overlap", "uniform")
)
```

**Accepts**

- Spectrum-row numeric matrix `x`.
- Logical mask with the same dimensions.
- Optional pair index table.

**Returns**

- Pairwise target distances and validity/weight information, or a dense distance matrix when requested by implementation.

**Required Behavior**

- Must use only overlapping finite observed values.
- Must mark low-overlap, zero-variance, or zero-norm pairs invalid.
- Correlation distance must be mapped to `[0, 1]`.
- Spectral-angle distance must be mapped to `[0, 1]`.

## `masked_reconstruction_loss()`

```r
masked_reconstruction_loss(x, x_hat, mask)
```

**Accepts**

- Observed normalized matrix `x`.
- Reconstructed matrix `x_hat`.
- Logical observed-value mask.

**Returns**

- Mean per-spectrum masked squared error, normalized by each spectrum's observed count.

**Required Behavior**

- Unobserved positions must not affect the loss.
- Spectra with more observed points must not dominate solely because they have wider measured range.

## `diagnose_masked_circular_ae()`

```r
diagnose_masked_circular_ae(
  model,
  x,
  wavenumber = NULL,
  encoded = NULL,
  target_distance = model$target_distance,
  n_pairs = 10000,
  k = 5,
  metadata = NULL,
  seed = 1
)
```

**Returns**

- A list with `reconstruction`, `distance_preservation`, `neighbor_preservation`, `missingness_range`, and `angular_spread` components.

**Required Behavior**

- Must allow metadata only for post-training summaries.
- Must report insufficient-data conditions without non-finite metric leakage.
- Must include enough information to detect range-driven embeddings.

## `plot_circular_embedding()`

```r
plot_circular_embedding(
  encoded,
  color_by = NULL,
  label_by = NULL,
  metadata = NULL
)
```

**Accepts**

- Encoded embedding from `encode_masked_circular_ae()`.
- Optional metadata for post-training coloring or labels.

**Returns**

- A plot object when plotting dependencies are available.

**Required Behavior**

- Must not fit or update a model.
- Must not use metadata for anything beyond display.
- Must place points by the learned circular coordinates.
