# Quickstart: Masked Circular Autoencoder

This example shows the intended user workflow. It uses synthetic OpenSpecy-style spectra so labels can be present for diagnostics without being used during fitting.

```r
library(OpenSpecy)

set.seed(1)

wavenumber <- seq(600, 3200, length.out = 300)
n <- 48
theta_true <- seq(0, 2 * pi, length.out = n + 1)[-1]

spectra <- vapply(theta_true, function(theta) {
  peak_a <- exp(-((wavenumber - (1100 + 120 * cos(theta))) / 90)^2)
  peak_b <- exp(-((wavenumber - (1800 + 150 * sin(theta))) / 120)^2)
  peak_c <- 0.4 * cos(wavenumber / 180 + theta)
  peak_a + peak_b + peak_c
}, FUN.VALUE = numeric(length(wavenumber)))

colnames(spectra) <- paste0("synthetic_", seq_len(n))

metadata <- data.frame(
  sample_name = colnames(spectra),
  diagnostic_group = rep(c("A", "B", "C"), length.out = n)
)

os <- as_OpenSpecy(
  x = wavenumber,
  spectra = spectra,
  metadata = metadata
)

# Artificially introduce missing spectral ranges. These NA values mean
# "not observed", not zero intensity.
for (i in seq_len(ncol(os$spectra))) {
  if (i %% 3 == 1) {
    os$spectra[wavenumber > 2400, i] <- NA_real_
  } else if (i %% 3 == 2) {
    os$spectra[wavenumber < 900, i] <- NA_real_
  } else {
    os$spectra[wavenumber > 1450 & wavenumber < 1750, i] <- NA_real_
  }
}

model <- fit_masked_circular_ae(
  os,
  target_distance = "spectral_angle",
  lambda_rec = 1,
  lambda_dist = 1,
  lambda_uniform = 0.01,
  min_overlap_points = 40,
  min_overlap_fraction = 0.20,
  decoder_degree = 3,
  validation_fraction = 0.2,
  seed = 1
)

encoded <- encode_masked_circular_ae(model, os)

# Metadata are used only after training for diagnostics or display.
diagnostics <- diagnose_masked_circular_ae(
  model,
  os,
  encoded = encoded,
  metadata = os$metadata,
  n_pairs = 1000,
  k = 5,
  seed = 1
)

reconstructed <- reconstruct_masked_circular_ae(model, x = os)

plot_circular_embedding(
  encoded,
  color_by = "diagnostic_group",
  metadata = os$metadata
)
```

Expected checks after implementation:

- `model$history` contains training and validation losses.
- `encoded$theta` is finite and on `[0, 360)` for spectra with enough observed points.
- `encode_masked_circular_ae(..., as_specs = TRUE)` stores a single latent variable named `theta`.
- Diagnostics report reconstruction error, distance preservation, neighbor preservation, and missingness/range summaries.
- Changing `metadata$diagnostic_group` before fitting does not change the fitted model when the same seed and spectra are used.
