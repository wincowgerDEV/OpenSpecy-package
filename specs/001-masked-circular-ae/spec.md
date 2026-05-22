# Feature Specification: Masked Circular Autoencoder

**Feature Branch**: `[001-masked-circular-ae]`

**Created**: 2026-05-22

**Status**: Draft

**Input**: User description: "Add capability to train a masked circular autoencoder for spectra, centered on OpenSpecy-style data, that learns a class-agnostic one-dimensional circular coordinate while respecting missing spectral ranges."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Train a Class-Agnostic Circular Spectrum Model (Priority: P1)

A spectroscopy user trains a dimensionality-reduction model on preprocessed spectra that may have incomplete wavenumber coverage. The model learns one circular coordinate per spectrum from spectral values and observed-value masks only, so similar spectra are nearby around the circle and maximally different spectra are separated across the circle.

**Why this priority**: This is the core scientific capability. Without class-agnostic training that respects missing values, the learned coordinate would not meet the feature goal.

**Independent Test**: Can be fully tested by fitting on OpenSpecy-style spectra with missing ranges and verifying that fitting completes without requiring or consuming polymer labels, material names, classes, or supervised targets.

**Acceptance Scenarios**:

1. **Given** an OpenSpecy object with aligned `wavenumber`, `spectra`, `metadata`, and missing spectral ranges, **When** the user fits the circular model, **Then** the fitted model records the spectral grid, mask-aware normalization information, training settings, loss history, and target distance choice.
2. **Given** input data with metadata containing class-like columns, **When** the user fits the model, **Then** those metadata values are not required, are not used to split the data, and do not enter the model objective.
3. **Given** spectra with different numbers of observed wavenumbers, **When** reconstruction error contributes to fitting, **Then** each spectrum's reconstruction contribution is normalized by its observed-value count.
4. **Given** two spectra with insufficient shared observed wavenumbers, **When** target spectral distances are computed, **Then** that pair is excluded from distance-preservation loss rather than filled with a misleading value.

---

### User Story 2 - Encode, Compare, and Reconstruct Spectra (Priority: P1)

A user applies a fitted model to OpenSpecy-style spectra to obtain circular coordinates, compare spectra with wraparound-safe circular distances, and reconstruct spectra on the model's common wavenumber grid.

**Why this priority**: A trained model is useful only if users can encode new spectra, compute meaningful circular distances, and inspect reconstructions in terms of the original spectral grid.

**Independent Test**: Can be tested by fitting on a small synthetic data set, encoding held-out spectra, verifying finite circular coordinates and unit-length internal circular representations, and reconstructing spectra with outputs aligned to the model grid.

**Acceptance Scenarios**:

1. **Given** a fitted model and compatible spectra, **When** the user encodes the spectra, **Then** the output includes a finite angle and a two-coordinate unit circular representation for each spectrum.
2. **Given** encoded angles near the wraparound boundary, **When** the user computes circular distance, **Then** angles such as 359 degrees and 1 degree are treated as near neighbors.
3. **Given** a fitted model and either encoded coordinates or new compatible spectra, **When** the user requests reconstruction, **Then** the returned spectra are on the model's wavenumber grid and can be evaluated only at originally observed positions when a mask is available.

---

### User Story 3 - Diagnose Whether the Circle Preserves Spectral Structure (Priority: P2)

A researcher evaluates whether the learned circle is a meaningful representation for their data before interpreting it scientifically. They review reconstruction quality, distance preservation, neighbor preservation, and whether missingness or spectral range drives the learned angles.

**Why this priority**: A single circular coordinate is intentionally restrictive. Diagnostics are necessary to decide whether the embedding is appropriate for a given spectral collection.

**Independent Test**: Can be tested by running diagnostics on a fitted model and checking that reconstruction, distance-preservation, neighbor-preservation, and missingness/range summaries are produced without needing labels.

**Acceptance Scenarios**:

1. **Given** a fitted model and spectra, **When** the user requests reconstruction diagnostics, **Then** the result reports masked RMSE and masked observed-versus-reconstructed correlation for each spectrum where enough observed values exist.
2. **Given** a fitted model and spectra, **When** the user requests distance-preservation diagnostics, **Then** sampled spectrum pairs report original masked spectral distance, circular distance, and aggregate Pearson and Spearman agreement where enough valid pairs exist.
3. **Given** a fitted model and spectra, **When** the user requests neighbor-preservation diagnostics, **Then** the result compares nearest neighbors in original spectral-distance space with nearest neighbors in circular-distance space.
4. **Given** a fitted model and spectra with varying missing ranges, **When** the user requests missingness diagnostics, **Then** the result relates circular angle to observed-point count and observed wavenumber range so users can detect range-driven embeddings.

---

### User Story 4 - Plot Circular Embeddings With Optional Post-Training Metadata (Priority: P3)

A user visualizes the learned circular embedding and optionally colors or labels points with metadata after the model has already been trained.

**Why this priority**: Visualization helps users interpret the embedding, but metadata use must remain diagnostic only so training stays class-agnostic.

**Independent Test**: Can be tested by plotting encoded spectra with and without metadata fields and verifying that metadata are accepted only by diagnostic or plotting functions after training.

**Acceptance Scenarios**:

1. **Given** encoded spectra without metadata, **When** the user plots the circular embedding, **Then** the plot shows each spectrum at its learned circular location.
2. **Given** encoded spectra and metadata such as polymer class, instrument, source dataset, or preprocessing type, **When** the user colors or labels the plot, **Then** the metadata affect only the displayed diagnostics and not the fitted model or stored loss history.

---

### User Story 5 - Learn Robustly From Partially Hidden Spectral Regions (Priority: P3)

A user enables training-time random block masking so the model learns to encode spectra from partial spectral evidence without treating intentionally hidden observations as truly missing targets.

**Why this priority**: Spectral ranges often differ across instruments or data sources, and robustness to partial coverage improves practical utility.

**Independent Test**: Can be tested by fitting with random spectral block masking enabled and verifying that hidden blocks are excluded from encoder-visible inputs while remaining eligible for reconstruction scoring when they were truly observed.

**Acceptance Scenarios**:

1. **Given** spectra with true observed-value masks, **When** training-time random block masking is enabled, **Then** contiguous observed wavenumber blocks may be hidden from the model input for training batches.
2. **Given** randomly hidden but originally observed values, **When** reconstruction loss is computed, **Then** those values remain valid reconstruction targets.

### Edge Cases

- Input spectra contain no missing values; the feature still creates an all-observed mask and trains normally.
- A spectrum contains too few observed wavenumbers; the feature reports or excludes that spectrum according to documented minimum-observation rules.
- A batch has no valid spectrum pairs for distance preservation; fitting continues using the remaining valid loss terms and records that no distance pairs were available for that batch.
- Pairwise target distances encounter zero variance or zero norm over the shared observed range; those pairs are marked invalid rather than producing non-finite distances.
- Input wavenumber grids differ from the fitted model grid during encoding or reconstruction; the feature provides a clear compatibility error or documented alignment behavior.
- Metadata rows are missing, duplicated, or out of alignment with spectra; OpenSpecy object integrity checks prevent silent misalignment.
- The learned angles collapse to one region of the circle; diagnostics expose collapse through angular-spread and distance-preservation summaries.
- The source data are clustered, branching, or multi-dimensional; diagnostics avoid claiming the circular coordinate is chemically correct by default.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The package MUST provide a user-facing way to fit a masked circular spectral dimensionality-reduction model from an OpenSpecy object.
- **FR-002**: The package MUST also accept documented matrix-like tabular spectral inputs when a wavenumber grid can be supplied or inferred unambiguously.
- **FR-003**: The fitted model MUST learn exactly one circular coordinate per spectrum while storing an equivalent two-coordinate unit circular representation for stable wraparound behavior.
- **FR-004**: Missing spectral values MUST be treated as unobserved or untrusted measurements, not as zero intensity values.
- **FR-005**: The model input MUST distinguish filled spectral values from observed-value masks so that missingness information is explicit.
- **FR-006**: Reconstruction scoring MUST evaluate only truly observed spectral positions and MUST normalize each spectrum's contribution by its number of observed positions.
- **FR-007**: The feature MUST support class-free pairwise target distances based on masked correlation distance and masked spectral-angle distance.
- **FR-008**: Pairwise target spectral distances MUST use only shared finite observed wavenumbers and MUST ignore pairs that fail documented minimum overlap thresholds.
- **FR-009**: Circular distance MUST be wraparound-safe, scaled from 0 for the same circular location to 1 for opposite circular locations.
- **FR-010**: The fitting objective MUST combine masked reconstruction quality, class-free distance preservation, and an optional small angular-spread term that discourages total collapse.
- **FR-011**: Users MUST be able to configure the relative influence of reconstruction, distance preservation, and angular-spread terms.
- **FR-012**: Users MUST be able to configure overlap thresholds, training length, batch size, learning rate, validation fraction, random seed, verbosity, and hidden representation size or depth at a user-facing level.
- **FR-013**: The validation split MUST be produced without using labels, classes, material names, or other supervised metadata.
- **FR-014**: The encoder, loss function, train/validation split, and model fitting process MUST NOT use polymer identity, class labels, material names, or supervised labels.
- **FR-015**: Metadata such as polymer class, instrument, source dataset, and preprocessing type MAY be accepted only by post-training diagnostic or plotting functions.
- **FR-016**: Training-time random block masking MUST be available as an optional setting that hides contiguous observed wavenumber regions from the model input while preserving them as reconstruction targets.
- **FR-017**: The fitted model record MUST include the wavenumber grid, normalization information, preprocessing assumptions, hyperparameters, loss histories, target distance choice, random masking settings, and enough model state to encode and reconstruct compatible spectra.
- **FR-018**: Users MUST be able to encode compatible spectra with a fitted model and receive finite angles, unit circular coordinates, spectrum identifiers where available, and alignment information.
- **FR-019**: Users MUST be able to reconstruct spectra from a fitted model using encoded coordinates, supplied circular coordinates, or compatible spectra.
- **FR-020**: The package MUST provide diagnostics for reconstruction error, distance preservation, neighbor preservation, angular spread, and missingness/range relationships.
- **FR-021**: Diagnostic results MUST make clear that the circular coordinate is an empirical embedding and does not by itself prove chemical correctness.
- **FR-022**: The package MUST provide a plotting workflow for circular embeddings with optional post-training metadata coloring or labeling.
- **FR-023**: The feature MUST include an example workflow using OpenSpecy-style spectra with artificially introduced missing spectral ranges.
- **FR-024**: Automated tests MUST cover circular distance behavior, mask-aware reconstruction scoring, masked target distances, unit circular encoder outputs, no-label fitting, and a small synthetic circular-manifold validation.

### Key Entities *(include if feature involves data)*

- **Spectral Input**: An OpenSpecy object or documented matrix-like spectral representation containing a wavenumber grid, spectra, optional metadata, and possible missing values.
- **Observed-Value Mask**: A logical record of which spectrum-by-wavenumber values are truly observed and eligible as reconstruction targets.
- **Visible Training Input**: The spectral values and mask visible to the model during a training step after true missingness and optional random block masking are applied.
- **Circular Coordinate**: The learned one-dimensional angle for a spectrum, with a corresponding two-coordinate unit representation used for circular comparisons.
- **Fitted Circular Model**: The trained artifact containing model state, spectral grid, normalization settings, fitting configuration, preprocessing assumptions, and training and validation histories.
- **Reconstruction**: A spectrum predicted on the fitted model's wavenumber grid, evaluated against observed values when source masks are available.
- **Diagnostic Summary**: Reconstruction, distance-preservation, neighbor-preservation, angular-spread, and missingness/range metrics used to assess whether the embedding is meaningful.
- **Diagnostic Metadata**: Optional post-training labels or metadata used only for plotting and interpretation, never for fitting.

## Package Maintenance Impact *(mandatory)*

- **Tests**: Add focused `tests/testthat/` coverage for circular distance wraparound cases, mask-aware reconstruction scoring, masked target-distance validity and scaling, unit circular encoded outputs, no-label fitting behavior, and a lightweight synthetic circular-manifold case with missing spectral blocks. Any heavier training validation must be explicitly skipped locally or guarded for GitHub Actions/manual execution.
- **OpenSpecy object flow**: Public workflows must accept canonical `OpenSpecy` objects with `wavenumber`, `spectra`, and `metadata`, preserve spectrum identifiers and metadata alignment in outputs, respect relevant object attributes such as preprocessing state or units, and document any matrix-like conversion boundary. Matrix-like training orientation may differ internally, but user-facing behavior must remain aligned with the OpenSpecy object contract.
- **Benchmarks**: N/A for v1 because this is a new modeling capability rather than a same-output performance improvement to an existing function. Planning should still consider an optional training-time or diagnostic benchmark if performance claims are made.
- **Roxygen**: Add roxygen documentation for user-facing fitting, encoding, reconstruction, circular-distance, diagnostic, and plotting workflows. Generated help pages must be regenerated from roxygen rather than edited directly.
- **Vignettes/README/pkgdown**: Add or update an example workflow showing OpenSpecy-style spectra, artificial missing ranges, class-agnostic fitting, encoding, reconstruction, and diagnostics. README or pkgdown links should be updated only if the workflow becomes part of the recommended public surface.
- **DESCRIPTION**: Review and update dependency metadata for any modeling, plotting, or optional backend requirements selected during planning. R version and package metadata changes must remain compatible with the current package baseline unless explicitly justified.
- **NEWS.md**: Add a user-visible entry describing the new masked circular autoencoder capability, no-label training constraint, and diagnostics.
- **Generated artifacts**: Run `devtools::document()` after roxygen or export changes. Do not manually edit `NAMESPACE`, `man/*.Rd`, or generated pkgdown HTML.
- **External Shiny compatibility**: Consider whether fitted-model outputs, encoded coordinates, and diagnostic summaries can be consumed by `wincowgerDEV/OpenSpecy-shiny`, but do not add Shiny application code to this repository and keep package functionality authoritative.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: On a synthetic circular spectral data set with missing contiguous ranges, encoded angles preserve the known circular ordering with circular association of at least 0.80 in a lightweight validation run.
- **SC-002**: Circular distance returns 0 for identical angles and equivalent wraparound angles, 1 for opposite angles, and less than 0.02 for 359-degree versus 1-degree comparisons.
- **SC-003**: Mask-aware reconstruction scoring is unchanged when unobserved values are altered, demonstrating that missing positions do not affect reconstruction loss.
- **SC-004**: Pairwise target-distance calculations return finite values only for pairs meeting overlap thresholds and produce values within the documented 0-to-1 range.
- **SC-005**: Fitting succeeds on data without any label, class, or material-name column and produces finite encoded coordinates for at least 95% of spectra that meet minimum observation requirements in the test data.
- **SC-006**: Post-training diagnostics report reconstruction, distance-preservation, neighbor-preservation, and missingness/range summaries within one user-facing workflow.
- **SC-007**: Documentation includes at least one runnable example using OpenSpecy-style spectra with artificially introduced missing ranges and no supervised labels in fitting.

## Assumptions

- Input spectra have already been harmonized by the user with respect to preprocessing choices such as derivative or baseline subtraction before fitting.
- The first package-facing release prioritizes a clean experimental modeling workflow and clear diagnostics over a claim that every spectral collection is well represented by one circle.
- Matrix-like inputs can be converted to a common wavenumber grid before training, while OpenSpecy objects remain the preferred documented workflow.
- Values missing from the input are unobserved or untrusted measurements; random block masking during training is a separate temporary visibility change.
- Optional metadata used for diagnostic plots may include polymer class, material name, instrument, source dataset, or preprocessing type, but those fields are never used for fitting.
- The final modeling backend and dependency strategy will be selected during implementation planning based on package maintainability, install burden, and CRAN readiness.
