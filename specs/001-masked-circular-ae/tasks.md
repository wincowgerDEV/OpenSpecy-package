# Tasks: Masked Circular Autoencoder

**Input**: Design documents from `/specs/001-masked-circular-ae/`

**Prerequisites**: [plan.md](plan.md), [spec.md](spec.md), [research.md](research.md), [data-model.md](data-model.md), [contracts/public-r-api.md](contracts/public-r-api.md), [quickstart.md](quickstart.md)

**Tests**: Tests are required by FR-024 and the package constitution. Routine helper tests must run under `devtools::test()`. Neural-backend tests must be small and guarded with `skip_if_not_installed("torch")`. Long-running synthetic validation must be manual or GitHub Actions guarded.

**Organization**: Tasks are grouped by user story so each story can be implemented, tested, documented, and validated as an independent increment where its dependencies are met.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel because it touches different files or has no dependency on incomplete tasks.
- **[Story]**: User-story label for traceability.
- Every task includes an exact file path.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Prepare package metadata, source files, and shared test/example scaffolding for the feature.

- [ ] T001 Add guarded optional `torch` dependency metadata to `DESCRIPTION`
- [ ] T002 [P] Create source scaffold and roxygen family for public model APIs in `R/masked_circular_ae.R`
- [ ] T003 [P] Create source scaffold and roxygen family for circular and masked spectral distance helpers in `R/masked_circular_ae_distances.R`
- [ ] T004 [P] Create source scaffold and roxygen family for diagnostics and plotting helpers in `R/masked_circular_ae_diagnostics.R`
- [ ] T005 [P] Create reusable synthetic OpenSpecy test fixture helper in `tests/testthat/test-masked_circular_ae.R`
- [ ] T006 [P] Add initial user-visible feature note stub to `NEWS.md`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Build shared mask, input-preparation, validation, and backend guard helpers required by every user story.

**CRITICAL**: No user story implementation should begin until this phase is complete.

- [ ] T007 Implement `.check_masked_circular_ae_backend()` with clear `torch` availability errors in `R/masked_circular_ae.R`
- [ ] T008 Implement `.prepare_masked_circular_input()` for OpenSpecy objects and matrix-like inputs in `R/masked_circular_ae.R`
- [ ] T009 Implement `.validate_masked_circular_wavenumber()` for model/input grid compatibility in `R/masked_circular_ae.R`
- [ ] T010 Implement `.masked_circular_normalize()` and `.masked_circular_apply_normalization()` using observed finite values only in `R/masked_circular_ae.R`
- [ ] T011 Implement `.masked_circular_observed_summary()` for observed count and min/max observed wavenumber in `R/masked_circular_ae.R`
- [ ] T012 [P] Add tests for OpenSpecy input preparation, matrix input preparation, metadata alignment, and missing-value mask creation in `tests/testthat/test-masked_circular_ae.R`
- [ ] T013 [P] Add tests for mask-aware normalization and neutral filling that never treats `NA` as zero intensity in `tests/testthat/test-masked_circular_ae.R`
- [ ] T014 Confirm no same-output benchmark is required for v1 and record that decision in `specs/001-masked-circular-ae/tasks.md`

**Checkpoint**: Shared input and mask foundation is ready.

---

## Phase 3: User Story 1 - Train a Class-Agnostic Circular Spectrum Model (Priority: P1) MVP

**Goal**: Fit a masked circular autoencoder from spectra and observed-value masks without requiring or using labels.

**Independent Test**: Fit on OpenSpecy-style spectra with missing ranges and metadata label columns; verify a `MaskedCircularAEModel` is returned with spectral grid, normalization, hyperparameters, target distance type, and loss history, and that labels are not used by fitting or validation splitting.

### Tests for User Story 1

- [ ] T015 [P] [US1] Add `circ_dist()` wraparound tests for same angle, `0` vs `2*pi`, `0` vs `pi`, and 359-degree vs 1-degree cases in `tests/testthat/test-masked_circular_ae.R`
- [ ] T016 [P] [US1] Add `masked_reconstruction_loss()` tests for ignored masked positions and per-spectrum observed-count normalization in `tests/testthat/test-masked_circular_ae.R`
- [ ] T017 [P] [US1] Add `masked_spectral_distance()` tests for finite overlap, low-overlap invalid pairs, zero-variance invalid pairs, and correlation distance range in `tests/testthat/test-masked_circular_ae.R`
- [ ] T018 [P] [US1] Add no-label fitting tests that mutate class-like metadata and verify fitting inputs and validation split ignore metadata in `tests/testthat/test-masked_circular_ae.R`
- [ ] T019 [P] [US1] Add guarded tiny `torch` fit test for model object fields, finite training history, and unit latent vectors in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 1

- [ ] T020 [US1] Implement `circ_dist()` with clamped dot-product or angular wraparound behavior in `R/masked_circular_ae_distances.R`
- [ ] T021 [US1] Implement `masked_reconstruction_loss()` with per-spectrum observed-count normalization in `R/masked_circular_ae_distances.R`
- [ ] T022 [US1] Implement `masked_spectral_distance()` for `correlation` and `spectral_angle` targets with overlap weights and `NA` invalid pairs in `R/masked_circular_ae_distances.R`
- [ ] T023 [US1] Implement `.masked_circular_validation_split()` using seeded random spectrum indices only in `R/masked_circular_ae.R`
- [ ] T024 [US1] Implement `.masked_circular_module()` encoder/decoder definition with unit-circle latent normalization in `R/masked_circular_ae.R`
- [ ] T025 [US1] Implement `.masked_circular_training_step()` combining reconstruction, distance-preservation, and angular-spread losses in `R/masked_circular_ae.R`
- [ ] T026 [US1] Implement `fit_masked_circular_ae()` public API, returned `MaskedCircularAEModel` structure, model id, hyperparameter capture, and loss histories in `R/masked_circular_ae.R`
- [ ] T027 [US1] Add roxygen examples and parameter documentation for `fit_masked_circular_ae()`, `circ_dist()`, `masked_reconstruction_loss()`, and `masked_spectral_distance()` in `R/masked_circular_ae.R`
- [ ] T028 [US1] Update the feature note in `NEWS.md` with no-label training and masked-distance behavior

**Checkpoint**: MVP training works, helper losses are tested, and metadata labels are excluded from fitting.

---

## Phase 4: User Story 2 - Encode, Compare, and Reconstruct Spectra (Priority: P1)

**Goal**: Apply a fitted model to compatible spectra, compute circular distances, and reconstruct spectra on the model grid.

**Independent Test**: Fit a small guarded model, encode compatible spectra, verify finite `theta` and unit `z`, compute wraparound-safe distances, and reconstruct an OpenSpecy object aligned to the model wavenumber grid.

### Tests for User Story 2

- [ ] T029 [P] [US2] Add guarded encode tests for finite `theta`, unit `z1`/`z2`, spectrum identifiers, observed-count summaries, and wavenumber compatibility errors in `tests/testthat/test-masked_circular_ae.R`
- [ ] T030 [P] [US2] Add guarded reconstruction tests for `theta`, `z`, and `x` inputs plus exactly-one-source validation in `tests/testthat/test-masked_circular_ae.R`
- [ ] T031 [P] [US2] Add optional `as_specs = TRUE` encoded output tests for `Specs` variables, metadata `theta`, and model metadata attachment in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 2

- [ ] T032 [US2] Implement `.check_masked_circular_model()` for model class, grid, normalization, and backend state validation in `R/masked_circular_ae.R`
- [ ] T033 [US2] Implement `encode_masked_circular_ae()` with OpenSpecy and matrix-like input handling in `R/masked_circular_ae.R`
- [ ] T034 [US2] Implement optional `as_specs = TRUE` conversion for encoded `z1`/`z2` coordinates in `R/masked_circular_ae.R`
- [ ] T035 [US2] Implement `reconstruct_masked_circular_ae()` for `theta`, `z`, and `x` sources returning an OpenSpecy object in `R/masked_circular_ae.R`
- [ ] T036 [US2] Preserve reconstruction metadata alignment and model attributes through `as_OpenSpecy()` in `R/masked_circular_ae.R`
- [ ] T037 [US2] Add roxygen documentation for `encode_masked_circular_ae()` and `reconstruct_masked_circular_ae()` in `R/masked_circular_ae.R`

**Checkpoint**: Users can encode, compare, and reconstruct spectra from a fitted model.

---

## Phase 5: User Story 3 - Diagnose Whether the Circle Preserves Spectral Structure (Priority: P2)

**Goal**: Provide post-training diagnostics for reconstruction quality, distance preservation, neighbor preservation, angular spread, and missingness/range relationships.

**Independent Test**: Run diagnostics on a fitted model and spectra without labels; verify reconstruction, distance-preservation, neighbor-preservation, angular-spread, and missingness/range components are returned and handle insufficient data cleanly.

### Tests for User Story 3

- [ ] T038 [P] [US3] Add reconstruction diagnostic tests for masked RMSE, masked correlation, and insufficient-observation handling in `tests/testthat/test-masked_circular_ae.R`
- [ ] T039 [P] [US3] Add distance-preservation diagnostic tests for sampled pair output, Pearson/Spearman summaries, invalid-pair filtering, and bounded pair counts in `tests/testthat/test-masked_circular_ae.R`
- [ ] T040 [P] [US3] Add neighbor-preservation and missingness/range diagnostic tests for configurable `k`, observed counts, and min/max observed wavenumber in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 3

- [ ] T041 [US3] Implement `.masked_circular_reconstruction_diagnostics()` in `R/masked_circular_ae_diagnostics.R`
- [ ] T042 [US3] Implement `.masked_circular_distance_preservation()` with pair sampling and correlation summaries in `R/masked_circular_ae_diagnostics.R`
- [ ] T043 [US3] Implement `.masked_circular_neighbor_preservation()` for original-distance and circular-distance nearest-neighbor overlap in `R/masked_circular_ae_diagnostics.R`
- [ ] T044 [US3] Implement `.masked_circular_missingness_range_diagnostics()` and angular-spread summaries in `R/masked_circular_ae_diagnostics.R`
- [ ] T045 [US3] Implement `diagnose_masked_circular_ae()` public API and insufficient-data messages in `R/masked_circular_ae_diagnostics.R`
- [ ] T046 [US3] Add roxygen documentation for diagnostics and empirical-embedding interpretation warnings in `R/masked_circular_ae_diagnostics.R`

**Checkpoint**: Diagnostics can judge whether the learned circle is useful before scientific interpretation.

---

## Phase 6: User Story 4 - Plot Circular Embeddings With Optional Post-Training Metadata (Priority: P3)

**Goal**: Plot encoded circular coordinates and optionally color or label by metadata after training.

**Independent Test**: Plot encoded spectra with and without metadata and verify metadata affect only the plot data/display, not model state or loss history.

### Tests for User Story 4

- [ ] T047 [P] [US4] Add plot data preparation tests for metadata joins, missing metadata fields, and no model mutation in `tests/testthat/test-masked_circular_ae.R`
- [ ] T048 [P] [US4] Add guarded `ggplot2` plot-object test for `plot_circular_embedding()` in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 4

- [ ] T049 [US4] Implement `.prepare_circular_embedding_plot_data()` with post-training metadata joins only in `R/masked_circular_ae_diagnostics.R`
- [ ] T050 [US4] Implement `plot_circular_embedding()` with optional `color_by` and `label_by` support in `R/masked_circular_ae_diagnostics.R`
- [ ] T051 [US4] Add roxygen documentation that metadata are diagnostic-only in `R/masked_circular_ae_diagnostics.R`

**Checkpoint**: Users can visualize embeddings without accidentally introducing supervised metadata into training.

---

## Phase 7: User Story 5 - Learn Robustly From Partially Hidden Spectral Regions (Priority: P3)

**Goal**: Add optional random contiguous block masking during training while preserving original observed values as reconstruction targets.

**Independent Test**: Enable random block masking during guarded fitting and verify hidden blocks are removed from encoder-visible input but remain valid reconstruction targets under the original observed mask.

### Tests for User Story 5

- [ ] T052 [P] [US5] Add random block mask tests for contiguous hidden ranges, visible-mask dimensions, and preservation of original observed target mask in `tests/testthat/test-masked_circular_ae.R`
- [ ] T053 [P] [US5] Add guarded fit test comparing `random_block_mask = TRUE` and `FALSE` settings captured in model metadata in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 5

- [ ] T054 [US5] Implement `.apply_random_spectral_block_mask()` using the original observed mask and `block_mask_fraction` in `R/masked_circular_ae.R`
- [ ] T055 [US5] Integrate random block masking into `.masked_circular_training_step()` without changing reconstruction targets in `R/masked_circular_ae.R`
- [ ] T056 [US5] Store random block masking settings and per-epoch availability summaries in the fitted model history in `R/masked_circular_ae.R`
- [ ] T057 [US5] Add roxygen documentation for `random_block_mask` and `block_mask_fraction` behavior in `R/masked_circular_ae.R`

**Checkpoint**: Training can hide contiguous spectral regions from the encoder while preserving true observed targets.

---

## Phase 8: Documentation, Examples, Generated Artifacts, and Validation

**Purpose**: Complete package-facing documentation, example workflow, and validation gates required by the constitution.

- [ ] T058 [P] Add runnable OpenSpecy-style example script with artificial missing ranges in `inst/examples/masked_circular_ae.R`
- [ ] T059 [P] Add optional long-form workflow vignette centered on OpenSpecy objects in `vignettes/masked-circular-autoencoder.Rmd`
- [ ] T060 [P] Update README or pkgdown references only if the feature becomes part of the recommended public workflow in `README.md`
- [ ] T061 Run `devtools::document()` to regenerate `NAMESPACE` and `man/*.Rd` from roxygen changes
- [ ] T062 Verify generated `NAMESPACE` exports the new public functions and no `man/*.Rd` file was edited by hand in `NAMESPACE`
- [ ] T063 Run `devtools::test()` and record any skipped `torch` or long-running tests in `specs/001-masked-circular-ae/tasks.md`
- [ ] T064 Run the quickstart workflow or a guarded shortened equivalent from `specs/001-masked-circular-ae/quickstart.md`
- [ ] T065 Run `devtools::check()` or document equivalent GitHub Actions/R CMD check coverage in `specs/001-masked-circular-ae/tasks.md`
- [ ] T066 Verify no Shiny application code was added and note OpenSpecy-shiny compatibility considerations in `NEWS.md`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies; can start immediately.
- **Foundational (Phase 2)**: Depends on Setup completion and blocks user stories.
- **US1 Training MVP (Phase 3)**: Depends on Foundational.
- **US2 Encode/Reconstruct (Phase 4)**: Depends on US1 model object and foundational distance helpers.
- **US3 Diagnostics (Phase 5)**: Depends on US1 and US2 outputs.
- **US4 Plotting (Phase 6)**: Depends on US2 encoded output; can proceed in parallel with US3 after US2.
- **US5 Random Block Masking (Phase 7)**: Depends on US1 training loop; can proceed in parallel with US3/US4 after US1 if file ownership is coordinated.
- **Documentation and Validation (Phase 8)**: Depends on selected stories being complete.

### User Story Dependencies

- **User Story 1 (P1)**: MVP; no story dependencies after foundation.
- **User Story 2 (P1)**: Requires a fitted model from US1.
- **User Story 3 (P2)**: Requires fitted model and encoded/reconstructed outputs from US1/US2.
- **User Story 4 (P3)**: Requires encoded output from US2.
- **User Story 5 (P3)**: Requires US1 training loop; reinforces robustness of training.

### Within Each User Story

- Add tests first for the story-specific contract.
- Implement helpers before public APIs.
- Update roxygen in the same source file as behavior.
- Keep metadata out of fitting, validation splitting, encoder inputs, and losses.
- Keep OpenSpecy alignment and attributes intact before examples and final validation.
- Run `devtools::document()` only after roxygen/export changes are complete.

### Parallel Opportunities

- T002, T003, T004, T005, and T006 can run in parallel after T001 is understood.
- T012, T013, T015, T016, and T017 can be drafted in parallel because they target distinct test expectations.
- T020, T021, and T022 can be implemented in parallel only if source-file ownership is coordinated within `R/masked_circular_ae_distances.R`.
- US3 diagnostic helpers T041 through T044 can be split across workers within `R/masked_circular_ae_diagnostics.R` only with careful non-overlapping sections.
- US4 plotting tasks can run in parallel with US5 random masking after US2 and US1 respectively.
- T058, T059, and T060 can run in parallel after public APIs stabilize.

---

## Implementation Strategy

### MVP First

1. Complete Phase 1 and Phase 2.
2. Complete Phase 3 only.
3. Validate with helper tests plus guarded tiny `torch` fit test.
4. Confirm users can train a no-label masked circular model and inspect loss history.

### Incremental Delivery

1. Add US1 training MVP.
2. Add US2 encoding, circular comparison, and reconstruction.
3. Add US3 diagnostics before encouraging scientific interpretation.
4. Add US4 plotting for post-training metadata inspection.
5. Add US5 random block masking robustness.
6. Complete docs, generated artifacts, and package checks.

### Quality Gates

- `devtools::test()` passes with optional backend skips clearly reported.
- `devtools::document()` regenerates exports and help pages; no direct edits to `NAMESPACE` or `man/*.Rd`.
- `devtools::check()` or equivalent CI/R CMD check coverage is documented before release-facing work.
- No class labels, polymer identities, material names, or supervised metadata enter fitting, splitting, encoder input, or losses.
- Long-running training validation remains manual or GitHub Actions guarded.

## Notes

- No same-output benchmark is required for v1 because this is a new modeling capability, not a replacement or optimization of an existing function.
- Any future performance claim for training or diagnostics should add a benchmark under `benchmarks/`.
- Metadata are allowed only after training for diagnostics and plotting.
- The Shiny app remains outside this repository; keep outputs stable enough for future consumption without adding Shiny application code.
