# Tasks: Masked Circular Autoencoder

**Input**: Design documents from `/specs/001-masked-circular-ae/`

**Prerequisites**: [plan.md](plan.md), [spec.md](spec.md), [research.md](research.md), [data-model.md](data-model.md), [contracts/public-r-api.md](contracts/public-r-api.md), [quickstart.md](quickstart.md)

**Tests**: Tests are required by FR-024 and the package constitution. Routine helper tests and tiny R-native model workflow tests must run under `devtools::test()`. Long-running synthetic validation must be manual or GitHub Actions guarded.

**Organization**: Tasks are grouped by user story so each story can be implemented, tested, documented, and validated as an independent increment where its dependencies are met.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel because it touches different files or has no dependency on incomplete tasks.
- **[Story]**: User-story label for traceability.
- Every task includes an exact file path.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Prepare package metadata, source files, and shared test/example scaffolding for the feature.

- [X] T001 Confirm no new modeling dependency metadata is required in `DESCRIPTION`
- [X] T002 [P] Create source scaffold and roxygen family for public model APIs in `R/masked_circular_ae.R`
- [X] T003 [P] Create source scaffold and roxygen family for circular and masked spectral distance helpers in `R/masked_circular_ae_distances.R`
- [X] T004 [P] Create source scaffold and roxygen family for diagnostics and plotting helpers in `R/masked_circular_ae_diagnostics.R`
- [X] T005 [P] Create reusable synthetic OpenSpecy test fixture helper in `tests/testthat/test-masked_circular_ae.R`
- [X] T006 [P] Add initial user-visible feature note stub to `NEWS.md`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Build shared mask, input-preparation, validation, and R-native model helpers required by every user story.

**CRITICAL**: No user story implementation should begin until this phase is complete.

- [X] T007 Remove external backend guards so fitting uses only package imports and base `stats` in `R/masked_circular_ae.R`
- [X] T008 Implement `.prepare_masked_circular_input()` for OpenSpecy objects and matrix-like inputs in `R/masked_circular_ae.R`
- [X] T009 Implement `.validate_masked_circular_wavenumber()` for model/input grid compatibility in `R/masked_circular_ae.R`
- [X] T010 Implement `.masked_circular_normalize()` and `.masked_circular_apply_normalization()` using observed finite values only in `R/masked_circular_ae.R`
- [X] T011 Implement `.masked_circular_observed_summary()` for observed count and min/max observed wavenumber in `R/masked_circular_ae.R`
- [X] T012 [P] Add tests for OpenSpecy input preparation, matrix input preparation, metadata alignment, and missing-value mask creation in `tests/testthat/test-masked_circular_ae.R`
- [X] T013 [P] Add tests for mask-aware normalization and neutral filling that never treats `NA` as zero intensity in `tests/testthat/test-masked_circular_ae.R`
- [X] T014 Confirm no same-output benchmark is required for v1 and record that decision in `specs/001-masked-circular-ae/tasks.md`

**Checkpoint**: Shared input and mask foundation is ready.

---

## Phase 3: User Story 1 - Train a Class-Agnostic Circular Spectrum Model (Priority: P1) MVP

**Goal**: Fit a masked circular autoencoder from spectra and observed-value masks without requiring or using labels.

**Independent Test**: Fit on OpenSpecy-style spectra with missing ranges and metadata label columns; verify a `MaskedCircularAEModel` is returned with spectral grid, normalization, hyperparameters, target distance type, and loss history, and that labels are not used by fitting or validation splitting.

### Tests for User Story 1

- [X] T015 [P] [US1] Add `circ_dist()` wraparound tests for same angle, `0` vs `360`, `0` vs `180`, and 359-degree vs 1-degree cases in `tests/testthat/test-masked_circular_ae.R`
- [X] T016 [P] [US1] Add `masked_reconstruction_loss()` tests for ignored masked positions and per-spectrum observed-count normalization in `tests/testthat/test-masked_circular_ae.R`
- [X] T017 [P] [US1] Add `masked_spectral_distance()` tests for finite overlap, low-overlap invalid pairs, zero-variance invalid pairs, and correlation distance range in `tests/testthat/test-masked_circular_ae.R`
- [X] T018 [P] [US1] Add no-label fitting tests that mutate class-like metadata and verify fitting inputs and validation split ignore metadata in `tests/testthat/test-masked_circular_ae.R`
- [X] T019 [P] [US1] Add tiny R-native fit test for model object fields, finite training history, and unit latent vectors in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 1

- [X] T020 [US1] Implement `circ_dist()` with clamped dot-product or angular wraparound behavior in `R/masked_circular_ae_distances.R`
- [X] T021 [US1] Implement `masked_reconstruction_loss()` with per-spectrum observed-count normalization in `R/masked_circular_ae_distances.R`
- [X] T022 [US1] Implement `masked_spectral_distance()` for `correlation` and `spectral_angle` targets with overlap weights and `NA` invalid pairs in `R/masked_circular_ae_distances.R`
- [X] T023 [US1] Implement `.masked_circular_validation_split()` using seeded random spectrum indices only in `R/masked_circular_ae.R`
- [X] T024 [US1] Implement R-native mask-aware circular embedding with unit-circle latent normalization in `R/masked_circular_ae.R`
- [X] T025 [US1] Implement periodic least-squares decoder and loss evaluation combining reconstruction, distance-preservation, and angular-spread summaries in `R/masked_circular_ae.R`
- [X] T026 [US1] Implement `fit_masked_circular_ae()` public API, returned `MaskedCircularAEModel` structure, model id, hyperparameter capture, and loss histories in `R/masked_circular_ae.R`
- [X] T027 [US1] Add roxygen examples and parameter documentation for `fit_masked_circular_ae()`, `circ_dist()`, `masked_reconstruction_loss()`, and `masked_spectral_distance()` in `R/masked_circular_ae.R`
- [X] T028 [US1] Update the feature note in `NEWS.md` with no-label training and masked-distance behavior

**Checkpoint**: MVP training works, helper losses are tested, and metadata labels are excluded from fitting.

---

## Phase 4: User Story 2 - Encode, Compare, and Reconstruct Spectra (Priority: P1)

**Goal**: Apply a fitted model to compatible spectra, compute circular distances, and reconstruct spectra on the model grid.

**Independent Test**: Fit a small R-native model, encode compatible spectra, verify finite `theta` and unit `z`, compute wraparound-safe distances, and reconstruct an OpenSpecy object aligned to the model wavenumber grid.

### Tests for User Story 2

- [X] T029 [P] [US2] Add encode tests for finite degree-valued `theta`, spectrum identifiers, observed-count summaries, and wavenumber compatibility errors in `tests/testthat/test-masked_circular_ae.R`
- [X] T030 [P] [US2] Add reconstruction tests for `theta`, `z`, and `x` inputs plus exactly-one-source validation in `tests/testthat/test-masked_circular_ae.R`
- [X] T031 [P] [US2] Add optional `as_specs = TRUE` encoded output tests for `Specs` variables, metadata `theta`, and model metadata attachment in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 2

- [X] T032 [US2] Implement `.check_masked_circular_model()` for model class, grid, normalization, and backend state validation in `R/masked_circular_ae.R`
- [X] T033 [US2] Implement `encode_masked_circular_ae()` with OpenSpecy and matrix-like input handling in `R/masked_circular_ae.R`
- [X] T034 [US2] Implement optional `as_specs = TRUE` conversion with one latent `theta` value per spectrum in `R/masked_circular_ae.R`
- [X] T035 [US2] Implement `reconstruct_masked_circular_ae()` for `theta` and `x` sources returning an OpenSpecy object in `R/masked_circular_ae.R`
- [X] T036 [US2] Preserve reconstruction metadata alignment and model attributes through `as_OpenSpecy()` in `R/masked_circular_ae.R`
- [X] T037 [US2] Add roxygen documentation for `encode_masked_circular_ae()` and `reconstruct_masked_circular_ae()` in `R/masked_circular_ae.R`

**Checkpoint**: Users can encode, compare, and reconstruct spectra from a fitted model.

---

## Phase 5: User Story 3 - Diagnose Whether the Circle Preserves Spectral Structure (Priority: P2)

**Goal**: Provide post-training diagnostics for reconstruction quality, distance preservation, neighbor preservation, angular spread, and missingness/range relationships.

**Independent Test**: Run diagnostics on a fitted model and spectra without labels; verify reconstruction, distance-preservation, neighbor-preservation, angular-spread, and missingness/range components are returned and handle insufficient data cleanly.

### Tests for User Story 3

- [X] T038 [P] [US3] Add reconstruction diagnostic tests for masked RMSE, masked correlation, and insufficient-observation handling in `tests/testthat/test-masked_circular_ae.R`
- [X] T039 [P] [US3] Add distance-preservation diagnostic tests for sampled pair output, Pearson/Spearman summaries, invalid-pair filtering, and bounded pair counts in `tests/testthat/test-masked_circular_ae.R`
- [X] T040 [P] [US3] Add neighbor-preservation and missingness/range diagnostic tests for configurable `k`, observed counts, and min/max observed wavenumber in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 3

- [X] T041 [US3] Implement `.masked_circular_reconstruction_diagnostics()` in `R/masked_circular_ae_diagnostics.R`
- [X] T042 [US3] Implement `.masked_circular_distance_preservation()` with pair sampling and correlation summaries in `R/masked_circular_ae_diagnostics.R`
- [X] T043 [US3] Implement `.masked_circular_neighbor_preservation()` for original-distance and circular-distance nearest-neighbor overlap in `R/masked_circular_ae_diagnostics.R`
- [X] T044 [US3] Implement `.masked_circular_missingness_range_diagnostics()` and angular-spread summaries in `R/masked_circular_ae_diagnostics.R`
- [X] T045 [US3] Implement `diagnose_masked_circular_ae()` public API and insufficient-data messages in `R/masked_circular_ae_diagnostics.R`
- [X] T046 [US3] Add roxygen documentation for diagnostics and empirical-embedding interpretation warnings in `R/masked_circular_ae_diagnostics.R`

**Checkpoint**: Diagnostics can judge whether the learned circle is useful before scientific interpretation.

---

## Phase 6: User Story 4 - Plot Circular Embeddings With Optional Post-Training Metadata (Priority: P3)

**Goal**: Plot encoded circular coordinates and optionally color or label by metadata after training.

**Independent Test**: Plot encoded spectra with and without metadata and verify metadata affect only the plot data/display, not model state or loss history.

### Tests for User Story 4

- [X] T047 [P] [US4] Add plot data preparation tests for metadata joins, missing metadata fields, and no model mutation in `tests/testthat/test-masked_circular_ae.R`
- [X] T048 [P] [US4] Add base graphics plot test for `plot_circular_embedding()` in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 4

- [X] T049 [US4] Implement `.prepare_circular_embedding_plot_data()` with post-training metadata joins only in `R/masked_circular_ae_diagnostics.R`
- [X] T050 [US4] Implement base R `plot_circular_embedding()` with optional `color_by` and `label_by` support in `R/masked_circular_ae_diagnostics.R`
- [X] T051 [US4] Add roxygen documentation that metadata are diagnostic-only in `R/masked_circular_ae_diagnostics.R`

**Checkpoint**: Users can visualize embeddings without accidentally introducing supervised metadata into training.

---

## Phase 7: User Story 5 - Preserve One-Value Embedding Economy (Priority: P3)

**Goal**: Keep the encoded representation to a single degree-valued `theta` per spectrum, including the `Specs` representation.

**Independent Test**: Encode spectra and request `as_specs = TRUE`; verify one latent row named `theta` and no saved cosine/sine latent columns.

### Tests for User Story 5

- [X] T052 [P] [US5] Add tests that encoded output contains `theta` but no saved `z1`/`z2` columns in `tests/testthat/test-masked_circular_ae.R`
- [X] T053 [P] [US5] Add `Specs` test verifying a single latent variable named `theta` in `tests/testthat/test-masked_circular_ae.R`

### Implementation for User Story 5

- [X] T054 [US5] Remove unused neural-era tuning and random masking arguments from `fit_masked_circular_ae()` in `R/masked_circular_ae.R`
- [X] T055 [US5] Store only `theta` as the public encoded value while computing cosine/sine terms privately when needed in `R/masked_circular_ae.R`
- [X] T056 [US5] Update reconstruction and diagnostics to consume degree-valued `theta` directly in `R/masked_circular_ae.R` and `R/masked_circular_ae_diagnostics.R`
- [X] T057 [US5] Add roxygen documentation for the one-value degree embedding in `R/masked_circular_ae.R`

**Checkpoint**: Training can hide contiguous spectral regions from the encoder while preserving true observed targets.

---

## Phase 8: Documentation, Examples, Generated Artifacts, and Validation

**Purpose**: Complete package-facing documentation, example workflow, and validation gates required by the constitution.

- [X] T058 [P] Add runnable OpenSpecy-style example script with artificial missing ranges in `inst/examples/masked_circular_ae.R`
- [X] T059 [P] Add optional long-form workflow vignette centered on OpenSpecy objects in `vignettes/masked-circular-autoencoder.Rmd`
- [X] T060 [P] Update README or pkgdown references only if the feature becomes part of the recommended public workflow in `README.md`
- [X] T061 Run `devtools::document()` to regenerate `NAMESPACE` and `man/*.Rd` from roxygen changes
- [X] T062 Verify generated `NAMESPACE` exports the new public functions and no `man/*.Rd` file was edited by hand in `NAMESPACE`
- [X] T063 Run `devtools::test()` and record any long-running or network-dependent test limitations in `specs/001-masked-circular-ae/tasks.md`
- [X] T064 Run the quickstart workflow or a guarded shortened equivalent from `specs/001-masked-circular-ae/quickstart.md`
- [X] T065 Run `devtools::check()` or document equivalent GitHub Actions/R CMD check coverage in `specs/001-masked-circular-ae/tasks.md`
- [X] T066 Verify no Shiny application code was added and note OpenSpecy-shiny compatibility considerations in `NEWS.md`

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
3. Validate with helper tests plus a tiny R-native fit test.
4. Confirm users can train a no-label masked circular model and inspect loss history.

### Incremental Delivery

1. Add US1 training MVP.
2. Add US2 encoding, circular comparison, and reconstruction.
3. Add US3 diagnostics before encouraging scientific interpretation.
4. Add US4 plotting for post-training metadata inspection.
5. Add US5 random block masking robustness.
6. Complete docs, generated artifacts, and package checks.

### Quality Gates

- `devtools::test()` passes with the R-native backend exercised for masked circular AE tests.
- `devtools::document()` regenerates exports and help pages; no direct edits to `NAMESPACE` or `man/*.Rd`.
- `devtools::check()` or equivalent CI/R CMD check coverage is documented before release-facing work.
- No class labels, polymer identities, material names, or supervised metadata enter fitting, splitting, encoder input, or losses.
- Long-running training validation remains manual or GitHub Actions guarded.

## Notes

- No same-output benchmark is required for v1 because this is a new modeling capability, not a replacement or optimization of an existing function.
- Any future performance claim for training or diagnostics should add a benchmark under `benchmarks/`.
- Metadata are allowed only after training for diagnostics and plotting.
- The Shiny app remains outside this repository; keep outputs stable enough for future consumption without adding Shiny application code.

## Implementation Validation Notes

- Replaced the torch prototype with an R-native `stats` backend using mask-aware circular embedding and periodic least-squares reconstruction.
- Removed `torch` from `DESCRIPTION` imports and removed roxygen `importFrom(torch, ...)` entries.
- `devtools::load_all(".")` and `devtools::test(filter = "masked_circular_ae")` passed with 52 passing assertions, 0 warnings, and 0 skips using the R-native single-theta backend.
- `devtools::document()` completed and regenerated `NAMESPACE` plus `man/*.Rd` outputs from roxygen source.
- `devtools::test()` passed with 690 passing assertions, 30 existing expected-warning reports, and 3 skips. The OSF model library download test now runs only on GitHub Actions and is skipped locally.
- `inst/examples/masked_circular_ae.R` was sourced successfully through `devtools::load_all(".")`.
- `devtools::check(args = c("--no-tests", "--no-manual"), env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false"), error_on = "never")` built the package, examples, and vignettes with 0 errors and 0 warnings. Remaining notes are environmental future-time verification and an existing `run_app()` global-variable note for `.rs.invokeShinyWindowExternal`.
- README/pkgdown references were not changed because this experimental backend is not yet a recommended default public workflow.

