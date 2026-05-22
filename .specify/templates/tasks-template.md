---

description: "Task list template for OpenSpecy R package feature implementation"
---

# Tasks: [FEATURE NAME]

**Input**: Design documents from `/specs/[###-feature-name]/`

**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are REQUIRED for behavior changes. Include `tests/testthat/`
tasks for each affected user story or document the approved exception in
plan.md. Tests cover current package behavior only; old implementations used
for performance comparison belong in `benchmarks/`. Long-running tests must be
manual or GitHub Actions guarded.

**Organization**: Tasks are grouped by user story to enable independent
implementation, testing, documentation, and validation of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Source and roxygen: `R/[topic].R`
- Tests: `tests/testthat/test-[topic].R`
- Benchmarks: `benchmarks/[topic].R`
- Vignettes: `vignettes/[topic].Rmd` plus supporting assets
- Package metadata: `DESCRIPTION`
- Release notes: `NEWS.md`
- Generated roxygen output: `NAMESPACE` and `man/*.Rd` via `devtools::document()` only
- Website output: `docs/` via pkgdown tooling only

<!--
  ============================================================================
  IMPORTANT: The tasks below are SAMPLE TASKS for illustration purposes only.

  The /speckit-tasks command MUST replace these with actual tasks based on:
  - User stories from spec.md (with their priorities P1, P2, P3...)
  - Package maintenance impact from spec.md
  - Feature requirements from plan.md
  - Entities from data-model.md
  - Public function and object expectations from contracts/

  Tasks MUST be organized by user story so each story can be:
  - Implemented independently
  - Tested independently
  - Documented independently
  - Delivered as an MVP increment

  DO NOT keep these sample tasks in the generated tasks.md file.
  ============================================================================
-->

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Confirm package context and prepare shared scaffolding

- [ ] T001 Confirm affected R package surfaces from plan.md
- [ ] T002 [P] Identify existing tests in `tests/testthat/` that cover the affected behavior
- [ ] T003 [P] Identify existing benchmarks in `benchmarks/` for affected same-output function improvements
- [ ] T004 [P] Identify affected roxygen blocks, vignettes, `DESCRIPTION`, and `NEWS.md`
- [ ] T005 [P] Identify external OpenSpecy-shiny compatibility impact, if any, without adding Shiny application code to this repository

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core decisions that MUST be complete before user story work begins

**CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T006 Define scientific/data integrity expectations and numerical tolerances
- [ ] T007 Define `OpenSpecy` object invariants for this feature: `wavenumber`, `spectra`, `metadata`, object attributes via `attr()`, alignment, coercion, and examples
- [ ] T008 Define function compatibility and migration behavior
- [ ] T009 Define test data, fixtures, or representative spectra needed for validation
- [ ] T010 Identify any long-running tests and decide whether they are manual or GitHub Actions guarded
- [ ] T011 Define benchmark strategy for same-output function improvements, including previous implementation retention in `benchmarks/`
- [ ] T012 Confirm generated-file strategy: roxygen sources first, then `devtools::document()`

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel.

---

## Phase 3: User Story 1 - [Title] (Priority: P1) MVP

**Goal**: [Brief description of what this story delivers]

**Independent Test**: [How to verify this story works on its own]

### Tests for User Story 1

> **NOTE: Write or update these tests FIRST and confirm they fail when they are regression tests.**

- [ ] T013 [P] [US1] Add or update testthat coverage in `tests/testthat/test-[topic].R`
- [ ] T014 [P] [US1] Add regression or edge-case fixture coverage for [scenario]
- [ ] T015 [P] [US1] Add manual or GitHub Actions guard for long-running test coverage, if needed
- [ ] T016 [P] [US1] Add or update benchmark in `benchmarks/[topic].R` for same-output function improvement, retaining the previous implementation there

### Implementation for User Story 1

- [ ] T017 [US1] Implement package behavior in `R/[topic].R`
- [ ] T018 [US1] Preserve or intentionally update `OpenSpecy` object structure, attributes, and alignment through `as_OpenSpecy()`, `OpenSpecy()`, or documented conversion helpers
- [ ] T019 [US1] Preserve package-first behavior while considering external OpenSpecy-shiny compatibility when relevant
- [ ] T020 [US1] Update roxygen comments in `R/[topic].R`
- [ ] T021 [US1] Update affected vignette or README examples, centering `OpenSpecy` object flow when relevant
- [ ] T022 [US1] Update `DESCRIPTION` if dependencies, metadata, or package config change
- [ ] T023 [US1] Add `NEWS.md` entry for user-visible impact

**Checkpoint**: User Story 1 is functional, tested, documented, and independently verifiable.

---

## Phase 4: User Story 2 - [Title] (Priority: P2)

**Goal**: [Brief description of what this story delivers]

**Independent Test**: [How to verify this story works on its own]

### Tests for User Story 2

- [ ] T024 [P] [US2] Add or update testthat coverage in `tests/testthat/test-[topic].R`
- [ ] T025 [P] [US2] Add regression or edge-case fixture coverage for [scenario]
- [ ] T026 [P] [US2] Add manual or GitHub Actions guard for long-running test coverage, if needed
- [ ] T027 [P] [US2] Add or update benchmark in `benchmarks/[topic].R` for same-output function improvement, retaining the previous implementation there

### Implementation for User Story 2

- [ ] T028 [US2] Implement package behavior in `R/[topic].R`
- [ ] T029 [US2] Preserve or intentionally update `OpenSpecy` object structure, attributes, and alignment
- [ ] T030 [US2] Update roxygen comments in `R/[topic].R`
- [ ] T031 [US2] Update vignettes, `DESCRIPTION`, or `NEWS.md` as required by spec.md

**Checkpoint**: User Stories 1 and 2 both work independently.

---

## Phase 5: User Story 3 - [Title] (Priority: P3)

**Goal**: [Brief description of what this story delivers]

**Independent Test**: [How to verify this story works on its own]

### Tests for User Story 3

- [ ] T032 [P] [US3] Add or update testthat coverage in `tests/testthat/test-[topic].R`
- [ ] T033 [P] [US3] Add regression or edge-case fixture coverage for [scenario]
- [ ] T034 [P] [US3] Add manual or GitHub Actions guard for long-running test coverage, if needed
- [ ] T035 [P] [US3] Add or update benchmark in `benchmarks/[topic].R` for same-output function improvement, retaining the previous implementation there

### Implementation for User Story 3

- [ ] T036 [US3] Implement package behavior in `R/[topic].R`
- [ ] T037 [US3] Preserve or intentionally update `OpenSpecy` object structure, attributes, and alignment
- [ ] T038 [US3] Update roxygen comments in `R/[topic].R`
- [ ] T039 [US3] Update vignettes, `DESCRIPTION`, or `NEWS.md` as required by spec.md

**Checkpoint**: All user stories are independently functional.

---

[Add more user story phases as needed, following the same pattern]

---

## Phase N: Documentation, Generated Artifacts, and Validation

**Purpose**: Package-level checks and generated outputs required by the constitution

- [ ] TXXX Run `devtools::document()` after roxygen, export, method, or package documentation changes
- [ ] TXXX Verify generated `NAMESPACE` and `man/*.Rd` reflect roxygen changes without direct hand edits
- [ ] TXXX Verify `OpenSpecy` object invariants, attributes, and examples remain centered in affected function flows
- [ ] TXXX Run or update `benchmarks/[topic].R` for same-output function improvements; record output equivalence and ensure runtime is not substantially slower (~>10%)
- [ ] TXXX Verify long-running tests are manual or GitHub Actions guarded
- [ ] TXXX Verify external OpenSpecy-shiny compatibility was considered when relevant and no Shiny application code was added
- [ ] TXXX [P] Update or validate vignettes in `vignettes/`
- [ ] TXXX [P] Update `NEWS.md` for user-visible changes
- [ ] TXXX [P] Update `DESCRIPTION` for dependency, metadata, R version, or config changes
- [ ] TXXX Run `devtools::test()` and record results
- [ ] TXXX Run `devtools::check()` or document the equivalent CI/R CMD check validation
- [ ] TXXX Run quickstart.md validation if present for this feature

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - blocks all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
- **Documentation and Validation (Final Phase)**: Depends on desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2)
- **User Story 2 (P2)**: Can start after Foundational (Phase 2); may integrate with US1 but remains independently testable
- **User Story 3 (P3)**: Can start after Foundational (Phase 2); may integrate with US1/US2 but remains independently testable

### Within Each User Story

- Tests before implementation
- Benchmark scaffold before same-output optimization work
- R source and roxygen updates together
- OpenSpecy object invariants and attributes before examples and validation
- Core implementation before vignette or README examples
- `DESCRIPTION` and `NEWS.md` updates before final validation
- `devtools::document()` after roxygen/export changes
- Story complete before moving to next priority unless parallel work uses disjoint files

### Parallel Opportunities

- Discovery tasks that read different files can run in parallel
- Tests for different user stories can run in parallel when they touch different files
- R source work can run in parallel only when ownership of files is disjoint
- Documentation updates can run in parallel with validation once behavior is stable

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1
4. Stop and validate tests, docs, and generated artifacts for User Story 1
5. Demo or review if ready

### Incremental Delivery

1. Complete Setup and Foundation
2. Add User Story 1, test independently, document, validate
3. Add User Story 2, test independently, document, validate
4. Add User Story 3, test independently, document, validate
5. Run final package validation

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to a user story for traceability
- Each user story must be independently completable and testable
- Avoid direct edits to `NAMESPACE`, `man/*.Rd`, and generated pkgdown HTML
- Keep previous implementations for speed comparison in `benchmarks/`, not in `tests/`
- Center `OpenSpecy` object examples unless a lower-level helper is the explicit subject
- Keep long-running tests manual or GitHub Actions guarded
- Do not add Shiny application code to this package repository; consider external compatibility only
- Avoid vague tasks, same-file conflicts, and hidden documentation work
