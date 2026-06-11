# Feature Plan: Library Builder Functions

**Feature dir**: `specs/002-library-builder`  
**Date**: 2026-06-11  
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Add general-purpose package functions for building Open Specy reference libraries, reduced libraries, and model libraries from raw spectra, `OpenSpecy` objects, and metadata lookup tables.
- Preserve scientific spectral meaning while replacing project paths, literal exclusions, and script-only dependency workflows with explicit inputs and OpenSpecy primitives.

## Scope

- **In**: Source merging, lookup-template creation, audited metadata joins, hierarchical material-type joins, stable IDs, duplicate/problem filtering, raw/derivative/nobaseline recipes, PAM medoid reduction, `glmnet` model builders, and library consistency summaries.
- **Out**: Bundling large generated `.rds` libraries, OSF/AWS publishing automation, Shiny app code, generic fuzzy-matching curation, and one-off curation lists baked into package functions.
- **Users**: Package users building their own libraries, and maintainers rebuilding official Open Specy libraries from future source batches.

## Requirements

- R1. Public builders accept `OpenSpecy`, lists of `OpenSpecy`, or file paths read through existing readers, and return valid `OpenSpecy` objects, model-library lists, or metadata join reports.
- R2. Spectra column names, `metadata` rows, and the chosen ID column remain aligned after merging, filtering, deduplication, processing, reduction, and metadata joins.
- R3. Hardcoded paths, known-bad IDs, metadata aliases, lookup columns, material labels, intensity-unit rules, range/resolution choices, model settings, reduction settings, and output names are user-supplied arguments with documented defaults.
- R4. Simple lookup joins behave like left joins from a metadata column to a lookup table, report unmatched metadata keys, duplicate lookup keys, and rows with missing joined values, and can fail when `require_complete = TRUE`.
- R5. Template helpers create CSV/data.table lookup templates from deduplicated standardized metadata values with blank user-fillable columns, so users can expand class/type lookup files easily.
- R6. Hierarchical material joins are generic, not polymer-specific: users supply ordered levels such as `material`, `material_class`, and `material_type`; matching proceeds from most-specific to parent levels and reports matched/unmatched values by level.
- R7. A helper can merge `OpenSpecy` metadata with a data.frame/data.table by metadata key and return either an updated `OpenSpecy` object or the joined table without breaking row/spectra alignment.
- R8. Processing recipes reuse `as_OpenSpecy()`, `read_any()`, `split_spec()`, `c_spec()`, `filter_spec()`, `manage_na()`, `process_spec()`, `adj_intens()`, `conform_spec()`, `smooth_intens()`, `subtr_baseline()`, `make_rel()`, and `sig_noise()` where applicable.
- R9. Medoid reduction uses configurable group columns, `k`, `min_n`, excluded wavenumber ranges, and preprocessing; it selects representatives with `cluster::pam()` on correlation distance and returns reduced `OpenSpecy` objects or selected IDs.
- R10. Model-library helpers use existing `glmnet` multinomial functionality and return the current model artifact structure: `model`, `dimension_conversion`, `coefficients`, `confusion`, accuracy summaries, `all_variables`, and selected variables.
- R11. Same-output script refactors include benchmarks that keep old comparison logic under `benchmarks/` and check output equivalence plus runtime regression.
- R12. Long-running full-library rebuilds, medoid reduction, model training, and large correlation checks are documented as manual or CI-guarded workflows, not routine unit tests.

## Technical Decisions

- **Approach**: Add a composable API in `R/build_lib.R`: `build_lib()`, `standardize_lib_metadata()`, `make_lib_lookup_template()`, `join_lib_metadata()`, `join_material_hierarchy()`, `dedupe_spec()`, `reduce_lib()`, `build_model_lib()`, and `assess_lib()` or close names chosen during implementation.
- **Lookup examples**: `classes_reference_2.csv` maps `SpectrumIdentity -> new_label`; `librarytypes2.csv` maps `Organization -> LibraryType`; material hierarchy examples should be expressed generically instead of as polymer-only inputs.
- **Dependencies**: Add `cluster` to `DESCRIPTION` for PAM medoid reduction. Do not add `dplyr`, `tidyr`, `stringr`, `stringdist`, `qs`, `fs`, `fuzzyjoin`, `safejoin`, `factoextra`, `TTR`, `zoo`, or plotting packages.
- **OpenSpecy contract**: Coerce inputs with `as_OpenSpecy()` before mutation. Outputs retain `wavenumber`, matrix `spectra`, row-aligned `metadata`, selected IDs as spectra column names and `metadata[[id_col]]`, and relevant attributes such as `intensity_unit`, `derivative_order`, `baseline`, and `spectra_type`.
- **Speed**: Prefer vectorized matrix/data.table operations, existing fast package helpers, and model/reduction workflows that avoid per-spectrum loops when combined-object operations are possible.
- **Generated artifacts**: Update roxygen and package metadata, then regenerate `NAMESPACE` and `man/*.Rd` with `devtools::document()`; do not edit generated files directly.

## Package Surfaces

- `R/`: Add `R/build_lib.R`; add internal helpers to existing files only when broadly reusable.
- `tests/testthat/`: Add `tests/testthat/test-build_lib.R` with small synthetic `OpenSpecy` fixtures and small lookup tables.
- `benchmarks/`: Add `benchmarks/library_builder.R` with old-script comparison snippets for same-output helper refactors.
- `vignettes/README/pkgdown`: Add or update a compact library-building and lookup-template example; defer full official rebuild walkthroughs unless requested.
- `DESCRIPTION`: Add `cluster` to imports; no other dependency additions.
- `NEWS.md`: Add a user-visible feature entry.
- External Shiny compatibility: Existing `load_lib()` outputs and official library filenames remain unchanged; no Shiny code enters this repo.

## Work Checklist

- [ ] Create `R/build_lib.R` with public builder, lookup-template, metadata-join, hierarchy-join, dedupe, reduction, model, and assessment helpers plus roxygen examples.
- [ ] Recast logic from `MergeRawFiles.R`, `CleanRawFiles.R`, and `PAM.R` into argument-driven functions using package primitives and no script-only dependencies except new `cluster`.
- [ ] Add `cluster` to `DESCRIPTION` and roxygen imports only where `cluster::pam()` is used.
- [ ] Add `tests/testthat/test-build_lib.R` covering object validity, metadata alignment, lookup completeness alerts, template creation, hierarchy joins, exclusions, recipes, medoid selection, model artifact structure, and attribute behavior.
- [ ] Add `benchmarks/library_builder.R` comparing old workflow snippets against new helpers for same-output steps.
- [ ] Update `NEWS.md` and any concise vignette/README example, then run documentation generation.

## Verification

- `devtools::test()`: required for new focused tests; long medoid/model cases skipped or CI-guarded.
- `devtools::document()`: required after roxygen/export/import changes.
- `devtools::check()` or CI/R CMD check: required before release-facing merge.
- Benchmarks: run `benchmarks/library_builder.R`; confirm equivalent outputs and no material slowdown for same-output refactors.
- Manual: optional full official-library rebuild from external source files; record library counts, join misses, medoid counts, model accuracy summaries, and known curation exclusions outside package code.

## Risks And Open Questions

- Confirm final exported function names and whether template helpers write CSV files by default or return tables unless `path` is supplied.
- Confirm canonical generic hierarchy column names for material metadata, replacing current `polymer`, `polymer_class`, and `plastic_or_not` examples.
- Decide whether model training should expose seed, class weighting, and lambda-selection controls in the first implementation or keep current defaults.

## Approval Notes

- Approved by:
- Follow-up:
