# Feature Plan: Streamlined Reference Library Workflow

**Feature dir**: `specs/002-library-builder`
**Date**: 2026-06-19
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Make file path(s), one `OpenSpecy`, or a list of `OpenSpecy` objects convenient `build_lib()` inputs, including RDS files that store either object shape.
- Keep the official workflow short, declarative, and composed only from existing OpenSpecy functions plus versioned curation CSVs.

## Scope

- **In**: Unit inference/conversion, optional range restriction, canonical lookup/curation CSVs, post-build filtering, raw/derivative/nobaseline libraries, medoid reductions, model libraries, and a tracked straight-line workflow.
- **Out**: Bundling generated `.rds` libraries, publishing to OSF/AWS, generic fuzzy curation, Shiny app code, and hidden organization heuristics in package defaults.
- **Users**: Maintainers rebuilding official libraries and users building interoperable custom libraries with the same package defaults.

## Requirements

- R1. `build_lib()` gains `convert_intensity = TRUE`; `FALSE` preserves supplied intensities and unit declarations.
- R1a. `x` accepts one `OpenSpecy`, a nonempty list containing only `OpenSpecy` objects, or a nonempty character vector of readable paths.
- R1b. Each `.rds` path is loaded directly and may contain one `OpenSpecy` or a nonempty list of them; other paths continue through `read_any()` without changing that reader.
- R2. For each source object, canonical `attr(x, "intensity_unit")`, when nonempty, is authoritative for every spectrum; otherwise cleaned metadata column `intensity_units` is evaluated per spectrum.
- R3. Unit matching is case-insensitive and recognizes absorbance, `reflec*`, and `transm*`; reflectance and transmittance use `adj_intens(..., make_rel = FALSE)`, while absorbance is unchanged.
- R4. Conversion occurs before optional merging, range restriction, deduplication, recipes, SNR, and assessment so IDs describe converted spectra.
- R5. Mixed metadata units are converted column-wise without breaking `wavenumber`, spectra/metadata alignment, names, or unrelated attributes.
- R6. Converted outputs set `attr(x, "intensity_unit")` and converted metadata values to `"absorbance"`; unknown or missing declarations remain unchanged and produce an actionable warning summary.
- R7. Attribute/metadata conflicts follow the attribute without double conversion; tests cover absorbance override, reflectance override, mixed metadata, multiple sources, unknown units, and opt-out.
- R8. `build_lib()` accepts `restrict_range_args = NULL`; a non-`NULL` list is passed to `restrict_range(..., make_rel = FALSE)` before deduplication and recipes.
- R9. `workflows/OpenSpecy_reference_library.R` defines no functions and uses only package/base calls; `build_lib()` creates the full libraries and `filter_spec()`, `restrict_range()`, `reduce_lib()`, `build_model_lib()`, `assess_lib()`, and `write_spec()` handle later steps.
- R10. Official outputs cover raw, derivative, nobaseline, medoid derivative/nobaseline, and derivative/nobaseline models for combined, FTIR, and Raman use, guided by `CleanRawFiles.R` and `andrea_ai/PAM.R`.
- R11. `workflows/data/` stores canonical lowercase/underscore lookup columns plus `known_bad_ids.csv`; organization-based intensity inference is removed and source corrections remain external to this repository.
- R12. Known-bad IDs remain integrated into `build_lib()`; plate-position and other dataset filters run with `filter_spec()` after the build.
- R13. `build_lib()` reports elapsed, named progress stages by default, including source loading, preparation, merging, joins, filtering, deduplication, recipes, SNR, assessment, and completion; users can disable messages.
- R14. Automatic metadata lookups infer the single shared column with overlapping values and unique lookup keys; no-key lookups are skipped with a progress message and multiple usable keys still stop as ambiguous.
- R15. Large lists of same-axis `OpenSpecy` sources are prepared in bulk: metadata names are cleaned once, legacy data.table spectra are converted silently, source attributes still supply primary intensity units, and conforming runs once on the combined object.
- R16. Metadata value cleanup is opt-in and normalizes character/factor metadata values before lookup joins; the official workflow enables it.
- R17. NA-aware processing and signal-to-noise improvements preserve current outputs and are benchmarked against legacy-style loops.

## Technical Decisions

- **Approach**: Normalize direct objects to a source list; inspect `.rds` paths with `readRDS()` and use `read_any()` otherwise; bulk-combine same-axis source lists when possible, then normalize units, merge when needed, and report progress through the builder pipeline.
- **Public API**: `convert_intensity` and `progress` default to `TRUE`; value cleanup is opt-in via `clean_metadata_values`/`clean_values` because it intentionally mutates labels.
- **Reference pipeline**: `build_lib(source_file, ...)` reads source paths itself; post-build `filter_spec()` applies dataset exclusions, then existing reduction/model functions operate on explicitly restricted copies.
- **Defaults**: Preserve full-range/resolution-6 and current named recipes initially; compare against legacy range `c(100, 11994)`, window 15 derivative, baseline, normalization, SNR step 10, excluded 2200-2420 model/reduction region, `k = 50`, minimum class size 10, and model `alpha = 0.1` before resetting any default.
- **Dependencies**: No new package dependency; workflow development may require external source/lookup files supplied by configured paths.
- **OpenSpecy contract**: Preserve source order, unique spectra names, row alignment, and valid attributes; conversion deliberately updates intensity units while recipes continue updating derivative/baseline attributes.
- **Generated artifacts**: Update roxygen and regenerate with configured roxygen2 only after resolving the current configured/installed version mismatch; never edit `NAMESPACE` or `man/*.Rd` directly.
- **External resources**: Canonical workflow CSVs are versioned; the large source object and full rebuild remain external/manual.

## Package Surfaces

- `R/`: Simplify `R/build_lib.R`, add `restrict_range_args`, and retain the NA-aware fix in `R/process_spec.R`.
- `tests/testthat/`: Extend `tests/testthat/test-build_lib.R` with small numerical unit-precedence and workflow-order tests.
- `benchmarks/`: Update `benchmarks/library_builder.R` with repeated legacy-loop equivalence and regression checks for conversion, NA processing, and signal-to-noise.
- `workflows/`: Replace the script with straight-line calls; add canonical CSVs under `workflows/data/`.
- `.Rbuildignore`: Add `^workflows$`; `.gitignore` remains unchanged.
- `vignettes/README/pkgdown`: Update the library-builder vignette with auto-conversion and link to the GitHub workflow; no direct pkgdown HTML edits.
- `DESCRIPTION`: Unchanged unless documentation/toolchain metadata must be corrected.
- `NEWS.md`: Add auto-conversion and reproducible reference-workflow entries.
- External Shiny compatibility: Keep official artifact structures/names compatible where scientifically valid; no Shiny code enters this repository.

## Work Checklist

- [x] Support direct objects and per-path RDS object/list inspection without changing `read_any()`.
- [x] Add default-on progress messages with elapsed time and an opt-out.
- [x] Create canonical workflow lookup CSVs and `known_bad_ids.csv`.
- [x] Compress repeated workflow stages across named library types with `lapply()`.
- [x] Infer one usable automatic metadata key, skip no-key lookups, and keep ambiguous lookups strict.
- [x] Add bulk same-axis source-list preparation to avoid per-source full coercion.
- [x] Add opt-in metadata value cleanup and curated `spectrum_type` lookup values.
- [x] Speed up grouped NA-aware processing and matrix signal-to-noise calculations.
- [x] Update tests, benchmark, roxygen, vignette, NEWS, and generated docs.
- [x] Run focused tests, benchmark, representative workflow, full tests, and package check.

## Verification

- Focused: resolved Windows Rscript running `devtools::test(filter = "build_lib")`.
- Benchmark: run `benchmarks/library_builder.R`; require expected numerical equivalence and no material repeated-timing regression.
- Workflow: run a small fixture/subset without personal paths, then manually run the full external library rebuild and record counts, misses, exclusions, class balance, medoid counts, model accuracy, and artifact names.
- Documentation: confirm configured roxygen2 version, run `devtools::document()`, and inspect authorship/aliases/exports immediately.
- Full: `devtools::test()`, vignette validation, then `devtools::check()` or the GitHub Actions R CMD check matrix.

## Risks And Open Questions

- Source metadata/intensity corrections are explicitly out of scope and must be completed in the external raw-data workflow before this script runs.
- The canonical class lookup drops blank, unknown, and conflicting duplicate keys following the current legacy cleanup behavior.

## Approval Notes

- Approved by:
- Follow-up: Run the full external-source rebuild before publishing the next official library artifacts.
