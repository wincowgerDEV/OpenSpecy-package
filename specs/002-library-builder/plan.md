# Feature Plan: Exact Reference Library Workflow

**Feature dir**: `specs/002-library-builder`
**Date**: 2026-06-19
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Make `build_lib()` reproduce the official Open Specy reference-library cleanup, including automatic conversion of reflectance/transmittance spectra to absorbance.
- Keep the exact maintainer workflow visible and version controlled without placing machine paths, generated libraries, or Shiny code in the package build.

## Scope

- **In**: Unit inference/conversion, attribute precedence, reference recipes/default audit, explicit curation, raw/derivative/nobaseline libraries, medoid reductions, model libraries, and a tracked `workflows/OpenSpecy_reference_library.R`.
- **Out**: Bundling generated `.rds` libraries, publishing to OSF/AWS, generic fuzzy curation, Shiny app code, and hidden organization heuristics in package defaults.
- **Users**: Maintainers rebuilding official libraries and users building interoperable custom libraries with the same package defaults.

## Requirements

- R1. `build_lib()` gains `convert_intensity = TRUE`; `FALSE` preserves supplied intensities and unit declarations.
- R2. For each input source, canonical `attr(x, "intensity_unit")`, when nonempty, is authoritative for every spectrum; otherwise cleaned metadata column `intensity_units` is evaluated per spectrum.
- R3. Unit matching is case-insensitive and recognizes absorbance, `reflec*`, and `transm*`; reflectance and transmittance use `adj_intens(..., make_rel = FALSE)`, while absorbance is unchanged.
- R4. Conversion occurs before `c_spec()`, deduplication, recipes, SNR, and assessment so conflicting source attributes are not lost and IDs describe converted spectra.
- R5. Mixed metadata units are converted column-wise without breaking `wavenumber`, spectra/metadata alignment, names, or unrelated attributes.
- R6. Converted outputs set `attr(x, "intensity_unit")` and converted metadata values to `"absorbance"`; unknown or missing declarations remain unchanged and produce an actionable warning summary.
- R7. Attribute/metadata conflicts follow the attribute without double conversion; tests cover absorbance override, reflectance override, mixed metadata, multiple sources, unknown units, and opt-out.
- R8. `workflows/OpenSpecy_reference_library.R` uses `build_lib()` as the sole full-library builder and package helpers (`reduce_lib()`, `build_model_lib()`, `assess_lib()`, `write_spec()`) for downstream artifacts; it copies no legacy helper implementations.
- R9. The workflow records source inputs, lookup tables, curated exclusions/corrections, exact range/resolution/recipes, rounding, reduction groups and `k`, model settings, output names, counts, and assessment summaries without hardcoded personal paths.
- R10. Official outputs cover raw, derivative, nobaseline, medoid derivative/nobaseline, and derivative/nobaseline models for combined, FTIR, and Raman use, guided by `CleanRawFiles.R` and `andrea_ai/PAM.R`.
- R11. Domain-general official settings become or remain package defaults; source-specific fixes such as NIST scaling, organization exceptions, known-bad IDs, and plate-position exclusions stay explicit in the workflow.

## Technical Decisions

- **Approach**: Normalize each source's metadata names, resolve its unit vector with attribute-first precedence, transform affected columns through an internal one-caller helper, then merge and continue the existing `build_lib()` pipeline.
- **Public API**: `convert_intensity` is a meaningful standard-workflow policy and defaults to `TRUE`; unit type is derived state, so no second unit argument or exported helper is added. Advanced manual conversion remains `x |> adj_intens(...)`.
- **Reference pipeline**: `sources |> build_lib(...)` creates full libraries; returned derivative/nobaseline libraries feed `reduce_lib()`, then `build_model_lib()`. Exact dataset curation remains readable script data.
- **Defaults**: Preserve full-range/resolution-6 and current named recipes initially; compare against legacy range `c(100, 11994)`, window 15 derivative, baseline, normalization, SNR step 10, excluded 2200-2420 model/reduction region, `k = 50`, minimum class size 10, and model `alpha = 0.1` before resetting any default.
- **Dependencies**: No new package dependency; workflow development may require external source/lookup files supplied by configured paths.
- **OpenSpecy contract**: Preserve source order, unique spectra names, row alignment, and valid attributes; conversion deliberately updates intensity units while recipes continue updating derivative/baseline attributes.
- **Generated artifacts**: Update roxygen and regenerate with configured roxygen2 only after resolving the current configured/installed version mismatch; never edit `NAMESPACE` or `man/*.Rd` directly.
- **External resources**: Full rebuild is manual; no routine test downloads or publication side effects.

## Package Surfaces

- `R/`: Update `R/build_lib.R`; change other processing defaults only when reference-output comparison justifies it.
- `tests/testthat/`: Extend `tests/testthat/test-build_lib.R` with small numerical unit-precedence and workflow-order tests.
- `benchmarks/`: Update `benchmarks/library_builder.R` with repeated legacy-loop equivalence and regression checks for auto-conversion.
- `workflows/`: Add `workflows/OpenSpecy_reference_library.R`; track in git and exclude from package builds.
- `.Rbuildignore`: Add `^workflows$`; `.gitignore` remains unchanged.
- `vignettes/README/pkgdown`: Update the library-builder vignette with auto-conversion and link to the GitHub workflow; no direct pkgdown HTML edits.
- `DESCRIPTION`: Unchanged unless documentation/toolchain metadata must be corrected.
- `NEWS.md`: Add auto-conversion and reproducible reference-workflow entries.
- External Shiny compatibility: Keep official artifact structures/names compatible where scientifically valid; no Shiny code enters this repository.

## Work Checklist

- [ ] Implement attribute-first per-source intensity conversion in `R/build_lib.R`.
- [ ] Add focused tests and the repeated conversion benchmark.
- [ ] Add the buildignored, tracked reference workflow and encode current curation/settings through package APIs.
- [ ] Rebuild a representative subset, compare counts/numerics/attributes with legacy outputs, then decide any default changes.
- [ ] Update roxygen, vignette, NEWS, and inspect generated diffs.
- [ ] Run staged package quality gates and the manual full-library workflow.

## Verification

- Focused: resolved Windows Rscript running `devtools::test(filter = "build_lib")`.
- Benchmark: run `benchmarks/library_builder.R`; require expected numerical equivalence and no material repeated-timing regression.
- Workflow: run a small fixture/subset without personal paths, then manually run the full external library rebuild and record counts, misses, exclusions, class balance, medoid counts, model accuracy, and artifact names.
- Documentation: confirm configured roxygen2 version, run `devtools::document()`, and inspect authorship/aliases/exports immediately.
- Full: `devtools::test()`, vignette validation, then `devtools::check()` or the GitHub Actions R CMD check matrix.

## Risks And Open Questions

- Confirm whether legacy source corrections that override declared units should be encoded as workflow-side metadata corrections or a versioned curation table.
- Decide default changes only after the new workflow reproduces the intended official outputs; dataset-specific policy must not leak into general package behavior.

## Approval Notes

- Approved by:
- Follow-up: Use this plan for iterative reference-library cleanup and default tuning.
