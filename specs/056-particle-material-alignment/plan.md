# Feature Plan: Particle Material Alignment

**Feature dir**: `specs/056-particle-material-alignment`  
**Date**: 2026-11-13  
**Current tranche**: Preserve one material result per source spectrum when `automate_particle_analysis(..., particle_id_strategy = "all_cell_id")` uses an ad-hoc reference library without `sample_name` or a map whose metadata join fields repeat.  
**Change class**: package/scientific
**Status**: Implemented and focused/hosted/live verified; full-suite pass blocked by unrelated current-worktree failures.

## Goal And Scope

- Fix the current-session failure `"'material' must have one value per spectrum"` without changing matching, thresholds, collapse policy, or the public API.
- **In**: in-memory and FileSpecs reference-label resolution, safe cell-match projection, absent `sample_name`, repeated `col_id` or `x`/`y` metadata, focused regressions, NEWS, package and hosted-static verification.
- **Out**: public arguments/exports, Shiny behavior, reference-library artifacts, generated documentation, and changes to scientific decision thresholds.

## Requirements

- R1. Reference labels are resolved by the library spectrum names returned by `match_spec()`; `library_id_col` remains a guarded legacy fallback and is not required when `material_col` exists.
- R2. Cell-match metadata is projected back by the source object's unique spectrum column names before any non-unique metadata key is considered.
- R3. Projection preserves source spectrum order and exactly `ncol(map$spectra)` metadata rows; unmatched spectra receive missing match fields and cannot multiply rows.
- R4. In-memory and FileSpecs `all_cell_id` support the current custom-library shape plus repeated map keys while retaining aligned `OpenSpecy` objects; malformed objects fail with a targeted alignment error.

## Technical Decisions

- **Approach**: resolve material rows from canonical library spectrum names in one internal helper shared by eager and FileSpecs matching. Replace the cardinality-unsafe cell metadata merge with an order-preserving spectrum-name `match()`; retain only a unique, row-count-preserving metadata-key fallback for legacy objects.
- **Public API/dependencies**: no signature, export, argument, flag, or dependency change; the fix is internal and inferred from canonical object identifiers.
- **OpenSpecy contract**: unique `spectra` column names remain the primary spectrum identity; `metadata` stays one row per spectrum in the same order; wavenumbers, values, coordinates, and attributes are unchanged.
- **Generated artifacts**: no roxygen or package-metadata change, so `devtools::document()` is not triggered and generated files remain untouched.
- **Performance**: linear matching replaces a merge; this is a correctness fix rather than a same-output optimization, so no benchmark is required.
- **Pipeline diagram/bundled app**: N/A; no `inst/shiny` source, reactive, control, download, state transition, or canonical app-pipeline component changes.
- **Hosted impact**: `R/` is shared package input, so run fast `-HostedAppStatic`; hosted routes, runtime assembly, dependencies, pins, staged libraries, and generated artifacts are unchanged, so matching-artifact and clean wasm rebuild tiers are not triggered.

## Package Surfaces

- `R/automate_particle_analysis.R`, `R/automate_particle_filespecs.R`: canonical reference-label resolution, safe cell-match projection, and alignment guards.
- `tests/testthat/test-automate_particle_analysis.R`, `tests/testthat/test-FileSpecs-particle.R`: custom-library and repeated-key regressions with spectra/metadata invariants.
- `NEWS.md`: user-visible bug-fix entry.
- `benchmarks/`, `DESCRIPTION`, `NAMESPACE`, `man/`, `inst/`, `site/`, vignettes, README/pkgdown, workflows, and reference workflows: unchanged.

## Work Checklist

- [x] Add failing custom-library/repeated-key alignment regressions in `tests/testthat/` and capture the live 0-versus-268,786 mismatch.
- [x] Implement canonical reference-label resolution, spectrum-name projection, and guarded fallbacks in `R/`.
- [x] Record the fix in `NEWS.md` and inspect owned diffs.
- [x] Run focused particle tests, `-HostedAppStatic`, and the exact live analysis.
- [ ] Obtain a full `devtools::test()` pass after the maintainer resolves the unrelated spatial-calibration and encoding/JDX failures in the current worktree.
- [x] Reconcile evidence, record deferred gates, inspect owned processes/status, and clean task scratch artifacts.

## Verification

- Focused: eager particle tests pass 280 assertions; FileSpecs particle tests pass 92. Both use a library with `material_class` but no `sample_name`; repeated keys cannot expand metadata; raw/processed outputs remain aligned.
- Live acceptance: after `devtools::load_all()`, the exact 268,786-spectrum Positron call reaches `library matching`, writes outputs, and reports `complete` without the former zero-length material error.
- Hosted: `-HostedAppStatic` passes 391 assertions plus bundled/hosted source parsing and ZIP self-test; no exact-artifact preflight or clean wasm rebuild is triggered.
- Full suite: attempted once; 5,190 pass, 10 fail, 3 warn, 2 skip. Eight failures are in the unrelated already-modified spatial-calibration app surface; the other two are invalid-byte and JDX failures in unchanged `R/build_lib.R` and `R/read_ext.R`. Re-run after those owners restore their baseline.
- Documentation/check: `devtools::document()` N/A; R CMD check deferred because this is not release-facing and the maintainer did not request it.
- Reference intake/rebuild, spectrum disposition, frozen holdout, browser/download, assets, and long-stage gates: N/A.
- Closure: `git diff --check` passes; the full-test `Rplots.pdf` scratch file was removed; no agent-owned R process remains; unrelated feature-055 and local settings work is preserved.

## Risks And Open Questions

- Legacy objects without valid unique spectrum names may still require metadata-key projection; that fallback must reject ambiguous keys rather than guess across repeated regions.
- The current live session has the old namespace loaded; after source verification it will require `devtools::load_all()` or a session restart before rerunning the existing call.

## Approval Notes

- Approved by the user's request to fix the current working-session analysis on 2026-11-13.
- Remote synchronization remains maintainer-owned.
