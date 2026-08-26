# Feature Plan: Reference Library Taxonomy, Pruning, And Recipe Performance

**Feature dir**: `specs/019-reference-library-validation`  
**Date**: 2026-08-26  
**Review budget**: Under 100 nonblank lines and about 1,500 words.  
**Current tranche**: Correct taxonomy, integrate library QA/QC and ordered top-match pruning into `build_lib()`, and accelerate derivative/baseline recipes without changing their scientific output.  
**Change class**: package/scientific

## Goal

- Make `classes_reference.csv` and `material_hierarchy.csv` chemically clearer and safer to join, incorporating every Hauser note as an applied correction or explicit deferral.
- Make class reassignment and top-match pruning a tested package stage that cleans the full processed libraries before medoid selection or model training, without allowing the largest classes to control the pruning order.
- Reduce derivative and nobaseline recipe times while preserving `OpenSpecy` values, axes, metadata alignment, identifiers, and processing attributes.

## Scope

- **In**: `build_lib()` and existing lookup/join/processing helpers; one exported `prune_lib()`; internal reassignment/pruning helpers; the existing reference-library script; taxonomy CSV cleanup; pruning QA reports; derivative and polynomial-baseline profiling, optimization, tests, and benchmarks.
- **Deferred checkpoint**: When the maintainer saves the currently building `libraries` object, inspect its real metadata and amend this plan with concrete column coalescing, `metadata_drop_columns.csv`, and `library_types.csv` changes before implementing those items.
- **Out**: New standalone build/validation workflows, new taxonomy dimensions, degradation modeling, cloud publication, and replacing hosted or packaged library artifacts.

## Planning Baseline

- Repository inspection confirms `rename_and_remove()`, `reassign_other_classes()`, and `prune_by_top_match()` are absent from `R/`, tests, and `workflows/OpenSpecy_reference_library.R`; the current workflow goes from `build_lib()` and blanket metadata filtering directly to `reduce_lib()` and `build_model_lib()`.
- `PAM.R` runs hard-coded class renames, reassigns `other material`/`other plastic` by nearest eligible same-technique spectrum, then repeatedly removes spectra whose best non-self match has another class. It defaults to `min_n = 10`, excludes 2200–2420 cm-1 from matching, and prunes FTIR/NIR separately from Raman, but evaluates all eligible classes together rather than largest-to-smallest.
- The PAM hard-coded merge of polyamides with polyacrylamides conflicts with the accepted taxonomy direction. Its cleanup intent belongs in curated lookup/hierarchy data; only data-independent reassignment and pruning logic belongs in code.
- Current taxonomy baseline: 12,123 unique class lookup keys; 266 unique hierarchy materials across 57 classes; 19 singleton classes. Current derivative work is matrix Savitzky-Golay plus normalization; nobaseline performs iterative polynomial fitting by spectrum.

## Requirements

- R1. Move the three adipate polymers from the nitrile class to `polyesters`; separate `polyacrylamides` from renamed `polyamides`; retain nylons, Kevlar, Nomex, and aramids under `polyamides`; prevent bare `pa` from mapping to polyacrylamide.
- R2. Review all 66 spreadsheet notes by issue type. Apply source-supported corrections; report unresolved judgments rather than guessing. Preserve useful aliases while keeping canonical material labels consistent, and do not remove rare classes solely for rarity.
- R3. Integrated builder QA must reject blank/contradictory/duplicate keys, report join coverage/count deltas, expose unmatched/ambiguous keys, and preserve source order plus spectrum/metadata alignment.
- R4. Add exported `prune_lib(x, ...)` for one `OpenSpecy` library. It returns a valid, aligned `OpenSpecy` by default and supports an audit return containing the object, retained IDs, ordered class schedule, reassignment events, removal events/reasons, and before/after counts.
- R5. `prune_lib()` defaults reproduce the useful PAM policy: `sample_name`, `material_class`, and `spectrum_type` metadata; `min_n = 10`; relative/mean-replaced matching intensities; excluded 2200–2420 cm-1; FTIR/NIR and Raman candidate pools; and nearest eligible reassignment for `other material` and `other plastic`. Missing eligible candidates are retained and reported, not errored.
- R6. Reassignment happens before pruning and updates `material_class` while preserving the prior value in an audit field/report. Implement it as an internal helper, not another export. Use explicit metadata columns and curated hierarchy/type eligibility rather than parsing compound `technique_organization_class` strings or reviving obsolete hard-coded chemistry merges.
- R7. Within each technique pool, freeze a deterministic material-class schedule from initial counts, descending by count with lexical tie-breaking. Process only the scheduled target class at each step; after its removals, update the remaining candidate set before evaluating the next class. For each target spectrum, retain it when its best finite non-self match has the same class or no eligible partner; otherwise remove it, iterating that target class to stability while respecting the `min_n` protected floor. This makes large-class cleanup precede smaller-class decisions.
- R8. Compute correlations in bounded blocks from a normalized matrix rather than materializing an avoidable full `n x n` matrix. Deterministically resolve equal/near-equal maxima by stable spectrum ID/order; preserve original retained-spectrum order. Handle one class, all-small classes, constant/NA spectra, empty technique pools, and a class reaching the protected floor without error.
- R9. Add input-triggered `build_lib(prune = NULL)`: `NULL` preserves general-package behavior; a named list maps recipe names to `prune_lib()` argument lists. The official workflow passes `prune = list(derivative = list(), nobaseline = list())`, so both full processed libraries use the documented defaults before their own medoid IDs and models are created. Raw remains unpruned; each processed recipe is pruned from its own spectral representation and retains its own audit report.
- R10. Profile derivative/nobaseline recipes and pruning independently on deterministic fixtures plus a genuine subset with imbalanced classes and representative NA boundaries. Preserve derivative output at `1e-12` and baseline output at a measured tight tolerance (target `1e-10`), including attributes and NA locations. Target 2x derivative and 1.5x baseline speedups; retain only demonstrated improvements with no same-output benchmark more than 10% slower.
- R11. The saved full `libraries` object is the acceptance checkpoint: compare taxonomy and per-class pruning counts, reassignment/removal reports, stable IDs, axes, metadata names, warnings, representative joins/matches, and pre/post medoid/model inputs before publication.

## Technical Decisions

- **API**: `prune_lib()` is public because it is independently useful and base-pipe composable; its one-caller kernels and reassignment helpers remain internal. `build_lib()` gets one input-triggered `prune` argument rather than paired flags or many pruning parameters.
- **Stage order**: read/merge -> metadata cleanup/lookups/hierarchy -> exclusions/deduplication -> recipes -> selected-recipe reassignment/pruning -> returned full libraries -> `reduce_lib()` medoids -> `build_model_lib()` models.
- **Taxonomy boundary**: `rename_and_remove()` is not copied wholesale. Canonical naming/removal stays reviewable in `classes_reference.csv`, `material_hierarchy.csv`, and known-bad IDs; `prune_lib()` performs only spectrum-supported reassignment/pruning.
- **Performance**: Keep old same-output derivative/baseline implementations under `benchmarks/`; add a focused pruning benchmark/oracle for imbalanced classes, block-size memory, and deterministic output. No new dependencies unless profiling proves unavoidable.
- **Generated/docs**: Document/export from roxygen and regenerate `NAMESPACE`/`man`; update the library-builder vignette and `NEWS.md`. Never hand-edit generated files.
- **Bundled Shiny/pipeline diagram**: no app reactive/data-flow change; diagram N/A. **Hosted Shinylive**: `R/` changes trigger fast `-HostedAppStatic`; exact-artifact matching waits for approved replacement libraries, and no clean wasm rebuild is required in this tranche.

## Package Surfaces

- Data/workflow: `workflows/data/classes_reference.csv`, `material_hierarchy.csv`, and `workflows/OpenSpecy_reference_library.R`.
- Package: chiefly `R/build_lib.R`; processing files only where profiling directs. Roxygen-generated `man/build_lib.Rd`/`NAMESPACE` follow regeneration.
- Evidence/docs: `tests/testthat/test-build_lib.R`, processing tests, `benchmarks/library_builder.R`, a pruning benchmark, `vignettes/library-builder.Rmd`, and `NEWS.md`.

## Work Checklist

- [ ] Apply or defer every attachment row; implement explicit adipate, polyamide/polyacrylamide, PA, aramid, duplicate, typo, and canonical-name corrections.
- [ ] Implement/test `prune_lib()` with internal reassignment and deterministic largest-to-smallest, blockwise top-match pruning plus an auditable report.
- [ ] Integrate named recipe pruning into `build_lib()` and enable derivative/nobaseline pruning in the existing workflow before medoid/model creation.
- [ ] Add integrated lookup/hierarchy QA; profile and implement only proven derivative/baseline optimizations, preserving legacy comparisons in benchmarks.
- [ ] Inspect the saved genuine build, amend deferred metadata decisions, and run staged taxonomy/pruning/recipe/legacy comparisons on temporary outputs.
- [ ] Run focused tests/benchmarks, documentation, full tests once, and `-HostedAppStatic`; report readiness without publishing artifacts.

## Verification And Risks

- Synthetic tests must prove schedule order, imbalanced-class protection, deterministic ties, `min_n`, reassignment/no-candidate behavior, per-recipe independence, ID/order alignment, and that pruning precedes medoids/models in the official script.
- Data checks must prove unique keys, corrected taxonomy rows, stable join cardinality, and no forbidden cross-mapping. Genuine staged checks must report all changed classes/IDs rather than asserting identical pruning to PAM, because the ordering is intentionally different.
- Blockwise matching can trade runtime for memory and ordered pruning is path-dependent. Benchmark both, record the schedule/report, and stop on invalid metadata rather than silently producing unauditable output.
- Some attachment chemistry remains judgment-dependent; saved-build column/drop/type work remains deferred until inspectable. R CMD check and hosted exact-artifact/clean rebuild remain release-stage work.

## Approval Notes

- Updated for the requested PAM integration and ordered pruning. Implementation is not authorized in this planning turn.
