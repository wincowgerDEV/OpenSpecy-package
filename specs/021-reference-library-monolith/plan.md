# Feature Plan: End-to-End Reference Library Builder

**Feature dir**: `specs/021-reference-library-monolith`  
**Date**: 2026-08-27  
**Review budget**: Under 100 nonblank lines and about 1,500 words.  
**Current tranche**: Make `build_lib()` own the official source-to-library/medoid/model workflow, its progress reporting, saved outputs, and reviewable old/new assessments; reduce `workflows/OpenSpecy_reference_library.R` to one default builder call.  
**Change class**: package/scientific

## Goal

- Let a maintainer run `build <- build_lib()` and receive every official raw, processed, medoid, model, and assessment artifact in one object, with default paths and curated transformations resolved inside the function.
- Make scientific and compatibility consequences observable through deterministic progress, leakage-resistant old/new accuracy comparisons, and clear `assess_spec()` shifts.

## Scope

- **In**: `build_lib()` defaults/return contract; official source discovery, curated lookups, class completion, pruning, one-off exclusions and metadata drops, medoid/model creation, output writing, model test consolidation, assessment collection, progress, exact-vs-regex cleanup, and the 1,000-spectrum legacy comparison workflow.
- **Out**: Correcting raw source files, inventing fuzzy material mappings, publishing/replacing OSF/AWS libraries, changing `get_lib()` download URLs, bundled Shiny UI changes, and unrelated processing algorithms.
- **Users**: Maintainers use the no-argument official build; advanced users may still supply `x`, paths, lookup inputs, or named stage argument lists and may compose the existing helpers independently.

## Requirements

- R1. With `x` omitted, `build_lib()` resolves `data_dir`, processed-source directory, `source_file`, workflow-data directory, prior-library location, and `output_dir` from documented environment-aware argument defaults inside `R/build_lib.R`; explicit arguments override them. No path/configuration objects precede the call in the workflow script.
- R2. The default pipeline performs source reading/merging, metadata name/value standardization, identity cleanup, exact class and source-type lookup, regex-only class completion, hierarchy join, bad-ID exclusion/deduplication, coverage checks, raw/derivative/nobaseline recipes, derivative/nobaseline pruning, superseded and special-case filters, reviewed metadata drops, rounding, medoid selection, model training/testing, legacy comparison, assessment, and atomic/temp-first output writing.
- R3. Return one predictably named list: `libraries` (`raw`, `derivative`, `nobaseline`), `medoids` (`derivative`, `nobaseline`), `models` (recipe then `both`/`ftir`/`raman`), and `assessments`. Preserve every `OpenSpecy` axis, spectra/metadata alignment, unique ID, processing attribute, and audit attribute through filtering and nesting. Emit legacy individual RDS filenames from this object for `get_lib()` compatibility plus one aggregate build RDS; never overwrite an existing official build until the staged candidate passes.
- R4. Each model object retains inference-required fields and contains exactly one spreadsheet-friendly `tests` `data.table` with held-out spectrum ID, technique, expected/predicted class, correctness, score/probability where available, split/provenance, and model/artifact labels. Replace scattered training-only `accuracy`, `confusion`, and duplicate scalar assessment fields only with an explicit migration, consumer tests, roxygen, and `NEWS.md` entry.
- R5. `assessments` is a named list of reviewable `data.table` items covering build/artifact summary, source and lookup coverage, identity/class-regex/hierarchy/type joins, exclusions/deduplication, filter/drop status including `superseded_drop`, pruning, medoid/model summaries, split manifest, library identification, model identification, `assess_spec` shifts, old/new compatibility, warnings, and output manifest. Old-library checks must be stored here, not left as exploratory code.
- R6. Standardize and coalesce internal metadata columns before any external lookup. Preserve populated canonical values, fill only blank canonical values from reviewed aliases such as `user_name -> organization`, report conflicts/fills, then join `library_types.csv` on canonical `organization`; remove `fallback_by` from the official lookup contract and deprecate it in the general lookup spec.
- R7. Audit `classes_regex.csv` for anchored literal-only patterns that can match only one normalized identity; move those mappings to `classes_reference.csv` as exact keys, reject collisions, and retain in regex only rules with genuine controlled variability. Exact matches remain authoritative; regex runs only on blanks and reports overlaps/clashes/unmatched rows without speculative chemistry.
- R8. `progress = TRUE` reports start/end, elapsed time, and before/after or matched/unmatched counts for source discovery/read, metadata coalesces, every lookup/hierarchy join, exclusions, dedupe, each recipe and assessment, every pruning pool/class, special filters/drops, medoids, each model, prior-artifact loading, split/evaluation, and writes. `progress = FALSE` suppresses builder and delegated prune progress.
- R9. With a fixed recorded seed, retrieve `get_lib("raw")`, load it, sample 1,000 spectra without replacement, run the complete candidate workflow in a temporary output directory, and compare against downloaded `derivative`, `nobaseline`, both medoids, and both model artifacts. Record inputs, revisions/checksums when available, seed, IDs, warnings, timing, axes, metadata names/counts, class/type distributions, and artifact shapes.
- R10. Form one reproducible 10% holdout from the combined old/new comparison population, stratified where feasible by representation, technique, and material class and grouped by stable source/content identity so duplicates or the same spectrum in old/new cannot cross train/test. Reference-library accuracy must identify held-out spectra only against each candidate's remaining 90%; report coverage, top-1 accuracy, macro class accuracy, confusion, and match-score summaries overall and by technique/class/source.
- R11. Test each new model trained only from its corresponding 90% training reference against the matching 10% derivative/nobaseline holdout. Apply downloaded legacy models to the same held-out rows as a clearly labeled compatibility comparison; report that their original training membership is unknown and do not present that result as independent generalization accuracy.
- R12. Run `assess_spec(report = "all")` on comparable old/new artifact subsets and store per-check/status counts, rates, absolute/relative shifts, unavailable checks, and severity changes, with enough IDs to inspect regressions. Metrics may be worse for the 1,000-spectrum candidate but must be interpretable side by side and must flag missing/undefined denominators.

## Technical Decisions

- **Primary pipeline**: `build_lib()` -> `libraries` -> pruned processed libraries -> `medoids` -> `models`; all visible artifacts and assessment tables derive from those canonical aligned `OpenSpecy` objects. One-caller orchestration stays internal; exported `prune_lib()`, `reduce_lib()`, `build_model_lib()`, and lookup helpers remain pipe-composable for compatibility.
- **API classification**: `x` is optional required data (official paths when missing); path arguments and lookup objects are meaningful overrides; non-`NULL` inputs trigger optional overrides; pruning/reduction/model tuning uses named lists passed only to the owning helper. Canonical coalesces, table order, filter rules, artifact names, progress counts, and assessment derivations are inferred rather than exposed as flags.
- **Dependencies**: Reuse base R, `data.table`, existing package dependencies, and existing download helpers; no Excel-writing dependency. Network/large comparison stages guard the actual OSF/AWS hosts and skip only in routine tests, while the official workflow fails with actionable recovery guidance.
- **Compatibility/docs**: Treat the return/model layout and default output side effects as public changes. Update the vignette, roxygen, examples, and `NEWS.md`; regenerate with configured roxygen and inspect `NAMESPACE`/`man` diffs rather than editing them.
- **Bundled Shiny/pipeline diagram**: N/A; no `inst/shiny` source or diagram stage changes. **Hosted**: `R/` and vignette inputs trigger fast `-HostedAppStatic`; run matching-artifact preflight when a candidate medoid/model bundle is staged. A clean wasm rebuild is deferred unless library pins/images or release artifacts are updated.

## Package Surfaces And Work Checklist

- [ ] `R/build_lib.R`: implement official/custom modes, canonical pre-join harmonization, all current workflow stages, bundle/model-test/assessment contracts, delegated progress, staged writes, and compatibility shims; touch `R/match_spec.R`/`R/manage_lib.R` only if consumers require it.
- [ ] `workflows/OpenSpecy_reference_library.R` and `workflows/data/classes_{reference,regex}.csv`: reduce the script to setup plus one default `build_lib()` call and migrate exact-only regex rows with curation audit.
- [ ] `tests/testthat/test-build_lib.R`: cover tiny end-to-end bundle shape, default-path resolution without real writes/downloads, pre-join coalescing/conflicts, removal/deprecation of `fallback_by`, filters/drops, progress/quiet behavior, model `tests`, assessments, atomic outputs, alignment/attributes, and legacy filenames; keep network/long tests guarded.
- [ ] `benchmarks/reference_library_validation.R`: retain the prior straight-line workflow as comparison evidence; add the seeded 1,000-spectrum staged build, leakage checks, 10% reference/model evaluations, `assess_spec` deltas, compatibility tables, and runtime/memory regression reporting.
- [ ] `vignettes/library-builder.Rmd`, `NEWS.md`, roxygen/generated docs: document the one-call workflow, return schema, progress, assessment interpretation, split limitations, custom composition, and breaking/deprecated fields. `DESCRIPTION` stays unchanged absent an unavoidable dependency; `.github/workflows/`, `inst/`, `site/`, README, and pkgdown sources stay unchanged.

## Verification

- Focused: resolve Windows Rscript, then `devtools::test(filter = "build_lib|match_spec|manage_lib", reporter = "check", stop_on_failure = TRUE)`; assert no stable identity/hash appears on both sides of the split and every artifact passes `check_OpenSpecy()` where applicable.
- Benchmark/staging: set a fixed seed and temp output, retrieve the seven named legacy types with host guards, sample 1,000 raw spectra, run `benchmarks/reference_library_validation.R`, and inspect all `assessments` tables plus aggregate/legacy RDS reloads; never publish or replace downloaded/official files.
- Same-output portions: compare the retained straight-line implementation to the monolith for IDs, axes, spectra at declared recipe tolerances, metadata, attributes, pruning/medoid choices, and runtime; flag >10% slowdown unless justified by the new assessment work.
- Final candidate: confirm configured roxygen version, run `devtools::document()` and inspect generated diffs, render `vignettes/library-builder.Rmd`, run full `devtools::test()` once, then `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -Filter shinylive_wasm -HostedAppStatic`. `devtools::check()`/CI R CMD check is required before release, not routine iteration.
- Reusable evidence: focused/split/benchmark results remain valid only while builder, lookup CSVs, comparison artifacts/revisions, seed, processing/matching/model contracts, and dependencies are unchanged.

## Risks And Open Questions

- Legacy model training membership cannot be reconstructed from downloaded models; label those scores as compatibility evidence and use the leakage-free library/new-model split for inferential comparisons.
- A no-argument build performs network, compute, and writes; failures must leave existing outputs recoverable and clearly identify the last completed stage and staged paths.

## Approval Notes

- Approved by:
- Follow-up:
