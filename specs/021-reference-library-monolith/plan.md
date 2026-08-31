# Feature Plan: End-to-End Reference Library Builder

**Feature dir**: `specs/021-reference-library-monolith`  
**Date**: 2026-08-27  
**Review budget**: Under 100 nonblank lines and about 1,500 words.  
**Current tranche**: Make `build_lib()` own the official source-to-library/medoid/model workflow, its progress reporting, saved outputs, and reviewable old/new assessments; reduce `workflows/OpenSpecy_reference_library.R` to one default builder call.  
**Change class**: package/scientific

## Goal

- Let a maintainer pass discovered source paths and an output directory to `build_lib()` and receive every raw, processed, medoid, model, and assessment artifact in one object.
- Make scientific and compatibility consequences observable through deterministic progress, leakage-resistant old/new accuracy comparisons, and clear `assess_spec()` shifts.

## Scope

- **In**: `build_lib()` inputs/return contract; helper-table discovery, curated lookups including the reviewed Clarissa recommendations, class completion, constrained generic-class reassignment and pruning, exclusions and metadata drops, medoid/model creation, resumable exports, assessment collection, progress, regex cleanup, and a visual workflow diagram.
- **Out**: Correcting raw source files, inventing fuzzy material mappings, publishing/replacing OSF/AWS libraries, changing `get_lib()` download URLs, bundled Shiny UI changes, and unrelated processing algorithms. Restoring the established compiled PAM engine after the replacement fails at production scale is in scope.
- **Users**: Maintainers use the no-argument official build; advanced users may still supply `x`, paths, lookup inputs, or named stage argument lists and may compose the existing helpers independently.

## Requirements

- R1. The workflow script retains its `data_dir`, processed-source, raw-source, and output finders, then passes source paths and `output_dir` explicitly. `build_lib()` rejects missing `x`; when `workflow_data` is omitted it finds complete helper tables under `data/` beside the calling script, then the working directory or `workflows/data/`.
- R2. The default pipeline performs source reading/merging, metadata name/value standardization, identity cleanup, exact class and source-type lookup, regex-only class completion, hierarchy join, bad-ID exclusion/deduplication, coverage checks, raw/derivative/nobaseline recipes, derivative/nobaseline pruning, superseded and special-case filters, reviewed metadata drops, rounding, medoid selection, model training/testing, full legacy comparison, assessment, and checkpointed output writing.
- R3. Return one predictably named list: `libraries` (`raw`, `derivative`, `nobaseline`), `medoids` (`derivative`, `nobaseline`), `models` (recipe then `both`/`ftir`/`raman`), and `assessments`. Preserve every `OpenSpecy` axis, spectra/metadata alignment, unique ID, processing attribute, and audit attribute through filtering and nesting. Export each completed component plus its manifest to a staging/checkpoint area under `output_dir`; after validation, emit legacy individual RDS filenames for `get_lib()` compatibility plus one aggregate build RDS without partially replacing an existing official build.
- R4. Each model object retains inference-required fields and contains exactly one spreadsheet-friendly `tests` `data.table` with held-out spectrum ID, technique, expected/predicted class, correctness, score/probability where available, split/provenance, and model/artifact labels. Replace scattered training-only `accuracy`, `confusion`, and duplicate scalar assessment fields only with an explicit migration, consumer tests, roxygen, and `NEWS.md` entry.
- R5. `assessments` is a named list of reviewable `data.table` items covering build/artifact summary, source and lookup coverage, identity/class-regex/hierarchy/type joins, exclusions/deduplication, filter/drop status including `superseded_drop`, pruning, medoid/model summaries, split manifest, library identification, model identification, `assess_spec` shifts, old/new compatibility, warnings, and output manifest. Old-library checks must be stored here, not left as exploratory code.
- R6. Standardize and coalesce internal metadata columns before any external lookup. Preserve populated canonical values, fill only blank canonical values from reviewed aliases such as `user_name -> organization`, report conflicts/fills, then join `library_types.csv` on canonical `organization`; remove `fallback_by` from the official lookup contract and deprecate it in the general lookup spec.
- R7. Audit `classes_regex.csv` for anchored literal-only patterns that can match only one normalized identity; move those mappings to `classes_reference.csv` as exact keys, reject collisions, and retain in regex only rules with genuine controlled variability. Exact matches remain authoritative; regex runs only on blanks and reports overlaps/clashes/unmatched rows without speculative chemistry.
- R8. `progress = TRUE` reports start/end, elapsed time, and before/after or matched/unmatched counts for source discovery/read, metadata coalesces, every lookup/hierarchy join, exclusions, dedupe, each recipe and assessment, every pruning pool/class, special filters/drops, medoids, each model, prior-artifact loading, split/evaluation, and writes. `progress = FALSE` suppresses builder and delegated prune progress.
- R9. Add logical `reuse = TRUE`. A reusable stage must have a completed artifact and manifest whose input files/checksums, lookup files, relevant arguments, package/version contract, and upstream artifact signatures match; otherwise recompute it and every dependent stage. `reuse = FALSE` recomputes all stages. Progress and `assessments$output_manifest` report reused, rebuilt, failed, staged, and promoted components.
- R10. Assess the complete candidate raw/processed/medoid/model artifacts against the complete downloaded legacy `raw`, `derivative`, `nobaseline`, both medoids, and both models. Form one reproducible 10% holdout from the full combined old/new comparison population, stratified where feasible by representation, technique, and material class and grouped by stable source/content identity so duplicates or the same spectrum in old/new cannot cross train/test. Reference-library accuracy must identify held-out spectra only against each candidate's remaining 90%; report coverage, top-1 accuracy, macro class accuracy, confusion, and match-score summaries overall and by technique/class/source.
- R11. Test each new model trained only from its corresponding 90% training reference against the matching 10% derivative/nobaseline holdout. Apply downloaded legacy models to the same held-out rows as a clearly labeled compatibility comparison; report that their original training membership is unknown and do not present that result as independent generalization accuracy.
- R12. Run `assess_spec(report = "all")` across the complete comparable old/new artifacts and store per-check/status counts, rates, absolute/relative shifts, unavailable checks, and severity changes, with enough IDs to inspect regressions. Metrics must be interpretable side by side and flag missing/undefined denominators. Sampling 1,000 legacy raw spectra is permitted only for internal staged development, never as the function's assessment scope or final evidence.
- R13. Translate the reviewed Clarissa workbook identities into existing canonical `material` values: monomers and other confirmed non-polymers use `organic matter`; confirmed polymers use existing ABS, nylon 6,6, cellulose derivative, methyl cellulose, polyurethane, or conservative generic labels; polymer-natural mixtures use the explicitly requested `other` label. Do not import the workbook's alternate taxonomy names.
- R14. Official class completion labels otherwise blank standards as `other` and fails if that unresolved class exceeds 1% of an artifact. During `prune_lib()`, reassign `other` to the nearest established class in its spectral pool, `other plastic` only to plastic candidates, and `other material` only to `organic matter` or `mineral`; update the reassigned material type and retain a correlation audit. A generic class with no eligible finite match remains explicit.

## Technical Decisions

- **Primary pipeline**: `build_lib()` -> `libraries` -> pruned processed libraries -> `medoids` -> `models`; all visible artifacts and assessment tables derive from those canonical aligned `OpenSpecy` objects. One-caller orchestration stays internal; exported `prune_lib()`, `reduce_lib()`, `build_model_lib()`, and lookup helpers remain pipe-composable for compatibility.
- **Full assessment computation**: For each leakage-free old/new reference side, call optimized `cor_spec()` once for the complete train-by-test matrix and immediately reduce it with `max_cor_named()`; available build resources favor this over repeated library normalization in small blocks.
- **API classification**: `x` is required; explicit `output_dir` triggers the official monolith while `NULL` preserves custom in-memory composition. Lookup objects and `reuse` are policies; pruning/reduction/model tuning uses named lists owned by each helper. Canonical coalesces, generic-class candidate restrictions, the 1% unresolved cap, names, progress, and assessments are inferred. The one-caller reassignment helper remains internal.
- **Dependencies**: Reuse base R, `data.table`, existing package dependencies, and existing download helpers; move recommended package `cluster` from Suggests to Imports and restore the supplied/reference `cluster::pam(pamonce = 6)` medoid path. Add no Excel-writing dependency. Network/large comparison stages guard the actual OSF/AWS hosts and skip only in routine tests, while the official workflow fails with actionable recovery guidance.
- **Compatibility/docs**: Treat the return/model layout and default output side effects as public changes. Update the vignette, roxygen, examples, and `NEWS.md`; regenerate with configured roxygen and inspect `NAMESPACE`/`man` diffs rather than editing them.
- **Bundled Shiny/pipeline diagram**: N/A; no `inst/shiny` source or diagram stage changes. **Hosted**: `R/` and vignette inputs trigger fast `-HostedAppStatic`; run matching-artifact preflight when a candidate medoid/model bundle is staged. A clean wasm rebuild is deferred unless library pins/images or release artifacts are updated.

## Package Surfaces And Work Checklist

- [ ] `R/build_lib.R`: require explicit sources, discover adjacent helper data, fix regex classification, and retain official/custom modes, assessments, progress, validated reuse, checkpoints, and promotion.
- [ ] `workflows/OpenSpecy_reference_library.R` and curated CSVs: retain source/output finders, pass explicit paths to one call, apply reviewed exact-name corrections, and keep the exact-vs-regex curation audit.
- [ ] `tests/testthat/test-build_lib.R`: cover missing inputs, helper discovery, regex escapes, exact recommendation mappings, unresolved-class cap, constrained generic reassignment, end-to-end shape, reuse, assessments, outputs, alignment, and legacy filenames.
- [x] Benchmarks: retain saved-build comparison evidence and the seeded 1,000-spectrum internal probe; compare the superseded pure-R PAM path with compiled PAM for medoid equivalence and representative runtime.
- [ ] Vignette, HTML diagram, NEWS, and generated help: document explicit paths, helper discovery, all five reference inputs to splitting, return schema, reuse, assessments, and migrations.

## Verification

- Focused: resolve Windows Rscript, then `devtools::test(filter = "build_lib|match_spec|manage_lib", reporter = "check", stop_on_failure = TRUE)`; assert no stable identity/hash appears on both sides of the split and every artifact passes `check_OpenSpecy()` where applicable.
- Benchmark/staging: first use a fixed seed, temp output, and 1,000 sampled legacy raw spectra as an internal probe; then run the separately guarded complete old/new assessment against all seven legacy artifacts and inspect every `assessments` table plus aggregate/legacy RDS reloads. Interrupt/restart a probe to prove only manifest-compatible checkpoints are reused; never publish or replace downloaded/official files.
- Same-output portions: compare the retained straight-line implementation to the monolith for IDs, axes, spectra at declared recipe tolerances, metadata, attributes, pruning/medoid choices, and runtime; flag >10% slowdown unless justified by the new assessment work.
- Final candidate: confirm configured roxygen version, run `devtools::document()` and inspect generated diffs, render `vignettes/library-builder.Rmd`, run full `devtools::test()` once, then `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -Filter shinylive_wasm -HostedAppStatic`. `devtools::check()`/CI R CMD check is required before release, not routine iteration.
- Reusable evidence: focused/split/benchmark results remain valid only while builder, lookup CSVs, comparison artifacts/revisions, seed, processing/matching/model contracts, and dependencies are unchanged.

## Risks And Open Questions

- Legacy model training membership cannot be reconstructed from downloaded models; label those scores as compatibility evidence and use the leakage-free library/new-model split for inferential comparisons.
- Full old/new identification and `assess_spec()` evaluation may be expensive; keep it manual/CI-guarded, checkpoint it, and report complete logs without weakening the final full-library evidence requirement.
- PAM still requires a quadratic dissimilarity object for each group; group-level correlation/PAM timings expose this bound, while the compiled engine keeps the largest observed 18,971-spectrum group finite.

## Approval Notes

- Approved by: user implementation request, 2026-08-27.
- Follow-up: run the guarded complete seven-artifact assessment and R CMD check before publishing a release.
