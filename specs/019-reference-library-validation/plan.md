# Feature Plan: Reference Library Taxonomy, Pruning, And Recipe Performance

**Feature dir**: `specs/019-reference-library-validation`  
**Date**: 2026-08-26  
**Review budget**: Under 100 nonblank lines and about 1,500 words.  
**Current tranche**: Correct taxonomy and saved-build metadata, integrate ordered top-match pruning into `build_lib()`, and accelerate derivative/baseline recipes without changing their scientific output.
**Change class**: package/scientific

## Goal

- Make the official lookup/hierarchy/type/drop tables accurate, source-traceable, and effective on the genuine 67,738-spectrum build.
- Make class reassignment and top-match pruning a tested package stage before medoid selection or model training, without allowing the largest classes to control the outcome.
- Reduce derivative and nobaseline recipe times while preserving `OpenSpecy` values, axes, metadata alignment, identifiers, and attributes.

## Scope

- **In**: `build_lib()` and its existing metadata/processing helpers; exported `prune_lib()` and `predict_class_reference()`; the existing reference-library script; column harmonization; `classes_reference.csv`, separate `classes_regex.csv`, `material_hierarchy.csv`, `library_types.csv`, and `metadata_drop_columns.csv`; a reusable curation skill; pruning QA; recipe profiling, tests, and benchmarks.
- **Saved-build evidence**: read-only inspection of `reference-library-build/libraries.rds`; staged rebuilt outputs must use a different temporary directory and never overwrite it.
- **Out**: New standalone build/validation workflows, new taxonomy dimensions, degradation modeling, cloud publication, and replacing hosted or packaged library artifacts.

## Planning Baseline

- The RDS contains valid aligned `raw`, `derivative`, and `nobaseline` `OpenSpecy` objects, each 1,983 x 67,738 with 232 metadata columns (about 1.18 GiB expanded per object). IDs and axes align.
- Current class lookup has 12,123 unique, nonblank, nonconflicting keys, but matches only 49,199 of 67,490 populated identities; 18,291 spectra across 12,493 identities remain unmatched.
- Deterministic gaps include 3,000 spectra across 30 `_ref.csv` aliases whose suffix-free keys already map, and 303 spectra whose identity already equals a hierarchy material. The remaining `_ref` labels, fouling/“like” labels, morphotypes, abbreviations, and unknowns require explicit review rather than broad fuzzy matching.
- Elise Granek/Kellie Teague contribute 12,412 unmatched numbered identities; 5,190 are MFFRC-style keys. Their material/producer/sample components can be parsed, but only 249 currently resolve to a reviewed class/hierarchy label, so source-scoped mappings must be curated before expansion.
- `library_types.csv` covers 37,021 of 43,104 spectra with an organization. Missing organizations are `microplastix` (3,906), `nist` (1,352), `hcmr` (355), `cnr` (353), `vliz` (112), and `nicolas coca` (5); the existing NREL row lacks `library_type`. Another 24,634 spectra lack `organization`, but all fall into seven identifiable `user_name` source groups suitable for explicit alias fallback.
- Safe column coalesces are evidenced for `interpretation -> spectrum_identity` (9,637 overlapping values, all equal), `form_factor -> material_form` (1,352, all equal), `shape -> material_form` (805 complementary), `datatype -> data_type` (381 complementary), and `xunits` plus `x_unit -> wavenumber_units` (9,327 + 3,906 complementary). Do not merge `name/names`, `sample/samples`, `file/file_name`, or generated `sample_name` with source sample IDs.
- The configured drop table removes 70 columns present in this artifact plus five optional assessment columns. Nine all-missing columns are already covered, but malformed import fields, redundant parser fields, and legacy classifier outputs remain; scientific provenance must not be dropped merely because it is constant within one source.
- `rename_and_remove()`, `reassign_other_classes()`, and `prune_by_top_match()` remain absent from the package/workflow. PAM defaults are `min_n = 10`, exclusion of 2200–2420 cm-1, and separate FTIR/NIR versus Raman matching, but its hard-coded polyamide/polyacrylamide merge conflicts with the accepted taxonomy.

## Requirements

- R1. Move the three adipate polymers from the nitrile class to `polyesters`; separate `polyacrylamides` from renamed `polyamides`; retain nylons, Kevlar, Nomex, and aramids under `polyamides`; map bare `pa`/`pa_ref.csv` to polyamide rather than polyacrylamide.
- R2. Review all 66 Hauser spreadsheet notes by issue type. Apply source-supported corrections, preserve useful aliases, and report unresolved judgments; never delete a rare class solely for rarity.
- R3. Extend `lib_metadata_name_lookup()` with the five evidenced coalesces above plus unambiguous spelling forms such as `spectrumid -> spectrum_id` and `locationdescription -> location_description`. Preserve values and precedence, test complementary/identical/conflicting rows, and explicitly test the unsafe non-merges.
- R4. Keep `classes_reference.csv` strictly exact and store flexible reviewed patterns in `classes_regex.csv`. Apply exact material matches first, then use exported `predict_class_reference()` only on remaining blank materials; allow/report pattern overlap with populated exact rows, never overwrite exact values, and leave distinct-material clashes blank for manual exact specification. Permit source-scoped normalization only when it produces one unique reviewed material; retain the original identity and do not classify fouling, “like”, unknown, morphotype, or ambiguous abbreviations. Mark remaining gaps explicitly as `unclassified`/`unknown` in built artifacts without inventing chemistry.
- R5. Use one `organization` key in `library_types.csv`. Before joining, preserve a populated organization and otherwise fill it from exact `user_name`; fill only blank `library_type`/`spectrum_type` values so source metadata stays authoritative. Cover the six missing organizations, NREL, and seven user-only sources; use evidenced techniques (`nist` NIR; HCMR/CNR/VLIZ/Nicolas Coca and reviewed user-only FTIR sources; Microplastix mixed, so no forced technique) and assert both fields are complete on the saved build.
- R6. Update `metadata_drop_columns.csv` by category: remove superseded alias names after coalescing; add malformed/import-only fields (`1`, `v1`, `3997_91411`), obsolete raw-parser geometry/header fields, and reviewed legacy classifier/cluster outputs. Remove exact duplicates such as `col_id` and the redundant HCMR/LABS hit field. Retain license/citation/contact, stable current/old IDs, instrument/acquisition metadata, and canonical wavenumber units. A constant value alone is not a deletion rule.
- R7. Integrated builder QA must reject blank/contradictory/duplicate lookup keys, report regex fills/clashes/manual overrides and pre/post coverage by source, expose unmatched identities as blank exact reference rows plus a durable built-artifact `unclassified` queue, identify stale drop entries separately from optional columns, assert 100% final class/type coverage, and preserve spectrum/metadata order and attributes.
- R8. Add exported, pipe-compatible `prune_lib(x, ...)`, returning an aligned `OpenSpecy` by default and optionally an audit with retained IDs, class schedule, reassignments/removals, reasons, and counts. Reassignment is an internal helper and must use explicit metadata/type eligibility, not compound-string parsing or obsolete hard-coded chemistry merges.
- R9. Match relative, mean-replaced intensities in bounded blocks; exclude 2200–2420 cm-1; use FTIR/NIR and Raman pools; default `min_n = 10`; retain/report missing candidates. Freeze class order from initial counts, descending with lexical ties, and prune each target class to stability against the updated pool while respecting the protected floor and stable ID tie-breaking.
- R10. Add input-triggered `build_lib(prune = NULL)`. The official workflow completes and audits class coverage after building, then calls `prune_lib()` independently for derivative and nobaseline before `reduce_lib()` and `build_model_lib()`; raw remains unpruned and unresolved spectra remain outside similarity pruning until labeled.
- R11. Preserve derivative output at `1e-12` and baseline output at a measured tight tolerance (target `1e-10`), including NA locations/attributes. Target 2x derivative and 1.5x baseline speedups; retain only demonstrated improvements with no same-output case more than 10% slower.
- R12. Before class lookup, normalize `spectrum_identity` by removing recognizable path prefixes and every trailing file extension supported by `read_any()`; OPUS extensions are a terminal period followed only by one or more digits, including `.10` and longer values. Apply the same rule to exact lookup keys, attach a change-count audit, and reject post-normalization key collisions. Compress extension/path aliases from the reviewed exact CSV without losing resolved mappings. Populate source-level spectrum techniques from saved metadata, record mixed sources explicitly without using a mixed summary as a per-spectrum fallback, and correct MBARI to Raman.
- R13. Add a concise project skill for repeatable exact/regex curation: normalize exact keys, keep regex rules separate, run unmatched-only prediction, audit allowed overlaps and blocking clashes, retain scientific uncertainty, and validate read-only against saved metadata before staged rebuilds.

## Technical Decisions

- **Object flow**: read/merge -> name/value and spectrum-identity cleanup -> source aliases/lookups/hierarchy -> exclusions/deduplication -> recipes -> selected-recipe pruning -> full libraries -> medoids -> models. Every filter/join retains source order and `sample_name` alignment.
- **API**: `prune_lib()` and table-focused `predict_class_reference(metadata, regex_reference, return)` are public; one-caller kernels stay internal. Prediction preserves row order and populated exact materials, returning either the updated table or one audit with predictions, clashes, and overlaps. Explicit build lookup specs may merge one fallback key and fill lookup values without overwriting source metadata; `build_lib()` keeps one named `prune` input.
- **Taxonomy boundary**: do not copy `rename_and_remove()` wholesale. Canonical chemistry stays in reviewed CSVs; code performs only deterministic harmonization, reporting, and spectrum-supported pruning.
- **Performance/docs**: keep old same-output recipe implementations under `benchmarks/`; add pruning memory/determinism evidence. Update roxygen, vignette, and `NEWS.md`, then regenerate—never hand-edit `NAMESPACE`/`man`.
- **Bundled Shiny/pipeline diagram**: N/A. **Hosted Shinylive**: `R/` changes trigger fast `-HostedAppStatic`; matching-artifact validation waits for approved replacement libraries; no clean wasm rebuild in this tranche.

## Package Surfaces And Work Checklist

- [x] Update `R/build_lib.R` harmonization/source-alias QA and the four `workflows/data/*.csv` tables; apply/defer all attachment rows and saved-build unmatched groups.
- [x] Implement/test `prune_lib()` and enable derivative/nobaseline pruning in `workflows/OpenSpecy_reference_library.R` before medoid/model creation.
- [x] Profile and implement only proven derivative/baseline optimizations; retain old comparison code under `benchmarks/`.
- [x] Add focused `tests/testthat/test-build_lib.R` coverage for aliases, unsafe non-merges, source/type fallback, drop categories, coverage reports, ordered pruning, and alignment.
- [x] Complete final class coverage with reviewed Gicquel/MFFRC keys plus an explicit, identity-preserving `unclassified` review queue.
- [x] Merge organization/username before one type lookup; assert saved-build type coverage; curate regex-filled exact class-reference rows with clash/override QA.
- [x] Normalize and audit filename-derived identities; compress normalized class keys; audit remaining unmatched classes and source techniques against the saved build.
- [ ] Separate regex rules, implement/test unmatched-only prediction, support multi-digit OPUS extensions, and add/validate the reusable curation skill.
- [x] Rebuild a representative subset to temporary output; compare IDs, metadata schema/coverage, taxonomy distributions, pruning audit, axes, attributes, warnings, joins, and matches against this saved RDS.
- [x] Run focused tests/benchmarks, documentation, full tests once, and `-HostedAppStatic`; report readiness without publishing artifacts.

## Risks And Acceptance

- Source-derived identity parsing is dangerous without source scoping; every generated alias must trace to one reviewed base mapping. Ambiguous labels remain unmatched and reported.
- Organization aliases can overwrite provenance if applied broadly; fallback applies only when organization is missing and the exact alias has one target.
- Final acceptance requires count-delta tables for all four CSVs and zero unexpected column loss; R CMD check and hosted exact-artifact/clean rebuild remain release-stage work.
- Hauser rows requiring broad regrouping (fluoropolymer/styrenic/rubber/sector schemes, questionable higher-alpha-olefin identities, rarity, and degradation policy) are explicitly deferred; exact adipate, amide, aramid/Nomex, polyester-family, silicone, duplicate, typo, and common-name corrections are applied without fuzzy classification.

## Approval Notes

- Genuine validation increases class coverage from 49,199 to 59,538 after exact lookup and to 62,682 after unmatched-only regex prediction. Source-scoped normalization recovers another 179 classes; 4,877 spectra remain explicitly `unclassified`, giving 67,738/67,738 nonblank class coverage without speculative assignments. Identity cleanup compresses 20,502 prior exact rows to 12,917 normalized keys; 72 reviewed regex rules predict 3,144 spectra with zero clashes while reporting 1,796 allowed overlaps with authoritative exact rows. Organization/username coalescing fills library and spectrum type to 67,738/67,738, records mixed sources without forcing mixed values onto spectra, and fills MBARI as Raman. A 240-spectrum imbalanced derivative probe remained aligned; `unclassified` is protected from pruning.
