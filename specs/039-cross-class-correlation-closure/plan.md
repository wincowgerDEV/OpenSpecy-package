# Feature Plan: Cross-Class Correlation Closure

**Feature dir**: `specs/039-cross-class-correlation-closure`  
**Date**: 2026-09-30  
**Current tranche**: Resolve same-library conflicts by iterative majority evidence, stop systematic ambiguity for review, and close retained derivative/no-baseline parents on full and medoid matching views.  
**Change class**: Package/scientific; libraries and assessments can change.

## Goal

- For every resolved class in the official derivative and no-baseline identification libraries, ensure no retained pair from different `material_class` values has correlation strictly greater than 0.9 on any deployed matching axis.
- Use iterative majority removal within libraries, pausing systematic ambiguity for review. Preserve every cross-class exclusion/hold in one metadata-rich RDS bundle, then apply later pruning and derive medoids only from closed parents.

## Scope

- **In**: same-library majority resolution/review; cross-library/final closure; parent filtering; medoid validation; audits; external quarantine review bundle; staged rebuild; review-directed correction, chemistry-class merge, or quarantine.
- **Out**: destructive raw-artifact pruning, changing 0.9, treating correlation as chemical identity, changing medoid/model algorithms, or publishing rebuilt files. Raw conflicts remain reported.
- **Users**: maintainers and matching users.

## Requirements

- R1. `cross_class = TRUE` first builds a graph per canonical `library_name`, spectrum type, and deployed view. Vertices are resolved spectra; edges join different classes whose deployed `cor_spec()` correlation is strictly above `cross_class_threshold`. Equality is retained; generic labels wait until reassignment.
- R2. Score vertices by active wrong-class neighbors, remove the unique maximum, and recompute. If adjacent endpoints tie at the maximum, remove both; order nonadjacent ties by stable ID and recompute. Record scores/rounds so block size and input order cannot change survivors.
- R3. Before mutation, emit class-pair/component diagnostics. Mark recurrent pairs and removals that threaten global support for review, quarantine disputed spectra, and continue without them; never enforce `min_n` within one `library_name`. After closure, each retained class/type across the complete recipe artifact must have at least `min_n` spectra. Reviewers may later correct labels or merge at a resolvable chemistry level. No pair allowlist may break the guarantee; archival spectra remain preserved.
- R4. Only after internal resolution, run the existing different-library pass: count distinct opposing libraries, remove the higher-evidence endpoint first, remove both on a tie, and retain deterministic ordering.
- R5. Existing generic and undersupported-class reassignment/pruning follows. Freeze labels, then repeat closure so reassignment cannot introduce a conflict; drop newly undersupported remnants without another label mutation.
- R6. Evaluate rounded output intensities on every typed axis and `.lib_restrict_model_range()` (FTIR/Raman 800--3200; supported NIR). The final postcondition covers all resolved pairs/provenance; legacy 2200--2420 exclusion cannot weaken it.
- R7. Filter derivative/no-baseline `OpenSpecy` parents by stable ID before medoid/model construction, preserving axes, spectra, metadata, attributes, and sources. Never prune/relabel completed medoids; validate that each medoid is an unchanged parent spectrum and that complete parent-to-medoid searches have zero qualifying wrong-class matches.
- R8. Audit phase, view, component/pair, IDs/classes/libraries, correlation, degree/evidence, round, threshold, decision, and status. Export per-spectrum/pair CSVs with affected/total counts, degree distribution, correlation range, source recurrence, and disposition.
- R9. Atomically checkpoint `output_dir/review/quarantined_spectra.rds` after closure so later build failures preserve it. A successful build promotes it beside `assessments.rds` and records its hash in the release manifest. Include every spectrum excluded or held by internal, independent, or final cross-class closure, but not unrelated pruning; write a valid empty bundle when none qualify.
- R10. Keep `prune_lib()` arguments and object/IDs/report returns. `cross_class` activates this policy and `cross_class_threshold` remains its tuning input; review criteria derive from global class/type `min_n` and provenance.
- R11. Rebuild with `reuse = FALSE`; compare all library, assessment, and quarantine artifacts with `e2d1941530ef`, reconcile every removal/hold, and prove zero qualifying conflicts in typed outputs and medoid searches.

## Technical Decisions

- **Approach**: Emit provenance-aware edges for multiple views. Apply a deterministic active-degree resolver after an ambiguity gate, construct exact output views, filter parents once, derive medoids/models, then enforce the terminal postcondition on frozen labels/views.
- **Public API**: no new argument or export; quarantine is a derived builder artifact when cross-class pruning runs.
- **OpenSpecy contract/dependencies**: reuse `filter_spec()`, `cor_spec()`, `data.table`, and block operations; add no dependency. Stable IDs map views to one parent; `wavenumber`, `spectra`, `metadata`, and attributes stay aligned.
- **Medoid cause**: pruning currently precedes rounding and uses a different range; later restricted searches can cross 0.9. Multi-view parent closure removes that mismatch.
- **Scientific guardrail**: Correlation/HQI is similarity, not calibrated identity ([Clough et al. 2024](https://doi.org/10.1021/acs.est.4c05167)); weathering/additives alter spectra ([Miller et al. 2022](https://doi.org/10.1038/s41597-022-01883-5)), and mode/range affect accuracy ([De Frond et al. 2023](https://doi.org/10.1016/j.chemosphere.2022.137300)). Review uses class-balanced fractions, within/between distributions, characteristic bands, metadata, source recurrence, and complementary evidence; automation never declares chemical equivalence.
- **Quarantine artifact**: one versioned RDS list contains `spectra` (valid full-parent `OpenSpecy` objects by recipe/type), `conflicts` (edge audit), and `manifest` (schema/build ID, threshold, source hashes). Original metadata gains `quarantine_status`, `quarantine_phase`, `quarantine_reason`, `component_id`, `correlation_view`, `threshold`, `decision_round`, `active_degree`, `max_wrong_class_correlation`, `opposing_classes`, `opposing_libraries`, `class_n_before`, and `min_n`. Composite recipe/type/stable IDs prevent collapse. This external review artifact is not package data or a published library.
- **Docs**: update roxygen, vignette, NEWS, and reference-build diagram; regenerate Rd with roxygen2 8.0.0 and inspect diffs.
- **Performance**: benchmark 1,000 x 600 and the largest source. Target 10--16 hours and <48 GiB; checkpoint phases. Stop at 2x projection, near memory limit, or 15 inactive minutes; optimize/resume.
- **Bundled Shiny/pipeline diagram**: no `inst/shiny` reactive, control, output, asset, or `.specify/memory/pipeline-diagram.html` change. The separate reference-build diagram changes to show internal closure, independent evidence, final-view closure, and medoid validation.
- **Hosted Shinylive/WebAssembly**: `R/` and vignette changes trigger fast `-HostedAppStatic`. The quarantine RDS is never staged; no route/runtime/dependency/pin change or clean wasm rebuild.

## Package Surfaces

- `R/build_lib.R`: graph resolver/review gate, quarantine writer, audits, closure, medoid validation, caches. `tests/testthat/test-build_lib.R`: majority/ties, review stops, quarantine schema/round-trip, phase ordering, threshold, multi-view closure, OpenSpecy alignment, medoid invariant, and recovery.
- `benchmarks/reference_cross_class_pruning.R`: internal/external multi-view scaling, deterministic equivalence across block sizes, memory estimate, and runtime guard. `workflows/OpenSpecy_reference_library.R`: unchanged call surface; clean run uses stricter official defaults.
- `workflows/data/classes_reference.csv` and `classes_regex.csv`: change only after approved correction/merge. Vignette, NEWS, reference-build diagram, and generated Rd explain the guarantee/review gate. Other surfaces remain unchanged.

## Work Checklist

- [x] Implement majority resolution, review gate, CSV audits, versioned quarantine RDS, independent/final closure, and audits in `R/build_lib.R` without public arguments.
- [x] Filter full parents before medoid/model construction; add fail-closed parent/subset and complete parent-to-medoid correlation invariants.
- [x] Add focused current-behavior tests and update the representative pruning benchmark.
- [x] Update roxygen, vignette, NEWS, and reference-build diagram; regenerate and inspect documentation.
- [x] Run focused tests/benchmark, full tests, vignette validation, and fast hosted-static gate once on the final source candidate.
- [x] Run subset/largest-source probes, then a clean monitored external rebuild and old/new artifact/assessment comparison with zero-conflict acceptance evidence.
- [x] Reconcile checkboxes/evidence, record deferred gates, inspect processes/status, and clean scratch artifacts.

## Verification

- Focused acceptance: a high-degree mislabeled hub is removed before valid neighbors; degrees recompute; adjacent maximum ties remove both; nonadjacent ties are deterministic; exact 0.9 survives; recurrent pairs and globally class-eroding removals are quarantined with review evidence while a source library may fall below `min_n`; internal resolution precedes external weights; final classes have zero qualifying pairs on both views.
- Medoid acceptance: IDs/classes are an unchanged subset of the closed parent; no medoid-specific pruning/relabeling; complete parent-to-medoid wrong-class correlations above 0.9 equal zero for derivative/no-baseline Raman, FTIR, and NIR.
- Object/audit: retained and quarantined signals equal parents by composite ID; metadata/attributes validate; every cross-class removal/hold reconciles to RDS metadata plus edge audit; empty and fail-before-exit bundles round-trip; manifest sizes/hashes verify.
- Gates: configured R 4.3.3; focused tests/benchmark, generated-diff audit, vignette render, full tests, and `-HostedAppStatic` passed. The maintainer-requested staged `R CMD check` passed with 0 errors, 0 warnings, and the existing packaged-data UTF-8 NOTE; the clean rebuild completed in 24.2 hours with 12/12 release hashes verified and zero qualifying parent or parent-to-medoid conflicts.
- Long workflow: probe one high-conflict source and Raman/FTIR/NIR views; keep external logs/checkpoints; compare `e2d1941530ef` IDs, axes, shared spectra, counts, audits, medoids, model accuracy, warnings, functionality, and postconditions.
- Reusable evidence: the 2026-09-30 assessment recovery establishes the baseline and typed-bundle compatibility. Source changes invalidate prior pruning tests/build hashes but not the baseline artifact itself.

## Risks And Open Questions

- Raw degree is biased by class size and duplicate density: in a complete two-class conflict, every member of the smaller class has the larger degree and could be erased even when correctly labeled. The systematic-ambiguity stop prevents that failure mode; professional review remains required before publication.
- Truly distinct classes can be spectrally indistinguishable on a deployed axis. Retaining both labels while also promising zero cross-class correlation above 0.9 is mathematically incompatible; resolution must improve/correct evidence, merge to a defensible resolvable chemistry class while preserving `spectrum_identity`, or quarantine the disputed spectra from the identification artifact.
- The guarantee applies to resolved derivative/no-baseline identification artifacts and their medoids. Raw remains assessment-only and can still contain greater-than-0.9 wrong-class matches unless a later approved plan extends destructive pruning to raw.

## Approval Notes

- Approved by: maintainer direction on 2026-09-30 for internal-library majority resolution before independent-library pruning and no separate medoid pruning/reclassification.
- Follow-up: implementation and a new monitored clean rebuild require explicit `$speckit-implement`; publishing remains maintainer-owned.
