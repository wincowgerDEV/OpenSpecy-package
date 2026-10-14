# Feature Plan: Curated Taxonomy And Strict Gated Polymer Hierarchy

**Feature dir**: `specs/048-gated-polymer-family-taxonomy`  
**Date**: 2026-10-12  
**Current tranche**: Retain generic manufactured spectra through correlation reassignment, correct high-confidence taxonomy errors, add a material-group routing layer, replace new hierarchical fits with strict gates, and remove the production correlation bottleneck before frozen validation.  
**Change class**: Package/scientific behavior plus long-running external library/model validation.

## Goal

- Route `material_type -> material_group -> material_class` by top-1 gates, never composing confidence across levels.
- Improve labels and build speed while preserving leaf reporting, legacy artifacts, and locked holdout discipline.

## Scope

- **In**: reviewed lookup/hierarchy fixes; generic-reference correlation reassignment; organic-molecule correction; a group crosswalk; dense correlation reuse; gated logistic training/inference; multi-metric selection; staged rebuild; and holdout confirmation.
- **Out**: quarantining generic manufactured spectra instead of reassigning them; final class merges; a monolithic vinyl class; pooled/joint probability promotion; random forests; holdout-driven tuning; artifact publication; app UI changes; and remote synchronization.
- **Deferred taxonomy**: EVOH, SEBS, mixed copolymers, halogenated diene/styrene precedence, contested rayon/cellulose status, and composites without a defensible primary chemistry.

## Requirements

- R1. Change exact `pva` to `other plastic`; correct reviewed rubber aliases; move poly(4-vinyl phenol) to `polystyrenes`; add PVP, poly(4-vinylpyridine), and polyketone leaves; narrow source-dependent acrylic/nitrile/rubber regexes. Exact lookup wins and clashes remain fatal.
- R2. Retain bare paint, varnish, rubber, latex, resins, and unresolved blends/composites by reassigning `other`, `other plastic`, and `other material` to the highest-correlation eligible non-generic `material_class` within the spectral pool. Preserve current eligibility: `other plastic` searches plastic targets, `other material` searches organic matter/mineral, and `other` searches any established class; add no correlation cutoff. Preserve original identity/form plus matched ID, class, and score. If no finite candidate exists, retain and audit the generic row rather than delete it. Reassigned rows then follow ordinary closure, support, medoid, and model rules. Map curated named organic monomers/small organic molecules directly to retained `organic matter`/`not plastic` before this step, never by correlation to a plastic leaf or bulk token matching.
- R3. Add one complete `material_class -> material_type + material_group` path per leaf. Groups cover olefinic, diene-elastomer, styrenic, vinyl alcohol/acetate/acetal, acrylic, halogenated, ester/carbonate, nitrogen-carbonyl, ether/epoxy, cellulose, siloxane, and specialty routes with copolymer precedence. Optimize only with >=100 spectra/5 organizations and each child >=25/3.
- R4. No general `vinyl` group. Use a narrow vinyl alcohol/acetate/acetal route for PVOH/PVB/PVF, PVAc/polyvinyl esters, and EVA while keeping leaves distinct; PVP, vinylpyridine, and source-concentrated vinyl ethers remain specialty routes.
- R5. `train_spec_model()`/`build_model_lib()` accept ordered `hierarchy_col` names; `NULL` stays flat. A schema-versioned gated model chooses root top-1 and scores only the selected child path to its terminal node; one-child gates are deterministic. Never compose probabilities; saved flat/pooled artifacts predict unchanged.
- R6. Gated `name` remains the leaf and `value` is `P(leaf | selected path)`. `top_n` exposes only terminal-group leaves. Preserve match columns, add routed names/node probabilities, and keep particle recovery calculations unchanged.
- R7. Reuse leaf-stratified, identity-stable folds at every node and report grouped/source-held-out root, group, routed-leaf, worst-source, log-loss, and Brier metrics. Select lexicographically: keep overall accuracy within 0.01 of its maximum; then keep minimum root recall, supported-class recall, worst-source accuracy, and macro accuracy within 0.01 of each eligible best; minimize node-local loss/Brier and prefer the largest lambda. Do not invent end-to-end probability.
- R8. Before Stage 2, OS1 medoid/model recovery accuracy and RSD reproduce within 1.0 absolute percentage point overall and by sample/type/size after the two exclusions; otherwise return to Stage 1. FTIR model fitting and inference use the same 800--2200 plus 2420--3200 axis already specified by that workflow, never training CO2-region predictors that particle inputs fill. Frozen Stage 2 compares OS1, plan-047, taxonomy-clean flat/two-level, and strict three-level candidates under identical particle settings and existing promotion gates.
- R9. For each immutable artifact/type/view, normalize once and allocate a canonical full double correlation matrix keyed by stable IDs, axis, processing/rounding state, range, and spectral hashes. Same-view conflicts, removals, relabeling, medoids, and validation index it; label/active-set changes never recompute it. After all arbitrary-score consumers finish, losslessly compact the full-range view to exact directed edges strictly above its fixed threshold before allocating a changed numeric view; reject lower-threshold reuse. Decisions/reasons/order/survivors remain exact and `>0.9` stays strict.

## Technical Decisions

- **Architecture/API**: broaden the hierarchy input, not a mode flag. Emit a generic node tree with `model_type = "gated_hierarchical_logistic_regression"` and `model_schema = "OpenSpecy_gated_hierarchy_v1"`; legacy hierarchical artifacts remain loadable.
- **Taxonomy and reuse**: keep chemistry in `material_hierarchy.csv` and add `material_group_reference.csv`. Generic assignment indexes the same correlation matrix used by pruning; taxonomy edits invalidate labels and downstream artifacts, but verified pre-prune spectra may be reused after ID, axis, intensity-hash, metadata, and warning comparison.
- **Diagnostics/artifacts**: store topology, support/source diversity, lambdas/folds, node metrics, routed OOF confusion, first failed gate, semantics, and provenance; recursively slim nodes and bump signatures without changing flat aliases.
- **OpenSpecy contract**: preserve aligned `wavenumber`, `spectra`, `metadata`, IDs, attributes, and history. Correlation matrices use an immutable ID index; object filtering changes only an active mask, and label changes only class masks.
- **Measured performance design**: the 24.223-hour build spent 16.23 hours (67%) in pruning/final closure. On the actual pre-closure derivative FTIR set (42,096 x 600), normalization took 5.88 s; the full 13.203-GiB matrix took 22.32 s; a complete `>0.9` class/library query took 142.52 s; indexing 6,340 medoids took 3.80 s; and a 10%-removed/2%-renamed requery took 165.25 s, with observed process private memory about 24.8 GiB. At 4,000 spectra, the frozen independent pass took 5.80 s versus 1.53 s for dense build plus query. Make dense reuse the current-machine default, preflight exact matrix/working-memory bytes, compact completed threshold-only views before allocating a changed range, and keep bounded blocks only as a lower-memory fallback. A 5,413 x 400 x 12 grouped gate exceeded 570 s; ungrouped finished in 141 s but worsened representative gate accuracy, macro accuracy, log loss, and Brier score, so retain grouped penalties and a one-hour full-fit ceiling with review at 20 minutes. Target <=5 minutes per production matrix query, <=2 hours total correlation work, and <5 minutes per map; stop at 2x measured projection, 80% available memory, or 10 minutes without progress.
- **Docs/generated/dependencies**: no new dependency. Update roxygen/vignette/NEWS, regenerate with configured roxygen, and inspect generated diffs.
- **Bundled Shiny/pipeline**: no app change. Update `.specify/memory/build-lib-diagram.html` for generic reassignment, correlation reuse, and gated nodes; the app pipeline diagram is unchanged.
- **Hosted impact**: `R/` is shared hosted input, so run fast `-HostedAppStatic`. No app, route, staged-library, pin, dependency, or assembly change; matching-artifact preflight and clean wasm rebuild are N/A unless later promotion stages a gated artifact.

## Package Surfaces

- `R/build_lib.R`: lookup validation, generic reassignment, view-keyed correlation matrix lifecycle/indexing, path trainer, selection, artifacts, assessments, checkpoints, and roxygen. `R/match_spec.R`: strict routed prediction with legacy dispatch. Particle workflows preserve rank-1 path audit metadata without changing recovery calculations.
- `workflows/data/{classes_reference,classes_regex,material_hierarchy,common_use_reference,material_group_reference}.csv`: reviewed chemistry and routes.
- `tests/testthat/{test-build_lib,test-gated-model-reporting,test-match_spec,test-automate_particle_analysis}.R` plus file-backed tests: mappings, constrained nearest-class reassignment, cache invalidation/equivalence, topology, source/fold reporting, routing, serialization, legacy parity, and dense/fallback parity.
- `benchmarks/reference_cross_class_pruning.R`: freeze the old kernel; measure real dimensions, dense lifecycle, exact decisions/audits, memory, view invalidation, relabel/removal reuse, medoid indexing, and fallback. `benchmarks/gated_hierarchy_validation.R`: freeze settings/hashes, checkpoint fits/maps, and write `Positive_Controls/gated_hierarchy_validation` without tuning from it. `benchmarks/gated_candidate_reference_assessment.R` and `gated_model_development_assessment.R`: bind complete-reference confusion to the exact candidate hash and reconcile it with OOF/holdout evidence.
- `vignettes/library-builder.Rmd`, `NEWS.md`, `.specify/memory/build-lib-diagram.html`, generated `man/build_lib.Rd`/`man/match_spec.Rd`: changed. `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README/pkgdown: unchanged.

## Work Checklist

- [x] Curate the P0 exact/regex/hierarchy fixes, organic monomers/molecules, generic labels, and group crosswalk; validate coverage, clashes, source support, and copolymer precedence.
- [x] Route generic manufactured spectra through constrained nearest-class correlation reassignment using the shared matrix; preserve identity/form and complete match/no-op audits, then add regressions and update the build diagram.
- [x] Implement the fingerprinted dense correlation lifecycle and index-based consumers; prove frozen-kernel equivalence and guarded fallback before any production rebuild.
- [x] Implement schema-versioned gated training, source-aware diagnostics/selection, checkpointing, assessment tables, recursive slimming, and official artifact handling.
- [x] Implement strict routed inference and particle path metadata while proving flat and saved pooled artifacts predict unchanged.
- [x] Update tests, roxygen, vignette, NEWS, and generated docs; inspect all generated and taxonomy diffs.
- [x] Stage the downstream rebuild from verified pre-prune spectra; enforce correlation/runtime gates, Stage 1, then one frozen Stage 2 and emit confusion, accuracy/RSD, runtime, and taxonomy-impact evidence.
- [x] Run focused tests, full `devtools::test()`, and fast hosted-static verification; reconcile checkboxes, owned processes, `git status`, and scratch cleanup.

## Verification

- Focused: compact `devtools::test(filter = "build_lib|match_spec|automate_particle", reporter = "summary")`; assert exact-first zero-clash lookup, constrained generic-to-leaf assignment, direct organic mapping, no-candidate retention, one path per leaf, no fold leakage, hard routing, and legacy prediction identity.
- Staged reference/model: compare reused core and rebuilt outputs by IDs, axes, spectra hashes, metadata, warnings, quarantines, class/group/source support, representative joins/matching, and artifact hashes; keep logs/checkpoints outside the repository.
- Benchmark/holdout: compare every cached/fallback removal, reason, correlation, order, survivor, medoid, and fingerprint to the frozen kernel, including 0.9+/-epsilon and relabel/removal fixtures; enforce R7/R8 and report gate errors, PE-to-PVA/nonplastic, and PVA false positives. The controls contain no true PVA, so require source-held-out PVOH/PVB/PVAc/EVA evidence.
- Final gates: run the gated benchmark before one configured `devtools::document()` and generated-diff audit, then one full test suite and `-HostedAppStatic`. R CMD check is deferred unless artifact promotion becomes release-facing.
- Reusable evidence: plan-047 truth/scorer/exclusions and completed saved-candidate outputs remain reusable while their inputs/contracts are unchanged; taxonomy, trainer, prediction, medoid, or benchmark changes invalidate the corresponding downstream evidence only.

## Risks And Open Questions

- Strict routing makes an upstream error unrecoverable; acceptance therefore protects each gate and reports error propagation rather than allowing strong leaf metrics to mask a weak root.
- Generic nearest-class labels are spectral proxies, not chemical proof; retain source identity/form and report assignment-score distributions. Named organics may expose real organic-versus-polymer `>0.9` ambiguity but must not be reassigned as polymers. Dense matrices are ephemeral and view-specific: stale fingerprints, lossy threshold compaction, or concurrent full-size views are release-blocking.

## Approval Notes

- Approved by: maintainer request on 2026-10-12 for taxonomy repair, a polymer-group layer, strict gating, and improved model selection.
- Validation outcome: the hash-bound 363-bin diagnostic probe reproduced Stage 1 exactly and ran all 20 maps below 100 seconds, but failed 11/15 locked non-inferiority checks (worst exact-polyethylene delta -60.64 percentage points); do not promote it. A future promotable artifact also requires fresh parent-to-medoid closure.
- Follow-up: publication/staging of a winning model remains a separate maintainer decision.
