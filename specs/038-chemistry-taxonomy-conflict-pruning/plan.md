# Feature Plan: Chemistry Taxonomy And Cross-Class Conflict Pruning

**Feature dir**: `specs/038-chemistry-taxonomy-conflict-pruning`  
**Date**: 2026-09-29  
**Review budget**: Under 100 nonblank lines and approximately 1,500 words.  
**Current tranche**: Remove physical paint labels from the material-class ontology, map the updated PLOPP binder identities to chemistry classes, normalize plastic class names, and add independent-library evidence pruning before generic reassignment.  
**Change class**: Package/scientific; taxonomy labels and retained reference spectra can change.

## Goal

- Make `material_class` chemistry-only while retaining binder detail in `spectrum_identity` and `paint` in `material_form`.
- Remove high-confidence cross-class reference conflicts with an auditable, deterministic weight-of-evidence rule before generic classes are reassigned.

## Scope

- **In**: 18 PLOPP binder identities (17 chemistry-bearing labels plus `other paint`) from `OpenSpecy_Paint_Binder_Labels.xlsx`; removal of paint classes; `poly...` naming for standard plastic classes; class/common-use crosswalk updates; a public `prune_lib()` switch and correlation threshold; official-build audits; a clean full rebuild from all source RDS files.
- **Out**: changing raw spectral values, automatically relabeling a conflict instead of removing it, treating spectral similarity as proof of chemistry, app UI changes, publishing the rebuilt artifacts, or changing model algorithms.
- **Users**: maintainers running `build_lib()` and reviewers auditing chemistry assignments and removals.

## Requirements

- R1. Exact PLOPP identities map to chemistry materials/classes: acrylic to polyacrylates; alkyd/polyester and alkyd modifications to polyesters; urethane and acrylic-urethane to polyurethanes; nitrocellulose to polycellulose derivatives; vinyl acetate to polyvinylesters; vinyl chloride combinations and chlorosulfonated polyethylene to polyhaloolefins; acrylic-epoxy to polydiglycidyl ethers; polyvinyl formal to polyvinylalcohols; ethylene-acrylate to polyolefins; vinyl-/styrene-acrylic to polyacrylates; unresolved `other paint` to `other plastic`. Exact binder wording remains `spectrum_identity`; `material_form` remains `paint`.
- R2. Remove `paint`, `acrylic paint`, `alkyd paint`, and `urethane paint` from `material_class`, hierarchy, regex outcomes, and common-use keys. Existing generic paint/varnish identities map to defensible chemistry when explicit and otherwise `other plastic`.
- R3. Every reviewed standard class with `material_type == "plastic"` starts with `poly`; `other plastic` remains the sole explicit catch-all exception. Rename cellulosics, EPDM, silicones, and SBR to chemistry labels beginning with `poly`, and update every dependent lookup/test/document.
- R4. `prune_lib()` adds `cross_class = FALSE` and `cross_class_threshold = 0.9`. The threshold is a Pearson correlation in `[0,1]`; the switch is explicit for composable backward compatibility, while the official derivative and nobaseline workflow enables it by default. No second `build_lib()` flag duplicates the existing `prune` policy surface.
- R5. Before any `other`, `other plastic`, or `other material` reassignment, compare spectra only within the existing Raman or FTIR/NIR pools. Exclude generic/unclassified rows as queries and candidates. A conflict requires correlation strictly above the threshold and a different non-generic `material_class`.
- R6. Same-source spectra do not count as independent evidence. Conflict weight is the count of distinct populated *other* `library_name` values represented by cross-class matches. Process active spectra from highest weight downward; a higher-weight endpoint is removed, equal-weight endpoints in a conflicting pair are both removed, and deterministic IDs break ordering only, never the scientific tie rule. Retained spectra must have no remaining eligible cross-class match above threshold.
- R7. Record every flagged/removed spectrum, opposing spectrum/class/library, correlation, evidence-library count, decision round, threshold, and reason in `prune_report` and official upstream assessment evidence. Preserve `wavenumber`, retained intensity columns, metadata alignment, attributes, and stable IDs.
- R8. Existing nearest-other-class pruning runs after conflict removal and excludes unresolved generic/unclassified classes from target schedules and candidate sets. Small-class handling and class floors remain otherwise unchanged.
- R9. Validate exact keys, hierarchy completeness, absence of paint classes, and the `poly` invariant. Update common-use keys without inventing new use evidence; PLOPP rows inherit use from their chemistry class.
- R10. Run a subset probe and representative conflict-kernel benchmark before a clean `reuse = FALSE` full build from the 36 processed sources plus `library_raw.rds`; compare with the current installed/release artifacts by counts, shared/missing IDs, axes, metadata, warnings, class changes, removals, hashes, and representative matches.

## Technical Decisions

- **Public API**: `cross_class` is a demonstrated policy choice owned by exported `prune_lib()`; `cross_class_threshold` is its primary tuning parameter. Canonical `library_name` is inferred upstream and required only when enabled. Helpers and the graph/block scan remain internal. `build_lib(prune = ...)` stays the composable entry point.
- **Algorithm**: normalize once using the existing CO2-excluded pruning matrix. A bounded block pass finds eligible cross-class correlations and independent-library counts; ordered resolution removes the strongest-conflict node first and both endpoints on equal evidence. Recheck the retained set as a postcondition before generic reassignment.
- **OpenSpecy contract**: only complete spectra/metadata rows are removed. Filtering uses `filter_spec()`; axes, column/row order, IDs, and valid attributes remain aligned. Removal is intentionally scientific and fully audited.
- **Dependencies/generated artifacts**: reuse base R, `data.table`, and existing matrix helpers; no dependency change. Update roxygen sources and generate help with configured roxygen2 8.0.0; never edit `NAMESPACE` or `man/*.Rd` directly.
- **Reference compatibility**: exact binder identities and renamed classes are expected deltas. Spectral hashes for shared retained IDs must match; unmatched differences must reconcile to source additions, quality gates, or pruning audit rows.
- **Performance and observability**: benchmark at least 1,000 spectra × representative wavenumbers/libraries before production. Use bounded correlation blocks and report pool, pass/round, eligible spectra, conflicts, removals, elapsed time, and memory-relevant dimensions. Prior clean production was 25,459 s; budget this candidate at 8–12 hours and <48 GiB. Stop at a checkpoint if conflict pruning exceeds 2× the measured projection, the R process is CPU-inactive for 15 minutes without progress, or memory pressure approaches the machine limit; isolate/optimize, then restart clean.
- **External resources**: read-only source RDS files under the authorized H: tree; staged output under a new external root. No network is required except legacy retrieval if the installed artifacts are absent.
- **Bundled Shiny/pipeline diagram**: N/A; no app reactive path changes. Update `.specify/memory/build-lib-diagram.html` because the official reference-build pruning sequence changes.
- **Hosted Shinylive/WebAssembly**: shared `R/` and vignette inputs trigger fast `-HostedAppStatic`; no runtime, route, dependency, pin, image, or assembly change, so matching-artifact and clean-wasm tiers are N/A.

## Package Surfaces

- `R/build_lib.R`, `R/zzz.R`, `R/particle_image.R`: API, validation, cross-class kernel, ordering, audit schemas, official assessment plumbing, progress, signatures, and the matching class-color key. `tests/testthat/test-build_lib.R`: taxonomy/API/tie/evidence/generic/alignment/end-to-end regressions.
- `workflows/data/{classes_reference,classes_regex,material_hierarchy,common_use_reference}.csv`: exact binder mappings and chemistry-only class vocabulary. `workflows/OpenSpecy_reference_library.R`: environment-overridable staged output while preserving current defaults.
- `benchmarks/reference_cross_class_pruning.R`: representative runtime/memory, deterministic result, retained-set postcondition. `vignettes/library-builder.Rmd`, `NEWS.md`, `.specify/memory/build-lib-diagram.html`: scientific behavior and review guidance. `DESCRIPTION`, `.github/workflows/`, `inst/`, `site/`, README: unchanged.

## Work Checklist

- [x] Curate binder mappings, remove paint classes, normalize plastic class names, and validate all dependent workflow tables.
- [x] Implement and audit cross-class conflict pruning before generic reassignment, including official default wiring and assessment evidence.
- [x] Add focused taxonomy, API, weight/tie, threshold, generic-exclusion, postcondition, and object-alignment tests plus benchmark/probe fixtures.
- [x] Update roxygen/vignette/NEWS/build diagram and regenerate/inspect generated documentation.
- [x] Run focused tests, benchmark, vignette, full tests, and fast hosted-static gate on the final source candidate.
- [x] Run and monitor the clean full external build, improve/restart if budgets fail, and compare promoted staged artifacts with legacy/current libraries.
- [x] Reconcile every checkbox with evidence; inspect processes and `git status`, remove task scratch, and identify retained external artifacts.

## Verification

- Focused: configured R 4.3.3 parse plus `devtools::test(filter = "build_lib|match_spec", reporter = "check", stop_on_failure = TRUE)`; exact/regex clash and hierarchy/common-use validators; read-only PLOPP coverage probe.
- API/algorithm: invalid switch/threshold/library provenance errors; strict `>` boundary; same-class and generic exclusions; independent-library counts; higher-weight single removal; equal-weight pair removal; deterministic order; no retained eligible conflict; unchanged retained spectra/axes/attributes.
- Benchmark/docs/broad: run `benchmarks/reference_cross_class_pruning.R` before one final `devtools::document()`, generated diff audit, vignette render, one full `devtools::test()`, and `-HostedAppStatic`. `devtools::check()` is deferred because this is not release/CRAN publication work.
- Long workflow: inventory inputs, write full logs/checkpoints outside the repository, emit progress at source/core/quality/prune/partition/medoid/model/assessment/promotion boundaries, and preserve the failed checkpoint/log for focused diagnosis before any restart.
- Clean build: completed in 22,716 seconds at `reference-library-build-chemistry-conflicts-20260929/releases/5aeb0ccd7321`; all 11 promoted files exist, match recorded sizes and SHA-256 checksums, and all 15 OpenSpecy partitions validate. The derivative/no-baseline recipes retained 62,491/58,873 spectra after 1,372/6,371 cross-class removals. PLOPP retained all 263 paint spectra with all 18 binder identities, `material_form = paint`, chemistry classes, and no paint material class.
- Assessment recovery: typed legacy Raman/FTIR/NIR bundles are now accepted and empty legacy comparisons fail clearly. The assessment-only rerun completed in 1,544 seconds at `reference-library-assessment-rerun-20260930/releases/e2d1941530ef`; reference, medoid, model, compatibility, and 70 functionality comparisons are populated, every manifest checksum verifies, and all nine scientific artifacts are byte-identical to the clean candidate release.
- Legacy comparison: all 20,925 shared Raman spectra were numerically unchanged; 22,088 of 22,577 shared FTIR spectra were unchanged. The 489 reconciled FTIR differences comprise 105 newly declared transmittance conversions plus 271 Chabuka and 113 NIST records whose current sources omit intensity units. Acquisition mode is not substituted for signal units because that would double-convert the already transformed NIST records; the build warning leaves these sources visible for metadata-owner correction.
- Closure: verify checklist evidence, no owned R workers remain, repository scratch is clean, and the external staged release is named with its manifest/hash; publishing remains maintainer-owned.

## Risks And Open Questions

- Correlation above 0.9 can reflect genuinely similar chemistry, mixtures, derivatives, or preprocessing rather than a bad label. The independent-library rule and complete audit reduce but do not eliminate expert-review risk.
- Composite paint binders map to one broad chemistry class for interoperability; their exact composite chemistry remains visible in `spectrum_identity` and must not be reconstructed from `material_class` alone.
- The current source metadata leaves 46,502 spectra without authoritative intensity units. The builder warns and preserves those values rather than inferring units from acquisition mode; source owners should review the missing-unit inventory before publication.

## Approval Notes

- Approved by: maintainer request on 2026-09-29 for implementation and a monitored clean full build.
- Follow-up: professional review remains required for binder mappings and all high-confidence cross-class removals before publication.
