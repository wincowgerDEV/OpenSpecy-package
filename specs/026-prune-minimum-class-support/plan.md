# Feature Plan: Prune Minimum Class Support

**Feature dir**: `specs/026-prune-minimum-class-support`
**Date**: 2026-09-04
**Status**: Complete
**Current tranche**: Make `prune_lib()` enforce its class-support floor before correlation pruning and expose actionable exclusions in build assessments.
**Change class**: package/scientific

## Goal And Scope

- Remove every resolved material-class/spectral-pool group with fewer than `min_n` spectra before correlation-based pruning, so full libraries, medoids, and models begin from comparable eligible classes.
- Make excluded classes and spectra reviewable for taxonomy reassignment or targeted data acquisition.
- Out of scope: automatic reassignment of established rare classes, changing medoid/model tuning, taxonomy CSV edits, Shiny behavior, publishing artifacts, or remote synchronization.

## Requirements

- R1. `prune_lib()` resolves supported generic `other` labels, calculates class support within each `spectrum_type`, retains groups with `n >= min_n`, and removes groups with `n < min_n` before the correlation schedule or matrices are created. `min_n = 1` preserves every nonempty resolved class.
- R2. Missing/blank classes and generic labels that cannot be resolved retain their existing behavior; `unclassified` remains excluded from cross-class correlation pruning but must pass the same minimum-support gate.
- R3. `return = "report"` and the `prune_report` attribute add a stable `excluded_classes` table containing spectrum type, correlation pool, class, observed count, threshold, shortfall, spectra removed, action, and reason. Spectrum-level `removals` includes threshold removals with `reason = "class_below_min_n"` and typed missing match fields.
- R4. End-to-end and saved-library assessments add stable `assessments$pruning_excluded_classes`, including the artifact name. Progress reports the threshold-gate counts and names removed classes without flooding output.
- R5. Filtering preserves `OpenSpecy` wavenumbers, retained intensity columns, aligned metadata rows/IDs, and applicable attributes. Exactly-at-threshold classes remain eligible.

## Technical Decisions

- **Public API**: no new argument or export. This intentionally tightens the existing `min_n` meaning from a reduction floor to an eligibility floor plus reduction floor. The return list gains one named audit table; ordinary object/ID returns remain unchanged and composable.
- **Order**: generic placeholder reassignment establishes usable labels first; the support gate is then the first destructive pruning decision. This preserves the documented semisupervised pathway while preventing unresolved or established undersupported groups from entering expensive correlation pruning.
- **Grouping**: enforce support by normalized `spectrum_type + material_class`, matching downstream type-keyed medoid/model artifacts. Preserve existing correlation pools (`raman` and shared `ftir_nir`) only for nearest-match pruning. Report original class spelling and deterministic type/class order.
- **OpenSpecy contract**: filter through `filter_spec()` and validate retained spectra/metadata ID alignment in tests; no intensity, axis, or attribute transformations are introduced.
- **Generated artifacts**: update roxygen, vignette, and NEWS; regenerate `NAMESPACE`/`man` only with configured roxygen2 8.0.0 and inspect generated diffs.
- **Performance/observability**: the new vectorized class-count gate runs before correlation allocation and should reduce work. A fixture benchmark will compare eligible output and correlation scheduling; no same-output benchmark is required because output intentionally changes.
- **Reference compatibility**: run representative mixed-class fixtures plus a read-only audit of the completed candidate libraries to enumerate affected pool/class counts. A production rebuild is deferred unless explicitly requested because it would replace the completed external artifact set.
- **Bundled Shiny app / pipeline diagram**: N/A; no app source, reactive, control, or visible pipeline behavior changes.
- **Hosted Shinylive/WebAssembly**: `R/build_lib.R` is a shared hosted source, so run fast `-HostedAppStatic`. No hosted runtime, route, assembly, dependency, pin, or generated artifact changes; matching-artifact and clean-rebuild tiers are not triggered.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`: implement threshold-first removal, progress, stable schemas, report attributes, and assessment aggregation.
- [x] `tests/testthat/test-build_lib.R`: cover below/equal threshold, pool isolation, unclassified/generic behavior, removal reasons, schemas, alignment, and end-to-end assessment propagation.
- [x] Roxygen/generated help, `vignettes/library-builder.Rmd`, `NEWS.md`, and `.specify/memory/build-lib-diagram.html`: document the changed `min_n` contract, assessment review workflow, and stage order.
- [x] Run focused `build_lib` tests, representative timing/compatibility audit, documentation generation/diff review, full tests, and fast `-HostedAppStatic`; R CMD check remains deferred because this is not release/CRAN-facing.
- [x] Reconcile every checkbox with evidence; record deferred gates, inspect owned processes and `git status`, and remove task scratch.

Verification: 406 focused builder assertions and 3,186 full-suite assertions passed with zero failures; hosted static passed 304 assertions. Roxygen2 8.0.0 regenerated only `man/build_lib.Rd`; its parse and the vignette knit passed. A 1,000 × 400 threshold probe had 0.06 s median runtime. The completed candidate audit found 21 Raman classes/59 spectra and one FTIR class/two spectra below 10 in each processed recipe; NIR had none. Production rebuilding/publishing and R CMD check remain explicitly deferred.

## Risks And Open Questions

- The default will intentionally remove rare classes that prior builds retained, reducing coverage while improving evaluation comparability; the class-level audit must make this loss explicit.
- Missing/blank spectrum types retain their existing behavior because no type-specific eligibility decision can be inferred; official builds already reject unresolved types before this stage.

## Approval Notes

- Approved by user request, 2026-09-04. Production artifact rebuilding/publishing remains a separate explicit operation.
