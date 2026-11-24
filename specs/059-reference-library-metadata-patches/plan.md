# Feature Plan: Reference-Library Metadata Patches

**Feature dir**: `specs/059-reference-library-metadata-patches`  
**Date**: 2026-11-23  
**Current tranche**: Add canonical app-facing dataset metadata, make it native in future dataset-local builds, and create one immutable metadata-only child release from the completed r7 libraries.  
**Change class**: package/scientific plus hosted/release (official external artifacts, workflow governance, and internal source-key precedence; app runtime code unchanged)

## Goal And Scope

- Give every retained spectrum a nonblank, human-readable `library_name` and a controlled `spectrum_origin` equal to `Observed` or `Computed`.
- Patch the completed r7 release without rerunning cleaning, recipes, pruning, medoids, or models; preserve all spectra, axes, IDs, attributes, and non-target metadata exactly.
- Make the same rules run at the per-dataset artifact boundary in future builds, so regular final joins only validate them and do not perform metadata cleanup.
- Out of scope: changing Shiny controls, source classifications, spectra, dispositions/quarantine, class mappings, medoids/models, publication, hosted pins, or remote synchronization.

## Requirements

- R1. A tracked slug-keyed dictionary covers all 68 admitted datasets exactly once; `library_name` is unique, nonblank, descriptive rather than acronym-only, and `spectrum_origin` derives from manifest `origin` (`experimental` to `Observed`; `computed_solid`/`computed_molecule` to `Computed`).
- R2. Dataset-local typed artifacts receive both fields before their immutable manifest is written; final join rejects missing, blank, unknown, or inconsistent values and never synthesizes them.
- R3. A generic post-join patcher creates a signed child release from an immutable parent, processes one typed artifact at a time in bounded metadata/fingerprint blocks, writes new files atomically, and never mutates the parent.
- R4. The patch audit proves identical wavenumbers, spectral values, column IDs, attributes, row order, and all metadata except the two governed fields; all six typed artifacts and three recipe bundles re-read as valid aligned `OpenSpecy` objects.
- R5. The constitution and workflow guidance distinguish temporary metadata-only migrations from full rebuilds and require each migration rule to become dataset-native before the next full build.

## Technical Decisions

- **Approach**: store canonical names in `workflows/data/reference_dataset_metadata.csv`; derive `spectrum_origin` from the admitted manifest; share one by-reference, 100,000-row metadata updater between dataset-local promotion and the post-join migration script.
- **Release contract**: parent pointer/index/artifact hashes, admitted manifest hash, dictionary hash, consumed workflow hashes, rule version, 100,000-row metadata block, and 5,000-column spectral fingerprint block determine the child signature. Output target is `rawrefdata/updatedv1/Processed/reference-library-overhaul-r7-metadata-v1`.
- **OpenSpecy contract**: only metadata columns `library_name` and `spectrum_origin` may differ. Strong block fingerprints plus exact structural comparisons protect `wavenumber`, `spectra`, `sample_name`/column alignment, attributes, and untouched metadata.
- **Resource boundary**: process one typed artifact and one recipe bundle at a time; stop above 80% RAM, below 50 GiB free disk, on changed parent/input hashes, or after 10 minutes without progress. Preserve completed child artifacts for signature-compatible resume.
- **Public API/dependencies/generated docs**: no export or dependency change. Internal `build_lib()` source-key precedence preserves populated `library_name` and fills only blanks; its existing roxygen details and generated Rd are refreshed.
- **Reference workflow compatibility**: compare child versus r7 for exact spectral fingerprints, IDs/counts/axes/attributes and target-only metadata deltas; quarantine/dispositions are inherited unchanged and rehashed, not regenerated.
- **Bundled Shiny/pipeline diagram**: no app source or analysis-reactive change; `library_name` is prepared as the future filter label. `.specify/memory/pipeline-diagram.html` is N/A; update the reference build diagram only.
- **Hosted impact**: `R/build_lib.R` is shared hosted source, so the fast `-HostedAppStatic` gate is triggered. No route/runtime assembly, pin, dependency, or staged small-library artifact changes require exact-artifact preflight or wasm rebuild.

## Package Surfaces

- `workflows/reference_library_staging.R`: shared metadata rules, future dataset-local integration, final-join validation, and child-release patch helpers.
- `R/build_lib.R`, generated `man/build_lib.Rd`, and `tests/testthat/test-build_lib.R`: preserve populated `library_name`, fill only blanks, and lock precedence behavior.
- `workflows/patch_reference_library_metadata.R`, `workflows/data/reference_dataset_metadata.csv`: generic CLI migration and canonical dictionary.
- `tests/testthat/test-reference-library-staging.R`: coverage, derivation, exact preservation, resume/signature, and failure tests.
- `benchmarks/reference_library_staging.R`: repeated metadata-only patch allocation/runtime and exact spectral equivalence.
- `.specify/memory/constitution.md`, `.specify/templates/plan-template.md`, `AGENTS.md`, build diagram, and owning skills: durable/synchronized guidance.
- `NEWS.md`: record the canonical fields and metadata-only child-release workflow. `DESCRIPTION`, `inst/`, `site/`, README/pkgdown, and deployment workflows remain unchanged.

## Work Checklist

- [x] Amend constitution 3.24.0 to 3.25.0 and synchronize affected planning/rebuild guidance and the reference build diagram.
- [x] Add the canonical 68-dataset dictionary and shared dataset-level metadata normalization/validation.
- [x] Add the signed immutable post-join patch script, exact-preservation audit, tests, benchmark, and NEWS entry.
- [x] Run focused tests/benchmark and a small ordinary/forced-resume child-release probe.
- [x] Freeze r7 parent/mapping/code hashes; create and audit the complete metadata-v1 raw/derivative/nobaseline child release with no medoid/model work.
- [x] Reconcile evidence, inspect processes and `git status`, and clean task scratch while retaining signed production logs/audits.

## Verification And Approval

- Focused `build_lib` and `reference-library-staging` tests, deterministic benchmark, regenerated-document diff review, full package tests, and fast `-HostedAppStatic`. Stage R CMD check if the focused/full gates reveal broader package effects; exact hosted-artifact preflight and wasm rebuild are not triggered.
- Production audit: 68/68 dictionary coverage, zero blank target fields, exact allowed origins, six typed plus three bundle hashes/counts, exact spectra/axes/IDs/attributes/untouched metadata, valid serialization, unchanged parent/quarantine hashes, and no medoid/model artifacts.
- Evidence: focused `build_lib` 1,137 PASS; staging 48 PASS; full package 5,365 PASS with two explicit large-AWS skips; `HostedAppStatic` 391 PASS plus hosted script checks; repeated benchmark deterministic with exact protected hashes.
- Release: child `0377ccf73c12c1e39e9acdecb02c986536c5203a393d12e3b75b8c939ed42af0` from parent `66b66ec8e00e95f674170de44cffd5af3752e9f1caffb60640e3e5a8e939a059`; 68 datasets (57 Observed, 11 Computed), 5,461,080 typed rows, and recipe totals derivative 1,973,851, nobaseline 1,477,496, raw 2,009,733.
- Production stayed below the 80% RAM stop line (observed peak 66.5%) with at least 185.31 GiB free at recorded boundaries. The first partial signature was preserved as invalidated after exposing a reread-retention issue; the corrected fresh run passed exact post-write and parent-rehash audits.
- Approved by: maintainer request on 2026-11-23. Publication and remote synchronization remain maintainer-owned.
