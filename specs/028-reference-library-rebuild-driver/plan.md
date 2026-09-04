# Feature Plan: Reference Library Full-Rebuild Driver

**Feature dir**: `specs/028-reference-library-rebuild-driver`
**Date**: 2026-09-04
**Status**: Complete
**Current tranche**: Leave `workflows/OpenSpecy_reference_library.R` ready for the maintainer to start and, if interrupted, resume the complete official build.
**Change class**: package/scientific workflow maintenance

## Goal And Requirements

- Keep the maintainer's hard-coded `data_dir`, processed-source, raw-source, and output paths grouped at the top of the script.
- Load the current hard-coded package source tree with `devtools::load_all()` rather than the obsolete installed OpenSpecy release before resolving `build_lib()`.
- Validate the processed directory, raw library, discovered processed RDS files, and output location before the long build starts.
- Make exactly one explicit `build_lib()` call that requests the complete official workflow with progress, generic-row removal, legacy comparison, and manifest-compatible checkpoint reuse.
- Remove stale post-build exploratory code and hard-coded references to an older release; leave the returned aggregate in `reference_library_build` and print its promoted release location.

## Technical Decisions

- No package API, scientific algorithm, helper table, generated documentation, or artifact format changes. The driver uses the existing `build_lib()` contract and adjacent `workflows/data` discovery.
- `reuse = TRUE` permits safe restart while manifests force changed or incompatible stages to rebuild; a fresh output root still performs every stage.
- The production run remains maintainer-owned. Verification will parse the script, exercise its real path discovery with a stubbed builder when those external paths are mounted, and run the focused builder workflow test—not the full rebuild.
- Hosted impact: N/A. `workflows/` is not consumed by the hosted build, so no hosted gate is triggered.

## Work Checklist

- [x] Clean and harden `workflows/OpenSpecy_reference_library.R` as the single-call full-rebuild driver.
- [x] Parse the script and verify real source/helper discovery without invoking production computation.
- [x] Run the focused `build_lib` workflow test and inspect status/processes/scratch before handoff.

## Verification And Risks

- Expected production inputs: one raw `library_raw.rds`, at least one RDS below a `Processed` directory, and all six curated CSVs beside the script under `data/`.
- The full build is intentionally not run by Codex in this tranche; its elapsed time is expected to be hours, with progress and completed stages persisted beneath `output_dir/checkpoints`.
- If interrupted, rerunning the unchanged script reuses only manifest-compatible checkpoints.

Verification result: the script parses and its single `build_lib()` call explicitly supplies `previous_library_dir = "system"`, `reuse = TRUE`, `remove_other = TRUE`, and `progress = TRUE`. A stubbed end-to-end driver preflight loaded the current source with `devtools::load_all()`, found 35 processed RDS files plus `library_raw.rds`, resolved all six curated helper tables, and reached the expected output location without starting production computation. The focused `build_lib` test file passed. No full rebuild, documentation, full suite, hosted gate, or R CMD check was triggered by this workflow-only maintenance change.

## Approval Notes

- Approved by maintainer request, 2026-09-04; the maintainer will launch the production rebuild.
