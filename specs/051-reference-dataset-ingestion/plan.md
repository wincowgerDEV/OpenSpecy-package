# Feature Plan: Reference Dataset Intake and Library Rebuild

**Feature dir**: `specs/051-reference-dataset-ingestion`  
**Date**: 2026-10-19  
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.  
**Current tranche**: Normalize the 21 measured or physically simulated Raman/IR candidates from the shared research, admit eligible records, and stage an audited Open Specy library rebuild.  
**Change class**: Hosted/release with package/scientific workflow changes; the bundled and hosted app runtimes are otherwise unchanged.

## Goal

- Add reproducible, provenance-rich `OpenSpecy` inputs for every eligible candidate while keeping large downloads, cleanup code, and generated objects local under ignored `rawrefdata/`.
- Feed only validated and legally redistributable spectra into `build_lib()`, with complete disposition accounting and a quarantine containing every ingested spectrum later removed and why.

## Scope

- **Candidate inventory (experimental)**: 1 RaSPI/RaMPI (`10.5683/SP3/8UQQQN`); 2 FTIR-Plastics C4/C8 (`10.5281/zenodo.11176740`); 3 textile fibres (`10.17632/rx3fjgz96x.3`); 4 API Raman (`10.6084/m9.figshare.27931131`); 5 USGS v7 (`10.5066/F7RR1WDJ`).
- **Candidate inventory (rights/provenance review)**: 6 BLoP (`10.1021/acs.est.5c18940`); 7 INFRA-ART (`10.5281/zenodo.21993883`); 8 RamanBiolib (`mteranm/ramanbiolib`); 9 MLROD (`10.48667/yv5q-2606`).
- **Candidate inventory (computed solids)**: 10 CRD (`10.6084/m9.figshare.29458907`); 11 Canepa inorganic Raman/IR (`caneparesearch/project_Raman` plus its archival release); 12 WURM; 13 C2DB Raman subset.
- **Candidate inventory (computed molecules)**: 14 QM9S (`10.6084/m9.figshare.24235333`); 15 QMe14S (Figshare `889262a4e999b5c9a5b3`); 16 Raman/IR-ChEMBL (`m9.figshare.28593698`, `28594295`); 17 SQuIRL (`m9.figshare.30734843`); 18 RXN4Chemistry (`10.5281/zenodo.14770232`); 19 IR-NMR 177K (`10.5281/zenodo.16417648`); 20 MVE (Materials Cloud `2021.205`); 21 NASA Ames PAH IR.
- **In**: source/license recording; local downloads outside actively populated `FTIR/` and `Raman/`; one portable cleaner/folder per dataset; cleaned-output validation; class curation after save; workflow integration; comprehensive quarantine; cohort probes and review.
- **Out**: candidate 22's artificial-class synthetic patterns; RamanBench as an independent source; promotion of records with unresolved redistribution rights; hand-editing generated docs; hosted artifact deployment; remote synchronization.
- **Users**: maintainers preparing, rebuilding, auditing, and later publishing Open Specy's measured and computed reference collections.

## Requirements

- R1. Keep `/rawrefdata/` ignored. Each candidate uses `rawrefdata/clean_<slug>.R`, `rawrefdata/<slug>/` for raw files, and `rawrefdata/<slug>/Processed/CleanedOS_raw.rds`; one cleaner owns one folder. Do not create, modify, rename, or delete anything under the concurrently populated `rawrefdata/FTIR/` or `rawrefdata/Raman/` boundaries.
- R2. A local ignored manifest records candidate/slug, canonical release, download date, checksum/size, citation, source-declared license, maintainer use basis, origin, technique/mode, cleaner/output hashes, and state. Where no clear grant exists, retain `license_status = "not_source_verified"` and separately record the maintainer-directed noncommercial/fair-use, CC-BY-NC-like assumption rather than falsely attributing a license to the publisher.
- R3. Cleaners are standalone and idempotent, accept a dataset path, avoid `setwd()` and fixed drives, read cached data without hidden network calls, and atomically replace `CleanedOS_raw.rds` only after R4 passes.
- R4. The pre-save gate requires: source-verified `metadata$spectrum_type` containing canonical `ftir` or `raman`; faithful per-spectrum `metadata$intensity_units` (transmission data use `transmittance`, plus `instrument_mode = "transmission"` when applicable); a rendered full-range envelope plus deterministic representative/outlier spectra reviewed as visually reasonable; and `isTRUE(check_OpenSpecy(candidate))` with warnings treated as failure. The object must also have aligned finite spectra/metadata, stable IDs, provenance, citation, license, and release identifiers; re-read and recheck a temporary RDS before atomic promotion.
- R5. Preserve physical meaning: transmittance is not relabeled absorbance; maps/pixels and processing derivatives are not independent materials; calculated modes need documented scaling/broadening. QMe14S is precleaned out-of-core to a 6 cm^-1 grid and content-hash deduplicated before constructing `OpenSpecy`; retain a stable source-to-survivor/removal ledger.
- R6. The workflow pairs explicitly admitted `clean_<slug>.R` scripts and folders, runs/validates selected cleaners, records script/raw/output hashes, and passes the exact `CleanedOS_raw.rds` list to `build_lib()`; it must not execute every legacy root script or recursively ingest arbitrary RDS files.
- R7. Curate exact identities before regex fallbacks, normalize terminal numeric extensions, stop on regex clashes or duplicate keys, and report exact/regex/unmatched coverage by dataset before admission.
- R8. Every successfully ingested spectrum balances to retained, reassigned, or removed. Quarantine stores the full removed spectrum plus stable ID, source, stage, rule/reason, and relevant threshold/comparison evidence; unreadable or invalid source records are separate ingestion failures, never quarantine substitutes.

## Technical Decisions

- **Approach**: use manifest-driven intake, deterministic normalization, then cohort probes. Withhold incomplete records and do not let million-record sources force an unreviewed default-library policy. No full `build_lib()` run occurs until cleaned data and class mappings are reviewed.
- **Public API/dependencies**: no new export or speculative `build_lib()` flag; keep orchestration in `workflows/` and internal helpers. Reuse installed readers where possible and review any new format dependency before adding it to `DESCRIPTION`.
- **OpenSpecy/generated artifacts**: preserve object parts, IDs, attributes, units, and origin. Update `R/build_lib.R`, tests, and roxygen, then regenerate `NAMESPACE`/`man/` with the configured toolchain; never edit generated files directly.
- **External resources**: download only from the canonical DOI/repository hosts above into `rawrefdata/<slug>/`; capture redirects, versions, checksums, terms, and failures. Network acquisition is separate from offline cleaning and rebuilding.
- **Reference compatibility**: compare staged results with release 049 for IDs, axes, metadata, warnings, coverage, retention, matches, model schema, and reviewed source-addition deltas.
- **Official-build contract**: freeze the admitted manifest plus raw/cleaner/output/package/lookup hashes and output root before launch. Preserve per-artifact pruning/reassignment, `cross_class = FALSE` (strict `> 0.9` closure disabled), flat `logistic_regression`, seed/holdout, clean `reuse = FALSE`, signature/checkpoints, and the 049 legacy comparator unless a separately approved plan changes policy.
- **Frozen validation**: pin positive-control inputs, truth, artifacts, and controls; run only after probes pass, never tune against holdout outcomes, and invalidate evidence when a pin changes.
- **Performance/observability**: the pool exceeds 1.6 million records and QMe14S is about 56.66 GB; no monolithic first run. Probe each cohort's largest format and the prior ~42,096 x 600 correlation envelope; record projected time/RAM/disk and restart boundaries; stop at >80% RAM, >2x projection, or 10 minutes without progress.
- **Apps/diagrams/hosted**: bundled Shiny and `.specify/memory/pipeline-diagram.html` are unchanged; update `.specify/memory/build-lib-diagram.html` with intake/admission/quarantine flow. Changes to `R/` trigger `-HostedAppStatic`; exact-artifact preflight or clean wasm rebuild is deferred unless runtime libraries, routes, dependencies, or pins are promoted.

## Package Surfaces

- `rawrefdata/` (ignored): intake manifest, 21 dataset folders, raw downloads, one root cleaner per dataset, and `*/Processed/CleanedOS_raw.rds`; confirm with `git check-ignore` and never force-add.
- `workflows/OpenSpecy_reference_library.R`: manifest-selected cleanup/output validation, exact local input list, hashes, source failures, staged output, and explicit existing build policy.
- `R/build_lib.R`, `tests/testthat/test-build_lib.R`, `benchmarks/reference_release_postbuild_audit.R`: comprehensive quarantine/disposition accounting, focused regressions, and release audit; benchmark is release assessment, not a same-output optimization benchmark.
- `workflows/data/classes_reference.csv`, `workflows/data/classes_regex.csv`: exact mappings and narrowly scoped regex additions with coverage/clash evidence; `.specify/memory/build-lib-diagram.html`: synchronized workflow map.
- `DESCRIPTION`, `NEWS.md`, roxygen/generated docs: update only when implementation changes require them; app/site/deployment sources remain unchanged.

## Work Checklist

- [ ] Verify all 21 canonical releases; create the ignored manifest/folders outside active `FTIR/`/`Raman/`, download raw files, record checksums, and distinguish declared licenses from the maintainer use assumption.
- [ ] Write one portable `clean_<slug>.R` per dataset; require the type, units, visual-plot, and `check_OpenSpecy()` pre-save gate; then generate each exact `Processed/CleanedOS_raw.rds` and its validation report.
- [ ] After each validated `CleanedOS_raw.rds` is saved, curate class mappings and resolve duplicates, overlaps, provenance, map/replicate independence, and measured-versus-computed admission.
- [x] Add manifest-selected `rawrefdata/` code/output execution and hashing to the workflow; update the build diagram and deterministic driver tests/static assertions.
- [x] Extend `build_lib()` and audits so all post-ingestion removals enter quarantine with reasons; add balance, source-failure, empty/nonempty, and checkpoint/signature tests.
- [ ] Run format/subset probes, then separately staged measured and computed cohort builds; freeze the accepted contract before any full production build and run legacy plus frozen-control assessment.
- [ ] Run focused tests, documentation, full tests/check, and `-HostedAppStatic` once on the final candidate; reconcile evidence, processes, status, and scratch files.

## Verification

- Per dataset before final save: verify `ftir`/`raman` against the source, verify intensity semantics and transmission metadata, inspect the deterministic full-range diagnostic plot, require warning-free `isTRUE(check_OpenSpecy())`, then re-read/recheck the temporary RDS; also reconcile raw/clean counts and duplicate/derivative/map policy.
- Curation: exact/regex/unmatched coverage, terminal-extension normalization, zero clashes/duplicate keys, and manual review of new mappings by source.
- Workflow/build: parse and one-production-call assertions; identical manifest reruns yield identical hashes; missing/stale/mismatched cleaner-folder-output triples fail before `build_lib()`; staged writes never replace the current release.
- Spectrum disposition: source successes/failures are explicit; `ingested = retained + removed` at each artifact boundary with reassignments tracked; quarantine contains every removed spectrum and why, and an empty quarantine is valid only when removal count is zero.
- Gates: compact focused `build_lib`/reader tests, `devtools::document()` when roxygen changes, one final `devtools::test()` and `devtools::check()`, fast `-HostedAppStatic`; no browser/exact-wasm/full-wasm gate unless a hosted artifact or runtime contract changes.
- Long workflow: record source/kernel dimensions, expected and actual time/RAM/disk, progress/checkpoints/stop decisions, release manifest/signature, postbuild audit, 049 comparison, and frozen positive-control outputs without tuning.

## Risks And Open Questions

- Unclear source terms remain explicitly unresolved even when local noncommercial use proceeds under the maintainer assumption; any later public redistribution decision must preserve that distinction and source attribution.
- Before production, the maintainer must approve whether very large molecular computations are a separately downloadable computed collection or a reproducibly curated subset of the default library; no plan step may silently merge them into default/hosted artifacts.

## Approval Notes

- Implementation approved by maintainer on 2026-10-19; full `build_lib()` execution and artifact publication remain deferred until joint review of new cleaned data and class mappings.
