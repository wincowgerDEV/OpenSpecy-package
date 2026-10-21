# Feature Plan: Reference Dataset Intake and Library Rebuild

**Feature dir**: `specs/051-reference-dataset-ingestion`
**Date**: 2026-10-19
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.
**Current tranche**: Normalize the 21 researched candidates plus every independently sourced legacy dataset under the local `FTIR/` and `Raman/` transfer trees, admit eligible records, and stage an audited Open Specy library rebuild.
**Change class**: Hosted/release with package/scientific workflow changes; the bundled and hosted app runtimes are otherwise unchanged.

## Goal

- Create reproducible, provenance-rich `OpenSpecy` inputs under ignored `rawrefdata/`, then feed only validated, approved spectra to `build_lib()` with complete disposition and quarantine evidence.

## Scope

- **Experimental**: RaSPI/RaMPI, FTIR-Plastics C4/C8, textile fibres, API Raman, and USGS v7.
- **Rights/provenance review**: BLoP, INFRA-ART, RamanBiolib, and MLROD.
- **Computed solids**: CRD, Canepa inorganic Raman/IR, WURM, and the C2DB Raman subset.
- **Computed molecules**: QM9S, private QMe14S, Raman/IR-ChEMBL, SQuIRL, RXN4Chemistry, IR-NMR 177K, MVE, and NASA Ames PAH IR. Keep private QMe14S distinct from the admitted public TranSpec QM14S IR proxy (`10.6084/m9.figshare.26333170.v3`).
- **In**: provenance/licenses, downloads, read-only legacy inventory/correction, one cleaner/folder per source, validation, class curation, workflow integration, quarantine, and probes. **Out**: artificial-class patterns, RamanBench, unresolved redistribution, hosted deployment, generated-doc hand edits, and remote synchronization.

## Requirements

- R1. Keep `rawrefdata/` ignored and put all new intake code, downloads, outputs, and diagnostics under `rawrefdata/updatedv1/`. One `clean_<slug>.R` owns one `<slug>/` and exact `Processed/CleanedOS_raw.rds`; legacy manifests may point read-only into `FTIR/` or `Raman/`.
- R2. The local manifest records releases, dates, hashes, citations, origin, technique/mode, state, source-declared terms, and the separate maintainer use basis. Unclear grants remain `not_source_verified`; never attribute the maintainer's noncommercial/fair-use assumption to the publisher.
- R3. Cleaners are portable, standalone, idempotent, offline after acquisition, and atomically promote output only after validation.
- R4. Before saving, verify canonical `ftir`/`raman`, faithful intensity units and transmission mode, a reasonable full-range/representative/outlier plot, aligned finite data, stable provenance, and warning-free `check_OpenSpecy()`. Re-read and recheck the temporary RDS before promotion.
- R5. Preserve physical meaning: do not relabel transmittance, derivatives, pixels, or computed modes as independent measurements. Preclean QMe14S out of core at 6 cm^-1 with cross-chunk content-hash deduplication and a stable disposition ledger.
- R6. The workflow pairs explicitly admitted `clean_<slug>.R` scripts and folders, runs/validates selected cleaners, records script/raw/output hashes, and passes the exact `CleanedOS_raw.rds` list to `build_lib()`; it must not execute every legacy root script or recursively ingest arbitrary RDS files.
- R7. Curate exact identities before regex fallbacks, normalize terminal numeric extensions, stop on regex clashes or duplicate keys, and report exact/regex/unmatched coverage by dataset before admission.
- R8. Every successfully ingested spectrum balances to retained, reassigned, or removed. Quarantine stores the full removed spectrum plus stable ID, source, stage, rule/reason, and relevant threshold/comparison evidence; unreadable or invalid source records are separate ingestion failures, never quarantine substitutes.

## Technical Decisions

- **Approach**: use manifest-driven intake, deterministic normalization, then cohort probes. Withhold incomplete records and do not let million-record sources force an unreviewed default-library policy. No full `build_lib()` run occurs until cleaned data and class mappings are reviewed. The official build preserves source-provided `library_type` and audits a provenance-derived fallback where new sources lack that legacy field.
- **Public API/dependencies**: no new export or speculative `build_lib()` flag; keep orchestration in `workflows/` and internal helpers. Reuse installed readers where possible and review any new format dependency before adding it to `DESCRIPTION`.
- **OpenSpecy/generated artifacts**: preserve object parts, IDs, attributes, units, and origin. Regenerate `NAMESPACE`/`man/` only from changed roxygen; never edit them directly.
- **External resources**: download only from the canonical DOI/repository hosts above into `rawrefdata/updatedv1/<slug>/`; capture redirects, versions, checksums, terms, and failures. Network acquisition is separate from offline cleaning and rebuilding.
- **Reference compatibility**: compare staged results with release 049 for IDs, axes, metadata, retention, matches, model schema, and source-addition deltas.
- **Official-build contract**: freeze the admitted manifest plus raw/cleaner/output/package/lookup hashes and output root before launch. Preserve per-artifact pruning/reassignment, `cross_class = FALSE` (strict `> 0.9` closure disabled), flat `logistic_regression`, seed/holdout, clean `reuse = FALSE`, signature/checkpoints, and the 049 legacy comparator unless a separately approved plan changes policy.
- **Frozen validation**: pin positive-control inputs, truth, artifacts, and controls; run only after probes pass, never tune against holdout outcomes, and invalidate evidence when a pin changes.
- **Performance/observability**: the pool exceeds 1.6 million records and QMe14S is about 56.66 GB. Probe before a full run, record time/RAM/disk and restart boundaries, and stop at >80% RAM, >2x projection, or 10 minutes without progress.
- **Apps/diagrams/hosted**: bundled Shiny and `.specify/memory/pipeline-diagram.html` are unchanged; update `.specify/memory/build-lib-diagram.html` with intake/admission/quarantine flow. Changes to `R/` trigger `-HostedAppStatic`; exact-artifact preflight or clean wasm rebuild is deferred unless runtime libraries, routes, dependencies, or pins are promoted.

## Package Surfaces

- `rawrefdata/updatedv1/` (ignored): intake manifest, researched and pointer-backed legacy folders, downloads/source manifests, one cleaner per dataset, and `*/Processed/CleanedOS_raw.rds`; confirm with `git check-ignore` and never force-add.
- `workflows/OpenSpecy_reference_library.R`: manifest-selected cleanup/output validation, exact local input list, hashes, source failures, staged output, and explicit existing build policy.
- `R/build_lib.R`, its tests, and `benchmarks/reference_release_postbuild_audit.R`: quarantine/disposition accounting, regressions, and release audit.
- `workflows/data/classes_reference.csv`, `workflows/data/classes_regex.csv`: exact mappings and narrowly scoped regex additions with coverage/clash evidence; `.specify/memory/build-lib-diagram.html`: synchronized workflow map.
- `DESCRIPTION`, `NEWS.md`, roxygen/generated docs: update only when implementation changes require them; app/site/deployment sources remain unchanged.

## Work Checklist

- [x] Verify all researched releases and inventory every legacy transfer source; create ignored dataset folders outside active `FTIR/`/`Raman/`, record source paths/checksums, and distinguish declared licenses from the maintainer use assumption.
- [x] Write one portable `clean_<slug>.R` per dataset, repairing reusable legacy artifacts or re-reading raw spectra as required; enforce the type, units, plot, and `check_OpenSpecy()` pre-save gate, then generate each exact `Processed/CleanedOS_raw.rds` plus validation and disposition reports.
- [x] After each validated `CleanedOS_raw.rds` is saved, curate class mappings and resolve duplicates, overlaps, provenance, map/replicate independence, and measured-versus-computed admission.
- [x] Add manifest-selected `rawrefdata/` code/output execution and hashing to the workflow; update the build diagram and deterministic driver tests/static assertions.
- [x] Extend `build_lib()` and audits so all post-ingestion removals enter quarantine with reasons; add balance, source-failure, empty/nonempty, and checkpoint/signature tests.
- [x] Run format/subset and measured/computed cohort probes; freeze the validated intake/build-policy snapshot with production authorization false and unresolved external pins named.
- [ ] After joint data/class review and a decision on huge computed molecules, run the production build plus release-049 and frozen-control assessments.
- [x] Run focused tests, documentation when source comments require it, full tests/check, and `-HostedAppStatic` once on the final candidate; reconcile evidence, processes, status, and scratch files.

## Verification

- Per dataset before final save: verify `ftir`/`raman` against the source, verify intensity semantics and transmission metadata, inspect the deterministic full-range diagnostic plot, require warning-free `isTRUE(check_OpenSpecy())`, then re-read/recheck the temporary RDS; also reconcile raw/clean counts and duplicate/derivative/map policy.
- Curation: exact/regex/unmatched coverage, terminal-extension normalization, zero clashes/duplicate keys, and manual review of new mappings by source.
- Workflow/build: parse and one-production-call assertions; identical manifest reruns yield identical hashes; missing/stale/mismatched cleaner-folder-output triples fail before `build_lib()`; staged writes never replace the current release.
- Spectrum disposition: source successes/failures are explicit; `ingested = retained + removed` at each artifact boundary with reassignments tracked; quarantine contains every removed spectrum and why, and an empty quarantine is valid only when removal count is zero.
- Gates: compact focused `build_lib`/reader tests, `devtools::document()` when roxygen changes, one final `devtools::test()` and `devtools::check()`, fast `-HostedAppStatic`; no browser/exact-wasm/full-wasm gate unless a hosted artifact or runtime contract changes.
- Long workflow: record source/kernel dimensions, expected and actual time/RAM/disk, progress/checkpoints/stop decisions, release manifest/signature, postbuild audit, 049 comparison, and frozen positive-control outputs without tuning.

## Risks And Open Questions

- Unclear source terms remain unresolved even when local noncommercial use follows the maintainer assumption; later redistribution must preserve that distinction. Never represent public TranSpec QM14S IR as private QMe14S IR/Raman.
- Before production, the maintainer must approve whether very large molecular computations are a separately downloadable computed collection or a reproducibly curated subset of the default library; no plan step may silently merge them into default/hosted artifacts.

## Approval Notes

- Implementation approved by maintainer on 2026-10-19; full `build_lib()` execution and artifact publication remain deferred until joint review of new cleaned data and class mappings.
- Intake status on 2026-10-20: 19 researched candidates are admitted and gated (1,970,813 retained; 169,134 removals with reasons; 2,681 source failures). Of 52 legacy transfers, 49 are admitted (67,661 spectra), 2 review-only (24), and NIR is rejected; all 16,540 removals have full payloads/reasons and one empty source is a separate failure. Class audit has zero clashes/unmatched admissions; 483 spectra across 265 opaque identities are explicit exclusions. C2DB and NASA remain request-only.
- Four manifest intake probes passed. Official-mode 300/source probes also passed: measured 900 input / 44 quarantined in 108 seconds; computed 867 input / 187 quarantined in 206 seconds. Both quarantine ledgers exactly balance their spectral payloads and name stage/rule/reason. Probe comparison to release 049 was intentionally disabled; production retains the frozen 049 comparator, and no production `build_lib()` run has started.
- Final gates: focused/full tests and hosted static checks passed; 2 AWS integrations remain guarded. Staged package check passed with 2 existing notes after warning-order-independent test repair. No roxygen source changed.
