# Feature Plan: Reference Dataset Intake and Library Rebuild

**Feature dir**: `specs/051-reference-dataset-ingestion`
**Date**: 2026-10-19
**Review budget**: Keep this file under 100 nonblank lines and target no more than 1,500 words.
**Current tranche**: Normalize researched and independently sourced legacy datasets, admit eligible records, and complete an audited library rebuild.
**Change class**: Hosted/release and package/scientific; app runtimes unchanged.

## Goal

- Create provenance-rich ignored `OpenSpecy` inputs, then build only from validated spectra with complete disposition/quarantine evidence.

## Scope

- **Experimental**: RaSPI/RaMPI, FTIR-Plastics C4/C8, textile fibres, API Raman, and USGS v7.
- **Rights/provenance review**: BLoP, INFRA-ART, RamanBiolib, and MLROD.
- **Computed solids**: CRD, Canepa inorganic Raman/IR, WURM, and the C2DB Raman subset.
- **Computed molecules**: QM9S, private QMe14S, Raman/IR-ChEMBL, SQuIRL, RXN4Chemistry, IR-NMR 177K, MVE, and NASA Ames PAH IR; do not conflate QMe14S with public TranSpec QM14S IR.
- **In**: provenance, downloads, legacy correction, source-owned cleaners, validation, curation, build integration, quarantine, and probes. **Out**: artificial classes, RamanBench, unresolved redistribution, hosted deployment, generated-doc hand edits, and remote sync.

## Requirements

- R1. Keep all intake work ignored under `rawrefdata/updatedv1/`. One source-owned `clean_<slug>.R` declares input mapping, spectral semantics, metadata, and source-specific corrections for one folder and exact `Processed/CleanedOS_raw.rds`; shared helpers own only generic mechanics. Legacy pointers into `FTIR/`/`Raman/` are read-only.
- R2. The local manifest records releases, dates, hashes, citations, origin, technique/mode, state, source-declared terms, and the separate maintainer use basis. Unclear grants remain `not_source_verified`; never attribute the maintainer's noncommercial/fair-use assumption to the publisher.
- R3. Cleaners are portable, idempotent, offline after acquisition, and promote only after validation. The local declaration is authoritative; the catalog is an index, not hidden policy.
- R4. Before saving, verify `ftir`/`raman`, units/transmission mode, diagnostic plot, alignment/finite data, provenance, warning-free `check_OpenSpecy()`, and a serialization recheck.
- R5. Preserve physical meaning: do not relabel transmittance, derivatives, pixels, or computed modes as independent measurements. Preclean QMe14S out of core at 6 cm^-1 with cross-chunk content-hash deduplication and a stable disposition ledger.
- R6. Pair admitted cleaners/folders, validate selected outputs, record script/raw/output hashes, and pass the exact RDS list to `build_lib()`; never execute or ingest arbitrary files recursively.
- R7. Curate exact identities before regex, normalize terminal extensions, stop on clashes/duplicate keys, and report exact/regex/unmatched coverage before admission.
- R8. Every successfully ingested spectrum balances to retained, reassigned, or removed. Quarantine stores the full removed spectrum plus stable ID, source, stage, rule/reason, and relevant threshold/comparison evidence; unreadable or invalid source records are separate ingestion failures, never quarantine substitutes.
- R9. After exact content-hash removal, the official build removes a spectrum only when its correlation is strictly `> 0.99` to an earlier deterministic survivor from the same independent `library_name`, spectral type, and cleaned source identity (falling back to canonical material only when identity is unavailable). It must not compare distinct molecules merely because they share a broad class. Correlation work must be block-streamed with bounded memory; every removal records survivor ID, grouping fields, observed correlation, threshold, and operator in the ledger and quarantine. File-backed production merging uses output-identical two-pass staging rather than retaining every source matrix.
- R10. If one merged artifact or quarantine payload exceeds the frozen budget, materialize recipes and removals as deterministic file-backed blocks, process one artifact at a time, and promote hashed shards plus a complete disposition index; never require raw, derivative, no-baseline, and quarantine matrices to coexist.

## Technical Decisions

- **Approach**: manifest intake, deterministic normalization, exact hashes, source-local near-duplicate pruning, then probes. The exact admitted `updatedv1` list replaces opaque `library_raw.rds` input; raw inclusion is explicit opt-in. Preserve source `library_type` and audit fallbacks.
- **Public API/dependencies**: add one input-triggered numeric `build_lib()` argument, `dedupe_correlation_threshold = NULL`; `NULL` preserves prior behavior and the official driver supplies `0.99`. Keep identity-first grouping, stable survivor selection, and chunk dimensions internal and derived; add no dependency or paired boolean flag.
- **OpenSpecy/generated artifacts**: preserve object parts, IDs, attributes, units, and origin. Regenerate `NAMESPACE`/`man/` only from changed roxygen; never edit them directly.
- **External resources**: canonical DOI/repository downloads only; capture redirects, versions, hashes, terms, and failures. Acquisition remains separate from offline cleaning/building.
- **Reference compatibility**: compare release 049 IDs, axes, metadata, retention, matches, models, and source deltas.
- **Official build**: freeze input/code/lookup hashes, output, and `prune = list()`: the identity-local exact/hash and strict `>0.99` source-cohort rules replace global class-based `prune_lib()` for this scale. Keep `cross_class = FALSE`, flat `logistic_regression`, seed/holdout, `reuse = FALSE`, signatures, checkpoints, and a postbuild 049 audit that never co-loads multi-gigabyte libraries.
- **Frozen validation**: pin inputs, truth, artifacts, and controls; run after probes, never tune on holdouts, and invalidate evidence when a pin changes.
- **Performance/observability**: bound missing-value scanning and current/legacy source hashes to 20,000 columns, source-merge/recipe blocks to 5,000, and metadata lookups to 100,000 rows. Before target allocation, quarantine source-local hash/strict `>0.99` removals, stage retained blocks only, apply global exclusions/hashes, drop approved/blank metadata, and losslessly compact repeated non-key fields. Release path-list sources before replacement matrices, collect merge temporaries every 10 chunks plus each source boundary, update lookup/hierarchy columns in place, count retention without full metadata copies, and serialize recipe metadata by swapping only the S/N column. Record prefilter/compaction/hash/merge/GC/metadata-join/correlation telemetry, time/RAM/disk/checkpoints, and stop at >80% RAM, >2x projection, or 10 minutes without progress.
- **Large artifacts**: switch above 250,000 spectra to HDF5-backed recipe completion, quality/type partitioning, and source-class medoid selection; promote ordinary recipe RDS files one at a time. Store every removal in hashed OpenSpecy quarantine shards with a balanced v3 index, keep the release index lightweight, and preserve ordinary in-memory behavior for smaller/composable builds.
- **Apps/diagrams/hosted**: bundled Shiny and `.specify/memory/pipeline-diagram.html` are unchanged; update `.specify/memory/build-lib-diagram.html` with intake/admission, exact and `>0.99` source-local deduplication, bounded correlation, and quarantine flow. Changes to `R/` trigger `-HostedAppStatic`; exact-artifact preflight or clean wasm rebuild is deferred unless runtime libraries, routes, dependencies, or pins are promoted.

## Package Surfaces

- `rawrefdata/updatedv1/` (ignored): intake manifest, researched and pointer-backed legacy folders, downloads/source manifests, one cleaner per dataset, and `*/Processed/CleanedOS_raw.rds`; confirm with `git check-ignore` and never force-add.
- `workflows/OpenSpecy_reference_library.R`: manifest-selected cleanup/output validation, exact local input list, hashes, source failures, staged output, and explicit existing build policy.
- `R/build_lib*.R`, its tests, and `benchmarks/{source_record_staging,source_prefilter_staging,metadata_join_staging,recipe_completion_staging,reference_release_postbuild_audit}.R`: bounded prefiltering/merging/metadata enrichment/completion, quarantine regressions, and release audit.
- `workflows/data/classes_reference.csv`, `workflows/data/classes_regex.csv`: exact mappings and narrowly scoped regex additions with coverage/clash evidence; `.specify/memory/build-lib-diagram.html`: synchronized workflow map.
- `DESCRIPTION`, `NEWS.md`, roxygen/generated docs: update only when implementation changes require them; app/site/deployment sources remain unchanged.

## Work Checklist

- [x] Verify all researched releases and inventory every legacy transfer source; create ignored dataset folders outside active `FTIR/`/`Raman/`, record source paths/checksums, and distinguish declared licenses from the maintainer use assumption.
- [x] Write one portable `clean_<slug>.R` per dataset, repairing reusable legacy artifacts or re-reading raw spectra as required; enforce the type, units, plot, and `check_OpenSpecy()` pre-save gate, then generate each exact `Processed/CleanedOS_raw.rds` plus validation and disposition reports.
- [x] After each validated `CleanedOS_raw.rds` is saved, curate class mappings and resolve duplicates, overlaps, provenance, map/replicate independence, and measured-versus-computed admission.
- [x] Add manifest-selected `rawrefdata/` code/output execution and hashing to the workflow; update the build diagram and deterministic driver tests/static assertions.
- [x] Extend `build_lib()` and audits so all post-ingestion removals enter quarantine with reasons; add balance, source-failure, empty/nonempty, and checkpoint/signature tests.
- [x] Refresh measured/computed probes after the bounded-hash change and freeze the validated intake/build-policy snapshot, including the 20,000-column source-hash cap.
- [ ] Run the maintainer-authorized 68-source production build plus release-049 and frozen-control assessments.
- [x] Stage recipe materialization and full quarantine out of core; prove same-output blocks, forced large-path measured/computed probes, and v3 payload/ledger/hash balance.
- [ ] Run documentation, focused tests, full tests/check, and `-HostedAppStatic` on the final candidate; reconcile evidence, processes, status, and scratch files.
- [x] Implement and benchmark strict `>0.99` source-local correlation deduplication plus bounded correlation kernels; regenerate its documentation.
- [x] Rerun eligible source-owned legacy cleaners, prove byte-identical approved objects, inspect diagnostics, and audit class coverage. Bound source hashing and path-backed merging, with output-equivalence tests and repeated benchmarks.
- [x] Bound post-merge metadata lookup/hierarchy/retention work and recipe metadata serialization after the v22 threshold stop; preserve public join semantics and record chunk/copy telemetry.

## Verification

- Per dataset: verify type, intensity/transmission semantics, full-range plot, warning-free `check_OpenSpecy()`, serialized RDS, counts, and duplicate/derivative/map policy.
- Curation: exact/regex/unmatched coverage, normalized extensions, zero clashes/duplicate keys, and source-level review.
- Workflow/build: parse/one-call assertions; deterministic hashes; stale cleaner-folder-output triples fail before building; staging never replaces release. Tests prove output-identical chunked current/legacy hashes across NA envelopes, identity-local strict-threshold removal, cross-cohort retention, bounded blocks, and no full matrix.
- Spectrum disposition: source successes/failures are explicit; `ingested = retained + removed` at each artifact boundary with reassignments tracked; quarantine contains every removed spectrum and why, and an empty quarantine is valid only when removal count is zero.
- Gates: compact focused `build_lib`/reader tests, `devtools::document()` when roxygen changes, one final `devtools::test()` and `devtools::check()`, fast `-HostedAppStatic`; no browser/exact-wasm/full-wasm gate unless a hosted artifact or runtime contract changes.
- Long workflow: record source/kernel dimensions, expected and actual time/RAM/disk, progress/checkpoints/stop decisions, release manifest/signature, postbuild audit, 049 comparison, and frozen positive-control outputs without tuning.

## Risks And Open Questions

- Preserve unresolved source terms separately from maintainer assumptions; never represent public TranSpec QM14S IR as private QMe14S IR/Raman.
- Very large molecular computations remain a distinct computed collection unless the maintainer explicitly changes that release policy.

## Approval Notes

- Intake status 2026-10-20: 19 researched candidates admitted (1,970,813 retained; 169,134 quarantined; 2,681 failures). Legacy: 49 admitted/67,661 spectra, 2 review-only/24, NIR rejected, 16,540 quarantined, one source failure. Class audit has zero clashes/unmatched admissions; 483 spectra/265 opaque identities are excluded. C2DB/NASA remain request-only.
- On 2026-10-20 the maintainer authorized source-owned legacy cleaners, all reruns/approval gates, then a production build from admitted `updatedv1` inputs. They additionally authorized strict `>0.99` within-library correlation dedupe after hashes and bounded correlation computation.
- Refreshed forced-large-path probes passed with balanced v3 quarantine: measured 903 input/41 removed/14 source-correlation removals; computed 870/177/72. Probe 049 comparison remains disabled while production retains it.
- Six attempts were stopped safely before promotion: unbounded hashing, retained-source merging, a whole-source reread during staged merging, a retained-all-columns merge that crossed 80% at source 64/68, a prefiltered source-59 merge transient at 80.4%, and v22 after its full merge succeeded but lookup 1/2 copied metadata and reached 82.75%. V22 proved 2,009,712 retained spectra could merge with source 59 at 72.7% and source 64 peaking at 75.8%; v23 replaces full-table lookup/hierarchy/retention copies with bounded in-place column updates. Refreshed measured/computed probes must pass before the fresh v23 production target.
