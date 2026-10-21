---
name: openspecy-rebuild-reference-library
description: Stage, run, monitor, audit, and explain official OpenSpecy reference-library rebuilds and frozen positive-control comparisons. Use for build_lib production runs, checkpoint resume/restart decisions, pruning or cross-class policy changes, model-method selection, release assessment, complete quarantine/removal provenance, spectrum disposition accounting, or official artifact comparisons.
---

# Rebuild The OpenSpecy Reference Library

Use maintained package workflows and audit scripts. Keep large inputs, releases,
logs, and positive-control outputs outside the package repository unless the
active plan explicitly says otherwise.

## Freeze The Run Contract

Before code changes or production launch, record:

- exact source inventory and hashes, curated lookup hashes, prior artifact, and
  new staged output directory;
- per-artifact pruning and generic reassignment policy;
- source-local near-duplicate enablement, cohort keys, threshold/operator,
  deterministic survivor order, and position relative to exact content hashes;
- whether cross-class pruning/closure is enabled, its numeric threshold, and
  the strict/inclusive comparison implemented by the owning code;
- model method, hierarchy/schema, selection policy, seed, and holdout;
- build signature inputs, clean/reuse choice, compatible checkpoints, worker
  count, source-hash block size and per-source telemetry, correlation allocation
  mode/block size and largest source cohort, path-backed merge enablement and
  block size plus per-source merge telemetry, quarantine-first source
  prefilter enablement, retained-only block policy, metadata drop/compaction
  policy and public expansion check, early path-reference release and explicit
  merge garbage-collection cadence, metadata-join block size and no-copy
  production policy for lookups/hierarchy/retention, recipe metadata-column
  swap policy, file-backed recipe trigger and
  block size, quarantine schema/shard root, and whether global class-based
  `prune_lib()` is enabled independently of source-local deduplication,
  representative kernel dimensions, runtime/memory budget, progress
  boundaries, and stop/restart thresholds.

Resolve wording such as "the 0.9 rule" against the implementation and restate
the exact operator. Do not infer policy from artifact names, empty quarantine,
defaults, or an earlier run.

## Staged Workflow

1. Read the active plan, `workflows/OpenSpecy_reference_library.R`, relevant
   `build_lib()` source/tests, and `.specify/memory/build-lib-diagram.html`.
   Apply `openspecy-ingest-reference-datasets` for raw local inputs and require
   its exact manifest-selected `CleanedOS_raw.rds` list, validation report,
   source failures, hashes, and maintainer review before freezing the run.
   Apply `openspecy-curate-class-reference` when lookup/taxonomy data change.
2. Inventory the newest authorized local inputs without remote synchronization.
   Parse the driver and confirm exactly one production `build_lib()` call with
   the frozen contract.
   Do not launch while an admitted dataset or class mapping is still being
   added/reviewed, or while the maintainer has deferred the full rebuild.
3. If package code changed, run the focused build-library tests, configured
   documentation generation when triggered, one final full package test, and
   `-HostedAppStatic`. Reuse still-valid evidence instead of repeating it.
4. Probe one representative/new source and the highest-risk kernel. Validate
   `OpenSpecy` axes, spectra/metadata alignment, attributes, warnings,
   checkpoint signatures, projected time, and memory before production.
   Inventory source-local dedupe cohort cardinalities; reject broad-class
   pooling of distinct identities. For production-scale correlations, prove the
   selected bounded mode does not allocate a full square matrix and records the
   largest comparison block unless a measured dense exception is frozen.
   For large cleaned objects, separate compressed read, metadata adoption,
   missing-value envelope discovery, current/legacy hashing, axis merge, and
   recipe materialization. Source hashing is bounded only when every
   column-scaled intermediate is bounded. Prove current and legacy IDs match the
   frozen path for complete, gapped, internal-NA, and no-common-range inputs.
   When retaining source records exceeds the budget, apply exact hashes and
   frozen identity-local correlation within each live source, persist every
   removal to hashed quarantine immediately, and stage only retained normalized
   metadata and bounded spectral blocks. Before allocating the target, capture
   global exclusions and cross-source hashes from the descriptors, drop fields
   already designated for removal and globally empty fields, and losslessly
   compact repeated non-key character metadata. Verify unchanged source files,
   release the original path-list reference before an exact-filter replacement,
   then merge only staged retained blocks into one preallocated target; do not
   reread a whole large source during assembly. If empirical heap retention can
    breach the budget, collect released temporaries at a frozen chunk cadence
    and every source boundary, and report that cadence in merge telemetry.
    Once the merged target is resident, update lookup output columns in bounded
    row blocks without copying the whole metadata table. Apply hierarchy updates
    by reference, compute retention counts from the minimum projected columns,
    and serialize recipe metadata by temporarily swapping only the recipe-specific
    column and restoring it. Record rows, chunks, largest block, and whether a
    metadata copy occurred; the production path must report no full copy.
    Expand staging-only metadata
   representations before any public object is returned. Prove same- and
   variable-axis results equal the retained-record path, exercise both ordinary
   and forced file-backed completion, and record prefilter, compaction, and
   merge telemetry.
   If the merged target fits but simultaneous raw/derivative/no-baseline or
   quarantine payloads do not, retain HDF5-backed recipes through class,
   quality, and type-partition stages. Process one recipe/block at a time,
   materialize medoid source-class groups independently, and promote ordinary
   release RDS artifacts one recipe at a time. The official large molecular
   run uses source-local exact/hash plus strict identity-cohort correlation;
   do not invoke global `prune_lib()` merely because `cross_class = FALSE`.
5. Launch once into a new staged output. Record the exact command, parent and R
   child PIDs, PID/log paths, working directory, signature, and free disk/RAM.
   If launch output is ambiguous, inspect processes and logs before relaunching;
   never create a duplicate build speculatively.
   Treat all consumed code/input hashes as frozen after launch. Parallel work
   may touch only non-consumed surfaces; recheck launch hashes before promotion.
6. Monitor stage boundaries using log advancement, CPU, memory, checkpoint
   creation, and elapsed time. Report meaningful state changes and threshold
   reviews. Stop at a safe checkpoint when a declared limit is crossed; isolate
   and verify the replacement before restart.
   A transient measured threshold crossing still invalidates the candidate even
   when the next garbage collection recovers memory. Preserve an over-budget target for diagnosis, but restart only into a fresh
   target after the replacement benchmark, focused gates, probe, and contract
   hashes pass; never treat a partial target as a compatible checkpoint by name.
   Treat a post-merge lookup or hierarchy threshold crossing exactly like a
   merge-stage crossing: successful source assembly does not make an enrichment
   allocation safe.
7. Run the standard saved-release assessment and
   `benchmarks/reference_release_postbuild_audit.R` where its contract matches
   the candidate. Compare IDs, axes, metadata, warnings, object validity,
   manifests/checksums, model schema, quarantine completeness, and representative
   joins or matches. Stop release promotion when a removed ingested spectrum is
   absent from quarantine or lacks removal provenance.

## Reconcile Spectrum Dispositions

Use saved assessment/provenance tables to balance defined stage inputs across:

- retained unchanged;
- retained after reviewed reassignment, with old/new class and reason;
- removed by ordinary cleanup, range, duplicate, quality, or threshold rules;
- removed by cross-class conflict/closure rules;
- removed by whole-source or downstream policy.

Preserve every successfully ingested spectrum in the removed groups inside the
quarantine artifact. Its ledger must retain stable ID, source, removal stage,
rule, and human-readable reason; include the cutoff/operator, comparison ID, and
observed value for threshold or similarity decisions when available. Record
unreadable source records as source failures rather than inventing a quarantined
spectrum. State each balance boundary, avoid double-counting spectra removed
once, and require quarantine counts to equal the removal ledger. Empty
quarantine is valid only when no ingested spectrum was removed.
When removal payloads are too large to bind, write bounded valid-`OpenSpecy`
RDS shards immediately. Freeze every shard's count, bytes, and SHA-256 in a v3
index; verify hashes before and after release copying, and derive review CSVs
from the balanced ledger without loading all payload spectra.

## Frozen Positive Controls

Pin input maps, truth, eligibility/exclusions, artifacts, code, and every
output-determining control. Run the predeclared representative-map probe, then
restartable per-map checkpoints within the declared resource limits. If any pin
changes, invalidate downstream results and return through compatibility and the
probe before the full cohort. Do not tune a candidate using outcomes from the
same frozen holdout. Report paired accuracy/RSD or other planned metrics with
uncertainty, warnings, runtime/memory, and decision-relevant confusion.

## Handoff

State the signature/output, sources, source-local dedupe cohort and exact
threshold/operator/survivor policy, effective pruning and cross-class policy,
source-hash block/peak telemetry, correlation allocation mode/peak block, model
method, path-backed merge block/telemetry, file-backed recipe trigger/block,
source-prefilter and metadata-compaction telemetry, global prune enablement,
merge garbage-collection cadence, checkpoint reuse, timings/resources, warnings,
disposition balance, quarantine schema/shard counts and hashes, compatibility
results, and positive-control scope. Confirm that quarantine contains every
removed ingested spectrum and summarize its reasons by stage. Say whether
publication/staging remains maintainer-owned.
Confirm owned processes stopped, `git status` was inspected, and task scratch
was removed or retained with an explicit cleanup condition.
