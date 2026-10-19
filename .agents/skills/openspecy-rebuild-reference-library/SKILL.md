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
- whether cross-class pruning/closure is enabled, its numeric threshold, and
  the strict/inclusive comparison implemented by the owning code;
- model method, hierarchy/schema, selection policy, seed, and holdout;
- build signature inputs, clean/reuse choice, compatible checkpoints, worker
  count, representative kernel dimensions, runtime/memory budget, progress
  boundaries, and stop/restart thresholds.

Resolve wording such as "the 0.9 rule" against the implementation and restate
the exact operator. Do not infer policy from artifact names, empty quarantine,
defaults, or an earlier run.

## Staged Workflow

1. Read the active plan, `workflows/OpenSpecy_reference_library.R`, relevant
   `build_lib()` source/tests, and `.specify/memory/build-lib-diagram.html`.
   Apply `openspecy-curate-class-reference` when lookup/taxonomy data change.
2. Inventory the newest authorized local inputs without remote synchronization.
   Parse the driver and confirm exactly one production `build_lib()` call with
   the frozen contract.
3. If package code changed, run the focused build-library tests, configured
   documentation generation when triggered, one final full package test, and
   `-HostedAppStatic`. Reuse still-valid evidence instead of repeating it.
4. Probe one representative/new source and the highest-risk kernel. Validate
   `OpenSpecy` axes, spectra/metadata alignment, attributes, warnings,
   checkpoint signatures, projected time, and memory before production.
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

## Frozen Positive Controls

Pin input maps, truth, eligibility/exclusions, artifacts, code, and every
output-determining control. Run the predeclared representative-map probe, then
restartable per-map checkpoints within the declared resource limits. If any pin
changes, invalidate downstream results and return through compatibility and the
probe before the full cohort. Do not tune a candidate using outcomes from the
same frozen holdout. Report paired accuracy/RSD or other planned metrics with
uncertainty, warnings, runtime/memory, and decision-relevant confusion.

## Handoff

State the signature/output, sources, effective pruning and cross-class policy,
exact threshold/operator, model method, checkpoint reuse, timings/resources,
warnings, disposition balance, quarantine meaning, compatibility results, and
positive-control scope. Confirm that quarantine contains every removed ingested
spectrum and summarize its reasons by stage. Say whether publication/staging
remains maintainer-owned.
Confirm owned processes stopped, `git status` was inspected, and task scratch
was removed or retained with an explicit cleanup condition.
