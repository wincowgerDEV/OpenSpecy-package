# Feature Plan: Model Diagnostics, Metadata Order, and Homepage Video

**Feature dir**: `specs/024-assessment-diagnostics-homepage`
**Date**: 2026-09-03
**Status**: Complete
**Current tranche**: Add interpretable model-error associations and human-first metadata ordering to the completed reference build, plus update the hosted landing acknowledgement and hero media.
**Change class**: mixed (package/scientific output plus hosted presentation)

## Goal

- Make model identification failures easier to diagnose from `assessments` and make library metadata easier to review.
- Present the requested Pew-Gerstner acknowledgement verbatim and autoplay the supplied YouTube video in the hosted homepage hero.

## Scope

- **In**: derived `assess_spec()` metric/error correlations; stable typed schema and rankings; final metadata NA ordering/removal for full and medoid libraries; reuse of completed libraries/models; `site/` hero video and acknowledgement; tests, NEWS, builder diagram, hosted checks.
- **Out**: retraining models, changing spectra/wavenumbers/IDs, altering Shiny analysis reactives, editing README/pkgdown, publishing, pushing, or changing the existing tutorial section.
- **Users**: maintainers inspect the new assessment table and easier metadata order; homepage visitors see the new video and exact funding statement.

## Requirements

- R1. `assessments$model_assessment_correlations` reports, for every model/source/technique/check metric with sufficient variation, the point-biserial Pearson correlation between numeric assessment value and incorrect identification, sample counts, correct/incorrect means, absolute correlation, and within-output rank; preserve a typed empty schema and put the strongest association first.
- R2. Assess each held-out spectrum once per recipe/source/actual spectrum type and reuse those metrics across corresponding type-specific and combined model outputs; do not train or modify models.
- R3. After full and medoid libraries are complete, drop metadata columns whose every value is `NA`, then stably order remaining columns by increasing `NA` count. Preserve spectra, axes, row alignment, IDs, class, and attributes, and record a typed metadata-finalization audit.
- R4. Reuse the completed `reference-library-build-taxonomy-20260902` libraries, medoids, models, and compatible checkpoints; invalidate only assessment/release output and promote a new versioned release.
- R5. Replace the hero sample-spectrum SVG with an eager privacy-enhanced embed of YouTube video `8zrlQeTCwkQ`; request autoplay with muted inline playback to satisfy modern browser policy, retain accessible title/fullscreen behavior, and keep responsive 16:9 layout.
- R6. Replace the Pew-Gerstner list entry with exactly: “Support for this project was provided by the Pew-Gerstner Fellows Program in Marine Conservation at The Pew Charitable Trusts. The views expressed herein are those of the author(s) and do not necessarily reflect the views of The Pew Charitable Trusts.”

## Technical Decisions

- **Public API**: no new argument or export. Both behaviors are deterministic finalization/assessment output owned by `build_lib()`.
- **OpenSpecy contract**: copy only `metadata`; drop/reorder columns without changing rows. Stable ties retain their prior column order, and all resulting objects must pass `check_OpenSpecy()`.
- **Association**: correlate `assess_spec(report = "all")$value` with binary `!correct`; require at least three complete observations plus variation in both variables. Rank by absolute correlation within artifact/model/source/technique/provenance; retain `NA` correlations for unevaluable metrics.
- **Performance**: vectorized metadata counts; model diagnostics run only on existing 10% holdouts and cache spectrum assessments by actual type. Expect assessment-only reuse under 60 minutes; investigate/stop at 90 minutes or a silent stage over 15 minutes.
- **Generated artifacts**: update roxygen/NEWS and regenerate help with configured roxygen2 only if the return documentation changes; update `.specify/memory/build-lib-diagram.html`. The Shiny pipeline diagram is N/A because no app analysis flow changes.
- **Hosted**: `site/index.html` and `site/assets/site.css` change. Run focused landing assertions and `-HostedAppStatic`; use a local/matching assembled artifact for the landing browser screenshot if available. No wasm dependency, pin, library staging, or app runtime change, so no clean wasm rebuild.

## Package Surfaces And Work Checklist

- [x] `R/build_lib.R`: add metadata finalization and ranked model-assessment association helpers; integrate without model retraining.
- [x] `tests/testthat/test-build_lib.R`: cover typed schemas, association math/ranking, NA drop/order, and `OpenSpecy` preservation.
- [x] `site/index.html`, `site/assets/site.css`, hosted tests: add the hero autoplay embed and exact acknowledgement; remove orphaned sample-spectrum presentation CSS.
- [x] `NEWS.md`, roxygen help if affected, and `.specify/memory/build-lib-diagram.html`: document both new assessment outputs and metadata finalization.
- [x] Run focused tests, app/static parsing, hosted static, full tests, and relevant metadata microbenchmark; inspect the homepage at desktop/mobile when a usable local assembled site is available.
- [x] Reuse the completed production checkpoints, run assessment-only finalization, reload every release artifact, and report the strongest model-error associations plus metadata-order invariants.
- [x] Reconcile every checkbox with evidence; stop owned processes, inspect status, and remove task scratch.

## Verification And Risks

- Direct: synthetic known-sign correlations; constant/NA metric handling; stable tie ordering; exact normalized acknowledgement text; eager iframe URL includes `autoplay=1`, `mute=1`, and `playsinline=1`.
- Final: configured R 4.3.3 focused/full tests and hosted static gate. R CMD check is deferred because this is not a CRAN/release tranche and the prior check remains valid outside changed files.
- Browser: external YouTube playback can be blocked by network or user browser policy; muted autoplay is the standards-compatible request and tests verify the embed contract rather than audio playback.
- Reuse: bump assessment/release version only. Do not bump artifact/core/model signatures; verify output spectral matrices are identical to release `17b419a919a3`.

## Completion Evidence

- Focused builder/hosted tests, `-HostedAppStatic`, and the full test suite pass; the metadata finalizer benchmarked at a 0.080-second median for 60,000 rows and 61 columns.
- Desktop/mobile browser inspection confirmed responsive layout and muted autoplay; the player visibly entered its playing state.
- Release `b7b75b699a47` reused libraries/models, completed all 15 holdout comparisons, and passed reload checks for object validity, unchanged axes/spectra/model scientific fields, metadata values/order, typed correlations, and audit counts.

## Approval Notes

- Approved by user request, 2026-09-03. Remote push/publish remains maintainer-owned.
