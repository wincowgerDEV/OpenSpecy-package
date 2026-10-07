# Feature Plan: Positive-Control Identification Validation

**Feature dir**: `specs/043-positive-control-library-validation`  
**Date**: 2026-10-06  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Reproduce the published Open Specy 1.0 positive-control recovery with `automate_particle_analysis()`, then freeze the compatible in-memory workflow while comparing three derivative FTIR libraries and recommending the next library-development path.  
**Change class**: Package/scientific; external validation plus permitted Stage 1 particle-analysis corrections and performance work.  
**Stage order**: Stage 2 is blocked until Stage 1 accuracy/recovery means and RSDs reproduce the saved OS1 baseline within 1.0 percentage point and every standard map completes in memory in under 5 minutes.
**Study reference**: `C:\Users\winco\Downloads\1527b98b-b7d1-4878-8e9b-88511153c6bc (3).pdf`; Cowger et al., *Open Specy 1.0: Automated (Hyper)spectroscopy for Microplastics*, DOI `10.1021/acs.analchem.5c00962`.

## Goal And Scope

- Reproduce identification, count, projected-area, and Feret-length recovery on the 20 eligible holdout maps, then compare the published, pre-0.9-closure, and current libraries with no non-library setting changes.
- **In**: read-only source maps/OS1 evidence/study, one maintained benchmark script, allowed Stage 1 fixes to `automate_particle_analysis()`, three complete result sets in a new `Positive_Controls` folder, paired diagnostics, and a scientific report.
- **Out**: PMMA and Red PET Fiber controls; tuning thresholds/classes on this holdout; rebuilding, replacing, publishing, or uploading a library; medoid/model routes; app changes; remote synchronization.
- **Users**: maintainers deciding whether cross-class correlation closure, taxonomy curation, performance work, or expanded reference coverage should lead the next library release.

## Requirements

- R1. Freeze the 22-map inventory, exclude `PMMA_15Nov223_control` and `RedPETFibers_15Nov223_control` before every probe, run, aggregation, plot, and conclusion, and require the remaining 20 IDs to join one-to-one with `OS1_Results/true_values2.csv`.
- R2. Recreate the study scorer: count recovery, median-area recovery, median-Feret recovery, Specific ID accuracy, and Plastic/Not ID accuracy, all as percentages; RSD is `sd/mean * 100` across finite sample recoveries. Preserve the study context of 22 images/2,880 particles and accredited 50--150% recovery with CV below 40%, while clearly labelling the user-directed 20-map analysis as a new cohort.
- R3. Stage 1 compares the OS1 library run with `OS1_Results/base/particle_details_all.csv` per sample, material/type, truth-only size stratum, and overall. Every finite mean and RSD delta must be <=1.0 percentage point; absent size truth must yield matching `NA` metrics.
- R4. Stage 1 may update `automate_particle_analysis()` or shared helpers to restore scientific compatibility or the runtime contract. Any behavior fix must have a focused regression; any same-output performance change must keep the prior implementation/comparison in `benchmarks/` and pass output-equivalence plus the 10% regression guard.
- R5. On a Stage 1 discrepancy, stop at the first divergence across cohort/order, ENVI read and smoothing, S/N mask, feature IDs and historical `area > 1` semantics, median collapse, processing axis/range, match identity/score, geometry, and scoring. Use `automated_steel_pipeline_Validation.R`, the study, and saved outputs as historical evidence; do not start Stage 2 until the full gate passes.
- R6. Run the standard path as dense in-memory `OpenSpecy` analysis (`file_processing="memory"`), not a compact/file-backed substitute. After one library load, every map must finish end-to-end from read start through requested result writes in <5 minutes; record cold library load separately, plus map dimensions, elapsed time, peak memory, and warnings.
- R7. Stage 2 changes only the selected FTIR library. Freeze source-file hashes, package/source hash, cohort/order, arguments, scorer/crosswalk, size strata, output set, and runtime mode. If `automate_particle_analysis()` or any shared analysis helper changes during Stage 2, discard affected Stage 2 evidence, return to the complete Stage 1 OS1 gate, then rerun both new libraries from the newly frozen candidate.
- R8. Report paired deltas and sample-level bootstrap uncertainty for all five metrics overall and by expected material/type and size; include confusion shifts, match score/margin distributions, class coverage, removed-reference attribution, runtime/memory, warnings, and the fixed 0.66 match-threshold sensitivity without optimizing it. Recommendations must require independent development data and a new/sequestered confirmation set before changing library policy.

## Technical Decisions

- **Frozen libraries**: OS1 `OS1_Results/derivative.rds` (SHA-256 `A1C0E073...22FA1`, filter combined object to FTIR); pre-cross-class-closure `reference-library-assessment-rerun-20260930/releases/e2d1941530ef/derivative.rds` (`D57DCD47...C71ED`, FTIR 600 x 41,005); current closed `reference-library-build-2.0.0/releases/bb8cd82c7ecb/derivative.rds` (`DEED05DA...D292`, FTIR 600 x 39,281). Record full hashes/manifests in results.
- **Output contract**: create `C:\Users\winco\OneDrive\Documents\Positive_Controls\library_validation_3_libraries\` with `01_os1_published/`, `02_pre_0.9_closure/`, `03_current_closed/`, and `comparison/`. Each library folder contains per-map details/summary/processed/time outputs plus warnings and a configuration manifest; `comparison/` contains joined CSVs, plots, a run manifest, and the rendered report inputs. Never overwrite `OS1_Results/base`, existing `Positive_Controls/output`, or `corcontraintoutput`.
- **Saved 20-map target**: current read-only recomputation gives mean/RSD (%) of count `91.003/41.538`, area `109.716/53.575`, Feret `97.659/35.940`, Specific ID `94.508/9.097`, and Plastic/Not ID `96.409/5.999`; the benchmark must independently reproduce these before treating them as fixtures.
- **Initial settings**: median particle collapse; spectral smoothing with `sigma1=c(1,1,1)`; no closing/baseline subtraction; S/N `[0.01, Inf]`; correlation `0.66`; no unknown relabel/removal; 25 micrometre pixels; signed `sig_times_noise`; conform to the active library axis with `res=NULL`; restrict to 800--2200 and 2420--3200 cm^-1; first-derivative Savitzky-Golay processing and relative normalization. Resolve the study's 90-wavenumber derivative description and legacy strict area rule against saved outputs before freezing settings.
- **Fair scoring**: retain raw labels and apply one versioned, directionally explicit taxonomy crosswalk before the unchanged `true_values2.csv` regex scorer; every truth pattern must be evaluable for every library. A cached-query comparison may diagnose reference-content versus axis effects but cannot replace full primary runs.
- **OpenSpecy/API/dependencies**: preserve dense `wavenumber`, spectra/metadata alignment, IDs, attributes, and library bytes. The isolated Stage 1 defect requires `sn_range=list(min,max)` in cm^-1: retain the current 750--2200/2420--4000 default, pass the published 800--2200/2420--3200 windows explicitly, validate one-to-one finite bounds, and use it identically in dense and file-backed paths without adding a dependency.
- **Performance**: probe small `Recovery_red_beads` (196 x 202 x 427), mid `RedBrick` (451 x 445 x 432), and large `ClearSiliconeTubing` (466 x 439 x 427), each <5 minutes. Expect <100 minutes per 20-map library and <16 GiB peak/map; checkpoint after every map. Stop on any >=5-minute map, >80% physical memory, error, or 10 minutes without progress, isolate/benchmark, and restart only invalidated work.
- **Generated/app/hosted**: regenerate and inspect roxygen output for `sn_range`, update `NEWS.md`, and run fast `-HostedAppStatic`. The argument changes the S/N spectral window but not pipeline stage topology, so the Shiny app and canonical pipeline diagram remain unchanged; matching-artifact and clean-wasm tiers remain N/A.

## Package Surfaces

- `benchmarks/positive_control_library_validation.R`: the complete restartable analysis/scoring/report-input driver, including study criteria, hashes, Stage 1 gate, Stage 2 invalidation, per-map runtime assertions, and old/current equivalence checks. No analysis script is maintained in `Positive_Controls`.
- `C:\Users\winco\OneDrive\Documents\Positive_Controls\library_validation_3_libraries\`: authorized external output root for all three result sets and comparisons; source maps remain read-only.
- `specs/043-positive-control-library-validation/report.md`: methods, results, study comparison, limitations, and prioritized recommendation. `R/automate_particle_analysis.R`, focused tests, roxygen/`NEWS.md`, benchmarks, and the pipeline diagram change only when Stage 1 evidence requires them.
- `workflows/`, `DESCRIPTION`, `.github/workflows/`, `inst/`, site/vignettes/README/pkgdown, generated docs, and reference-library artifacts: unchanged unless an approved Stage 1 fix directly triggers a listed surface.

## Work Checklist

- [x] Implement `benchmarks/positive_control_library_validation.R`; create the new external output root/manifests and independently reproduce the saved scorer after both exclusions.
- [x] Run the three-map in-memory OS1 probe; resolve correctness and <5-minute failures with focused Stage 1 fixes/tests/benchmarks before the checkpointed 20-map OS1 gate.
- [x] Freeze the passing source/configuration hashes and run the same 20 maps into the two new-library folders, enforcing rollback to Stage 1 after any analysis-code change.
- [x] Trace paired gains/losses through raw/canonical labels, matches/scores, reference IDs/classes, axes, and `bb8cd82c7ecb` quarantine/removal evidence; quantify material/size effects and study-criteria status.
- [x] Write `report.md`, populate `comparison/`, and run only gates triggered by actual package changes.
- [x] Reconcile every checkbox with evidence; record deferred gates, stop/record owned processes, inspect `git status`, and remove temporary PDF/scratch artifacts while retaining the authorized result directory.

## Verification And Risks

- Focused acceptance: input/hash/cohort assertions; scorer fixture; one-file versus vector equivalence; dense `OpenSpecy` invariants; deterministic joins/crosswalk; Stage 1 means/RSDs within 1.0 percentage point; <5 minutes for every map; identical non-library configuration hashes across Stage 2.
- Scientific acceptance: paired sample bootstrap intervals without particle pseudoreplication; confusion/coverage denominators; axis/metadata/ID compatibility; warning reconciliation; explicit separation of detection/size recovery from identification accuracy; comparison to the study's recovery/CV claims without implying the reduced cohort is the published cohort.
- Gates: always run the benchmark/evidence audit. If package code changes, run focused particle/matching tests and equivalence/performance benchmarks before full `devtools::test()` and `-HostedAppStatic`; run roxygen2 8.0.0 documentation/diff review only when triggered. `devtools::check()` remains deferred because this is not release-facing.
- Reusable evidence: a stage remains reusable only while source, dependencies, input/library bytes, scorer/crosswalk, and configuration hashes match. Any Stage 2 analysis-code edit explicitly invalidates the Stage 1 certificate and downstream comparisons.
- Risks: library axes/taxonomies can manufacture naive scoring shifts; saved outputs may encode historical defaults; closure may reduce rare-class coverage while resolving ambiguity; in-memory copies may challenge RAM; the small heterogeneous cohort supports paired diagnosis but not holdout-driven tuning.

## Approval Notes

- Approved by: maintainer direction on 2026-10-06 for benchmark placement, external three-library outputs, Stage 1 function updates, Stage 2 rollback, and <5-minute in-memory maps.
- Follow-up: implementation requires `$speckit-implement`; publishing or replacing reference artifacts remains maintainer-owned.
- Completion evidence: 60 map runs, zero warnings, 89.6-second maximum; Stage 1 deltas are zero overall, per sample, by material type, and by size; focused tests, full `devtools::test()`, roxygen generation/diff review, benchmark audit, and `-HostedAppStatic` passed. R CMD check, action-artifact preflight, and wasm rebuild were not triggered.
