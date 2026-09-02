# `build_lib()` Assessment Report — 2026-09-01

## Run status

The full production build completed successfully. `build_lib()` reported
`complete` after 20,196.2 seconds (5 h 36 min 36 s), and the process exited
normally after serialization. Release ID: `02c9cd94a3d6`.

- Output root: `C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/reference-library-build-spectrum-type-20260901-124121`
- Aggregate: `releases/02c9cd94a3d6/reference_library_build.rds`
- Build log: `build.stderr.log`
- Aggregate size: 402,977,206 bytes (about 384 MiB)
- Manifest: 118 components built and one release promoted
- Reload validation: all eight release files reload; every component file is
  exactly identical to the corresponding object inside the aggregate.

## Executive findings

1. **The type-specific reference libraries are the strongest result.** All 15
   old/new reference-library comparisons used exactly the same test counts.
   Coverage was 100% except both old and new nobaseline Raman medoids, which
   evaluated 72 of 73 spectra (98.63%). Most primary macro class-accuracy
   results improved or held steady.
2. **The derivative Raman improvements are substantial.** Full-library macro
   accuracy rose 5.80 percentage points and derivative Raman medoid accuracy
   rose 6.67 points. Raw FTIR and Raman also gained about 3.3 points each.
3. **The full nobaseline libraries are highly accurate.** FTIR reached 99.90%
   macro accuracy and Raman reached 89.47%; overall accuracy was 99.47% and
   99.90%, respectively. NIR was 100%, but its 44-spectrum test is small.
4. **The new multinomial models are not publication-ready as primary
   identifiers.** Typed derivative FTIR is promising at 95.16% macro accuracy,
   but Raman models are only 34–39% macro accuracy and nobaseline FTIR is
   84.53%. The old/new model rows also use different test counts, so their
   deltas are not valid paired comparisons yet.
5. **Quality gates performed material work.** CO2 flattening corrected 27,458
   derivative FTIR and 3,958 nobaseline FTIR spectra. High-tail trimming
   corrected 3,313 spectra. Running S/N filtering removed 884 spectra.
6. **Class coverage meets the requested threshold.** Every row has a populated
   class after review. There are 554 `other` spectra (0.8137%), below the 1%
   ceiling, including 549 unresolved `other` assignments.
7. **This artifact is valid, but one more final-code build is advisable before
   publication.** The running R process loaded code before the later model
   warning-capture and symmetric self-correlation patches. Therefore the saved
   `warnings` table is empty even though the build log contains `glmnet`
   convergence warnings.

## Paired reference-library accuracy

Macro class accuracy is the primary metric. Delta is new minus old in
percentage points. Every row below used the same test-spectrum count for old
and new.

| Artifact | Test n | New macro | Old macro | Delta | New overall |
|---|---:|---:|---:|---:|---:|
| Raw Raman | 2,092 | 73.39% | 70.05% | +3.34 | 98.23% |
| Raw FTIR | 2,258 | 89.06% | 85.73% | +3.33 | 94.77% |
| Raw NIR | 45 | 100.00% | 100.00% | 0.00 | 100.00% |
| Derivative Raman | 2,051 | 93.68% | 87.88% | +5.80 | 99.80% |
| Derivative FTIR | 2,114 | 98.60% | 98.67% | -0.08 | 99.72% |
| Derivative NIR | 45 | 100.00% | 100.00% | 0.00 | 100.00% |
| Nobaseline Raman | 2,061 | 89.47% | 89.47% | 0.00 | 99.90% |
| Nobaseline FTIR | 2,068 | 99.90% | 99.85% | +0.05 | 99.47% |
| Nobaseline NIR | 44 | 100.00% | 100.00% | 0.00 | 100.00% |
| Medoid derivative Raman | 72 | 99.30% | 92.63% | +6.67 | 97.22% |
| Medoid derivative FTIR | 248 | 95.16% | 92.98% | +2.18 | 93.15% |
| Medoid derivative NIR | 10 | 100.00% | 100.00% | 0.00 | 100.00% |
| Medoid nobaseline Raman | 73 | 94.41% | 94.15% | +0.26 | 97.22% |
| Medoid nobaseline FTIR | 229 | 91.39% | 91.20% | +0.19 | 88.65% |
| Medoid nobaseline NIR | 8 | 87.50% | 87.50% | 0.00 | 87.50% |

The macro and overall metrics tell different stories where classes are
imbalanced. Raw Raman, for example, has 98.23% overall accuracy but only 73.39%
macro accuracy because the abundant mineral class is nearly perfect while
several rare classes remain weak.

Class-label sets also changed during standardization. Depending on the
artifact, only 2–29 expected classes have the same name on both sides, with
additional new-only and old-only classes. Thus equal test IDs make the spectra
paired, but some macro gains also reflect the intended class remapping rather
than improved predictions within an unchanged label set.

### Weak classes worth reviewing

Among classes with at least five test spectra, the weakest new results were:

- Raw Raman polystyrenes: 40.0% (n = 5)
- Raw Raman `other material`: 45.2% (n = 31)
- Raw Raman organic matter and `other plastic`: 66.7% each (n = 6 each)
- Raw FTIR acrylonitrile butadiene styrene: 75.0% (n = 8)
- Medoid derivative FTIR ethylene-vinyl acetate: 75.0% (n = 8), down 12.5
  percentage points from old
- Medoid derivative FTIR polystyrenes: 85.0% (n = 20), down 5 points

The small denominators make these directional findings, not stable population
estimates. Raw FTIR mineral declined only 1.09 points across 276 tests, and raw
FTIR polyolefins declined 0.16 points across 645 tests.

## Model assessment

The Shiny `all` route should prefer the type-specific FTIR, Raman, and NIR
models; the compatibility `both` models were consistently weaker.

| New typed model | Test n | Macro accuracy | Overall accuracy |
|---|---:|---:|---:|
| Derivative FTIR | 2,108 | 95.16% | 80.88% |
| Derivative Raman | 2,047 | 34.48% | 68.34% |
| Derivative NIR | 45 | 100.00% | 100.00% |
| Nobaseline FTIR | 2,055 | 84.53% | 68.13% |
| Nobaseline Raman | 2,057 | 39.17% | 62.86% |
| Nobaseline NIR | 44 | 99.07% | 97.73% |

Every potential old/new model comparison is currently unpaired: FTIR differs
by 6–13 spectra and Raman by four spectra, while no old NIR model exists. The
published old models also have unknown training membership. The reported old
model metrics are useful context but **must not be interpreted as causal
old-versus-new deltas**. Fix the cohort construction before using model deltas
for a release decision.

The build log records multiple `glmnet` messages stating that smaller lambda
solutions did not converge within `maxit = 100000`, while larger-lambda
solutions were returned. Model quality—especially Raman—should be reviewed
alongside selected lambdas and these warnings before retraining or publishing.

## Quality-control effects

| Recipe/type | Check and action | Spectra affected | Removed |
|---|---|---:|---:|
| Derivative FTIR | CO2 flatten | 27,458 | 0 |
| Nobaseline FTIR | CO2 flatten | 3,958 | 0 |
| Derivative FTIR | High-tail trim | 1,161 | 0 |
| Derivative Raman | High-tail trim | 1,382 | 0 |
| Nobaseline FTIR | High-tail trim | 342 | 0 |
| Nobaseline Raman | High-tail trim | 428 | 0 |
| Derivative FTIR | S/N < 2 drop | 75 | 75 |
| Derivative Raman | S/N < 2 drop | 173 | 173 |
| Nobaseline FTIR | S/N < 2 drop | 548 | 548 |
| Nobaseline Raman | S/N < 2 drop | 79 | 79 |
| Nobaseline NIR | S/N < 2 drop | 9 | 9 |

Pruning reassigned 8,302 derivative and 8,210 nobaseline spectra, then removed
1,583 and 1,710 spectra respectively. No failed high-tail correction appears in
the quality-control table; all detected high tails shown there were corrected.

The clearest `assess_spec` shift is derivative FTIR CO2 issues: 46.14% old to
4.38% new (-41.76 points). Its high-tail issue rate fell from 2.46% to 0.009%.
Medoid derivative FTIR CO2 issues fell 42.27 points. Conversely, raw FTIR CO2
issues rose 11.57 points and nobaseline FTIR rose 6.06 points; raw is expected
to be uncorrected, while the residual nobaseline behavior deserves review.

Missing-value findings increased because the new type-specific objects retain
broader, source-dependent support. All NIR spectra have at least one missing
value somewhere on the 4002–12000 axis; this does not mean they are unusable in
the finite 5130–6450 identification interval. FTIR missing-value issue rates
also rose about 16 points. These checks should be interpreted as support
heterogeneity, not automatically as failed spectra.

## Artifact structure and compatibility

| Artifact family | Raman range | FTIR range | NIR range |
|---|---|---|---|
| Full libraries | 204–3996 (633 points) | 402–3996 (600 points) | 4002–12000 (1,334 points) |
| Medoids/models | 804–3198 (400 points) | 804–3198 (400 points) | 5130–6450 (221 points) |

Full-library counts are 20,934 Raman, 46,072 FTIR, and 905 NIR for raw;
20,566/44,612/905 for derivative; and 20,671/44,006/895 for nobaseline.
All artifacts validate as `OpenSpecy` where applicable.

Raman and FTIR axes are identical to their old counterparts. Full NIR axes are
not identical because the new libraries have 1,334 points versus 1,333 old;
the NIR medoid axes are identical. The new raw FTIR library is much larger
(46,072 versus 22,585 spectra). NIR identity overlap is limited: raw shares 451
identifiers, with 454 new-only and 440 old-only.

The pre-partition population is 68,081, but the typed raw libraries total
67,911. The 170-spectrum difference is not explained by the empty `filters`
assessment and should receive an explicit audit row before release.

## Data-quality and reporting caveats

- `assessments$warnings` has the correct stable empty schema but contains zero
  rows, despite convergence and short-support S/N warnings in the log. The
  current working tree now captures model warnings, but this run began before
  that change was loaded.
- Reference-library test counts are paired; model test counts are not.
- Macro comparisons span changed class vocabularies in several artifacts.
- NIR and medoid test cohorts are small, especially the 8–10-spectrum medoid
  tests, so their perfect or near-perfect estimates have high uncertainty.
- `assess_spec` reported that some medoid spectra lacked 20 usable intensity
  values for running S/N. Those warnings need to be associated with spectrum
  IDs in the saved warnings/quality tables on the next build.

## Recommended starting actions

1. Treat the full and medoid type-specific libraries as the release candidate;
   do not promote the new Raman models as primary identification artifacts.
2. Repair model assessment so old and new inference use exactly the same IDs,
   expected labels, and eligibility mask; report new-only NIR separately.
3. Audit the 170 pre-partition spectra absent from the typed raw totals and
   populate `filters` with the responsible rule and IDs.
4. Review residual nobaseline FTIR CO2 findings and a sample of the 27,458
   derivative FTIR flatten operations to confirm that the ratio criterion is
   not over-correcting broad features.
5. Review the weakest class confusion rows, prioritizing raw Raman `other
   material`, organic matter, `other plastic`, and polystyrenes.
6. Run the final working-tree code once more so the faster symmetric
   self-correlation path and model-warning capture are represented in the
   production log and `assessments$warnings` object.

