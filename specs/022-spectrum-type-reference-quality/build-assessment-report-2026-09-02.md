# Downstream Reference-Library Rebuild Assessment — 2026-09-02

## Outcome

The downstream rebuild completed successfully from the previously processed
derivative and nobaseline libraries. Release `9cfc00e59556` contains the full
libraries, NA-preserving medoids, NA-tolerant models, and 25 assessment tables.

- Output root: `C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/reference-library-rebuild-na-support-20260902`
- Aggregate: `releases/9cfc00e59556/reference_library_build.rds`
- Aggregate size: 433,195,916 bytes (about 413 MiB)
- Approximate elapsed time: 2 h 54 min, including model fitting, old/new
  assessments, serialization, and release validation
- Manifest: 134 components built and one release promoted
- Reload check: all seven standalone library, medoid, and model RDS files are
  exactly identical to their corresponding objects in the aggregate

This run intentionally reused upstream processing. It did not rerun ingestion,
CO2 correction, high-tail correction, S/N filtering, or pruning.

## Optimized missing-value fill

The optimized matrix method is retained. On a fixed 401 by 1,000 matrix with
20% missing values, five repetitions gave a median of 0.03 seconds versus 0.07
seconds for the literal spectrum-by-spectrum implementation: approximately
2.3 times faster, or a 57% elapsed-time reduction, with numerically identical
output. `mean_replace()` now fills matrix columns as spectra while preserving
its original vector behavior.

The benchmark also confirmed the selected FasterPAM path: 0.18 versus 0.50
seconds median on the probe, with only a 0.024% objective increase. These are
retained benchmarks with regression guards, not one-off timing claims.

## Support recovery and NA preservation

The 10% finite-support rule retained many spectra that complete-case filtering
previously excluded.

| Recipe | Type | Input | Retained for medoid selection | Removed below 10% | Previous complete cases |
|---|---|---:|---:|---:|---:|
| Derivative | Raman | 20,566 | 20,519 | 47 | 9,821 |
| Derivative | FTIR | 44,612 | 44,517 | 95 | 31,943 |
| Derivative | NIR | 905 | 905 | 0 | 905 |
| Nobaseline | Raman | 20,671 | 20,621 | 50 | 9,883 |
| Nobaseline | FTIR | 44,006 | 43,873 | 133 | 31,710 |
| Nobaseline | NIR | 895 | 895 | 0 | 895 |

Every medoid selected by PAM was pulled back from the original unfilled
library. Consequently, published medoids retain real missingness: 111,977 and
115,070 missing values in derivative and nobaseline Raman, 116,766 and 115,582
in FTIR, and none in the NIR identification interval. Raman/FTIR medoids use
804–3198 cm^-1 (400 points); NIR uses 5130–6450 cm^-1 (221 points).

Model training then used wavenumber-wise mean replacement. It filled 103,870
Raman and 116,766 FTIR values for derivative typed models, and 109,085 Raman
and 117,182 FTIR values for nobaseline typed models. NIR required no fills.
All model candidates passed the 10% support gate; class-size filtering retained
16 Raman, 40 FTIR, and four NIR classes. Data-poor classes remain deliberately
excluded until they have at least ten training spectra.

## Candidate reference-library accuracy

Macro class accuracy is the primary metric. Each candidate result uses its own
reproducible, class/type-stratified approximately 10% holdout, and reference
members with selected query identifiers removed to prevent exact self-matches.

| Candidate artifact | Test n | Coverage | Macro class accuracy | Overall accuracy |
|---|---:|---:|---:|---:|
| Derivative FTIR | 4,461 | 100.00% | 99.72% | 99.64% |
| Derivative Raman | 2,058 | 100.00% | 84.91% | 99.56% |
| Derivative NIR | 90 | 100.00% | 100.00% | 100.00% |
| Medoid derivative FTIR | 4,452 | 100.00% | 97.96% | 95.69% |
| Medoid derivative Raman | 2,059 | 99.66% | 89.47% | 95.52% |
| Medoid derivative NIR | 90 | 100.00% | 100.00% | 100.00% |
| Nobaseline FTIR | 4,401 | 99.98% | 99.95% | 99.80% |
| Nobaseline Raman | 2,062 | 100.00% | 94.44% | 99.76% |
| Nobaseline NIR | 90 | 100.00% | 100.00% | 100.00% |
| Medoid nobaseline FTIR | 4,387 | 99.98% | 96.93% | 93.32% |
| Medoid nobaseline Raman | 2,060 | 97.52% | 88.70% | 95.87% |
| Medoid nobaseline NIR | 90 | 100.00% | 99.55% | 98.89% |

The full typed libraries are strong. Medoids lose some accuracy and, for
nobaseline Raman, 2.48% coverage, but now evaluate thousands of source-local
holdouts rather than the very small medoid-only cohorts used previously.

## Candidate model accuracy

| Candidate model | Test type | Test n | Coverage | Macro class accuracy | Overall accuracy |
|---|---|---:|---:|---:|---:|
| Derivative typed FTIR | FTIR | 4,452 | 100% | 93.04% | 76.35% |
| Derivative typed Raman | Raman | 2,051 | 100% | 74.22% | 81.18% |
| Derivative typed NIR | NIR | 90 | 100% | 98.43% | 97.78% |
| Nobaseline typed FTIR | FTIR | 4,387 | 100% | 89.76% | 71.76% |
| Nobaseline typed Raman | Raman | 2,069 | 100% | 75.40% | 92.36% |
| Nobaseline typed NIR | NIR | 90 | 100% | 99.11% | 97.78% |
| Derivative combined | FTIR | 4,452 | 100% | 90.59% | 73.00% |
| Derivative combined | Raman | 2,051 | 100% | 69.02% | 23.60% |
| Nobaseline combined | FTIR | 4,387 | 100% | 83.01% | 64.67% |
| Nobaseline combined | Raman | 2,069 | 100% | 69.00% | 27.55% |

The missing-value coverage problem is resolved: every model row has 100%
evaluation coverage. Typed Raman macro accuracy is also much stronger than the
34–39% seen in the prior run, although cross-run changes in support, lambda
selection, and cohort construction mean that difference is diagnostic rather
than a controlled causal estimate.

The combined models remain unsuitable as the preferred Raman route. Their
macro accuracy is moderately below the typed models, while overall Raman
accuracy collapses to 23.60–27.55%. The largest errors are dominance failures:
combined derivative predicts only 21.72% of 1,989 Raman mineral tests correctly,
and combined nobaseline predicts 25.82% of 2,006 correctly. The typed Raman
models achieve 74–75% macro accuracy and avoid that severe combined-type
failure, so identification should continue to dispatch by spectrum type.

The weakest candidate classes with at least five observations also include
nobaseline FTIR organic matter (49.21% typed), FTIR mineral (43.47% typed), and
several small polymer/paint classes around 50–64%. These are the most useful
targets for data review; changing the calibrated alpha, intercept, weighting,
or coefficient grouping is not justified by this tranche.

## Old/new cohort interpretation

The unequal-cohort issue is now handled explicitly instead of forcing spectra
through a cross-version taxonomy. Candidate artifacts are assessed on candidate
library data; published artifacts are assessed independently on published
library data. Each row records its source, denominator, evaluated classes, and
provenance. Counts therefore differ legitimately and no old-minus-new accuracy
delta should be inferred.

For context only, published typed model macro accuracy on its own data was
89.74% FTIR and 30.18% Raman for derivative, and 84.63% FTIR and 37.53% Raman
for nobaseline. Training membership of published models is unknown, so those
figures may be optimistic and are not paired comparators. The updated source-
local design is more durable when class standards change and removes the need
for fuzzy class matching.

## Warnings and quality shifts

The stable warnings table contains 13 rows, all `glmnet` convergence notices
for small lambda values after `maxit = 100000`. Larger-lambda solutions were
returned, and every model has a selected lambda and complete predictions. The
warnings should remain visible in release review, but they do not indicate a
failed artifact. No model settings were changed.

Because upstream quality processing was reused, `assess_spec` shifts match the
source build. The clearest favorable shift remains FTIR CO2 status: derivative
FTIR pass rate rises from 53.86% to 95.62%, and derivative FTIR medoid pass rate
from 51.23% to 93.51%. Full-range NIR reports missing values for every spectrum,
while its 5130–6450 identification interval has complete support; this is a
range-coverage distinction, not a model failure. FTIR medoids retain more
missing positions by design, reflecting restored original spectra rather than
the temporary PAM fill.

## Recommendations

1. Use the spectrum-type-specific models, especially for Raman; retain the
   combined models only as compatibility artifacts until their cross-technique
   class dominance is resolved.
2. Review Raman mineral behavior in the combined models and FTIR mineral/
   organic-matter confusion in the nobaseline typed model before publication.
3. Keep the calibrated model settings unchanged for now. Add training spectra
   to excluded and weak classes, then reassess with the same source-local
   protocol.
4. Preserve the support, fill-count, warning, and provenance assessment tables
   in every future release; they now separate coverage failures from accuracy
   failures cleanly.
5. Treat the 13 convergence messages as a review item. Only investigate model
   tuning if the selected lambda itself is affected or future accuracy regresses.

## Verification evidence

- Focused builder/matching tests: 320 passed
- Optimized fill and FasterPAM benchmark: passed output/objective/runtime guards
- Vignette render: passed
- Full suite: 3,058 tests passed; four stale Shiny helper expectations were
  isolated, updated, and the affected helper file then passed all 83 tests
- Fast hosted static gate: 293 passed
- Production aggregate and seven component reload comparisons: passed
- External output scan: no temporary/partial release files remain

