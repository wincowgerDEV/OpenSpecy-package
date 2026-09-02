# Raman Alpha Benchmark — 2026-09-02

## Design

The benchmark used release `9cfc00e59556` and varied only multinomial elastic-
net `alpha`. Stepping from 0 through 1 by 0.1 produces 11 values, despite the
request referring to ten tests, so both endpoints and all nine interior values
were evaluated.

Derivative and nobaseline Raman were assessed separately. Every fit reused the
released Raman medoid training object, wavenumber-wise mean fill, seed 123,
stratified folds, inverse-frequency weights, grouped multinomial coefficients,
`intercept = FALSE`, relative normalization, class eligibility, and lambda
selection by cross-validated macro class accuracy. Every alpha was then tested
on the exact same source-local production holdout within its recipe: 2,051
derivative and 2,069 nobaseline spectra across 21 classes. Coverage was 100% and
no fit warnings were captured.

The `alpha = 0.1` refits exactly reproduced the released model assessments,
validating the benchmark cohort and implementation.

## Results

All values are percentages. CV macro is the out-of-fold value used to select
lambda within each fit. Holdout macro is the primary class-balanced comparison;
holdout total accuracy is the percentage of all spectra classified correctly
and is included to expose majority-class effects.

| Alpha | Derivative CV macro | Derivative holdout macro | Derivative total accuracy | Nobaseline CV macro | Nobaseline holdout macro | Nobaseline total accuracy |
|---:|---:|---:|---:|---:|---:|---:|
| 0.0 | 89.20 | 74.32 | 83.18 | 89.54 | 73.84 | 87.39 |
| **0.1** | **90.53** | 74.22 | 81.18 | 89.58 | 75.40 | 92.36 |
| **0.2** | 90.05 | 74.32 | 83.03 | **89.80** | **75.42** | **92.80** |
| 0.3 | 90.17 | 74.36 | 83.96 | **89.80** | 75.42 | 92.75 |
| 0.4 | 89.82 | 74.38 | 84.40 | 89.72 | 75.41 | 92.70 |
| 0.5 | 89.93 | 74.34 | 83.57 | 89.70 | 74.56 | 91.35 |
| 0.6 | 89.75 | 74.34 | 83.57 | 89.65 | 74.68 | 90.38 |
| 0.7 | 89.73 | 74.32 | 83.03 | 89.34 | 74.67 | 90.19 |
| 0.8 | 89.75 | 74.39 | 84.59 | 89.01 | 74.61 | 89.08 |
| 0.9 | 89.51 | 74.32 | 83.03 | 88.48 | 74.11 | 87.58 |
| 1.0 | 89.51 | **74.47** | **86.20** | 88.10 | 74.57 | 88.30 |

## Interpretation

For derivative Raman, `alpha = 0.1` remains the cross-validation winner at
90.53% macro accuracy. The fixed holdout is nearly flat across the entire alpha
range. Its nominal winner, `alpha = 1.0`, reaches 74.47% versus 74.22% at 0.1,
a gain of only 0.25 percentage points. All of that gain comes from Raman
mineral, which rises from 80.95% to 86.12% across 1,989 spectra. The other 20
classes are unchanged. Overall accuracy therefore rises 5.02 points even though
macro accuracy barely moves, and CV macro declines 1.02 points.

For nobaseline Raman, `alpha = 0.2` is the narrow winner: CV macro is 89.80%
and holdout macro is 75.42%, versus 89.58% and 75.40% at 0.1. The holdout gain
is only 0.021 percentage points. Again, the only changed class is mineral,
rising from 92.42% to 92.87% across 2,006 spectra; the other 20 classes are
unchanged. Values from 0.5 upward form a clearly weaker holdout region.

If one common alpha is required for both Raman recipes, 0.1 has the best mean
CV macro accuracy (90.06%). Alpha 0.4 has the highest mean holdout macro value,
but improves on 0.1 by only 0.087 percentage points and does so through mineral
predictions rather than broader class recovery.

## Recommendation

Keep Raman `alpha = 0.1` for now. It is the derivative CV winner, nearly tied
on both fixed holdouts, and avoids choosing a parameter because it improves only
the dominant mineral class. A recipe-specific nobaseline value of 0.2 is
defensible but not materially better at the present precision.

Before changing the model policy, repeat the alpha sweep over several fixed
stratified split seeds and compare the distribution of macro accuracy and
per-class changes. That would distinguish a stable alpha effect from the
single-split differences of at most one quarter of a percentage point observed
here.

## Reproduction

The retained script is `benchmarks/reference_library_raman_alpha.R`. It accepts
a completed aggregate, recipe, output directory, and optional comma-separated
alpha values; each fit is checkpointed and emits CV, holdout, runtime, warning,
and class-level results.
