# Positive-Control Medoid And Model Validation Report

**Completed**: 2026-10-07  
**Cohort**: 20 holdout maps; `PMMA_15Nov223_control` and
`RedPETFibers_15Nov223_control` excluded throughout.  
**Result root**:
`C:\Users\winco\OneDrive\Documents\Positive_Controls\library_validation_3_libraries`

## Decision

Keep the current full derivative library as the primary development candidate.
The current medoid is a credible compact alternative, but it is descriptively
1.61 percentage points lower in specific accuracy than current full. Do not
replace the OS1 FTIR derivative model with either new model.

Current-model versus OS1-model specific accuracy changed by **-9.777 points**
(paired map-bootstrap interval -17.759 to -3.825); plastic/non-plastic accuracy
changed by **-2.600 points** (-4.596 to -0.680). Both intervals exclude zero.
Within the new model family, cross-class closure is beneficial: current versus
pre-closure improves specific accuracy by **+4.067 points** (1.270 to 7.909).
The closure rule should therefore continue, but it does not solve the model's
class/objective problem.

## Stage 1 Reproduction

The OS1 medoid and OS1 model each ran through the same
`automate_particle_analysis()` workflow used for all later artifacts. Both
complete 20-map routes exactly reproduced their supplied saved results at the
overall, per-sample, material-type, and size-stratum levels:

| Representation | Specific mean/RSD (%) | Broad mean/RSD (%) | Maximum gate delta |
| --- | ---: | ---: | ---: |
| OS1 medoid | 92.228 / 12.162 | 94.067 / 11.199 | 0.000 pp |
| OS1 model | 93.998 / 7.570 | 95.496 / 6.098 | 0.000 pp |

The independent saved-output scorers also reproduced all locked recovery and
identification targets exactly. No package-function change was required in
this extension.

During comparison, the validation scorer exposed one taxonomy mismatch. The
new models emit `ftir_polyethylene`; OS1 emits `ftir_poly(ethylene)`. The
existing validation-only `polyethylene` crosswalk was extended to this
FTIR-prefixed spelling. Raw labels remain unchanged in the trace files. Both
Stage 1 routes were rerun first and remained exact; all Stage 2 folders were
then re-scored from unchanged map checkpoints.

## All-Nine Accuracy

| Artifact | Specific mean/RSD (%) | Broad mean/RSD (%) |
| --- | ---: | ---: |
| OS1 full | 94.508 / 9.097 | 96.409 / 5.999 |
| Pre-closure full | 94.555 / 7.540 | 95.953 / 6.726 |
| Current full | **94.725 / 7.295** | **96.090 / 6.405** |
| OS1 medoid | 92.228 / 12.162 | 94.067 / 11.199 |
| Pre-closure medoid | **93.187 / 8.900** | **95.350 / 8.081** |
| Current medoid | 93.118 / 8.495 | 95.075 / 8.030 |
| OS1 model | **93.998 / 7.570** | **95.496 / 6.098** |
| Pre-closure model | 80.154 / 29.111 | 91.346 / 11.039 |
| Current model | 84.221 / 23.537 | 92.896 / 8.340 |

Current medoid versus OS1 medoid changed specific accuracy by +0.890 points
(-2.249 to 4.525) and broad accuracy by +1.008 (-1.131 to 4.025). Current
versus pre-closure medoid was essentially neutral: -0.069 specific (-0.514 to
0.345) and -0.275 broad (-0.678 to 0.068).

Recovery was exactly invariant across all artifacts because segmentation
precedes matching. The same count, area, and Feret mean/RSD values therefore
apply to all nine representations.

## Why The New Models Underperform

The release target changed from 26 OS1 classes/363 predictors to 41
classes/400 predictors. After the label crosswalk correction, current versus
OS1 still has 285 specific-ID particle regressions and 87 corrections. The
largest regression transitions are:

| OS1 prediction | Current prediction | Particles |
| --- | --- | ---: |
| `ftir_poly(ethylene)` | `ftir_polyvinylalcohols` | 93 |
| `ftir_organic matter` | `ftir_polyamides` | 47 |
| `ftir_organic matter` | `ftir_polyacrylamides` | 20 |
| `ftir_mineral` | `ftir_polyesters` | 19 |
| `ftir_organic matter` | `ftir_polyacrylates` | 17 |
| `ftir_mineral` | `ftir_polyamides` | 15 |
| `ftir_organic matter` | `ftir_polyesters` | 15 |

The red-bead recovery map alone contributes 94 specific regressions; compost
contributes 51, soil 33, soil/cypress 26, and human hair 18. The loss is not
confined to one size: current-versus-OS1 specific accuracy is -9.350 points for
maps above 500 um and -10.892 for 50--500 um. It also affects both material
groups: -13.364 points for plastic maps and -7.125 for non-plastic maps.

The current builder's independent grouped-medoid holdout reveals the objective
tradeoff. Moving from its old to new source raises macro-class accuracy from
86.51% to 95.59%, but lowers overall accuracy from 90.32% to 89.05%. Important
high-support classes also fall: mineral 87.34% to 77.25%, organic matter 87.31%
to 83.15%, and polyethylene 94.33% to 91.56%. `train_spec_model()` selects
lambda using macro-class accuracy; with the expanded fine taxonomy, that can
reward rare-class performance while sacrificing abundant environmental and
polyethylene classes.

A fixed probability threshold is not an adequate repair. At 0.66, the current
model retains only 70.77% coverage and reaches 90.17% particle-weighted
specific accuracy, versus OS1 model's 77.69% coverage and 97.02% retained
accuracy. Of 420 current-model specific errors, 192 (45.7%) have probability
at least 0.66, indicating confident class-structure errors rather than only
low-confidence edge cases.

## Recommended Development Pathway

1. Freeze this cohort and workflow as a confirmation gate. Do not tune class
   definitions, lambda, thresholds, or individual references on this holdout.
2. Use current full as the primary release candidate. Use current medoid only
   when deployment size or latency justifies its accuracy tradeoff. Keep the
   OS1 model until a new model passes predeclared non-inferiority gates.
3. Establish stable canonical class IDs before training. Keep display labels
   separate; fail builds on unmapped, duplicate, or normalization-colliding
   `dimension_conversion` labels.
4. Develop a hierarchical model on independent data: first classify broad
   plastic/polymer versus mineral/organic/other material, then predict polymer
   subtype. Compare it with the flat 41-class model using overall, broad,
   macro-class, calibration, and per-class metrics together.
5. Retain class-aware closure. It materially improves the current model over
   pre-closure, but closure and model-objective changes should be evaluated as
   separate experiments with minimum class support and preserved spectral
   modes.
6. Calibrate probability and abstention rules on separate development data.
   Report coverage alongside conditional accuracy; do not select 0.66 here.
7. Add independent class-balanced maps targeting polyethylene versus PVA,
   organic matter versus polyamides/acrylates/polyesters, and mineral versus
   polymer subclasses. Finish with a sequestered confirmation set.

## Runtime And Reproducibility

All 120 added map evaluations completed in memory. The slowest map took 115.9
seconds, maximum recorded post-map physical-memory use was 58.3%, and all runs
emitted zero warnings. All maps therefore met the requested <5-minute and <80%
memory limits.

The maintained runner is
`benchmarks/positive_control_medoid_model_validation.R`. Each of the six new
result folders contains artifact/input/source/configuration manifests,
per-map outputs, warning/runtime records, and compatible checkpoints. The
`comparison` folder contains all-nine summaries, paired material/size/particle
tables, top-two margins, label and artifact audits, model failure attribution,
builder-holdout diagnostics, the plot, and the generated detailed report.
