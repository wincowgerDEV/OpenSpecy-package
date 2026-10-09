# Hierarchical FTIR Model Validation Report

**Completed**: 2026-10-08  
**External evidence**: `Positive_Controls/hierarchical_model_validation`  
**Decision**: retain the OS1 FTIR derivative model; do not promote any current-source flat or hierarchical candidate.

## Executive finding

The frozen workflow reproduced the OS1 medoid and model accuracy and recovery RSDs exactly. All overall, sample, material-type, and size-stratum Stage 1 deltas were 0.000 percentage points after excluding `PMMA_15Nov223_control` and `RedPETFibers_15Nov223_control`. This establishes that the package workflow, not a scoring mismatch, produced the Stage 2 differences.

None of the four new model candidates met the predeclared non-inferiority requirement. OS1 achieved 93.998% mean per-map specific accuracy and 95.496% plastic/non-plastic accuracy. The best new specific result was hierarchical-guardrailed at 87.567% (-6.431 points), while the best new broad result was flat-guardrailed at 93.640% (-1.856 points). Every candidate also missed the polyethylene-recall requirement by at least 6.571 points.

Guardrailed lambda selection is nevertheless preferable to macro-only selection as the next development baseline. It improved mean holdout accuracy within both topologies without materially sacrificing grouped-development macro accuracy. It is not sufficient on its own: the next selector needs broad-class, critical-class, and worst-domain constraints before optimizing calibration.

## Frozen design and compatibility

- Four candidates used the same saved current FTIR medoid input, preprocessing, folds, class support, `automate_particle_analysis()` settings, and 20 eligible maps. Only topology and lambda-selection rule changed.
- Recovery outputs were invariant for every candidate: count, area, and Feret accuracy matched OS1 on all comparable maps.
- Every artifact used the same source and workflow hashes. All 20 maps completed without warnings.
- The slowest map took 109.8 seconds across the six evaluated artifacts, satisfying the five-minute map requirement.
- Model fitting converged, but the four fits took 30.2-48.3 minutes, failing the plan's ten-minute fit target. Hierarchical deploy artifacts were about 137-138 KiB versus 267 KiB for flat models; size does not offset the accuracy loss.

## Accuracy and variability

| Model | Selection | Specific accuracy | Specific RSD | Broad accuracy | Broad RSD | Gate failures |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| OS1 model | Historical | 93.998% | 7.570% | 95.496% | 6.098% | Reference |
| Flat current | Macro | 84.221% | 23.537% | 92.896% | 8.340% | 12/15 |
| Flat current | Guardrailed | 86.065% | 22.297% | 93.640% | 7.591% | 12/15 |
| Hierarchical current | Macro | 85.669% | 17.688% | 88.016% | 15.720% | 13/15 |
| Hierarchical current | Guardrailed | 87.567% | 19.593% | 89.144% | 18.704% | 11/15 |

The guardrailed rule improved specific accuracy by 1.844 points for flat models and 1.898 points for hierarchical models relative to macro selection. Broad accuracy improved by 0.744 and 1.128 points, respectively. This consistent direction supports keeping the one-absolute-percentage-point accuracy guard as the first selector constraint.

The positive-control gate still rejects every new model. Flat-guardrailed missed specific accuracy by 7.933 points overall, 10.983 points for plastic samples, and 12.306 points for polyethylene particles. Hierarchical-guardrailed missed broad accuracy by 6.351 points overall and 13.365 points for plastic samples. Its non-plastic specific and broad accuracies were the only difficult subgroup results within one point of OS1 (-0.993 and -0.674 points).

## What the hierarchy fixed and broke

The current hierarchy is a soft two-stage model: a plastic/not-plastic root followed by mineral-versus-organic or a 39-class plastic branch. This removes direct competition between environmental spectra and polymer subclasses only when the root probability is correct; all plastic subclasses still compete inside one large conditional model.

Compared with flat-guardrailed, hierarchical-guardrailed:

- reduced nonplastic-to-plastic errors from 226 to 134;
- reduced polyethylene-to-PVA errors from 93 to 4 among regressions versus OS1;
- increased plastic-to-nonplastic errors from 5 to 115;
- most visibly replaced the PE-to-PVA failure with 69 PE-to-organic-matter failures;
- improved the large soil controls by 6.8-7.8 accuracy points versus OS1, but severely regressed the red PE beads (-43.0 specific, -53.7 broad), clear silicone tubing (-36.4 for both), and blue silicone gasket (-25.6 specific, -30.2 broad).

That imbalance explains why hierarchical-guardrailed made only 275 particle-level specific errors, versus 394 for flat-guardrailed, yet still failed the mean-per-map gate. It performs well on large environmental maps that dominate the particle count and poorly on several smaller clean-plastic controls. Equal map weighting correctly prevents those large maps from concealing the deployment risk.

Grouped builder results did not predict this domain shift. The hierarchical-guardrailed root had 89.45% OOF overall accuracy, with 90.26% plastic recall and 84.68% not-plastic recall; its plastic branch had 94.25% overall and 96.24% macro accuracy. On the locked controls, plastic broad accuracy fell to 84.98%. The current spectrum-identity grouping prevents identical identities crossing folds but does not fully hold out source library, instrument, acquisition campaign, or material presentation.

## Calibration and model selection

OS1 retained the best top-class Brier score (0.0815). Hierarchical-guardrailed was next at 0.1052 but had the worst expected calibration error (0.1502). Flat-macro had the lowest ECE (0.0834) but a much worse Brier score (0.1290). ECE alone is therefore not a safe selector: binning can make a less accurate model look calibrated, while the proper Brier score still penalizes incorrect confident predictions.

Future selection should be lexicographic and constraint-based, not one weighted score:

1. Keep candidates within one absolute point of the maximum OOF overall accuracy.
2. Within that set, require broad recall floors for both plastic and not-plastic, plus a floor on the minimum of the two recalls.
3. Require critical supported-class floors, initially polyethylene recall and explicit PE-to-PVA and PE-to-nonplastic confusion caps.
4. Require worst-source/domain recall and worst-domain broad recall once source-aware folds are available.
5. Keep candidates within one point of the best eligible macro accuracy.
6. Among eligible candidates, minimize multiclass log loss, then Brier score, then choose the largest lambda for the existing stability tie-break.
7. Apply these constraints to end-to-end joint OOF probabilities as well as each node. Strong branch metrics must not rescue a root that loses the correct branch.

This extends the implemented guardrail rather than replacing it. Macro accuracy remains useful for rare classes, but it becomes one protected dimension instead of the sole objective.

## Recommended development pathway

1. **Keep OS1 in production.** Preserve the historical flat alias and published artifact until a frozen candidate passes every map, broad-class, size, and polyethylene gate. The present hierarchy is diagnostic only.

2. **Make development validation source-aware.** Build folds that hold out entire reference source/library, instrument, or acquisition campaign while preserving class support. Continue grouping stable spectrum identities, but nest them inside the broader source group. Report both identity-grouped and source-grouped OOF results; select on the harder source-grouped result.

3. **Audit the broad and family taxonomy before refitting.** Confirm every reference spectrum's plastic/not-plastic label and add a durable polymer-family mapping. Quantify source and class support, duplicate/near-duplicate clusters, and spectral range coverage. Acquire independent development spectra resembling PE beads, colored spheres, silicone controls, soil, hair, mineral, and organic matter; do not move these locked positive controls into training.

4. **Test a three-level hierarchy.** The present plastic branch is still a 39-class flat problem. Add an intermediate family layer such as polyolefin, vinyl polymer, polyester, polyamide, elastomer/siloxane, and other polymer, then predict the final class conditionally. This directly separates polyethylene from PVA before leaf competition. Validate that each family has adequate source diversity; deterministic or undersupported branches should remain explicit and auditable.

5. **Prevent root dominance.** Compare class/source weighting, OOF probability calibration, and a development-selected blend of flat and hierarchical posteriors. A calibrated flat/hierarchical mixture or tempered root probability can retain environmental corrections without allowing one broad error to suppress every correct leaf. Select the blend only on source-grouped development folds.

6. **Optimize the fitting kernel.** Cache the aligned predictor matrix, folds, and preprocessing once; reuse complete `glmnet` paths rather than rebuilding data per lambda/node; checkpoint each node; and profile branch parallelism. The 30-48 minute fits must be brought below ten minutes before routine library iteration, although deployed map inference already meets its target.

7. **Repeat the locked confirmation once.** Freeze all topology, weights, taxonomy, calibration, and thresholds before rerunning these positive controls. A failed candidate should be reported and retired, not tuned against this cohort. Promotion should also require no material calibration regression and a separate maintainer decision.

## Artifact index

The detailed machine-readable outputs are in `C:/Users/winco/OneDrive/Documents/Positive_Controls/hierarchical_model_validation/comparison`, including:

- `noninferiority_gate.csv` and `paired_*_vs_os1.csv`;
- `calibration_summary.csv` and `calibration_bins.csv`;
- `particle_transitions_vs_os1.csv`, targeted failures, and truth-pattern confusion tables;
- builder node/class metrics and model-fit manifests;
- map runtimes, artifact manifests, and exact Stage 1 gate summaries.

The external `hierarchical_model_validation_report.md` is the compact generated summary. This report adds the development interpretation and recommendation while preserving the holdout as a confirmation set.
