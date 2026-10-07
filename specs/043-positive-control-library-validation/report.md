# Positive-Control Library Validation Report

**Completed**: 2026-10-06  
**Cohort**: 20 holdout maps; `PMMA_15Nov223_control` and
`RedPETFibers_15Nov223_control` excluded throughout.  
**Primary result root**:
`C:\Users\winco\OneDrive\Documents\Positive_Controls\library_validation_3_libraries`

## Decision

The current cross-class-closed library is the preferred development candidate,
but this holdout does not demonstrate that it is superior to the published OS1
library. Against OS1, its mean sample-level Specific ID accuracy changed by
**+0.218 percentage points** (paired bootstrap interval -1.496 to 2.364) and
plastic/non-plastic accuracy changed by **-0.319 points** (-2.144 to 1.275).
Both intervals include zero.

Closure itself had a small favorable signal. Holding everything except the
library fixed, current versus pre-closure changed Specific ID by **+0.171
points** (0.000 to 0.444) and broad ID by **+0.137 points** (-0.060 to 0.430).
At particle level it corrected five specific and five broad IDs, produced no
specific regressions, and produced one broad regression. This supports
continued class-aware closure development, not immediate replacement or a
tighter global removal rule.

## Stage 1: Published-Result Reproduction

The first current implementation run diverged at the S/N mask. The historical
validation used 800--2200 and 2420--3200 cm^-1, while the package's newer
hard-coded window was 750--2200 and 2420--4000 cm^-1. On the red-bead
reproducer this changed the retained mask from 2,659 to 2,451 pixels and the
particle result from 149 features with median area 13 pixels to 146 features
with median area 12 pixels.

`automate_particle_analysis()` now exposes a validated `sn_range` argument for
dense and file-backed workflows. Its default preserves current behavior; this
benchmark explicitly supplies the historical ranges. The legacy strict
`area > 1` rule is represented by the current inclusive `area_threshold=2`.

With those settings, the new OS1 run exactly reproduced the supplied saved
outputs:

| Metric | Mean (%) | RSD (%) | Mean delta | RSD delta |
| --- | ---: | ---: | ---: | ---: |
| Particle count recovery | 91.003 | 41.538 | 0.000 | 0.000 |
| Median area recovery | 109.716 | 53.575 | 0.000 | 0.000 |
| Median maximum Feret recovery | 97.659 | 35.940 | 0.000 | 0.000 |
| Specific ID accuracy | 94.508 | 9.097 | 0.000 | 0.000 |
| Plastic/non-plastic accuracy | 96.409 | 5.999 | 0.000 | 0.000 |

All 100 per-sample comparisons passed. All 15 material-type and all 15 size-
stratum mean/RSD rows passed with maximum absolute delta 0.000 percentage
points, including agreement for missing values. This is stronger than the
requested one-percentage-point gate.

All recovery means meet the study's 50--150% range. Only Feret RSD (35.94%) is
below 40%; count RSD (41.54%) and area RSD (53.57%) are not. These statistics
describe the requested 20-map subset, not the original 22-image cohort, and
one control lacks finite size truth so recovery summaries use N=19.

## Three-Library Results

| Library | Specific ID mean / RSD (%) | Broad ID mean / RSD (%) |
| --- | ---: | ---: |
| Published OS1 | 94.508 / 9.097 | 96.409 / 5.999 |
| Pre-closure | 94.555 / 7.540 | 95.953 / 6.726 |
| Current closed | 94.725 / 7.295 | 96.090 / 6.405 |

Segmentation precedes matching, so count, area, and Feret results were exactly
identical across libraries. The slowest full map took 89.6 seconds, versus the
300-second limit. Maximum recorded post-map physical-memory use was 45.5%, and
all 60 map runs emitted zero warnings.

Current-versus-pre-closure gains were small and sparse: three maps improved
specific accuracy, 17 tied, and none worsened; three improved broad accuracy,
16 tied, and one worsened. Specific changes were positive in both plastic and
non-plastic strata and in both measured size strata, but subgroup sample counts
are small and their intervals are descriptive.

## Particle And Reference Diagnosis

All six correctness-changing particles had a pre-closure top-hit reference
that is absent from current. Five became correct: one silicone particle moved
from organic matter to polysiloxanes, three soil particles moved from polymer
classes to organic matter, and one soil particle moved from a cellulose class
to mineral. The sole broad regression was red-bead particle `unit_000127`,
which moved from polypropylene (score 0.660) to organic matter (0.598) after
reference `a808d3bdca28b0bfd6edb9ed7709cc00` was removed. It is a sentinel for
independent review, not evidence for holdout-driven reinstatement.

The pre-closure library contains 41,005 FTIR spectra and current contains
39,281. Current lacks 2,320 pre-closure IDs and adds 596 IDs not present there;
2,173 of the removed IDs occur in the derivative-FTIR quarantine, leaving 147
without that provenance link. The largest removed classes are polyethylene
(423), organic matter (422), mineral (253), polypropylene (193), polyesters
(165), polyolefins (157), and polystyrenes (150).

Across 2,761 particles, current and pre-closure have nearly identical median
top-two margins (0.00569 and 0.00562); 64.51% and 64.65% respectively have a
margin at or below 0.01. Closure therefore did not materially resolve reference
ambiguity. The fixed 0.66 sensitivity would withhold 15.72% of current matches
and raises particle-weighted accuracy among retained matches to 97.08% specific
and 97.68% broad. This holdout cannot be used to choose that threshold.

## Recommended Library Path

1. Keep this exact workflow and cohort frozen as a confirmation gate. Do not
   tune thresholds, classes, or individual references on these results.
2. Continue class-aware closure rather than tightening one global correlation
   cutoff. Preserve minimum class coverage and within-class spectral modes,
   using diversity or medoid selection before adjudicating cross-class
   conflicts with metadata and expert evidence.
3. Audit the six changed particles on independent evidence, especially the
   removed polypropylene sentinel, and require a reason code plus conflict
   partners for every quarantine decision.
4. Reconcile the 147 removed IDs without a quarantine link and document the
   596 current-only IDs before release. Keep raw labels and stable IDs.
5. Develop abstention/confidence rules on separate data and report both
   coverage and conditional accuracy; do not optimize 0.66 on this holdout.
6. Add class-balanced positive controls, rare/confusable materials, and unseen
   environmental matrices. Require predeclared overall non-inferiority, no
   important class/size loss, and a final sequestered confirmation run.

## Reproducibility Artifacts

The maintained runner is `benchmarks/positive_control_library_validation.R`.
The external `comparison` folder contains overall, material, size, paired,
particle-transition, match-margin, threshold, runtime, reference-flow, and
quarantine-attribution CSVs; the plot and detailed generated report live there
as well. Each library folder includes exact library/input/source/configuration
manifests, per-map outputs, runtime and warning records, and restartable
checkpoints. SHA-256 verification and frozen workflow hashes guard reuse.

The only validation taxonomy crosswalk is directional: raw `polyethylene` and
model label `ftir_polyethylene` are scored as their parenthesized equivalents
for compatibility with the published truth regex. Raw labels remain present in
all particle traces.
