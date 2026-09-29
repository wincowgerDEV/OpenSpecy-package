# Feature Plan: Despiking, Analysis Presets, And Particle Uncertainty

**Feature dir**: `specs/036-despiking-presets-particle-uncertainty`  
**Date**: 2026-09-28  
**Review budget**: Under 100 nonblank lines and approximately 1,500 words.  
**Current tranche**: Make Nicolas Coca Lopez's automated two-sided despiking the default, add reusable app-setting presets, and propagate particle-composition uncertainty and total-count RSD through package/app summaries and interactive plots.  
**Change class**: Mixed; highest class is package/scientific because default spectral correction and published uncertainty calculations change.

## Goal

- Give package and app users a dependency-free, attributed default despiker while retaining every existing spike-removal method.
- Make the MIPPR Thermo Fisher iN10 MX workflow reproducible and report statistically explicit uncertainty for material percentages and total particle concentration.

## Scope

- **In**: `correct_spike()` default/method controls; package contributor metadata; Default and MIPPR presets beside Advanced Load Settings; one exported material-class percentage uncertainty function; summary/download columns; Plotly material and size summaries.
- **Out**: enabling spike correction globally, `pracma`, multi-property/group/Šidák or correlation adjustments, finite-population correction without a supplied population, changes to identification science, and hand edits to generated docs/hosted artifacts.
- **Users**: R package users calling `correct_spike()`, `process_spec()`, `assess_spec()`, `automate_particle_analysis()`, or the uncertainty function, plus local/hosted app users.

## Requirements

- R1. Add a named automatic MAD-prominence-width method to `correct_spike()` and make it the first/default method. Detect positive and negative narrow peaks; estimate noise as raw `median(abs(diff(y) - median(diff(y))))`; use an automatic prominence threshold when none is supplied; merge marked intervals; and repair them from finite, unflagged local neighbors with conservative edge behavior.
- R2. Use base R plus the existing peak-feature machinery, never attach/install/import `pracma`. Use the maintainer-confirmed package/app defaults: maximum spike width 2 points, automatic prominence, noise multiplier 10, interpolation window 5 points, and both directions. Preserve explicit `method = "residual"` and the existing prominence/FWHM methods with their prior behavior.
- R3. Preserve the `OpenSpecy` axis, spectrum names/dimensions, metadata alignment, existing attributes, idempotence, and a complete `automatic_spike` diagnostic. `process_spec(correct_spike = TRUE)` and default `assess_spec(..., spike_args = list())` use the new detector; explicit old-method calls remain reproducible.
- R4. Credit `NICOLAS COCA LOPEZ` in `correct_spike()` authorship and add `person("Nicolas", "Coca Lopez", role = "ctb")` to `DESCRIPTION`; retain the existing Coca-Lopez reference and add script provenance where appropriate.
- R5. Add an action-style **Standard Settings** dropdown in the existing Advanced **Load Settings** box. **Default** resets every recognized control and saved quantification definition to captured app defaults. **MIPPR - Thermo Fisher iN10 MX** starts from those defaults, then enables Spatial Smooth, Collapse Particle Spectra, and Threshold Signal / Noise; selects Signal Times Noise with minimum 0.01; disables Threshold Correlation; enables Load Entire File into Memory; selects FTIR; disables Top N per organization; enables manual Flatten Region; and enables manual Range Selection from 800 to 3200 cm^-1.
- R6. Preset and CSV restoration share one validated update path, preserve owner/child gating, invalidate stale canonical/quantified/plot state, never replace uploaded spectra, and require Run. User Metadata continues to serialize effective controls rather than depending on a preset label.
- R7. Export `material_percentage_uncertainty(count, percentage, confidence = 0.95)`: for any material-class taxonomy, vectorized positive total counts, percentages in [0,100], and confidence in (0,1) produce the publication's single-property half-width `100 * abs(qnorm((1-confidence)/2)) * sqrt(p*(1-p)/count)`. Return a numeric percentage-point half-width; reject invalid/recycling-ambiguous input. No `groups` argument per maintainer clarification.
- R8. Both `automate_particle_analysis()` material summaries and the app's Thresholded Particles `particle_summary.csv` include count, observed percentage, confidence level, percentage-point uncertainty, clipped lower/upper percentage CI, and `total_concentration_rsd = total_particle_count^(-1/2)`; empty/single-class/zero-boundary cases are explicit and finite where the formula permits.
- R9. Replace the live material summary with Plotly count bars and error bars. Each material uses its percentage half-width converted to count units; an **All Materials** bar uses total count and `count * RSD = sqrt(count)` uncertainty. Report all labels only on hover, without in-bar text or adding All Materials to exported per-material rows.
- R10. Replace the live particle-size histogram with Plotly while retaining calibrated nominal-size units; hover reports bin bounds and count. A one-particle result must render one visible bar centered on its measured size rather than a compressed automatic histogram. Static ZIP figures remain PNGs but use the same summary/bin data as the interactive plots.
- R11. Keep the heatmap selection marker compact and white above registered imagery. Project collapsed-particle `x`, `y`, `centroid_*`, and `first_*` coordinate metadata through the uploaded map origin and signed axis steps exactly once, while retaining raw grid coordinates for connectivity.

## Technical Decisions

- **Public API**: `count`, `percentage`, and `confidence` are the only demonstrated inputs; the return is a singular numeric half-width composable with base `|>`. CI bounds, count-scale errors, and total RSD remain internal derived state. The primary workflow is `OpenSpecy |> correct_spike()`, followed independently by summary uncertainty where particle counts exist.
- **Dependencies**: no new dependency. Reuse base `stats::qnorm()`, `approx()`, and existing base-R peak/prominence helpers; verify representative detection fixtures against the supplied script's expected masks/results without shipping `pracma`.
- **Generated artifacts**: edit roxygen and `DESCRIPTION`, confirm configured roxygen2 8.0.0, run `devtools::document()`, and inspect `NAMESPACE`/`man/*.Rd` plus authorship/export diffs immediately; never hand-edit them.
- **OpenSpecy contract**: spectral correction changes only accepted intensity cells and its diagnostic attribute. Particle uncertainty reads canonical final particle metadata and never mutates spectra, IDs, spatial mappings, or calibration attributes.
- **Bundled Shiny app**: `canonical_state()`/`canonical_final()` remain the only source for visible/exported particle results. Add method-specific spike controls and adjacent guidance; presets respect muted children; Plotly and ZIP builders share derived tables. Affected states are no-upload preset/reset, processed spike on/off/method switch, identified collapsed particles, empty/one/many materials, and genuine summary/figure downloads. No asset is added.
- **Pipeline diagram**: update **Click Run**, **Ordinary Process**, **Particle Size Plot**, **Material Class Plot**, and **Download: Thresholded Particles (zip)** in `.specify/memory/pipeline-diagram.html` in the same change.
- **Hosted app**: shared `R/`, `DESCRIPTION`, `inst/shiny/`, vignette, and diagram inputs change. Preserve `/`, relative `/app/`, `/pkgdown/`, hard pins, dependency closure, small-library staging, and generated boundaries. Run fast `-HostedAppStatic` and an exact matching-artifact preset/spike/Plotly/download smoke when available; no clean wasm rebuild unless dependency, image, driver, pin, or release scope changes.
- **Performance**: new scientific behavior, not a same-output optimization; no benchmark is required. Add a bounded regression fixture with many spectra and stop/isolate if the new default is materially slower or allocates materially more than the residual path before running the full suite.

## Package Surfaces

- `R/{correct_spike,process_spec,assess_spec,material_percentage_uncertainty,automate_particle_analysis,automate_particle_filespecs}.R`: method/default, exported equation, and summaries. `tests/testthat/`: focused detector, API, processing, assessment, automation, app-helper, and Plotly/download tests.
- `inst/shiny/{ui,server,global}.R`: presets, method controls, shared summary/bin builders, Plotly outputs, and ZIP summaries/figures; asset inventory only, with no new asset. `.specify/memory/pipeline-diagram.html`: named boxes above.
- `DESCRIPTION`: Nicolas Coca Lopez contributor only; no import. `NEWS.md`, `vignettes/`: document default change, presets, formulas/assumptions, random-sampling limitation, percentage-point versus RSD interpretation, and interactive hover/error bars.
- `benchmarks/`: N/A - new output/default behavior, not a same-output refactor. `workflows/` and `.github/workflows/`: unchanged. `site/README/pkgdown`: source site/README unchanged; generated pkgdown remains build output.

## Work Checklist

- [x] Revise the dependency-free detector defaults to 2 width points and a 5-point interpolation window while retaining legacy methods (`R/correct_spike.R`, app controls, docs/tests).
- [x] Implement/test `material_percentage_uncertainty()` and propagate percentage CI/RSD columns through dense and file-backed automation summaries (`R/`, `tests/testthat/`).
- [x] Refine coordinate projection, the heatmap selection marker, hover-only material bars, and the one-particle size histogram (`inst/shiny/`, focused helper/server tests).
- [x] Update roxygen, vignette, `NEWS.md`, and affected pipeline boxes; regenerate and inspect generated documentation.
- [x] Apply `openspecy-develop-shiny-app`; run focused tests/browser states, full package tests, requested R CMD check, `-HostedAppStatic`, and conditional matching-artifact smoke.
- [x] Reconcile every checkbox with evidence; record deferred gates, inspect/stop owned processes and `git status`, and remove task-created scratch files.

## Verification

- Focused: source/app parse; `test-correct_spike`, `test-process_spec`, `test-assess_spec`, new uncertainty tests, `test-automate_particle_analysis`, FileSpecs particle tests, and targeted `test-run_app`; cover clean/broad/boundary/tied/NA/positive/negative spikes, default-versus-explicit residual, scalar/vector uncertainty, invalid confidence/count/percentage, and exact summary columns.
- Browser/genuine files: use a real map; apply Default after manual edits, apply MIPPR and inspect every override/muted child, enable each spike method and Run, verify no-upload/processed/identified/collapsed states, Plotly hover/error bars/All Materials/histogram bins, console, desktop/mobile screenshots, and unzip/read the genuine summary plus PNGs.
- Final candidate: Windows Rscript preflight; roxygen2 8.0.0 `devtools::document()` and generated-diff audit; vignette render; one full `devtools::test()`; `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -HostedAppStatic`. Defer `devtools::check()` to release/CRAN review unless requested or broader metadata failure appears.
- Hosted: use an exact matching action artifact for `/`, `/app/`, `/pkgdown/` startup, preset/run, Plotly, download, and one library match if available; otherwise record Actions deferral. No dependency/pin change means no clean rebuild. Reuse evidence only while covered source, fixtures, dependency closure, and package pin are unchanged.
- Long-stage/closure: no production-scale stage; fixture probes target seconds and bounded memory. If a focused or broad stage fails twice, isolate a reproducer before rerunning; finish with checklist, process, `git status`, asset/size, and scratch audits.

## Risks And Open Questions

- The supplied script used wider exploratory settings; the maintainer has clarified that a cosmic spike may span at most two adjacent points and that interpolation should search five points per side, so 2/10/5 is authoritative.
- Wald-style observed-proportion intervals can collapse at 0% or 100% and assume random particle sampling; preserve the published equation exactly, document that limitation, and do not imply that composition uncertainty captures laboratory, spectral, or concentration error. The separately requested `count^-1/2` RSD is labeled as a total-concentration estimate, not as an equation from the manuscript.

## Approval Notes

- Approved by: maintainer planning request and groups clarification, 2026-09-28.
- Closure: refinements passed focused/full tests, the genuine particle browser journey, documentation regeneration, hosted-static checks, and staged R CMD check (0 errors, 0 warnings, one pre-existing marked-UTF-8-data note). Exact-artifact hosted smoke still awaits a committed matching Actions artifact; remote synchronization remains maintainer-owned.
