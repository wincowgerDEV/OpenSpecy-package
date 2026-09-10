# Feature Plan: Radiometric Temperature-Emissivity Diagnostics

**Feature dir**: `specs/031-temperature-emissivity-separation`  
**Date**: 2026-09-09  
**Status**: Implemented and verified as experimental; measured validation is required before production recommendation.  
**Current tranche**: Add guarded, in-memory-first, 100,000-spectrum-capable temperature/emissivity APIs plus diagnostic map metrics; do not change identification spectra or app defaults.  
**Change class**: package/scientific.

## Goal And Scope

- Calculate blackbody radiance and model-conditioned temperature/emissivity from calibrated FTIR thermal-emission spectra while preserving `OpenSpecy` alignment.
- Expose temperature and emissivity summaries for evaluating particle/background contrast without implying that ordinary absorbance, transmittance, reflectance, or detector counts are radiance.
- **In**: an internal Planck kernel; experimental spectral-smoothness TES; optimized blocked `OpenSpecy` estimation with FileSpecs parity; selectable scalar emissivity summaries; bounded emissivity materialization; failure diagnostics; a `def_features()` example.
- **Out**: raw interferogram/count calibration; instrument self-emission correction; at-sensor atmospheric or solar/BRDF correction; automatic thresholds; direct `sig_noise()`, `assess_spec()`, `automate_particle_analysis()`, bundled-app, or `match_spec()` integration; changing measured spectra for identification.
- **Users**: analysts with radiometrically calibrated, surface-leaving FTIR emission spectra and a measured/modelled downwelling radiance spectrum.

## Requirements

- R1. The internal Planck kernel returns spectral radiance per cm^-1 using exact SI constants, a coordinate-correct wavelength/wavenumber Jacobian, and stable `expm1()` evaluation; it rejects non-finite/non-positive wavenumber or Kelvin inputs.
- R2. Retrieval accepts only `OpenSpecy` FTIR spectra explicitly labelled `W m^-2 sr^-1 (cm^-1)^-1` with surface-leaving/calibration provenance. It rejects absorbance/transmittance/reflectance, normalized, derivative, baseline-corrected, arbitrary-count, or unknown-unit input.
- R3. The surface model is `L = eps * B(T) + (1 - eps) * L_down` for an opaque, isothermal target. `downwelling`, Kelvin search bounds, and the fitting wavenumber range are required; scalar zero is allowed only as an explicit no-background assumption.
- R4. Temperature selection uses a wavenumber-aware spectral-smoothness objective and requires a resolvable interior minimum. Boundary, flat, multiple, near-singular, or insufficient-band fits return a non-success status rather than a temperature estimate; no arbitrary denominator replacement or emissivity clipping is allowed.
- R5. `estimate_temperature()` returns a compact source-aligned `data.table`: index/ID/coordinates, estimated material temperature K, one `emissivity_value` selected by Planck-weighted band-effective (default), mean, median, or max, roughness/physical/valid fractions, band/statistic provenance, and status. Failed fits remain aligned with `NA`; Celsius remains derived.
- R6. `calculate_emissivity()` explicitly materializes an `OpenSpecy` emissivity object only for user-selected or collapsed spectra and supplied scalar/per-spectrum temperatures. It preserves axis, columns, metadata, coordinates, and valid attributes; values remain unclipped so model/calibration failures remain visible.
- R7. The primary dense `OpenSpecy` path estimates 100,000 spectra in bounded BLAS-backed blocks without full emissivity output or bands x spectra x temperatures allocation. FileSpecs streams the same kernel and caches keyed metrics; results are deterministic and block-size invariant.
- R8. Temperature/emissivity outputs are diagnostics, not material identities. Users may visualize metrics with `heatmap_spec()` and pass an explicitly validated predicate to `def_features()` on an aligned conventional FTIR map. Re-estimate temperature after particle collapse; no package default or library match consumes the metric.
- R9. Documentation names assumptions, units, metric interpretation, rejection states, and effective-pixel versus intrinsic emissivity. Do not recommend TES-derived particle metrics until held-out data reproducibly outperform or complement direct radiance/SNR contrast.

## Technical Decisions

- **Evidence basis**: follow the [NIST wavenumber Planck formulation](https://nvlpubs.nist.gov/nistpubs/Legacy/TN/nbstechnicalnote910-8.pdf) and Borel's [original underdetermined ISSTES model](https://digital.library.unt.edu/ark:/67531/metadc696880/). Do not copy the proposed global `which.min()` solver: absolute roughness can collapse toward zero at high trial temperatures ([Wu et al., 2017](https://doi.org/10.1109/IGARSS.2017.8128417)), and full MIR use needs terms beyond the simple longwave model ([Cheng et al., 2011](https://doi.org/10.1109/TGRS.2010.2076818)).
- **Public API**: export `estimate_temperature(x, downwelling, temperature_range_k, fit_range_cm1, radiance_uncertainty = NULL, emissivity_stat = c("planck_weighted", "mean", "median", "max"), block_size = NULL, ...)` with `OpenSpecy` and `FileSpecs` methods, and `calculate_emissivity(x, temperature_k, downwelling, block_size = NULL, ...)` for selected `OpenSpecy` spectra. `emissivity_stat` always yields one `emissivity_value`; `block_size = NULL` derives bounded memory. Add no public Planck, grid-step, clamp, matching, correction, worker, or progress switches. Existing `as_OpenSpecy.hyperSpec()` is the bridge.
- **Retrieval**: let `N = L - L_down`. Precompute Planck reciprocals and coordinate-aware curvature coefficients on small shared coarse/fine grids; evaluate direct masked curvature in bounded spectrum blocks and use BLAS only for non-cancelling positive scale terms. This avoids quadratic-expansion cancellation, scalar missing-band fallbacks, and 100,000 optimizers. Require a statistically resolvable objective prominence, make one final summary pass, and use uncertainty for weighted masking/singular-channel rejection.
- **OpenSpecy/FileSpecs contract**: inputs remain unchanged. Temperature rows retain source order, `col_id`, coordinates, and FileSpecs indices. `calculate_emissivity()` sets `attr(., "intensity_unit")` and per-spectrum `intensity_units` to `"emissivity"`, retains `spectra_type = "ftir"`, clears incompatible derivative/baseline state, and appends method/provenance without changing IDs. Extend source documentation for radiance/emissivity units and provenance fields.
- **Dependencies/generated artifacts**: use base R plus existing `digest`, `matrixStats`, and `data.table` dependencies; add none. Update roxygen sources and regenerate `NAMESPACE`/`man/*.Rd` with configured roxygen2 8.0.0; inspect exports, aliases, authorship, and references immediately.
- **Performance**: optimize dense `OpenSpecy` first with bounded masked BLAS blocks; FileSpecs reuses that kernel/readers and caches metrics. Target <=2 minutes and <=640 MiB incremental R heap (<=1.5 GiB total) for in-memory 850 x 100,000; FileSpecs target <=5 minutes/750 MiB. Probe 10,000 first; stop/rework above a 5-minute projection or 2 GiB peak. Each block is a progress boundary.
- **Bundled Shiny app / pipeline diagram**: N/A; `inst/shiny/` and `.specify/memory/pipeline-diagram.html` remain unchanged because this tranche adds only an opt-in package workflow.
- **Hosted Shinylive/WebAssembly**: shared `R/` and vignette inputs change, so run the fast `-HostedAppStatic` tier. No route, runtime interaction, dependency, package pin, library staging, generated app, or `/`-`/app/`-`/pkgdown/` contract changes; exact-artifact preflight and clean wasm rebuild are not triggered.

## Package Surfaces

- `R/temperature_emissivity.R`: generic/methods, bounded BLAS search, emissivity materialization, validation helpers, and reuse of existing FileSpecs streaming/cache primitives; `R/as_OpenSpecy.R`: document the two intensity units and radiometric provenance; `R/make_rel.R`: record normalization provenance so radiance validation can reject it.
- `tests/testthat/test-temperature_emissivity.R` and `test-process_spec.R`: physics checkpoints, input rejection, recovery/failure cases, chunk/cache equivalence, alignment, attributes, map metrics, and normalization provenance.
- `benchmarks/temperature_emissivity.R`: in-memory-first 1/100/10,000/100,000-spectrum time/memory, block invariance, summary options, false minima/noise, and particle/background discrimination; FileSpecs equivalence remains a focused-test responsibility.
- `workflows/`, `.github/workflows/`, `inst/`, `site/`, and `README.md`: unchanged. `DESCRIPTION`: unchanged unless implementation proves otherwise.
- `vignettes/advanced.Rmd`: calibrated-radiance setup, diagnostics, and aligned `def_features()` example; `NEWS.md`: experimental feature and limitations. Generated help/pkgdown only through their generators.

## Work Checklist

- [x] Implement internal kernels, blocked in-memory/FileSpecs estimation, four scalar summaries, selected-spectrum emissivity, validation, provenance, and source docs.
- [x] Cover independent physics, recovery/rejection, noise, irregular/missing axes, false/boundary minima, block/cache equivalence, alignment, aliases, and immutability.
- [x] Stage and run 1/100/10,000/100,000 benchmarks plus direct-radiance/SNR and shared-missing-band comparisons; retain measured validation as the recommendation gate.
- [x] Regenerate roxygen outputs and update the advanced vignette and NEWS without matching/preprocessing integration.
- [x] Complete focused, full-package, vignette, benchmark, and hosted-static gates; reconcile generated diffs, processes, status, and scratch artifacts.

## Verification

- Focused: `devtools::test(filter = "temperature_emissivity|as_OpenSpecy")`; require `B(1000 cm^-1, 300 K) = 0.0992403333 W m^-2 sr^-1 (cm^-1)^-1` within tolerance and integral agreement with `sigma*T^4/pi`.
- Scientific acceptance: synthetic flat/sharp emissivities across temperatures, backgrounds, and noise; ascending/descending and irregular axes; boundary/multimodal/singularity failures. Report temperature and emissivity error, invalid-fit rate, and direct-radiance/SNR versus TES contrast; measured traceable replicates remain a later recommendation gate.
- Performance: benchmark dense `OpenSpecy` at 1/100/10,000 first, project then run 100,000 only below the stop threshold; log dimensions, blocks, time/memory, summary modes, and block equivalence; verify FileSpecs equivalence in focused tests.
- Toolchain/docs: resolve Windows Rscript, confirm roxygen2 8.0.0, run `devtools::document()`, inspect generated diffs, and render `vignettes/advanced.Rmd`.
- Broad/hosted: run full `devtools::test()` once on the final candidate and `.agents/skills/openspecy-run-quality-gates/scripts/quality-gates.ps1 -HostedAppStatic`; defer `devtools::check()` to release/CRAN-facing review. Browser, matching-artifact, and clean-rebuild gates are N/A.
- Reusable evidence: R 4.3.3 focused/full tests passed; roxygen2 8.0.0 regenerated expected exports/help; the vignette rendered with zero warnings; hosted static passed 344 checks. Actual 850 x 100,000 clean/shared-gap runs took 77.89/78.87 s, peaked at 1385.8/1387.0 MiB, recovered all fits, and stayed within 0.0112 K. `devtools::check()` and measured-acquisition validation remain deferred as planned.

## Risks And Open Questions

- TES is underdetermined and the smooth-emissivity prior can suppress real sharp material structure. Thin/subpixel/transmitting/scattering or non-isothermal particles yield effective pixel properties, not intrinsic material constants.
- Near-room-temperature examples should use a validated longwave window within the FTIR range; 3-5 um data need reflected-source/solar/BRDF modelling and remain out of scope.
- Planck-weighted output is band-effective directional emissivity, not necessarily a lookup table's total hemispherical value. A traceable measured fixture and labelled replicates are required before this becomes a recommended metric; otherwise keep TES experimental.

## Approval Notes

- Approved by: Maintainer request, 2026-09-09.
- Follow-up: app or `automate_particle_analysis()` integration requires a later plan only after the metric and thresholds validate on genuine maps.
