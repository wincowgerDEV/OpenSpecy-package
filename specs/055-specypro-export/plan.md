# Feature Plan: SpecyPro Export

**Feature dir**: `specs/055-specypro-export`  
**Date**: 2026-11-12  
**Current tranche**: Download one retained spectrum from the current committed Run as `SpecyProExportV1`.  
**Change class**: bundled-app behavior  
**Status**: Implemented and verified 2026-11-12; closed to unrelated refinements.

## Goal And Scope

- Add **SpecyPro Export** to the existing native download selector so a user can take one committed Raman/FTIR result into the separate generative-AI interpretation app.
- **In**: versioned JSON payload/schema, checksum, committed processing/selection metadata, bounded candidates/references, current-state gating, pipeline diagram, NEWS, focused and browser regressions.
- **Out**: popup/network handoff, authentication, credits/payment, API keys, uploads from OpenSpecy, NIR, a public R API, and changes to scientific processing or identification.

## Requirements

- R1. The option appears only after a successful current Run has a retained active selection; no upload, dirty, running, failed/reset, or rejected selections cannot download it.
- R2. The payload validates as `SpecyProExportV1` v1.0.0, contains one final processed `OpenSpecy` spectrum, primitive selected metadata, committed settings/ranges/corrections/thresholds/metrics/quantification, up to ten ordered candidates, optional aligned reference spectra, OpenSpecy version, stable Run hash, UTC export time, and canonical SHA-256 checksum.
- R3. Only unambiguous Raman/FTIR selections with 16--200,000 finite aligned points and supported units export. Output stays below 10 MB, preserves selection identifiers, and fails with a useful message rather than serializing stale/invalid state.
- R4. A settings change after Run removes the option until the next successful Run; selection changes within the same current Run produce a new payload while retaining the committed Run hash.
- R5. The app performs no network request and receives no SpecyPro/OpenAI credential; users explicitly import the downloaded `.specypro.json` elsewhere.

## Technical Decisions

- **State owner**: `canonical_state()` plus `analysis_dirty()`, `analysis_running()`, `analysis_needs_reset()`, and retained `active_spectrum_view()` status own availability. Capture state, selected processed spectrum, metadata, matches, and committed library in one export reactive turn; never rebuild settings from live controls.
- **OpenSpecy flow**: export `active_spectrum_view()` derived from `canonical_final()`/the bounded file-backed selection. Preserve its `wavenumber`, one spectra column, aligned one-row metadata, identifier, and intensity/type attributes; no second processing path or raw aggregate is created.
- **Helpers/API**: pure schema/checksum/mapping helpers live in `inst/shiny/global.R`; no package export or generated documentation. The external JSON schema is bundled under `inst/schema/` as a compatibility artifact.
- **Run snapshot**: extend committed `run_settings` with identification type/transformation/library provenance, saved definitions, automatic corrections, and a deterministic SHA-256 Run hash derived from committed output-determining state and source identifiers.
- **Candidates**: real-library matches join committed library metadata and the exact selected reference columns; model results carry bounded candidate labels/scores without reference arrays. Missing optional provenance is omitted, never emitted as `NA`.
- **Download**: extend the existing `download_data` binding, managed browser bridge, nonempty-file check, choices, and label. No additional JS or asset is introduced.
- **Pipeline diagram**: add a click-triggered SpecyPro snapshot/output branch from the retained active spectrum and committed state.
- **Hosted impact**: `inst/shiny/` is shared local/hosted source, so run `-HostedAppStatic`; the changed download interaction also requires the local browser journey and an exact matching-artifact preflight when one is available. Routes, dependencies, pins, staged libraries, workflows, and generated outputs are unchanged; clean wasm rebuild is deferred because no dependency/image/driver/pin changes occur.

## Package Surfaces

- `inst/shiny/global.R`: internal validation, canonicalization, checksum, selection/candidate/payload helpers, choices/label.
- `inst/shiny/server.R`: committed Run snapshot/hash, availability, one-turn payload builder, `.specypro.json` download.
- `inst/shiny/ui.R`: adjacent local-file and no-credential guidance for the selected export type.
- `inst/schema/specypro-export-v1.schema.json`: copied v1 external contract with source hash recorded in code comments/tests.
- `tests/testthat/test-run_app.R`: helper contract, checksum, bounds, choice/order, and static committed-state assertions.
- `tools/shiny-local-smoke.spec.js`: genuine Raman upload/Run/export inspection plus dirty-state removal.
- `.specify/memory/pipeline-diagram.html`, `NEWS.md`: synchronized output branch and user-visible feature entry.
- `R/`, `DESCRIPTION`, generated docs, `NAMESPACE`, `man/`, site/routes, workflows, dependencies, and bundled image assets: unchanged.
- Benchmarks: N/A -- new bounded export behavior, not a same-output performance change.

## Work Checklist

- [x] Add the internal v1 schema/checksum/mapping helpers and bundled schema.
- [x] Commit all export-determining Run fields and wire the gated existing download control.
- [x] Add focused helper/static tests and a genuine browser download journey.
- [x] Update pipeline diagram and NEWS; run focused/static, targeted browser, and hosted-static gates.
- [x] Reconcile evidence, inspect the downloaded JSON/screenshot/console, run `git diff --check`, report assets/hosted deferrals, inspect owned processes/status, and clean scratch.

## Verification

- Focused: parse app sources; source `global.R`; validate fixture-shaped payloads, cross-language-style checksum, scalar metadata sanitizing, finite/length/unit limits, 10 MB bound, candidate cap/order, and unchanged OpenSpecy alignment.
- Static integration: assert the choice is current-state-only, mapper reads committed state and retained active view, live `input$...` values are absent from payload construction, and the native download binding/nonempty check remains.
- Browser: upload a genuine Raman fixture, explicitly set identification controls, Run, download `openspecy-<hash>.specypro.json`, inspect schema fields/checksum/selection/candidates, change an output-determining setting, and confirm the option disappears; inspect screenshot and severe console/server errors.
- Hosted: run `-HostedAppStatic`; run exact-artifact preflight only if a matching action-built artifact exists. Full clean wasm rebuild, full package tests, documentation, and R CMD check are not triggered by this app-internal, dependency-neutral tranche.
- Assets: no new `inst/shiny/www` file; report count/bytes unchanged. Reference/holdout/rebuild/intake/benchmark gates are N/A.

## Risks And Open Questions

- Real-library reference arrays can exceed the service's 10 MB limit; add optional references in rank order only while the serialized payload remains bounded, always retaining candidate metadata.
- Ambiguous `id_spec_type = "all"` resolves from the selected spectrum metadata/attributes; if exactly one Raman/FTIR modality cannot be proven, export is unavailable with an explicit error.

## Approval Notes

- Approved by the user's implementation request on 2026-11-12. Remote synchronization remains maintainer-owned.
- Final evidence: `run_app` 1,062 assertions and the named Playwright journey passed; `HostedAppStatic` passed 391 checks; the browser verified a real Raman JSON, JavaScript checksum, schema-shaped arrays/objects, screenshot, dirty-state removal, and clean severe console/server channels.
- The bundled external schema matches SHA-256 `7a9a05b84f8df48b9d71637608292f39882ab87f5afe5052c13c902bfe80be50`; `inst/shiny/www` remains 6 files / 107,924 bytes, while app source grows 28,497 bytes and the schema adds 4,581 bytes.
- Exact-artifact hosted preflight is deferred: cached artifacts are for `dca659c`, `d2899d9`, or `da6fd42`, not current HEAD `c0369f9f6338f35110e196f42d247e4d9284b63f`. Full package tests, documentation, R CMD check, and clean wasm rebuild remain untriggered per this dependency-neutral app plan.
