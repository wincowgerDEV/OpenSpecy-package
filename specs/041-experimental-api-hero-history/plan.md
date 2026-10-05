# Feature Plan: Experimental APIs and Project History

**Feature dir**: `specs/041-experimental-api-hero-history`  
**Date**: 2026-10-05  
**Review budget**: Under 100 nonblank lines and 1,500 words.  
**Current tranche**: Add experimental lifecycle notices to selected package help and improve the static landing page with project history and an autoplaying muted hero video.  
**Change class**: Hosted/release (landing interaction plus release-facing R documentation; no scientific behavior change).

## Goal

- Clearly identify the Hilbert encoding/decoding and temperature/emissivity APIs as experimental.
- Present OpenSpecy's 2017–2026 development history and play the hero project video automatically without sound.

## Scope

- **In**: Roxygen notices for `encode_specs_hilbert()`, `decode_specs_hilbert()`, `estimate_temperature()`, and `calculate_emissivity()`; generated Rd; a concise accessible timeline in `site/`; direct muted autoplay for the hero video; focused contracts and final 2.0.2 source-tar verification.
- **Out**: Function behavior/signatures, other lifecycle classifications, tutorial-video privacy behavior, bundled Shiny behavior, wasm package/dependency pins, deployment, and remote synchronization.
- **Users**: Package users evaluating experimental APIs and landing-page visitors learning the project history.

## Requirements

- R1. Each named public API is visibly labeled experimental in its generated help without adding a dependency or changing exports, signatures, examples, or object behavior.
- R2. The landing page exposes an accessible 2017–2026 chronological timeline containing every maintainer-supplied milestone.
- R3. The hero project video is a direct YouTube privacy-enhanced iframe configured for muted autoplay and inline playback; the tutorial remains click-to-load.
- R4. The root `/`, relative `/app/`, and `/pkgdown/` contracts remain intact, and the 2.0.2 tarball passes exact-source CRAN checks; 2.0.2 supersedes 2.0.1 because CRAN reports that 2.0.1 already exists.

## Technical Decisions

- **Approach**: Add plain roxygen `Experimental` sections rather than a lifecycle-package dependency. Keep the two Hilbert notices on their shared `Specs` help page and add separate notices to the two thermal help pages. Use semantic HTML (`section`, ordered list, time elements) and responsive CSS for the timeline. Replace only the hero consent button with an eager muted-autoplay iframe; retain JavaScript click-to-load support for the tutorial.
- **Public API/dependencies/OpenSpecy contract**: Documentation only; no new API, dependency, or change to `wavenumber`, `spectra`, `metadata`, identifiers, attributes, or scientific output.
- **Generated artifacts**: Update `R/*.R`, verify configured roxygen2 8.0.0, run `devtools::document()`, and inspect `NAMESPACE`/Rd diffs immediately.
- **Bundled Shiny/pipeline diagram**: N/A; `inst/shiny/` and the analysis pipeline are unchanged.
- **Hosted impact**: `site/index.html` and `site/assets/site.css` are shared hosted inputs. Run `-HostedAppStatic`, focused landing contracts, and action-equivalent preflight/browser evidence when a matching wasm artifact is available. No clean wasm rebuild: package dependencies, image, driver, pin, libraries, and app source are unchanged.
- **External resources**: Existing YouTube privacy-enhanced host only; no new resource or download.

## Package Surfaces

- `R/Specs.R`, `R/temperature_emissivity.R` and generated `man/*.Rd`: experimental notices only; `NAMESPACE` unchanged.
- `site/index.html`, `site/assets/site.css`: timeline and hero-video markup/layout; `site/assets/site.js` unchanged unless static inspection exposes dead hero-only logic.
- `tests/testthat/test-shinylive_wasm.R`: focused static contracts for timeline and video attributes; existing scientific tests unchanged.
- `DESCRIPTION`/`NEWS.md`: version 2.0.2 and documentation/landing release entry.
- README/pkgdown inputs, `inst/`, workflows, wasm pins/repository, libraries, and generated hosted output: unchanged.

## Work Checklist

- [x] Add source roxygen experimental notices and regenerate/audit Rd output.
- [x] Add the 2017–2026 timeline and direct muted-autoplay hero iframe; retain tutorial privacy loading.
- [x] Add/run focused hosted landing contracts and inspect responsive rendered states.
- [x] Run `-HostedAppStatic`; run matching-artifact preflight if available and record any justified deferral.
- [x] Build and inspect `OpenSpecy_2.0.2.tar.gz`, run the exact CRAN check, and reconcile status/processes/scratch files.

## Verification

- Roxygen: configured/installed 8.0.0; generated only `Specs.Rd`, `estimate_temperature.Rd`, and `calculate_emissivity.Rd`; `NAMESPACE` unchanged.
- Focused/hosted: `shinylive_wasm` and final `-HostedAppStatic` passed 370 assertions with 0 failures/warnings/skips; JS/R/PowerShell source checks passed.
- Browser: network-enabled Playwright against the staged Pages shell passed at 1440×900 and 390×844; all years were ordered, the YouTube child player loaded with autoplay/mute/playsinline and autoplay permission, the tutorial remained click-to-load, and neither viewport overflowed. Screenshots were visually reviewed.
- Action preflight: deferred to post-commit CI because no wasm artifact matches current HEAD `aa26254151da3c72c938616037ad57891d5b2ee1`, and the maintained preflight prohibits pairing an artifact with dirty package/site inputs.
- Release: the live 2.0.1 check reported that 2.0.1 already exists, so the candidate was bumped to 2.0.2. Exact network-enabled `R CMD check --as-cran --no-manual` on 2.0.2 completed in 561.2 seconds with 0 errors, 0 warnings, and 2 environmental notes (updated today; unavailable time verification).
- Artifact: `OpenSpecy_2.0.2.tar.gz` contains 250 entries, no repository/site/scratch debris, and all three experimental help sections; 1,962,421 bytes; SHA-256 `2E8A847D219E540DB54C8B51BFF64C126F4F2968B94600181EB27AC77D717595`.
- Manual: local PDF generation remains dependent on configured MiKTeX; all Rd structure/content checks passed.
- Benchmarks/reference workflows/long stages: N/A; no computational behavior or library workflow changes.

## Risks And Open Questions

- Browser autoplay is policy-controlled; muted autoplay plus `allow="autoplay"` is the standards-compatible request, but individual user settings may still block it.
- YouTube still receives a request when the page loads; this is explicitly requested for the hero video, while the longer tutorial preserves click-to-load privacy.
