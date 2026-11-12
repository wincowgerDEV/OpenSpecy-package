# Feature Plan: Dated Library Downloads

**Feature dir**: `specs/053-dated-library-downloads`  
**Date**: 2026-11-12  
**Review budget**: Under 100 nonblank lines and approximately 1,500 words.  
**Current tranche**: Report downloaded reference-library dates and resolve an optional target date to the nearest available version.  
**Change class**: package/scientific (package download API and reproducibility metadata; spectral content is not transformed)

## Goal

- Make every `get_lib()` download identify the resolved library date so users can judge currency and confirm a targeted release.
- Add a simple date policy without weakening exact `revision` pins or changing the downloaded `OpenSpecy` objects.

## Scope

- **In**: one optional `date` argument, package-owned historical version metadata, live current-object header discovery, nearest-date selection, messages, tests, help, SOP, and NEWS.
- **Out**: AWS upload/deployment changes, public S3 enumeration, library rebuilding, caching policy changes, checksum policy changes, and changes to `load_lib()`.
- **Users**: analysts downloading the latest library, reproducing work near a calendar date, or continuing to use exact AWS S3 version IDs.

## Requirements

- R1. `get_lib()` reports each downloaded type's UTC calendar date and exact S3 version ID, including latest unversioned and explicitly pinned downloads.
- R2. `get_lib(date = ...)` accepts one `Date` or strict `YYYY-MM-DD` string, resolves each requested type to the smallest absolute calendar-date difference, prefers the earlier version on an exact tie, downloads that version ID, and reports requested and resolved dates.
- R3. `date` and `revision` are mutually exclusive; malformed, missing, or empty version metadata fails clearly before a misleading dated download.
- R4. Existing calls without `date` still download the current unversioned object, exact `revision` calls retain their URL contract, unknown types still warn, and the function remains invisibly side-effect-only.
- R5. The package catalog contains the verified 2026-09-22 and 2026-10-21 releases. Live current headers are appended for date selection so a newer current object remains targetable; superseded intermediate versions require a package catalog update because the private S3 origin cannot be enumerated publicly.

## Technical Decisions

- **Approach**: add internal header parsing, strict date normalization, known-version catalog, and deterministic resolver helpers in `R/manage_lib.R`. Use base `curlGetHeaders()` against the existing CloudFront object URL to obtain `Last-Modified` and `x-amz-version-id`; use the selected version ID in the existing `download.file()` call.
- **Public API**: `date = NULL` is meaningful policy requested by users and triggers nearest-date selection by input presence. `revision` remains the advanced exact pin. Resolution/header helpers stay internal because they have no independent package workflow. `...` continues to belong only to `download.file()`.
- **Dependencies**: none; base R and existing `utils` functionality suffice.
- **OpenSpecy contract**: downloaded RDS bytes and later `load_lib()` return structures (`wavenumber`, aligned `spectra`, `metadata`, and attributes) are untouched. Only selection and console observability change.
- **Generated artifacts**: edit roxygen in `R/manage_lib.R`, confirm installed roxygen2 8.0.0 matches `DESCRIPTION`, run `devtools::document()`, and inspect only the expected `man/manage_lib.Rd` diff; never edit generated files directly.
- **External resources**: `d2jrxerjcsjhs7.cloudfront.net`; live integration remains opt-in and guarded against that actual host. Unit tests mock header and download boundaries.
- **Reference intake / official build / frozen validation**: N/A; this consumes published runtime artifacts and does not rebuild, admit, prune, or validate spectra.
- **Reference workflow compatibility**: verify latest and exact-revision URL behavior, dated per-type resolution, messages, and unchanged invisible return. No large library download is required locally.
- **Performance and observability**: one small HEAD request per requested type before the existing object download; each message gives type, resolved date, and version ID. N/A for production-scale staging.
- **Bundled Shiny app**: no source or asset change. Its latest-library call remains unversioned; only package console messages become more informative.
- **Pipeline diagram**: N/A; spectral analysis state and data flow are untouched.
- **Hosted Shinylive/WebAssembly app**: `R/` is a shared hosted input, so run fast `-HostedAppStatic`. No route, runtime, interaction, dependency, package pin, library-staging contract, or generated artifact changes; exact-artifact and clean-rebuild tiers are not triggered.

## Package Surfaces

- `R/manage_lib.R`: public argument/docs plus internal version/date resolution and messages.
- `tests/testthat/test-manage_lib.R`: parsing, tie behavior, validation, URL and message regressions; large AWS test remains opt-in.
- `vignettes/sop.Rmd`: current, dated, and exact-revision examples and catalog limitation.
- `man/manage_lib.Rd`: regenerated by roxygen2 8.0.0.
- `NEWS.md`: user-visible date reporting and selection entry.
- `benchmarks/`: N/A - new low-volume network-selection behavior, not a same-output performance refactor.
- `DESCRIPTION`, `NAMESPACE`, workflows, `.github/workflows/`, `inst/`, `site/`, README: unchanged.

## Work Checklist

- [x] Implement and document date/version resolution in `R/manage_lib.R` while preserving latest and exact-revision behavior.
- [x] Add focused mocked current-behavior tests in `tests/testthat/test-manage_lib.R`.
- [x] Update `NEWS.md` and `vignettes/sop.Rmd`; regenerate and inspect `man/manage_lib.Rd` with the configured toolchain.
- [x] Run focused tests, vignette source validation, full `devtools::test()`, and fast `-HostedAppStatic`; leave R CMD check deferred because this is not release/CRAN-facing.
- [x] Reconcile every checkbox with evidence; record deferred gates, stop or record owned processes, and remove task-created scratch files.

## Verification

- Direct acceptance: mocked HEAD/download checks for latest, exact revision, closest date on both sides, earlier tie, per-type resolution, messages, and failures.
- Toolchain: `C:\Program Files\R\R-4.3.3\bin\Rscript.exe`; roxygen2 must equal 8.0.0 before documentation.
- Focused: quality gate filter `manage_lib`; opt-in live AWS download remains skipped unless explicitly enabled.
- Documentation: `devtools::document()` then inspect `man/manage_lib.Rd`, `NAMESPACE`, attribution, aliases, usage, and examples.
- Broad: one full `devtools::test()` after the focused candidate passes; `-HostedAppStatic` because `R/` changes. R CMD check is proportionally deferred.
- Vignette: parse/knit-sensitive source checks through existing tests; no evaluated network example.
- Shinylive/WebAssembly: fast source tier only; no matching-artifact or rebuild trigger.
- Closure: zero unchecked items, clean diff check, no owned R processes, status reviewed, scratch removed, unrelated existing worktree changes preserved.

## Risks And Open Questions

- Public CloudFront objects expose exact current/historical timestamps but the private origin returns 403 for version listings. The package catalog is therefore the reproducible historical authority and must add a row before a current release is superseded; live current discovery prevents the newest object itself from becoming unreachable by date.

## Approval Notes

- Approved by: user request on 2026-11-12.
- Follow-up: none.
