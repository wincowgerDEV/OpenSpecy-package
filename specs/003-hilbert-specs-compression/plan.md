# Feature Plan: Monolithic Specs Hilbert Compression

**Feature dir**: `specs/003-hilbert-specs-compression`
**Date**: 2026-06-26
**Review budget**: Keep this file under 100 nonblank lines.

## Goal

- Make `as_Specs()` the high-level compression workflow for PCA, K-means, and Hilbert encoding, with helpers still callable independently.
- Preserve `OpenSpecy` reconstruction boundaries: Hilbert decode restores quantized variables first, then PCA inverse reconstruction restores the original wavenumber axis when a PCA model is present.

## Scope

- **In**: `as_Specs()` pipeline defaults for PCA then Hilbert, optional K-means at any legal step, exact 64-bit Hilbert code storage, Hilbert-only compression/decompression, Hilbert-distance matching, tests, roxygen, NEWS, and benchmarks.
- **Out**: Morton/Z-order encoding, approximate nearest-neighbor indexing beyond scalar Hilbert distance, Shiny app code, and claims of bit-exact recovery after PCA truncation or quantization.
- **Users**: Maintainers and advanced users compressing spectral libraries/maps after PCA, usually with no more than 16 variables at 4+ bits per variable.

## Source Thread Notes

- The shared discussion describes n-dimensional grid coordinates transformed into one Hilbert bit string, with coarse-to-fine bits serialized into a 64-bit code.
- R cannot represent all 64-bit integers exactly as doubles, so code storage should avoid a single unsafe numeric; use exact high/low 32-bit numeric rows and derive hex only for display or metadata.
- Morton was recommended earlier as simpler, but this feature explicitly rejects Morton because it is too simplified for this package goal.

## Requirements

- R1. `as_Specs.OpenSpecy()` can fit PCA, run K-means, and Hilbert-encode in one call; default behavior fits PCA then Hilbert-encodes because this is the common workflow.
- R2. Users can choose the compression order; K-means may run before, between, or after PCA/Hilbert, while any order that places PCA after Hilbert errors.
- R3. `encode_specs_hilbert(x, bits_per_variable = NULL, limits = NULL)` accepts only `Specs` objects and returns a valid `Specs` object with exact Hilbert codes.
- R3a. Default `bits_per_variable` is computed from the current variable count to pack the 64-bit space densely; requested variable/bit combinations error with a message reporting maximum variables for the requested bits and maximum bits for the requested variables.
- R4. Quantization maps each finite variable value to integer bins `0:(2^bits_per_variable - 1)` using fitted or supplied limits; limits and bin reconstruction policy are stored in attributes.
- R5. Encoded `values` use two exact numeric rows, `hilbert_hi` and `hilbert_lo`, with one column per active spectrum or cluster; metadata/coords alignment and `value_id` columns remain valid.
- R6. `decode_specs_hilbert(x)` restores the quantized variable matrix and a `Specs` object whose variables match the pre-Hilbert variables.
- R7. `decompress_spec()` detects Hilbert-compressed `Specs`, decodes Hilbert first, then applies the existing PCA inverse when `attr(x, "variable_model")` is a PCA model.
- R8. Hilbert-only decompression returns an `OpenSpecy` object on the stored original variables; PCA+Hilbert decompression returns an `OpenSpecy` object on the PCA model's original wavenumber axis.
- R9. `check_Specs()` accepts the encoded shape while still rejecting broken code rows, missing Hilbert metadata, invalid coords, or metadata/value misalignment.
- R10. `match_spec.Specs()` automatically uses fast Euclidean distance between Hilbert codes when both inputs are compatible Hilbert `Specs`; standard correlation remains the fallback.
- R11. No Morton API, argument, dependency, example, test fixture, or transformation record is added.
- R12. `decompress_spec(specs, index = i)` can reconstruct one or more spectra by numeric index without decoding/decompressing unrelated spectra; with `expand = TRUE`, indexes refer to `x$coords` rows.
- R13. Unpublished backwards-compatibility arguments are removed from `Specs` APIs before release.

## Technical Decisions

- **Approach**: Extend `as_Specs()` to build a `Specs` object, apply ordered compression steps, and reuse helper functions; Hilbert quantizes columns of `x$values`, stores exact high/low 32-bit code rows, and appends a transformation record containing variables, bits, limits, and reconstruction centers.
- **Public API**: Add `steps`, `n_components`, `model`, `centers`, `bits_per_variable`, and `limits` to `as_Specs.OpenSpecy()` as demonstrated workflow choices; add `index` to `decompress_spec()` for plotting/subset retrieval; export `encode_specs_hilbert()` and `decode_specs_hilbert()` because users need composable control.
- **Dependencies**: No new dependency initially; avoid archived or visualization-oriented Hilbert packages and avoid `bit64` unless implementation proves base numeric high/low rows inadequate.
- **OpenSpecy contract**: Preserve spectra/metadata IDs through `Specs` coords and metadata; decompressed spectra are reconstructed approximations with documented quantization/PCA tolerance, not guaranteed original measured intensities.
- **Generated artifacts**: Update roxygen in `R/*.R` and regenerate `NAMESPACE`/`man/*.Rd` only with configured roxygen tooling; inspect generated export/alias/authorship diffs immediately.
- **External resources**: N/A; no network or large external files.
- **Reference workflow compatibility**: No official library rebuild in this feature; if later used in `workflows/OpenSpecy_reference_library.R`, stage subset comparisons before full artifacts.

## Package Surfaces

- `R/`: Extend `R/Specs.R` with the monolithic pipeline, Hilbert helpers, and subset decompression; update `R/Specs_methods.R` for Hilbert-distance matching.
- `tests/testthat/`: Extend `tests/testthat/test-Specs.R` for default PCA+Hilbert, legal/illegal step orders, Hilbert-only and subset decompression, capacity errors, supplied limits, K-means positions, matching, and no Morton surface.
- `benchmarks/`: Add `benchmarks/specs_hilbert.R` with repeated encode/decode timing plus Hilbert match versus standard correlation timing.
- `workflows/`: Unchanged.
- `vignettes/README/pkgdown`: Add concise `Specs` workflow documentation to the relevant vignette or README section; no direct pkgdown HTML edits.
- `DESCRIPTION`: Unchanged unless a dependency becomes necessary.
- `NEWS.md`: Add a user-visible feature entry.
- External Shiny compatibility: Consider reading/writing `Specs` with new transformation metadata; no Shiny code in this repo.

## Work Checklist

- [x] Implement internal Hilbert bit transforms and high/low 32-bit code conversion in `R/Specs.R`.
- [x] Make `as_Specs.OpenSpecy()` run ordered PCA/K-means/Hilbert compression with common-case defaults.
- [x] Add `encode_specs_hilbert()`/`decode_specs_hilbert()` with roxygen and transformation metadata.
- [x] Update `decompress_spec.Specs()` to decode Hilbert before PCA inverse and support Hilbert-only reconstruction.
- [x] Update `check_Specs()` validation for encoded `Specs` invariants.
- [x] Add Hilbert-distance matching in `R/Specs_methods.R` with compatibility guards.
- [x] Add focused tests in `tests/testthat/test-Specs.R`.
- [x] Add benchmark script, README documentation, and NEWS.
- [x] Add indexed subset decompression and remove unpublished compatibility arguments.
- [ ] Regenerate generated docs with configured tooling once roxygen2 8.0.0 is available.

## Verification

- Focused tests: `devtools::test(filter = "Specs")`.
- Toolchain/version preflight: confirm installed roxygen2 matches `Config/roxygen2/version: 8.0.0` before documentation generation.
- `devtools::document()`: required after roxygen/export changes; inspect generated diffs immediately.
- Full `devtools::test()`: required after focused tests pass.
- `devtools::check()` or CI/R CMD check: required before release-facing merge.
- Benchmarks: run `benchmarks/specs_hilbert.R`; require successful roundtrip tolerance checks and faster Hilbert matching than correlation on representative data.
- Reference-library/long workflow staging: N/A for this feature unless the official workflow starts writing Hilbert-compressed artifacts.

## Risks And Open Questions

- The Hilbert algorithm must be a true n-dimensional Hilbert mapping, not bit interleaving; add small known-value fixtures to guard against accidental Morton behavior.
- Should low-level point encoders be exported later, or remain internal behind `Specs` workflows until there is a demonstrated non-`Specs` use case?
- Shared `limits` are important for future library/query comparability; matching should require compatible Hilbert metadata rather than silently comparing unrelated code spaces.

## Approval Notes

- Approved by:
- Follow-up: Consider candidate-window or nearest-neighbor retrieval after scalar Hilbert matching behavior is stable.
