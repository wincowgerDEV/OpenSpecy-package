# Feature Plan: Console Progress for Batched Matching

**Tranche** Package console feedback for direct `match_spec()` calls.
**Change class** Package behavior with shared hosted source.

## Goal and requirements

- `query |> match_spec(library, top_n = 1L, batch_size = 100L)` prints completed and total batches, elapsed time, and estimated remaining time as it runs.
- An explicit batch size enables the reporter for spectral `OpenSpecy` matching. `progress = FALSE` or `NULL` is quiet. An existing callback still receives the same fields.
- Match results, ordering, ties, metadata, and both input objects are identical with or without progress. No new export, dependency, or matching algorithm.

## Technical decisions and surfaces

- Make the existing named `progress` option explicit after `...` in `R/match_spec.R`. Infer its default from `batch_size`. Reuse the existing per-block callback and flush each stdout line immediately. Dense and implicitly grouped calls retain their current default output.
- The bundled app already supplies its own callback or explicit `NULL`. Preserve the particle workflow's existing progress by passing `FALSE` at its internal batched call. The app pipeline and visual state need no source or diagram changes.
- Add targeted tests in `tests/testthat/test-match_spec.R`, repeated timing and result equivalence in `benchmarks/match_spec_grouped.R`, update NEWS and regenerate `man/match_spec.Rd`.
- Shared `R/` changes trigger `-HostedAppStatic`. No hosted route, staged library, runtime assembly, pin or dependency change. No full hosted preflight, WebAssembly rebuild, or release package check is needed.

## Work checklist

- [x] Add default console progress and preserve custom callbacks and existing caller behavior.
- [x] Run a direct console call and focused tests. Run the repeated benchmark before full tests.
- [x] Regenerate docs with the configured roxygen version. Inspect generated diffs. Run hosted static and the full local suite.
- [x] Review status and processes, then clean task scratch.

## Verification and open questions

- Assert one line per completed batch, correct counts, elapsed and ETA fields, quiet overrides and callback preservation. Time estimates use average elapsed time per finished block and can change across groups of different sizes.
- Compare the public reporter and silent output to the previous bounded execution on the same inputs. Apply the package benchmark threshold to the timing comparison.
- The direct call printed three batch lines and returned five matches. Focused tests passed 912 assertions. The batched timing benchmark returned identical matches and medians of 82 ms with and without the reporter.
- Roxygen2 8.0.0 generated only the expected `match_spec.Rd` diff. Hosted static passed 391 assertions and script checks. The full suite passed 5,521 assertions with two skips and zero warnings or failures.
