# FTIR Polymer Collision Screen

**Tranche** Local scientific analysis of an existing release, with no hosted impact.

## Goals and requirements

- Fix the standalone PolyTest workflow to print completed batch counts, elapsed time, and an ETA.
- Compare each observed plastic-labeled FTIR derivative spectrum against all eligible computed non-plastic-labeled FTIR derivative spectra. Use only native grid points within 800–3200 cm⁻¹ and exclude incomplete, non-finite, and constant spectra.
- Save the top Pearson score per query, a histogram, high-match metadata from both records, and overlays and regional scores to investigate the strongest pairs.

## Decisions and surfaces

- Use the typed r7 metadata-v1 derivative FTIR artifact from the original script. Use explicit origin and material type and report generic or otherwise unresolved labels. Preserve the published objects and use stable spectrum IDs.
- Verify bounded `match_spec()` with a progress callback. At full scale use exact blocked Pearson with a first-maximum scan because sorting all candidate scores for each query projected nearly an hour. Keep the best result across partitions, with exact ties resolved in input order.
- Screen cutoffs are inclusive and descriptive. Interpret scores against the screened library only. Real-world error rates require representative measured negatives, a competing plastic library, and a frozen classification rule.
- The runnable script belongs in `workflows/PolyTest.R` with the updated working copy beside the package. Keep output under ignored `rawrefdata/`. No package API, dependencies, published library, class mappings, tests, generated docs, package build inputs, or app pipeline changes. Package, documentation, and hosted gates are not triggered.

## Work checklist

- [x] Verify cohort tags, axis, IDs, and input artifact identity.
- [x] Verify bounded matching against direct Pearson scores on a small subset and then run the complete comparison with progress.
- [x] Reconcile exclusions and scores, review top overlays and metadata, and verify saved results, working copy, and process cleanup.

## Verification and open questions

- Use an existing artifact and a representative first block before continuing across all candidate blocks. Observe elapsed time, memory headroom, and callback advancement. Reevaluate if several minutes pass without progress or system available RAM falls below 5 GiB.
- The typed FTIR artifact's SHA-256 matched the release audit. The real-data probe matched `match_spec()` and direct `cor()` to within 1e-12. A synthetic fixture proved the window exclusions, across-block winners, progress, metadata joins, and exports.
- The 50,000-reference × 256-query comparison block held at most 12.8 million scores, about 98 MiB. The run evaluated all 16,609,811,220 pairs in 1,364 batches and finished in 20.0 minutes including loading and reports. The sampled peak working set was about 17.9 GiB. The initial sorted-matcher timing log is retained with the results.
- Output is in `rawrefdata/updatedv1/Processed/polytest/20261124-derivative-ftir-exact`. Re-read checks verified all 11,034 winners, threshold counts, and the 567 joined review pairs, including exact pair IDs and direct correlations within 1e-12. Both plots were visually checked. Fixed a Windows locale issue in the histogram label and re-rendered it from the saved winners.
- Source review found that Gicquel's `cellulose_like` label was present in the cleaned source, and `morphotype 1` remains an unresolved identity despite its broad organic matter class. No labels were adjudicated. These rates describe lookalikes in the screened collection.

## Gicquel exclusion sensitivity run

- Exclude eligible computed non-plastic candidates whose canonical `library_name` is `Gicquel Synthetic Microplastics - Computed Fourier-transform Infrared`. Keep the published FTIR artifact, query cohort, range and correlation method.
- Reuse the frozen unfiltered output only after matching the artifact hash, eligibility counts, complete query IDs and reference membership. Recompute retained winners' Pearson scores and search every remaining candidate for each query whose original winner was excluded.
- Use the new ignored output folder `20261124-derivative-ftir-no-gicquel`. Verify no Gicquel winner, unchanged retained IDs and scores, no score increase, direct Pearson agreement for the reviewed pairs, and agreement between reported counts and saved matches.
- Completed in 5.7 minutes. Excluded 100 candidates and kept 1,505,230. Rescanned 715 queries against the full retained pool in 93 batches and verified the other 10,319 winners against their traces. The output audit passed all stated checks. Counts at r ≥ 0.90 fell from 567 to 75 and at r ≥ 0.95 from 196 to 7. The strongest remaining pair was nylon 6 vs. QM14S at r = 0.9796957.

## Bidirectional screen

- Keep the same artifact, range, eligible populations, and Gicquel exclusion. Use each bounded score block once for both directions and for each observed polymer class. Retain stable input-order tie handling and record score and best-hit IDs.
- Save required 0.5-step thresholds plus the endpoint, a 0.05-step companion, overlaid empirical CDF, per-query-library boxes for each direction, and per-target-polymer-type boxes for computed queries. Label sample counts and type reference counts. Retain scores and box statistics for follow-up.
- Verify on a small exhaustive fixture and a real-data matched subset. Compare saved forward scores to the prior Gicquel-excluded result. Monitor bounded blocks and memory; revisit if system available RAM falls below 5 GiB or progress stops for several minutes. This changes only the local exploratory script and has no hosted impact.
- The run computed all 16,608,707,820 pairs in 1,705 batches and wrote results in 23.0 minutes to `20261124-bidirectional-no-gicquel`. Maximum comparison block was 25.6 million scores. The file audit verified all 11,034 forward winners against the previous run and all 1,505,230 reverse maxima against the saved per-type maxima. The reverse counts were 45,919 at r ≥ 0.80, 3,284 at r ≥ 0.90, and 173 at r ≥ 0.95.
- The first scan wrote its complete outputs before a trailing Rscript read error after a presentation edit. The final script passed the exhaustive fixture, all saved outputs passed their audit, and the corrected plots were regenerated from saved scores and visually reviewed. The library figure shows 23 observed and eight computed sources, and the type figure shows 38 observed polymer types with query and reference counts.
