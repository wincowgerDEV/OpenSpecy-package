---
name: openspecy-ingest-reference-datasets
description: Prepare, validate, and admit local measured or physically simulated FTIR/Raman datasets into the OpenSpecy reference-library workflow. Use for rawrefdata manifests, dataset downloads, one-folder/one-cleaner scripts, CleanedOS_raw.rds creation, intensity/type/provenance checks, diagnostic plots, large out-of-core sources such as QMe14S, or deciding when class-reference curation may begin.
---

# Ingest OpenSpecy Reference Datasets

Keep raw downloads, dataset-specific cleaners, diagnostics, and cleaned outputs
under ignored `rawrefdata/`. Use tracked `workflows/reference_intake.R` for the
shared contract and `workflows/OpenSpecy_reference_library.R` for exact
manifest-selected admission. Do not start a production `build_lib()` run while
the maintainer is still adding or reviewing sources.

## Establish Safe Boundaries

1. Read the active plan and local `rawrefdata/datasets.csv` manifest.
2. Inventory current files and modification times. Treat any directory the
   maintainer says is receiving data as read-only: do not create, overwrite,
   rename, or delete within it. Reading and checksumming are allowed only when
   they will not interfere with the transfer.
3. Confirm `rawrefdata/` is ignored with `git check-ignore`; never force-add its
   downloads, cleaners, diagnostics, or RDS outputs.
4. Keep source-declared license/terms separate from the maintainer's use basis.
   When no clear grant is found, record `license_status = not_source_verified`,
   the unavailable/unclear source statement, and the maintainer-directed
   noncommercial/fair-use, CC-BY-NC-like assumption. Never attribute that
   assumption to the source publisher.

## Prepare One Dataset

1. Use `rawrefdata/<slug>/` for raw files,
   `rawrefdata/clean_<slug>.R` for its sole cleaner, and exactly
   `rawrefdata/<slug>/Processed/CleanedOS_raw.rds` for output. Do not make one
   cleaner recursively ingest unrelated RDS files.
2. Record the canonical release URL/DOI, retrieval date, citation, declared
   terms, origin (`experimental`, `computed_solid`, or `computed_molecule`), raw
   checksums/sizes, expected technique, and spectral mode before cleaning.
3. Make the cleaner define
   `clean_reference_dataset(dataset_dir, output_path)`. Avoid `setwd()`, fixed
   drive letters, and hidden downloads. Read cached raw data, preserve source
   IDs, and add per-spectrum provenance.
4. Preserve physical semantics. Label source-verified technique as lowercase
   `ftir` or `raman`; record faithful `intensity_units`; label transmittance as
   `transmittance` and set `instrument_mode = transmission`. Do not relabel it
   absorbance. Document calculated-mode scaling/broadening and avoid treating
   pixels, maps, replicates, or processing derivatives as independent materials.
5. For very large inputs, preclean out of core. QMe14S uses a fixed 6 cm^-1
   target axis and `reference_new_hash_deduper()` chunk by chunk before a full
   `OpenSpecy` object is assembled. Persist a source-ID/content-hash/action
   ledger and restartable chunk boundaries; stop above 80% RAM, above twice the
   projected runtime, or after 10 minutes without progress.

## Gate Before Saving

Do not promote `CleanedOS_raw.rds` until all gates pass:

- every row has the source-verified `spectrum_type`, correct `intensity_units`,
  and transmission mode where applicable;
- spectra, metadata, IDs, axis, attributes, citations, release, origin, and
  license/provenance fields are aligned and populated;
- `check_OpenSpecy()` returns exactly `TRUE` without warnings;
- `reference_write_diagnostic_plot()` produces the full-range deterministic
  diagnostic and a human records `visual_review_status = approved` after
  judging it reasonable;
- a temporary RDS can be re-read and passes the same checks; then promote it
  atomically, record its checksum, and set the manifest state to `reviewed` or
  `admitted` only with maintainer approval.

Record missing, unreadable, or invalid records as intake source failures. They
are not quarantine spectra because they were never successfully ingested.

## Curate And Admit

Only after the exact cleaned RDS exists and passes the gate, apply
`openspecy-curate-class-reference`. Normalize terminal numeric file extensions,
add exact identities before narrow regex fallbacks, stop on duplicate keys or
regex clashes, and report exact/regex/unmatched coverage by dataset. Do not
preemptively edit `classes_reference.csv` or `classes_regex.csv` from filenames
that have not survived cleaning.

Set `state = admitted` only when cleaner/raw/output hashes, the approved plot,
license distinction, technique/units, and class coverage are complete. Run the
manifest intake tests and a small candidate probe. Apply
`openspecy-rebuild-reference-library` only after the maintainer freezes the
complete admitted inventory and authorizes the staged/full build.

## Handoff

Report per dataset: download/cleaned/admitted state, raw and cleaned counts,
duplicate/removal ledger, technique, intensity semantics, visual-review state,
`check_OpenSpecy()` result, license distinction, class coverage, hashes, and
any source failure. Explicitly name active read-only folders and confirm they
were not modified. Leave class edits and production rebuilding deferred when
their prerequisites or maintainer review are incomplete.
