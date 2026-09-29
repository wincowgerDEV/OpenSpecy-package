# Feature Plan: Reference Metadata Enrichment

**Feature dir**: `specs/037-reference-metadata-enrichment`  
**Date**: 2026-09-29  
**Review budget**: Under 100 nonblank lines and approximately 1,500 words.  
**Current tranche**: Enrich official `build_lib()` reference-library metadata with standardized `material_form` and evidence-backed `common_use` values from two curated workflow tables.  
**Change class**: Package/scientific; spectral values are unchanged, but published reference metadata and its interpretation change.

## Goal

- Add auditable, conservative `material_form` and `common_use` metadata to every official full and derived reference-library object.
- Use all available source metadata for form evidence while leaving genuinely unsupported or conflicting classifications as `NA`.

## Scope

- **In**: controlled form vocabulary and regex table; one rowwise metadata search document; a material-class use table with quantitative or cited qualitative evidence; exact SBR/EPDM and binder-specific paint taxonomy curation; official-build joins, reports, documentation, tests, and staged reference validation.
- **Out**: inferring a form or class from spectral intensities, online lookups during a build, forcing a label when evidence is insufficient, broad tire/gasket/paint guesses without explicit metadata support, app UI changes, and hand-editing generated docs or built libraries.
- **Users**: maintainers running official `build_lib(..., output_dir = ...)` and downstream users reading its `OpenSpecy$metadata`.

## Requirements

- R1. Add required `workflows/data/material_form_regex.csv` with unique nonblank `pattern` and canonical `material_form`. Seed a documented vocabulary including `paint`, `rubber`, `hard plastic`, `fiber`, `pellet`, and `film plastic`; add only reviewed categories such as foam or sphere/bead. Regexes use token/context boundaries and audited synonyms, not unrestricted substrings.
- R2. After name/value cleaning but before metadata columns are dropped, create one normalized search document per metadata row from every atomic metadata value (including `spectrum_identity`, existing `material_form`, aliases already coalesced into canonical columns, material labels, notes, citations, and source-specific fields). Concatenate once with unambiguous separators; do not rebuild the corpus for each pattern.
- R3. Match an existing `material_form` value first when it resolves uniquely; otherwise match the full-row document. Multiple patterns yielding the same form are valid. Matches yielding different forms leave `material_form = NA` and enter a clash report; no match also yields `NA`. The standardized output replaces free-text `material_form`, while audit rows retain the triggering evidence for review.
- R4. Add required `workflows/data/common_use_reference.csv`, keyed uniquely to every current `material_class`, with `common_use`, optional mass shares, basis year/geography, `source_url`, `retrieved_date`, and `evidence_notes`. Allowed output is `consumer`, `industrial`, `mixed`, or `NA`; supplied shares are proportions in `[0,1]` and may not sum above one.
- R5. Define `common_use` as the predominant **ultimate end-market**, not whether resin manufacture occurs at an industrial site. Prefer global mass shares: quantitative `consumer` or `industrial` requires that side above 50%, while quantitative `mixed` requires each side at least 25%, at least 75% classified, and neither above 50%. Without shares, allow a cited qualitative proposal when a source describes main or common applications. Direct individual endpoints include passenger tires, household goods, clothing, and patient-facing healthcare; substantial individual and industrial endpoints are `mixed`. Join after final class completion and pruning/reassignment.
- R6. Source hierarchy: use reproducible global polymer-by-application mass data where available, initially the [OECD Plastics Use 1990–2019 dataset](https://data-explorer.oecd.org/vis?df%5Bag%5D=OECD.ENV.EEI&df%5Bds%5D=dsDisseminateFinalDMZ&df%5Bid%5D=DSD_PU%40DF_PU&df%5Bvs%5D=1.0&lc=en); otherwise use material-specific technical, trade, government, manufacturer, scholarly, or general references that explicitly describe applications. Record qualitative status in the notes and leave shares blank. Catch-all or unsupported classes stay `NA`.
- R7. Validate both tables at load time and emit form coverage/clash and common-use coverage/source reports in official build assessments. Reports include populated, unmatched, conflicting, and by-category counts without changing spectrum identifiers.
- R8. All final full libraries and medoids retain the two columns. Wavenumbers, intensities, spectrum/metadata order, `sample_name`, `col_id`, class labels, and existing object attributes remain identical apart from the documented enrichment reports/signatures.
- R9. No new export or `build_lib()` argument is added. Composable/custom builds keep their present caller-supplied lookup behavior; curated automatic enrichment belongs to the official workflow-data path.
- R10. Curate exact identities and hierarchy rows so explicit SBR and reviewed tire identities resolve to class `styrene-butadiene rubber`, while explicit EPDM identities resolve to `ethylene-propylene-diene-monomer rubber`. Split paint into `acrylic paint` or `urethane paint` only when the binder is explicit; generic/unknown paint remains `paint`. Regex fallback may cover explicit chemistry terms but must not broadly infer tire or gasket composition.

## Technical Decisions

- **Research finding**: no ready-made universal key was found. OECD offers global polymer × application mass estimates but only broad polymer groups. [EPA CDR](https://www.epa.gov/chemical-data-reporting/access-cdr-data) uses U.S. regulatory stages and excludes many polymers. The checked-in CSV is therefore a professional-review draft: retain quantitative rows where defensible, and prefill other classes from cited predominant-use descriptions without presenting qualitative judgments as measured shares.
- **Form taxonomy**: align vocabulary where practical with NOAA categories (fiber, fragment, film, foam, pellet, bead) while retaining requested project labels and reviewed paint/rubber categories; NOAA itself notes that reporting methods are not fully harmonized ([NOAA overview](https://www.ncei.noaa.gov/products/microplastics)).
- **Approach**: curate exact identities before narrow regex fallback, then join the hierarchy. Add internal table validators and enrichment helpers in `R/build_lib.R`; run form inference on pre-drop cleaned metadata, then attach use only after final `material_class`. Add the enrichment files to workflow discovery, signatures, checkpoints, progress, and assessments while preserving core preprocessing reuse when only enrichment tables change.
- **Public API/dependencies**: no new API or dependency; use base R and `data.table`. `common_use` becomes a documented recommended metadata field in `R/as_OpenSpecy.R`.
- **OpenSpecy/generated artifacts**: only aligned metadata columns and report attributes change. Edit roxygen sources, use configured roxygen2 8.0.0, run `devtools::document()`, and inspect generated `man/*.Rd`/`NAMESPACE`; never edit them directly.
- **External resources**: curation-time HTTPS only; official builds are offline and deterministic. Every populated row records a source URL, retrieval date, and evidence note; quantitative rows additionally record year, geography, aggregation, and crosswalk assumptions.
- **Reference compatibility**: compare old/new artifacts by IDs, axes, spectral hashes, row counts, class/type labels, warnings, and representative joins; expected differences are the two columns, enrichment reports, and signatures/checkpoints.
- **Performance**: build the row corpus once and vectorize pattern evaluation. Benchmark 25,000 rows × 50 metadata columns × up to 75 patterns; target ≤10 seconds and <500 MiB incremental memory on the maintainer machine. Stop a production rebuild if enrichment exceeds 2× the measured kernel budget or 1 GiB, isolate the pattern/corpus bottleneck, then restart from the preserved core checkpoint.
- **Bundled Shiny/pipeline diagram**: N/A; no `inst/shiny/` behavior or `.specify/memory/pipeline-diagram.html` component changes.
- **Hosted Shinylive/WebAssembly**: shared `R/` and vignette inputs change, so run fast `-HostedAppStatic`. No route, interaction, dependency, image, driver, pin, or staged-library contract changes; matching-artifact smoke and clean wasm rebuild are not triggered.

## Package Surfaces

- `R/build_lib.R`, `R/as_OpenSpecy.R`, `R/zzz.R`: table discovery/validation, internal enrichment, assessments, progress, signatures, and metadata documentation/global variables.
- `workflows/data/{material_form_regex,common_use_reference,classes_reference,classes_regex,material_hierarchy}.csv`: new enrichment inputs plus reviewed taxonomy updates. `tests/testthat/test-build_lib.R`: focused behavior, table-contract, ambiguity, taxonomy, and end-to-end metadata tests.
- `benchmarks/reference_metadata_enrichment.R`: representative corpus/pattern timing and memory evidence; N/A for old/new same-output comparison because this is new output.
- `vignettes/library-builder.Rmd`, `NEWS.md`: vocabulary, inference order, missingness, common-use meaning, source limits, and audit workflow. `DESCRIPTION`: unchanged. Generated docs follow roxygen.
- `.github/workflows/`, `inst/`, `site/`, `README.md`: unchanged. Hosted impact is the required fast static gate only.

## Work Checklist

- [x] Curate and validate both enrichment CSVs and the SBR/EPDM/paint exact, regex, and hierarchy rows, including source/version notes (`workflows/data/`).
- [x] Implement ordered enrichment, table discovery/signatures, audit reports, and progress without changing public arguments (`R/build_lib.R`, `R/zzz.R`).
- [x] Add focused and official-workflow fixtures for every match/conflict/missingness/class-reassignment contract (`tests/testthat/test-build_lib.R`).
- [x] Document fields, semantics, limitations, provenance, and review workflow; regenerate and inspect generated docs (`R/as_OpenSpecy.R`, `R/build_lib.R`, `vignettes/library-builder.Rmd`, `NEWS.md`).
- [x] Run the representative benchmark, subset artifact comparison, full affected package gates once on the final candidate, and fast hosted-static verification.
- [x] Reconcile every checkbox with evidence; record deferred production/CI gates, inspect owned processes and `git status`, and remove task-created scratch files.

## Verification

- **Direct/focused**: Windows Rscript preflight; parse changed R; `devtools::test(filter = "build_lib", reporter = "check", stop_on_failure = TRUE)`. Cover explicit-form precedence, clashes, unsupported classes, complete quantitative shares and thresholds, qualitative source provenance, and post-prune joining.
- **Table/reference audit**: require every hierarchy class exactly once in the use table; reject duplicate/invalid regexes, invalid labels/shares, stale keys, and undocumented non-`NA` decisions. Audit exact-before-regex coverage and clashes for SBR, EPDM, tires, gaskets, and binder-specific versus generic paints. Demonstrate polyethylene and PTFE decisions from their own evidence rather than the OECD `Other` aggregate.
- **Docs/broad gates**: roxygen2 8.0.0 `devtools::document()` plus generated-diff audit; render `vignettes/library-builder.Rmd`; one final `devtools::test()`; run `openspecy-run-quality-gates` with `-HostedAppStatic`. Defer `devtools::check()` to release/CRAN review unless broader failures or metadata changes trigger it.
- **Reference/long workflow**: run the synthetic kernel benchmark, focused source fixtures, and read-only probes against current packaged medoid libraries; compare IDs, axes, metadata rows/names, warnings, class joins, enrichment coverage, and spectra hashes. Do not run the full production `build_lib`; promotion remains manual unless explicitly requested.
- **Reusable evidence/closure**: reuse passing focused, documentation, full-test, and hosted-static evidence only while their files, fixtures, dependencies, and contracts are unchanged; finish with checklist, process, `git status`, and scratch audits.

## Risks And Open Questions

- `consumer` versus `industrial` is not a universal ontology: packaging and clothing are usually consumer-facing, while construction/machinery are industrial/infrastructure, but electronics, transport, institutional products, and “other” can be mixed. The crosswalk and unresolved mass must remain visible and reviewable.
- `mixed` does not mean equal use. Quantitative rows expose their shares; qualitative rows are visibly proposed classifications for professional review and must not imply measured market fractions.
- Full-row matching increases recall but can match citations or organization names; boundary rules, direct-field precedence, clash-to-`NA`, and the audit report are required safeguards.

## Approval Notes

- Approved by: maintainer request; `mixed` accepted for common use.
- Follow-up: maintainer directed SBR to `mixed` and approved cited qualitative evidence to prefill the review table. Full production `build_lib()` and `R CMD check` remain deferred; current packaged medoids are probed read-only.
