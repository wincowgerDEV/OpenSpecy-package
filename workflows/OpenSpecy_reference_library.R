# Build the updatedv1 OpenSpecy reference libraries one dataset at a time.
#
# build_lib() owns source-local scientific processing. This driver admits only
# the reviewed manifest outputs, completes raw/derivative/nobaseline artifacts
# in each dataset's Processed folder, and performs a final no-filter join. It
# intentionally does not create medoids or models.

library(fs)

package_dir <- Sys.getenv(
  "OPENSPECY_PACKAGE_DIR", unset = normalizePath(getwd(), mustWork = FALSE)
)

setwd(package_dir)

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install 'devtools' before running the reference-library rebuild.",
       call. = FALSE)
}
if (!file_exists(file.path(package_dir, "DESCRIPTION"))) {
  stop("OpenSpecy package source directory does not exist: ", package_dir,
       call. = FALSE)
}
devtools::load_all(package_dir)
intake_helpers <- file.path(package_dir, "workflows", "reference_intake.R")
if (!file_exists(intake_helpers)) {
  stop("Reference intake helpers do not exist: ", intake_helpers,
       call. = FALSE)
}
sys.source(intake_helpers, envir = environment())
staging_helpers <- file.path(
  package_dir, "workflows", "reference_library_staging.R"
)
if (!file_exists(staging_helpers)) {
  stop("Reference staging helpers do not exist: ", staging_helpers,
       call. = FALSE)
}
sys.source(staging_helpers, envir = environment())

build_workers <- min(8L, parallelly::availableCores(omit = 1L))
options(
  OpenSpecy.build_workers = build_workers,
  future.globals.maxSize = 8 * 1024^3,
  OpenSpecy.correlation_cache = "bounded",
  OpenSpecy.source_hash_chunk_size = 20000L,
  OpenSpecy.source_record_staging = TRUE,
  OpenSpecy.source_prefilter_staging = TRUE,
  OpenSpecy.compact_staged_metadata = TRUE,
  OpenSpecy.source_merge_file_backed = TRUE,
  OpenSpecy.source_merge_chunk_size = 5000L,
  OpenSpecy.source_merge_gc_interval = 10L,
  OpenSpecy.metadata_join_chunk_size = 100000L,
  OpenSpecy.metadata_join_report_value_limit = 1000L,
  OpenSpecy.recipe_record_staging = TRUE,
  OpenSpecy.recipe_chunk_size = 5000L,
  OpenSpecy.staged_prune_query_block_size = 256L,
  OpenSpecy.staged_prune_candidate_block_size = 5000L,
  OpenSpecy.staged_completion_threshold = 250000L
)
data.table::setDTthreads(build_workers)

rawrefdata_dir <- Sys.getenv(
  "OPENSPECY_RAWREFDATA_DIR", unset = file.path(package_dir, "rawrefdata")
)
intake_manifest <- Sys.getenv(
  "OPENSPECY_REFERENCE_INTAKE_MANIFEST",
  unset = file.path(rawrefdata_dir, "updatedv1", "datasets.csv")
)
if (!file_exists(intake_manifest)) {
  stop("Reference intake manifest does not exist: ", intake_manifest,
       call. = FALSE)
}
intake <- reference_collect_admitted_inputs(
  rawrefdata_dir = rawrefdata_dir,
  manifest_path = intake_manifest,
  run_cleaners = identical(
    tolower(Sys.getenv("OPENSPECY_REFERENCE_RUN_CLEANERS", unset = "false")),
    "true"
  )
)
manifest <- reference_read_intake_manifest(intake_manifest, rawrefdata_dir)
admitted <- manifest[state == "admitted"]
if (!nrow(admitted) || !identical(admitted$output_path, intake$files)) {
  stop("Intake validation and admitted manifest order differ", call. = FALSE)
}
output_dir <- Sys.getenv(
  "OPENSPECY_REFERENCE_OUTPUT_DIR",
  unset = file.path(rawrefdata_dir, "updatedv1", "Processed",
                    "reference-library-overhaul")
)
comparator_dir <- Sys.getenv(
  "OPENSPECY_REFERENCE_PREVIOUS_LIBRARY_DIR",
  unset = file.path(
    dirname(package_dir),
    "reference-library-build-bounded-prune-reassignment-20261119-r1",
    "releases", "57da878d87b2"
  )
)
probe_evidence_path <- Sys.getenv(
  "OPENSPECY_REFERENCE_PROBE_EVIDENCE",
  unset = file.path(rawrefdata_dir, "updatedv1", "diagnostics", "probes",
                    "frozen_probe_evidence.rds")
)
intake_review_dir <- file.path(output_dir, "review")
if (!dir.exists(intake_review_dir) &&
    !dir.create(intake_review_dir, recursive = TRUE, showWarnings = FALSE)) {
  stop("Could not create intake review directory: ", intake_review_dir,
       call. = FALSE)
}
data.table::fwrite(
  intake$validation,
  file.path(intake_review_dir, "reference_intake_validation.csv")
)
data.table::fwrite(
  intake$source_failures,
  file.path(intake_review_dir, "reference_intake_source_failures.csv")
)
if (nrow(intake$validation) && any(!intake$validation$valid)) {
  stop("Admitted reference inputs failed validation: ",
       paste(intake$validation[
         valid == FALSE, paste0(slug, " (", problems, ")")
       ], collapse = "; "), call. = FALSE)
}

contract <- reference_freeze_overhaul_contract(
  admitted = admitted,
  manifest_path = intake_manifest,
  output_dir = output_dir,
  package_dir = package_dir,
  comparator_dir = comparator_dir,
  probe_evidence_path = probe_evidence_path
)

message("OpenSpecy staged reference-library overhaul")
message("  Manifest-admitted sources: ", nrow(admitted))
message("  Output root: ", output_dir)
message("  Previous-library comparator: ", comparator_dir)
message("  Frozen contract: ", contract$signature)
message("  Checkpoint reuse: enabled per dataset")
message("  Source-local correlation deduplication: strict > 0.99")
message("  Correlation cache: bounded blocks (no full square allocation)")
message("  Source hashing: bounded blocks of at most 20,000 spectra")
message(paste0(
  "  Source merge: path-backed two-pass staging with blocks of at most ",
  "5,000 retained spectra; temporary blocks collected every 10 chunks"
))
message("  Source prefilter: per-source hash/correlation before global merge")
message(paste0(
  "  Metadata: cross-source filters planned before retained-row binding; ",
  "blank fields dropped without a full-table copy; repeated values compacted"
))
message(paste0(
  "  Metadata joins: in-place/coalesced output columns in blocks of at most ",
  "100,000 rows; at most 1,000 representative diagnostic values with exact totals"
))
message("  Recipe materialization: HDF5-backed blocks of at most 5,000 spectra")
message("  Staged pruning: at most 256 queries by 5,000 candidates per block")
message(paste0(
  "  Dataset-local pruning: derivative/no-baseline ordinary prune_lib; ",
  "generic labels reassigned before ordinary removals; reviewed classes ",
  "below min_n are preserved"
))
message("  Final join: no cross-dataset removals or reassignment")
message("  Medoid/model stages: disabled")
message("  Stop threshold: 80% RAM, 75 GiB free disk, or 10 min no progress")
message("  Runtime stop threshold: twice the frozen representative estimate")
message("  High-throughput workers: ", build_workers)

dataset_manifests <- vector("list", nrow(admitted))
for (i in seq_len(nrow(admitted))) {
  message(sprintf(
    "Dataset %d/%d: %s", i, nrow(admitted), admitted$slug[[i]]
  ))
  dataset_manifests[[i]] <- reference_build_dataset(
    input_path = admitted$output_path[[i]],
    slug = admitted$slug[[i]],
    workflow_data = file.path(package_dir, "workflows", "data"),
    build_fun = build_lib,
    source_checksum = admitted$output_sha256[[i]],
    source_spectra = intake$validation[
      match(admitted$slug[[i]], slug), spectra
    ],
    progress = TRUE
  )
  gc(verbose = FALSE, full = TRUE)
}

message("All dataset-local builds complete; validating durable manifests")
dataset_manifests <- lapply(seq_len(nrow(admitted)), function(i) {
  reference_read_dataset_manifest(
    input_path = admitted$output_path[[i]],
    slug = admitted$slug[[i]],
    verify_hashes = TRUE
  )
})

message("Joining completed dataset artifacts without cross-dataset filtering")
reference_library_build <- reference_join_dataset_manifests(
  dataset_manifests, output_dir = output_dir, progress = TRUE
)
release_dir <- dirname(reference_library_build$libraries$path[[1L]])
message("Reference-library overhaul complete: ", release_dir)
message("Release index: ", file.path(release_dir, "reference_library_build.rds"))
message("Raw library: ", reference_library_build$libraries[recipe == "raw", path])
message("Derivative library: ",
        reference_library_build$libraries[recipe == "derivative", path])
message("No-baseline library: ",
        reference_library_build$libraries[recipe == "nobaseline", path])
