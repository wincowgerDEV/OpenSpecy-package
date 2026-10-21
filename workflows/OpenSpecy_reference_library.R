# Rebuild the official Open Specy reference-library artifacts.
#
# build_lib() owns source discovery, curated metadata joins, class completion,
# pruning, one-off filters, medoid/model creation, resumable checkpoints,
# complete legacy comparisons, assessments, and versioned output promotion.
# Paths can be overridden without editing this production driver.

library(fs)

package_dir <- Sys.getenv(
  "OPENSPECY_PACKAGE_DIR", unset = normalizePath(getwd(), mustWork = FALSE)
)

setwd(package_dir)

data_dir <- Sys.getenv(
  "OPENSPECY_REFERENCE_SOURCE_DIR",
  unset = "H:\\My Drive\\Work\\Projects\\OpenSpecy\\SpectraFilesCodeProcessedSpectra"
)

processed_dir <- data_dir

output_dir <- Sys.getenv(
  "OPENSPECY_REFERENCE_OUTPUT_DIR",
  unset = file.path(dirname(package_dir), "reference-library-build-2.0.0")
)

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

build_workers <- min(8L, parallelly::availableCores(omit = 1L))
options(
  OpenSpecy.build_workers = build_workers,
  future.globals.maxSize = 8 * 1024^3
)
data.table::setDTthreads(build_workers)

if (!dir_exists(processed_dir)) {
  stop("Processed source directory does not exist: ", processed_dir,
       call. = FALSE)
}

processed_files_env <- Sys.getenv(
  "OPENSPECY_REFERENCE_PROCESSED_FILES", unset = ""
)
if (nzchar(processed_files_env)) {
  metadatafiles <- trimws(strsplit(
    processed_files_env, .Platform$path.sep, fixed = TRUE
  )[[1L]])
  metadatafiles <- sort(unique(metadatafiles[nzchar(metadatafiles)]))
  invalid_processed <- !file_exists(metadatafiles) |
    tolower(path_ext(metadatafiles)) != "rds" |
    path_file(path_dir(metadatafiles)) != "Processed"
  if (!length(metadatafiles) || any(invalid_processed)) {
    stop(
      "OPENSPECY_REFERENCE_PROCESSED_FILES must contain existing RDS files ",
      "whose parent directory is Processed.", call. = FALSE
    )
  }
  message("  Processed source discovery: explicit validated file list")
} else {
  metadatafiles <- dir_ls(
    path = processed_dir,
    recurse = TRUE,
    type = "file",
    glob = "*.rds",
    fail = FALSE
  )
  metadatafiles <- sort(metadatafiles[
    path_file(path_dir(metadatafiles)) == "Processed"
  ])
}

rawrefdata_dir <- Sys.getenv(
  "OPENSPECY_RAWREFDATA_DIR", unset = file.path(package_dir, "rawrefdata")
)
intake_manifest <- Sys.getenv(
  "OPENSPECY_REFERENCE_INTAKE_MANIFEST",
  unset = file.path(rawrefdata_dir, "updatedv1", "datasets.csv")
)
intake <- list(
  files = character(), validation = data.table::data.table(),
  source_failures = data.table::data.table()
)
if (file_exists(intake_manifest)) {
  intake <- reference_collect_admitted_inputs(
    rawrefdata_dir = rawrefdata_dir,
    manifest_path = intake_manifest,
    run_cleaners = identical(
      tolower(Sys.getenv("OPENSPECY_REFERENCE_RUN_CLEANERS", unset = "false")),
      "true"
    )
  )
  metadatafiles <- sort(unique(c(metadatafiles, intake$files)))
}
intake_review_dir <- file.path(output_dir, "review")
dir_create(intake_review_dir, recurse = TRUE)
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

source_file <- file.path(data_dir, "library_raw.rds")
if (!file_exists(source_file)) {
  stop("Raw source library does not exist: ", source_file, call. = FALSE)
}
if (!length(metadatafiles)) {
  stop("No processed RDS sources were found below: ", processed_dir,
       call. = FALSE)
}
files <- unique(c(metadatafiles, source_file))

message("OpenSpecy reference-library full rebuild")
message("  Processed sources: ", length(metadatafiles))
message("  Manifest-admitted sources: ", length(intake$files))
message("  Raw source: ", source_file)
message("  Output root: ", output_dir)
message("  Checkpoint reuse: disabled (clean full rebuild)")
message("  Cross-class 0.9 closure: disabled")
message("  Classification model: flat guardrailed logistic regression")
message("  High-throughput workers: ", build_workers)

reference_library_build <- build_lib(
  x = files,
  output_dir = output_dir,
  previous_library_dir = "system",
  reuse = FALSE,
  remove_other = TRUE,
  prune = list(
    derivative = list(cross_class = FALSE),
    nobaseline = list(cross_class = FALSE)
  ),
  model_method = "logistic_regression",
  progress = TRUE
)

release_dir <- attr(reference_library_build, "output_dir")
message("Reference-library build complete: ", release_dir)
message(
  "Release index: ",
  file.path(release_dir, "reference_library_build.rds")
)
message("Assessments: ", file.path(release_dir, "assessments.rds"))
message(
  "Quarantined spectra: ",
  file.path(release_dir, "quarantined_spectra.rds")
)
