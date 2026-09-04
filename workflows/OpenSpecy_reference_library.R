# Rebuild the official Open Specy reference-library artifacts.
#
# build_lib() owns source discovery, curated metadata joins, class completion,
# pruning, one-off filters, medoid/model creation, resumable checkpoints,
# complete legacy comparisons, assessments, and versioned output promotion.
# Maintainer paths for the current package source and official full rebuild.

library(fs)

package_dir <- paste0(
  "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\",
  "OpenSpecy-package"
)
data_dir <- "H:\\My Drive\\Work\\Projects\\OpenSpecy\\SpectraFilesCodeProcessedSpectra"

processed_dir <- data_dir

output_dir <- paste0(
  "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\",
  "reference-library-build"
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

if (!dir_exists(processed_dir)) {
  stop("Processed source directory does not exist: ", processed_dir,
       call. = FALSE)
}

metadatafiles <- sort(dir_ls(
  path = processed_dir,
  recurse = TRUE,
  regexp = "/Processed/.*\\.rds$",
  fail = FALSE
))

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
message("  Raw source: ", source_file)
message("  Output root: ", output_dir)
message("  Checkpoint reuse: enabled (manifest-compatible stages only)")

reference_library_build <- build_lib(
  x = files,
  output_dir = output_dir,
  previous_library_dir = "system",
  reuse = TRUE,
  remove_other = TRUE,
  progress = TRUE
)

release_dir <- attr(reference_library_build, "output_dir")
message("Reference-library build complete: ", release_dir)
message(
  "Aggregate object: ",
  file.path(release_dir, "reference_library_build.rds")
)
