# Rebuild the official Open Specy reference-library artifacts.
#
# build_lib() owns source discovery, curated metadata joins, class completion,
# pruning, one-off filters, medoid/model creation, resumable checkpoints,
# complete legacy comparisons, assessments, and versioned output promotion.
# Maintainer paths for the current package source and official full rebuild.

library(fs)

package_dir <- paste0(
  "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\",
  "OpenSpecy-package")

setwd(package_dir)

data_dir <- "H:\\My Drive\\Work\\Projects\\OpenSpecy\\SpectraFilesCodeProcessedSpectra"

processed_dir <- data_dir

output_dir <- paste0(
  "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\",
  "reference-library-build-2.0.0"
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

build_workers <- min(8L, parallelly::availableCores(omit = 1L))
options(
  OpenSpecy.build_workers = build_workers,
  future.globals.maxSize = 8 * 1024^3
)
data.table::setDTthreads(build_workers)
previous_future_plan <- future::plan()
doFuture::registerDoFuture()
future::plan(future::multisession, workers = min(5L, build_workers))

if (!dir_exists(processed_dir)) {
  stop("Processed source directory does not exist: ", processed_dir,
       call. = FALSE)
}

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
message("  Checkpoint reuse: disabled (clean full rebuild)")
message("  High-throughput workers: ", build_workers)

reference_library_build <- tryCatch(
  build_lib(
    x = files,
    output_dir = output_dir,
    previous_library_dir = "system",
    reuse = FALSE,
    remove_other = TRUE,
    progress = TRUE
  ),
  finally = {
    future::plan(previous_future_plan)
    foreach::registerDoSEQ()
  }
)

release_dir <- attr(reference_library_build, "output_dir")
message("Reference-library build complete: ", release_dir)
message(
  "Release index: ",
  file.path(release_dir, "reference_library_build.rds")
)
message("Assessments: ", file.path(release_dir, "assessments.rds"))
