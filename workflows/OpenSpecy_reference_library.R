# Rebuild the official Open Specy reference-library artifacts.
#
# build_lib() owns source discovery, curated metadata joins, class completion,
# pruning, one-off filters, medoid/model creation, resumable checkpoints,
# complete legacy comparisons, assessments, and versioned output promotion.
# Override its environment-aware path defaults with OPENSPECY_LIBRARY_DATA,
# OPENSPECY_SOURCE_FILE, OPENSPECY_PROCESSED_DIR, or
# OPENSPECY_LIBRARY_OUTPUT when the official files are elsewhere.

library(OpenSpecy)
library(fs)

data_dir <- Sys.getenv(
  "OPENSPECY_LIBRARY_DATA",
  unset = "H:\\My Drive\\Work\\Projects\\OpenSpecy\\SpectraFilesCodeProcessedSpectra"
)
processed_dir <- Sys.getenv("OPENSPECY_PROCESSED_DIR", unset = data_dir)
output_dir <- Sys.getenv(
  "OPENSPECY_LIBRARY_OUTPUT",
  unset = paste0(
    "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\",
    "reference-library-build"
  )
)

metadatafiles <- dir_ls(
  path = processed_dir,
  recurse = TRUE,
  regexp = "/Processed/.*\\.rds$",
  fail = FALSE
)

source_file <- Sys.getenv(
  "OPENSPECY_SOURCE_FILE",
  unset = file.path(data_dir, "library_raw.rds")
)
files <- c(metadatafiles, source_file)
if (!length(files) || any(!file.exists(files))) {
  stop("Every source path supplied to build_lib() must exist", call. = FALSE)
}

reference_library_build <- build_lib(
  x = files,
  output_dir = output_dir,
  reuse = F
)
