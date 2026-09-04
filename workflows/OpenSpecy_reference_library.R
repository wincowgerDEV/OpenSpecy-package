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

build_file <- paste0(
  "C:\\Users\\winco\\OneDrive\\Documents\\OpenSpecy_offline\\reference-library-rebuild-na-support-20260902\\releases\\9cfc00e59556\\reference_library_build.rds"
)

reference_library_build <- readRDS(build_file)

der_ftir <- reference_library_build$libraries$derivative$ftir

der_ram_mod <- reference_library_build$models$logistic_regression$derivative$raman

sample_spec(der_ftir, 5) |> plot(offset = 1)

library(dplyr)
lib_type_mats <- der_ftir$metadata |>
  dplyr::group_by(organization,material, material_class) |>
  summarise(count = n())


lib_type_mats <- der_ftir$metadata |>
  dplyr::filter(is.na(spectrum_identity)) |>
  dplyr::group_by(organization, spectrum_identity, material, material_class) #|>
  summarise(count = n())

get_lib("raw")

raw_lib <- load_lib("raw")
raw_mat <- raw_lib$metadata |>
  dplyr::filter(is.na(spectrum_identity)) 


names(reference_library_build)
names(reference_library_build$assessments)
