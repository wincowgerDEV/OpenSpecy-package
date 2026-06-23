# Rebuild the official Open Specy reference-library artifacts.
#
# The large source object stays outside the repository. Set
# OPENSPECY_LIBRARY_DATA when it is not in the package parent directory.
# All lookup and exclusion tables used below are version controlled in
# workflows/data/.

library(OpenSpecy)

data_dir <- Sys.getenv(
  "OPENSPECY_LIBRARY_DATA",
  unset = normalizePath("..", mustWork = TRUE)
)
source_file <- Sys.getenv(
  "OPENSPECY_SOURCE_FILE",
  unset = file.path(data_dir, "library_raw.rds")
)
output_dir <- Sys.getenv(
  "OPENSPECY_LIBRARY_OUTPUT",
  unset = file.path(data_dir, "reference-library-build")
)
workflow_data <- file.path("workflows", "data")

classes_reference <- data.table::fread(
  file.path(workflow_data, "classes_reference.csv")
)
library_types <- data.table::fread(
  file.path(workflow_data, "library_types.csv")
)
material_hierarchy <- data.table::fread(
  file.path(workflow_data, "material_hierarchy.csv")
)
known_bad_ids <- data.table::fread(
  file.path(workflow_data, "known_bad_ids.csv")
)

libraries <- build_lib(
  source_file,
  restrict_range_args = list(
    min = c(100, 2420),
    max = c(2200, 11994)
  ),
  exclude_ids = known_bad_ids$sample_name,
  metadata_lookups = list(classes_reference, library_types),
  material_hierarchy = material_hierarchy,
  clean_metadata_values = TRUE,
  assess = TRUE
)

keep <- !is.na(libraries$raw$metadata$material_type) &
  !grepl(
    paste0(
      "(6_f12)|(6_c8)|(7_b1)|(6_e5)|(7_c7)|(7_e6)|(7_c9)|",
      "(7_g6)|(7_c4)|(7_a8)|(6_h4)|(6_g5)"
    ),
    libraries$raw$metadata$spectrumid,
    ignore.case = TRUE
  )
keep[is.na(keep)] <- TRUE

libraries <- lapply(libraries, filter_spec, logic = keep)
processed_types <- c("derivative", "nobaseline")
libraries[processed_types] <- lapply(libraries[processed_types], \(x) {
  x$spectra <- round(x$spectra, 3)
  x
})

medoid_ids <- lapply(
  libraries[processed_types],
  reduce_lib,
  group_cols = c("spectrum_type", "organization", "material_class"),
  k = 50,
  min_n = 50,
  return = "ids"
)
medoid_libraries <- Map(
  filter_spec,
  libraries[processed_types],
  medoid_ids
)
names(medoid_libraries) <- paste0("medoid_", names(medoid_libraries))

model_inputs <- lapply(
  medoid_libraries,
  restrict_range,
  min = 800,
  max = 3200,
  make_rel = FALSE
)
model_libraries <- lapply(model_inputs, \(x) {
  model_sources <- list(
    both = x,
    ftir = filter_spec(x, x$metadata$spectrum_type == "ftir"),
    raman = filter_spec(x, x$metadata$spectrum_type == "raman")
  )
  lapply(model_sources, build_model_lib)
})
names(model_libraries) <- sub("^medoid_", "model_", names(model_libraries))

artifacts <- c(libraries, medoid_libraries)
assessment <- data.table::rbindlist(
  lapply(
    artifacts,
    assess_lib,
    class_col = "material_class",
    nearest = FALSE
  ),
  idcol = "artifact"
)
print(assessment)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
invisible(Map(
  write_spec,
  artifacts,
  file.path(output_dir, paste0(names(artifacts), ".rds"))
))
invisible(Map(
  saveRDS,
  model_libraries,
  file.path(output_dir, paste0(names(model_libraries), ".rds"))
))
data.table::fwrite(
  assessment,
  file.path(output_dir, "library_assessment.csv")
)

