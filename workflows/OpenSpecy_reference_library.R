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
    min = c(100, 2420.000001),
    max = c(2199.999999, 11994)
  ),
  exclude_ids = known_bad_ids$sample_name,
  metadata_lookups = list(classes_reference, library_types),
  material_hierarchy = material_hierarchy
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

raw <- filter_spec(libraries$raw, keep)
derivative <- filter_spec(libraries$derivative, keep)
nobaseline <- filter_spec(libraries$nobaseline, keep)
derivative$spectra <- round(derivative$spectra, 3)
nobaseline$spectra <- round(nobaseline$spectra, 3)

derivative_ids <- reduce_lib(
  derivative,
  group_cols = c("spectrum_type", "organization", "material_class"),
  k = 50,
  min_n = 50,
  return = "ids"
)
nobaseline_ids <- reduce_lib(
  nobaseline,
  group_cols = c("spectrum_type", "organization", "material_class"),
  k = 50,
  min_n = 50,
  return = "ids"
)

medoid_derivative <- filter_spec(derivative, derivative_ids)
medoid_nobaseline <- filter_spec(nobaseline, nobaseline_ids)

model_derivative_input <- restrict_range(
  medoid_derivative,
  min = 800,
  max = 3200,
  make_rel = FALSE
)
model_nobaseline_input <- restrict_range(
  medoid_nobaseline,
  min = 800,
  max = 3200,
  make_rel = FALSE
)

model_derivative <- list(
  both = build_model_lib(model_derivative_input),
  ftir = build_model_lib(filter_spec(
    model_derivative_input,
    model_derivative_input$metadata$spectrum_type == "ftir"
  )),
  raman = build_model_lib(filter_spec(
    model_derivative_input,
    model_derivative_input$metadata$spectrum_type == "raman"
  ))
)
model_nobaseline <- list(
  both = build_model_lib(model_nobaseline_input),
  ftir = build_model_lib(filter_spec(
    model_nobaseline_input,
    model_nobaseline_input$metadata$spectrum_type == "ftir"
  )),
  raman = build_model_lib(filter_spec(
    model_nobaseline_input,
    model_nobaseline_input$metadata$spectrum_type == "raman"
  ))
)

assessment <- data.table::rbindlist(list(
  raw = assess_lib(raw, class_col = "material_class", nearest = FALSE),
  derivative = assess_lib(
    derivative, class_col = "material_class", nearest = FALSE
  ),
  nobaseline = assess_lib(
    nobaseline, class_col = "material_class", nearest = FALSE
  ),
  medoid_derivative = assess_lib(
    medoid_derivative, class_col = "material_class", nearest = FALSE
  ),
  medoid_nobaseline = assess_lib(
    medoid_nobaseline, class_col = "material_class", nearest = FALSE
  )
), idcol = "artifact")
print(assessment)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_spec(raw, file.path(output_dir, "raw.rds"))
write_spec(derivative, file.path(output_dir, "derivative.rds"))
write_spec(nobaseline, file.path(output_dir, "nobaseline.rds"))
write_spec(
  medoid_derivative,
  file.path(output_dir, "medoid_derivative.rds")
)
write_spec(
  medoid_nobaseline,
  file.path(output_dir, "medoid_nobaseline.rds")
)
saveRDS(model_derivative, file.path(output_dir, "model_derivative.rds"))
saveRDS(model_nobaseline, file.path(output_dir, "model_nobaseline.rds"))
data.table::fwrite(
  assessment,
  file.path(output_dir, "library_assessment.csv")
)
