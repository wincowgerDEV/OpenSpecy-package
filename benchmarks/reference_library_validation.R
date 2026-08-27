# Manual validation of a saved full reference-library build. Set
# OPENSPECY_SAVED_LIBRARIES to a libraries.rds path before running. This script
# is intentionally outside tests because the artifact is large and external.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

path <- Sys.getenv("OPENSPECY_SAVED_LIBRARIES")
if (!nzchar(path) || !file.exists(path)) {
  stop("Set OPENSPECY_SAVED_LIBRARIES to an existing libraries.rds")
}

libraries <- readRDS(path)
stopifnot(identical(names(libraries), c("raw", "derivative", "nobaseline")))
valid <- vapply(libraries, check_OpenSpecy, logical(1))
stopifnot(all(valid))
stopifnot(all(vapply(libraries[-1L], function(x) {
  identical(x$wavenumber, libraries$raw$wavenumber)
}, logical(1))))
stopifnot(all(vapply(libraries[-1L], function(x) {
  identical(colnames(x$spectra), colnames(libraries$raw$spectra))
}, logical(1))))

workflow_data <- file.path("workflows", "data")
classes <- data.table::fread(file.path(workflow_data, "classes_reference.csv"))
classes_regex <- data.table::fread(
  file.path(workflow_data, "classes_regex.csv")
)
classes_exact <- classes[
  !is.na(material) & nzchar(material), .(spectrum_identity, material)
]
types <- data.table::fread(file.path(workflow_data, "library_types.csv"))
hierarchy <- data.table::fread(file.path(workflow_data, "material_hierarchy.csv"))
drops <- data.table::fread(
  file.path(workflow_data, "metadata_drop_columns.csv")
)

raw <- libraries$raw
raw$metadata <- lib_clean_metadata(raw$metadata, clean_values = TRUE)
raw$metadata$spectrum_identity <- OpenSpecy:::.lib_clean_spectrum_identity(
  raw$metadata$spectrum_identity
)
before <- data.table::data.table(
  populated_identity = sum(!is.na(raw$metadata$spectrum_identity)),
  populated_material = sum(!is.na(raw$metadata$material)),
  populated_class = sum(!is.na(raw$metadata$material_class)),
  populated_library_type = sum(!is.na(raw$metadata$library_type))
)

coalesce <- intersect(
  names(raw$metadata), setdiff(names(classes_exact), "spectrum_identity")
)
joined <- suppressWarnings(join_lib_metadata(
  raw, classes_exact, by = "spectrum_identity"
))
joined$metadata <- OpenSpecy:::.lib_coalesce_joined_metadata(
  joined$metadata, coalesce
)
source_blank <- is.na(joined$metadata$organization) |
  !nzchar(joined$metadata$organization)
source_fallback <- !is.na(joined$metadata$user_name) &
  nzchar(joined$metadata$user_name)
joined$metadata$organization[source_blank & source_fallback] <-
  joined$metadata$user_name[source_blank & source_fallback]
coalesce <- intersect(names(joined$metadata),
                      setdiff(names(types), "organization"))
joined <- suppressWarnings(join_lib_metadata(
  joined, types, by = "organization"
))
joined$metadata <- OpenSpecy:::.lib_coalesce_joined_metadata(
  joined$metadata, coalesce, lookup_precedence = FALSE
)
stopifnot(
  all(!is.na(joined$metadata$library_type) &
        nzchar(joined$metadata$library_type)),
  all(!is.na(joined$metadata$spectrum_type) &
        nzchar(joined$metadata$spectrum_type))
)
class_prediction <- predict_class_reference(
  joined$metadata, classes_regex, return = "report"
)
print(class_prediction$summary)
stopifnot(nrow(class_prediction$clashes) == 0L)
joined$metadata <- class_prediction$data
joined <- suppressWarnings(join_material_hierarchy(joined, hierarchy))

after_lookup <- data.table::data.table(
  populated_identity = sum(!is.na(joined$metadata$spectrum_identity)),
  populated_material = sum(!is.na(joined$metadata$material)),
  populated_class = sum(!is.na(joined$metadata$material_class)),
  populated_library_type = sum(!is.na(joined$metadata$library_type))
)
joined <- OpenSpecy:::.lib_complete_reference_classes(
  joined, classes, hierarchy
)
after_completion <- data.table::data.table(
  populated_identity = sum(!is.na(joined$metadata$spectrum_identity)),
  populated_material = sum(!is.na(joined$metadata$material)),
  populated_class = sum(!is.na(joined$metadata$material_class)),
  populated_library_type = sum(!is.na(joined$metadata$library_type))
)
coverage <- data.table::rbindlist(
  list(before = before, after_lookup = after_lookup,
       after_completion = after_completion),
  idcol = "stage"
)
print(coverage)
print(attr(joined, "class_coverage_report"))
print(joined$metadata[
  class_assignment_reason == "unresolved_identity",
  .N,
  by = .(organization, user_name, spectrum_identity)
][order(-N, spectrum_identity)][1:min(.N, 100L)], nrows = 100L)
stopifnot(after_completion$populated_material >= before$populated_material)
stopifnot(after_completion$populated_class == nrow(joined$metadata))
stopifnot(after_completion$populated_library_type >=
            before$populated_library_type)

print(joined$metadata[
  material_class %in% c("polyamides", "polyacrylamides", "polyesters"),
  .N,
  by = material_class
][order(material_class)])

optional_drop <- grepl("^assessment_", drops$metadata_column)
drop_report <- data.table::data.table(
  metadata_column = drops$metadata_column,
  status = ifelse(
    drops$metadata_column %in% names(joined$metadata),
    "present",
    ifelse(optional_drop, "optional_absent", "stale_absent")
  )
)
print(drop_report[, .N, by = status])

# Exercise genuine, imbalanced class groups without running a full quadratic
# validation in routine development.
groups <- joined$metadata[
  !is.na(material_class) & !is.na(spectrum_type),
  .N,
  by = .(spectrum_type, material_class)
][N >= 12L][order(-N)][1:min(.N, 6L)]
indices <- unlist(lapply(seq_len(nrow(groups)), function(i) {
  which(
    joined$metadata$spectrum_type == groups$spectrum_type[[i]] &
      joined$metadata$material_class == groups$material_class[[i]]
  )[seq_len(min(groups$N[[i]], 40L))]
}), use.names = FALSE)
representative <- filter_spec(libraries$derivative, indices)
representative$metadata <- data.table::copy(joined$metadata[indices])
pruned <- prune_lib(
  representative, min_n = 10, return = "report", progress = FALSE
)
stopifnot(check_OpenSpecy(pruned$object))
stopifnot(identical(colnames(pruned$object$spectra),
                    pruned$object$metadata$sample_name))
print(pruned$summary)
print(pruned$schedule)
