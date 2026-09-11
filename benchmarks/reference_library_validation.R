# Manual reference-library validation with four explicit modes:
# - saved (default): retain the earlier saved-build taxonomy/pruning checks;
# - probe: development-only end-to-end build of up to 1,000 class-balanced
#   FTIR/Raman legacy spectra;
# - artifact-metrics: repeated same-output comparisons of unbatched and bounded
#   artifact-ratio, signal/noise, and pruning correlations;
# - full: complete candidate build and comprehensive comparison with all seven
#   downloaded legacy artifacts.
# Set OPENSPECY_REFERENCE_VALIDATION_MODE and the path variables named below.
# This script is intentionally outside tests because artifacts are large,
# network-backed, and computationally expensive.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

mode <- Sys.getenv("OPENSPECY_REFERENCE_VALIDATION_MODE", unset = "saved")
if (identical(mode, "artifact-metrics")) {
  spectra <- raman_hdpe$spectra[, rep(1L, 1000L), drop = FALSE]
  spectra <- sweep(spectra, 2L, seq_len(ncol(spectra)) / 1e6, "+")
  colnames(spectra) <- paste0("metric_", seq_len(ncol(spectra)))
  object <- as_OpenSpecy(
    raman_hdpe$wavenumber, as.data.frame(spectra),
    metadata = data.table::data.table(sample_name = paste0("metric_", 1:1000))
  )
  old <- OpenSpecy:::.artifact_ratio_metrics_block(object)
  new <- OpenSpecy:::.artifact_ratio_metrics(object, batch_size = 200L)
  stopifnot(identical(names(old), names(new)))
  for (field in setdiff(names(old), "tail_n")) {
    stopifnot(isTRUE(all.equal(old[[field]], new[[field]])))
  }
  elapsed <- function(fun) {
    replicate(3L, system.time(fun())[["elapsed"]])
  }
  old_seconds <- elapsed(function() {
    OpenSpecy:::.artifact_ratio_metrics_block(object)
  })
  new_seconds <- elapsed(function() {
    OpenSpecy:::.artifact_ratio_metrics(object, batch_size = 200L)
  })
  result <- data.table::data.table(
    implementation = rep(c("unbatched", "bounded"), each = 3L),
    elapsed_seconds = c(old_seconds, new_seconds)
  )
  print(result)
  if (stats::median(new_seconds) > 3 * stats::median(old_seconds)) {
    stop("Bounded artifact metrics are more than 3x slower than unbatched")
  }
  old_snr <- OpenSpecy:::.run_sig_over_noise_matrix(
    object$spectra, step = 10L, batch_size = ncol(object$spectra)
  )
  new_snr <- OpenSpecy:::.run_sig_over_noise_matrix(
    object$spectra, step = 10L, batch_size = 200L
  )
  stopifnot(isTRUE(all.equal(old_snr, new_snr)))
  old_snr_seconds <- elapsed(function() {
    OpenSpecy:::.run_sig_over_noise_matrix(
      object$spectra, step = 10L, batch_size = ncol(object$spectra)
    )
  })
  new_snr_seconds <- elapsed(function() {
    OpenSpecy:::.run_sig_over_noise_matrix(
      object$spectra, step = 10L, batch_size = 200L
    )
  })
  snr_result <- data.table::data.table(
    implementation = rep(c("unbatched_snr", "bounded_snr"), each = 3L),
    elapsed_seconds = c(old_snr_seconds, new_snr_seconds)
  )
  print(snr_result)
  if (stats::median(new_snr_seconds) > 3 * stats::median(old_snr_seconds)) {
    stop("Bounded running S/N is more than 3x slower than unbatched")
  }
  prune_query <- seq_len(100L)
  prune_candidates <- seq_len(ncol(object$spectra))
  prune_ids <- colnames(object$spectra)
  normalized <- OpenSpecy:::.lib_prune_normalize(
    object$spectra, object$wavenumber, c(2200, 2420)
  )
  old_prune <- OpenSpecy:::.lib_prune_correlations(
    object, prune_query, prune_candidates, c(2200, 2420), prune_ids
  )
  new_prune <- OpenSpecy:::.lib_prune_best_match(
    prune_query, prune_candidates, normalized, prune_ids,
    exclude_self = TRUE, block_size = 32L
  )
  old_prune_best <- matrixStats::rowMaxs(old_prune)
  tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(old_prune_best))
  stopifnot(all(abs(new_prune$correlation - old_prune_best) <= tolerance))
  old_prune_seconds <- elapsed(function() {
    OpenSpecy:::.lib_prune_correlations(
      object, prune_query, prune_candidates, c(2200, 2420), prune_ids
    )
  })
  new_prune_seconds <- elapsed(function() {
    OpenSpecy:::.lib_prune_best_match(
      prune_query, prune_candidates, normalized, prune_ids,
      exclude_self = TRUE, block_size = 32L
    )
  })
  prune_result <- data.table::data.table(
    implementation = rep(c("full_matrix_prune", "bounded_prune"), each = 3L),
    elapsed_seconds = c(old_prune_seconds, new_prune_seconds)
  )
  print(prune_result)
  if (stats::median(new_prune_seconds) >
      5 * stats::median(old_prune_seconds)) {
    stop("Bounded pruning correlation is more than 5x slower than full matrix")
  }
  quit(save = "no", status = 0L)
}
if (identical(mode, "probe")) {
  seed <- as.integer(Sys.getenv("OPENSPECY_VALIDATION_SEED", unset = "123"))
  output <- Sys.getenv(
    "OPENSPECY_VALIDATION_OUTPUT",
    unset = tempfile("openspecy-reference-probe-")
  )
  reuse <- tolower(Sys.getenv(
    "OPENSPECY_VALIDATION_REUSE", unset = "true"
  )) %in% c("true", "1", "yes")
  raw <- tryCatch(
    load_lib("raw"),
    error = function(error) {
      get_lib("raw")
      load_lib("raw")
    }
  )
  set.seed(seed)
  types <- data.table::fread(file.path(
    "workflows", "data", "library_types.csv"
  ))
  organization <- as.character(raw$metadata$organization)
  fallback <- as.character(raw$metadata$user_name)
  fill <- is.na(organization) | !nzchar(organization)
  organization[fill] <- fallback[fill]
  eligible <- organization %in% types$organization &
    tolower(raw$metadata$spectrum_type) %in% c("ftir", "raman") &
    !is.na(raw$metadata$material_class) & nzchar(raw$metadata$material_class)
  eligible[is.na(eligible)] <- FALSE
  pool <- data.table::data.table(
    row = which(eligible),
    spectrum_type = tolower(raw$metadata$spectrum_type[eligible]),
    material_class = as.character(raw$metadata$material_class[eligible])
  )[!tolower(material_class) %in% c("other", "other plastic", "other material")]
  supported <- pool[, .N, by = .(spectrum_type, material_class)][N >= 40L]
  supported <- supported[order(spectrum_type, -N)][, head(.SD, 5L),
                                                   by = spectrum_type]
  if (supported[, data.table::uniqueN(material_class), by = spectrum_type][
      , any(V1 < 2L)]) {
    stop("Probe requires at least two supported classes per selected type")
  }
  selected <- unlist(lapply(seq_len(nrow(supported)), function(i) {
    rows <- pool[
      spectrum_type == supported$spectrum_type[[i]] &
        material_class == supported$material_class[[i]], row
    ]
    sample(rows, min(length(rows), 100L))
  }), use.names = FALSE)
  probe <- filter_spec(raw, selected)
  result <- build_lib(
    probe, output_dir = output, previous_library_dir = NULL,
    reuse = reuse, remove_other = TRUE, seed = seed
  )
  valid_type_map <- function(x) {
    is.list(x) && length(x) > 0L &&
      all(vapply(x, check_OpenSpecy, logical(1)))
  }
  stopifnot(
    identical(names(result),
              c("libraries", "medoids", "models", "assessments")),
    all(vapply(result$libraries, valid_type_map, logical(1))),
    all(vapply(result$medoids, valid_type_map, logical(1))),
    identical(names(result$assessments),
              c("cleanup", "ref_lib", "medoid", "model", "functionality")),
    sum(lengths(result$assessments)) <= 10L
  )
  print(result$assessments$cleanup$summary[, .N, by = assessment_kind])
  print(result$assessments$functionality$comparison)
  message("Development-only class-balanced probe retained at: ", output)
  quit(save = "no", status = 0L)
}

if (identical(mode, "full")) {
  workflow <- new.env(parent = globalenv())
  sys.source("workflows/OpenSpecy_reference_library.R", envir = workflow)
  result <- workflow$reference_library_build
  evidence <- attr(result$assessments, "evidence", exact = TRUE)
  leaves <- unlist(result$assessments, recursive = FALSE)
  stopifnot(
    identical(names(result$assessments),
              c("cleanup", "ref_lib", "medoid", "model", "functionality")),
    sum(lengths(result$assessments)) <= 10L,
    all(vapply(leaves, nrow, integer(1L)) > 0L),
    identical(names(result$assessments$cleanup$dropped_spectrum_identities),
              "spectrum_identity"),
    nrow(evidence$split_manifest) > 0L,
    nrow(evidence$library_tests) > 0L,
    nrow(evidence$model_tests) > 0L,
    !nrow(evidence$split_manifest[
      , data.table::uniqueN(split), by = .(artifact, source, group_id)
    ][V1 > 1L])
  )
  print(result$assessments$ref_lib$accuracy)
  print(result$assessments$medoid$accuracy)
  print(result$assessments$model$accuracy)
  print(result$assessments$model$diagnostics)
  print(result$assessments$functionality$comparison)
  message("Full candidate release retained at: ", attr(result, "output_dir"))
  quit(save = "no", status = 0L)
}

if (!identical(mode, "saved")) {
  stop(paste(
    "OPENSPECY_REFERENCE_VALIDATION_MODE must be saved, probe,",
    "artifact-metrics, or full"
  ))
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
  all(colnames(x$spectra) %in% colnames(libraries$raw$spectra)) &&
    identical(colnames(x$spectra), x$metadata$sample_name)
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
# Recompute derived classification from the current curated tables. Saved
# libraries may carry stale populated values that would correctly block a
# fill-only lookup and make a table-curation audit measure the prior build.
for (column in c("material", "material_class", "material_type")) {
  raw$metadata[[column]] <- NA_character_
}
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
blank_class <- is.na(joined$metadata$material_class) |
  !nzchar(trimws(joined$metadata$material_class))
print(joined$metadata[blank_class, .N, by = .(
  material, organization, user_name, spectrum_identity
)][order(-N, spectrum_identity)][1:min(.N, 100L)], nrows = 100L)
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
selected_ids <- colnames(raw$spectra)[indices]
derivative_indices <- match(
  intersect(selected_ids, colnames(libraries$derivative$spectra)),
  colnames(libraries$derivative$spectra)
)
representative <- filter_spec(libraries$derivative, derivative_indices)
representative$metadata <- data.table::copy(joined$metadata[
  match(colnames(representative$spectra), colnames(raw$spectra))
])
pruned <- prune_lib(
  representative, min_n = 10, return = "report", progress = FALSE
)
stopifnot(check_OpenSpecy(pruned$object))
stopifnot(identical(colnames(pruned$object$spectra),
                    pruned$object$metadata$sample_name))
print(pruned$summary)
print(pruned$schedule)
