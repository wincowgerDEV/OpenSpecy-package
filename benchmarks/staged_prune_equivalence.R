# Compare the maintained in-memory prune implementation with the bounded
# file-backed implementation used by official large reference builds.

devtools::load_all(quiet = TRUE)
stopifnot(requireNamespace("hdf5r", quietly = TRUE))

set.seed(462)
wavenumber <- seq(500, 3500, length.out = 180L)
classes <- rep(paste0("polymer_", letters[1:6]), each = 20L)
spectra <- vapply(seq_along(classes), function(i) {
  class_i <- match(classes[[i]], unique(classes))
  sin(wavenumber / (35 + class_i * 8)) +
    cos(wavenumber / (62 + class_i * 5)) + rnorm(length(wavenumber), sd = 0.01)
}, numeric(length(wavenumber)))
spectra <- cbind(spectra, spectra[, 21L] + 1e-4, spectra[, 81L] + 1e-4)
colnames(spectra) <- paste0("benchmark_", seq_len(ncol(spectra)))
object <- as_OpenSpecy(
  wavenumber, spectra,
  metadata = data.table::data.table(
    sample_name = colnames(spectra),
    material_class = c(classes, "other plastic", "other"),
    material_type = "plastic", spectrum_type = "ftir",
    library_name = "benchmark"
  )
)

root <- tempfile("OpenSpecy-staged-prune-benchmark-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
writer <- OpenSpecy:::.lib_stage_artifact_writer(
  file.path(root, "input.h5"), file.path(root, "input-metadata.rds"),
  wavenumber = object$wavenumber, capacity = ncol(object$spectra),
  chunk_size = 32L, recipe = "derivative",
  attributes = list(intensity_unit = "absorbance")
)
writer$append(object$spectra, object$metadata)
artifact <- writer$finalize()

in_memory_time <- system.time({
  expected <- prune_lib(
    object, min_n = 10L, cross_class = FALSE,
    return = "report", progress = FALSE
  )
})[["elapsed"]]
file_backed_time <- system.time({
  quarantine <- OpenSpecy:::.lib_quarantine_shard_writer(
    file.path(root, "quarantine")
  )
  staged <- OpenSpecy:::.lib_prune_staged_recipe(
    artifact, args = list(min_n = 10L, cross_class = FALSE),
    staging_dir = file.path(root, "pruned"),
    quarantine_writer = quarantine, progress = FALSE,
    report = function(...) NULL
  )
  observed <- OpenSpecy:::.lib_materialize_staged_recipe(staged$artifact)
})[["elapsed"]]

reassignment_equivalence <- all.equal(
  staged$reassignments[order(spectrum_id)],
  expected$reassignments[order(spectrum_id)],
  tolerance = sqrt(.Machine$double.eps)
)

stopifnot(
  identical(observed$wavenumber, expected$object$wavenumber),
  isTRUE(all.equal(observed$spectra, expected$object$spectra, tolerance = 0)),
  isTRUE(all.equal(observed$metadata, expected$object$metadata, tolerance = 0)),
  isTRUE(reassignment_equivalence),
  isTRUE(all.equal(
    staged$removals[order(spectrum_id)],
    expected$removals[order(spectrum_id)],
    tolerance = sqrt(.Machine$double.eps)
  )),
  max(staged$telemetry$query_block) <= 32L,
  max(staged$telemetry$candidate_block) <= 1024L
)

print(data.table::data.table(
  implementation = c("in_memory", "file_backed"),
  elapsed_seconds = c(in_memory_time, file_backed_time),
  input_spectra = ncol(object$spectra),
  retained_spectra = ncol(observed$spectra)
))
print(staged$telemetry)
