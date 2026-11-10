# Compare dense and direct file-backed retained-source merging.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
source_count <- if (length(args) >= 1L) as.integer(args[[1L]]) else 4L
spectra_per_source <- if (length(args) >= 2L) as.integer(args[[2L]]) else 2000L
n_wavenumbers <- if (length(args) >= 3L) as.integer(args[[3L]]) else 300L
chunk_size <- if (length(args) >= 4L) as.integer(args[[4L]]) else 250L
repetitions <- if (length(args) >= 5L) as.integer(args[[5L]]) else 3L
values <- c(source_count, spectra_per_source, n_wavenumbers, chunk_size,
            repetitions)
if (anyNA(values) || min(values) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}
if (!requireNamespace("hdf5r", quietly = TRUE)) {
  stop("The source-merge benchmark requires hdf5r", call. = FALSE)
}

load_all(".", quiet = TRUE)
set.seed(51052L)
work_dir <- tempfile("OpenSpecy-file-backed-merge-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
wavenumber <- seq(400, by = 6, length.out = n_wavenumbers)
paths <- vapply(seq_len(source_count), function(source_index) {
  spectra <- matrix(
    rnorm(n_wavenumbers * spectra_per_source),
    nrow = n_wavenumbers, ncol = spectra_per_source
  )
  spectra <- sweep(spectra, 1L, sin(wavenumber / 120), "+")
  ids <- sprintf("merge_%02d_%06d", source_index,
                 seq_len(spectra_per_source))
  colnames(spectra) <- ids
  object <- as_OpenSpecy(
    wavenumber, spectra = spectra,
    metadata = data.table(
      sample_name = ids, file_id = ids,
      spectrum_identity = paste0("benchmark_", source_index),
      library_name = paste0("benchmark_", source_index),
      spectrum_type = "raman", intensity_units = "arbitrary units"
    ),
    attributes = list(intensity_unit = "arbitrary units")
  )
  path <- file.path(work_dir, sprintf("source-%02d.rds", source_index))
  saveRDS(object, path, compress = FALSE, version = 3)
  path
}, character(1L))

run_build <- function(file_backed) {
  stage_dir <- tempfile("recipe-stage-", tmpdir = work_dir)
  dir.create(stage_dir)
  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)
  old_options <- options(
    OpenSpecy.source_record_staging = TRUE,
    OpenSpecy.source_prefilter_staging = TRUE,
    OpenSpecy.source_merge_file_backed = file_backed,
    OpenSpecy.source_merge_chunk_size = chunk_size,
    OpenSpecy.source_merge_gc_interval = 10L,
    OpenSpecy.recipe_record_staging = TRUE,
    OpenSpecy.recipe_chunk_size = chunk_size,
    OpenSpecy.recipe_staging_dir = stage_dir
  )
  on.exit(options(old_options), add = TRUE)
  result <- build_lib(
    paths, recipes = list(raw = list()), range = NULL,
    dedupe = FALSE, convert_intensity = FALSE,
    signal_noise = FALSE, progress = FALSE
  )
  telemetry <- result$artifacts$raw$object_attributes$
    source_merge_chunk_report
  object <- OpenSpecy:::.lib_materialize_staged_recipe(
    result$artifacts$raw
  )
  if (file.exists(file.path(stage_dir, "merged-core.h5"))) {
    stop("Temporary merged core survived recipe staging", call. = FALSE)
  }
  list(object = object, telemetry = telemetry)
}

dense <- run_build(FALSE)
file_backed <- run_build(TRUE)
metadata_equivalence <- all.equal(
  dense$object$metadata, file_backed$object$metadata
)
if (!isTRUE(metadata_equivalence)) print(metadata_equivalence)
stopifnot(
  identical(dense$object$wavenumber, file_backed$object$wavenumber),
  identical(dense$object$spectra, file_backed$object$spectra),
  isTRUE(metadata_equivalence),
  all(file_backed$telemetry$file_backed_target),
  all(file_backed$telemetry$max_chunk_columns <= chunk_size)
)

elapsed <- function(file_backed) {
  median(replicate(
    repetitions, system.time(run_build(file_backed))[["elapsed"]]
  ))
}
dense_seconds <- elapsed(FALSE)
file_backed_seconds <- elapsed(TRUE)
total_cells <- as.double(source_count) * spectra_per_source * n_wavenumbers
largest_source_cells <- as.double(spectra_per_source) * n_wavenumbers
block_cells <- as.double(min(chunk_size, spectra_per_source)) * n_wavenumbers
dense_peak_cells <- total_cells + largest_source_cells + block_cells
file_backed_peak_cells <- largest_source_cells + 3 * block_cells
result <- data.table(
  implementation = c("dense_target", "file_backed_target"),
  seconds = c(dense_seconds, file_backed_seconds),
  modeled_spectral_peak_mib = 8 * c(
    dense_peak_cells, file_backed_peak_cells
  ) / 1024^2,
  sources = source_count,
  spectra = source_count * spectra_per_source,
  wavenumbers = n_wavenumbers,
  chunk_size = chunk_size,
  repetitions = repetitions
)
print(result)
cat(sprintf(
  paste0("file-backed modeled peak/dense peak: %.4f; elapsed ratio: %.2fx; ",
         "maximum merge block: %d spectra\n"),
  file_backed_peak_cells / dense_peak_cells,
  file_backed_seconds / max(dense_seconds, .Machine$double.eps),
  min(chunk_size, spectra_per_source)
))
stopifnot(
  file_backed_peak_cells < dense_peak_cells,
  file_backed_seconds <= max(6 * dense_seconds, dense_seconds + 30)
)
