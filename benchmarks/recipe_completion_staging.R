# Compare frozen all-recipe materialization with bounded file-backed completion.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
  library(hdf5r)
})

args <- commandArgs(trailingOnly = TRUE)
n_spectra <- if (length(args) >= 1L) as.integer(args[[1L]]) else 6000L
n_wavenumbers <- if (length(args) >= 2L) as.integer(args[[2L]]) else 600L
chunk_size <- if (length(args) >= 3L) as.integer(args[[3L]]) else 500L
repetitions <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (anyNA(c(n_spectra, n_wavenumbers, chunk_size, repetitions)) ||
    min(n_spectra, n_wavenumbers, chunk_size, repetitions) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
set.seed(51053L)
wavenumber <- seq(400, by = 6, length.out = n_wavenumbers)
spectra <- matrix(
  rnorm(n_wavenumbers * n_spectra),
  nrow = n_wavenumbers, ncol = n_spectra
)
spectra <- sweep(spectra, 1L, sin(wavenumber / 140), "+")
ids <- sprintf("recipe_%07d", seq_len(n_spectra))
colnames(spectra) <- ids
metadata <- data.table(
  sample_name = ids, spectrum_type = "raman",
  intensity_units = "arbitrary units"
)
source <- as_OpenSpecy(
  wavenumber, spectra, metadata,
  attributes = list(intensity_unit = "arbitrary units")
)
spectra <- source$spectra
metadata <- source$metadata
blocks <- OpenSpecy:::.lib_recipe_blocks(n_spectra, chunk_size)

frozen_in_memory <- function() {
  parts <- lapply(blocks, function(columns) {
    spectra[, columns, drop = FALSE]
  })
  output <- do.call(cbind, parts)
  colnames(output) <- ids
  list(spectra = output, metadata = copy(metadata))
}

file_backed <- function(materialize = TRUE) {
  path <- tempfile("OpenSpecy-recipe-completion-benchmark-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)
  writer <- OpenSpecy:::.lib_stage_artifact_writer(
    file.path(path, "spectra.h5"), file.path(path, "metadata.rds"),
    wavenumber = wavenumber, capacity = n_spectra,
    chunk_size = chunk_size, recipe = "benchmark",
    attributes = list(intensity_unit = "arbitrary units")
  )
  on.exit(writer$close(), add = TRUE)
  for (columns in blocks) {
    writer$append(
      spectra[, columns, drop = FALSE], metadata[columns]
    )
  }
  artifact <- writer$finalize()
  if (!materialize) return(artifact)
  object <- OpenSpecy:::.lib_materialize_staged_recipe(artifact)
  list(spectra = object$spectra, metadata = object$metadata)
}

expected <- frozen_in_memory()
observed <- file_backed()
stopifnot(
  identical(expected$spectra, observed$spectra),
  isTRUE(all.equal(
    expected$metadata, observed$metadata, check.attributes = FALSE
  ))
)

elapsed <- function(fun) {
  median(replicate(repetitions, system.time(fun())[["elapsed"]]))
}
memory_seconds <- elapsed(frozen_in_memory)
staged_seconds <- elapsed(file_backed)
total_cells <- as.double(n_wavenumbers) * n_spectra
block_cells <- as.double(n_wavenumbers) * min(chunk_size, n_spectra)
result <- data.table(
  implementation = c("all_recipe_matrices", "file_backed_blocks"),
  seconds = c(memory_seconds, staged_seconds),
  modeled_incremental_peak_mib = 8 * c(2 * total_cells, block_cells) / 1024^2,
  spectra = n_spectra, wavenumbers = n_wavenumbers,
  chunk_size = chunk_size, repetitions = repetitions
)
print(result)
cat(sprintf(
  paste0("file-backed modeled peak/all-recipe peak: %.4f; ",
         "elapsed ratio: %.2fx; blocks: %d\n"),
  block_cells / (2 * total_cells),
  staged_seconds / max(memory_seconds, .Machine$double.eps),
  length(blocks)
))
stopifnot(
  block_cells < 2 * total_cells,
  staged_seconds <= max(8 * memory_seconds, memory_seconds + 30)
)
