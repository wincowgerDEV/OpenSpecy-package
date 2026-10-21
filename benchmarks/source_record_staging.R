# Compare retained source records with the path-backed two-pass merge.

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
if (anyNA(c(source_count, spectra_per_source, n_wavenumbers, chunk_size,
            repetitions)) ||
    min(source_count, spectra_per_source, n_wavenumbers, chunk_size,
        repetitions) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
set.seed(51052L)
work_dir <- tempfile("OpenSpecy-source-record-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
wavenumber <- seq(400, by = 6, length.out = n_wavenumbers)
paths <- vapply(seq_len(source_count), function(source_index) {
  spectra <- matrix(
    rnorm(n_wavenumbers * spectra_per_source),
    nrow = n_wavenumbers, ncol = spectra_per_source
  )
  spectra <- sweep(spectra, 1L, sin(wavenumber / 120), "+")
  ids <- sprintf(
    "stage_%02d_%06d", source_index, seq_len(spectra_per_source)
  )
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

run_build <- function(staged) {
  old_options <- options(
    OpenSpecy.source_record_staging = staged,
    OpenSpecy.source_merge_chunk_size = chunk_size,
    OpenSpecy.source_merge_gc_interval = 10L
  )
  on.exit(options(old_options), add = TRUE)
  build_lib(
    paths, recipes = list(raw = list()), range = NULL,
    dedupe = FALSE, convert_intensity = FALSE,
    signal_noise = FALSE, progress = FALSE
  )$raw
}

retained_result <- run_build(FALSE)
staged_result <- run_build(TRUE)
stopifnot(
  identical(retained_result$wavenumber, staged_result$wavenumber),
  identical(retained_result$spectra, staged_result$spectra),
  identical(retained_result$metadata, staged_result$metadata),
  all(attr(staged_result, "source_merge_chunk_report")$gc_interval == 10L)
)

elapsed <- function(staged) {
  median(replicate(repetitions, system.time(run_build(staged))[["elapsed"]]))
}
retained_seconds <- elapsed(FALSE)
staged_seconds <- elapsed(TRUE)
total_cells <- as.double(source_count) * spectra_per_source * n_wavenumbers
largest_source_cells <- as.double(spectra_per_source) * n_wavenumbers
block_cells <- as.double(min(chunk_size, spectra_per_source)) * n_wavenumbers
retained_peak_cells <- 2 * total_cells
staged_peak_cells <- total_cells + largest_source_cells + block_cells
result <- data.table(
  implementation = c("retained_records", "path_backed_two_pass"),
  seconds = c(retained_seconds, staged_seconds),
  modeled_spectral_peak_mib = 8 * c(
    retained_peak_cells, staged_peak_cells
  ) / 1024^2,
  sources = source_count,
  spectra = source_count * spectra_per_source,
  wavenumbers = n_wavenumbers,
  repetitions = repetitions
)
print(result)
cat(sprintf(
  paste0("staged modeled peak/retained peak: %.4f; elapsed ratio: %.2fx; ",
         "maximum merge block: %d spectra; GC interval: 10 chunks\n"),
  staged_peak_cells / retained_peak_cells,
  staged_seconds / max(retained_seconds, .Machine$double.eps),
  min(chunk_size, spectra_per_source)
))
stopifnot(
  staged_peak_cells < retained_peak_cells,
  staged_seconds <= max(4 * retained_seconds, retained_seconds + 15)
)
