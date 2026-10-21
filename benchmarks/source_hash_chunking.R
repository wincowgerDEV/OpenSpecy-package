# Compare the frozen full-matrix source hash with bounded column chunks.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
n_spectra <- if (length(args) >= 1L) as.integer(args[[1L]]) else 2000L
n_wavenumbers <- if (length(args) >= 2L) as.integer(args[[2L]]) else 600L
chunk_size <- if (length(args) >= 3L) as.integer(args[[3L]]) else 250L
repetitions <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (anyNA(c(n_spectra, n_wavenumbers, chunk_size, repetitions)) ||
    min(n_spectra, n_wavenumbers, chunk_size, repetitions) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
set.seed(51051L)
wavenumber <- seq(400, by = 4, length.out = n_wavenumbers)
spectra <- matrix(
  stats::rnorm(n_wavenumbers * n_spectra),
  nrow = n_wavenumbers,
  ncol = n_spectra
)
spectra <- sweep(spectra, 1L, sin(wavenumber / 120), "+")
metadata <- data.table(sample_name = paste0("hash_", seq_len(n_spectra)))

frozen_full_hash <- function() {
  ignored <- OpenSpecy:::.spectra_ignore_info(
    spectra, lead_tail_only = TRUE, ig = c(NA)
  )
  keep <- rowSums(ignored$ignored) == 0L
  local_spectra <- spectra[keep, , drop = FALSE]
  local_wavenumber <- wavenumber[keep]
  list(
    current = OpenSpecy:::.lib_hash_processed_matrix(
      local_wavenumber, local_spectra, range = NULL, res = 8,
      scale = 100, algo = "md5"
    ),
    old = OpenSpecy:::.lib_hash_processed_matrix(
      local_wavenumber, local_spectra, range = c(100, 4000), res = 8,
      scale = 100, algo = "md5", min_rows = 3L,
      short_value = "new format"
    )
  )
}

bounded_hash <- function() {
  old_options <- options(OpenSpecy.source_hash_chunk_size = chunk_size)
  on.exit(options(old_options), add = TRUE)
  OpenSpecy:::.lib_source_hash_ids(wavenumber, spectra, metadata)
}

full_result <- frozen_full_hash()
bounded_result <- bounded_hash()
stopifnot(
  identical(full_result$current, bounded_result$current),
  identical(full_result$old, bounded_result$old)
)

elapsed <- function(fun) {
  median(replicate(repetitions, system.time(fun())[["elapsed"]]))
}
full_seconds <- elapsed(frozen_full_hash)
bounded_seconds <- elapsed(bounded_hash)
full_cells <- as.double(n_wavenumbers) * n_spectra
bounded_cells <- as.double(n_wavenumbers) * min(chunk_size, n_spectra)
result <- data.table(
  implementation = c("frozen_full_matrix", "bounded_column_chunks"),
  seconds = c(full_seconds, bounded_seconds),
  peak_input_mib = 8 * c(full_cells, bounded_cells) / 1024^2,
  spectra = n_spectra,
  wavenumbers = n_wavenumbers,
  repetitions = repetitions
)
print(result)
cat(sprintf(
  "bounded peak/full peak: %.4f; elapsed ratio: %.2fx; chunks: %d\n",
  bounded_cells / full_cells,
  bounded_seconds / max(full_seconds, .Machine$double.eps),
  ceiling(n_spectra / chunk_size)
))
stopifnot(
  bounded_cells <= full_cells,
  bounded_seconds <= max(4 * full_seconds, full_seconds + 10)
)
