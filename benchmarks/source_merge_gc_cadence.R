# Compare the historical per-source merge collections with a bounded cadence.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
  library(hdf5r)
})

args <- commandArgs(trailingOnly = TRUE)
record_count <- if (length(args) >= 1L) as.integer(args[[1L]]) else 200L
repetitions <- if (length(args) >= 2L) as.integer(args[[2L]]) else 2L
gc_interval <- if (length(args) >= 3L) as.integer(args[[3L]]) else 10L
if (anyNA(c(record_count, repetitions, gc_interval)) ||
    min(record_count, repetitions, gc_interval) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
work_dir <- tempfile("OpenSpecy-source-merge-gc-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
wavenumbers <- seq(400, 4000, length.out = 128L)
block_paths <- vapply(seq_len(record_count), function(i) {
  path <- file.path(work_dir, sprintf("spectra-%06d.rds", i))
  block <- matrix(sin(wavenumbers / (20 + i)), ncol = 1L)
  saveRDS(block, path, compress = FALSE, version = 3)
  path
}, character(1L))

merge_kernel <- function(historical) {
  output_path <- tempfile("merged-", tmpdir = work_dir, fileext = ".h5")
  handle <- H5File$new(output_path, mode = "w")
  on.exit({
    try(handle$close_all(), silent = TRUE)
    unlink(output_path, force = TRUE)
  }, add = TRUE)
  dataset <- handle$create_dataset(
    "spectra", dtype = h5types$H5T_IEEE_F64LE,
    dims = c(length(wavenumbers), record_count),
    chunk_dims = c(length(wavenumbers), min(200L, record_count)),
    gzip_level = 1L
  )
  collections <- 0L
  blocks_since_gc <- 0L
  write_calls <- 0L
  if (!historical) {
    buffer <- matrix(
      NA_real_, nrow = length(wavenumbers), ncol = record_count
    )
  }
  for (i in seq_along(block_paths)) {
    block <- readRDS(block_paths[[i]])
    if (historical) {
      dataset[, i] <- block
      write_calls <- write_calls + 1L
    } else {
      buffer[, i] <- block
    }
    rm(block)
    if (historical) {
      gc(verbose = FALSE)
      collections <- collections + 1L
      gc(verbose = FALSE)
      collections <- collections + 1L
    } else {
      blocks_since_gc <- blocks_since_gc + 1L
      if (blocks_since_gc >= gc_interval) {
        gc(verbose = FALSE)
        collections <- collections + 1L
        blocks_since_gc <- 0L
      }
    }
  }
  if (!historical) {
    dataset[, seq_len(record_count)] <- buffer
    write_calls <- write_calls + 1L
    rm(buffer)
  }
  if (!historical && blocks_since_gc > 0L) {
    gc(verbose = FALSE)
    collections <- collections + 1L
  }
  handle$flush()
  spectra <- dataset[, ]
  handle$close_all()
  list(
    spectra = spectra, collections = collections,
    write_calls = write_calls
  )
}

historical <- merge_kernel(TRUE)
bounded <- merge_kernel(FALSE)
stopifnot(identical(historical$spectra, bounded$spectra))

elapsed <- function(historical) {
  median(replicate(
    repetitions,
    system.time(merge_kernel(historical))[["elapsed"]]
  ))
}
historical_seconds <- elapsed(TRUE)
bounded_seconds <- elapsed(FALSE)
result <- data.table(
  implementation = c("historical_gc_twice_per_source", "bounded_gc_cadence"),
  seconds = c(historical_seconds, bounded_seconds),
  records = record_count,
  gc_calls = c(historical$collections, bounded$collections),
  hdf5_write_calls = c(historical$write_calls, bounded$write_calls),
  repetitions = repetitions
)
print(result)
cat(sprintf(
  "bounded/historical elapsed ratio: %.3f; GC interval: %d blocks\n",
  bounded_seconds / max(historical_seconds, .Machine$double.eps),
  gc_interval
))
stopifnot(
  bounded_seconds <= max(0.75 * historical_seconds,
                          historical_seconds - 0.05)
)
