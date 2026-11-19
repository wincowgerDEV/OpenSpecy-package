# Compare the historical per-record collection with the bounded source cadence.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
record_count <- if (length(args) >= 1L) as.integer(args[[1L]]) else 500L
repetitions <- if (length(args) >= 2L) as.integer(args[[2L]]) else 3L
gc_interval <- if (length(args) >= 3L) as.integer(args[[3L]]) else 10L
if (anyNA(c(record_count, repetitions, gc_interval)) ||
    min(record_count, repetitions, gc_interval) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
work_dir <- tempfile("OpenSpecy-source-gc-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
records <- lapply(seq_len(record_count), function(i) {
  metadata <- data.table(
    sample_name = sprintf("sample_%06d", i),
    sample_name_old = sprintf("legacy_%06d", i),
    col_id = sprintf("column_%06d", i)
  )
  path <- file.path(work_dir, sprintf("metadata-%06d.rds", i))
  saveRDS(metadata, path, compress = FALSE, version = 3)
  list(
    n = 1L,
    spectrum_names = metadata$sample_name,
    metadata_path = path,
    metadata_prototype = metadata[0]
  )
})

historical_plan <- function(records, exclude_ids) {
  counts <- vapply(records, `[[`, integer(1L), "n")
  starts <- cumsum(c(1L, head(counts, -1L)))
  source_names <- unlist(lapply(records, `[[`, "spectrum_names"),
                         use.names = FALSE)
  excluded <- source_names %in% exclude_ids
  for (i in seq_along(records)) {
    local <- readRDS(records[[i]]$metadata_path)
    rows <- starts[[i]] + seq_len(counts[[i]]) - 1L
    for (column in c("sample_name_old", "col_id")) {
      excluded[rows] <- excluded[rows] |
        as.character(local[[column]]) %in% exclude_ids
    }
    rm(local)
    gc(verbose = FALSE)
  }
  excluded
}

current_plan <- function(records, exclude_ids) {
  old_options <- options(OpenSpecy.source_merge_gc_interval = gc_interval)
  on.exit(options(old_options), add = TRUE)
  OpenSpecy:::.lib_staged_global_filter_plan(
    records,
    id_col = "sample_name",
    exclude_ids = exclude_ids,
    dedupe_duplicate = "none",
    report = function(...) invisible(NULL)
  )$excluded
}

exclude_ids <- c("legacy_000001", "column_000002", "not_present")
expected <- historical_plan(records, exclude_ids)
actual <- current_plan(records, exclude_ids)
stopifnot(identical(expected, actual))

elapsed <- function(fun) {
  median(replicate(
    repetitions,
    system.time(fun(records, exclude_ids))[["elapsed"]]
  ))
}
historical_seconds <- elapsed(historical_plan)
current_seconds <- elapsed(current_plan)
result <- data.table(
  implementation = c("historical_gc_each_record", "bounded_gc_cadence"),
  seconds = c(historical_seconds, current_seconds),
  records = record_count,
  gc_calls = c(record_count, ceiling(record_count / gc_interval)),
  repetitions = repetitions
)
print(result)
cat(sprintf(
  "bounded/historical elapsed ratio: %.3f; GC interval: %d records\n",
  current_seconds / max(historical_seconds, .Machine$double.eps),
  gc_interval
))
stopifnot(
  current_seconds <= max(0.75 * historical_seconds,
                          historical_seconds - 0.05)
)
