# Compare the former post-bind metadata filter with retained-row pre-binding.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
  library(bench)
})

args <- commandArgs(trailingOnly = TRUE)
source_count <- if (length(args) >= 1L) as.integer(args[[1L]]) else 4L
rows_per_source <- if (length(args) >= 2L) as.integer(args[[2L]]) else 50000L
repeated_columns <- if (length(args) >= 3L) as.integer(args[[3L]]) else 20L
repetitions <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
values <- c(source_count, rows_per_source, repeated_columns, repetitions)
if (anyNA(values) || min(values) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
old_options <- options(OpenSpecy.compact_staged_metadata = TRUE)
on.exit(options(old_options), add = TRUE)
work_dir <- tempfile("OpenSpecy-metadata-prebind-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

records <- lapply(seq_len(source_count), function(source_index) {
  ids <- sprintf("hash_%02d_%07d", source_index, seq_len(rows_per_source))
  if (source_index > 1L) ids[[1L]] <- "shared_cross_source_hash"
  metadata <- data.table(
    sample_name = ids,
    sample_name_old = sprintf(
      "old_%02d_%07d", source_index, seq_len(rows_per_source)
    ),
    spectrum_identity = sprintf(
      "identity_%02d_%07d", source_index, seq_len(rows_per_source)
    ),
    source_label = paste0("source_", source_index),
    blank_column = NA_character_
  )
  for (column in seq_len(repeated_columns)) {
    metadata[[paste0("repeated_", column)]] <- paste0(
      "source_", source_index, "_value_", column
    )
  }
  path <- file.path(work_dir, sprintf("metadata-%03d.rds", source_index))
  saveRDS(metadata, path, compress = FALSE, version = 3)
  list(
    n = nrow(metadata), spectrum_names = ids,
    metadata_path = path, metadata_prototype = metadata[0]
  )
})
report <- function(...) NULL

postbind_filter <- function() {
  metadata <- OpenSpecy:::.lib_combine_staged_metadata(records, report)
  ids <- unlist(lapply(records, `[[`, "spectrum_names"), use.names = FALSE)
  metadata <- metadata[!duplicated(ids)]
  metadata <- OpenSpecy:::.lib_drop_blank_staged_metadata(metadata)
  compact <- OpenSpecy:::.lib_compact_staged_metadata(metadata)
  OpenSpecy:::.lib_expand_staged_metadata(compact$metadata)
}

prebind_filter <- function() {
  plan <- OpenSpecy:::.lib_staged_global_filter_plan(
    records, id_col = "sample_name", exclude_ids = NULL,
    dedupe_duplicate = "first", report = report
  )
  metadata <- OpenSpecy:::.lib_combine_staged_metadata(
    records, report, record_rows = plan$merge_keep
  )
  metadata <- OpenSpecy:::.lib_drop_blank_staged_metadata(
    metadata, copy = FALSE
  )
  compact <- OpenSpecy:::.lib_compact_staged_metadata(metadata)
  OpenSpecy:::.lib_expand_staged_metadata(compact$metadata)
}

expected <- postbind_filter()
observed <- prebind_filter()
stopifnot(identical(expected, observed))
rm(expected, observed)
gc(verbose = FALSE, full = TRUE)

result <- bench::mark(
  postbind_filter = postbind_filter(),
  prebind_filter = prebind_filter(),
  iterations = repetitions,
  check = FALSE,
  filter_gc = FALSE
)[, c("expression", "median", "mem_alloc", "n_gc")]
print(result)
labels <- as.character(result$expression)
postbind_row <- match("postbind_filter", labels)
prebind_row <- match("prebind_filter", labels)
stopifnot(!is.na(postbind_row), !is.na(prebind_row))
postbind_memory <- as.numeric(result$mem_alloc[[postbind_row]])
prebind_memory <- as.numeric(result$mem_alloc[[prebind_row]])
postbind_seconds <- as.numeric(result$median[[postbind_row]])
prebind_seconds <- as.numeric(result$median[[prebind_row]])
cat(sprintf(
  "pre-bind/post-bind allocation ratio: %.4f; elapsed ratio: %.2fx\n",
  prebind_memory / postbind_memory,
  prebind_seconds / max(postbind_seconds, .Machine$double.eps)
))
stopifnot(
  prebind_memory <= 1.1 * postbind_memory,
  prebind_seconds <= max(3 * postbind_seconds, postbind_seconds + 15)
)
