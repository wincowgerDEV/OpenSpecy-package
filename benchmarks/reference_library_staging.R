# Repeated bounded final-join probe for staged reference-library artifacts.
#
# Rscript benchmarks/reference_library_staging.R

if (!requireNamespace("devtools", quietly = TRUE) ||
    !requireNamespace("hdf5r", quietly = TRUE)) {
  stop("This benchmark requires devtools and hdf5r", call. = FALSE)
}
devtools::load_all(".", quiet = TRUE)
helpers <- new.env(parent = globalenv())
sys.source("workflows/reference_library_staging.R", envir = helpers)

source_count <- 4L
spectra_per_source <- 500L
wavenumber_count <- 401L
block_size <- 500L
repetitions <- 3L
root <- tempfile("OpenSpecy-reference-staging-benchmark-")
dir.create(root, recursive = TRUE, showWarnings = FALSE)

make_source <- function(source_index) {
  wavenumber <- seq(400, by = 6, length.out = wavenumber_count)
  phase <- seq(0, 4 * pi, length.out = wavenumber_count)
  columns <- seq_len(spectra_per_source)
  spectra <- outer(phase, columns, function(x, y) {
    sin(x + y / 37 + source_index / 11) + cos(x / 3 + y / 53)
  })
  names <- sprintf(
    "benchmark_%02d_%05d", source_index, seq_len(spectra_per_source)
  )
  colnames(spectra) <- names
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(
      sample_name = names,
      spectrum_identity = sprintf("polymer_%03d", columns %% 25L),
      organization = sprintf("benchmark source %02d", source_index),
      user_name = sprintf("benchmark source %02d", source_index),
      spectrum_type = "ftir",
      intensity_units = "absorbance",
      origin = "experimental"
    ),
    attributes = list(intensity_unit = "absorbance")
  )
}

paths <- vapply(seq_len(source_count), function(i) {
  path <- file.path(root, sprintf("source-%02d.rds", i))
  saveRDS(make_source(i), path)
  path
}, character(1L))
input_hashes <- vapply(paths, helpers$.reference_sha256, character(1L))
options(
  OpenSpecy.build_workers = 1L,
  OpenSpecy.source_hash_chunk_size = block_size,
  OpenSpecy.source_record_staging = TRUE,
  OpenSpecy.source_prefilter_staging = TRUE,
  OpenSpecy.compact_staged_metadata = TRUE,
  OpenSpecy.source_merge_file_backed = TRUE,
  OpenSpecy.source_merge_chunk_size = block_size,
  OpenSpecy.source_merge_gc_interval = 2L,
  OpenSpecy.metadata_join_chunk_size = 1000L,
  OpenSpecy.metadata_join_report_value_limit = 100L,
  OpenSpecy.recipe_record_staging = TRUE,
  OpenSpecy.recipe_chunk_size = block_size
)

rows <- lapply(seq_len(repetitions), function(repetition) {
  release_dir <- file.path(
    root, sprintf("run-%02d", repetition), "releases", "benchmark"
  )
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
  profile <- file.path(root, sprintf("allocation-%02d.out", repetition))
  gc(reset = TRUE)
  Rprofmem(profile, threshold = 100000L)
  started <- proc.time()[["elapsed"]]
  artifact <- helpers$.reference_join_typed_paths(
    paths = paths,
    expected_counts = rep(spectra_per_source, source_count),
    source_slugs = sprintf("benchmark-%02d", seq_len(source_count)),
    recipe = "raw",
    type = "ftir",
    release_dir = release_dir,
    signature = "benchmark",
    progress = FALSE
  )
  elapsed <- proc.time()[["elapsed"]] - started
  Rprofmem(NULL)
  allocations <- readLines(profile, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", allocations)))
  object <- readRDS(artifact$path)
  if (!isTRUE(check_OpenSpecy(object)) ||
      ncol(object$spectra) != source_count * spectra_per_source ||
      anyDuplicated(object$metadata$sample_name) ||
      artifact$cross_dataset_removals != 0L ||
      !identical(vapply(paths, helpers$.reference_sha256, character(1L)),
                 input_hashes)) {
    stop("Bounded final-join benchmark contract failed", call. = FALSE)
  }
  data.table::data.table(
    repetition = repetition,
    sources = source_count,
    spectra = ncol(object$spectra),
    wavenumbers = nrow(object$spectra),
    elapsed_seconds = elapsed,
    profiled_allocated_mib = sum(bytes, na.rm = TRUE) / 1024^2,
    modeled_dense_target_mib =
      nrow(object$spectra) * ncol(object$spectra) * 8 / 1024^2,
    max_merge_block_mib =
      nrow(object$spectra) * block_size * 8 / 1024^2,
    output_sha256 = helpers$.reference_sha256(artifact$path)
  )
})
result <- data.table::rbindlist(rows)
if (data.table::uniqueN(result$output_sha256) != 1L ||
    any(result$max_merge_block_mib >= result$modeled_dense_target_mib)) {
  stop("Repeated final joins were not deterministic and bounded", call. = FALSE)
}
print(result)
cat(sprintf(
  "max merge block/dense target: %.4f; median elapsed: %.3fs\n",
  max(result$max_merge_block_mib / result$modeled_dense_target_mib),
  stats::median(result$elapsed_seconds)
))

# Preserve the superseded all-pairs scan as a repeated same-output benchmark.
# A pool containing only one reviewed class cannot yield a class conflict, so
# the optimized path proves that condition before allocating correlation tiles.
set.seed(902)
single_class_n <- 1000L
single_class_features <- 256L
single_class_normalized <- matrix(
  rnorm(single_class_n * single_class_features), nrow = single_class_n
)
single_class_normalized <- single_class_normalized /
  sqrt(rowSums(single_class_normalized^2))
single_class_ids <- sprintf("single_class_%05d", seq_len(single_class_n))
single_class_labels <- rep("organic matter", single_class_n)
single_class_path <- file.path(root, "single-class-normalized.h5")
single_class_handle <- hdf5r::H5File$new(single_class_path, mode = "w")
single_class_dataset <- single_class_handle$create_dataset(
  "normalized", dtype = hdf5r::h5types$H5T_IEEE_F64LE,
  dims = dim(single_class_normalized),
  chunk_dims = c(100L, single_class_features)
)
single_class_dataset[,] <- single_class_normalized

legacy_single_class_scan <- function() {
  best <- OpenSpecy:::.lib_staged_prune_best_match(
    seq_len(single_class_n), seq_len(single_class_n),
    single_class_dataset, single_class_ids, single_class_features,
    exclude_self = TRUE, query_block_size = 64L,
    candidate_block_size = 500L
  )
  which(single_class_labels[best$index] != "organic matter")
}
optimized_single_class_scan <- function() {
  if (all(!is.na(single_class_labels) &
          single_class_labels == "organic matter")) integer() else {
    legacy_single_class_scan()
  }
}

single_class_rows <- lapply(seq_len(repetitions), function(repetition) {
  legacy_started <- proc.time()[["elapsed"]]
  legacy <- legacy_single_class_scan()
  legacy_elapsed <- proc.time()[["elapsed"]] - legacy_started
  optimized_started <- proc.time()[["elapsed"]]
  optimized <- optimized_single_class_scan()
  optimized_elapsed <- proc.time()[["elapsed"]] - optimized_started
  if (!identical(legacy, optimized)) {
    stop("Single-class pruning shortcut changed removals", call. = FALSE)
  }
  data.table::data.table(
    repetition = repetition, spectra = single_class_n,
    legacy_elapsed_seconds = legacy_elapsed,
    optimized_elapsed_seconds = optimized_elapsed,
    avoided_square_bytes = 8 * as.double(single_class_n)^2
  )
})

# Preserve the full all-candidate scan for sparse opposing-class pools too.
# The adaptive kernel must identify the exact same different-class winners,
# while same-class witnesses can discharge most queries after one block.
minority_labels <- c(
  rep("organic matter", single_class_n - 3L), rep("mineral", 3L)
)
minority_query <- which(minority_labels == "organic matter")
minority_candidates <- seq_len(single_class_n)
minority_candidate_block <- 100L
minority_rows <- lapply(seq_len(repetitions), function(repetition) {
  legacy_started <- proc.time()[["elapsed"]]
  legacy <- OpenSpecy:::.lib_staged_prune_best_match(
    minority_query, minority_candidates, single_class_dataset,
    single_class_ids, single_class_features, exclude_self = TRUE,
    query_block_size = 64L,
    candidate_block_size = minority_candidate_block
  )
  legacy_conflict <- !is.na(legacy$index) &
    minority_labels[legacy$index] != "organic matter"
  legacy_elapsed <- proc.time()[["elapsed"]] - legacy_started

  adaptive_started <- proc.time()[["elapsed"]]
  adaptive <- OpenSpecy:::.lib_staged_prune_conflict_match(
    minority_query, minority_candidates, "organic matter", minority_labels,
    single_class_dataset, single_class_ids, single_class_features,
    query_block_size = 64L,
    candidate_block_size = minority_candidate_block
  )
  adaptive_conflict <- !is.na(adaptive$index)
  adaptive_elapsed <- proc.time()[["elapsed"]] - adaptive_started
  if (!identical(which(legacy_conflict), which(adaptive_conflict)) ||
      !identical(
        legacy$index[legacy_conflict], adaptive$index[adaptive_conflict]
      ) ||
      !isTRUE(all.equal(
        legacy$correlation[legacy_conflict],
        adaptive$correlation[adaptive_conflict],
        tolerance = 0
      ))) {
    stop("Adaptive sparse-conflict pruning changed winning removals",
         call. = FALSE)
  }
  data.table::data.table(
    repetition = repetition, spectra = single_class_n,
    conflicts = sum(legacy_conflict),
    legacy_elapsed_seconds = legacy_elapsed,
    adaptive_elapsed_seconds = adaptive_elapsed,
    max_correlation_delta = max(abs(
      legacy$correlation[legacy_conflict] -
        adaptive$correlation[adaptive_conflict]
    )),
    same_class_blocks_scanned = adaptive$same_class_blocks_scanned,
    early_retained = adaptive$early_retained,
    fallback_queries = adaptive$fallback_queries
  )
})
single_class_handle$close_all()
single_class_result <- data.table::rbindlist(single_class_rows)
if (stats::median(single_class_result$optimized_elapsed_seconds) >=
    stats::median(single_class_result$legacy_elapsed_seconds)) {
  stop("Single-class pruning shortcut did not improve runtime", call. = FALSE)
}
print(single_class_result)
cat(sprintf(
  "single-class median legacy/optimized: %.3fs / %.6fs; avoided %.2f MiB square\n",
  stats::median(single_class_result$legacy_elapsed_seconds),
  stats::median(single_class_result$optimized_elapsed_seconds),
  max(single_class_result$avoided_square_bytes) / 1024^2
))
minority_result <- data.table::rbindlist(minority_rows)
print(minority_result)
if (stats::median(minority_result$adaptive_elapsed_seconds) >=
    stats::median(minority_result$legacy_elapsed_seconds) ||
    any(minority_result$early_retained < 1L)) {
  stop("Adaptive sparse-conflict pruning did not improve runtime",
       call. = FALSE)
}
cat(sprintf(
  paste0(
    "sparse-conflict median legacy/adaptive: %.3fs / %.3fs; ",
    "median early retained: %.0f; median fallback: %.0f\n"
  ),
  stats::median(minority_result$legacy_elapsed_seconds),
  stats::median(minority_result$adaptive_elapsed_seconds),
  stats::median(minority_result$early_retained),
  stats::median(minority_result$fallback_queries)
))
