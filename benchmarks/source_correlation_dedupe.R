# Compare the source-local streaming correlation deduplicator with its frozen
# full-matrix reference. Run from the package root with:
# Rscript benchmarks/source_correlation_dedupe.R
# Override representative dimensions with OPENSPECY_DEDUPE_BENCHMARK_N,
# OPENSPECY_DEDUPE_BENCHMARK_P, OPENSPECY_DEDUPE_BENCHMARK_GROUP, and
# OPENSPECY_DEDUPE_BENCHMARK_REPETITIONS.

devtools::load_all(quiet = TRUE)

main <- function() {
  set.seed(20261020)
  n <- as.integer(Sys.getenv(
    "OPENSPECY_DEDUPE_BENCHMARK_N", unset = "2000"
  ))
  p <- as.integer(Sys.getenv(
    "OPENSPECY_DEDUPE_BENCHMARK_P", unset = "600"
  ))
  group_size <- as.integer(Sys.getenv(
    "OPENSPECY_DEDUPE_BENCHMARK_GROUP", unset = "500"
  ))
  repetitions <- as.integer(Sys.getenv(
    "OPENSPECY_DEDUPE_BENCHMARK_REPETITIONS", unset = "3"
  ))
  threshold <- 0.99
  query_block_size <- 64L
  survivor_block_size <- 256L
  stopifnot(
    n > group_size, p > 20L, group_size > query_block_size,
    repetitions > 0L
  )

  spectra <- matrix(rnorm(p * n), nrow = p, ncol = n)
  spectra <- apply(spectra, 2L, cumsum)
  duplicate_rows <- seq.int(10L, n, by = 10L)
  spectra[, duplicate_rows] <- spectra[, duplicate_rows - 1L, drop = FALSE] +
    matrix(rnorm(p * length(duplicate_rows), sd = 1e-4), nrow = p)
  ids <- sprintf("source-dedupe-%06d", seq_len(n))
  colnames(spectra) <- ids
  materials <- sprintf(
    "benchmark-material-%03d", ceiling(seq_len(n) / group_size)
  )
  object <- as_OpenSpecy(
    seq_len(p), spectra = spectra,
    metadata = data.table::data.table(
      sample_name = ids, library_name = "benchmark-library",
      spectrum_type = "raman", material = materials,
      spectrum_identity = materials
    ),
    attributes = list(intensity_unit = "arbitrary units")
  )

  full_matrix_reference <- function() {
    groups <- split(seq_len(n), materials)
    removed <- integer()
    survivors_for_removed <- integer()
    correlations <- numeric()
    for (rows in groups) {
      normalized <- OpenSpecy:::.lib_correlation_normalize_columns(object, rows)
      dense <- tcrossprod(normalized)
      retained <- integer()
      for (local_row in seq_along(rows)) {
        if (!length(retained)) {
          retained <- local_row
          next
        }
        scores <- dense[local_row, retained]
        scores[!is.finite(scores)] <- -Inf
        best <- which.max(scores)
        if (scores[[best]] > threshold) {
          removed <- c(removed, rows[[local_row]])
          survivors_for_removed <- c(survivors_for_removed,
                                     rows[retained[[best]]])
          correlations <- c(correlations, scores[[best]])
        } else {
          retained <- c(retained, local_row)
        }
      }
    }
    data.table::data.table(
      source_row = removed, spectrum_id = ids[removed],
      survivor_row = survivors_for_removed,
      survivor_id = ids[survivors_for_removed], correlation = correlations
    )
  }

  streamed <- function() {
    OpenSpecy:::.lib_stream_correlation_dedupe(
      object, threshold = threshold, query_block_size = query_block_size,
      survivor_block_size = survivor_block_size, progress = FALSE
    )
  }

  previous <- full_matrix_reference()
  current <- streamed()
  stopifnot(
    identical(previous$source_row, current$removals$source_row),
    identical(previous$survivor_row, current$removals$survivor_row),
    isTRUE(all.equal(
      previous$correlation, current$removals$correlation,
      tolerance = 1e-12
    )),
    max(current$groups$max_correlation_cells) <=
      query_block_size * survivor_block_size,
    max(current$groups$max_correlation_bytes) <
      max(current$groups$full_matrix_bytes)
  )

  elapsed <- function(fun) {
    median(replicate(repetitions, system.time(fun())[["elapsed"]]))
  }
  dense_seconds <- elapsed(full_matrix_reference)
  streamed_seconds <- elapsed(streamed)
  result <- data.frame(
    implementation = c("frozen_full_matrix", "streamed_survivor_blocks"),
    seconds = c(dense_seconds, streamed_seconds),
    peak_correlation_mib = c(
      max(current$groups$full_matrix_bytes),
      max(current$groups$max_correlation_bytes)
    ) / 1024^2,
    removed = nrow(current$removals)
  )
  print(result, row.names = FALSE)
  cat(sprintf(
    paste0("bounded peak/full-matrix peak: %.4f; elapsed ratio: %.2fx; ",
           "largest group: %d spectra x %d features\n"),
    max(current$groups$max_correlation_bytes) /
      max(current$groups$full_matrix_bytes),
    streamed_seconds / dense_seconds, max(current$groups$spectra), p
  ))
}

main()
