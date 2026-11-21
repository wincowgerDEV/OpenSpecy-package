# Measure the bounded staged best-match kernel at an explicit representative
# dimension. Override OPENSPECY_PRUNE_PROBE_N/P when a different probe is needed.

devtools::load_all(quiet = TRUE)
stopifnot(requireNamespace("hdf5r", quietly = TRUE))

n <- as.integer(Sys.getenv("OPENSPECY_PRUNE_PROBE_N", unset = "2000"))
p <- as.integer(Sys.getenv("OPENSPECY_PRUNE_PROBE_P", unset = "2000"))
stopifnot(is.finite(n), n > 1L, is.finite(p), p > 2L)
set.seed(463)
normalized <- matrix(rnorm(n * p), nrow = n)
normalized <- normalized / sqrt(rowSums(normalized^2))
ids <- sprintf("probe_%08d", seq_len(n))

path <- tempfile(fileext = ".h5")
on.exit(unlink(path, force = TRUE), add = TRUE)
handle <- hdf5r::H5File$new(path, mode = "w")
dataset <- handle$create_dataset(
  "normalized", dtype = hdf5r::h5types$H5T_IEEE_F64LE,
  dims = c(n, p), chunk_dims = c(min(256L, n), p), gzip_level = 1L
)
dataset[,] <- normalized
rm(normalized)
gc(verbose = FALSE)

elapsed <- system.time({
  result <- OpenSpecy:::.lib_staged_prune_best_match(
    seq_len(n), seq_len(n), dataset, ids, p, exclude_self = TRUE,
    query_block_size = 256L, candidate_block_size = 5000L
  )
})[["elapsed"]]
handle$close_all()

work <- as.double(n) * as.double(n) * as.double(p)
print(data.table::data.table(
  spectra = n, wavenumbers = p, pair_wavenumbers = work,
  elapsed_seconds = elapsed,
  pair_wavenumbers_per_second = work / elapsed,
  max_correlation_bytes = result$largest_correlation_bytes,
  avoided_square_bytes = 8 * as.double(n)^2
))
