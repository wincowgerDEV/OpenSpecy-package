# Repeated bounded FileSpecs connected-mean benchmark.
#
# Run the portable synthetic case:
#   Rscript benchmarks/shinylive_filespec_collapse.R
# Or append a genuine ENVI DAT/HDR pair for a full current-kernel run. The
# retired full-map kernel is projected but deliberately never allocated there.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this benchmark.", call. = FALSE)
}
devtools::load_all(quiet = TRUE)
app_env <- new.env(parent = globalenv())
sys.source(file.path(run_app(test_mode = TRUE), "global.R"), envir = app_env)

make_fixture <- function(directory, rows = 48L, columns = 64L, bands = 128L) {
  axis <- seq(600, by = 4, length.out = bands)
  # BIP stores every sample across one line before advancing to the next line.
  grid <- expand.grid(col = seq_len(columns), row = seq_len(rows))
  retained <- with(grid,
    (row >= 8 & row <= 22 & col >= 7 & col <= 20) |
    (row >= 28 & row <= 43 & col >= 38 & col <= 58)
  )
  set.seed(3302)
  values <- matrix(stats::rnorm(bands * nrow(grid), sd = 0.01), nrow = bands)
  profile <- exp(-((seq_len(bands) - bands / 2) / 14)^2)
  values[, retained] <- sweep(values[, retained, drop = FALSE], 1L,
                              profile, "+")
  header <- file.path(directory, "benchmark.hdr")
  binary <- file.path(directory, "benchmark.dat")
  writeLines(c(
    "ENVI", paste0("samples = ", columns), paste0("lines = ", rows),
    paste0("bands = ", bands), "header offset = 0", "data type = 4",
    "interleave = bip", "byte order = 0",
    paste0("wavelength = {", paste(axis, collapse = ", "), "}")
  ), header)
  connection <- file(binary, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.numeric(values), connection, size = 4L, endian = "little")
  list(header = header, binary = binary, retained = retained, axis = axis)
}

# Retired app kernel: materialize every retained spectrum as one dense matrix,
# then collapse it. This is intentionally run only on the bounded fixture.
legacy_collapse <- function(x, eligible) {
  eager <- decompress_spec(x, region = "Region1")
  OpenSpecy:::.partition_particle_map(
    eager, eligible = eligible, strategy = "collapse",
    collapse_function = mean, area_threshold = 1L
  )
}
current_collapse <- function(x, eligible) {
  OpenSpecy:::.filespec_collapse_connected_mean(
    x, eligible = eligible, area_threshold = 1L,
    spectral_smooth = FALSE, chunk_size = 8192L
  )
}

directory <- tempfile("filespec-collapse-benchmark-")
dir.create(directory)
on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
fixture <- make_fixture(directory)
source_hash <- digest::digest(fixture$binary, algo = "sha256", file = TRUE)
x <- open_specs(c(fixture$binary, fixture$header),
                cache_dir = file.path(directory, "cache"))
old <- legacy_collapse(x, fixture$retained)
current <- current_collapse(x, fixture$retained)
stopifnot(isTRUE(all.equal(
  old$analysis_units$spectra, current$analysis_units$spectra,
  tolerance = 1e-10, check.attributes = FALSE
)))

elapsed <- function(fun, n = 5L) {
  replicate(n, unname(system.time(invisible(fun()))[["elapsed"]]))
}
old_time <- elapsed(function() legacy_collapse(x, fixture$retained))
current_time <- elapsed(function() current_collapse(x, fixture$retained))
print(data.frame(
  kernel = c("legacy dense retained map", "current particle stream"),
  median_seconds = c(stats::median(old_time), stats::median(current_time))
), row.names = FALSE)
stopifnot(identical(
  source_hash, digest::digest(fixture$binary, algo = "sha256", file = TRUE)
))

# Correlation-threshold identification deliberately trades dense-map speed for
# bounded memory. The eager oracle is retained here for identical-output
# evidence; production maps use the streamed kernel because a full processed
# query plus library-by-query correlation matrix need not fit in memory.
library <- decompress_spec(x, index = seq_len(12L))
prepared_library <- app_env$app_prepare_correlation_reference(library)
eager_best <- function() {
  query <- decompress_spec(x, index = which(fixture$retained))
  OpenSpecy:::.match_spec_blockwise(
    query, library, top_n = 1L, block_size = ncol(query$spectra),
    conform = FALSE, type = "roll"
  )
}
stream_best <- function() {
  app_env$app_stream_filespec_best_matches(
    x, fixture$retained, process = identity, chunk_size = 64L,
    identify = function(query) {
      app_env$app_match_prepared_best(
        query, prepared_library, library_block_size = 6L
      )
    }
  )
}
eager_matches <- eager_best()
streamed_matches <- stream_best()
stopifnot(isTRUE(all.equal(
  eager_matches, streamed_matches, tolerance = 1e-12,
  check.attributes = FALSE
)))
eager_match_time <- elapsed(eager_best)
stream_match_time <- elapsed(stream_best)
print(data.frame(
  kernel = c("eager pixel match", "bounded file-backed pixel match"),
  median_seconds = c(
    stats::median(eager_match_time), stats::median(stream_match_time)
  )
), row.names = FALSE)
cat(sprintf(
  "Fixture dense correlation allocation %.2f MiB; streamed block %.2f MiB.\n",
  sum(fixture$retained) * ncol(library$spectra) * 8 / 1024^2,
  64 * ncol(library$spectra) * 8 / 1024^2
))

arguments <- commandArgs(trailingOnly = TRUE)
if(length(arguments) == 2L && all(file.exists(arguments))) {
  cache <- tempfile("filespec-genuine-cache-")
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)
  genuine <- open_specs(arguments, cache_dir = cache)
  index <- OpenSpecy:::.filespec_index(genuine)
  axis <- OpenSpecy:::.filespec_axis(genuine)
  snr_time <- system.time({
    snr <- OpenSpecy:::.filespec_particle_snr(
      genuine, index, seq_along(axis), "sig_times_noise", FALSE,
      FALSE, c(1, 1, 1), 8192L
    )
  })
  eligible <- is.finite(snr) & snr > 0.01
  projected_legacy <- sum(eligible) * length(axis) * 8
  collapse_time <- system.time({
    result <- current_collapse(genuine, eligible)
  })
  units <- if(is.null(result$analysis_units)) 0L else
    ncol(result$analysis_units$spectra)
  cat(sprintf(
    paste0("Genuine source: %d spectra, %d retained, %d units; S/N %.2fs; ",
           "collapse %.2fs; retired retained-map allocation %.2f MiB ",
           "(not allocated).\n"),
    nrow(index), sum(eligible), units, snr_time[["elapsed"]],
    collapse_time[["elapsed"]], projected_legacy / 1024^2
  ))
} else {
  cat("Genuine ENVI benchmark skipped; append existing DAT and HDR paths.\n")
}
