# Repeated blockwise top-N matching benchmark.
# Run manually from the package root with:
# Rscript benchmarks/particle_in_memory_workflow.R
#
# Optional environment variables:
# OPENSPECY_BENCH_REPETITIONS (default 3)
# OPENSPECY_BENCH_QUERY_SPECTRA (default 1600)
# OPENSPECY_BENCH_LIBRARY_SPECTRA (default 2000)
# OPENSPECY_BENCH_WAVENUMBERS (default 512)
# OPENSPECY_BENCH_TOP_N (default 10)
# OPENSPECY_BENCH_BLOCK_SIZE (default 100)
# OPENSPECY_BENCH_NA_WAVENUMBERS (default 1024)
# OPENSPECY_BENCH_NA_SPECTRA (default 5000)
# OPENSPECY_BENCH_H5_WAVENUMBERS (default 256)
# OPENSPECY_BENCH_H5_REGIONS (default 4)
# OPENSPECY_BENCH_H5_ROWS (default 32)
# OPENSPECY_BENCH_H5_COLUMNS (default 32)

devtools::load_all(export_all = TRUE, quiet = TRUE)

positive_integer_env <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if(length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

repetitions <- positive_integer_env("OPENSPECY_BENCH_REPETITIONS", 3L)
query_count <- positive_integer_env("OPENSPECY_BENCH_QUERY_SPECTRA", 1600L)
library_count <- positive_integer_env("OPENSPECY_BENCH_LIBRARY_SPECTRA", 2000L)
wavenumber_count <- positive_integer_env("OPENSPECY_BENCH_WAVENUMBERS", 512L)
top_n <- positive_integer_env("OPENSPECY_BENCH_TOP_N", 10L)
block_size <- positive_integer_env("OPENSPECY_BENCH_BLOCK_SIZE", 100L)
top_n <- min(top_n, library_count)

set.seed(20260813L)
wavenumber <- seq_len(wavenumber_count)
library <- as_OpenSpecy(
  wavenumber,
  spectra = matrix(
    stats::rnorm(wavenumber_count * library_count),
    nrow = wavenumber_count,
    dimnames = list(NULL, sprintf("library_%04d", seq_len(library_count)))
  )
)
query <- as_OpenSpecy(
  wavenumber,
  spectra = matrix(
    stats::rnorm(wavenumber_count * query_count),
    nrow = wavenumber_count,
    dimnames = list(NULL, sprintf("query_%05d", seq_len(query_count)))
  )
)

canonical_full_match <- function() {
  scores <- cor_spec(query, library, compute = "optimized")
  matches <- suppressMessages(ident_spec(
    scores, query, library, top_n = top_n
  ))
  object_order <- match(matches$object_id, colnames(scores))
  library_order <- match(matches$library_id, rownames(scores))
  matches[
    order(object_order, -matches$match_val, library_order, na.last = TRUE)
  ]
}

blockwise_match <- function() {
  OpenSpecy:::.match_spec_blockwise(
    query, library, top_n = top_n, block_size = block_size
  )
}

reference <- canonical_full_match()
current <- blockwise_match()
if(!isTRUE(all.equal(current, reference, tolerance = 1e-14))) {
  stop("blockwise and full-matrix top-N results differ", call. = FALSE)
}

elapsed_samples <- function(fun) {
  vapply(seq_len(repetitions), function(index) {
    iterations <- 1L
    repeat {
      gc(FALSE)
      elapsed <- system.time({
        for(inner in seq_len(iterations)) invisible(fun())
      })[["elapsed"]]
      if(elapsed >= 0.05 || iterations >= 128L) break
      iterations <- iterations * 2L
    }
    elapsed / iterations
  }, numeric(1))
}

full_elapsed <- elapsed_samples(canonical_full_match)
block_elapsed <- elapsed_samples(blockwise_match)
full_median <- stats::median(full_elapsed)
block_median <- stats::median(block_elapsed)
runtime_ratio <- block_median / max(full_median, .Machine$double.eps)
material_runtime_limit <- 1.10
failure_runtime_limit <- 1.50
runtime_regression <- runtime_ratio > material_runtime_limit

full_matrix_bytes <- 8 * library_count * query_count
retained_bytes <- as.numeric(object.size(current))
storage_ratio <- retained_bytes / full_matrix_bytes
if(retained_bytes >= full_matrix_bytes) {
  stop("blockwise top-N storage is not smaller than the full matrix",
       call. = FALSE)
}
if(runtime_ratio > failure_runtime_limit) {
  stop(
    "material blockwise matching runtime regression: block/full = ",
    sprintf("%.3f", runtime_ratio), " (failure limit ",
    failure_runtime_limit, ")",
    call. = FALSE
  )
}

print(data.frame(
  queries = query_count,
  references = library_count,
  wavenumbers = wavenumber_count,
  top_n = top_n,
  block_size = block_size,
  repetitions = repetitions,
  full_matrix_mib = full_matrix_bytes / 1024^2,
  retained_top_n_mib = retained_bytes / 1024^2,
  retained_to_full_storage = storage_ratio,
  full_median_seconds = full_median,
  block_median_seconds = block_median,
  block_to_full_runtime = runtime_ratio,
  material_runtime_limit = material_runtime_limit,
  failure_runtime_limit = failure_runtime_limit,
  material_runtime_regression = runtime_regression,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)

# Repeated H5 region-assembly benchmark. Input cubes stand in for data already
# read from each H5 region so this isolates the former list-plus-cbind peak from
# the current single preallocated output matrix without requiring an H5 file.
h5_wavenumber_count <- positive_integer_env(
  "OPENSPECY_BENCH_H5_WAVENUMBERS", 256L
)
h5_region_count <- positive_integer_env("OPENSPECY_BENCH_H5_REGIONS", 4L)
h5_row_count <- positive_integer_env("OPENSPECY_BENCH_H5_ROWS", 32L)
h5_column_count <- positive_integer_env("OPENSPECY_BENCH_H5_COLUMNS", 32L)

set.seed(20260814L)
h5_cubes <- lapply(seq_len(h5_region_count), function(region) {
  array(
    stats::rnorm(h5_wavenumber_count * h5_row_count * h5_column_count),
    dim = c(h5_wavenumber_count, h5_row_count, h5_column_count)
  )
})

legacy_h5_assemble <- function() {
  regions <- lapply(h5_cubes, function(cube) {
    matrix(cube, nrow = h5_wavenumber_count,
           ncol = h5_row_count * h5_column_count)
  })
  do.call(cbind, regions)
}

current_h5_assemble <- function() {
  columns_per_region <- h5_row_count * h5_column_count
  spectra <- matrix(
    NA_real_, nrow = h5_wavenumber_count,
    ncol = h5_region_count * columns_per_region
  )
  for(region in seq_len(h5_region_count)) {
    columns <- seq.int(
      (region - 1L) * columns_per_region + 1L,
      region * columns_per_region
    )
    spectra[, columns] <- matrix(
      h5_cubes[[region]], nrow = h5_wavenumber_count,
      ncol = columns_per_region
    )
  }
  spectra
}

legacy_h5 <- legacy_h5_assemble()
current_h5 <- current_h5_assemble()
if(!identical(current_h5, legacy_h5)) {
  stop("preallocated and former H5 region assembly differ", call. = FALSE)
}

legacy_h5_elapsed <- elapsed_samples(legacy_h5_assemble)
current_h5_elapsed <- elapsed_samples(current_h5_assemble)
legacy_h5_median <- stats::median(legacy_h5_elapsed)
current_h5_median <- stats::median(current_h5_elapsed)
h5_runtime_ratio <- current_h5_median /
  max(legacy_h5_median, .Machine$double.eps)
h5_failure_runtime_limit <- 1.50
if(h5_runtime_ratio > h5_failure_runtime_limit) {
  stop(
    "material preallocated H5 assembly runtime regression: current/old = ",
    sprintf("%.3f", h5_runtime_ratio), " (failure limit ",
    h5_failure_runtime_limit, ")", call. = FALSE
  )
}

h5_output_bytes <- as.numeric(object.size(current_h5))
h5_largest_region_bytes <- as.numeric(object.size(h5_cubes[[1L]]))
former_h5_temporary_bytes <- 2 * h5_output_bytes
current_h5_temporary_bytes <- h5_output_bytes + h5_largest_region_bytes
print(data.frame(
  h5_wavenumbers = h5_wavenumber_count,
  h5_regions = h5_region_count,
  h5_rows = h5_row_count,
  h5_columns = h5_column_count,
  repetitions = repetitions,
  former_peak_mib = former_h5_temporary_bytes / 1024^2,
  current_peak_mib = current_h5_temporary_bytes / 1024^2,
  current_to_former_temporary_storage = current_h5_temporary_bytes /
    former_h5_temporary_bytes,
  former_median_seconds = legacy_h5_median,
  current_median_seconds = current_h5_median,
  current_to_former_runtime = h5_runtime_ratio,
  failure_runtime_limit = h5_failure_runtime_limit,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)

# Repeated upload-cleanup benchmark. The former implementation materialized
# raw, leading, trailing, and combined full-size logical masks. The current
# implementation scans only the boundary rows needed to find the common valid
# range.
na_wavenumber_count <- positive_integer_env(
  "OPENSPECY_BENCH_NA_WAVENUMBERS", 1024L
)
na_spectrum_count <- positive_integer_env(
  "OPENSPECY_BENCH_NA_SPECTRA", 5000L
)
na_spectra <- matrix(1, nrow = na_wavenumber_count,
                     ncol = na_spectrum_count)
boundary_width <- min(4L, max(1L, na_wavenumber_count %/% 4L))
na_spectra[seq_len(boundary_width), seq(1L, na_spectrum_count, by = 2L)] <- 0
na_spectra[
  na_wavenumber_count - seq_len(boundary_width) + 1L,
  seq(2L, na_spectrum_count, by = 2L)
] <- NA_real_

legacy_remove_rows <- function() {
  ignored <- is.na(na_spectra)
  matched <- na_spectra == 0
  matched[is.na(matched)] <- FALSE
  ignored <- ignored | matched
  leading <- ignored
  trailing <- ignored
  if (nrow(ignored) > 1L) {
    for (index in 2:nrow(ignored)) {
      leading[index, ] <- leading[index, ] & leading[index - 1L, ]
    }
    for (index in (nrow(ignored) - 1L):1L) {
      trailing[index, ] <- trailing[index, ] & trailing[index + 1L, ]
    }
  }
  rowSums(leading | trailing) == 0L
}

current_remove_rows <- function() {
  OpenSpecy:::.rows_without_ignored_values(
    na_spectra, lead_tail_only = TRUE, ig = c(NA, 0)
  )
}

legacy_rows <- legacy_remove_rows()
current_rows <- current_remove_rows()
if (!identical(current_rows, legacy_rows)) {
  stop("matrix-native and former NA-removal rows differ", call. = FALSE)
}

legacy_na_elapsed <- elapsed_samples(legacy_remove_rows)
current_na_elapsed <- elapsed_samples(current_remove_rows)
legacy_na_median <- stats::median(legacy_na_elapsed)
current_na_median <- stats::median(current_na_elapsed)
na_runtime_ratio <- current_na_median /
  max(legacy_na_median, .Machine$double.eps)
na_failure_runtime_limit <- 1.25
if (na_runtime_ratio > na_failure_runtime_limit) {
  stop(
    "material matrix-native NA-removal runtime regression: current/old = ",
    sprintf("%.3f", na_runtime_ratio), " (failure limit ",
    na_failure_runtime_limit, ")",
    call. = FALSE
  )
}

logical_cell_bytes <- 4
former_mask_bytes <- 4 * logical_cell_bytes *
  na_wavenumber_count * na_spectrum_count
# Conservative allowance for one numeric row and several logical row vectors.
current_scan_bytes <- 32 * na_spectrum_count
print(data.frame(
  na_wavenumbers = na_wavenumber_count,
  na_spectra = na_spectrum_count,
  repetitions = repetitions,
  former_mask_mib = former_mask_bytes / 1024^2,
  current_scan_mib = current_scan_bytes / 1024^2,
  current_to_former_temporary_storage = current_scan_bytes /
    former_mask_bytes,
  former_median_seconds = legacy_na_median,
  current_median_seconds = current_na_median,
  current_to_former_runtime = na_runtime_ratio,
  failure_runtime_limit = na_failure_runtime_limit,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)
