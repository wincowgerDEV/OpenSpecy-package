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
# OPENSPECY_BENCH_LARGE_ZIP (optional external ENVI ZIP)
# OPENSPECY_BENCH_STOP_AFTER_INGEST (set to true for the hosted-ingest slice)

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

# Keep the former caTools array + two-aperm materialization here as the
# same-output reference for the blockwise ENVI reader. Tests assert the current
# contract; this benchmark owns the retired comparison implementation.
envi_fixture_zip <- read_extdata("CA_tiny_map.zip")
envi_fixture_dir <- tempfile("openspecy-envi-benchmark-")
dir.create(envi_fixture_dir)
on.exit(unlink(envi_fixture_dir, recursive = TRUE), add = TRUE)
utils::unzip(envi_fixture_zip, exdir = envi_fixture_dir)
envi_header_source <- list.files(
  envi_fixture_dir, pattern = "\\.hdr$", recursive = TRUE,
  full.names = TRUE, ignore.case = TRUE
)[1L]
envi_binary_source <- list.files(
  envi_fixture_dir, pattern = "\\.(dat|img)$", recursive = TRUE,
  full.names = TRUE, ignore.case = TRUE
)[1L]
if (is.na(envi_header_source) || is.na(envi_binary_source)) {
  stop("the ENVI benchmark fixture is incomplete", call. = FALSE)
}

# The package fixture is too small for stable relative timing on Windows.
# Repeat complete BIP rows into a temporary ~11 MiB map so a 10% threshold is
# meaningful while every value and pixel-order comparison remains exact.
envi_repeat_rows <- 32L
envi_header <- file.path(envi_fixture_dir, "benchmark.hdr")
envi_binary <- file.path(envi_fixture_dir, "benchmark.dat")
header_lines <- readLines(envi_header_source, warn = FALSE)
source_header <- OpenSpecy:::.read_envi_header(envi_header_source)
header_lines <- sub(
  "^([[:space:]]*lines[[:space:]]*=[[:space:]]*).*$",
  paste0("\\1", as.integer(source_header[["lines"]]) * envi_repeat_rows),
  header_lines, ignore.case = TRUE
)
writeLines(header_lines, envi_header)
source_connection <- file(envi_binary_source, "rb")
source_bytes <- readBin(
  source_connection, raw(), n = file.info(envi_binary_source)$size
)
close(source_connection)
benchmark_connection <- file(envi_binary, "wb")
writeBin(rep(source_bytes, envi_repeat_rows), benchmark_connection)
close(benchmark_connection)
rm(source_bytes)

legacy_envi_materialize <- function() {
  arr <- caTools::read.ENVI(envi_binary, envi_header)
  matrix(aperm(arr, c(3, 2, 1)), nrow = dim(arr)[3],
         ncol = dim(arr)[1] * dim(arr)[2])
}
blockwise_envi_materialize <- function() {
  OpenSpecy:::.read_envi_spectra(
    envi_binary, OpenSpecy:::.read_envi_header(envi_header)
  )
}
legacy_envi <- legacy_envi_materialize()
blockwise_envi <- blockwise_envi_materialize()
if (!identical(blockwise_envi, legacy_envi)) {
  stop("blockwise and former ENVI materializations differ", call. = FALSE)
}
legacy_envi_elapsed <- elapsed_samples(legacy_envi_materialize)
blockwise_envi_elapsed <- elapsed_samples(blockwise_envi_materialize)
envi_runtime_ratio <- stats::median(blockwise_envi_elapsed) /
  max(stats::median(legacy_envi_elapsed), .Machine$double.eps)
if (envi_runtime_ratio > 1.50) {
  stop(
    "material blockwise ENVI runtime regression: blockwise/legacy = ",
    sprintf("%.3f", envi_runtime_ratio), " (failure limit 1.50)",
    call. = FALSE
  )
}
print(data.frame(
  envi_pixels = ncol(blockwise_envi),
  envi_bands = nrow(blockwise_envi),
  repetitions = repetitions,
  legacy_median_seconds = stats::median(legacy_envi_elapsed),
  blockwise_median_seconds = stats::median(blockwise_envi_elapsed),
  blockwise_to_legacy_runtime = envi_runtime_ratio,
  material_runtime_limit = 1.10,
  failure_runtime_limit = 1.50,
  material_runtime_regression = envi_runtime_ratio > 1.10,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)
rm(legacy_envi, blockwise_envi)

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

# Repeated native-upload proxy versus browser-mounted-path ingestion. The
# native case includes a full temporary-file copy before the ordinary reader;
# the mounted case starts at the selected path because the browser bridge has
# already exposed the File through WORKERFS. Both must materialize the same
# complete OpenSpecy object before downstream work begins.
materialize_ingest <- function(paths) {
  members <- read_any(paths, c_spec = FALSE)
  combined <- if (is_OpenSpecy(members)) {
    members
  } else {
    c_spec(members, range = "common", res = 8)
  }
  manage_na(combined, ig = c(NA, 0), type = "remove")
}

ingest_fixture <- read_extdata("CA_tiny_map.zip")
mounted_ingest <- function() materialize_ingest(ingest_fixture)
native_ingest <- function() {
  uploaded <- tempfile(fileext = ".zip")
  on.exit(unlink(uploaded), add = TRUE)
  if (!file.copy(ingest_fixture, uploaded, overwrite = TRUE)) {
    stop("unable to create the native-upload benchmark copy", call. = FALSE)
  }
  materialize_ingest(uploaded)
}

mounted_reference <- mounted_ingest()
native_reference <- native_ingest()
if (!isTRUE(all.equal(mounted_reference, native_reference,
                      tolerance = 1e-14))) {
  stop("mounted-path and native-copy materialized objects differ",
       call. = FALSE)
}
mounted_ingest_elapsed <- elapsed_samples(mounted_ingest)
native_ingest_elapsed <- elapsed_samples(native_ingest)
mounted_ingest_median <- stats::median(mounted_ingest_elapsed)
native_ingest_median <- stats::median(native_ingest_elapsed)
ingest_runtime_ratio <- mounted_ingest_median /
  max(native_ingest_median, .Machine$double.eps)
ingest_failure_limit <- 1.50
if (ingest_runtime_ratio > ingest_failure_limit) {
  stop(
    "material mounted-path ingestion regression: mounted/native = ",
    sprintf("%.3f", ingest_runtime_ratio), " (failure limit ",
    ingest_failure_limit, ")", call. = FALSE
  )
}
print(data.frame(
  ingest_fixture_bytes = file.info(ingest_fixture)$size,
  materialized_mib = as.numeric(object.size(mounted_reference)) / 1024^2,
  repetitions = repetitions,
  native_copy_read_median_seconds = native_ingest_median,
  mounted_read_median_seconds = mounted_ingest_median,
  mounted_to_native_runtime = ingest_runtime_ratio,
  material_runtime_limit = 1.10,
  failure_runtime_limit = ingest_failure_limit,
  material_runtime_regression = ingest_runtime_ratio > 1.10,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)

# An external large ENVI ZIP is deliberately opt-in. It is read once through
# the ZIP route and once as its extracted HDR+DAT pair, never copied into the
# repository. Only compact signatures survive between reads so the benchmark
# does not retain two gigabyte-scale matrices at once.
large_zip <- Sys.getenv("OPENSPECY_BENCH_LARGE_ZIP", "")
if (nzchar(large_zip)) {
  if (!file.exists(large_zip)) {
    stop("OPENSPECY_BENCH_LARGE_ZIP does not exist: ", large_zip,
         call. = FALSE)
  }
  object_signature <- function(x) {
    rows <- unique(c(1L, max(1L, nrow(x$spectra) %/% 2L), nrow(x$spectra)))
    columns <- unique(c(1L, max(1L, ncol(x$spectra) %/% 2L),
                        ncol(x$spectra)))
    list(
      dimensions = dim(x$spectra),
      wavenumber = range(x$wavenumber),
      values = unname(x$spectra[rows, columns, drop = FALSE]),
      metadata_names = names(x$metadata),
      metadata_rows = nrow(x$metadata)
    )
  }

  gc(FALSE)
  zip_timing <- system.time(zip_object <- materialize_ingest(large_zip))
  zip_signature <- object_signature(zip_object)
  materialized_bytes <- as.numeric(object.size(zip_object))
  rm(zip_object)
  gc(FALSE)

  listing <- utils::unzip(large_zip, list = TRUE)$Name
  members <- listing[grepl("\\.(hdr|dat|img)$", listing, ignore.case = TRUE)]
  if (!any(grepl("\\.hdr$", members, ignore.case = TRUE)) ||
      !any(grepl("\\.(dat|img)$", members, ignore.case = TRUE))) {
    stop("external ZIP does not contain an ENVI HDR+DAT/IMG pair",
         call. = FALSE)
  }
  extracted <- tempfile("openspecy-envi-")
  dir.create(extracted)
  on.exit(unlink(extracted, recursive = TRUE), add = TRUE)
  extract_timing <- system.time(utils::unzip(
    large_zip, files = members, exdir = extracted
  ))
  direct_paths <- file.path(extracted, members)
  direct_paths <- direct_paths[grepl("\\.(hdr|dat|img)$", direct_paths,
                                     ignore.case = TRUE)]
  gc(FALSE)
  direct_timing <- system.time(direct_object <- materialize_ingest(direct_paths))
  direct_signature <- object_signature(direct_object)
  if (!isTRUE(all.equal(zip_signature, direct_signature,
                        tolerance = 1e-14))) {
    stop("ZIP and direct ENVI materialization signatures differ",
         call. = FALSE)
  }
  print(data.frame(
    external_zip = normalizePath(large_zip, winslash = "/"),
    zip_bytes = file.info(large_zip)$size,
    materialized_mib = materialized_bytes / 1024^2,
    spectra = zip_signature$dimensions[[2L]],
    wavenumbers = zip_signature$dimensions[[1L]],
    zip_read_seconds = unname(zip_timing[["elapsed"]]),
    extract_seconds = unname(extract_timing[["elapsed"]]),
    direct_pair_read_seconds = unname(direct_timing[["elapsed"]]),
    equivalent = TRUE,
    stringsAsFactors = FALSE
  ), row.names = FALSE)
}

if (identical(tolower(Sys.getenv("OPENSPECY_BENCH_STOP_AFTER_INGEST", "")),
              "true")) {
  quit(save = "no", status = 0L)
}

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

# Structural validation formerly materialized a full logical matrix for
# `!is.na(spectra)`. The bounded implementation holds one row at a time and
# exits as soon as every spectrum has two valid values.
legacy_validate_values <- function() {
  !any(colSums(!is.na(na_spectra)) < 2L)
}
current_validate_values <- function() {
  OpenSpecy:::.spectra_have_valid_values(na_spectra, minimum = 2L)
}
if(!identical(current_validate_values(), legacy_validate_values())) {
  stop("bounded and former structural validation differ", call. = FALSE)
}
legacy_validation_elapsed <- elapsed_samples(legacy_validate_values)
current_validation_elapsed <- elapsed_samples(current_validate_values)
validation_runtime_ratio <- stats::median(current_validation_elapsed) /
  max(stats::median(legacy_validation_elapsed), .Machine$double.eps)
validation_failure_limit <- 1.25
if(validation_runtime_ratio > validation_failure_limit) {
  stop(
    "material bounded-validation runtime regression: current/old = ",
    sprintf("%.3f", validation_runtime_ratio), " (failure limit ",
    validation_failure_limit, ")", call. = FALSE
  )
}
print(data.frame(
  validation_wavenumbers = na_wavenumber_count,
  validation_spectra = na_spectrum_count,
  repetitions = repetitions,
  former_mask_mib = 4 * na_wavenumber_count * na_spectrum_count / 1024^2,
  current_row_mib = 16 * na_spectrum_count / 1024^2,
  former_median_seconds = stats::median(legacy_validation_elapsed),
  current_median_seconds = stats::median(current_validation_elapsed),
  current_to_former_runtime = validation_runtime_ratio,
  failure_runtime_limit = validation_failure_limit,
  equivalent = TRUE,
  stringsAsFactors = FALSE
), row.names = FALSE)
