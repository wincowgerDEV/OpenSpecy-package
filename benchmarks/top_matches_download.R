# Repeated Top Matches download benchmark.
# Run manually from the package root with:
# Rscript benchmarks/top_matches_download.R
#
# Optional environment variables:
# OPENSPECY_BENCH_REPETITIONS controls legacy/current repetitions (default 3).
# OPENSPECY_BENCH_TOP_MATCH_SPECTRA controls the safe legacy slice (default 25).
# OPENSPECY_BENCH_TOP_N controls matches per uploaded spectrum (default 3).
# This script only uses bundled or already-installed data and never downloads.

installed_library_file <- tryCatch(
  system.file("extdata", "medoid_derivative.rds", package = "OpenSpecy"),
  error = function(error) ""
)
library_candidates <- unique(c(
  installed_library_file,
  file.path(
    tools::R_user_dir("OpenSpecy", "cache"), "reference_libraries",
    "medoid_derivative.rds"
  )
))
library_candidates <- library_candidates[
  nzchar(library_candidates) & file.exists(library_candidates)
]

devtools::load_all(export_all = TRUE, quiet = TRUE)
app_path <- run_app(test_mode = TRUE)
sys.source(file.path(app_path, "global.R"), envir = environment())

positive_integer_env <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if(length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

repetitions <- positive_integer_env("OPENSPECY_BENCH_REPETITIONS", 3L)
legacy_spectra <- positive_integer_env(
  "OPENSPECY_BENCH_TOP_MATCH_SPECTRA", 25L
)
top_n <- positive_integer_env("OPENSPECY_BENCH_TOP_N", 3L)

if(!length(library_candidates)) {
  stop(
    "The installed or cached medoid_derivative library is required; no ",
    "download was attempted.", call. = FALSE
  )
}
library <- suppressWarnings(as_OpenSpecy(readRDS(library_candidates[[1L]])))
if(!check_OpenSpecy(library)) {
  stop("The available medoid_derivative library is not a valid OpenSpecy.",
       call. = FALSE)
}
test_map <- suppressWarnings(read_any(read_extdata("CA_tiny_map.zip")))
legacy_spectra <- min(legacy_spectra, nrow(test_map$metadata))
top_n <- min(top_n, ncol(library$spectra))

make_scores <- function(count) {
  set.seed(20260802L + count)
  matrix(
    stats::runif(ncol(library$spectra) * count, min = -1, max = 1),
    nrow = ncol(library$spectra), ncol = count,
    dimnames = list(
      library$metadata$sample_name,
      test_map$metadata$col_id[seq_len(count)]
    )
  )
}

legacy_scores <- make_scores(legacy_spectra)
legacy_metadata <- test_map$metadata[seq_len(legacy_spectra)]
legacy_snr <- seq(5, 45, length.out = legacy_spectra)
match_threshold <- 0.7
signal_threshold <- 20
quant_columns <- character()

# Retained comparison implementation from the former server download handler.
legacy_top_matches <- function(columns_selected = c("Simple", "All"),
                               profile = FALSE) {
  columns_selected <- match.arg(columns_selected)
  dataR_metadata <- data.table::data.table(
    match_threshold = match_threshold,
    signal_to_noise = legacy_snr,
    signal_threshold = signal_threshold,
    good_signal = legacy_snr > signal_threshold
  ) |>
    dplyr::bind_cols(legacy_metadata)

  expanded <- reshape2::melt(legacy_scores) |>
    data.table::as.data.table() |>
    dplyr::left_join(
      library$metadata |>
        dplyr::select(-dplyr::any_of(c("col_id", "file_name"))),
      by = c("Var1" = "sample_name")
    ) |>
    dplyr::left_join(dataR_metadata, by = c("Var2" = "col_id")) |>
    dplyr::rename(
      "sample_name" = "Var1",
      "col_id" = "Var2",
      "match_val" = "value"
    ) |>
    dplyr::mutate(
      good_match_vals = match_val > match_threshold,
      good_matches = match_val > match_threshold &
        signal_to_noise > signal_threshold
    )
  expanded_bytes <- if(profile) as.numeric(object.size(expanded)) else NA_real_

  keep <- !vapply(expanded, OpenSpecy::is_empty_vector, logical(1)) |
    names(expanded) %in% quant_columns
  result <- expanded[, keep, with = FALSE] |>
    dplyr::select(
      file_name, col_id, material_class, spectrum_identity,
      match_val, signal_to_noise, dplyr::everything()
    )
  result <- result[
    order(-match_val), utils::head(.SD, top_n), by = col_id
  ]
  if(identical(columns_selected, "Simple")) {
    result <- dplyr::select(
      result,
      dplyr::any_of(c(
        "file_name", "col_id", "material_class", "match_val",
        "signal_to_noise", quant_columns
      ))
    )
  }
  result <- result |>
    dplyr::mutate(
      material_class = ifelse(
        match_val < match_threshold, "unknown", material_class
      )
    ) |>
    data.table::as.data.table()

  list(
    result = result,
    expanded_rows = nrow(expanded),
    expanded_bytes = expanded_bytes
  )
}

current_top_matches <- function(scores = legacy_scores,
                                metadata = legacy_metadata,
                                snr = legacy_snr,
                                columns_selected = c("Simple", "All")) {
  app_top_matches_export(
    cor_matrix = scores,
    library_metadata = library$metadata,
    spectrum_metadata = metadata,
    signal_to_noise = snr,
    match_threshold = match_threshold,
    signal_threshold = signal_threshold,
    top_n = top_n,
    columns_selected = match.arg(columns_selected),
    quant_columns = quant_columns
  )
}

elapsed_samples <- function(fun, count) {
  vapply(seq_len(count), function(index) {
    gc(FALSE)
    system.time(invisible(fun()))[["elapsed"]]
  }, numeric(1))
}

csv_bytes <- function(x) {
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  data.table::fwrite(x, file)
  readBin(file, what = "raw", n = file.info(file)$size)
}

results <- lapply(c("Simple", "All"), function(columns_selected) {
  legacy_profile <- legacy_top_matches(columns_selected, profile = TRUE)
  current <- current_top_matches(columns_selected = columns_selected)
  if(!identical(current, legacy_profile$result)) {
    stop(columns_selected, " current and legacy tables are not identical",
         call. = FALSE)
  }
  if(!identical(csv_bytes(current), csv_bytes(legacy_profile$result))) {
    stop(columns_selected, " current and legacy CSV bytes are not identical",
         call. = FALSE)
  }

  legacy_runs <- elapsed_samples(
    function() legacy_top_matches(columns_selected)$result, repetitions
  )
  current_runs <- elapsed_samples(
    function() current_top_matches(columns_selected = columns_selected),
    repetitions
  )
  legacy_median <- stats::median(legacy_runs)
  current_median <- stats::median(current_runs)
  ratio <- current_median / max(legacy_median, .Machine$double.eps)
  if(ratio > 0.90) {
    stop(
      columns_selected, " Top Matches runtime regression: current/legacy = ",
      sprintf("%.3f", ratio), call. = FALSE
    )
  }

  data.frame(
    columns = columns_selected,
    uploaded_spectra = legacy_spectra,
    references = nrow(legacy_scores),
    top_n = top_n,
    legacy_expanded_rows = legacy_profile$expanded_rows,
    current_candidate_rows = nrow(app_top_match_rows(legacy_scores, top_n)),
    legacy_joined_mib = legacy_profile$expanded_bytes / 1024^2,
    current_result_mib = as.numeric(object.size(current)) / 1024^2,
    legacy_median_seconds = legacy_median,
    current_median_seconds = current_median,
    current_to_legacy_ratio = ratio,
    tables_and_csv_identical = TRUE,
    stringsAsFactors = FALSE
  )
})

full_count <- nrow(test_map$metadata)
full_scores <- make_scores(full_count)
full_metadata <- test_map$metadata[seq_len(full_count)]
full_snr <- seq(5, 45, length.out = full_count)
full_runs <- elapsed_samples(
  function() current_top_matches(
    scores = full_scores, metadata = full_metadata, snr = full_snr,
    columns_selected = "All"
  ),
  repetitions
)
full_result <- current_top_matches(
  scores = full_scores, metadata = full_metadata, snr = full_snr,
  columns_selected = "All"
)
stopifnot(nrow(full_result) == top_n * full_count)

cat("\nRepeated legacy/current slice\n")
print(do.call(rbind, results), row.names = FALSE)
cat("\nCurrent full Test Map scale\n")
print(data.frame(
  uploaded_spectra = full_count,
  references = nrow(full_scores),
  top_n = top_n,
  full_matrix_rows_avoided = length(full_scores),
  candidate_rows_enriched = nrow(full_result),
  result_columns = ncol(full_result),
  result_mib = as.numeric(object.size(full_result)) / 1024^2,
  median_seconds = stats::median(full_runs),
  runs_seconds = paste(sprintf("%.3f", full_runs), collapse = ", "),
  stringsAsFactors = FALSE
), row.names = FALSE)
