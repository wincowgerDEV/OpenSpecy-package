# Repeated same-output benchmark for the pre-v23 full-table metadata merge
# versus the bounded match/update/report path used by production build_lib().
suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
})

load_all(".", quiet = TRUE)

legacy_join <- function(metadata, lookup, by, suffixes = c(".x", ".y")) {
  metadata <- as.data.table(copy(metadata))
  lookup <- as.data.table(copy(lookup))
  metadata_key <- as.character(metadata[[by]])
  lookup_key <- as.character(lookup[[by]])
  lookup_values <- lookup[, setdiff(names(lookup), by), with = FALSE]
  meta <- copy(metadata)
  look <- copy(lookup_values)
  meta$..join_key <- metadata_key
  meta$..row_id <- seq_len(nrow(meta))
  look$..join_key <- lookup_key
  joined <- merge(
    meta, look, by = "..join_key", all.x = TRUE, sort = FALSE,
    suffixes = suffixes
  )
  setorder(joined, "..row_id")
  joined[, c("..join_key", "..row_id") := NULL]
  joined
}

make_fixture <- function(n = 200000L, keys = 10000L) {
  key <- sprintf("identity_%05d", rep(seq_len(keys), length.out = n))
  metadata <- data.table(
    spectrum_identity = key,
    sample_name = sprintf("sample_%07d", seq_len(n)),
    organization = rep(sprintf("library_%03d", 1:100), length.out = n),
    material = rep(NA_character_, n),
    citation = paste0("citation_", seq_len(n))
  )
  lookup <- data.table(
    spectrum_identity = sprintf("identity_%05d", seq_len(keys)),
    material = sprintf("material_%05d", seq_len(keys)),
    material_class = rep(sprintf("class_%03d", 1:250), length.out = keys)
  )
  list(metadata = metadata, lookup = lookup)
}

fixture <- make_fixture()
repetitions <- 3L
legacy_seconds <- numeric(repetitions)
bounded_seconds <- numeric(repetitions)
for (i in seq_len(repetitions)) {
  gc(verbose = FALSE, full = TRUE)
  legacy_seconds[[i]] <- system.time({
    legacy <- legacy_join(fixture$metadata, fixture$lookup,
                          "spectrum_identity")
  })[["elapsed"]]
  gc(verbose = FALSE, full = TRUE)
  bounded_seconds[[i]] <- system.time({
    bounded_result <- suppressWarnings(OpenSpecy:::.lib_join_metadata_table(
      copy(fixture$metadata), fixture$lookup, by = "spectrum_identity",
      copy_metadata = FALSE, chunk_size = 10000L,
      report_value_limit = 1000L, coalesce_existing = TRUE
    ))
    bounded <- bounded_result$data
  })[["elapsed"]]
  legacy <- OpenSpecy:::.lib_coalesce_joined_metadata(
    legacy, "material", lookup_precedence = TRUE
  )
  comparison <- all.equal(
    as.data.frame(legacy),
    as.data.frame(bounded[, names(legacy), with = FALSE]),
    check.attributes = FALSE
  )
  if (!isTRUE(comparison)) {
    stop("Bounded metadata join changed values: ",
         paste(comparison, collapse = "; "), call. = FALSE)
  }
}

metadata_bytes <- as.numeric(object.size(fixture$metadata))
legacy_bytes <- as.numeric(object.size(legacy))
bounded_bytes <- as.numeric(object.size(bounded))
key_bytes <- as.numeric(object.size(as.character(
  fixture$metadata$spectrum_identity
)))
matched_bytes <- 4 * nrow(fixture$metadata)
legacy_modeled_peak <- 2 * metadata_bytes + legacy_bytes + 2 * key_bytes
bounded_modeled_peak <- metadata_bytes + (bounded_bytes - metadata_bytes) +
  key_bytes + matched_bytes + 10000 * 16

result <- data.table(
  implementation = c("legacy_full_merge", "bounded_in_place"),
  repetitions = repetitions,
  median_seconds = c(median(legacy_seconds), median(bounded_seconds)),
  modeled_peak_mib = c(legacy_modeled_peak, bounded_modeled_peak) / 1024^2
)
result[, peak_fraction_of_legacy :=
         modeled_peak_mib / modeled_peak_mib[implementation ==
                                               "legacy_full_merge"]]
print(result)

# Stress the diagnostic cardinality independently of spectral data. Production
# retains exact row totals without allocating one report row per unique miss.
high_cardinality <- make_fixture(n = 200000L, keys = 200000L)
high_cardinality$lookup <- high_cardinality$lookup[seq_len(100L)]
unbounded_report <- suppressWarnings(OpenSpecy:::.lib_join_metadata_table(
  copy(high_cardinality$metadata), high_cardinality$lookup,
  by = "spectrum_identity", copy_metadata = FALSE, chunk_size = 10000L
))
unbounded_report$data <- OpenSpecy:::.lib_coalesce_joined_metadata(
  unbounded_report$data, "material", lookup_precedence = TRUE
)
bounded_report <- suppressWarnings(OpenSpecy:::.lib_join_metadata_table(
  copy(high_cardinality$metadata), high_cardinality$lookup,
  by = "spectrum_identity", copy_metadata = FALSE, chunk_size = 10000L,
  report_value_limit = 1000L, coalesce_existing = TRUE
))
unbounded_unmatched <- unbounded_report$report[
  problem == "unmatched_metadata_key"
]
bounded_unmatched <- bounded_report$report[
  problem == "unmatched_metadata_key"
]
report_result <- data.table(
  implementation = c("unbounded_report", "bounded_report"),
  rows = c(nrow(unbounded_unmatched), nrow(bounded_unmatched)),
  unmatched_spectra = c(
    sum(unbounded_unmatched$n), sum(bounded_unmatched$n)
  ),
  report_mib = c(
    as.numeric(object.size(unbounded_unmatched)),
    as.numeric(object.size(bounded_unmatched))
  ) / 1024^2
)
print(report_result)

stopifnot(
  result[implementation == "bounded_in_place", peak_fraction_of_legacy] < 0.7,
  result[implementation == "bounded_in_place", median_seconds] < 30,
  result[implementation == "bounded_in_place", median_seconds] < 2 ||
    result[implementation == "bounded_in_place", median_seconds] <=
      5 * result[implementation == "legacy_full_merge", median_seconds],
  identical(
    as.data.frame(unbounded_report$data),
    as.data.frame(bounded_report$data)
  ),
  sum(unbounded_unmatched$n) == sum(bounded_unmatched$n),
  nrow(bounded_unmatched) <= 1001L,
  bounded_report$telemetry$reported_unmatched_values <= 1000L,
  bounded_report$telemetry$collapsed_unmatched_rows > 0L,
  report_result[implementation == "bounded_report", report_mib] <
    0.1 * report_result[implementation == "unbounded_report", report_mib]
)
