# Compare the frozen post-merge filters with early source-prefilter staging.

suppressPackageStartupMessages({
  library(devtools)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
source_count <- if (length(args) >= 1L) as.integer(args[[1L]]) else 4L
spectra_per_source <- if (length(args) >= 2L) as.integer(args[[2L]]) else 1000L
n_wavenumbers <- if (length(args) >= 3L) as.integer(args[[3L]]) else 300L
chunk_size <- if (length(args) >= 4L) as.integer(args[[4L]]) else 250L
repetitions <- if (length(args) >= 5L) as.integer(args[[5L]]) else 3L
if (anyNA(c(source_count, spectra_per_source, n_wavenumbers, chunk_size,
            repetitions)) ||
    min(source_count, spectra_per_source, n_wavenumbers, chunk_size,
        repetitions) < 1L) {
  stop("Arguments must be positive integers", call. = FALSE)
}

load_all(".", quiet = TRUE)
set.seed(51053L)
work_dir <- tempfile("OpenSpecy-source-prefilter-benchmark-")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
wavenumber <- seq(400, by = 6, length.out = n_wavenumbers)
paths <- vapply(seq_len(source_count), function(source_index) {
  spectra <- matrix(
    rnorm(n_wavenumbers * spectra_per_source),
    nrow = n_wavenumbers, ncol = spectra_per_source
  )
  spectra <- sweep(spectra, 1L, sin(wavenumber / 120), "+")
  # Deliberately distinct hashes but a strict >0.99 correlation pair.
  if (spectra_per_source > 1L) {
    spectra[, 2L] <- spectra[, 1L] * 1.001 + 0.001
  }
  ids <- sprintf(
    "prefilter_%02d_%06d", source_index, seq_len(spectra_per_source)
  )
  identities <- paste0("benchmark_", source_index, "_", seq_len(
    spectra_per_source
  ))
  if (spectra_per_source > 1L) identities[2L] <- identities[1L]
  colnames(spectra) <- ids
  object <- as_OpenSpecy(
    wavenumber, spectra = spectra,
    metadata = data.table(
      sample_name = ids, file_id = ids,
      spectrum_identity = identities,
      organization = paste0("benchmark_", source_index),
      library_name = paste0("benchmark_", source_index),
      spectrum_type = "raman", intensity_units = "arbitrary units",
      repeated_source_label = paste0("source_", source_index)
    ),
    attributes = list(intensity_unit = "arbitrary units")
  )
  path <- file.path(work_dir, sprintf("source-%02d.rds", source_index))
  saveRDS(object, path, compress = FALSE, version = 3)
  path
}, character(1L))

run_build <- function(prefilter) {
  recipe_dir <- tempfile("recipe-stage-", tmpdir = work_dir)
  dir.create(recipe_dir, recursive = TRUE)
  on.exit(unlink(recipe_dir, recursive = TRUE, force = TRUE), add = TRUE)
  old_options <- options(
    OpenSpecy.source_record_staging = TRUE,
    OpenSpecy.source_prefilter_staging = prefilter,
    OpenSpecy.compact_staged_metadata = prefilter,
    OpenSpecy.source_merge_chunk_size = chunk_size
  )
  on.exit(options(old_options), add = TRUE)
  build_lib(
    paths, recipes = list(raw = list()), range = NULL,
    dedupe = TRUE, dedupe_correlation_threshold = 0.99,
    convert_intensity = FALSE, signal_noise = FALSE, progress = FALSE,
    .recipe_staging_dir = recipe_dir
  )$raw
}

postmerge_result <- run_build(FALSE)
prefilter_result <- run_build(TRUE)
stopifnot(
  identical(postmerge_result$wavenumber, prefilter_result$wavenumber),
  identical(postmerge_result$spectra, prefilter_result$spectra),
  ncol(prefilter_result$spectra) ==
    source_count * spectra_per_source - source_count
)
metadata_comparison <- all.equal(
  postmerge_result$metadata, prefilter_result$metadata,
  check.attributes = TRUE
)
if (!isTRUE(metadata_comparison)) {
  stop(
    "Source prefilter changed returned metadata: ",
    paste(metadata_comparison, collapse = "; "),
    "\npostmerge: ", paste(names(postmerge_result$metadata), collapse = ", "),
    "\nprefilter: ", paste(names(prefilter_result$metadata), collapse = ", "),
    call. = FALSE
  )
}

elapsed <- function(prefilter) {
  median(replicate(
    repetitions, system.time(run_build(prefilter))[["elapsed"]]
  ))
}
postmerge_seconds <- elapsed(FALSE)
prefilter_seconds <- elapsed(TRUE)
input_spectra <- as.double(source_count) * spectra_per_source
retained_spectra <- input_spectra - source_count
merged_input_mib <- 8 * input_spectra * n_wavenumbers / 1024^2
merged_retained_mib <- 8 * retained_spectra * n_wavenumbers / 1024^2
result <- data.table(
  implementation = c("postmerge_filter", "source_prefilter"),
  seconds = c(postmerge_seconds, prefilter_seconds),
  modeled_merged_spectral_mib = c(merged_input_mib, merged_retained_mib),
  sources = source_count,
  input_spectra = input_spectra,
  retained_spectra = retained_spectra,
  wavenumbers = n_wavenumbers,
  repetitions = repetitions
)
print(result)
cat(sprintf(
  paste0("prefilter/postmerge merged spectral ratio: %.6f; ",
         "elapsed ratio: %.2fx; maximum merge block: %d spectra\n"),
  merged_retained_mib / merged_input_mib,
  prefilter_seconds / max(postmerge_seconds, .Machine$double.eps),
  min(chunk_size, spectra_per_source)
))
stopifnot(
  merged_retained_mib < merged_input_mib,
  prefilter_seconds <= max(4 * postmerge_seconds, postmerge_seconds + 15)
)
