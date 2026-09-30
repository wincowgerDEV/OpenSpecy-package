# Repeated benchmark for independent-library cross-class conflict pruning.
# Run from the package root with:
# Rscript benchmarks/reference_cross_class_pruning.R

devtools::load_all(quiet = TRUE)

set.seed(20260929)
n_spectra <- 1000L
n_wavenumbers <- 600L
block_size <- 256L
repetitions <- 3L

normalized <- matrix(
  rnorm(n_spectra * n_wavenumbers),
  nrow = n_spectra,
  ncol = n_wavenumbers
)
normalized <- normalized / sqrt(rowSums(normalized^2))
ids <- sprintf("benchmark-%04d", seq_len(n_spectra))
classes <- sprintf("polyclass-%02d", (seq_len(n_spectra) - 1L) %% 20L + 1L)
libraries <- sprintf("library-%02d", (seq_len(n_spectra) - 1L) %% 25L + 1L)
pools <- rep(c("raman", "infrared"), each = n_spectra / 2L)

# Twenty independent components have one suspect supported by two other
# libraries. The two supporting spectra share a class, so the suspect has
# evidence weight two and each supporting spectrum has weight one.
groups <- matrix(seq_len(60L), ncol = 3L, byrow = TRUE)
for (group in seq_len(nrow(groups))) {
  index <- groups[group, ]
  normalized[index[2], ] <- normalized[index[1], ]
  normalized[index[3], ] <- normalized[index[1], ]
  classes[index] <- c(
    paste0("polysuspect-", group),
    paste0("polyreference-", group),
    paste0("polyreference-", group)
  )
  libraries[index] <- c("suspect-library", "reference-library-a",
                        "reference-library-b")
}

run_kernel <- function(block) {
  OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, normalized, ids, libraries, threshold = 0.9,
    progress = FALSE, block_size = block
  )
}

reference <- run_kernel(64L)
candidate <- run_kernel(block_size)
stopifnot(
  identical(reference$removed_rows, candidate$removed_rows),
  identical(reference$removals, candidate$removals),
  length(candidate$removed_rows) == nrow(groups),
  all(candidate$removals$evidence_libraries == 2L),
  all(candidate$removals$reason == "cross_class_more_library_evidence")
)

runs <- replicate(
  repetitions,
  system.time(invisible(run_kernel(block_size)))[["elapsed"]]
)
result <- data.frame(
  spectra = n_spectra,
  wavenumbers = n_wavenumbers,
  libraries = data.table::uniqueN(libraries),
  pools = data.table::uniqueN(pools),
  block_size = block_size,
  block_memory_mib = block_size * n_spectra * 8 / 1024^2,
  conflicts_removed = length(candidate$removed_rows),
  median_seconds = stats::median(runs),
  runs_seconds = paste(sprintf("%.3f", runs), collapse = ", ")
)
print(result, row.names = FALSE)

if (result$median_seconds > 30) {
  stop("Cross-class pruning exceeded the 30-second benchmark budget",
       call. = FALSE)
}
