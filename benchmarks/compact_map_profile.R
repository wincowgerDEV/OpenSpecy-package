# Profile exact and background-suppressed compact Specs feasibility.
#
# Usage:
# Rscript benchmarks/compact_map_profile.R <source> [metric] [minimum] [maximum]

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  args <- file.path("inst", "extdata", "CA_tiny_map.zip")
}
if (!file.exists(args[[1L]])) {
  stop("Supply one readable ENVI, H5, or ZIP source path.", call. = FALSE)
}

source <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
metric <- if (length(args) >= 2L) args[[2L]] else "run_sig_over_noise"
minimum <- if (length(args) >= 3L) as.numeric(args[[3L]]) else 4
maximum <- if (length(args) >= 4L) as.numeric(args[[4L]]) else Inf

devtools::load_all(quiet = TRUE)

bytes <- function(x) as.numeric(utils::object.size(x))
mib_bytes <- function(x) round(as.numeric(x) / 1024^2, 2)
mib <- function(x) mib_bytes(bytes(x))

started <- proc.time()[["elapsed"]]
map <- read_any(source)
read_seconds <- proc.time()[["elapsed"]] - started
n_pixels <- ncol(map$spectra)
n_bands <- nrow(map$spectra)
cat("Read", n_pixels, "spectra x", n_bands, "bands in", read_seconds,
    "seconds.\n")
flush.console()

grid <- data.table::as.data.table(map$metadata)
regular_grid <- all(c("x", "y") %in% names(grid)) &&
  data.table::uniqueN(grid, by = c("x", "y")) == n_pixels &&
  data.table::uniqueN(grid$x) * data.table::uniqueN(grid$y) == n_pixels
cardinality <- vapply(grid, data.table::uniqueN, integer(1), na.rm = FALSE)

# `duplicated.matrix(..., MARGIN = 1)` compares complete rows. Transposing here
# is intentionally a one-off feasibility measurement. Production compact
# readers deliberately do not hash or deduplicate spectra.
started_unique <- proc.time()[["elapsed"]]
unique_mask <- !duplicated(t(map$spectra), MARGIN = 1L)
unique_spectra <- sum(unique_mask)
unique_seconds <- proc.time()[["elapsed"]] - started_unique
cat("Found", unique_spectra, "exact unique spectra in", unique_seconds,
    "seconds.\n")
flush.console()

classify <- function(x, label) {
  values <- sig_noise(x, metric = metric, step = 10, spatial_smooth = FALSE,
                      abs = FALSE)
  keep <- is.finite(values) & values > minimum & values < maximum
  candidate <- order(abs(values - minimum), na.last = NA)
  spot <- utils::head(candidate, 10L)
  list(
    label = label, values = values, keep = keep,
    retained = sum(keep), removed = sum(!keep),
    nonfinite = sum(!is.finite(values)),
    threshold_spot = data.frame(
      pixel = spot, value = values[spot], retained = keep[spot]
    )
  )
}

raw <- classify(map, "raw")
cat("Raw retained:", raw$retained, "removed:", raw$removed, "\n")
flush.console()
smoothed_map <- spatial_smooth(map, sigma = c(1, 1, 1))
smoothed <- classify(smoothed_map, "smoothed_sigma_1")
cat("Smoothed retained:", smoothed$retained, "removed:", smoothed$removed,
    "\n")
flush.console()
rm(smoothed_map)
invisible(gc())

retained_unique <- function(keep) {
  if (!any(keep)) return(0L)
  sum(!duplicated(t(map$spectra[, keep, drop = FALSE]), MARGIN = 1L))
}
if (unique_spectra == n_pixels) {
  raw_unique <- raw$retained
  smoothed_unique <- smoothed$retained
} else {
  raw_unique <- retained_unique(raw$keep)
  smoothed_unique <- retained_unique(smoothed$keep)
}

project <- function(n_values) {
  n_values * n_bands * 8 + n_pixels * 4 + n_bands * 8
}

report <- list(
  source = source,
  source_bytes = file.info(source)$size,
  bands = n_bands,
  pixels = n_pixels,
  spectra_mib = mib(map$spectra),
  metadata_mib = mib(map$metadata),
  object_mib = mib(map),
  read_seconds = unname(read_seconds),
  exact_unique_spectra = unique_spectra,
  exact_duplicate_pixels = n_pixels - unique_spectra,
  exact_unique_seconds = unname(unique_seconds),
  regular_grid = regular_grid,
  metadata_cardinality = cardinality,
  threshold = list(metric = metric, minimum = minimum, maximum = maximum),
  raw = raw[c("label", "retained", "removed", "nonfinite",
              "threshold_spot")],
  smoothed = smoothed[c("label", "retained", "removed", "nonfinite",
                        "threshold_spot")],
  projected_mib = c(
    exact = mib_bytes(project(unique_spectra)),
    raw_background = mib_bytes(project(raw_unique)),
    smoothed_background = mib_bytes(project(smoothed_unique))
  )
)

profile_iterations <- suppressWarnings(as.integer(Sys.getenv(
  "OPENSPECY_COMPACT_PROFILE_ITERATIONS", "1"
)))
if (is.na(profile_iterations) || profile_iterations < 1L) {
  stop("OPENSPECY_COMPACT_PROFILE_ITERATIONS must be positive.", call. = FALSE)
}
time_read <- function(...) {
  elapsed <- numeric(profile_iterations)
  result <- NULL
  for (i in seq_len(profile_iterations)) {
    invisible(gc())
    started <- proc.time()[["elapsed"]]
    result <- read_any(source, ...)
    elapsed[[i]] <- proc.time()[["elapsed"]] - started
  }
  list(object = result, elapsed = elapsed,
       median_seconds = stats::median(elapsed))
}

exact_profile <- time_read(representation = "Specs")
if (!is_Specs(exact_profile$object) ||
    specs_source_count(exact_profile$object) != n_pixels ||
    !identical(unname(exact_profile$object$values), unname(map$spectra))) {
  stop("Exact compact read differs from the dense source values.",
       call. = FALSE)
}
raw_policy <- specs_background_filter(
  metric = metric, minimum = minimum, maximum = maximum
)
filtered_profile <- time_read(
  representation = "Specs", background_filter = raw_policy
)
if (!is_Specs(filtered_profile$object) ||
    !identical(specs_background_mask(filtered_profile$object), !raw$keep) ||
    !identical(unname(filtered_profile$object$values),
               unname(map$spectra[, raw$keep, drop = FALSE]))) {
  stop("Background-suppressed compact read differs from the dense oracle.",
       call. = FALSE)
}
report$actual_compact <- list(
  iterations = profile_iterations,
  exact_median_seconds = exact_profile$median_seconds,
  exact_object_mib = mib(exact_profile$object),
  filtered_median_seconds = filtered_profile$median_seconds,
  filtered_object_mib = mib(filtered_profile$object),
  filtered_values = ncol(filtered_profile$object$values)
)

print(report)
