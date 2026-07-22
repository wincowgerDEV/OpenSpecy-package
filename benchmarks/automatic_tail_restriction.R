# Compare automatic shared-bound search with a literal one-point-at-a-time
# implementation that rebuilds the OpenSpecy object after every crop.

devtools::load_all(quiet = TRUE)

make_tail_benchmark <- function(n_points = 1000L, n_spectra = 100L) {
  set.seed(42)
  wavenumber <- seq(400, 4000, length.out = n_points)
  spectra <- matrix(runif(n_points * n_spectra, 0, 1), nrow = n_points)
  spectra[seq_len(30L), ] <- 4^(30:1)
  colnames(spectra) <- paste0("s", seq_len(n_spectra))
  as_OpenSpecy(x = wavenumber, spectra = spectra)
}

legacy_one_point <- function(x, ratio = 3, tail_n = 5L,
                             co2_region = c(2200, 2420)) {
  repeat {
    metrics <- OpenSpecy:::.artifact_ratio_metrics(
      x, tail_n = tail_n, co2_region = co2_region
    )
    left <- any(metrics$left_ratio >= ratio, na.rm = TRUE)
    right <- any(metrics$right_ratio >= ratio, na.rm = TRUE)
    if (!left && !right) break
    rows <- seq.int(1L + as.integer(left),
                    nrow(x$spectra) - as.integer(right))
    x <- as_OpenSpecy(
      x = x$wavenumber[rows],
      spectra = x$spectra[rows, , drop = FALSE],
      metadata = x$metadata
    )
  }
  x
}

x <- make_tail_benchmark()
legacy <- legacy_one_point(x)
current <- restrict_range(
  x,
  make_rel = FALSE,
  automate = TRUE, artifact_ratio = 3, tail_n = 5L, max_crop = 0.2
)
stopifnot(
  identical(legacy$wavenumber, current$wavenumber),
  identical(legacy$spectra, current$spectra)
)

invisible(legacy_one_point(x))
invisible(restrict_range(
  x, make_rel = FALSE,
  automate = TRUE, artifact_ratio = 3, tail_n = 5L, max_crop = 0.2
))

repetitions <- 10L
legacy_times <- replicate(
  repetitions,
  system.time(legacy_one_point(x))[["elapsed"]]
)
current_times <- replicate(
  repetitions,
  system.time(restrict_range(
    x, make_rel = FALSE,
    automate = TRUE, artifact_ratio = 3, tail_n = 5L, max_crop = 0.2
  ))[["elapsed"]]
)
legacy_median <- stats::median(legacy_times)
current_median <- stats::median(current_times)
cat("Legacy median:", legacy_median, "seconds\n")
cat("Current median:", current_median, "seconds\n")
if (current_median > legacy_median * 1.1) {
  stop("Optimized automatic tail restriction is more than 10% slower than ",
       "the literal one-point implementation", call. = FALSE)
}
