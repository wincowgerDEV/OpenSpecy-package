# Repeated saturation, assessment, and exact-breakpoint SNR benchmark.
# Run manually from the package root with:
# Rscript benchmarks/spectral_quality.R

devtools::load_all(export_all = TRUE, quiet = TRUE)

elapsed_median <- function(fun, repetitions = 5L) {
  stats::median(replicate(
    repetitions,
    system.time(force(fun()))[["elapsed"]]
  ))
}

literal_breakpoint_snr <- function(values) {
  values <- sort.int(abs(values[is.finite(values)]), method = "auto")
  if (length(values) < 2L) return(NA_real_)
  error <- vapply(seq_len(length(values) - 1L), function(split) {
    lower <- values[seq_len(split)]
    upper <- values[seq.int(split + 1L, length(values))]
    sum((lower - mean(lower))^2) + sum((upper - mean(upper))^2)
  }, numeric(1))
  split <- which.min(error)
  lower <- stats::median(values[seq_len(split)])
  upper <- stats::median(values[seq.int(split + 1L, length(values))])
  if (lower == 0 && upper == 0) 1 else upper / lower
}

make_quality_case <- function(spectra_count, map = FALSE, points = 701L) {
  axis <- seq(400, 3200, length.out = points)
  spectra <- vapply(seq_len(spectra_count), function(i) {
    values <- 0.15 + sin(axis / 140)^2 + 0.02 * cos(axis / 17 + i)
    plateau <- 280L + ((i - 1L) %% 5L)
    values[plateau:(plateau + 1L)] <- 10
    spike <- 470L + ((i - 1L) %% 3L)
    values[spike] <- values[spike] + 25
    values
  }, numeric(points))
  colnames(spectra) <- paste0("sample_", seq_len(spectra_count))
  metadata <- if (map) {
    side <- ceiling(sqrt(spectra_count))
    data.frame(
      x = (seq_len(spectra_count) - 1L) %% side,
      y = (seq_len(spectra_count) - 1L) %/% side
    )
  } else {
    data.frame(sample_name = colnames(spectra))
  }
  as_OpenSpecy(axis, spectra, metadata = metadata)
}

benchmark_quality_case <- function(label, x, repetitions) {
  restricted <- restrict_range(
    x, saturation = 10, saturation_guard = 1L, make_rel = FALSE
  )
  stopifnot(
    check_OpenSpecy(restricted),
    ncol(restricted$spectra) == ncol(x$spectra),
    identical(restricted$metadata, x$metadata),
    isTRUE(attr(restricted, "saturation_restriction")$applied)
  )

  checks <- c("missing_values", "negative_intensity", "low_snr",
              "spike", "saturation")
  report <- assess_spec(x, checks = checks, report = "all",
                        saturation = 10)
  stopifnot(
    nrow(report) == ncol(x$spectra) * length(checks),
    !anyDuplicated(report$test_id)
  )

  current_snr <- sig_noise(x, metric = "breakpoint_snr")
  reference_snr <- apply(x$spectra, 2L, literal_breakpoint_snr)
  stopifnot(isTRUE(all.equal(
    unname(current_snr), unname(reference_snr), tolerance = 1e-12
  )))

  timings <- c(
    saturation = elapsed_median(function() restrict_range(
      x, saturation = 10, saturation_guard = 1L, make_rel = FALSE
    ), repetitions),
    assessment = elapsed_median(function() assess_spec(
      x, checks = checks, report = "all", saturation = 10
    ), repetitions),
    breakpoint_snr = elapsed_median(function() sig_noise(
      x, metric = "breakpoint_snr"
    ), repetitions),
    literal_breakpoint_snr = elapsed_median(function() apply(
      x$spectra, 2L, literal_breakpoint_snr
    ), repetitions)
  )
  ratio <- timings[["breakpoint_snr"]] /
    max(timings[["literal_breakpoint_snr"]], .Machine$double.eps)
  if (is.finite(ratio) && ratio > 1.10) {
    warning(sprintf(
      "%s exact breakpoint SNR runtime regression flag: %.1f%% slower",
      label, 100 * (ratio - 1)
    ), call. = FALSE)
  }
  message(label, ": ", paste(
    names(timings), sprintf("%.4fs", timings), collapse = ", "
  ))
  timings
}

results <- list(
  single = benchmark_quality_case(
    "single spectrum", make_quality_case(1L), repetitions = 5L
  ),
  batch_100 = benchmark_quality_case(
    "100 spectra", make_quality_case(100L), repetitions = 3L
  ),
  map = benchmark_quality_case(
    "12x12 map", make_quality_case(144L, map = TRUE), repetitions = 2L
  )
)

print(do.call(rbind, results))
