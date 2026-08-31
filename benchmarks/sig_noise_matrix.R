# Compare the former per-spectrum rolling S/N implementation with the current
# matrix path. This is isolated from unrelated library-builder benchmarks.
devtools::load_all(quiet = TRUE)

median_repeated_time <- function(fun, batches = 3L) {
  fun()
  stats::median(vapply(seq_len(batches), function(i) {
    unname(system.time(fun())[["elapsed"]])
  }, numeric(1)))
}

make_na_processing_lib <- function(n = 900L, p = 1000L) {
  set.seed(456)
  spectra <- matrix(runif(p * n), nrow = p)
  colnames(spectra) <- paste0("na_", seq_len(n))
  spectra[seq_len(80L), seq_len(n / 3L)] <- NA_real_
  spectra[(p - 79L):p, (n / 3L + 1L):(2L * n / 3L)] <- NA_real_
  as_OpenSpecy(
    seq_len(p), spectra,
    metadata = data.table::data.table(sample_name = colnames(spectra))
  )
}

old_run_sig_over_noise <- function(x, step = 10L, prob = 0.5) {
  vapply(seq_len(ncol(x$spectra)), function(i) {
    y <- x$spectra[, i]
    if (length(y[!is.na(y)]) < step) return(NA_real_)
    rolling_max <- data.table::frollapply(y[!is.na(y)], step, max)
    rolling_max[(length(rolling_max) - (step - 1L)):length(rolling_max)] <- NA
    max(rolling_max, na.rm = TRUE) /
      as.numeric(stats::quantile(
        rolling_max[rolling_max != 0], probs = prob, na.rm = TRUE,
        names = FALSE
      ))
  }, FUN.VALUE = numeric(1))
}

x <- make_na_processing_lib()
old <- old_run_sig_over_noise(x)
current <- sig_noise(x, step = 10L)
stopifnot(isTRUE(all.equal(old, current, tolerance = 1e-12)))

old_time <- median_repeated_time(function() old_run_sig_over_noise(x))
current_time <- median_repeated_time(function() sig_noise(x, step = 10L))
cat(sprintf(
  "legacy median: %.3fs; matrix median: %.3fs; speedup: %.1fx\n",
  old_time, current_time, old_time / current_time
))
stopifnot(current_time <= old_time * 1.1)
