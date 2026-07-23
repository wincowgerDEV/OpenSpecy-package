# Optional benchmark for the matrix-native Fill Peaks path.
# Run from the package root with:
#   Rscript benchmarks/fill_peaks_batch.R

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else if (!requireNamespace("OpenSpecy", quietly = TRUE)) {
  stop("Install or load OpenSpecy before running this benchmark.")
}

set.seed(73)
n_wavenumber <- 500L
n_spectra <- 30L
wavenumber <- seq(500, 3500, length.out = n_wavenumber)
template <- sin(wavenumber / 115) + cos(wavenumber / 61) +
  0.00000015 * (wavenumber - 1900)^2
spectra <- vapply(seq_len(n_spectra), function(i) {
  template + stats::rnorm(n_wavenumber, sd = 0.02) + i / 100
}, FUN.VALUE = numeric(n_wavenumber))
colnames(spectra) <- paste0("s", seq_len(n_spectra))
x <- OpenSpecy::as_OpenSpecy(wavenumber, spectra)

lambda <- 4
hwi <- 5L
it <- 3L
int <- 50L

one_spectrum_at_a_time <- function() {
  corrected <- vapply(seq_len(ncol(x$spectra)), function(i) {
    baseline::baseline.fillPeaks(
      matrix(x$spectra[, i], nrow = 1L),
      lambda = lambda,
      hwi = hwi,
      it = it,
      int = int
    )$corrected[1L, ]
  }, FUN.VALUE = numeric(nrow(x$spectra)))
  colnames(corrected) <- colnames(x$spectra)
  corrected
}

matrix_native <- function() {
  OpenSpecy::subtr_baseline(
    x,
    type = "fill_peaks",
    lambda = lambda,
    hwi = hwi,
    it = it,
    int = int,
    make_rel = FALSE
  )$spectra
}

legacy_result <- one_spectrum_at_a_time()
batch_result <- matrix_native()
if (!isTRUE(all.equal(legacy_result, batch_result, tolerance = 1e-10,
                      check.attributes = FALSE))) {
  stop("Fill Peaks batch output is not equivalent to per-spectrum output.")
}

invisible(one_spectrum_at_a_time())
invisible(matrix_native())
repetitions <- 3L
legacy_times <- replicate(
  repetitions,
  system.time(one_spectrum_at_a_time())[["elapsed"]]
)
batch_times <- replicate(
  repetitions,
  system.time(matrix_native())[["elapsed"]]
)
legacy_median <- stats::median(legacy_times)
batch_median <- stats::median(batch_times)

result <- data.frame(
  implementation = c("one_spectrum_at_a_time", "matrix_native"),
  median_seconds = c(legacy_median, batch_median),
  equivalent = TRUE
)
print(result)

if (batch_median > legacy_median * 1.10) {
  stop("Matrix-native Fill Peaks is more than 10% slower than the comparison.")
}
