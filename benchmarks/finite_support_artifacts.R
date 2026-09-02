# Compare the vectorized finite-support artifact assessment with a literal
# per-spectrum implementation. Run manually from the package root with:
# Rscript benchmarks/finite_support_artifacts.R

devtools::load_all(export_all = TRUE, quiet = TRUE)

set.seed(20260901)
axis <- seq(400, 4000, by = 2)
spectra <- matrix(
  stats::rnorm(length(axis) * 2000L),
  nrow = length(axis), ncol = 2000L
)
for (column in seq_len(ncol(spectra))) {
  left <- (column - 1L) %% 80L
  right <- (column * 3L) %% 80L
  if (left) spectra[seq_len(left), column] <- NA_real_
  if (right) spectra[seq.int(nrow(spectra) - right + 1L,
                             nrow(spectra)), column] <- NA_real_
}
colnames(spectra) <- sprintf("finite-mask-%04d", seq_len(ncol(spectra)))
library <- as_OpenSpecy(axis, spectra)

literal_metrics <- function(x, tail_n = 5L,
                            co2_region = c(2200, 2420),
                            silent_region = c(2420, 2550)) {
  normalized <- OpenSpecy:::.normalize_artifact_spectra(x$spectra)
  co2 <- x$wavenumber >= co2_region[[1L]] &
    x$wavenumber <= co2_region[[2L]]
  silent <- x$wavenumber >= silent_region[[1L]] &
    x$wavenumber <= silent_region[[2L]]
  one <- function(values) {
    finite <- which(is.finite(values))
    if (!length(finite)) return(rep(NA_real_, 4L))
    left <- utils::head(finite, tail_n)
    right <- utils::tail(finite, tail_n)
    control <- values
    control[unique(c(left, right))] <- NA_real_
    control[co2] <- NA_real_
    maxima <- c(
      max(values[left], na.rm = TRUE),
      max(values[right], na.rm = TRUE),
      max(values[co2], na.rm = TRUE),
      max(values[silent], na.rm = TRUE),
      max(control, na.rm = TRUE)
    )
    ratio <- function(a, b) {
      if (!is.finite(a) || !is.finite(b)) return(NA_real_)
      if (a == 0 && b == 0) return(1)
      if (a > 0 && b == 0) return(Inf)
      a / b
    }
    c(
      tail_ratio = ratio(max(maxima[1:2]), maxima[[5L]]),
      co2_ratio = ratio(maxima[[3L]], maxima[[4L]]),
      left_ratio = ratio(maxima[[1L]], maxima[[5L]]),
      right_ratio = ratio(maxima[[2L]], maxima[[5L]])
    )
  }
  apply(normalized, 2L, one)
}

current <- OpenSpecy:::.artifact_ratio_metrics(library)
literal <- literal_metrics(library)
for (metric in rownames(literal)) {
  stopifnot(isTRUE(all.equal(
    unname(current[[metric]]), unname(literal[metric, ]),
    tolerance = 1e-12
  )))
}

elapsed <- function(fun, repetitions = 3L) {
  stats::median(replicate(
    repetitions,
    system.time(force(fun()))[["elapsed"]]
  ))
}
literal_seconds <- elapsed(function() literal_metrics(library))
current_seconds <- elapsed(function() {
  OpenSpecy:::.artifact_ratio_metrics(library)
})
result <- data.frame(
  implementation = c("literal_per_spectrum", "vectorized_finite_mask"),
  seconds = c(literal_seconds, current_seconds)
)
print(result, row.names = FALSE)
cat(sprintf("speedup: %.2fx\n", literal_seconds / current_seconds))
if (current_seconds > literal_seconds * 1.1) {
  stop("The finite-mask artifact kernel regressed by more than 10%",
       call. = FALSE)
}

# Medoid reduction passes the same processed matrix as query and library.
# Confirm the symmetric one-matrix BLAS path is numerically identical and not
# materially slower than the former general two-matrix path.
set.seed(20260902)
self_matrix <- matrix(stats::rnorm(400L * 3000L), nrow = 400L)
general_self <- OpenSpecy:::.fast_correlation(self_matrix, self_matrix)
symmetric_self <- OpenSpecy:::.fast_correlation(self_matrix)
stopifnot(isTRUE(all.equal(general_self, symmetric_self, tolerance = 1e-12)))
general_seconds <- elapsed(function() {
  OpenSpecy:::.fast_correlation(self_matrix, self_matrix)
})
symmetric_seconds <- elapsed(function() {
  OpenSpecy:::.fast_correlation(self_matrix)
})
self_result <- data.frame(
  implementation = c("general_two_matrix", "symmetric_one_matrix"),
  seconds = c(general_seconds, symmetric_seconds)
)
print(self_result, row.names = FALSE)
cat(sprintf("self-correlation speedup: %.2fx\n",
            general_seconds / symmetric_seconds))
if (symmetric_seconds > general_seconds * 1.1) {
  stop("The symmetric self-correlation kernel regressed by more than 10%",
       call. = FALSE)
}
