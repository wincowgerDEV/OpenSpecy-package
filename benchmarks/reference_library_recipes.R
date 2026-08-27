# Same-output performance evidence for reference-library processing.
# This file is intentionally outside tests because it is a development
# benchmark, not a CRAN test surface.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

legacy_sgfilt_matrix <- function(y, p, n, m, ...) {
  filt <- OpenSpecy:::.sgolay_filter(p = p, n = n, m = m, ...)
  len <- nrow(y)
  n <- nrow(filt)
  k <- floor(n / 2)
  out <- matrix(NA_real_, nrow = len, ncol = ncol(y),
                dimnames = list(NULL, colnames(y)))
  out[seq_len(k), ] <- filt[seq_len(k), , drop = FALSE] %*%
    y[seq_len(n), , drop = FALSE]
  mid_n <- len - n + 1L
  mid <- matrix(0, nrow = mid_n, ncol = ncol(y))
  center <- filt[k + 1L, ]
  for (i in seq_len(n)) {
    rows <- i:(i + mid_n - 1L)
    mid <- mid + center[i] * y[rows, , drop = FALSE]
  }
  out[(k + 1L):(len - k), ] <- mid
  out[(len - k + 1L):len, ] <- filt[(k + 2L):n, , drop = FALSE] %*%
    y[(len - n + 1L):len, , drop = FALSE]
  out
}

legacy_polynomial_baseline <- function(x, y, degree = 8, raw = FALSE,
                                       iterations = 10,
                                       termination_diff = 0.05) {
  xout <- x
  yin <- y
  iteration <- 1L
  dev_prev <- 0
  criteria_met <- FALSE
  fit <- stats::lm(y ~ stats::poly(x, degree = degree, raw = raw))
  mod_poly <- fit$fitted.values
  dev_curr <- stats::sd(fit$residuals)
  peaks <- y > mod_poly + dev_curr
  y <- y[!peaks]
  x <- x[!peaks]
  while (!criteria_met) {
    fit <- stats::lm(y ~ stats::poly(x, degree = degree, raw = raw))
    mod_poly <- fit$fitted.values
    dev_curr <- stats::sd(fit$residuals)
    y <- pmin(mod_poly + dev_curr, y)
    criteria_met <-
      abs((dev_curr - dev_prev) / dev_curr) <= termination_diff ||
      iteration == iterations
    iteration <- iteration + 1L
    dev_prev <- dev_curr
  }
  baseline <- unname(unlist(stats::approx(
    x, y, xout = xout, rule = 2, method = "linear", ties = mean
  )[2]))
  yin - baseline
}

set.seed(42)
derivative_input <- matrix(runif(1983 * 1000), nrow = 1983)
legacy_derivative_time <- system.time(
  legacy_derivative <- legacy_sgfilt_matrix(derivative_input, 3, 15, 1)
)
current_derivative_time <- system.time(
  current_derivative <- OpenSpecy:::.sgfilt_matrix(
    derivative_input, 3, 15, 1
  )
)
stopifnot(max(abs(legacy_derivative - current_derivative)) <= 1e-12)
if (current_derivative_time[["elapsed"]] >
    legacy_derivative_time[["elapsed"]] * 1.1) {
  stop("Current derivative path is more than 10% slower than legacy")
}

wavenumber <- seq(400, 4000, length.out = 500)
baseline_input <- replicate(120, {
  0.2 + 0.00000003 * (wavenumber - 1800)^2 +
    exp(-((wavenumber - runif(1, 900, 3000)) / 80)^2) +
    stats::rnorm(length(wavenumber), 0, 0.002)
})
legacy_baseline_time <- system.time(
  legacy_baseline <- vapply(seq_len(ncol(baseline_input)), function(i) {
    legacy_polynomial_baseline(wavenumber, baseline_input[, i])
  }, numeric(length(wavenumber)))
)
current_baseline_time <- system.time(
  current_baseline <- vapply(seq_len(ncol(baseline_input)), function(i) {
    subtr_baseline(
      wavenumber, baseline_input[, i], make_rel = FALSE
    )
  }, numeric(length(wavenumber)))
)
stopifnot(max(abs(legacy_baseline - current_baseline)) <= 1e-10)
if (current_baseline_time[["elapsed"]] >
    legacy_baseline_time[["elapsed"]] * 1.1) {
  stop("Current baseline path is more than 10% slower than legacy")
}

message("Derivative legacy/current elapsed: ",
        legacy_derivative_time[["elapsed"]], " / ",
        current_derivative_time[["elapsed"]])
message("Baseline legacy/current elapsed: ",
        legacy_baseline_time[["elapsed"]], " / ",
        current_baseline_time[["elapsed"]])
