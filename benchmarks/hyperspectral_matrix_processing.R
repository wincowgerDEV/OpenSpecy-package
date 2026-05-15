# Optional benchmark sheet for hyperspectral matrix processing.
# Run after installing/loading OpenSpecy from this checkout:
#   source("benchmarks/hyperspectral_matrix_processing.R")

if (!requireNamespace("OpenSpecy", quietly = TRUE)) {
  stop("Install or load OpenSpecy before running this benchmark.")
}

set.seed(47)
n_wn <- 900L
n_spec <- 1000L
wn <- seq(500, 3500, length.out = n_wn)
template <- sin(wn / 120) + cos(wn / 70)
spectra <- vapply(seq_len(n_spec), function(i) {
  template + rnorm(n_wn, sd = 0.05) + i / n_spec
}, FUN.VALUE = numeric(n_wn))
colnames(spectra) <- paste0("s", seq_len(n_spec))

os <- OpenSpecy::as_OpenSpecy(wn, spectra = as.data.frame(spectra))
target <- seq(650, 3200, by = 4)
baseline <- OpenSpecy::as_OpenSpecy(
  wn,
  spectra = data.frame(baseline = template / 5)
)
lib <- OpenSpecy::as_OpenSpecy(
  target,
  spectra = as.data.frame(spectra[seq_len(length(target)), seq_len(50L)])
)
unk <- OpenSpecy::conform_spec(os, range = target, res = NULL)

legacy_make_rel <- function(mat) {
  out <- vapply(seq_len(ncol(mat)), function(i) {
    r <- range(mat[, i])
    (mat[, i] - r[1L]) / (r[2L] - r[1L])
  }, FUN.VALUE = numeric(nrow(mat)))
  colnames(out) <- colnames(mat)
  out
}

legacy_sg <- function(x) {
  filt <- signal::sgolay(p = 3, n = 11, m = 1)
  out <- vapply(seq_len(ncol(x$spectra)), function(i) {
    as.numeric(signal::filter(filt = filt, x = x$spectra[, i]))
  }, FUN.VALUE = numeric(nrow(x$spectra)))
  colnames(out) <- colnames(x$spectra)
  x$spectra <- out
  x
}

legacy_conform <- function(x) {
  out <- vapply(seq_len(ncol(x$spectra)), function(i) {
    approx(x = x$wavenumber, y = x$spectra[, i], xout = target)$y
  }, FUN.VALUE = numeric(length(target)))
  colnames(out) <- colnames(x$spectra)
  x$wavenumber <- target
  x$spectra <- out
  x
}

legacy_manual_baseline <- function(x) {
  bl <- approx(baseline$wavenumber, baseline$spectra[, 1L],
               xout = x$wavenumber, rule = 2, method = "linear",
               ties = mean)$y
  out <- vapply(seq_len(ncol(x$spectra)), function(i) {
    x$spectra[, i] - bl
  }, FUN.VALUE = numeric(nrow(x$spectra)))
  colnames(out) <- colnames(x$spectra)
  x$spectra <- out
  x
}

cor_mat <- matrix(runif(ncol(lib$spectra) * ncol(unk$spectra)),
                  nrow = ncol(lib$spectra),
                  dimnames = list(colnames(lib$spectra),
                                  colnames(unk$spectra)))

legacy_ident <- function(cor_matrix, x, library, top_n = 5L) {
  lib_names <- rownames(cor_matrix)
  unk_ids <- colnames(cor_matrix)
  out <- lapply(seq_len(ncol(cor_matrix)), function(j) {
    col <- cor_matrix[, j]
    ord <- head(order(col, decreasing = TRUE), top_n)
    data.table::data.table(
      object_id = unk_ids[j],
      library_id = lib_names[ord],
      match_val = col[ord]
    )
  }) |>
    data.table::rbindlist()
  data.table::setorder(out, -match_val)
  out
}

bench_case <- function(name, old, new, equivalent) {
  old_res <- old()
  new_res <- new()
  eq <- isTRUE(all.equal(equivalent(old_res), equivalent(new_res),
                         tolerance = 1e-10, check.attributes = FALSE))

  if (requireNamespace("bench", quietly = TRUE)) {
    timings <- bench::mark(
      legacy = old(),
      optimized = new(),
      iterations = 5,
      check = FALSE
    )
    data.frame(
      case = name,
      implementation = as.character(timings$expression),
      median_seconds = as.numeric(timings$median),
      memory_bytes = as.numeric(timings$mem_alloc),
      equivalent = eq
    )
  } else {
    old_time <- system.time(old())[["elapsed"]]
    new_time <- system.time(new())[["elapsed"]]
    data.frame(
      case = name,
      implementation = c("legacy", "optimized"),
      elapsed_seconds = c(old_time, new_time),
      memory_bytes = NA_real_,
      equivalent = eq
    )
  }
}

results <- rbind(
  bench_case(
    "make_rel_matrix",
    old = function() legacy_make_rel(os$spectra),
    new = function() OpenSpecy::make_rel(os$spectra),
    equivalent = identity
  ),
  bench_case(
    "smooth_intens_sg",
    old = function() legacy_sg(os),
    new = function() OpenSpecy::smooth_intens(os, make_rel = FALSE,
                                              abs = FALSE),
    equivalent = function(x) x$spectra
  ),
  bench_case(
    "conform_spec_interp",
    old = function() legacy_conform(os),
    new = function() OpenSpecy::conform_spec(os, range = target, res = NULL),
    equivalent = function(x) x$spectra
  ),
  bench_case(
    "manual_baseline",
    old = function() legacy_manual_baseline(os),
    new = function() OpenSpecy::subtr_baseline(
      os, type = "manual", baseline = baseline, make_rel = FALSE
    ),
    equivalent = function(x) x$spectra
  ),
  bench_case(
    "ident_spec_top_n",
    old = function() legacy_ident(cor_mat, unk, lib, top_n = 5L),
    new = function() OpenSpecy::ident_spec(cor_mat, unk, lib, top_n = 5L),
    equivalent = function(x) as.data.frame(x)
  )
)

print(results)
