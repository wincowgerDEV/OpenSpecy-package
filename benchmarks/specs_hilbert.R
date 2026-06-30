if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = TRUE)
} else {
  library(OpenSpecy)
}

set.seed(42)

n_library <- 1500L
n_query <- 200L
variables <- seq(600, 1800, length.out = 120)

make_spectra <- function(n, offset = 0) {
  vapply(seq_len(n), function(i) {
    sin(variables / 90 + i / 13 + offset) +
      cos(variables / 170 + i / 7) +
      rnorm(length(variables), sd = 0.02)
  }, FUN.VALUE = numeric(length(variables)))
}

library_os <- as_OpenSpecy(
  variables,
  spectra = make_spectra(n_library),
  metadata = data.frame(x = seq_len(n_library), y = 0)
)
query_os <- as_OpenSpecy(
  variables,
  spectra = make_spectra(n_query, offset = 0.03),
  metadata = data.frame(x = seq_len(n_query), y = 0)
)

model <- fit_specs_pca(library_os, n_components = 16)
library_pca <- as_Specs(library_os, model, steps = "pca")
query_pca <- as_Specs(query_os, model, steps = "pca")

library_hilbert <- encode_specs_hilbert(library_pca, bits_per_variable = 4)
query_hilbert <- encode_specs_hilbert(
  query_pca, limits = attr(library_hilbert, "hilbert_model")
)

decoded <- decode_specs_hilbert(library_hilbert)
stopifnot(all(dim(decoded$values) == dim(library_pca$values)))
stopifnot(max(abs(decoded$values - library_pca$values)) < 1)

bench_once <- function(expr) {
  gc()
  system.time(force(expr))[["elapsed"]]
}

cor_times <- replicate(5, bench_once({
  match_spec(query_pca, library_pca, top_n = 1)
}))
hilbert_times <- replicate(5, bench_once({
  match_spec(query_hilbert, library_hilbert, top_n = 1)
}))

cat("Correlation Specs match median seconds:", median(cor_times), "\n")
cat("Hilbert Specs match median seconds:", median(hilbert_times), "\n")

if (median(hilbert_times) >= median(cor_times)) {
  stop("Hilbert distance matching was not faster than correlation matching",
       call. = FALSE)
}
