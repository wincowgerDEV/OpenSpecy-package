# Compare reduce_lib()'s internal PAM medoid selector with the legacy
# cluster::pam(pamonce = 6) dependency path.

library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

if (!requireNamespace("cluster", quietly = TRUE)) {
  stop("Install cluster to run the legacy PAM reduction benchmark.",
       call. = FALSE)
}

median_repeated_time <- function(expr, batches = 5L, iterations = 1L) {
  times <- replicate(batches, {
    elapsed <- system.time(for (i in seq_len(iterations)) expr())[["elapsed"]]
    as.numeric(elapsed) / iterations
  })
  stats::median(times)
}

make_reduction_lib <- function(n = 80) {
  wavenumber <- seq(100, 1800, by = 25)
  base_a <- sin(wavenumber / 95)
  base_b <- cos(wavenumber / 135)
  spectra <- vapply(seq_len(n), function(i) {
    base <- if (i <= n / 2) base_a else base_b
    base + stats::rnorm(length(wavenumber), sd = 0.01) + i / 1000
  }, numeric(length(wavenumber)))
  colnames(spectra) <- paste0("s", seq_len(n))

  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table(
      sample_name = colnames(spectra),
      material_class = rep(c("class_a", "class_b"), each = n / 2)
    )
  )
}

legacy_pam_group_ids <- function(x, id_col, k) {
  ids <- getFromNamespace(".lib_ids", "OpenSpecy")(x, id_col)
  if (ncol(x$spectra) <= k) return(ids)
  cors <- cor_spec(x, x, compute = "optimized")
  cors[is.na(cors)] <- 0
  cors <- pmax(pmin(cors, 1), -1)
  diag(cors) <- 1
  distance <- stats::as.dist(1 - cors)
  res <- cluster::pam(distance, k = min(k, length(ids) - 1L), diss = TRUE,
                      pamonce = 6)
  ids[res$id.med]
}

legacy_reduce_lib <- function(x, group_cols = "material_class",
                              id_col = "sample_name", k = 8,
                              min_n = k) {
  ids <- getFromNamespace(".lib_ids", "OpenSpecy")(x, id_col)
  reduction_obj <- x
  reduction_obj$spectra <- getFromNamespace(
    ".matrix_mean_replace",
    "OpenSpecy"
  )(make_rel(x$spectra, na.rm = TRUE))
  groups <- do.call(paste, c(x$metadata[, group_cols, with = FALSE],
                             sep = "_"))

  unlist(lapply(split(seq_along(groups), groups), function(idx) {
    if (length(idx) <= min_n || length(idx) <= k) return(ids[idx])
    legacy_pam_group_ids(filter_spec(reduction_obj, idx),
                         id_col = id_col, k = k)
  }), use.names = FALSE)
}

set.seed(710)
reduction_lib <- make_reduction_lib(80)
legacy_ids <- legacy_reduce_lib(reduction_lib)
internal_ids <- reduce_lib(reduction_lib, k = 8, min_n = 8, return = "ids")
stopifnot(identical(legacy_ids, internal_ids))

legacy_time <- median_repeated_time(
  function() legacy_reduce_lib(reduction_lib),
  batches = 3L,
  iterations = 5L
)
internal_time <- median_repeated_time(
  function() reduce_lib(reduction_lib, k = 8, min_n = 8, return = "ids"),
  batches = 3L,
  iterations = 5L
)

message("Legacy cluster PAM reduction median: ", legacy_time)
message("Internal PAM reduction median: ", internal_time)
if (internal_time > legacy_time * 2) {
  warning("Internal PAM reduction is more than 2x slower than the legacy ",
          "cluster::pam path", call. = FALSE)
}
