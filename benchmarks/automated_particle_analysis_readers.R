if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = TRUE)
} else {
  library(OpenSpecy)
}

set.seed(42)

legacy_as_OpenSpecy <- function(wavenumber, spectra, metadata = NULL,
                                coords = "gen_grid") {
  spectra <- as.matrix(spectra)
  storage.mode(spectra) <- "double"
  if (is.null(colnames(spectra)))
    colnames(spectra) <- paste0("V", seq_len(ncol(spectra)))
  ord <- order(wavenumber)
  obj <- structure(list(), class = c("OpenSpecy", "list"),
                   intensity_unit = NULL, derivative_order = NULL,
                   baseline = NULL, spectra_type = NULL)
  obj$wavenumber <- wavenumber[ord]
  obj$spectra <- spectra[ord, , drop = FALSE]
  if (is.character(coords)) {
    obj$metadata <- do.call(coords, list(ncol(obj$spectra)))
  } else {
    obj$metadata <- data.table::as.data.table(coords)
  }
  if (!is.null(metadata)) {
    obj$metadata <- cbind(obj$metadata, data.table::as.data.table(metadata))
    obj$metadata$col_id <- colnames(obj$spectra)
    obj$metadata$file_id <- digest::digest(obj[c("wavenumber", "spectra")])
  }
  obj
}

bench_once <- function(expr) {
  gc()
  system.time(force(expr))[["elapsed"]]
}

bench_case <- function(name, old, new, equivalent, iterations = 5,
                       max_slowdown = 1.1, max_abs_slowdown = 0.05) {
  old_res <- old()
  new_res <- new()
  eq <- isTRUE(all.equal(equivalent(old_res), equivalent(new_res),
                         tolerance = 1e-10, check.attributes = FALSE))
  old_time <- median(replicate(iterations, bench_once(old())))
  new_time <- median(replicate(iterations, bench_once(new())))
  out <- data.frame(
    case = name,
    implementation = c("legacy", "current"),
    median_seconds = c(old_time, new_time),
    equivalent = eq
  )
  print(out)
  if (!eq) stop(name, " outputs were not equivalent", call. = FALSE)
  materially_slower <- is.finite(old_time) && old_time > 0 &&
    new_time > old_time * max_slowdown &&
    new_time - old_time > max_abs_slowdown
  if (materially_slower) {
    stop(name, " current implementation was more than ",
         max_slowdown, "x slower than legacy by more than ",
         max_abs_slowdown, "s", call. = FALSE)
  }
  invisible(out)
}

n_wn <- 500L
n_spec <- 300L
wn <- seq(500, 3500, length.out = n_wn)
spectra <- matrix(rnorm(n_wn * n_spec), nrow = n_wn)
colnames(spectra) <- paste0("s", seq_len(n_spec))
metadata <- data.frame(batch = rep("a", n_spec))

results <- list(
  bench_case(
    "as_OpenSpecy_default",
    old = function() legacy_as_OpenSpecy(wn, as.data.frame(spectra),
                                         metadata = metadata),
    new = function() as_OpenSpecy(wn, as.data.frame(spectra),
                                  metadata = metadata),
    equivalent = function(x) list(wavenumber = x$wavenumber,
                                  spectra = x$spectra,
                                  metadata_names = names(x$metadata))
  ),
  bench_case(
    "as_OpenSpecy_skip_file_id",
    old = function() legacy_as_OpenSpecy(wn, as.data.frame(spectra),
                                         metadata = metadata),
    new = function() as_OpenSpecy(wn, as.data.frame(spectra),
                                  metadata = metadata,
                                  compute_file_id = FALSE),
    equivalent = function(x) list(wavenumber = x$wavenumber,
                                  spectra = x$spectra,
                                  metadata_rows = nrow(x$metadata)),
    max_slowdown = Inf
  )
)

h5_file <- Sys.getenv("OPENSPECY_H5_BENCH_FILE", unset = "")
if (nzchar(h5_file) && file.exists(h5_file)) {
  h5_times <- replicate(3, bench_once({
    h5 <- read_h5(h5_file, collapse = FALSE, read_visual = FALSE)
    stopifnot(is_OpenSpecy(h5), check_OpenSpecy(h5))
  }))
  cat("read_h5 raw external median seconds:", median(h5_times), "\n")
} else {
  cat("Skipping external H5 benchmark; set OPENSPECY_H5_BENCH_FILE.\n")
}

invisible(results)
