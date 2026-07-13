#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
library_dir <- if (length(args)) args[[1]] else file.path("_wasm", "libraries")

library_types <- readLines(file.path("inst", "shiny", "wasm",
                                     "library-types.txt"), warn = FALSE)
library_types <- trimws(sub("#.*$", "", library_types))
library_types <- library_types[nzchar(library_types)]

for (type in library_types) {
  file <- file.path(library_dir, paste0(type, ".rds"))
  if (!file.exists(file)) {
    stop("Missing staged library file: ", file, call. = FALSE)
  }
  lib <- OpenSpecy::load_lib(type, path = library_dir)
  if (is.list(lib) && is.null(lib$wavenumber)) {
    if (!length(lib)) stop("Model library is empty: ", type, call. = FALSE)
  } else if (is.null(lib$wavenumber) || is.null(lib$spectra) ||
             is.null(lib$metadata)) {
    stop("Invalid OpenSpecy library object for: ", type, call. = FALSE)
  }
}

query <- OpenSpecy::read_any(OpenSpecy::read_extdata("raman_hdpe.csv"))
lib <- OpenSpecy::load_lib("medoid_derivative", path = library_dir)
query <- OpenSpecy::process_spec(
  query,
  active = TRUE,
  conform_spec = TRUE,
  conform_spec_args = list(range = NULL, res = 8, type = "interp"),
  smooth_intens = TRUE,
  smooth_intens_args = list(
    polynomial = 3,
    window = OpenSpecy::calc_window_points(seq(100, 4000, by = 8), 90),
    derivative = 1,
    abs = TRUE
  ),
  make_rel = TRUE
)
cors <- OpenSpecy::cor_spec(query, library = lib, conform = TRUE, type = "roll")
top <- OpenSpecy::max_cor_named(cors)
if (!length(top) || !is.finite(unname(top[[1]]))) {
  stop("Staged medoid library smoke match did not produce a finite score.",
       call. = FALSE)
}

cat("Staged library smoke test passed; top medoid score: ",
    round(unname(top[[1]]), 4), "\n", sep = "")
