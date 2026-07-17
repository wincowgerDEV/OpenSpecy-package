#!/usr/bin/env Rscript

check_pages_site <- function(site_dir, max_bytes = 950 * 1024^2) {
  fail <- function(...) stop(paste0(...), call. = FALSE)
  need_file <- function(path) {
    if (!file.exists(path)) fail("Missing expected Pages file: ", path)
  }

  root_index <- file.path(site_dir, "index.html")
  app_index <- file.path(site_dir, "openspecy", "index.html")
  need_file(root_index)
  need_file(app_index)

  if (dir.exists(file.path(site_dir, "wasm"))) {
    fail("The public Pages artifact must not contain a wasm repository tree.")
  }

  root_text <- paste(readLines(root_index, warn = FALSE), collapse = "\n")
  if (!grepl("pkgdown", root_text, fixed = TRUE)) {
    fail("The Pages root does not look like a pkgdown site.")
  }
  app_text <- paste(readLines(app_index, warn = FALSE), collapse = "\n")
  if (!grepl("runExportedApp", app_text, fixed = TRUE)) {
    fail("The /openspecy/ site does not look like a Shinylive export.")
  }

  files <- list.files(site_dir, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  bytes <- sum(file.info(files)$size, na.rm = TRUE)
  if (!is.finite(bytes) || bytes >= max_bytes) {
    fail("The assembled Pages site is too large: ", bytes,
         " bytes (limit ", max_bytes, ").")
  }

  cat("Pages site check passed: ", normalizePath(
    site_dir, winslash = "/", mustWork = FALSE
  ), " (", bytes, " bytes)\n", sep = "")
  invisible(bytes)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  site_dir <- if (length(args)) args[[1]] else "_site"
  check_pages_site(site_dir)
}
