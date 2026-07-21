#!/usr/bin/env Rscript

check_pages_site <- function(site_dir, max_bytes = 950 * 1024^2) {
  fail <- function(...) stop(paste0(...), call. = FALSE)
  need_file <- function(path) {
    if (!file.exists(path)) fail("Missing expected Pages file: ", path)
  }

  root_index <- file.path(site_dir, "index.html")
  app_index <- file.path(site_dir, "app", "index.html")
  need_file(root_index)
  need_file(app_index)

  if (dir.exists(file.path(site_dir, "wasm"))) {
    fail("The public Pages artifact must not contain a wasm repository tree.")
  }

  root_text <- paste(readLines(root_index, warn = FALSE), collapse = "\n")
  if (!grepl("pkgdown", root_text, fixed = TRUE)) {
    fail("The Pages root does not look like a pkgdown site.")
  }
  embed_markers <- c(
    "data-openspecy-embed",
    "id=\"openspecy-app-frame\"",
    "src=\"app/\""
  )
  missing_markers <- embed_markers[
    !vapply(embed_markers, grepl, logical(1), x = root_text, fixed = TRUE)
  ]
  if (length(missing_markers)) {
    fail("The pkgdown homepage is missing its embedded app markup: ",
         paste(missing_markers, collapse = ", "), ".")
  }
  embed_pos <- regexpr("data-openspecy-embed", root_text, fixed = TRUE)[[1]]
  frame_pos <- regexpr("id=\"openspecy-app-frame\"", root_text,
                       fixed = TRUE)[[1]]
  source_positions <- gregexpr('class="sourceCode"', root_text,
                               fixed = TRUE)[[1]]
  if (any(source_positions > embed_pos & source_positions < frame_pos)) {
    fail("The embedded app markup was rendered as a source-code block.")
  }
  app_text <- paste(readLines(app_index, warn = FALSE), collapse = "\n")
  if (!grepl("runExportedApp", app_text, fixed = TRUE)) {
    fail("The /app/ site does not look like a Shinylive export.")
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
