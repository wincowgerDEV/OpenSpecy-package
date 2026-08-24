#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: evict-wasm-cache-package.R REPO_DIR PACKAGE", call. = FALSE)
}

repo <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
package <- args[[2L]]
if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package)) {
  stop("Invalid package name: ", package, call. = FALSE)
}

escape_regex <- function(value) {
  gsub("([][{}()+*^$|\\?.])", "\\\\\\1", value)
}

indexes <- list.files(repo, pattern = "^PACKAGES$", recursive = TRUE,
                      full.names = TRUE)
if (!length(indexes)) {
  stop("The cache seed has no PACKAGES index under ", repo, ".",
       call. = FALSE)
}

for (index in indexes) {
  directory <- dirname(index)
  binary <- grepl("[/\\\\]bin[/\\\\]emscripten[/\\\\]contrib[/\\\\]",
                  index)
  archive_pattern <- paste0("^", escape_regex(package), "_.*\\.",
                            if (binary) "tgz" else "tar\\.gz", "$")
  archives <- list.files(directory, pattern = archive_pattern,
                         full.names = TRUE)
  if (length(archives)) unlink(archives, force = TRUE)

  tools::write_PACKAGES(
    directory,
    type = if (binary) "mac.binary" else "source",
    latestOnly = TRUE,
    addFiles = TRUE
  )
  rebuilt <- as.data.frame(read.dcf(file.path(directory, "PACKAGES")),
                           stringsAsFactors = FALSE)
  if ("Package" %in% names(rebuilt) && package %in% rebuilt$Package) {
    stop("Failed to evict ", package, " from ", directory, ".",
         call. = FALSE)
  }
}

cat("Dependency cache is ready; evicted local package ", package, ".\n",
    sep = "")
