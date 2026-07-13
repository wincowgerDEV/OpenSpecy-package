#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args)) args[[1]] else file.path("_site", "openspecy")

fail <- function(...) stop(paste0(...), call. = FALSE)
need_file <- function(path) {
  if (!file.exists(path)) fail("Missing expected file: ", path)
}

need_file(file.path(site_dir, "index.html"))

app_json <- list.files(site_dir, pattern = "app\\.json$", recursive = TRUE,
                       full.names = TRUE)
if (!length(app_json)) {
  fail("Unable to locate a Shinylive app.json under ", site_dir)
}

all_text <- unlist(lapply(
  list.files(site_dir, recursive = TRUE, full.names = TRUE),
  function(path) {
    if (file.info(path)$size > 5e6) return(character())
    tryCatch(readLines(path, warn = FALSE), error = function(...) character())
  }
))
app_text <- unlist(lapply(app_json, readLines, warn = FALSE))

if (!any(grepl("openspecy.shiny.wasm.repo", app_text, fixed = TRUE))) {
  fail("Exported app does not contain the pinned wasm repo config.")
}
if (any(grepl("repo.r-wasm.org", app_text, fixed = TRUE))) {
  fail("Exported app contains a floating repo.r-wasm.org runtime reference.")
}
if (!any(grepl("OPENSPECY_SHINY_WASM", app_text, fixed = TRUE))) {
  fail("Exported app does not contain wasm mode configuration.")
}

library_files <- list.files(site_dir, pattern = "\\.rds$", recursive = TRUE)
expected <- paste0(c("medoid_derivative", "medoid_nobaseline",
                     "model_derivative", "model_nobaseline"), ".rds")
missing <- setdiff(expected, basename(library_files))
if (length(missing)) {
  fail("Missing staged library files in export: ",
       paste(missing, collapse = ", "))
}

cat("Shinylive export check passed for ", site_dir, "\n", sep = "")
