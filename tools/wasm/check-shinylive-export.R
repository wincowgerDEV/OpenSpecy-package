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

expected <- paste0(c("medoid_derivative", "medoid_nobaseline",
                     "model_derivative", "model_nobaseline"), ".rds")
missing <- expected[!vapply(expected, function(file) {
  any(grepl(file, app_text, fixed = TRUE))
}, logical(1))]
if (length(missing)) {
  fail("Missing staged library files in Shinylive app manifest: ",
       paste(missing, collapse = ", "))
}

cat("Shinylive export check passed for ", site_dir, "\n", sep = "")
