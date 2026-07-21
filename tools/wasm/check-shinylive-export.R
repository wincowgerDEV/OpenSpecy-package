#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args)) args[[1]] else file.path("_site", "app")

fail <- function(...) stop(paste0(...), call. = FALSE)
need_file <- function(path) {
  if (!file.exists(path)) fail("Missing expected file: ", path)
}

need_file(file.path(site_dir, "index.html"))
need_file(file.path(site_dir, "pinned-wasm-library.json"))

app_json <- list.files(site_dir, pattern = "app\\.json$", recursive = TRUE,
                       full.names = TRUE)
if (!length(app_json)) {
  fail("Unable to locate a Shinylive app.json under ", site_dir)
}

app_text <- unlist(lapply(app_json, readLines, warn = FALSE))

if (!any(grepl("openspecy.shiny.wasm.artifact", app_text, fixed = TRUE))) {
  fail("Exported app does not contain its pinned wasm artifact reference.")
}
if (any(grepl("openspecy.shiny.wasm.repo", app_text, fixed = TRUE))) {
  fail("Exported app still contains a runtime wasm repository setting.")
}
if (any(grepl("repo.r-wasm.org", app_text, fixed = TRUE))) {
  fail("Exported app contains a floating repo.r-wasm.org runtime reference.")
}
if (!any(grepl("OPENSPECY_SHINY_WASM", app_text, fixed = TRUE))) {
  fail("Exported app does not contain wasm mode configuration.")
}
if (any(grepl("options(repos", app_text, fixed = TRUE)) ||
    any(grepl("webr::install", app_text, fixed = TRUE)) ||
    any(grepl("install_wasm_packages", app_text, fixed = TRUE))) {
  fail("Exported app still installs packages from a runtime wasm repository.")
}

desc <- read.dcf("DESCRIPTION")[1, ]
pin <- jsonlite::fromJSON(file.path(site_dir, "pinned-wasm-library.json"))
if (!identical(pin$package$name, unname(desc[["Package"]])) ||
    !identical(pin$package$version, unname(desc[["Version"]])) ||
    !nzchar(pin$package$commit)) {
  fail("Pinned wasm library manifest does not match DESCRIPTION.")
}
if (!any(grepl(paste0("openspecy.shiny.wasm.package_version = \\\"",
                      desc[["Version"]], "\\\""), app_text,
               fixed = TRUE))) {
  fail("Exported app does not require OpenSpecy ", desc[["Version"]], ".")
}
if (!any(grepl(pin$package$commit, app_text, fixed = TRUE))) {
  fail("Exported app does not contain its pinned package commit.")
}

metadata_file <- file.path(site_dir, "shinylive", "webr", "packages",
                           "metadata.rds")
need_file(metadata_file)
metadata <- readRDS(metadata_file)
pinned <- Filter(function(x) {
  identical(x$type, "library") &&
    identical(x$version, pin$package$version) &&
    grepl(pin$package$commit, x$ref, fixed = TRUE)
}, metadata)
if (length(pinned) != 1L) {
  fail("Shinylive metadata does not mount the pinned OpenSpecy library image.")
}
for (asset in pinned[[1]]$assets) {
  need_file(file.path(site_dir, "shinylive", "webr", "packages",
                      pinned[[1]]$name, asset$filename))
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
