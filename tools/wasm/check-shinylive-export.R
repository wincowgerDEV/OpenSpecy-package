#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args)) args[[1]] else file.path("_site", "app")

fail <- function(...) stop(paste0(...), call. = FALSE)
need_file <- function(path) {
  if (!file.exists(path)) fail("Missing expected file: ", path)
}

need_file(file.path(site_dir, "index.html"))
need_file(file.path(site_dir, "pinned-wasm-library.json"))
bundle_file <- file.path(site_dir, "shinylive", "shinylive.js")
need_file(bundle_file)
bundle_text <- rawToChar(readBin(
  bundle_file, what = "raw", n = file.info(bundle_file)$size
))
if (!grepl("OPENSPECY_WORKERFS_BRIDGE_V1", bundle_text, fixed = TRUE) ||
    !grepl('e2 !== "WORKERFS" && "packages" in t2', bundle_text,
           fixed = TRUE)) {
  fail("Exported app is missing its guarded WORKERFS bridge.")
}

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

worker_file <- file.path(site_dir, "shinylive-sw.js")
loader_file <- file.path(site_dir, "shinylive", "load-shinylive-sw.js")
need_file(worker_file)
need_file(loader_file)
worker_text <- rawToChar(readBin(
  worker_file, what = "raw", n = file.info(worker_file)$size
))
loader_text <- rawToChar(readBin(
  loader_file, what = "raw", n = file.info(loader_file)$size
))
cache_markers <- c(
  "OPENSPECY_RUNTIME_CACHE_V1",
  paste0('const openspecyPackageSha = "', tolower(pin$package$commit), '";'),
  'const openspecyCachePrefix = "openspecy-shinylive-runtime-v1-";',
  'relativePath === "app.json"',
  'relativePath === "pinned-wasm-library.json"',
  'relativePath.startsWith("shinylive/")',
  'url.origin !== self.location.origin',
  "networkResponse.ok && networkResponse.status === 200",
  "key.startsWith(openspecyCachePrefix)",
  "key !== openspecyCacheName"
)
missing_cache_markers <- cache_markers[!vapply(
  cache_markers, grepl, logical(1), x = worker_text, fixed = TRUE
)]
if (length(missing_cache_markers)) {
  fail("Exported service worker is missing runtime-cache markers: ",
       paste(missing_cache_markers, collapse = ", "))
}
if (grepl("key.indexOf(version + cacheName)", worker_text, fixed = TRUE) ||
    grepl("if (useCaching)", worker_text, fixed = TRUE)) {
  fail("Exported service worker retains unsafe upstream cache handling.")
}
cache_read <- regexpr("await cache.match(request)", worker_text,
                      fixed = TRUE)[[1L]]
network_read <- regexpr("addCoiHeaders(await fetch(request))", worker_text,
                        fixed = TRUE)[[1L]]
if (cache_read < 0L || network_read < 0L || cache_read >= network_read) {
  fail("Exported service worker is not cache-first before its network fallback.")
}
if (!grepl(".then((registration) => registration.update())", loader_text,
           fixed = TRUE)) {
  fail("Shinylive loader no longer checks automatically for worker updates.")
}
if (!grepl("if (!navigator.serviceWorker.controller)", loader_text,
           fixed = TRUE) ||
    !grepl("openspecyReloadForWorker();", loader_text, fixed = TRUE)) {
  fail(paste(
    "Shinylive loader no longer reloads the initial uncontrolled document;",
    "the first successful app load would not populate the runtime cache."
  ))
}
loader_markers <- c(
  "OPENSPECY_RUNTIME_UPDATE_V1",
  paste0('const openspecyWorkerSha = "', tolower(pin$package$commit), '";'),
  'navigator.serviceWorker.addEventListener(',
  '"controllerchange", openspecyReloadForWorker, { once: true }',
  "window.location.reload();"
)
missing_loader_markers <- loader_markers[!vapply(
  loader_markers, grepl, logical(1), x = loader_text, fixed = TRUE
)]
if (length(missing_loader_markers)) {
  fail("Exported loader is missing automatic update markers: ",
       paste(missing_loader_markers, collapse = ", "))
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
