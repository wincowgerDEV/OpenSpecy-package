#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

value_after <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1]] == length(args)) return(default)
  args[[hit[[1]] + 1L]]
}

repo_url <- value_after("--repo-url", Sys.getenv("OPENSPECY_WASM_REPO_URL", ""))
package_sha <- value_after("--package-sha", Sys.getenv("GITHUB_SHA", "local"))
out <- value_after("--out", "wasm-app-manifest.json")
package_roots_file <- value_after(
  "--package-roots",
  file.path("inst", "shiny", "wasm", "app-package-roots.txt")
)
library_types_file <- value_after(
  "--library-types",
  file.path("inst", "shiny", "wasm", "library-types.txt")
)

read_manifest_lines <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- sub("#.*$", "", x)
  x <- trimws(x)
  x[nzchar(x)]
}

desc <- read.dcf("DESCRIPTION")[1, ]
package_roots <- read_manifest_lines(package_roots_file)
package_names <- package_roots
package_names[package_names == "local::."] <- desc[["Package"]]
package_names <- unique(package_names)

manifest <- list(
  package = list(
    name = unname(desc[["Package"]]),
    version = unname(desc[["Version"]]),
    source = "local::.",
    commit = package_sha
  ),
  wasm_repo = list(
    url = repo_url,
    immutable = TRUE
  ),
  app = list(
    source = "inst/shiny",
    wasm_mode = TRUE,
    library_types = read_manifest_lines(library_types_file)
  ),
  package_roots = package_roots,
  package_names = package_names,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(manifest, out, auto_unbox = TRUE, pretty = TRUE)
cat("Wrote wasm app manifest: ", normalizePath(out, winslash = "/",
                                                mustWork = FALSE), "\n",
    sep = "")
