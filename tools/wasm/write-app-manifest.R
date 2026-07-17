#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

value_after <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1]] == length(args)) return(default)
  args[[hit[[1]] + 1L]]
}

artifact_ref <- value_after(
  "--artifact-ref",
  Sys.getenv("OPENSPECY_WASM_ARTIFACT_REF", "")
)
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
if (!nzchar(artifact_ref)) {
  stop("A pinned --artifact-ref or OPENSPECY_WASM_ARTIFACT_REF is required.",
       call. = FALSE)
}
package_roots <- read_manifest_lines(package_roots_file)
package_names <- package_roots
package_names[package_names == "local::."] <- desc[["Package"]]
package_names <- unique(package_names)
has_shinylive <- requireNamespace("shinylive", quietly = TRUE)
toolchain <- list(r = as.character(getRversion()))
if (has_shinylive) {
  toolchain$shinylive <- as.character(utils::packageVersion("shinylive"))
  toolchain$shinylive_assets <- shinylive:::SHINYLIVE_ASSETS_VERSION
  toolchain$webr_r <- shinylive:::WEBR_R_VERSION
}

manifest <- list(
  package = list(
    name = unname(desc[["Package"]]),
    version = unname(desc[["Version"]]),
    source = "local::.",
    commit = package_sha
  ),
  wasm_build = list(
    artifact = artifact_ref,
    immutable = TRUE,
    bundled_into_app = TRUE
  ),
  app = list(
    source = "inst/shiny",
    wasm_mode = TRUE,
    library_types = read_manifest_lines(library_types_file)
  ),
  toolchain = toolchain,
  package_roots = package_roots,
  package_names = package_names,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(manifest, out, auto_unbox = TRUE, pretty = TRUE)
cat("Wrote wasm app manifest: ", normalizePath(out, winslash = "/",
                                                mustWork = FALSE), "\n",
    sep = "")
