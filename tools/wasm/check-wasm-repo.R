#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1]] else file.path(
  "_site", "wasm", Sys.getenv("GITHUB_SHA"), "repo"
)
image_dir <- if (length(args) >= 2L) args[[2]] else file.path(dirname(repo),
                                                               "image")
manifest_out <- if (length(args) >= 3L) args[[3]] else file.path(
  dirname(repo), "metadata", "resolved-wasm-packages.json"
)

fail <- function(...) stop(paste0(...), call. = FALSE)

packages_files <- list.files(repo, pattern = "^PACKAGES$", recursive = TRUE,
                             full.names = TRUE)
binary_index <- packages_files[grepl("bin[/\\\\]emscripten[/\\\\]contrib",
                                     packages_files)]
if (length(binary_index) != 1L) {
  fail("Expected one binary PACKAGES index under ", repo, "; found ",
       length(binary_index), ".")
}

package_roots <- readLines(file.path("inst", "shiny", "wasm",
                                     "app-package-roots.txt"), warn = FALSE)
package_roots <- trimws(sub("#.*$", "", package_roots))
package_roots <- package_roots[nzchar(package_roots)]
desc <- read.dcf("DESCRIPTION")[1, ]
expected <- package_roots
expected[expected == "local::."] <- desc[["Package"]]
expected <- unique(expected)

packages <- as.data.frame(read.dcf(binary_index), stringsAsFactors = FALSE)
missing <- setdiff(expected, packages$Package)
if (length(missing)) {
  fail("Wasm repository is missing expected package roots: ",
       paste(missing, collapse = ", "))
}

platform_packages <- rownames(installed.packages(
  priority = "base"
))
dependency_db <- as.matrix(packages)
dependency_fields <- c("Package", "Depends", "Imports", "LinkingTo")
missing_fields <- setdiff(dependency_fields, colnames(dependency_db))
if (length(missing_fields)) {
  dependency_db <- cbind(
    dependency_db,
    matrix(NA_character_, nrow = nrow(dependency_db),
           ncol = length(missing_fields),
           dimnames = list(NULL, missing_fields))
  )
}
dependencies <- tools::package_dependencies(
  packages$Package,
  db = dependency_db,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = FALSE
)
required_dependencies <- unique(unlist(dependencies, use.names = FALSE))
# Shinylive supplies base R and the webr package, but recommended packages
# such as Matrix must be present in the pinned repository and image.
missing_dependencies <- setdiff(required_dependencies,
                                c("R", "webr", platform_packages,
                                  packages$Package))
if (length(missing_dependencies)) {
  fail("Wasm repository is missing hard package dependencies: ",
       paste(missing_dependencies, collapse = ", "))
}

package_row <- packages$Package == desc[["Package"]]
if (sum(package_row) != 1L ||
    !identical(packages$Version[package_row], unname(desc[["Version"]]))) {
  found <- if (any(package_row)) {
    paste(packages$Version[package_row], collapse = ", ")
  } else {
    "none"
  }
  fail("Wasm repository contains ", desc[["Package"]], " version ", found,
       "; expected ", desc[["Version"]], ".")
}

binary_dir <- dirname(binary_index)
missing_binaries <- vapply(seq_len(nrow(packages)), function(i) {
  !file.exists(file.path(binary_dir, paste0(packages$Package[[i]], "_",
                                            packages$Version[[i]], ".tgz")))
}, logical(1))
if (any(missing_binaries)) {
  fail("Wasm repository is missing indexed package binaries for: ",
       paste(packages$Package[missing_binaries], collapse = ", "))
}

image_files <- file.path(image_dir, c("library.data.gz",
                                      "library.js.metadata"))
if (any(!file.exists(image_files))) {
  fail("Wasm library image is incomplete under ", image_dir, ".")
}
image_metadata <- jsonlite::fromJSON(image_files[[2]])
missing_image_packages <- packages$Package[
  !paste0("/", packages$Package, "/DESCRIPTION") %in%
    image_metadata$files$filename
]
if (length(missing_image_packages)) {
  fail("Wasm library image is missing repository packages: ",
       paste(missing_image_packages, collapse = ", "))
}

manifest <- list(
  package = list(
    name = unname(desc[["Package"]]),
    version = unname(desc[["Version"]]),
    commit = Sys.getenv("GITHUB_SHA", "local")
  ),
  packages = packages[c("Package", "Version")],
  image = lapply(image_files, function(path) {
    list(name = basename(path), size = unname(file.info(path)$size),
         md5 = unname(tools::md5sum(path)))
  })
)
dir.create(dirname(manifest_out), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(manifest, manifest_out, auto_unbox = TRUE, pretty = TRUE)

cat("Wasm repository and pinned library image contain ", nrow(packages),
    " packages with a complete hard dependency closure, including ",
    desc[["Package"]], " ",
    desc[["Version"]], ".\n", sep = "")
