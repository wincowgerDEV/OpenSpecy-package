#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1]] else file.path("_site", "wasm",
                                                   Sys.getenv("GITHUB_SHA"),
                                                   "repo")

fail <- function(...) stop(paste0(...), call. = FALSE)

packages_files <- list.files(repo, pattern = "^PACKAGES$", recursive = TRUE,
                             full.names = TRUE)
if (!length(packages_files)) {
  fail("No PACKAGES index found under wasm repository: ", repo)
}

package_roots <- readLines(file.path("inst", "shiny", "wasm",
                                     "app-package-roots.txt"), warn = FALSE)
package_roots <- trimws(sub("#.*$", "", package_roots))
package_roots <- package_roots[nzchar(package_roots)]
desc <- read.dcf("DESCRIPTION")[1, ]
expected <- package_roots
expected[expected == "local::."] <- desc[["Package"]]
expected <- unique(expected)

available <- unique(unlist(lapply(packages_files, function(path) {
  as.data.frame(read.dcf(path), stringsAsFactors = FALSE)$Package
})))

missing <- setdiff(expected, available)
if (length(missing)) {
  fail("Wasm repository is missing expected package roots: ",
       paste(missing, collapse = ", "))
}

cat("Wasm repository check passed for ", length(expected),
    " expected package roots.\n", sep = "")
