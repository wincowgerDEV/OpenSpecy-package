#!/usr/bin/env Rscript

read_wasm_roots <- function(path) {
  roots <- readLines(path, warn = FALSE)
  roots <- trimws(sub("#.*$", "", roots))
  roots[nzchar(roots)]
}

hard_dependencies <- function(packages, db, recursive = FALSE) {
  required_fields <- c("Package", "Depends", "Imports", "LinkingTo")
  missing_fields <- setdiff(required_fields, colnames(db))
  if (length(missing_fields)) {
    additions <- matrix(
      NA_character_, nrow = nrow(db), ncol = length(missing_fields),
      dimnames = list(rownames(db), missing_fields)
    )
    db <- cbind(db, additions)
  }
  dependencies <- tools::package_dependencies(
    packages,
    db = db,
    which = c("Depends", "Imports", "LinkingTo"),
    recursive = recursive
  )
  unique(unlist(dependencies, use.names = FALSE))
}

wasm_available_packages <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  available.packages(repos = repos)
}

resolve_wasm_package_roots <- function(
    roots_file,
    description_file = "DESCRIPTION",
    available = wasm_available_packages(),
    platform_packages = rownames(installed.packages(
      priority = c("base", "recommended")
    ))) {
  roots <- read_wasm_roots(roots_file)
  desc <- read.dcf(description_file)[1, ]
  package_name <- unname(desc[["Package"]])

  unsupported <- roots[grepl("::", roots, fixed = TRUE) & roots != "local::."]
  if (length(unsupported)) {
    stop("Unsupported wasm package references: ",
         paste(unsupported, collapse = ", "), call. = FALSE)
  }

  local_db <- matrix(desc, nrow = 1L, dimnames = list(package_name,
                                                       names(desc)))
  local_dependencies <- hard_dependencies(package_name, local_db)
  requested <- unique(c(roots[roots != "local::."], local_dependencies))
  requested <- setdiff(requested, c("R", platform_packages))

  unavailable <- setdiff(requested, rownames(available))
  if (length(unavailable)) {
    stop("Unable to resolve wasm package roots from the configured R ",
         "repositories: ", paste(unavailable, collapse = ", "),
         call. = FALSE)
  }

  closure <- unique(c(requested, hard_dependencies(requested, available,
                                                    recursive = TRUE)))
  closure <- sort(setdiff(closure, c("R", platform_packages, package_name)))
  c(if ("local::." %in% roots) "local::.", closure)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop("Usage: resolve-wasm-package-roots.R ROOTS_FILE OUTPUT_FILE",
         call. = FALSE)
  }
  resolved <- resolve_wasm_package_roots(args[[1]])
  dir.create(dirname(args[[2]]), recursive = TRUE, showWarnings = FALSE)
  writeLines(resolved, args[[2]])
  cat("Resolved ", length(resolved),
      " wasm package build references.\n", sep = "")
}
