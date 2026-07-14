#!/usr/bin/env Rscript

value_after <- function(args, flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1]] == length(args)) return(default)
  args[[hit[[1]] + 1L]]
}

read_manifest_lines <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- trimws(sub("#.*$", "", x))
  x[nzchar(x)]
}

binary_packages_index <- function(repo_dir) {
  indexes <- list.files(repo_dir, pattern = "^PACKAGES$", recursive = TRUE,
                        full.names = TRUE)
  indexes <- indexes[grepl("bin[/\\\\]emscripten[/\\\\]contrib",
                           indexes)]
  if (length(indexes) != 1L) {
    stop("Expected one binary wasm PACKAGES index under ", repo_dir,
         "; found ", length(indexes), ".", call. = FALSE)
  }
  indexes[[1]]
}

bundle_wasm_library <- function(image_dir, repo_dir, site_dir, package_sha,
                                description_file = "DESCRIPTION",
                                package_roots_file = file.path(
                                  "inst", "shiny", "wasm",
                                  "app-package-roots.txt"
                                ),
                                manifest_out = file.path(
                                  site_dir, "pinned-wasm-library.json"
                                )) {
  if (!nzchar(package_sha)) {
    stop("A non-empty package commit SHA is required.", call. = FALSE)
  }

  desc <- read.dcf(description_file)[1, ]
  package_name <- unname(desc[["Package"]])
  package_version <- unname(desc[["Version"]])
  packages_index <- binary_packages_index(repo_dir)
  packages <- as.data.frame(read.dcf(packages_index), stringsAsFactors = FALSE)

  row <- packages$Package == package_name
  if (sum(row) != 1L || !identical(packages$Version[row], package_version)) {
    found <- if (any(row)) paste(packages$Version[row], collapse = ", ") else "none"
    stop("Pinned wasm repository has ", package_name, " version ", found,
         "; expected ", package_version, ".", call. = FALSE)
  }

  roots <- read_manifest_lines(package_roots_file)
  roots[roots == "local::."] <- package_name
  missing_roots <- setdiff(unique(roots), packages$Package)
  if (length(missing_roots)) {
    stop("Pinned wasm repository is missing package roots: ",
         paste(missing_roots, collapse = ", "), call. = FALSE)
  }

  platform_packages <- rownames(installed.packages(
    priority = c("base", "recommended")
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
  missing_dependencies <- setdiff(
    required_dependencies,
    c("R", platform_packages, packages$Package)
  )
  if (length(missing_dependencies)) {
    stop("Pinned wasm repository is missing hard package dependencies: ",
         paste(missing_dependencies, collapse = ", "), call. = FALSE)
  }

  image_files <- file.path(image_dir, c("library.data.gz",
                                        "library.js.metadata"))
  missing_image <- image_files[!file.exists(image_files)]
  if (length(missing_image)) {
    stop("Pinned wasm library image is incomplete: ",
         paste(missing_image, collapse = ", "), call. = FALSE)
  }

  image_metadata <- jsonlite::fromJSON(image_files[[2]])
  expected_descriptions <- paste0("/", packages$Package, "/DESCRIPTION")
  missing_descriptions <- setdiff(expected_descriptions,
                                  image_metadata$files$filename)
  if (length(missing_descriptions)) {
    stop("Pinned wasm library image is missing package descriptions: ",
         paste(missing_descriptions, collapse = ", "), call. = FALSE)
  }

  package_key <- "OpenSpecyPinned"
  package_dir <- file.path(site_dir, "shinylive", "webr", "packages",
                           package_key)
  if (dir.exists(package_dir)) unlink(package_dir, recursive = TRUE)
  dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(image_files, package_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Unable to copy the pinned wasm library image into Shinylive.",
         call. = FALSE)
  }

  bundled_files <- file.path(package_dir, basename(image_files))
  assets <- lapply(bundled_files, function(path) {
    list(filename = basename(path), size = unname(file.info(path)$size))
  })
  package_metadata <- list(
    name = package_key,
    version = package_version,
    ref = paste0(package_name, "@", package_version, "#", package_sha),
    type = "library",
    cached = TRUE,
    assets = assets,
    path = file.path("packages", package_key, "library.data.gz")
  )

  metadata_file <- file.path(site_dir, "shinylive", "webr", "packages",
                             "metadata.rds")
  saveRDS(setNames(list(package_metadata), package_key), metadata_file)

  manifest <- list(
    package = list(name = package_name, version = package_version,
                   commit = package_sha),
    source = list(type = "r-wasm-library-image",
                  packages_index = gsub("\\\\", "/", packages_index)),
    packages = packages[c("Package", "Version")],
    files = lapply(bundled_files, function(path) {
      list(
        name = basename(path),
        size = unname(file.info(path)$size),
        md5 = unname(tools::md5sum(path))
      )
    })
  )
  dir.create(dirname(manifest_out), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, manifest_out, auto_unbox = TRUE, pretty = TRUE)

  cat("Bundled pinned wasm library image for ", package_name, " ",
      package_version, " (", substr(package_sha, 1, 7), ").\n", sep = "")
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  image_dir <- value_after(args, "--image-dir")
  repo_dir <- value_after(args, "--repo-dir")
  site_dir <- value_after(args, "--site-dir")
  package_sha <- value_after(args, "--package-sha", Sys.getenv("GITHUB_SHA", ""))
  manifest_out <- value_after(
    args, "--manifest-out",
    if (is.null(site_dir)) NULL else file.path(site_dir,
                                               "pinned-wasm-library.json")
  )

  required <- list(image_dir = image_dir, repo_dir = repo_dir,
                   site_dir = site_dir, package_sha = package_sha)
  missing <- vapply(required, function(x) is.null(x) || !nzchar(x), logical(1))
  if (any(missing)) {
    stop("--image-dir, --repo-dir, --site-dir, and --package-sha are required.",
         call. = FALSE)
  }

  bundle_wasm_library(image_dir, repo_dir, site_dir, package_sha,
                      manifest_out = manifest_out)
}
