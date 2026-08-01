#!/usr/bin/env Rscript

fail <- function(...) stop(paste0(...), call. = FALSE)

check_wasm_artifact <- function(pinned_dir, package_sha,
                                description_file = "DESCRIPTION",
                                verified_manifest = NULL) {
  if (!grepl("^[0-9a-fA-F]{40}$", package_sha)) {
    fail("package_sha must be an exact 40-character commit.")
  }
  desc <- read.dcf(description_file)[1L, ]
  package_name <- unname(desc[["Package"]])
  package_version <- unname(desc[["Version"]])
  metadata_dir <- file.path(pinned_dir, "metadata")
  artifact_path <- file.path(metadata_dir, "wasm-app-manifest.json")
  resolved_path <- file.path(metadata_dir, "resolved-wasm-packages.json")
  required <- c(
    artifact_path,
    resolved_path,
    file.path(pinned_dir, "image", "library.data.gz"),
    file.path(pinned_dir, "image", "library.js.metadata")
  )
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    fail("Pinned artifact is incomplete: ", paste(missing, collapse = ", "))
  }

  artifact <- jsonlite::fromJSON(artifact_path, simplifyVector = FALSE)
  resolved <- jsonlite::fromJSON(resolved_path, simplifyVector = FALSE)
  expected_ref <- paste0("openspecy-wasm-", package_sha)
  assertions <- list(
    "artifact package name" = c(artifact$package$name, package_name),
    "artifact package version" = c(artifact$package$version, package_version),
    "artifact package commit" = c(artifact$package$commit, package_sha),
    "artifact reference" = c(artifact$wasm_build$artifact, expected_ref),
    "resolved package name" = c(resolved$package$name, package_name),
    "resolved package version" = c(resolved$package$version, package_version),
    "resolved package commit" = c(resolved$package$commit, package_sha)
  )
  for (label in names(assertions)) {
    values <- assertions[[label]]
    if (length(values) != 2L || !identical(values[[1L]], values[[2L]])) {
      found <- if (length(values)) values[[1L]] else "<missing>"
      expected <- if (length(values) >= 2L) values[[2L]] else "<missing>"
      fail(label, " mismatch: found '", found, "'; expected '", expected,
           "'.")
    }
  }

  package_rows <- vapply(resolved$packages, function(package) {
    identical(package$Package, package_name) &&
      identical(package$Version, package_version)
  }, logical(1L))
  if (sum(package_rows) != 1L) {
    fail("Resolved package list must contain exactly one ", package_name, " ",
         package_version, " entry.")
  }
  expected_images <- c("library.data.gz", "library.js.metadata")
  if (!is.list(resolved$image) || length(resolved$image) != 2L) {
    fail("Image manifest must contain exactly library.data.gz and ",
         "library.js.metadata.")
  }
  image_names <- vapply(resolved$image, function(image) {
    if (!is.list(image) || length(image$name) != 1L ||
        !is.character(image$name) || is.na(image$name)) return(NA_character_)
    image$name
  }, character(1L))
  if (anyNA(image_names) || anyDuplicated(image_names) ||
      !setequal(image_names, expected_images)) {
    fail("Image manifest must contain exactly library.data.gz and ",
         "library.js.metadata with unique names.")
  }
  for (image in resolved$image) {
    image_path <- file.path(pinned_dir, "image", image$name)
    if (!file.exists(image_path)) {
      fail("Image manifest refers to missing file ", image$name, ".")
    }
    actual_size <- unname(file.info(image_path)$size)
    if (length(image$size) != 1L || !is.numeric(image$size) ||
        is.na(image$size) || as.numeric(image$size) != actual_size) {
      fail("Image ", image$name, " size mismatch: found ", actual_size,
           "; expected ", if(length(image$size)) image$size else "<missing>",
           ".")
    }
    if (length(image$md5) != 1L || !is.character(image$md5) ||
        is.na(image$md5) || !grepl("^[0-9a-fA-F]{32}$", image$md5)) {
      fail("Image ", image$name, " has an invalid MD5 in the manifest.")
    }
    actual <- unname(tools::md5sum(image_path))
    if (!identical(tolower(actual), tolower(image$md5))) {
      fail("Image ", image$name, " MD5 mismatch: found ", actual,
           "; expected ", image$md5, ".")
    }
  }
  if (!is.null(verified_manifest)) {
    if (!file.exists(verified_manifest)) {
      fail("Verified wasm manifest is missing: ", verified_manifest)
    }
    verified <- jsonlite::fromJSON(verified_manifest, simplifyVector = FALSE)
    if (!identical(resolved[c("package", "packages", "image")],
                   verified[c("package", "packages", "image")])) {
      fail("Stored resolved wasm manifest does not match the repository and ",
           "image verification manifest.")
    }
  }
  cat("Pinned wasm artifact matches ", package_name, " ", package_version,
      " at ", package_sha, ".\n", sep = "")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L) {
    fail(paste(
      "Usage: check-wasm-artifact.R <pinned-dir> <package-sha>",
      "[verified-manifest]"
    ))
  }
  check_wasm_artifact(
    args[[1L]], args[[2L]],
    verified_manifest = if(length(args) >= 3L) args[[3L]] else NULL
  )
}
