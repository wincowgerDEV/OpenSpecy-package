#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

value_after <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1]] == length(args)) return(default)
  args[[hit[[1]] + 1L]]
}

out_dir <- value_after("--out-dir", file.path("_wasm", "libraries"))
library_types_file <- value_after(
  "--library-types",
  file.path("inst", "shiny", "wasm", "library-types.txt")
)
manifest_out <- value_after("--manifest-out",
                            file.path(out_dir, "library-manifest.json"))

read_manifest_lines <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- sub("#.*$", "", x)
  x <- trimws(x)
  x[nzchar(x)]
}

library_types <- read_manifest_lines(library_types_file)
library_revisions <- c(
  medoid_derivative = "iThmNyMeUKhkWMvbBxQqpf1sESdQBFTs",
  medoid_nobaseline = "CLJCDpeFCMZw4hFUW4Y1QFT2cj23W1Yz",
  model_derivative = "Wk7H.Zjj4coxiMGlqQlXjV5smmZou.IH",
  model_nobaseline = "rtJY7zQTDzRISfGpvYrU0bcj8nnRYs26"
)

missing_revisions <- setdiff(library_types, names(library_revisions))
if (length(missing_revisions)) {
  stop("Missing pinned revisions for: ",
       paste(missing_revisions, collapse = ", "), call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

entries <- lapply(library_types, function(type) {
  message("Staging OpenSpecy library: ", type)
  OpenSpecy::get_lib(
    type,
    path = out_dir,
    revision = unname(library_revisions[[type]]),
    aws = TRUE
  )

  lib <- OpenSpecy::load_lib(type, path = out_dir)
  file <- file.path(out_dir, paste0(type, ".rds"))
  md <- if (is.list(lib) && is.null(lib$wavenumber)) {
    first <- lib[[1]]
    list(
      spectra = sum(vapply(lib, function(x) ncol(x$spectra), integer(1))),
      wavenumber_min = min(vapply(lib, function(x) min(x$wavenumber),
                                  numeric(1))),
      wavenumber_max = max(vapply(lib, function(x) max(x$wavenumber),
                                  numeric(1)))
    )
  } else {
    list(
      spectra = ncol(lib$spectra),
      wavenumber_min = min(lib$wavenumber),
      wavenumber_max = max(lib$wavenumber)
    )
  }

  utils::modifyList(
    list(
      type = type,
      file = basename(file),
      revision = unname(library_revisions[[type]]),
      bytes = file.info(file)$size
    ),
    md
  )
})

manifest <- list(
  library_types = library_types,
  libraries = entries,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

jsonlite::write_json(manifest, manifest_out, auto_unbox = TRUE, pretty = TRUE)
print(data.frame(
  type = vapply(entries, `[[`, character(1), "type"),
  bytes = vapply(entries, `[[`, numeric(1), "bytes")
))
