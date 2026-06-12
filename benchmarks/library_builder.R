# Compare script-style library-building snippets with the package helpers.
# This file is intentionally outside tests because it is development evidence,
# not a CRAN test surface.

library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

make_benchmark_lib <- function(n = 40) {
  wavenumber <- seq(100, 700, by = 100)
  spectra <- replicate(n, runif(length(wavenumber)))
  colnames(spectra) <- paste0("s", seq_len(n))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table(
      sample_name = colnames(spectra),
      source = rep(c("a", "b", "c", "missing"), length.out = n),
      material_class = rep(c("class_a", "class_b"), length.out = n)
    )
  )
}

old_left_join_metadata <- function(x, lookup) {
  out <- copy(x$metadata)
  out <- merge(out, lookup, by = "source", all.x = TRUE, sort = FALSE)
  row_order <- match(x$metadata$sample_name, out$sample_name)
  out[row_order, ]
}

lib <- make_benchmark_lib()
lookup <- data.table(source = c("a", "b", "c"),
                     LibraryType = c("type_a", "type_b", "type_c"))

old_time <- system.time(old_joined <- old_left_join_metadata(lib, lookup))
new_time <- system.time(
  new_joined <- suppressWarnings(
    join_lib_metadata(lib, lookup, by = "source", return = "table")
  )
)

stopifnot(identical(old_joined$sample_name, new_joined$sample_name))
stopifnot(identical(old_joined$LibraryType, new_joined$LibraryType))

message("Old metadata join elapsed: ", old_time[["elapsed"]])
message("New metadata join elapsed: ", new_time[["elapsed"]])

old_template <- unique(lib$metadata[, .(source)])
old_template[, LibraryType := NA_character_]
new_template <- make_lib_lookup_template(lib, columns = "source",
                                         add = "LibraryType")

stopifnot(identical(old_template[order(source)], new_template[order(source)]))
