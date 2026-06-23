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

old_metadata_cleanup <- function(metadata) {
  out <- copy(metadata)
  cleaned <- tolower(trimws(names(out)))
  cleaned <- gsub("%", "perc", cleaned, fixed = TRUE)
  cleaned <- gsub("[^a-z0-9_]+", "_", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  setnames(out, cleaned)
  out[, user_name := fcoalesce(user_name, username)]
  out[, number_of_accumulations := fcoalesce(
    number_of_accumulations,
    numberofaccumulations,
    number_of_sample_scans
  )]
  out[, c("username", "numberofaccumulations",
          "number_of_sample_scans") := NULL]
  out
}

messy_metadata <- data.table(
  `user name` = rep(c(NA_character_, "curated"), 5000),
  UserName = rep(c("legacy", NA_character_), 5000),
  `number of accumulations` = rep(c(NA_integer_, 10L), 5000),
  NumberofAccumulations = rep(c(5L, NA_integer_), 5000),
  `Number of sample scans` = rep(c(20L, 30L), 5000)
)
clean_metadata <- getFromNamespace("lib_clean_metadata", "OpenSpecy")
name_lookup <- getFromNamespace("lib_metadata_name_lookup", "OpenSpecy")()
old_cleanup_time <- system.time(
  old_cleaned <- old_metadata_cleanup(messy_metadata)
)
new_cleanup_time <- system.time(
  new_cleaned <- clean_metadata(messy_metadata, name_lookup)
)

stopifnot(identical(old_cleaned$user_name, new_cleaned$user_name))
stopifnot(identical(old_cleaned$number_of_accumulations,
                    new_cleaned$number_of_accumulations))
message("Old metadata cleanup elapsed: ", old_cleanup_time[["elapsed"]])
message("New metadata cleanup elapsed: ", new_cleanup_time[["elapsed"]])

old_template <- unique(lib$metadata[, .(source)])
old_template[, LibraryType := NA_character_]
new_template <- make_lib_lookup_template(lib, columns = "source",
                                         add = "LibraryType")

stopifnot(identical(old_template[order(source)], new_template[order(source)]))

left <- lib
left$wavenumber <- lib$wavenumber[1:5]
left$spectra <- lib$spectra[1:5, , drop = FALSE]
right <- lib
right$wavenumber <- lib$wavenumber[3:7]
right$spectra <- lib$spectra[3:7, , drop = FALSE]
full_range <- c(min(left$wavenumber), max(right$wavenumber))

old_merge_time <- system.time({
  old_sources <- lapply(
    list(left, right),
    conform_spec,
    range = full_range,
    res = 6,
    allow_na = TRUE
  )
  old_merged <- c_spec(old_sources, range = NULL)
})
new_merge_time <- system.time({
  new_merged <- build_lib(
    list(left, right),
    recipes = list(raw = list()),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
})

stopifnot(identical(old_merged$wavenumber, new_merged$wavenumber))
stopifnot(isTRUE(all.equal(old_merged$spectra, new_merged$spectra)))
message("Old full-range merge elapsed: ", old_merge_time[["elapsed"]])
message("New build_lib merge elapsed: ", new_merge_time[["elapsed"]])

make_legacy_source_list <- function(n = 120, p = 80) {
  source <- make_benchmark_lib(n)
  source$wavenumber <- seq_len(p)
  source$spectra <- matrix(runif(p * n), nrow = p)
  colnames(source$spectra) <- paste0("legacy_", seq_len(n))
  source$metadata <- data.table(
    sample_name = colnames(source$spectra),
    intensity_units = "absorbance"
  )
  lapply(seq_len(n), function(i) {
    x <- filter_spec(source, i)
    x$spectra <- as.data.table(x$spectra)
    x
  })
}

legacy_sources <- make_legacy_source_list()
old_prepare_legacy_sources <- function(sources) {
  prepared <- lapply(sources, function(source) {
    source <- as_OpenSpecy(source)
    source$metadata <- lib_clean_metadata(source$metadata)
    source
  })
  c_spec(prepared, range = NULL)
}
new_prepare_legacy_sources <- function(sources) {
  build_lib(
    sources,
    recipes = list(raw = list()),
    range = NULL,
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
}
old_legacy_time <- system.time(
  old_legacy <- suppressMessages(old_prepare_legacy_sources(legacy_sources))
)
new_legacy_time <- system.time(
  new_legacy <- new_prepare_legacy_sources(legacy_sources)
)

stopifnot(identical(old_legacy$wavenumber, new_legacy$wavenumber))
stopifnot(isTRUE(all.equal(old_legacy$spectra, new_legacy$spectra)))
stopifnot(identical(old_legacy$metadata$sample_name,
                    new_legacy$metadata$sample_name))
message("Old legacy source-list preparation elapsed: ",
        old_legacy_time[["elapsed"]])
message("New bulk source-list preparation elapsed: ",
        new_legacy_time[["elapsed"]])
if (new_legacy_time[["elapsed"]] > old_legacy_time[["elapsed"]] * 1.1) {
  stop("Bulk source-list preparation is more than 10% slower than the ",
       "legacy per-source preparation loop")
}

make_intensity_benchmark_lib <- function(n = 400, p = 200) {
  set.seed(123)
  units <- rep(c("reflectance", "transmittance"), length.out = n)
  spectra <- matrix(runif(p * n, min = 0.2, max = 0.9), nrow = p)
  spectra[, units == "reflectance"] <-
    spectra[, units == "reflectance", drop = FALSE] * 100
  colnames(spectra) <- paste0("unit_", seq_len(n))
  as_OpenSpecy(
    seq_len(p),
    spectra = spectra,
    metadata = data.table(
      sample_name = colnames(spectra),
      intensity_units = units
    )
  )
}

old_convert_intensity <- function(x) {
  units <- x$metadata$intensity_units
  for (i in seq_len(ncol(x$spectra))) {
    x$spectra[, i] <- adj_intens(
      x$spectra[, i],
      type = units[i],
      make_rel = FALSE
    )
  }
  x$metadata$intensity_units <- "absorbance"
  attr(x, "intensity_unit") <- "absorbance"
  x
}

median_repeated_time <- function(fun, batches = 5L, iterations = 3L) {
  fun()
  median(replicate(batches, {
    unname(system.time(
      for (i in seq_len(iterations)) fun()
    )[["elapsed"]]) / iterations
  }))
}

intensity_lib <- make_intensity_benchmark_lib()
convert_intensity <- getFromNamespace(".lib_convert_intensity", "OpenSpecy")
old_converted <- old_convert_intensity(intensity_lib)
new_converted <- convert_intensity(intensity_lib)

stopifnot(isTRUE(all.equal(
  old_converted$spectra,
  new_converted$spectra,
  tolerance = 1e-12
)))
stopifnot(identical(
  old_converted$metadata$intensity_units,
  new_converted$metadata$intensity_units
))
stopifnot(identical(
  attr(old_converted, "intensity_unit"),
  attr(new_converted, "intensity_unit")
))

old_conversion_time <- median_repeated_time(
  function() old_convert_intensity(intensity_lib)
)
new_conversion_time <- median_repeated_time(
  function() convert_intensity(intensity_lib)
)
message("Old per-spectrum intensity conversion median: ",
        old_conversion_time)
message("New grouped intensity conversion median: ", new_conversion_time)
if (new_conversion_time > old_conversion_time * 1.1) {
  stop("Automatic intensity conversion is more than 10% slower than the ",
       "legacy per-spectrum loop")
}

make_na_processing_lib <- function(n = 900, p = 1000) {
  set.seed(456)
  spectra <- matrix(runif(p * n), nrow = p)
  colnames(spectra) <- paste0("na_", seq_len(n))
  spectra[seq_len(80), seq_len(n / 3)] <- NA_real_
  spectra[(p - 79):p, (n / 3 + 1):(2 * n / 3)] <- NA_real_
  as_OpenSpecy(
    seq_len(p),
    spectra = spectra,
    metadata = data.table(sample_name = colnames(spectra))
  )
}

old_manage_na_ignore <- function(x, fun) {
  x <- as_OpenSpecy(x)
  ignored <- vapply(seq_len(ncol(x$spectra)), function(i) {
    manage_na(x$spectra[, i])
  }, FUN.VALUE = logical(nrow(x$spectra)))
  ignored <- matrix(as.logical(ignored), nrow = nrow(x$spectra))
  mask_keys <- vapply(seq_len(ncol(ignored)), function(i) {
    paste(which(ignored[, i]), collapse = ",")
  }, FUN.VALUE = character(1))
  for (cols in split(seq_len(ncol(x$spectra)), mask_keys)) {
    keep <- !ignored[, cols[1L]]
    reduced <- x
    reduced$wavenumber <- x$wavenumber[keep]
    reduced$spectra <- x$spectra[keep, cols, drop = FALSE]
    reduced$metadata <- data.table::copy(x$metadata[cols])
    processed <- fun(reduced)
    x$spectra[keep, cols] <- processed$spectra
  }
  x
}

na_processing_lib <- make_na_processing_lib()
process_fun <- function(x) {
  smooth_intens(
    x,
    window = 11,
    derivative = 1,
    make_rel = TRUE
  )
}
old_processed_na <- old_manage_na_ignore(na_processing_lib, process_fun)
new_processed_na <- process_spec(
  na_processing_lib,
  conform_spec = FALSE,
  smooth_intens_args = list(window = 11, derivative = 1),
  make_rel = TRUE
)
stopifnot(isTRUE(all.equal(
  old_processed_na$spectra,
  new_processed_na$spectra,
  tolerance = 1e-12
)))

old_na_processing_time <- median_repeated_time(
  function() old_manage_na_ignore(na_processing_lib, process_fun),
  batches = 3L,
  iterations = 1L
)
new_na_processing_time <- median_repeated_time(
  function() process_spec(
    na_processing_lib,
    conform_spec = FALSE,
    smooth_intens_args = list(window = 11, derivative = 1),
    make_rel = TRUE
  ),
  batches = 3L,
  iterations = 1L
)
message("Old NA-managed processing median: ", old_na_processing_time)
message("New grouped NA-managed processing median: ", new_na_processing_time)
if (new_na_processing_time > old_na_processing_time * 1.1) {
  stop("Grouped NA-managed processing is more than 10% slower than the ",
       "legacy per-spectrum mask-key path")
}

old_run_sig_over_noise <- function(x, step = 10, prob = 0.5) {
  vapply(seq_len(ncol(x$spectra)), function(i) {
    y <- x$spectra[, i]
    if(length(y[!is.na(y)]) < step) return(NA_real_)
    rolling_max <- data.table::frollapply(y[!is.na(y)], step, max)
    rolling_max[(length(rolling_max) - (step-1)):length(rolling_max)] <- NA
    max(rolling_max, na.rm = TRUE) /
      as.numeric(stats::quantile(rolling_max[rolling_max != 0],
                                 probs = prob, na.rm = TRUE, names = FALSE))
  }, FUN.VALUE = numeric(1))
}

old_sn <- old_run_sig_over_noise(na_processing_lib, step = 10)
new_sn <- sig_noise(na_processing_lib, step = 10)
stopifnot(isTRUE(all.equal(old_sn, new_sn, tolerance = 1e-12)))

old_sn_time <- median_repeated_time(
  function() old_run_sig_over_noise(na_processing_lib, step = 10),
  batches = 3L,
  iterations = 1L
)
new_sn_time <- median_repeated_time(
  function() sig_noise(na_processing_lib, step = 10),
  batches = 3L,
  iterations = 1L
)
message("Old run signal-to-noise median: ", old_sn_time)
message("New matrix run signal-to-noise median: ", new_sn_time)
if (new_sn_time > old_sn_time * 1.1) {
  stop("Matrix run signal-to-noise is more than 10% slower than the ",
       "legacy per-spectrum loop")
}
