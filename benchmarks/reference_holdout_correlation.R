# Compare the full-matrix holdout correlation with the superseded loop that
# repeatedly normalized the same reference library for small query blocks.

library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

make_object <- function(wavenumber, spectra, prefix) {
  colnames(spectra) <- paste0(prefix, seq_len(ncol(spectra)))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table(sample_name = colnames(spectra))
  )
}

previous_blocked_top <- function(query, library, block_size = 32L) {
  blocks <- split(
    seq_len(ncol(query$spectra)),
    ceiling(seq_len(ncol(query$spectra)) / block_size)
  )
  pieces <- lapply(blocks, function(index) {
    cors <- cor_spec(
      filter_spec(query, index), library = library, compute = "optimized"
    )
    max_cor_named(cors)
  })
  stats::setNames(
    unlist(pieces, use.names = FALSE),
    unlist(lapply(pieces, names), use.names = FALSE)
  )
}

current_full_top <- function(query, library) {
  max_cor_named(cor_spec(query, library = library, compute = "optimized"))
}

median_time <- function(fun, batches = 3L) {
  stats::median(replicate(
    batches, as.numeric(system.time(fun())[["elapsed"]])
  ))
}

set.seed(811)
wavenumber <- seq(400, 1800, length.out = 180L)
library <- make_object(
  wavenumber,
  matrix(stats::rnorm(length(wavenumber) * 2000L), nrow = length(wavenumber)),
  "library_"
)
query <- make_object(
  wavenumber,
  matrix(stats::rnorm(length(wavenumber) * 256L), nrow = length(wavenumber)),
  "query_"
)

previous <- previous_blocked_top(query, library)
current <- current_full_top(query, library)
stopifnot(
  identical(names(previous), names(current)),
  isTRUE(all.equal(unname(previous), unname(current), tolerance = 1e-12))
)

# Warm both paths before repeated measurements.
invisible(previous_blocked_top(query, library))
invisible(current_full_top(query, library))
previous_time <- median_time(function() previous_blocked_top(query, library))
current_time <- median_time(function() current_full_top(query, library))
message("Previous repeated-library correlation median: ", previous_time, "s")
message("Current full-matrix correlation median: ", current_time, "s")
if (current_time > previous_time * 1.1) {
  stop("Full-matrix holdout correlation is more than 10% slower",
       call. = FALSE)
}
