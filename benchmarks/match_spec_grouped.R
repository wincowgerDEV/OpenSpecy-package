# Repeated grouped-library matching benchmark.
#
# Run from the package root:
#   Rscript benchmarks/match_spec_grouped.R

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this benchmark.", call. = FALSE)
}
devtools::load_all(quiet = TRUE)

set.seed(3301)
n_axis <- 240L
n_library <- 600L
n_query <- 180L
n_groups <- 6L
axis <- seq(600, by = 4, length.out = n_axis)
library_values <- matrix(stats::rnorm(n_axis * n_library), nrow = n_axis)
query_values <- matrix(stats::rnorm(n_axis * n_query), nrow = n_axis)
colnames(library_values) <- sprintf("library-%04d", seq_len(n_library))
colnames(query_values) <- sprintf("query-%04d", seq_len(n_query))
groups <- rep(sprintf("organization-%02d", seq_len(n_groups)),
              length.out = n_library)

library <- as_OpenSpecy(
  axis, spectra = library_values,
  metadata = data.frame(
    sample_name = colnames(library_values), organization = groups
  ), compute_file_id = FALSE
)
query <- as_OpenSpecy(axis, spectra = query_values, compute_file_id = FALSE)

# Retired comparison kernel: split the reference library, run the unchanged
# global Top-N matcher independently, combine, then establish global score order.
legacy_grouped <- function() {
  result <- data.table::rbindlist(lapply(unique(groups), function(group) {
    rows <- groups == group
    match_spec(query, filter_spec(library, rows), top_n = 2L)
  }))
  result[, .object_order := match(object_id, colnames(query$spectra))]
  result[, .library_order := match(library_id, colnames(library$spectra))]
  data.table::setorder(
    result, .object_order, -match_val, .library_order, na.last = TRUE
  )
  result[, c(".object_order", ".library_order") := NULL]
  result
}

current_grouped <- function() {
  match_spec(query, library, top_n = 2L, top_n_by = "organization")
}

# Retired ungrouped kernel kept here to detect overhead in the default route.
legacy_ungrouped <- function() {
  ident_spec(cor_spec(query, library), query, library, top_n = 2L)
}
current_ungrouped <- function() match_spec(query, library, top_n = 2L)

stopifnot(isTRUE(all.equal(
  legacy_grouped(), current_grouped(), tolerance = 1e-12,
  check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
  legacy_ungrouped(), current_ungrouped(), tolerance = 1e-12,
  check.attributes = FALSE
)))

elapsed <- function(fun, n = 5L) {
  replicate(n, unname(system.time(invisible(fun()))[["elapsed"]]))
}
grouped_old <- elapsed(legacy_grouped)
grouped_new <- elapsed(current_grouped)
ungrouped_old <- elapsed(legacy_ungrouped)
ungrouped_new <- elapsed(current_ungrouped)

result <- data.frame(
  route = c("grouped legacy split", "grouped current",
            "ungrouped legacy", "ungrouped current"),
  median_seconds = c(stats::median(grouped_old), stats::median(grouped_new),
                     stats::median(ungrouped_old), stats::median(ungrouped_new))
)
print(result, row.names = FALSE)
stopifnot(stats::median(ungrouped_new) <=
            stats::median(ungrouped_old) * 1.10)
cat(sprintf(
  "Grouped result rows: %d; maximum score block: %.2f MiB\n",
  n_query * n_groups * 2L,
  min(1000L, n_query) * ceiling(n_library / n_groups) * 8 / 1024^2
))
