# Compare the superseded blockwise pruning matcher with the one-shot
# cor_spec() path. Run from the package root with:
# Rscript benchmarks/prune_correlation.R

devtools::load_all(quiet = TRUE)

set.seed(20260831)
wavenumber <- seq(400, 4000, length.out = 512)
n_target <- 400L
n_eligible <- 1500L
n_total <- 3000L

spectra <- matrix(rnorm(length(wavenumber) * n_total),
                  nrow = length(wavenumber), ncol = n_total)
spectra <- apply(spectra, 2L, cumsum)
spectra[sample.int(length(spectra), length(spectra) * 0.002)] <- NA_real_
ids <- sprintf("benchmark-%05d", seq_len(n_total))
colnames(spectra) <- ids
library <- as_OpenSpecy(
  wavenumber,
  spectra,
  metadata = data.table::data.table(sample_name = ids)
)

target <- seq_len(n_target)
candidates <- seq_len(n_eligible)
exclude <- c(2200, 2420)
normalized <- OpenSpecy:::.lib_prune_normalize(
  library$spectra, library$wavenumber, exclude
)

previous_match <- function() {
  OpenSpecy:::.lib_prune_best_match(
    target, candidates, normalized, ids, exclude_self = TRUE
  )
}

current_match <- function() {
  correlations <- OpenSpecy:::.lib_prune_correlations(
    library, target, candidates, exclude, ids
  )
  local <- max.col(correlations, ties.method = "first")
  scores <- correlations[cbind(seq_along(target), local)]
  list(index = candidates[local], correlation = scores)
}

previous <- previous_match()
current <- current_match()
stopifnot(
  identical(previous$index, current$index),
  isTRUE(all.equal(previous$correlation, current$correlation,
                   tolerance = 1e-10))
)

elapsed <- function(fun, n = 3L) {
  median(replicate(n, system.time(fun())[["elapsed"]]))
}
previous_seconds <- elapsed(previous_match)
current_seconds <- elapsed(current_match)
result <- data.frame(
  implementation = c("previous_blockwise_full_library", "current_cor_spec"),
  seconds = c(previous_seconds, current_seconds)
)
print(result, row.names = FALSE)
cat(sprintf("speedup: %.2fx\n", previous_seconds / current_seconds))

if (current_seconds > previous_seconds * 1.1) {
  stop("The one-shot pruning correlation path regressed by more than 10%",
       call. = FALSE)
}
