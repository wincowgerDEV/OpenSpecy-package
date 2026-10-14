# Repeated same-output benchmark for the immutable dense correlation view.
# The raw-matrix path below is the frozen pre-cache blockwise kernel. Run from
# the package root with:
# Rscript benchmarks/reference_cross_class_pruning.R
# A 4,000-spectrum run automatically exercises sampled PAM at n=3,001:
# OPENSPECY_CROSS_CLASS_BENCHMARK_N=4000
# OPENSPECY_CROSS_CLASS_BENCHMARK_P=600
# Set OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_N=0 to skip that large case.

devtools::load_all(quiet = TRUE)

main <- function() {
  old_options <- options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )
  on.exit(options(old_options), add = TRUE)

  set.seed(20260929)
  n_spectra <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_N", unset = "1000"
  ))
  n_wavenumbers <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_P", unset = "600"
  ))
  block_size <- 256L
  repetitions <- 3L
  pam_small_n <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_SMALL_N", unset = "160"
  ))
  pam_large_default <- if (n_spectra > 3000L) "3001" else "0"
  pam_large_n <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_N",
    unset = pam_large_default
  ))
  pam_large_p <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_P", unset = "80"
  ))
  pam_large_samples <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_SAMPLES", unset = "2"
  ))
  pam_large_sample_size <- as.integer(Sys.getenv(
    "OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_SAMPLE_SIZE", unset = "400"
  ))
  stopifnot(
    is.finite(pam_small_n), pam_small_n > 20L,
    is.finite(pam_large_n), pam_large_n == 0L || pam_large_n > 3000L,
    is.finite(pam_large_p), pam_large_p > 10L,
    is.finite(pam_large_samples), pam_large_samples > 0L,
    is.finite(pam_large_sample_size), pam_large_sample_size > 20L
  )

  normalized <- matrix(
    rnorm(n_spectra * n_wavenumbers),
    nrow = n_spectra, ncol = n_wavenumbers
  )
  normalized <- normalized / sqrt(rowSums(normalized^2))
  ids <- sprintf("benchmark-%05d", seq_len(n_spectra))
  classes <- sprintf(
    "polyclass-%02d", (seq_len(n_spectra) - 1L) %% 20L + 1L
  )
  libraries <- sprintf(
    "library-%02d", (seq_len(n_spectra) - 1L) %% 25L + 1L
  )
  pools <- rep(c("raman", "infrared"), length.out = n_spectra)
  axis <- seq_len(n_wavenumbers)

  # Independent components have one suspect supported by two other libraries.
  independent_n <- min(20L, n_spectra %/% 7L)
  groups <- matrix(seq_len(3L * independent_n), ncol = 3L, byrow = TRUE)
  for (group in seq_len(nrow(groups))) {
    index <- groups[group, ]
    normalized[index[2L], ] <- normalized[index[1L], ]
    normalized[index[3L], ] <- normalized[index[1L], ]
    pools[index] <- "raman"
    classes[index] <- c(
      paste0("polysuspect-", group),
      paste0("polyreference-", group),
      paste0("polyreference-", group)
    )
    libraries[index] <- c(
      "suspect-library", "reference-library-a", "reference-library-b"
    )
  }

  # Same-library components contain one degree-three suspect and three
  # same-class references. The active-degree pass removes only each suspect.
  internal_start <- 3L * independent_n + 1L
  internal_n <- min(20L, (n_spectra - internal_start + 1L) %/% 4L)
  internal_groups <- matrix(
    seq.int(internal_start, length.out = 4L * internal_n),
    ncol = 4L, byrow = TRUE
  )
  for (group in seq_len(nrow(internal_groups))) {
    index <- internal_groups[group, ]
    normalized[index[-1L], ] <- normalized[
      rep(index[1L], length(index) - 1L), , drop = FALSE
    ]
    pools[index] <- "raman"
    classes[index] <- c(
      paste0("polyinternal-suspect-", group),
      rep(paste0("polyinternal-reference-", group), 3L)
    )
    libraries[index] <- "internal-majority-library"
  }

  # Frozen numerical kernel used before the dense view was introduced. The
  # package raw-matrix route intentionally retains this operation and shape.
  frozen_bounded_values <- function(query, candidates, values = normalized) {
    tcrossprod(
      values[query, , drop = FALSE], values[candidates, , drop = FALSE]
    )
  }
  run_frozen <- function(index = seq_len(n_spectra),
                         current_classes = classes[index], threshold = 0.9) {
    OpenSpecy:::.lib_prune_cross_class_conflicts(
      current_classes, pools[index], normalized[index, , drop = FALSE],
      ids[index], libraries[index], threshold = threshold,
      progress = FALSE, block_size = block_size
    )
  }
  make_view <- function() {
    OpenSpecy:::.lib_new_correlation_view(
      normalized, ids, pools, axis, state = "benchmark",
      progress = FALSE, block_size = block_size
    )
  }
  run_cached <- function(view, index = seq_len(n_spectra),
                         current_classes = classes[index], threshold = 0.9) {
    OpenSpecy:::.lib_prune_cross_class_conflicts(
      current_classes, pools[index], view, ids[index], libraries[index],
      threshold = threshold, progress = FALSE, block_size = block_size
    )
  }
  compare_table <- function(reference, candidate, label,
                            allow_tied_opponent = FALSE) {
    stopifnot(identical(names(reference), names(candidate)))
    tied_opponent_differences <- 0L
    numeric_columns <- names(reference)[vapply(reference, is.numeric, logical(1L))]
    for (column in numeric_columns) {
      stopifnot(isTRUE(all.equal(
        reference[[column]], candidate[[column]], tolerance = 1e-12
      )))
    }
    nonnumeric <- setdiff(names(reference), numeric_columns)
    exact_columns <- if (allow_tied_opponent) {
      setdiff(nonnumeric, "matched_id")
    } else {
      nonnumeric
    }
    if (!identical(reference[, ..exact_columns], candidate[, ..exact_columns])) {
      difference <- all.equal(
        reference[, ..exact_columns], candidate[, ..exact_columns],
        check.attributes = FALSE
      )
      stop(
        label, " nonnumeric audit columns differ: ",
        paste(difference, collapse = "; "), call. = FALSE
      )
    }
    if (allow_tied_opponent && "matched_id" %in% nonnumeric) {
      same_opponent <-
        (is.na(reference$matched_id) & is.na(candidate$matched_id)) |
        (!is.na(reference$matched_id) & !is.na(candidate$matched_id) &
           reference$matched_id == candidate$matched_id)
      different_opponent <- which(!same_opponent)
      tied_opponent_differences <- length(different_opponent)
      if (length(different_opponent)) {
        tie_tolerance <- sqrt(.Machine$double.eps) * pmax(
          1, abs(reference$correlation[different_opponent]),
          abs(candidate$correlation[different_opponent])
        )
        tie_equivalent <-
          is.finite(reference$correlation[different_opponent]) &
          is.finite(candidate$correlation[different_opponent]) &
          abs(reference$correlation[different_opponent] -
                candidate$correlation[different_opponent]) <= tie_tolerance
        if (!all(tie_equivalent)) {
          stop(label, " changed a non-tied matched opponent", call. = FALSE)
        }
      }
    }
    invisible(tied_opponent_differences)
  }
  compare_result <- function(reference, candidate, label) {
    stopifnot(identical(reference$removed_rows, candidate$removed_rows))
    tied_opponent_differences <- compare_table(
      reference$removals, candidate$removals, paste(label, "removals"),
      allow_tied_opponent = TRUE
    )
    compare_table(reference$conflicts, candidate$conflicts,
                  paste(label, "conflicts"))
    invisible(tied_opponent_differences)
  }

  view <- make_view()
  reference <- run_frozen()
  candidate <- run_cached(view)
  tied_opponent_differences <- compare_result(
    reference, candidate, "initial"
  )
  stopifnot(
    length(candidate$removed_rows) == nrow(groups) + nrow(internal_groups),
    all(candidate$removals[phase == "independent", evidence_libraries] == 2L),
    all(candidate$removals[phase == "internal", active_degree] == 3L)
  )

  # Reuse the same immutable view after a 10% removal and 2% relabel. Labels
  # and active membership remain masks and therefore cannot invalidate scores.
  removable <- seq.int(max(141L, n_spectra %/% 2L), n_spectra)
  drop <- head(removable, floor(0.1 * n_spectra))
  keep <- setdiff(seq_len(n_spectra), drop)
  relabeled <- classes[keep]
  relabel <- tail(seq_along(keep), max(1L, floor(0.02 * length(keep))))
  relabeled[relabel] <- paste0(relabeled[relabel], "-reviewed")
  masked_reference <- run_frozen(keep, relabeled)
  masked_candidate <- run_cached(view, keep, relabeled)
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    masked_reference, masked_candidate, "masked/relabelled"
  )
  stopifnot(OpenSpecy:::.lib_correlation_view_stats(view)$dense_builds > 0L)

  # Production compacts the immutable full-spectrum view before every
  # threshold-only conflict pass. Recreate a later closure against surviving
  # IDs after relabeling one member of a formerly same-class correlated pair.
  # The compact cache retains only strict >0.9 edges, so it must reproduce the
  # raw closure exactly without resurrecting removed rows or stale labels.
  closure_keep <- setdiff(seq_len(n_spectra), candidate$removed_rows)
  closure_pair <- groups[1L, 2:3]
  stopifnot(
    length(candidate$removed_rows) > 0L,
    all(closure_pair %in% closure_keep),
    identical(classes[closure_pair[[1L]]], classes[closure_pair[[2L]]])
  )
  closure_classes <- classes[closure_keep]
  closure_pair_position <- match(closure_pair, closure_keep)
  closure_classes[closure_pair_position[[2L]]] <-
    paste0(closure_classes[closure_pair_position[[2L]]], "-closure-review")
  closure_view <- make_view()
  dense_before_compaction <- OpenSpecy:::.lib_correlation_view_stats(
    closure_view
  )
  compaction_started <- proc.time()[["elapsed"]]
  closure_view <- OpenSpecy:::.lib_compact_threshold_correlation_view(
    closure_view, threshold = 0.9, progress = FALSE, block_size = 128L
  )
  compaction_seconds <- proc.time()[["elapsed"]] - compaction_started
  closure_started <- proc.time()[["elapsed"]]
  closure_reference <- run_frozen(closure_keep, closure_classes)
  closure_candidate <- run_cached(
    closure_view, closure_keep, closure_classes
  )
  closure_query_seconds <- proc.time()[["elapsed"]] - closure_started
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    closure_reference, closure_candidate, "compacted final closure"
  )
  stopifnot(any(
    ids[closure_pair] %in% closure_candidate$removals$spectrum_id
  ))
  compact_stats <- OpenSpecy:::.lib_correlation_view_stats(closure_view)
  compacted_directed_edges <- sum(
    closure_view$compaction$directed_edges
  )
  stopifnot(
    compact_stats$minimum_retained_correlation[[1L]] == 0.9,
    compact_stats$compacted_queries[[1L]] > 0L,
    compact_stats$compacted_bytes[[1L]] <
      dense_before_compaction$dense_bytes[[1L]]
  )
  below_cache_error <- tryCatch({
    run_cached(
      closure_view, closure_keep, closure_classes,
      threshold = 0.9 - 1e-12
    )
    NULL
  }, error = function(error) error)
  stopifnot(
    inherits(below_cache_error, "error"),
    grepl(
      "threshold exceeds the requested threshold",
      conditionMessage(below_cache_error), fixed = TRUE
    )
  )

  # Strict threshold parity, including exact and neighboring scores.
  threshold_values <- c(0.9, 0.9 + 1e-12, 0.9 - 1e-12)
  threshold_normalized <- rbind(
    anchor = c(1, 0),
    cbind(threshold_values, sqrt(1 - threshold_values^2))
  )
  threshold_ids <- c("anchor", "exact-0.9", "above-0.9", "below-0.9")
  threshold_pools <- rep("ftir", nrow(threshold_normalized))
  threshold_classes <- paste0("class-", seq_len(nrow(threshold_normalized)))
  threshold_libraries <- paste0("library-", seq_len(nrow(threshold_normalized)))
  threshold_view <- OpenSpecy:::.lib_new_correlation_view(
    threshold_normalized, threshold_ids, threshold_pools, 1:2,
    state = "threshold"
  )
  threshold_reference <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    threshold_classes, threshold_pools, threshold_normalized, threshold_ids,
    threshold_libraries, threshold = 0.9
  )
  threshold_candidate <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    threshold_classes, threshold_pools, threshold_view, threshold_ids,
    threshold_libraries, threshold = 0.9
  )
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    threshold_reference, threshold_candidate, "threshold"
  )
  threshold_compact <- OpenSpecy:::.lib_compact_threshold_correlation_view(
    threshold_view, threshold = 0.9
  )
  threshold_streamed_compact <- OpenSpecy:::.lib_new_correlation_view(
    threshold_normalized, threshold_ids, threshold_pools, 1:2,
    state = "threshold-streamed", compact_threshold = 0.9
  )
  threshold_compact_candidate <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    threshold_classes, threshold_pools, threshold_compact, threshold_ids,
    threshold_libraries, threshold = 0.9
  )
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    threshold_reference, threshold_compact_candidate,
    "compacted strict threshold"
  )
  threshold_streamed_candidate <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    threshold_classes, threshold_pools, threshold_streamed_compact,
    threshold_ids, threshold_libraries, threshold = 0.9
  )
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    threshold_reference, threshold_streamed_candidate,
    "per-pool compacted strict threshold"
  )
  strict_scores <- OpenSpecy:::.lib_correlation_values(
    threshold_compact, threshold_ids, 1L, 2:4
  )
  stopifnot(
    !is.finite(strict_scores[[1L]]),
    isTRUE(all.equal(
      strict_scores[[2L]], 0.9 + 1e-12, tolerance = 1e-14
    )),
    !is.finite(strict_scores[[3L]])
  )
  strict_threshold_error <- tryCatch({
    OpenSpecy:::.lib_prune_cross_class_conflicts(
      threshold_classes, threshold_pools, threshold_compact, threshold_ids,
      threshold_libraries, threshold = 0.9 - 1e-12
    )
    NULL
  }, error = function(error) error)
  stopifnot(
    inherits(strict_threshold_error, "error"),
    grepl(
      "threshold exceeds the requested threshold",
      conditionMessage(strict_threshold_error), fixed = TRUE
    )
  )

  # A prior streamed-compaction implementation retained the first dense
  # matrix in a loop-local `result` binding while allocating the next pool.
  # Exercise two unequal pools and observe the caller at each compaction so
  # this benchmark fails if that alias (and its production-scale peak-memory
  # regression) returns.
  lifecycle_normalized <- rbind(
    ftir_anchor = c(1, 0, 0),
    ftir_above = c(0.91, sqrt(1 - 0.91^2), 0),
    ftir_exact = c(0.9, sqrt(1 - 0.9^2), 0),
    ftir_distinct = c(0, 0, 1),
    raman_anchor = c(0, 1, 0),
    raman_above = c(0, 0.95, sqrt(1 - 0.95^2)),
    raman_distinct = c(1, 0, 0)
  )
  lifecycle_ids <- rownames(lifecycle_normalized)
  lifecycle_pools <- c(rep("ftir", 4L), rep("raman", 3L))
  lifecycle_observed <- list()
  lifecycle_collections <- logical()
  compact_pool <- OpenSpecy:::.lib_compact_threshold_correlation_pool
  compact_gc <- OpenSpecy:::.lib_compaction_gc
  lifecycle_view <- testthat::with_mocked_bindings(
    OpenSpecy:::.lib_new_correlation_view(
      lifecycle_normalized, lifecycle_ids, lifecycle_pools, 1:3,
      state = "benchmark/unequal-pool-lifecycle", block_size = 2L,
      compact_threshold = 0.9
    ),
    .lib_compact_threshold_correlation_pool = function(
        dense, threshold, block_size = 128L) {
      caller <- parent.frame()
      has_result <- exists("result", envir = caller, inherits = FALSE)
      lifecycle_observed[[length(lifecycle_observed) + 1L]] <<- list(
        spectra = nrow(dense),
        retained_result_binding = has_result,
        result_aliases_dense = has_result &&
          identical(get("result", envir = caller, inherits = FALSE), dense)
      )
      compact_pool(dense, threshold = threshold, block_size = block_size)
    },
    .lib_compaction_gc = function(full = FALSE) {
      lifecycle_collections <<- c(lifecycle_collections, isTRUE(full))
      compact_gc(full = full)
    },
    .package = "OpenSpecy"
  )
  lifecycle_parity <- vapply(unique(lifecycle_pools), function(pool) {
    rows <- which(lifecycle_pools == pool)
    expected <- tcrossprod(lifecycle_normalized[rows, , drop = FALSE])
    keep <- is.finite(expected) & expected > 0.9
    diag(keep) <- FALSE
    expected[!keep] <- -Inf
    dimnames(expected) <- NULL
    actual <- OpenSpecy:::.lib_correlation_values(
      lifecycle_view, lifecycle_ids[rows], seq_along(rows), seq_along(rows)
    )
    isTRUE(all.equal(actual, expected, tolerance = 0))
  }, logical(1L))
  lifecycle_result <- data.frame(
    pool_build_order = paste(vapply(
      lifecycle_observed, `[[`, integer(1L), "spectra"
    ), collapse = " -> "),
    dense_builds = lifecycle_view$metrics$dense_builds,
    retained_result_binding = any(vapply(
      lifecycle_observed, `[[`, logical(1L), "retained_result_binding"
    )),
    result_aliases_dense = any(vapply(
      lifecycle_observed, `[[`, logical(1L), "result_aliases_dense"
    )),
    gc_calls = length(lifecycle_collections),
    full_gc_calls = sum(lifecycle_collections),
    normalized_released = is.null(lifecycle_view$normalized),
    exact_compact_threshold_parity = all(lifecycle_parity)
  )
  stopifnot(
    identical(vapply(
      lifecycle_observed, `[[`, integer(1L), "spectra"
    ), c(4L, 3L)),
    lifecycle_view$metrics$dense_builds == 2L,
    !lifecycle_result$retained_result_binding,
    !lifecycle_result$result_aliases_dense,
    lifecycle_result$gc_calls == 5L,
    lifecycle_result$full_gc_calls == 0L,
    identical(lifecycle_collections, rep(FALSE, 5L)),
    !any(lifecycle_view$preflight$full_boundary_gc),
    lifecycle_result$normalized_released,
    lifecycle_result$exact_compact_threshold_parity
  )

  # Guarded bounded fallback remains scientifically identical.
  options(
    OpenSpecy.correlation_cache = "auto",
    OpenSpecy.correlation_cache_max_bytes = 1
  )
  fallback_view <- make_view()
  fallback <- run_cached(fallback_view)
  tied_opponent_differences <- tied_opponent_differences + compare_result(
    reference, fallback, "fallback"
  )
  stopifnot(
    OpenSpecy:::.lib_correlation_view_stats(fallback_view)$dense_builds == 0L,
    OpenSpecy:::.lib_correlation_view_stats(fallback_view)$bounded_queries > 0L
  )
  options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )

  # The cached parent-to-medoid summary must equal direct parent×medoid scores.
  active <- setdiff(seq_len(n_spectra), candidate$removed_rows)
  active <- active[pools[active] == "raman"]
  object <- as_OpenSpecy(
    axis, t(normalized[active, , drop = FALSE]),
    metadata = data.table::data.table(
      sample_name = ids[active], material_class = classes[active]
    )
  )
  summary <- OpenSpecy:::.lib_correlation_validation_summary(
    view, object, classes[active], threshold = 0.9, block_size = 128L
  )
  medoid <- !duplicated(classes[active])
  direct <- frozen_bounded_values(active, active[medoid])
  different <- outer(classes[active], classes[active][medoid], FUN = "!=")
  direct_scores <- direct[different & is.finite(direct)]
  stopifnot(
    isTRUE(all.equal(
      max(summary$max_wrong_class_correlation[medoid]),
      max(direct_scores), tolerance = 1e-12
    )),
    sum(summary$wrong_class_pairs_above_threshold[medoid]) ==
      sum(direct_scores > 0.9)
  )

  # Medoid reduction uses the same immutable numeric view. Exercise stable ID
  # lookup with a deliberately reordered cache and non-finite source values.
  # A >3000 case enters the sampled-PAM path when enabled; a 4,000-spectrum
  # benchmark enables a practical 3,001 x 80 case by default. Override its
  # dimensions with the OPENSPECY_CROSS_CLASS_BENCHMARK_PAM_LARGE_* variables,
  # or set PAM_LARGE_N=0 to skip it on a constrained machine.
  pam_parity_case <- function(case, spectra, stable_ids, k,
                              extra_args = list()) {
    count <- ncol(spectra)
    pam_axis <- seq_len(nrow(spectra))
    object <- as_OpenSpecy(
      pam_axis, spectra,
      metadata = data.table::data.table(
        sample_name = stable_ids, material_class = rep("pam-case", count)
      )
    )
    pam_normalized <- OpenSpecy:::.lib_prune_normalize(
      object$spectra, object$wavenumber, exclude = NULL
    )
    # The cache order intentionally differs from the OpenSpecy column order.
    view_order <- order(stable_ids, decreasing = TRUE)
    view_started <- proc.time()[["elapsed"]]
    pam_view <- OpenSpecy:::.lib_new_correlation_view(
      pam_normalized[view_order, , drop = FALSE],
      ids = stable_ids[view_order], pools = rep("ftir", count),
      wavenumber = pam_axis, state = paste0("pam/", case),
      progress = FALSE, block_size = 128L
    )
    view_seconds <- proc.time()[["elapsed"]] - view_started
    common <- list(
      x = object, group_cols = "material_class", id_col = "sample_name",
      k = k, min_n = k, return = "ids", progress = FALSE
    )
    uncached_started <- proc.time()[["elapsed"]]
    uncached_ids <- do.call(
      OpenSpecy:::.lib_reduce_lib,
      c(common, list(correlation_view = NULL), extra_args)
    )
    uncached_seconds <- proc.time()[["elapsed"]] - uncached_started
    cached_started <- proc.time()[["elapsed"]]
    cached_ids <- do.call(
      OpenSpecy:::.lib_reduce_lib,
      c(common, list(correlation_view = pam_view), extra_args)
    )
    cached_seconds <- proc.time()[["elapsed"]] - cached_started
    if (!identical(uncached_ids, cached_ids)) {
      stop(
        "Cached PAM medoid IDs differ for ", case, ": uncached-only=",
        paste(setdiff(uncached_ids, cached_ids), collapse = ", "),
        "; cached-only=",
        paste(setdiff(cached_ids, uncached_ids), collapse = ", "),
        call. = FALSE
      )
    }
    pam_stats <- OpenSpecy:::.lib_correlation_view_stats(pam_view)
    stopifnot(
      length(cached_ids) == k,
      !anyDuplicated(cached_ids),
      all(cached_ids %in% stable_ids),
      pam_stats$dense_builds[[1L]] == 1L,
      pam_stats$dense_queries[[1L]] > 0L
    )
    data.table::data.table(
      case = case,
      path = if (count > 3000L) "sampled_pam" else "exact_pam",
      spectra = count, wavenumbers = nrow(spectra), k = k,
      missing_values = sum(!is.finite(spectra)),
      cache_view_build_seconds = view_seconds,
      uncached_seconds = uncached_seconds,
      cached_seconds = cached_seconds,
      dense_matrix_mib = pam_stats$dense_bytes[[1L]] / 1024^2,
      dense_queries = pam_stats$dense_queries[[1L]],
      medoid_ids_sha256 = digest::digest(
        as.character(cached_ids), algo = "sha256"
      ),
      exact_ids = identical(uncached_ids, cached_ids)
    )
  }

  set.seed(20261012)
  pam_small_p <- min(120L, n_wavenumbers)
  pam_small <- matrix(
    rnorm(pam_small_p * pam_small_n),
    nrow = pam_small_p, ncol = pam_small_n
  )
  pam_small[sample.int(
    length(pam_small), max(1L, floor(0.04 * length(pam_small)))
  )] <- NA_real_
  pam_small_ids <- sprintf(
    "pam-stable-%06d", sample.int(900000L, pam_small_n)
  )
  pam_results <- list(pam_parity_case(
    "missing_stable_id_subset", pam_small, pam_small_ids,
    k = min(12L, pam_small_n - 1L)
  ))
  rm(pam_small)
  gc(verbose = FALSE, full = TRUE)

  if (pam_large_n > 3000L) {
    set.seed(20261013)
    pam_large <- matrix(
      rnorm(pam_large_p * pam_large_n),
      nrow = pam_large_p, ncol = pam_large_n
    )
    pam_large_ids <- sprintf(
      "pam-large-%06d", sample.int(900000L, pam_large_n)
    )
    pam_results[[length(pam_results) + 1L]] <- pam_parity_case(
      "large_sampled_stable_id_subset", pam_large, pam_large_ids,
      k = 12L,
      extra_args = list(
        samples = pam_large_samples, sample_size = pam_large_sample_size
      )
    )
    rm(pam_large)
    gc(verbose = FALSE, full = TRUE)
  }
  pam_results <- data.table::rbindlist(pam_results, fill = TRUE)

  # Fingerprints cover axis and numeric values, while the same view handles
  # label masks without rebuilding.
  changed_axis <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, axis + 1, state = "benchmark"
  )
  changed_axis_key <- changed_axis$key
  rm(changed_axis)
  gc(verbose = FALSE, full = TRUE)
  changed_values <- normalized
  changed_values[n_spectra, n_wavenumbers] <-
    changed_values[n_spectra, n_wavenumbers] + 1e-8
  changed_numeric <- OpenSpecy:::.lib_new_correlation_view(
    changed_values, ids, pools, axis, state = "benchmark"
  )
  stopifnot(
    !identical(view$key, changed_axis_key),
    !identical(view$key, changed_numeric$key)
  )
  rm(
    changed_numeric, fallback_view, threshold_view, threshold_compact,
    threshold_streamed_compact, closure_view
  )
  gc(verbose = FALSE, full = TRUE)

  # Warmed lifecycle timing: one matrix build plus initial and masked queries
  # versus two complete pre-cache blockwise runs.
  invisible(run_frozen())
  warm_view <- make_view()
  invisible(run_cached(warm_view))
  rm(warm_view)
  frozen_times <- replicate(repetitions, system.time({
    invisible(run_frozen())
    invisible(run_frozen(keep, relabeled))
  })[["elapsed"]])
  cached_times <- replicate(repetitions, system.time({
    timed_view <- make_view()
    invisible(run_cached(timed_view))
    invisible(run_cached(timed_view, keep, relabeled))
    rm(timed_view)
  })[["elapsed"]])
  stats <- OpenSpecy:::.lib_correlation_view_stats(view)
  result <- data.frame(
    implementation = c("frozen_bounded", "dense_view_lifecycle"),
    spectra = n_spectra, wavenumbers = n_wavenumbers,
    libraries = data.table::uniqueN(libraries),
    pools = data.table::uniqueN(pools), block_size = block_size,
    dense_matrix_mib = c(0, stats$dense_bytes / 1024^2),
    median_seconds = c(stats::median(frozen_times),
                       stats::median(cached_times)),
    runs_seconds = c(
      paste(sprintf("%.3f", frozen_times), collapse = ", "),
      paste(sprintf("%.3f", cached_times), collapse = ", ")
    )
  )
  compaction_result <- data.frame(
    threshold = 0.9,
    retained_spectra = length(closure_keep),
    relabelled_pair = paste(ids[closure_pair], collapse = " -> "),
    closure_removed = length(closure_candidate$removed_rows),
    directed_edges = compacted_directed_edges,
    dense_before_mib = dense_before_compaction$dense_bytes[[1L]] / 1024^2,
    compacted_mib = compact_stats$compacted_bytes[[1L]] / 1024^2,
    compact_to_dense_ratio = compact_stats$compacted_bytes[[1L]] /
      dense_before_compaction$dense_bytes[[1L]],
    compacted_queries = compact_stats$compacted_queries[[1L]],
    compaction_seconds = compaction_seconds,
    closure_query_seconds = closure_query_seconds,
    below_threshold_rejected = inherits(below_cache_error, "error") &&
      inherits(strict_threshold_error, "error")
  )
  print(result, row.names = FALSE)
  print(compaction_result, row.names = FALSE)
  print(lifecycle_result, row.names = FALSE)
  print(pam_results, row.names = FALSE)
  message(
    "Parity rule: removal decisions, reasons, order, survivors, matched ",
    "class/library, and scores are exact/tolerance-equivalent. A stable ",
    "matched_id may differ only when full-pool and subgroup GEMM select ",
    "tolerance-equivalent tied opponents. Observed allowed differences ",
    "across all parity comparisons: ",
    tied_opponent_differences, "."
  )
  if (result$median_seconds[[2L]] > result$median_seconds[[1L]] * 1.1) {
    stop("Dense correlation lifecycle regressed by more than 10%",
         call. = FALSE)
  }
}

main()
