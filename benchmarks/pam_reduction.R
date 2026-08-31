# Compare the restored compiled cluster::pam(pamonce = 6) path with the
# previous pure-R PAM implementation. This benchmark is intentionally outside
# tests because representative PAM groups are too expensive for routine checks.

library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE)
} else {
  library(OpenSpecy)
}

median_repeated_time <- function(expr, batches = 3L) {
  stats::median(replicate(
    batches,
    as.numeric(system.time(expr())[["elapsed"]])
  ))
}

make_reduction_lib <- function(n = 500L) {
  wavenumber <- seq(100, 1800, by = 25)
  base <- sin(wavenumber / 95) + cos(wavenumber / 135)
  spectra <- vapply(seq_len(n), function(i) {
    base + stats::rnorm(length(wavenumber), sd = 0.025) + i / 10000
  }, numeric(length(wavenumber)))
  colnames(spectra) <- paste0("s", seq_len(n))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table(
      sample_name = colnames(spectra), material_class = "class_a"
    )
  )
}

# Previous implementation retained for same-output/performance evidence.
previous_assign_info <- function(distance, medoids) {
  d_med <- distance[, medoids, drop = FALSE]
  cluster <- max.col(-d_med, ties.method = "last")
  nearest <- d_med[cbind(seq_len(nrow(distance)), cluster)]
  second <- rep(Inf, nrow(distance))
  if (length(medoids) > 1L) {
    second_dist <- d_med
    second_dist[cbind(seq_len(nrow(distance)), cluster)] <- Inf
    second <- matrixStats::rowMins(second_dist)
  }
  list(cluster = cluster, nearest = nearest, second = second)
}

previous_build_medoids <- function(distance, k) {
  n <- nrow(distance)
  medoids <- integer()
  nearest <- rep(Inf, n)
  for (step in seq_len(k)) {
    candidates <- setdiff(seq_len(n), medoids)
    totals <- vapply(candidates, function(candidate) {
      sum(pmin(nearest, distance[, candidate]))
    }, numeric(1))
    best <- min(totals)
    candidate <- candidates[max(which(totals <= best))]
    medoids <- c(medoids, candidate)
    nearest <- pmin(nearest, distance[, candidate])
  }
  medoids
}

previous_eager_swap <- function(distance, medoids) {
  n <- nrow(distance)
  tol <- sqrt(.Machine$double.eps)
  info <- previous_assign_info(distance, medoids)
  repeat {
    changed <- FALSE
    for (candidate in seq_len(n)) {
      if (candidate %in% medoids) next
      candidate_distance <- distance[, candidate]
      new_distance <- pmin(candidate_distance, info$nearest)
      base_delta <- sum(new_distance - info$nearest)
      delta <- rep(base_delta, length(medoids))
      for (medoid_i in seq_along(medoids)) {
        removed <- info$cluster == medoid_i
        if (!any(removed)) next
        delta[medoid_i] <- base_delta +
          sum(pmin(candidate_distance[removed], info$second[removed]) -
                new_distance[removed])
      }
      best_medoid <- which.min(delta)
      if (delta[best_medoid] < -tol) {
        medoids[best_medoid] <- candidate
        info <- previous_assign_info(distance, medoids)
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  sort.int(medoids)
}

previous_pam_group_ids <- function(x, id_col, k) {
  ids <- getFromNamespace(".lib_ids", "OpenSpecy")(x, id_col)
  cors <- cor_spec(x, x, compute = "optimized")
  cors[is.na(cors)] <- 0
  cors <- pmax(pmin(cors, 1), -1)
  diag(cors) <- 1
  distance <- as.matrix(stats::as.dist(1 - cors))
  medoids <- previous_build_medoids(distance, k)
  ids[previous_eager_swap(distance, medoids)]
}

previous_reduce_lib <- function(x, k = 20L) {
  reduction_obj <- x
  reduction_obj$spectra <- getFromNamespace(
    ".matrix_mean_replace", "OpenSpecy"
  )(make_rel(x$spectra, na.rm = TRUE))
  previous_pam_group_ids(reduction_obj, "sample_name", k)
}

set.seed(710)
reduction_lib <- make_reduction_lib()
previous_ids <- previous_reduce_lib(reduction_lib)
current_ids <- reduce_lib(
  reduction_lib, k = 20L, min_n = 20L, return = "ids"
)
stopifnot(
  length(previous_ids) == 20L, length(current_ids) == 20L,
  !anyDuplicated(previous_ids), !anyDuplicated(current_ids),
  setequal(previous_ids, current_ids)
)

relative_spectra <- getFromNamespace(
  ".matrix_mean_replace", "OpenSpecy"
)(make_rel(reduction_lib$spectra, na.rm = TRUE))
cors <- stats::cor(relative_spectra)
cors[is.na(cors)] <- 0
cors <- pmax(pmin(cors, 1), -1)
diag(cors) <- 1
distance <- 1 - cors
objective <- function(ids) {
  medoid_index <- match(ids, reduction_lib$metadata$sample_name)
  sum(matrixStats::rowMins(distance[, medoid_index, drop = FALSE]))
}
previous_objective <- objective(previous_ids)
current_objective <- objective(current_ids)
message(
  "Medoid overlap: ", length(intersect(previous_ids, current_ids)), "/20; ",
  "previous objective: ", signif(previous_objective, 6), "; ",
  "current objective: ", signif(current_objective, 6)
)
if (current_objective > previous_objective + 1e-10) {
  stop("Restored cluster PAM produced a worse medoid objective", call. = FALSE)
}

# Warm both paths before repeated timings.
invisible(previous_reduce_lib(reduction_lib))
invisible(reduce_lib(reduction_lib, k = 20L, min_n = 20L, return = "ids"))
previous_time <- median_repeated_time(function() {
  previous_reduce_lib(reduction_lib)
})
current_time <- median_repeated_time(function() {
  reduce_lib(reduction_lib, k = 20L, min_n = 20L, return = "ids")
})

message("Previous pure-R PAM reduction median: ", previous_time, "s")
message("Current cluster PAM reduction median: ", current_time, "s")
if (current_time > previous_time * 1.1) {
  stop("Current PAM reduction is more than 10% slower than the previous path",
       call. = FALSE)
}
