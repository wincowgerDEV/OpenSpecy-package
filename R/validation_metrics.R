#' @rdname validation_metrics
#' @title Particle analysis validation metrics
#'
#' @description
#' Helpers for assessing particle crowding, spike recovery, minimum detectable
#' amount (MDA), and batch detection limit (BDL) from particle-count tables.
#'
#' @param x a data frame or data table.
#' @param sample_col column identifying samples.
#' @param area_col column containing particle area.
#' @param size_col optional column containing particle size. If `NULL`, size is
#' inferred as `sqrt(area_col)`.
#' @param material_col optional material column to include in crowding groups.
#' @param group_cols columns used for grouped summaries.
#' @param size_threshold particles larger than this size are assessed for
#' possible crowding.
#' @param surface_area optional analyzed surface area used to calculate percent
#' area covered.
#' @param simulations number of simulated small-particle cumulative-area draws.
#' @param seed optional random seed for reproducible crowding simulations.
#' @param observed_col,expected_col columns with observed and expected spike
#' counts.
#' @param pre_recovered_col optional column with particles recovered before the
#' automated analysis.
#' @param count_col column with blank counts.
#' @param offset,md_multiplier,bdl_multiplier numeric constants used in MDA and
#' BDL formulas.
#' @param spike_replicates number of spike replicates in the MDA formula.
#' @param round one of `"integer"`, `"ceiling"`, or `"none"` for detection-limit
#' rounding. `"integer"` matches the historic workflow's `as.integer()`.
#' @param na.rm logical; remove missing values from summaries?
#'
#' @return A `data.table` containing the requested summary.
#'
#' @examples
#' blanks <- data.frame(sample_id = c("b1", "b2", "b3", "b4"),
#'                      area_bins = "(0,212]",
#'                      count = c(0, 0, 0, 11))
#' minimum_detectable_amount(blanks, group_cols = "area_bins")
#' batch_detection_limit(11)
#'
#' @importFrom data.table as.data.table copy data.table rbindlist
#' @export
crowd_lookup <- function(x, sample_col = "sample_id", area_col = "area_um2",
                         size_col = "min_length_um", material_col = NULL,
                         group_cols = sample_col, size_threshold = 500,
                         surface_area = NULL, simulations = 10000,
                         seed = NULL, na.rm = TRUE) {
  dt <- data.table::copy(data.table::as.data.table(x))
  needed <- c(group_cols, area_col, material_col)
  .require_columns(dt, needed)
  if (!is.null(size_col) && size_col %in% names(dt)) {
    size <- dt[[size_col]]
  } else {
    size <- sqrt(dt[[area_col]])
    size_col <- ".inferred_size"
    dt[[size_col]] <- size
  }
  if (!is.null(material_col) && !material_col %in% group_cols)
    group_cols <- c(group_cols, material_col)

  if (!is.null(seed)) set.seed(seed)

  groups <- split(dt, dt[, group_cols, with = FALSE], drop = TRUE)
  out <- lapply(groups, function(g) {
    area <- as.numeric(g[[area_col]])
    size <- as.numeric(g[[size_col]])
    small <- area[size <= size_threshold & area > 0]
    log_mean <- mean(log(small), na.rm = na.rm)
    log_sd <- stats::sd(log(small), na.rm = na.rm)

    potential <- rep(1, length(area))
    large <- size > size_threshold
    if (any(large) && is.finite(log_mean) && is.finite(log_sd) && log_sd > 0) {
      potential[large] <- vapply(area[large], function(a) {
        sum(cumsum(exp(stats::rnorm(simulations, log_mean, log_sd))) <= a,
            na.rm = TRUE)
      }, FUN.VALUE = numeric(1))
    } else if (any(large)) {
      potential[large] <- NA_real_
    }

    res <- g[1, group_cols, with = FALSE]
    res$potential_particle_ratio <- sum(potential, na.rm = na.rm) /
      length(potential)
    res$area_sum <- sum(area, na.rm = na.rm)
    res$count_sum <- length(area)
    res$area_bigger_ratio <- sum(area[size > size_threshold], na.rm = na.rm) /
      res$area_sum
    if (!is.null(surface_area))
      res$percent_area_covered <- res$area_sum / surface_area * 100
    res
  })
  data.table::rbindlist(out, fill = TRUE)
}

#' @rdname validation_metrics
#' @export
recovery_rate <- function(x, observed_col = "count",
                          expected_col = "total_spiked",
                          group_cols = NULL,
                          pre_recovered_col = NULL,
                          na.rm = TRUE) {
  dt <- data.table::copy(data.table::as.data.table(x))
  .require_columns(dt, c(observed_col, expected_col, group_cols,
                         pre_recovered_col))

  pre <- if (is.null(pre_recovered_col)) {
    0
  } else {
    as.numeric(dt[[pre_recovered_col]])
  }
  dt$observed_recovered <- as.numeric(dt[[observed_col]]) + pre
  dt$expected_spiked <- as.numeric(dt[[expected_col]])
  dt$recovery_rate <- dt$observed_recovered / dt$expected_spiked

  if (is.null(group_cols)) return(dt)

  groups <- split(dt, dt[, group_cols, with = FALSE], drop = TRUE)
  data.table::rbindlist(lapply(groups, function(g) {
    res <- g[1, group_cols, with = FALSE]
    res$n <- nrow(g)
    res$observed_recovered <- sum(g$observed_recovered, na.rm = na.rm)
    res$expected_spiked <- sum(g$expected_spiked, na.rm = na.rm)
    res$mean_recovery <- mean(g$recovery_rate, na.rm = na.rm)
    res$rsd_recovery <- .relative_sd(g$recovery_rate, na.rm = na.rm)
    res
  }), fill = TRUE)
}

#' @rdname validation_metrics
#' @export
minimum_detectable_amount <- function(x, count_col = "count",
                                      group_cols = NULL, offset = 3,
                                      md_multiplier = 3.29,
                                      bdl_multiplier = 4.65,
                                      spike_replicates = 4,
                                      round = c("integer", "ceiling", "none"),
                                      na.rm = TRUE) {
  round <- match.arg(round)
  dt <- data.table::copy(data.table::as.data.table(x))
  .require_columns(dt, c(count_col, group_cols))

  groups <- if (is.null(group_cols)) {
    list(dt)
  } else {
    split(dt, dt[, group_cols, with = FALSE], drop = TRUE)
  }

  data.table::rbindlist(lapply(groups, function(g) {
    counts <- as.numeric(g[[count_col]])
    counts <- counts[!is.na(counts) | !isTRUE(na.rm)]
    res <- if (is.null(group_cols)) data.table::data.table() else
      g[1, group_cols, with = FALSE]
    res$n_blanks <- length(counts)
    if (length(counts) == 1L) {
      value <- batch_detection_limit(
        counts,
        offset = offset,
        multiplier = bdl_multiplier,
        round = round
      )
      res$method <- "BDL"
      res$MDA <- value
    } else {
      value <- mean(counts, na.rm = na.rm) + offset +
        md_multiplier * stats::sd(counts, na.rm = na.rm) *
        sqrt(1 + 1 / spike_replicates)
      res$method <- "MDA"
      res$MDA <- .round_detection_limit(value, round)
    }
    res
  }), fill = TRUE)
}

#' @rdname validation_metrics
#' @export
batch_detection_limit <- function(x, count_col = NULL, offset = 3,
                                  multiplier = 4.65,
                                  round = c("integer", "ceiling", "none"),
                                  ...) {
  round <- match.arg(round)
  if (inherits(x, c("data.frame", "data.table"))) {
    dt <- data.table::copy(data.table::as.data.table(x))
    .require_columns(dt, count_col)
    dt$BDL <- .round_detection_limit(
      as.numeric(dt[[count_col]]) + offset +
        multiplier * sqrt(as.numeric(dt[[count_col]])),
      round
    )
    return(dt)
  }
  .round_detection_limit(as.numeric(x) + offset + multiplier * sqrt(x), round)
}

.require_columns <- function(x, cols) {
  cols <- cols[!is.null(cols)]
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

.relative_sd <- function(x, na.rm = TRUE) {
  mean_x <- mean(x, na.rm = na.rm)
  if (!is.finite(mean_x) || mean_x == 0) return(NA_real_)
  stats::sd(x, na.rm = na.rm) / mean_x * 100
}

.round_detection_limit <- function(x, round) {
  if (identical(round, "integer")) return(as.integer(x))
  if (identical(round, "ceiling")) return(ceiling(x))
  x
}
