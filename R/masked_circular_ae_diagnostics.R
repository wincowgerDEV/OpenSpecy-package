#' Diagnostics and plotting for masked circular autoencoders
#'
#' @description
#' Post-training diagnostics for a fitted masked circular autoencoder. Metadata
#' may be used here for display or summary after training, but never updates the
#' model or loss history.
#'
#' @param model A `MaskedCircularAEModel`.
#' @param x Spectra compatible with `model`.
#' @param wavenumber Optional wavenumber grid for matrix-like `x`.
#' @param encoded Optional encoded coordinates from
#'   `encode_masked_circular_ae()`.
#' @param target_distance Target spectral distance for diagnostic comparison.
#' @param n_pairs Maximum number of sampled spectrum pairs.
#' @param k Number of neighbors for neighbor-preservation diagnostics.
#' @param metadata Optional metadata used only after training.
#' @param seed Random seed for sampled pair diagnostics.
#' @param color_by Optional metadata or encoded column used for point color.
#' @param label_by Optional metadata or encoded column used for point labels.
#'
#' @return
#' `diagnose_masked_circular_ae()` returns a list of diagnostic tables and
#' summaries. `plot_circular_embedding()` draws a base R circular plot and
#' invisibly returns the plotting data.
#'
#' @examples
#' \dontrun{
#' data("test_lib")
#' model <- fit_masked_circular_ae(test_lib, decoder_degree = 2)
#' encoded <- encode_masked_circular_ae(model, test_lib)
#' diagnose_masked_circular_ae(model, test_lib, encoded = encoded)
#' plot_circular_embedding(encoded)
#' }
#'
#' @author
#' Win Cowger
#'
#' @export
diagnose_masked_circular_ae <- function(model,
                                        x,
                                        wavenumber = NULL,
                                        encoded = NULL,
                                        target_distance = model$target_distance,
                                        n_pairs = 10000,
                                        k = 5,
                                        metadata = NULL,
                                        seed = 1) {
  .check_masked_circular_model(model)
  prep <- .prepare_masked_circular_input(x, wavenumber = wavenumber)
  .validate_masked_circular_wavenumber(prep$wavenumber, model$wavenumber)

  if (is.null(encoded))
    encoded <- encode_masked_circular_ae(model, x, wavenumber = wavenumber)

  list(
    reconstruction = .masked_circular_reconstruction_diagnostics(
      model, prep, encoded
    ),
    distance_preservation = .masked_circular_distance_preservation(
      prep = prep,
      encoded = encoded,
      target_distance = target_distance,
      n_pairs = n_pairs,
      seed = seed,
      min_overlap_points = model$hyperparameters$min_overlap_points,
      min_overlap_fraction = model$hyperparameters$min_overlap_fraction
    ),
    neighbor_preservation = .masked_circular_neighbor_preservation(
      prep = prep,
      encoded = encoded,
      target_distance = target_distance,
      k = k,
      min_overlap_points = model$hyperparameters$min_overlap_points,
      min_overlap_fraction = model$hyperparameters$min_overlap_fraction
    ),
    missingness_range = .masked_circular_missingness_range_diagnostics(encoded),
    angular_spread = .masked_circular_angular_spread(encoded),
    note = paste(
      "The circular coordinate is an empirical embedding.",
      "Use diagnostics before treating it as chemically meaningful."
    )
  )
}

#' @rdname diagnose_masked_circular_ae
#' @export
plot_circular_embedding <- function(encoded, color_by = NULL, label_by = NULL,
                                    metadata = NULL) {
  dt <- .prepare_circular_embedding_plot_data(
    encoded = encoded,
    color_by = color_by,
    label_by = label_by,
    metadata = metadata
  )

  z <- .theta_degrees_to_z(dt$theta)
  circle_theta <- seq(0, 360, length.out = 181)
  circle <- .theta_degrees_to_z(circle_theta)

  point_col <- "black"
  if (!is.null(color_by)) {
    groups <- as.factor(dt[[color_by]])
    palette <- grDevices::hcl.colors(max(1L, nlevels(groups)), "Dark 3")
    point_col <- palette[as.integer(groups)]
  } else {
    groups <- NULL
    palette <- NULL
  }

  graphics::plot(
    circle[, 1],
    circle[, 2],
    type = "l",
    asp = 1,
    col = "grey75",
    xlab = "cos(theta)",
    ylab = "sin(theta)",
    xlim = c(-1.08, 1.08),
    ylim = c(-1.08, 1.08)
  )
  graphics::points(z[, 1], z[, 2], col = point_col, pch = 19)

  if (!is.null(label_by)) {
    graphics::text(
      z[, 1],
      z[, 2],
      labels = dt[[label_by]],
      pos = 3,
      cex = 0.8
    )
  }

  if (!is.null(groups)) {
    graphics::legend(
      "topright",
      legend = levels(groups),
      col = palette,
      pch = 19,
      bty = "n"
    )
  }

  invisible(dt)
}

.masked_circular_reconstruction_diagnostics <- function(model, prep, encoded) {
  recon <- reconstruct_masked_circular_ae(model, theta = encoded$theta)
  x_hat <- t(as.matrix(recon$spectra))
  x <- prep$x
  mask <- prep$mask_observed

  rmse <- vapply(seq_len(nrow(x)), function(i) {
    idx <- mask[i, ]
    if (!any(idx)) return(NA_real_)
    sqrt(mean((x[i, idx] - x_hat[i, idx])^2))
  }, numeric(1))

  cor <- vapply(seq_len(nrow(x)), function(i) {
    idx <- mask[i, ] & is.finite(x_hat[i, ])
    if (sum(idx) < 2L || stats::sd(x[i, idx]) == 0 ||
        stats::sd(x_hat[i, idx]) == 0) {
      return(NA_real_)
    }
    stats::cor(x[i, idx], x_hat[i, idx])
  }, numeric(1))

  data.table::data.table(
    spectrum_id = prep$spectrum_id,
    masked_rmse = rmse,
    masked_correlation = cor,
    observed_points = rowSums(mask)
  )
}

.masked_circular_distance_preservation <- function(prep, encoded,
                                                   target_distance,
                                                   n_pairs,
                                                   seed,
                                                   min_overlap_points,
                                                   min_overlap_fraction) {
  n <- nrow(prep$x)
  if (n < 2L) {
    return(list(
      pairs = data.table::data.table(),
      pearson = NA_real_,
      spearman = NA_real_,
      valid_pairs = 0L
    ))
  }

  all_pairs <- t(utils::combn(seq_len(n), 2L))
  if (nrow(all_pairs) > n_pairs) {
    set.seed(seed)
    all_pairs <- all_pairs[sample.int(nrow(all_pairs), n_pairs), , drop = FALSE]
  }

  target <- masked_spectral_distance(
    prep$x,
    prep$mask_observed,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction,
    pairs = all_pairs
  )
  target[["circular_distance"]] <- circ_dist(
    encoded$theta[target[["i"]]],
    encoded$theta[target[["k"]]]
  )
  valid <- target[
    target[["valid"]] &
      is.finite(target[["distance"]]) &
      is.finite(target[["circular_distance"]])
  ]

  list(
    pairs = target,
    pearson = if (nrow(valid) > 1L) {
      stats::cor(valid$distance, valid$circular_distance, method = "pearson")
    } else {
      NA_real_
    },
    spearman = if (nrow(valid) > 1L) {
      stats::cor(valid$distance, valid$circular_distance, method = "spearman")
    } else {
      NA_real_
    },
    valid_pairs = nrow(valid)
  )
}

.masked_circular_neighbor_preservation <- function(prep, encoded,
                                                   target_distance,
                                                   k,
                                                   min_overlap_points,
                                                   min_overlap_fraction) {
  n <- nrow(prep$x)
  if (n < 2L) {
    return(data.table::data.table(
      spectrum_id = prep$spectrum_id,
      neighbor_overlap = NA_real_
    ))
  }

  k <- min(as.integer(k), n - 1L)
  target <- .masked_spectral_distance_matrix(
    prep$x,
    prep$mask_observed,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )$distance

  circle <- outer(encoded$theta, encoded$theta, Vectorize(circ_dist))
  diag(target) <- NA_real_
  diag(circle) <- NA_real_

  overlap <- vapply(seq_len(n), function(i) {
    orig <- order(target[i, ], na.last = NA)[seq_len(min(k, sum(is.finite(target[i, ]))))]
    circ <- order(circle[i, ], na.last = NA)[seq_len(k)]
    if (!length(orig) || !length(circ)) return(NA_real_)
    length(intersect(orig, circ)) / k
  }, numeric(1))

  data.table::data.table(
    spectrum_id = prep$spectrum_id,
    k = k,
    neighbor_overlap = overlap
  )
}

.masked_circular_missingness_range_diagnostics <- function(encoded) {
  cols <- c(
    "spectrum_id",
    "theta",
    "observed_points",
    "min_wavenumber",
    "max_wavenumber"
  )
  data.table::as.data.table(encoded)[, cols, with = FALSE]
}

.masked_circular_angular_spread <- function(encoded) {
  z <- .theta_degrees_to_z(encoded$theta)
  mean_cos <- mean(z[, 1], na.rm = TRUE)
  mean_sin <- mean(z[, 2], na.rm = TRUE)
  data.table::data.table(
    mean_cos = mean_cos,
    mean_sin = mean_sin,
    mean_resultant_length = sqrt(mean_cos^2 + mean_sin^2)
  )
}

.prepare_circular_embedding_plot_data <- function(encoded, color_by = NULL,
                                                  label_by = NULL,
                                                  metadata = NULL) {
  dt <- data.table::as.data.table(encoded)
  required <- c("spectrum_id", "theta")
  if (!all(required %in% names(dt)))
    stop("'encoded' must include spectrum_id and theta columns",
         call. = FALSE)

  if (!is.null(metadata)) {
    md <- data.table::as.data.table(metadata)
    join_col <- intersect(c("spectrum_id", "sample_name", "value_id"),
                          names(md))[1]
    if (is.na(join_col)) {
      if (nrow(md) != nrow(dt)) {
        stop("'metadata' must have a join id or the same row count as 'encoded'",
             call. = FALSE)
      }
      md[["spectrum_id"]] <- dt[["spectrum_id"]]
      join_col <- "spectrum_id"
    }
    if (join_col != "spectrum_id")
      md[["spectrum_id"]] <- as.character(md[[join_col]])
    dt <- merge(dt, md, by = "spectrum_id", all.x = TRUE, sort = FALSE)
  }

  for (col in c(color_by, label_by)) {
    if (!is.null(col) && !col %in% names(dt))
      stop("'", col, "' is not a column in encoded data or metadata",
           call. = FALSE)
  }

  dt
}
