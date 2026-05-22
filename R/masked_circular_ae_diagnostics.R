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
#' summaries. `plot_circular_embedding()` returns a `ggplot` object.
#'
#' @examples
#' \dontrun{
#' data("test_lib")
#' model <- fit_masked_circular_ae(test_lib, epochs = 5)
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_circular_embedding() requires the optional 'ggplot2' package",
         call. = FALSE)
  }

  dt <- .prepare_circular_embedding_plot_data(
    encoded = encoded,
    color_by = color_by,
    label_by = label_by,
    metadata = metadata
  )

  p <- ggplot2::ggplot(dt, ggplot2::aes_string(x = "z1", y = "z2")) +
    ggplot2::geom_path(
      data = data.frame(
        z1 = cos(seq(0, 2 * pi, length.out = 181)),
        z2 = sin(seq(0, 2 * pi, length.out = 181))
      ),
      ggplot2::aes_string(x = "z1", y = "z2"),
      inherit.aes = FALSE,
      color = "grey75"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "z1", y = "z2")

  if (!is.null(color_by)) {
    p <- p + ggplot2::geom_point(ggplot2::aes_string(color = color_by))
  } else {
    p <- p + ggplot2::geom_point()
  }

  if (!is.null(label_by)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes_string(label = label_by),
      vjust = -0.6,
      size = 3
    )
  }

  p
}

.masked_circular_reconstruction_diagnostics <- function(model, prep, encoded) {
  recon <- reconstruct_masked_circular_ae(model, z = as.matrix(encoded[, c("z1", "z2")]))
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
  mean_z1 <- mean(encoded$z1, na.rm = TRUE)
  mean_z2 <- mean(encoded$z2, na.rm = TRUE)
  data.table::data.table(
    mean_z1 = mean_z1,
    mean_z2 = mean_z2,
    mean_resultant_length = sqrt(mean_z1^2 + mean_z2^2)
  )
}

.prepare_circular_embedding_plot_data <- function(encoded, color_by = NULL,
                                                  label_by = NULL,
                                                  metadata = NULL) {
  dt <- data.table::as.data.table(encoded)
  required <- c("spectrum_id", "theta", "z1", "z2")
  if (!all(required %in% names(dt)))
    stop("'encoded' must include spectrum_id, theta, z1, and z2 columns",
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
