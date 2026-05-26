#' Masked circular autoencoder for spectra
#'
#' @description
#' Fit, encode, and reconstruct spectra with an experimental masked circular
#' workflow. The model learns one circular coordinate per spectrum using
#' spectral values and observed-value masks only. Metadata such as polymer
#' identity, material class, instrument, or dataset may be used after training
#' for diagnostics, but is not used by fitting, validation splitting, encoder
#' input, or losses.
#'
#' The current implementation uses only base R and `stats`: it builds a
#' mask-aware pairwise distance table, places training spectra on a circle with
#' classical multidimensional scaling, encodes each spectrum as one angle in
#' degrees from 0 to 360, and reconstructs spectra with periodic least-squares
#' decoder terms.
#'
#' @param x An `OpenSpecy` object, or a numeric matrix/data frame with spectra
#'   in rows and wavenumbers in columns.
#' @param wavenumber Optional numeric wavenumber vector for matrix-like inputs.
#' @param target_distance Class-free target distance used for circular
#'   embedding and diagnostics.
#' @param lambda_rec,lambda_dist,lambda_uniform Non-negative diagnostic loss
#'   weights.
#' @param min_overlap_points Minimum shared observed wavenumber count for
#'   target distances.
#' @param min_overlap_fraction Minimum shared observed fraction for target
#'   distances.
#' @param decoder_degree Non-negative integer degree for periodic decoder terms.
#' @param validation_fraction Fraction of spectra held out using a seeded,
#'   label-free random split.
#' @param seed Random seed.
#' @param verbose Logical; print fitting progress.
#' @param model A `MaskedCircularAEModel` returned by
#'   `fit_masked_circular_ae()`.
#' @param as_specs Logical; return encoded coordinates as a `Specs` object.
#' @param theta Numeric angle vector in degrees for reconstruction.
#'
#' @return
#' `fit_masked_circular_ae()` returns a `MaskedCircularAEModel` object.
#' `encode_masked_circular_ae()` returns a `data.table` of circular angles
#' or, when `as_specs = TRUE`, a `Specs` object.
#' `reconstruct_masked_circular_ae()` returns an `OpenSpecy` object on the
#' model wavenumber grid.
#'
#' @examples
#' \dontrun{
#' data("test_lib")
#' small <- test_lib
#' small$spectra <- small$spectra[, seq_len(8)]
#' small$metadata <- small$metadata[seq_len(8), ]
#'
#' model <- fit_masked_circular_ae(
#'   small,
#'   min_overlap_points = 20,
#'   decoder_degree = 2,
#'   validation_fraction = 0.25
#' )
#' encoded <- encode_masked_circular_ae(model, small)
#' reconstructed <- reconstruct_masked_circular_ae(model, x = small)
#' }
#'
#' @author
#' Win Cowger
#'
#' @export
fit_masked_circular_ae <- function(x,
                                   wavenumber = NULL,
                                   target_distance = c("correlation",
                                                       "spectral_angle"),
                                   lambda_rec = 1,
                                   lambda_dist = 1,
                                   lambda_uniform = 0.01,
                                   min_overlap_points = 100,
                                   min_overlap_fraction = 0.25,
                                   decoder_degree = 3,
                                   validation_fraction = 0.2,
                                   seed = 1,
                                   verbose = TRUE) {
  target_distance <- match.arg(target_distance)

  if (any(!is.finite(c(lambda_rec, lambda_dist, lambda_uniform))) ||
      any(c(lambda_rec, lambda_dist, lambda_uniform) < 0) ||
      sum(c(lambda_rec, lambda_dist, lambda_uniform) > 0) == 0) {
    stop("loss weights must be finite, non-negative, and at least one must be positive",
         call. = FALSE)
  }

  decoder_degree <- as.integer(decoder_degree)
  if (length(decoder_degree) != 1L || is.na(decoder_degree) ||
      decoder_degree < 0L) {
    stop("'decoder_degree' must be one non-negative integer", call. = FALSE)
  }

  prep <- .prepare_masked_circular_input(x, wavenumber = wavenumber)
  if (!any(prep$mask_observed))
    stop("'x' must contain at least one observed finite spectral value",
         call. = FALSE)

  norm <- .masked_circular_normalize(prep$x, prep$mask_observed)
  split <- .masked_circular_validation_split(
    n = nrow(norm$x),
    validation_fraction = validation_fraction,
    seed = seed
  )

  train_idx <- split$train
  valid_idx <- split$validation

  effective_degree <- min(
    decoder_degree,
    max(0L, floor((length(train_idx) - 1L) / 2L))
  )

  embedding <- .fit_masked_circular_embedding(
    x = norm$x[train_idx, , drop = FALSE],
    mask = prep$mask_observed[train_idx, , drop = FALSE],
    target_distance = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction,
    seed = seed
  )

  decoder <- .fit_masked_circular_decoder(
    theta = embedding$theta,
    x = norm$x[train_idx, , drop = FALSE],
    mask = prep$mask_observed[train_idx, , drop = FALSE],
    degree = effective_degree
  )

  reference_x <- norm$x[train_idx, , drop = FALSE]
  reference_x_filled <- reference_x
  reference_x_filled[!is.finite(reference_x_filled)] <- 0

  state <- list(
    reference_ids = prep$spectrum_id[train_idx],
    reference_x = reference_x,
    reference_x_filled = reference_x_filled,
    reference_mask = prep$mask_observed[train_idx, , drop = FALSE],
    reference_theta = embedding$theta,
    reference_z = embedding$z,
    decoder = decoder,
    distance_bandwidth = embedding$distance_bandwidth
  )

  train_eval <- .masked_circular_evaluate_state(
    theta = embedding$theta,
    x = norm$x[train_idx, , drop = FALSE],
    mask = prep$mask_observed[train_idx, , drop = FALSE],
    decoder = decoder,
    target_distance = target_distance,
    lambda_rec = lambda_rec,
    lambda_dist = lambda_dist,
    lambda_uniform = lambda_uniform,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )
  history <- .masked_circular_loss_record(train_eval, "train")
  history[["epoch"]] <- 1L
  data.table::setcolorder(
    history,
    c("epoch", "split", "total_loss", "reconstruction_loss",
      "distance_loss", "uniform_loss", "valid_pairs")
  )

  if (length(valid_idx)) {
    valid_norm <- norm$x[valid_idx, , drop = FALSE]
    valid_mask <- prep$mask_observed[valid_idx, , drop = FALSE]
    valid_z <- .encode_masked_circular_state(
      x = valid_norm,
      mask = valid_mask,
      state = state,
      target_distance = target_distance,
      min_overlap_points = min_overlap_points,
      min_overlap_fraction = min_overlap_fraction
    )
    valid_eval <- .masked_circular_evaluate_state(
      theta = .z_to_theta_degrees(valid_z),
      x = valid_norm,
      mask = valid_mask,
      decoder = decoder,
      target_distance = target_distance,
      lambda_rec = lambda_rec,
      lambda_dist = lambda_dist,
      lambda_uniform = lambda_uniform,
      min_overlap_points = min_overlap_points,
      min_overlap_fraction = min_overlap_fraction
    )
    valid_history <- .masked_circular_loss_record(valid_eval, "validation")
    valid_history[["epoch"]] <- 1L
    data.table::setcolorder(valid_history, names(history))
    history <- data.table::rbindlist(list(history, valid_history))
  }

  if (isTRUE(verbose)) {
    message(
      "fitted R-native masked circular model; train loss: ",
      signif(history[split == "train"]$total_loss, 4)
    )
  }

  hyperparameters <- list(
    lambda_rec = lambda_rec,
    lambda_dist = lambda_dist,
    lambda_uniform = lambda_uniform,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction,
    decoder_degree = decoder_degree,
    effective_decoder_degree = effective_degree,
    validation_fraction = validation_fraction,
    seed = seed,
    verbose = verbose
  )

  model_id <- digest::digest(list(
    model_type = "masked_circular_ae",
    backend = "stats",
    wavenumber = prep$wavenumber,
    normalization = norm$normalization,
    hyperparameters = hyperparameters,
    target_distance = target_distance
  ))

  out <- list(
    model_type = "masked_circular_ae",
    backend = list(
      name = "stats",
      version = as.character(getRversion()),
      method = "cmdscale_periodic_lm"
    ),
    wavenumber = prep$wavenumber,
    normalization = norm$normalization,
    hyperparameters = hyperparameters,
    target_distance = target_distance,
    preprocessing_assumptions = "Input spectra are assumed to have been harmonized before fitting.",
    history = history,
    training_ids = prep$spectrum_id[train_idx],
    validation_ids = prep$spectrum_id[valid_idx],
    state = state,
    model_id = model_id
  )

  class(out) <- c("MaskedCircularAEModel", "list")
  out
}

#' @rdname fit_masked_circular_ae
#' @export
encode_masked_circular_ae <- function(model, x, wavenumber = NULL,
                                      as_specs = FALSE) {
  .check_masked_circular_model(model)

  prep <- .prepare_masked_circular_input(x, wavenumber = wavenumber)
  .validate_masked_circular_wavenumber(prep$wavenumber, model$wavenumber)
  norm <- .masked_circular_apply_normalization(
    prep$x,
    prep$mask_observed,
    model$normalization
  )

  z <- .encode_masked_circular_state(
    x = norm$x,
    mask = prep$mask_observed,
    state = model$state,
    target_distance = model$target_distance,
    min_overlap_points = model$hyperparameters$min_overlap_points,
    min_overlap_fraction = model$hyperparameters$min_overlap_fraction
  )
  theta <- .z_to_theta_degrees(z)
  summary <- .masked_circular_observed_summary(prep$mask_observed,
                                               prep$wavenumber)

  encoded <- data.table::data.table(
    spectrum_id = prep$spectrum_id,
    theta = theta,
    observed_points = summary$observed_points,
    min_wavenumber = summary$min_wavenumber,
    max_wavenumber = summary$max_wavenumber
  )

  if (isTRUE(as_specs)) {
    values <- matrix(encoded$theta, nrow = 1L)
    colnames(values) <- encoded$spectrum_id
    rownames(values) <- "theta"
    metadata <- data.table::data.table(
      value_id = encoded$spectrum_id,
      observed_points = encoded$observed_points,
      min_wavenumber = encoded$min_wavenumber,
      max_wavenumber = encoded$max_wavenumber
    )
    coords <- data.table::data.table(
      x = seq_along(encoded$spectrum_id) - 1L,
      y = 0,
      source_id = encoded$spectrum_id,
      value_id = encoded$spectrum_id
    )
    return(Specs(
      variables = "theta",
      values = values,
      coords = coords,
      metadata = metadata,
      attributes = list(
        variable_model = list(
          model_type = model$model_type,
          model_id = model$model_id,
          original_variables = model$wavenumber,
          variables = "theta",
          unit = "degrees"
        ),
        transformations = list(list(
          method = "masked_circular_ae",
          model_id = model$model_id
        ))
      )
    ))
  }

  encoded
}

#' @rdname fit_masked_circular_ae
#' @export
reconstruct_masked_circular_ae <- function(model, theta = NULL, x = NULL,
                                           wavenumber = NULL) {
  .check_masked_circular_model(model)

  supplied <- c(!is.null(theta), !is.null(x))
  if (sum(supplied) != 1L)
    stop("supply exactly one of 'theta' or 'x'", call. = FALSE)

  if (!is.null(x)) {
    encoded <- encode_masked_circular_ae(model, x, wavenumber = wavenumber)
    theta <- encoded$theta
    ids <- encoded$spectrum_id
    md <- encoded
  } else {
    theta <- .normalize_theta_degrees(theta)
    if (any(!is.finite(theta)))
      stop("'theta' must contain finite degree values", call. = FALSE)
    ids <- paste0("theta_", seq_along(theta))
    md <- data.table::data.table(
      spectrum_id = ids,
      theta = theta
    )
  }

  recon <- .masked_circular_decode(theta, model$state$decoder)
  recon <- sweep(recon, 2L, model$normalization$scale, "*")
  recon <- sweep(recon, 2L, model$normalization$center, "+")
  recon <- t(recon)
  colnames(recon) <- ids

  out <- as_OpenSpecy(
    x = model$wavenumber,
    spectra = recon,
    metadata = md
  )
  attr(out, "reconstruction_model") <- list(
    model_type = model$model_type,
    model_id = model$model_id
  )
  out
}

.prepare_masked_circular_input <- function(x, wavenumber = NULL) {
  attributes <- list()

  if (is_OpenSpecy(x)) {
    x <- as_OpenSpecy(x)
    if (!check_OpenSpecy(x))
      stop("'x' is not a valid OpenSpecy object", call. = FALSE)
    wavenumber <- as.numeric(x$wavenumber)
    spectra <- as.matrix(x$spectra)
    storage.mode(spectra) <- "double"
    spectrum_id <- colnames(spectra)
    if (is.null(spectrum_id))
      spectrum_id <- paste0("S", seq_len(ncol(spectra)))
    colnames(spectra) <- make.unique(as.character(spectrum_id))
    spectrum_id <- colnames(spectra)
    x_matrix <- t(spectra)
    metadata <- data.table::as.data.table(x$metadata)
    attributes <- attributes(x)
    attributes$names <- NULL
    attributes$class <- NULL
  } else if (inherits(x, c("matrix", "data.frame"))) {
    x_matrix <- as.matrix(x)
    storage.mode(x_matrix) <- "double"
    if (is.null(wavenumber)) {
      cn <- colnames(x_matrix)
      if (!is.null(cn)) {
        wavenumber <- suppressWarnings(as.numeric(cn))
      }
      if (is.null(wavenumber) || anyNA(wavenumber)) {
        wavenumber <- seq_len(ncol(x_matrix))
      }
    }
    wavenumber <- as.numeric(wavenumber)
    spectrum_id <- rownames(x_matrix)
    if (is.null(spectrum_id))
      spectrum_id <- paste0("S", seq_len(nrow(x_matrix)))
    spectrum_id <- make.unique(as.character(spectrum_id))
    rownames(x_matrix) <- spectrum_id
    metadata <- data.table::data.table(spectrum_id = spectrum_id)
  } else {
    stop("'x' must be an OpenSpecy object, matrix, or data.frame",
         call. = FALSE)
  }

  if (length(wavenumber) != ncol(x_matrix))
    stop("'wavenumber' must match the number of spectral columns",
         call. = FALSE)
  if (any(!is.finite(wavenumber)))
    stop("'wavenumber' must contain finite numeric values", call. = FALSE)

  mask <- is.finite(x_matrix)
  x_raw <- x_matrix
  x_raw[!mask] <- NA_real_

  list(
    x = x_raw,
    mask_observed = mask,
    wavenumber = wavenumber,
    spectrum_id = spectrum_id,
    metadata = metadata,
    attributes = attributes
  )
}

.validate_masked_circular_wavenumber <- function(input_wavenumber,
                                                 model_wavenumber,
                                                 tolerance = sqrt(.Machine$double.eps)) {
  if (length(input_wavenumber) != length(model_wavenumber) ||
      !isTRUE(all.equal(as.numeric(input_wavenumber),
                        as.numeric(model_wavenumber),
                        tolerance = tolerance,
                        check.attributes = FALSE))) {
    stop("input wavenumbers must match the fitted model grid exactly; ",
         "conform or process spectra before encoding or reconstruction",
         call. = FALSE)
  }
  invisible(TRUE)
}

.masked_circular_normalize <- function(x, mask, fill_value = 0) {
  x <- as.matrix(x)
  mask <- as.matrix(mask) & is.finite(x)
  center <- vapply(seq_len(ncol(x)), function(j) {
    vals <- x[mask[, j], j]
    if (length(vals)) mean(vals) else 0
  }, numeric(1))
  scale <- vapply(seq_len(ncol(x)), function(j) {
    vals <- x[mask[, j], j]
    if (length(vals) > 1L) stats::sd(vals) else 1
  }, numeric(1))
  scale[!is.finite(scale) | scale == 0] <- 1

  normalized <- sweep(x, 2L, center, "-")
  normalized <- sweep(normalized, 2L, scale, "/")
  normalized[!mask] <- NA_real_
  x_filled <- normalized
  x_filled[!is.finite(x_filled)] <- fill_value

  list(
    x = normalized,
    x_filled = x_filled,
    normalization = list(
      center = center,
      scale = scale,
      fill_value = fill_value,
      observed_count = colSums(mask)
    )
  )
}

.masked_circular_apply_normalization <- function(x, mask, normalization) {
  x <- as.matrix(x)
  mask <- as.matrix(mask) & is.finite(x)
  if (ncol(x) != length(normalization$center) ||
      ncol(x) != length(normalization$scale)) {
    stop("normalization statistics do not match input dimensions",
         call. = FALSE)
  }
  normalized <- sweep(x, 2L, normalization$center, "-")
  normalized <- sweep(normalized, 2L, normalization$scale, "/")
  normalized[!mask] <- NA_real_
  x_filled <- normalized
  x_filled[!is.finite(x_filled)] <- normalization$fill_value %||% 0
  list(x = normalized, x_filled = x_filled)
}

.masked_circular_observed_summary <- function(mask, wavenumber) {
  mask <- as.matrix(mask)
  data.table::data.table(
    observed_points = rowSums(mask, na.rm = TRUE),
    min_wavenumber = apply(mask, 1L, function(m) {
      if (any(m)) min(wavenumber[m]) else NA_real_
    }),
    max_wavenumber = apply(mask, 1L, function(m) {
      if (any(m)) max(wavenumber[m]) else NA_real_
    })
  )
}

.masked_circular_validation_split <- function(n, validation_fraction = 0.2,
                                              seed = 1) {
  if (!is.numeric(validation_fraction) || validation_fraction < 0 ||
      validation_fraction >= 1) {
    stop("'validation_fraction' must be >= 0 and < 1", call. = FALSE)
  }
  set.seed(seed)
  idx <- seq_len(n)
  n_validation <- floor(n * validation_fraction)
  validation <- if (n_validation > 0L) sample(idx, n_validation) else integer()
  train <- setdiff(idx, validation)
  list(train = train, validation = validation)
}

.fit_masked_circular_embedding <- function(x,
                                           mask,
                                           target_distance,
                                           min_overlap_points,
                                           min_overlap_fraction,
                                           seed = 1) {
  n <- nrow(x)
  if (n == 1L) {
    return(list(
      theta = 0,
      z = .theta_degrees_to_z(0),
      distance_bandwidth = 0.25,
      valid_pairs = 0L
    ))
  }

  target <- .masked_spectral_distance_matrix(
    x = x,
    mask = mask,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )

  complete <- .complete_masked_circular_distance(target$distance,
                                                 target$valid)
  coords <- NULL
  if (!is.null(complete)) {
    coords <- tryCatch(
      stats::cmdscale(stats::as.dist(complete), k = 2, eig = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(coords) || any(!is.finite(coords))) {
    coords <- .masked_circular_fallback_coords(x, seed = seed)
  }

  z <- .coords_to_unit_circle(coords, seed = seed)
  theta <- .z_to_theta_degrees(z)
  valid_values <- target$distance[target$valid & is.finite(target$distance)]
  positive_values <- valid_values[valid_values > 0]
  bandwidth <- stats::median(positive_values, na.rm = TRUE)
  if (!is.finite(bandwidth) || bandwidth <= 0)
    bandwidth <- 0.25

  list(
    theta = theta,
    z = z,
    distance_bandwidth = bandwidth,
    valid_pairs = sum(target$valid, na.rm = TRUE) / 2L
  )
}

.complete_masked_circular_distance <- function(distance, valid) {
  n <- nrow(distance)
  if (n < 2L)
    return(NULL)

  d <- distance
  good <- valid & is.finite(d)
  if (!any(good))
    return(NULL)

  fill <- max(d[good], na.rm = TRUE)
  if (!is.finite(fill) || fill <= 0)
    fill <- 1
  d[!good] <- fill
  diag(d) <- 0
  d <- (d + t(d)) / 2
  pmax(d, 0)
}

.masked_circular_fallback_coords <- function(x, seed = 1) {
  set.seed(seed)
  x_filled <- as.matrix(x)
  x_filled[!is.finite(x_filled)] <- 0
  n <- nrow(x_filled)
  if (n < 2L || all(!is.finite(x_filled)) ||
      sum(stats::var(as.vector(x_filled))) == 0) {
    theta <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
    return(cbind(cos(theta), sin(theta)))
  }

  pc <- tryCatch(
    stats::prcomp(x_filled, center = TRUE, scale. = FALSE, rank. = 2),
    error = function(e) NULL
  )
  if (is.null(pc) || is.null(pc$x) || ncol(pc$x) == 0L) {
    theta <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
    return(cbind(cos(theta), sin(theta)))
  }

  coords <- pc$x[, seq_len(min(2L, ncol(pc$x))), drop = FALSE]
  if (ncol(coords) == 1L)
    coords <- cbind(coords[, 1], 0)
  coords
}

.coords_to_unit_circle <- function(coords, seed = 1) {
  coords <- as.matrix(coords)
  n <- nrow(coords)
  if (ncol(coords) < 2L)
    coords <- cbind(coords[, 1], 0)
  coords <- coords[, 1:2, drop = FALSE]
  coords <- sweep(coords, 2L, colMeans(coords, na.rm = TRUE), "-")
  radius <- sqrt(rowSums(coords^2))

  if (all(!is.finite(radius) | radius <= sqrt(.Machine$double.eps))) {
    theta <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
    return(cbind(cos(theta), sin(theta)))
  }

  missing_radius <- !is.finite(radius) | radius <= sqrt(.Machine$double.eps)
  if (any(missing_radius)) {
    set.seed(seed)
    fill_theta <- seq(0, 2 * pi, length.out = sum(missing_radius) + 1L)[-1L]
    coords[missing_radius, ] <- cbind(cos(fill_theta), sin(fill_theta))
    radius <- sqrt(rowSums(coords^2))
  }

  coords / radius
}

.fit_masked_circular_decoder <- function(theta, x, mask, degree) {
  basis <- .masked_circular_basis(theta, degree)
  coefficients <- matrix(0, nrow = ncol(basis), ncol = ncol(x))
  rownames(coefficients) <- colnames(basis)

  for (j in seq_len(ncol(x))) {
    obs <- mask[, j] & is.finite(x[, j])
    if (!any(obs)) {
      coefficients[1L, j] <- 0
      next
    }
    if (sum(obs) == 1L) {
      coefficients[1L, j] <- x[obs, j]
      next
    }
    fit <- tryCatch(
      stats::lm.fit(x = basis[obs, , drop = FALSE], y = x[obs, j]),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      coefficients[1L, j] <- mean(x[obs, j])
      next
    }
    coefs <- fit$coefficients
    coefs[!is.finite(coefs)] <- 0
    coefficients[, j] <- coefs
  }

  list(
    degree = as.integer(degree),
    coefficients = coefficients,
    basis_names = colnames(basis)
  )
}

.masked_circular_basis <- function(theta, degree) {
  theta <- .theta_degrees_to_radians(theta)
  degree <- as.integer(degree)
  basis <- matrix(1, nrow = length(theta), ncol = 1L)
  colnames(basis) <- "intercept"
  if (degree <= 0L)
    return(basis)

  for (k in seq_len(degree)) {
    basis <- cbind(basis, cos(k * theta), sin(k * theta))
    colnames(basis)[ncol(basis) - 1:0] <- c(
      paste0("cos", k),
      paste0("sin", k)
    )
  }
  basis
}

.masked_circular_decode <- function(theta, decoder) {
  basis <- .masked_circular_basis(theta, decoder$degree)
  basis %*% decoder$coefficients
}

.encode_masked_circular_state <- function(x,
                                          mask,
                                          state,
                                          target_distance,
                                          min_overlap_points,
                                          min_overlap_fraction) {
  x <- as.matrix(x)
  mask <- as.matrix(mask) & is.finite(x)
  z <- matrix(NA_real_, nrow = nrow(x), ncol = 2L)

  cross <- .masked_spectral_distance_cross(
    x = x,
    mask = mask,
    reference_x = state$reference_x,
    reference_mask = state$reference_mask,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )

  for (i in seq_len(nrow(x))) {
    valid <- cross$valid[i, ] & is.finite(cross$distance[i, ])
    if (any(valid)) {
      d <- cross$distance[i, valid]
      ref_z <- state$reference_z[valid, , drop = FALSE]
      overlap_weight <- pmax(cross$overlap_fraction[i, valid], 0)

      if (any(d <= sqrt(.Machine$double.eps))) {
        w <- as.numeric(d <= sqrt(.Machine$double.eps)) * pmax(overlap_weight, 1)
      } else {
        bandwidth <- state$distance_bandwidth %||% 0.25
        w <- exp(-((d / bandwidth)^2)) * pmax(overlap_weight, 1e-6)
      }

      zi <- colSums(ref_z * w) / sum(w)
      zi_norm <- sqrt(sum(zi^2))
      if (!is.finite(zi_norm) || zi_norm <= sqrt(.Machine$double.eps)) {
        zi <- ref_z[which.min(d), ]
      } else {
        zi <- zi / zi_norm
      }
      z[i, ] <- zi
    } else {
      z[i, ] <- .masked_circular_nearest_fallback(
        x = x[i, ],
        state = state
      )
    }
  }

  z
}

.masked_spectral_distance_cross <- function(x,
                                            mask,
                                            reference_x,
                                            reference_mask,
                                            method,
                                            min_overlap_points,
                                            min_overlap_fraction) {
  method <- match.arg(method, c("correlation", "spectral_angle"))
  x <- as.matrix(x)
  reference_x <- as.matrix(reference_x)
  mask <- as.matrix(mask) & is.finite(x)
  reference_mask <- as.matrix(reference_mask) & is.finite(reference_x)

  if (ncol(x) != ncol(reference_x))
    stop("cross-distance matrices must have the same number of columns",
         call. = FALSE)

  n <- nrow(x)
  m <- nrow(reference_x)
  p <- ncol(x)
  distance <- matrix(NA_real_, n, m)
  valid <- matrix(FALSE, n, m)
  overlap_count <- matrix(0L, n, m)
  overlap_fraction <- matrix(0, n, m)

  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      overlap <- mask[i, ] & reference_mask[j, ]
      n_overlap <- sum(overlap)
      frac <- n_overlap / p
      overlap_count[i, j] <- n_overlap
      overlap_fraction[i, j] <- frac

      if (n_overlap < min_overlap_points || frac < min_overlap_fraction)
        next

      xi <- x[i, overlap]
      xj <- reference_x[j, overlap]
      if (method == "correlation") {
        if (stats::sd(xi) == 0 || stats::sd(xj) == 0)
          next
        r <- stats::cor(xi, xj)
        if (!is.finite(r))
          next
        d <- (1 - max(min(r, 1), -1)) / 2
      } else {
        denom <- sqrt(sum(xi^2)) * sqrt(sum(xj^2))
        if (!is.finite(denom) || denom <= 0)
          next
        cosang <- sum(xi * xj) / denom
        d <- acos(max(min(cosang, 1), -1)) / pi
      }

      distance[i, j] <- d
      valid[i, j] <- TRUE
    }
  }

  list(
    distance = distance,
    valid = valid,
    overlap_count = overlap_count,
    overlap_fraction = overlap_fraction
  )
}

.masked_circular_nearest_fallback <- function(x, state) {
  x_filled <- as.numeric(x)
  x_filled[!is.finite(x_filled)] <- 0
  diff <- sweep(state$reference_x_filled, 2L, x_filled, "-")
  d <- rowMeans(diff^2)
  if (!any(is.finite(d))) {
    z <- colMeans(state$reference_z)
  } else {
    z <- state$reference_z[which.min(d), ]
  }
  norm <- sqrt(sum(z^2))
  if (!is.finite(norm) || norm <= 0)
    return(c(1, 0))
  z / norm
}

.masked_circular_evaluate_state <- function(theta,
                                            x,
                                            mask,
                                            decoder,
                                            target_distance,
                                            lambda_rec,
                                            lambda_dist,
                                            lambda_uniform,
                                            min_overlap_points,
                                            min_overlap_fraction) {
  x_hat <- .masked_circular_decode(theta, decoder)
  rec_loss <- masked_reconstruction_loss(x, x_hat, mask)

  target <- .masked_spectral_distance_matrix(
    x = x,
    mask = mask,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )
  circle <- outer(theta, theta, Vectorize(circ_dist))
  valid <- target$valid & is.finite(target$distance) & is.finite(circle)
  upper <- upper.tri(valid) & valid
  valid_pairs <- sum(upper)

  if (valid_pairs > 0L) {
    weight <- target$weight
    weight[!is.finite(weight) | weight <= 0] <- 1
    dist_loss <- sum(((circle[upper] - target$distance[upper])^2) *
                       weight[upper]) / sum(weight[upper])
  } else {
    dist_loss <- 0
  }

  mean_z <- colMeans(.theta_degrees_to_z(theta))
  uniform_loss <- sum(mean_z^2)
  total_loss <- lambda_rec * rec_loss +
    lambda_dist * dist_loss +
    lambda_uniform * uniform_loss

  list(
    total_loss = total_loss,
    reconstruction_loss = rec_loss,
    distance_loss = dist_loss,
    uniform_loss = uniform_loss,
    valid_pairs = as.integer(valid_pairs)
  )
}

.masked_circular_loss_record <- function(res, split) {
  data.table::data.table(
    split = split,
    total_loss = as.numeric(res$total_loss),
    reconstruction_loss = as.numeric(res$reconstruction_loss),
    distance_loss = as.numeric(res$distance_loss),
    uniform_loss = as.numeric(res$uniform_loss),
    valid_pairs = as.integer(res$valid_pairs)
  )
}

.check_masked_circular_model <- function(model) {
  if (!inherits(model, "MaskedCircularAEModel") ||
      !identical(model$model_type, "masked_circular_ae") ||
      is.null(model$wavenumber) ||
      is.null(model$normalization) ||
      is.null(model$state$decoder) ||
      is.null(model$state$reference_z)) {
    stop("'model' must be a MaskedCircularAEModel returned by fit_masked_circular_ae()",
         call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
