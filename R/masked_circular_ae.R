#' Masked circular autoencoder for spectra
#'
#' @description
#' Fit, encode, and reconstruct spectra with an experimental masked circular
#' autoencoder. The model learns one circular coordinate per spectrum using
#' spectral values and observed-value masks only. Metadata such as polymer
#' identity, material class, instrument, or dataset may be used after training
#' for diagnostics, but is not used by fitting, validation splitting, encoder
#' input, or losses.
#'
#' @param x An `OpenSpecy` object, or a numeric matrix/data frame with spectra
#'   in rows and wavenumbers in columns.
#' @param wavenumber Optional numeric wavenumber vector for matrix-like inputs.
#' @param n_hidden Integer vector of hidden layer widths.
#' @param target_distance Class-free target distance used during training.
#' @param lambda_rec,lambda_dist,lambda_uniform Non-negative loss weights.
#' @param min_overlap_points Minimum shared observed wavenumber count for
#'   target distances.
#' @param min_overlap_fraction Minimum shared observed fraction for target
#'   distances.
#' @param random_block_mask Logical; randomly hide contiguous observed
#'   wavenumber blocks from the encoder during training.
#' @param block_mask_fraction Fraction of wavenumber positions to hide in a
#'   random contiguous block when `random_block_mask = TRUE`.
#' @param epochs Number of training epochs.
#' @param batch_size Mini-batch size.
#' @param learning_rate Optimizer learning rate.
#' @param validation_fraction Fraction of spectra held out using a seeded,
#'   label-free random split.
#' @param seed Random seed.
#' @param verbose Logical; print training progress.
#' @param model A `MaskedCircularAEModel` returned by
#'   `fit_masked_circular_ae()`.
#' @param as_specs Logical; return encoded coordinates as a `Specs` object.
#' @param theta Numeric angle vector in radians for reconstruction.
#' @param z Numeric matrix/data frame with two unit-circle columns for
#'   reconstruction.
#'
#' @return
#' `fit_masked_circular_ae()` returns a `MaskedCircularAEModel` object.
#' `encode_masked_circular_ae()` returns a `data.table` of circular coordinates
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
#'   n_hidden = c(32, 8),
#'   min_overlap_points = 20,
#'   epochs = 5,
#'   batch_size = 4
#' )
#' encoded <- encode_masked_circular_ae(model, small)
#' reconstructed <- reconstruct_masked_circular_ae(model, x = small)
#' }
#'
#' @author
#' Win Cowger
#'
#' @importFrom torch nn_linear nn_module nn_relu nn_sequential optim_adam
#'   torch_acos torch_cat torch_float torch_manual_seed torch_matmul
#'   torch_tensor torch_is_installed with_no_grad
#'
#' @export
fit_masked_circular_ae <- function(x,
                                   wavenumber = NULL,
                                   n_hidden = c(512, 128, 32),
                                   target_distance = c("correlation",
                                                       "spectral_angle"),
                                   lambda_rec = 1,
                                   lambda_dist = 1,
                                   lambda_uniform = 0.01,
                                   min_overlap_points = 100,
                                   min_overlap_fraction = 0.25,
                                   random_block_mask = TRUE,
                                   block_mask_fraction = 0.15,
                                   epochs = 100,
                                   batch_size = 128,
                                   learning_rate = 1e-3,
                                   validation_fraction = 0.2,
                                   seed = 1,
                                   verbose = TRUE) {
  target_distance <- match.arg(target_distance)
  .check_masked_circular_ae_backend()

  if (any(!is.finite(c(lambda_rec, lambda_dist, lambda_uniform))) ||
      any(c(lambda_rec, lambda_dist, lambda_uniform) < 0) ||
      sum(c(lambda_rec, lambda_dist, lambda_uniform) > 0) == 0) {
    stop("loss weights must be finite, non-negative, and at least one must be positive",
         call. = FALSE)
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

  torch <- asNamespace("torch")
  torch$torch_manual_seed(as.integer(seed))

  net <- .masked_circular_module(
    n_wavenumber = ncol(norm$x_filled),
    n_hidden = n_hidden
  )
  optimizer <- torch$optim_adam(net$parameters, lr = learning_rate)

  history <- data.table::data.table(
    epoch = integer(),
    split = character(),
    total_loss = numeric(),
    reconstruction_loss = numeric(),
    distance_loss = numeric(),
    uniform_loss = numeric(),
    valid_pairs = integer(),
    random_block_masked = integer()
  )

  train_idx <- split$train
  valid_idx <- split$validation

  for (epoch in seq_len(as.integer(epochs))) {
    train_idx <- sample(train_idx)
    batches <- split(train_idx, ceiling(seq_along(train_idx) / batch_size))

    epoch_parts <- lapply(batches, function(idx) {
      optimizer$zero_grad()
      res <- .masked_circular_training_step(
        net = net,
        x_norm = norm$x_filled[idx, , drop = FALSE],
        mask_observed = prep$mask_observed[idx, , drop = FALSE],
        target_distance = target_distance,
        lambda_rec = lambda_rec,
        lambda_dist = lambda_dist,
        lambda_uniform = lambda_uniform,
        min_overlap_points = min_overlap_points,
        min_overlap_fraction = min_overlap_fraction,
        random_block_mask = random_block_mask,
        block_mask_fraction = block_mask_fraction,
        training = TRUE
      )
      res$total_loss$backward()
      optimizer$step()
      .masked_circular_loss_record(res, "train")
    })

    history <- rbind(
      history,
      .masked_circular_average_records(epoch_parts, epoch, "train")
    )

    if (length(valid_idx)) {
      valid_res <- .masked_circular_training_step(
        net = net,
        x_norm = norm$x_filled[valid_idx, , drop = FALSE],
        mask_observed = prep$mask_observed[valid_idx, , drop = FALSE],
        target_distance = target_distance,
        lambda_rec = lambda_rec,
        lambda_dist = lambda_dist,
        lambda_uniform = lambda_uniform,
        min_overlap_points = min_overlap_points,
        min_overlap_fraction = min_overlap_fraction,
        random_block_mask = FALSE,
        block_mask_fraction = block_mask_fraction,
        training = FALSE
      )
      history <- rbind(
        history,
        .masked_circular_average_records(
          list(.masked_circular_loss_record(valid_res, "validation")),
          epoch,
          "validation"
        )
      )
    }

    if (isTRUE(verbose) && (epoch == 1L || epoch == epochs ||
                            epoch %% max(1L, floor(epochs / 10L)) == 0L)) {
      msg <- utils::tail(history[split == "train"]$total_loss, 1L)
      message("epoch ", epoch, " train loss: ", signif(msg, 4))
    }
  }

  hyperparameters <- list(
    n_hidden = as.integer(n_hidden),
    lambda_rec = lambda_rec,
    lambda_dist = lambda_dist,
    lambda_uniform = lambda_uniform,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction,
    epochs = epochs,
    batch_size = batch_size,
    learning_rate = learning_rate,
    validation_fraction = validation_fraction,
    seed = seed,
    verbose = verbose
  )

  random_masking <- list(
    enabled = isTRUE(random_block_mask),
    block_mask_fraction = block_mask_fraction
  )

  model_id <- digest::digest(list(
    model_type = "masked_circular_ae",
    wavenumber = prep$wavenumber,
    normalization = norm$normalization,
    hyperparameters = hyperparameters,
    target_distance = target_distance,
    random_block_mask = random_masking
  ))

  out <- list(
    model_type = "masked_circular_ae",
    backend = list(
      name = "torch",
      version = as.character(utils::packageVersion("torch"))
    ),
    wavenumber = prep$wavenumber,
    normalization = norm$normalization,
    hyperparameters = hyperparameters,
    target_distance = target_distance,
    random_block_mask = random_masking,
    preprocessing_assumptions = "Input spectra are assumed to have been harmonized before fitting.",
    history = history,
    training_ids = prep$spectrum_id[train_idx],
    validation_ids = prep$spectrum_id[valid_idx],
    state = list(net = net),
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
  .check_masked_circular_ae_backend()

  prep <- .prepare_masked_circular_input(x, wavenumber = wavenumber)
  .validate_masked_circular_wavenumber(prep$wavenumber, model$wavenumber)
  norm <- .masked_circular_apply_normalization(
    prep$x,
    prep$mask_observed,
    model$normalization
  )

  torch <- asNamespace("torch")
  model$state$net$eval()
  input <- torch$torch_tensor(norm$x_filled, dtype = torch$torch_float())
  mask <- torch$torch_tensor(prep$mask_observed * 1, dtype = torch$torch_float())
  res <- torch$with_no_grad({
    model$state$net$forward(input, mask)
  })
  z <- as.matrix(as.array(res$z$cpu()))
  theta <- atan2(z[, 2], z[, 1])
  summary <- .masked_circular_observed_summary(prep$mask_observed,
                                               prep$wavenumber)

  encoded <- data.table::data.table(
    spectrum_id = prep$spectrum_id,
    theta = theta,
    z1 = z[, 1],
    z2 = z[, 2],
    observed_points = summary$observed_points,
    min_wavenumber = summary$min_wavenumber,
    max_wavenumber = summary$max_wavenumber
  )

  if (isTRUE(as_specs)) {
    values <- t(as.matrix(encoded[, c("z1", "z2")]))
    colnames(values) <- encoded$spectrum_id
    rownames(values) <- c("z1", "z2")
    metadata <- data.table::data.table(
      value_id = encoded$spectrum_id,
      theta = encoded$theta,
      observed_points = encoded$observed_points,
      min_wavenumber = encoded$min_wavenumber,
      max_wavenumber = encoded$max_wavenumber
    )
    coords <- data.table::data.table(
      x = encoded$z1,
      y = encoded$z2,
      source_id = encoded$spectrum_id,
      value_id = encoded$spectrum_id
    )
    return(Specs(
      variables = c("z1", "z2"),
      values = values,
      coords = coords,
      metadata = metadata,
      attributes = list(
        variable_model = list(
          model_type = model$model_type,
          model_id = model$model_id,
          original_variables = model$wavenumber,
          variables = c("z1", "z2")
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
reconstruct_masked_circular_ae <- function(model, theta = NULL, z = NULL,
                                           x = NULL, wavenumber = NULL) {
  .check_masked_circular_model(model)
  .check_masked_circular_ae_backend()

  supplied <- c(!is.null(theta), !is.null(z), !is.null(x))
  if (sum(supplied) != 1L)
    stop("supply exactly one of 'theta', 'z', or 'x'", call. = FALSE)

  if (!is.null(x)) {
    encoded <- encode_masked_circular_ae(model, x, wavenumber = wavenumber)
    z_mat <- as.matrix(encoded[, c("z1", "z2")])
    ids <- encoded$spectrum_id
    md <- encoded
  } else if (!is.null(theta)) {
    theta <- as.numeric(theta)
    z_mat <- cbind(cos(theta), sin(theta))
    ids <- paste0("theta_", seq_along(theta))
    md <- data.table::data.table(
      spectrum_id = ids,
      theta = theta,
      z1 = z_mat[, 1],
      z2 = z_mat[, 2]
    )
  } else {
    z_mat <- as.matrix(z)
    if (ncol(z_mat) != 2L)
      stop("'z' must have exactly two columns", call. = FALSE)
    norm <- sqrt(rowSums(z_mat^2))
    if (any(!is.finite(norm) | norm <= 0))
      stop("'z' must contain finite non-zero coordinates", call. = FALSE)
    z_mat <- z_mat / norm
    theta <- atan2(z_mat[, 2], z_mat[, 1])
    ids <- rownames(z_mat)
    if (is.null(ids))
      ids <- paste0("z_", seq_len(nrow(z_mat)))
    md <- data.table::data.table(
      spectrum_id = ids,
      theta = theta,
      z1 = z_mat[, 1],
      z2 = z_mat[, 2]
    )
  }

  torch <- asNamespace("torch")
  model$state$net$eval()
  z_tensor <- torch$torch_tensor(z_mat, dtype = torch$torch_float())
  x_hat <- torch$with_no_grad({
    model$state$net$decode(z_tensor)
  })
  recon <- as.matrix(as.array(x_hat$cpu()))
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

.check_masked_circular_ae_backend <- function() {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("masked circular autoencoder fitting, encoding, and reconstruction ",
         "require the 'torch' package; install it to use this experimental ",
         "neural backend",
         call. = FALSE)
  }
  torch <- asNamespace("torch")
  if (!isTRUE(torch$torch_is_installed())) {
    stop("masked circular autoencoder fitting, encoding, and reconstruction ",
         "require torch's native libtorch/Lantern runtime; run ",
         "torch::install_torch() before using this neural backend",
         call. = FALSE)
  }
  invisible(TRUE)
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

.masked_circular_module <- function(n_wavenumber, n_hidden = c(512, 128, 32),
                                    eps = 1e-8) {
  torch <- asNamespace("torch")
  hidden <- as.integer(n_hidden)
  input_dim <- 2L * as.integer(n_wavenumber)
  output_dim <- as.integer(n_wavenumber)

  module <- torch$nn_module(
    "MaskedCircularAEModule",
    initialize = function() {
      enc_layers <- list()
      in_dim <- input_dim
      for (h in hidden) {
        enc_layers[[length(enc_layers) + 1L]] <- torch$nn_linear(in_dim, h)
        enc_layers[[length(enc_layers) + 1L]] <- torch$nn_relu()
        in_dim <- h
      }
      enc_layers[[length(enc_layers) + 1L]] <- torch$nn_linear(in_dim, 2L)
      self$encoder <- do.call(torch$nn_sequential, enc_layers)

      dec_layers <- list()
      in_dim <- 2L
      for (h in rev(hidden)) {
        dec_layers[[length(dec_layers) + 1L]] <- torch$nn_linear(in_dim, h)
        dec_layers[[length(dec_layers) + 1L]] <- torch$nn_relu()
        in_dim <- h
      }
      dec_layers[[length(dec_layers) + 1L]] <- torch$nn_linear(in_dim, output_dim)
      self$decoder <- do.call(torch$nn_sequential, dec_layers)
    },
    encode = function(x_visible, mask_visible) {
      input <- torch$torch_cat(list(x_visible, mask_visible), dim = 2)
      raw_z <- self$encoder(input)
      denom <- raw_z$pow(2)$sum(dim = 2, keepdim = TRUE)$add(eps)$sqrt()
      raw_z / denom
    },
    decode = function(z) {
      self$decoder(z)
    },
    forward = function(x_visible, mask_visible) {
      z <- self$encode(x_visible, mask_visible)
      list(z = z, x_hat = self$decode(z))
    }
  )

  module()
}

.masked_circular_training_step <- function(net, x_norm, mask_observed,
                                           target_distance,
                                           lambda_rec,
                                           lambda_dist,
                                           lambda_uniform,
                                           min_overlap_points,
                                           min_overlap_fraction,
                                           random_block_mask = FALSE,
                                           block_mask_fraction = 0.15,
                                           training = TRUE) {
  torch <- asNamespace("torch")
  visible <- .apply_random_spectral_block_mask(
    x = x_norm,
    mask_observed = mask_observed,
    random_block_mask = isTRUE(random_block_mask) && isTRUE(training),
    block_mask_fraction = block_mask_fraction
  )

  x_tensor <- torch$torch_tensor(x_norm, dtype = torch$torch_float())
  x_visible <- torch$torch_tensor(visible$x_visible, dtype = torch$torch_float())
  mask_obs <- torch$torch_tensor(mask_observed * 1, dtype = torch$torch_float())
  mask_visible <- torch$torch_tensor(visible$mask_visible * 1,
                                     dtype = torch$torch_float())

  if (isTRUE(training)) net$train() else net$eval()
  res <- net$forward(x_visible, mask_visible)
  diff <- (res$x_hat - x_tensor)$pow(2) * mask_obs
  obs <- mask_obs$sum(dim = 2)$clamp(min = 1)
  rec_loss <- (diff$sum(dim = 2) / obs)$mean()

  target <- .masked_spectral_distance_matrix(
    x = x_norm,
    mask = mask_observed,
    method = target_distance,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction
  )

  valid_pairs <- sum(target$valid, na.rm = TRUE) / 2L
  if (valid_pairs > 0L) {
    target_distance_values <- target$distance
    target_distance_values[
      !target$valid | !is.finite(target_distance_values)
    ] <- 0
    target_weight_values <- target$weight
    target_weight_values[!target$valid | !is.finite(target_weight_values)] <- 0
    target_tensor <- torch$torch_tensor(target_distance_values,
                                        dtype = torch$torch_float())
    weight_tensor <- torch$torch_tensor(target_weight_values,
                                        dtype = torch$torch_float())
    dot <- torch$torch_matmul(res$z, res$z$transpose(1, 2))
    circle <- torch$torch_acos(dot$clamp(min = -1 + 1e-7,
                                         max = 1 - 1e-7)) / pi
    dist_loss <- (((circle - target_tensor)$pow(2) * weight_tensor)$sum() /
                    weight_tensor$sum()$clamp(min = 1))
  } else {
    dist_loss <- rec_loss * 0
  }

  mean_z <- res$z$mean(dim = 1)
  uniform_loss <- mean_z$pow(2)$sum()
  total_loss <- lambda_rec * rec_loss +
    lambda_dist * dist_loss +
    lambda_uniform * uniform_loss

  list(
    total_loss = total_loss,
    reconstruction_loss = rec_loss,
    distance_loss = dist_loss,
    uniform_loss = uniform_loss,
    valid_pairs = as.integer(valid_pairs),
    random_block_masked = visible$masked_count,
    z = res$z
  )
}

.apply_random_spectral_block_mask <- function(x, mask_observed,
                                              random_block_mask = TRUE,
                                              block_mask_fraction = 0.15) {
  x_visible <- as.matrix(x)
  mask_visible <- as.matrix(mask_observed)

  if (!isTRUE(random_block_mask) || block_mask_fraction <= 0) {
    x_visible[!mask_visible] <- 0
    return(list(
      x_visible = x_visible,
      mask_visible = mask_visible,
      masked_count = 0L
    ))
  }

  n_wave <- ncol(x_visible)
  block_len <- max(1L, min(n_wave, ceiling(n_wave * block_mask_fraction)))
  masked_count <- 0L

  for (i in seq_len(nrow(x_visible))) {
    start <- sample.int(n_wave - block_len + 1L, 1L)
    idx <- seq.int(start, length.out = block_len)
    hide <- idx[mask_visible[i, idx]]
    if (length(hide)) {
      mask_visible[i, hide] <- FALSE
      masked_count <- masked_count + length(hide)
    }
  }

  x_visible[!mask_visible] <- 0
  list(
    x_visible = x_visible,
    mask_visible = mask_visible,
    masked_count = as.integer(masked_count)
  )
}

.masked_circular_loss_record <- function(res, split) {
  data.table::data.table(
    split = split,
    total_loss = as.numeric(res$total_loss$item()),
    reconstruction_loss = as.numeric(res$reconstruction_loss$item()),
    distance_loss = as.numeric(res$distance_loss$item()),
    uniform_loss = as.numeric(res$uniform_loss$item()),
    valid_pairs = as.integer(res$valid_pairs),
    random_block_masked = as.integer(res$random_block_masked)
  )
}

.masked_circular_average_records <- function(records, epoch, split) {
  dt <- data.table::rbindlist(records)
  data.table::data.table(
    epoch = as.integer(epoch),
    split = split,
    total_loss = mean(dt$total_loss),
    reconstruction_loss = mean(dt$reconstruction_loss),
    distance_loss = mean(dt$distance_loss),
    uniform_loss = mean(dt$uniform_loss),
    valid_pairs = sum(dt$valid_pairs),
    random_block_masked = sum(dt$random_block_masked)
  )
}

.check_masked_circular_model <- function(model) {
  if (!inherits(model, "MaskedCircularAEModel") ||
      !identical(model$model_type, "masked_circular_ae") ||
      is.null(model$wavenumber) ||
      is.null(model$normalization) ||
      is.null(model$state$net)) {
    stop("'model' must be a MaskedCircularAEModel returned by fit_masked_circular_ae()",
         call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
