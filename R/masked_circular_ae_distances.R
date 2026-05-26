#' Masked circular autoencoder distances and losses
#'
#' @description
#' Helper functions for the masked circular autoencoder workflow. These
#' functions do not use labels or metadata.
#'
#' @param theta1,theta2 Numeric angle vectors in degrees.
#' @param x Numeric matrix with spectra in rows and wavenumbers in columns.
#' @param x_hat Numeric matrix of reconstructed spectra matching `x`.
#' @param mask Logical matrix matching `x`; `TRUE` marks observed values.
#' @param method Target distance, either `"correlation"` or
#'   `"spectral_angle"`.
#' @param min_overlap_points Minimum shared observed wavenumber count required
#'   for a pairwise target distance.
#' @param min_overlap_fraction Minimum shared observed fraction required for a
#'   pairwise target distance.
#' @param pairs Optional two-column matrix or data frame of one-based row pairs.
#' @param weights Pair weight strategy, either `"overlap"` or `"uniform"`.
#'
#' @return
#' `circ_dist()` returns numeric circular distances scaled from 0 to 1.
#' `masked_reconstruction_loss()` returns one scalar mean loss. 
#' `masked_spectral_distance()` returns a `data.table` with pairwise distances,
#' validity, overlap counts, overlap fractions, and weights.
#'
#' @examples
#' circ_dist(0, 360)
#' circ_dist(0, 180)
#'
#' x <- matrix(c(1, 2, NA, 1, 4, 5), nrow = 2, byrow = TRUE)
#' mask <- is.finite(x)
#' masked_spectral_distance(x, mask, min_overlap_points = 2)
#'
#' @author
#' Win Cowger
#'
#' @export
circ_dist <- function(theta1, theta2) {
  theta1 <- .normalize_theta_degrees(theta1)
  theta2 <- .normalize_theta_degrees(theta2)
  delta <- abs((theta1 - theta2 + 180) %% 360 - 180)
  delta / 180
}

.normalize_theta_degrees <- function(theta) {
  theta <- as.numeric(theta)
  theta <- theta %% 360
  theta[theta < 0] <- theta[theta < 0] + 360
  theta
}

.theta_degrees_to_radians <- function(theta) {
  .normalize_theta_degrees(theta) * pi / 180
}

.theta_degrees_to_z <- function(theta) {
  theta_rad <- .theta_degrees_to_radians(theta)
  cbind(cos(theta_rad), sin(theta_rad))
}

.z_to_theta_degrees <- function(z) {
  z <- as.matrix(z)
  .normalize_theta_degrees(atan2(z[, 2], z[, 1]) * 180 / pi)
}

#' @rdname circ_dist
#' @export
masked_reconstruction_loss <- function(x, x_hat, mask) {
  x <- as.matrix(x)
  x_hat <- as.matrix(x_hat)
  mask <- as.matrix(mask)

  if (!identical(dim(x), dim(x_hat)) || !identical(dim(x), dim(mask))) {
    stop("'x', 'x_hat', and 'mask' must have identical dimensions",
         call. = FALSE)
  }

  mask <- isTRUE(mask) | (is.logical(mask) & mask) | (!is.logical(mask) & mask != 0)
  diff <- x - x_hat
  diff[!is.finite(diff)] <- 0
  observed <- rowSums(mask, na.rm = TRUE)
  row_loss <- rowSums((diff^2) * mask, na.rm = TRUE) / pmax(observed, 1)
  mean(row_loss)
}

#' @rdname circ_dist
#' @export
masked_spectral_distance <- function(x,
                                     mask = is.finite(x),
                                     method = c("correlation",
                                                "spectral_angle"),
                                     min_overlap_points = 100,
                                     min_overlap_fraction = 0.25,
                                     pairs = NULL,
                                     weights = c("overlap", "uniform")) {
  method <- match.arg(method)
  weights <- match.arg(weights)
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  mask <- as.matrix(mask)

  if (!identical(dim(x), dim(mask)))
    stop("'mask' must have the same dimensions as 'x'", call. = FALSE)

  mask <- mask & is.finite(x)
  n <- nrow(x)
  p <- ncol(x)

  if (is.null(pairs)) {
    pairs <- utils::combn(seq_len(n), 2L)
    pairs <- t(pairs)
  } else {
    pairs <- as.matrix(pairs)
    if (ncol(pairs) != 2L)
      stop("'pairs' must have exactly two columns", call. = FALSE)
    storage.mode(pairs) <- "integer"
  }

  out <- data.table::data.table(
    i = as.integer(pairs[, 1]),
    k = as.integer(pairs[, 2]),
    distance = NA_real_,
    valid = FALSE,
    overlap_count = 0L,
    overlap_fraction = 0,
    weight = 0
  )

  if (!nrow(out))
    return(out)

  for (row in seq_len(nrow(out))) {
    i <- out$i[row]
    k <- out$k[row]
    if (i < 1L || i > n || k < 1L || k > n || i == k)
      next

    overlap <- mask[i, ] & mask[k, ]
    overlap_count <- sum(overlap)
    overlap_fraction <- overlap_count / p
    out$overlap_count[row] <- overlap_count
    out$overlap_fraction[row] <- overlap_fraction

    if (overlap_count < min_overlap_points ||
        overlap_fraction < min_overlap_fraction) {
      next
    }

    xi <- x[i, overlap]
    xk <- x[k, overlap]

    if (method == "correlation") {
      if (stats::sd(xi) == 0 || stats::sd(xk) == 0)
        next
      r <- stats::cor(xi, xk)
      if (!is.finite(r))
        next
      distance <- (1 - max(min(r, 1), -1)) / 2
    } else {
      denom <- sqrt(sum(xi^2)) * sqrt(sum(xk^2))
      if (!is.finite(denom) || denom <= 0)
        next
      cosang <- sum(xi * xk) / denom
      distance <- acos(max(min(cosang, 1), -1)) / pi
    }

    out$distance[row] <- distance
    out$valid[row] <- TRUE
  }

  if (weights == "overlap") {
    max_overlap <- max(out$overlap_count[out$valid], 1L)
    out$weight[out$valid] <- out$overlap_count[out$valid] / max_overlap
  } else {
    out$weight[out$valid] <- 1
  }

  out
}

.masked_spectral_distance_matrix <- function(x, mask,
                                             method = c("correlation",
                                                        "spectral_angle"),
                                             min_overlap_points = 100,
                                             min_overlap_fraction = 0.25,
                                             weights = c("overlap",
                                                         "uniform")) {
  method <- match.arg(method)
  weights <- match.arg(weights)
  n <- nrow(x)
  distance <- matrix(NA_real_, n, n)
  weight <- matrix(0, n, n)
  valid <- matrix(FALSE, n, n)
  diag(distance) <- 0

  if (n < 2L) {
    return(list(distance = distance, weight = weight, valid = valid))
  }

  pairs <- t(utils::combn(seq_len(n), 2L))
  dt <- masked_spectral_distance(
    x = x,
    mask = mask,
    method = method,
    min_overlap_points = min_overlap_points,
    min_overlap_fraction = min_overlap_fraction,
    pairs = pairs,
    weights = weights
  )

  for (row in seq_len(nrow(dt))) {
    i <- dt$i[row]
    k <- dt$k[row]
    distance[i, k] <- distance[k, i] <- dt$distance[row]
    weight[i, k] <- weight[k, i] <- dt$weight[row]
    valid[i, k] <- valid[k, i] <- dt$valid[row]
  }

  list(distance = distance, weight = weight, valid = valid)
}
