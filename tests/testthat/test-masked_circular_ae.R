make_masked_circular_test_os <- function(n = 12, p = 80) {
  wavenumber <- seq(600, 3200, length.out = p)
  theta <- seq(0, 2 * pi, length.out = n + 1)[-1]
  spectra <- vapply(theta, function(th) {
    exp(-((wavenumber - (1200 + 120 * cos(th))) / 120)^2) +
      0.8 * exp(-((wavenumber - (2100 + 180 * sin(th))) / 160)^2) +
      0.15 * cos(wavenumber / 160 + th)
  }, FUN.VALUE = numeric(length(wavenumber)))
  colnames(spectra) <- paste0("s", seq_len(n))
  metadata <- data.frame(
    sample_name = colnames(spectra),
    polymer = rep(c("a", "b"), length.out = n),
    instrument = rep(c("ftir", "raman"), length.out = n)
  )
  os <- as_OpenSpecy(wavenumber, spectra = spectra, metadata = metadata)
  os$spectra[wavenumber < 850, seq(1, n, by = 3)] <- NA_real_
  os$spectra[wavenumber > 2850, seq(2, n, by = 3)] <- NA_real_
  os$spectra[wavenumber > 1450 & wavenumber < 1700, seq(3, n, by = 3)] <- NA_real_
  os
}

test_that("input preparation preserves OpenSpecy alignment and masks missing values", {
  os <- make_masked_circular_test_os(n = 6, p = 30)
  prep <- OpenSpecy:::.prepare_masked_circular_input(os)

  expect_equal(nrow(prep$x), ncol(os$spectra))
  expect_equal(ncol(prep$x), length(os$wavenumber))
  expect_equal(prep$spectrum_id, colnames(os$spectra))
  expect_equal(dim(prep$mask_observed), dim(prep$x))
  expect_true(any(!prep$mask_observed))
  expect_true(data.table::is.data.table(prep$metadata))
})

test_that("matrix input preparation infers or accepts wavenumbers", {
  mat <- matrix(1:12, nrow = 3)
  colnames(mat) <- c("100", "200", "300", "400")
  prep <- OpenSpecy:::.prepare_masked_circular_input(mat)
  expect_equal(prep$wavenumber, c(100, 200, 300, 400))
  expect_equal(prep$spectrum_id, paste0("S", 1:3))

  prep2 <- OpenSpecy:::.prepare_masked_circular_input(mat, wavenumber = 1:4)
  expect_equal(prep2$wavenumber, 1:4)
})

test_that("mask-aware normalization uses only observed values", {
  x <- matrix(c(1, NA, 3, 5, 100, 9), nrow = 2, byrow = TRUE)
  mask <- is.finite(x)
  norm <- OpenSpecy:::.masked_circular_normalize(x, mask)

  expect_equal(norm$normalization$observed_count, c(2, 1, 2))
  expect_equal(norm$x_filled[1, 2], 0)
  expect_false(any(is.na(norm$x_filled)))

  x2 <- x
  x2[1, 2] <- 999
  norm2 <- OpenSpecy:::.masked_circular_normalize(x2, mask)
  expect_equal(norm$normalization$center, norm2$normalization$center)
})

test_that("circ_dist() handles wraparound", {
  expect_equal(circ_dist(0, 0), 0)
  expect_equal(circ_dist(0, 360), 0, tolerance = 1e-12)
  expect_equal(circ_dist(0, 180), 1, tolerance = 1e-12)
  expect_lt(circ_dist(359, 1), 0.02)
})

test_that("masked_reconstruction_loss() ignores masked positions and normalizes rows", {
  x <- matrix(c(1, 2, 100, 4, 5, 6), nrow = 2, byrow = TRUE)
  x_hat <- matrix(c(1, 4, -1000, 2, 7, 6), nrow = 2, byrow = TRUE)
  mask <- matrix(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), nrow = 2, byrow = TRUE)

  loss <- masked_reconstruction_loss(x, x_hat, mask)
  expected <- mean(c(((0^2 + 2^2) / 2), ((2^2 + 2^2 + 0^2) / 3)))
  expect_equal(loss, expected)

  x_changed <- x
  x_changed[1, 3] <- -99999
  expect_equal(masked_reconstruction_loss(x_changed, x_hat, mask), loss)
})

test_that("masked_spectral_distance() uses only valid overlap", {
  x <- matrix(c(
    1, 2, 3, NA,
    1, 2, 3, 4,
    3, 2, 1, 4,
    5, 5, 5, 5
  ), nrow = 4, byrow = TRUE)
  mask <- is.finite(x)

  d <- masked_spectral_distance(
    x, mask,
    method = "correlation",
    min_overlap_points = 3,
    min_overlap_fraction = 0.5,
    pairs = matrix(c(1, 2, 1, 3, 1, 4), ncol = 2, byrow = TRUE)
  )

  expect_true(d$valid[1])
  expect_equal(d$distance[1], 0, tolerance = 1e-12)
  expect_true(d$valid[2])
  expect_equal(d$distance[2], 1, tolerance = 1e-12)
  expect_false(d$valid[3])
  expect_true(all(d$distance[d$valid] >= 0 & d$distance[d$valid] <= 1))

  low <- masked_spectral_distance(
    x, mask,
    min_overlap_points = 4,
    min_overlap_fraction = 1,
    pairs = matrix(c(1, 2), ncol = 2)
  )
  expect_false(low$valid[1])
})

test_that("spectral angle distance is bounded", {
  x <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
  d <- masked_spectral_distance(
    x,
    method = "spectral_angle",
    min_overlap_points = 2,
    min_overlap_fraction = 0.5
  )
  expect_true(d$valid[1])
  expect_equal(d$distance[1], 0.5, tolerance = 1e-12)
})

test_that("validation split ignores metadata labels", {
  a <- OpenSpecy:::.masked_circular_validation_split(10, 0.3, seed = 42)
  b <- OpenSpecy:::.masked_circular_validation_split(10, 0.3, seed = 42)
  expect_equal(a, b)
  expect_equal(length(a$validation), 3)
})

test_that("fitting ignores class-like metadata values", {
  os <- make_masked_circular_test_os(n = 6, p = 30)
  changed <- os
  changed$metadata$polymer <- rev(changed$metadata$polymer)
  changed$metadata$instrument <- paste0("instrument_", seq_len(nrow(changed$metadata)))

  args <- list(
    min_overlap_points = 4,
    min_overlap_fraction = 0.1,
    validation_fraction = 0,
    decoder_degree = 1,
    verbose = FALSE,
    seed = 2
  )
  model_a <- do.call(fit_masked_circular_ae, c(list(x = os), args))
  model_b <- do.call(fit_masked_circular_ae, c(list(x = changed), args))

  expect_equal(model_a$history, model_b$history)
  expect_equal(model_a$state$reference_z, model_b$state$reference_z,
               tolerance = 1e-12)
})

test_that("plot data preparation joins metadata after training only", {
  encoded <- data.table::data.table(
    spectrum_id = c("a", "b"),
    theta = c(0, 180)
  )
  metadata <- data.frame(sample_name = c("a", "b"), polymer = c("x", "y"))
  dt <- OpenSpecy:::.prepare_circular_embedding_plot_data(
    encoded,
    color_by = "polymer",
    metadata = metadata
  )
  expect_equal(dt$polymer, c("x", "y"))
  expect_error(
    OpenSpecy:::.prepare_circular_embedding_plot_data(encoded, color_by = "nope"),
    "not a column"
  )
})

test_that("plot_circular_embedding() uses base graphics and returns plot data", {
  encoded <- data.table::data.table(
    spectrum_id = c("a", "b"),
    theta = c(0, 180),
    group = c("x", "y")
  )
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  p <- plot_circular_embedding(encoded, color_by = "group")
  expect_s3_class(p, "data.table")
  expect_equal(p$theta, encoded$theta)
})

test_that("tiny R-native model fits, encodes, reconstructs, and diagnoses", {
  #os <- make_masked_circular_test_os(n = 8, p = 40)

  model <- fit_masked_circular_ae(
    test_lib,
    min_overlap_points = 5,
    min_overlap_fraction = 0.1,
    validation_fraction = 0.25,
    decoder_degree = 2,
    verbose = FALSE,
    seed = 1
  )

  expect_s3_class(model, "MaskedCircularAEModel")
  expect_equal(model$model_type, "masked_circular_ae")
  expect_equal(model$backend$name, "stats")
  expect_true(nrow(model$history) >= 1)
  expect_false("polymer" %in% names(model))

  encoded <- encode_masked_circular_ae(model, test_lib)
  expect_true(all(is.finite(encoded$theta)))
  expect_true(all(encoded$theta >= 0 & encoded$theta < 360))
  expect_named(encoded, c("spectrum_id", "theta", "observed_points",
                         "min_wavenumber", "max_wavenumber"))

  specs <- encode_masked_circular_ae(model, test_lib, as_specs = TRUE)
  expect_s3_class(specs, "Specs")
  expect_equal(specs$variables, "theta")
  expect_equal(nrow(specs$values), 1)

  rec <- reconstruct_masked_circular_ae(model, x = test_lib)
  expect_s3_class(rec, "OpenSpecy")
  expect_equal(rec$wavenumber, test_lib$wavenumber)

  rec_theta <- reconstruct_masked_circular_ae(model, theta = encoded$theta)
  expect_s3_class(rec_theta, "OpenSpecy")
  expect_equal(ncol(rec_theta$spectra), nrow(encoded))

  diag <- diagnose_masked_circular_ae(model, test_lib, encoded = encoded,
                                      n_pairs = 10, k = 2)
  expect_named(diag, c("reconstruction", "distance_preservation",
                      "neighbor_preservation", "missingness_range",
                      "angular_spread", "note"))
})
