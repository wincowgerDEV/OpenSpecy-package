test_that("automate_particle_analysis() returns details and summaries", {
  wn <- seq(750, 1800, length.out = 40)
  pe <- sin(wn / 120) + 1
  mineral <- cos(wn / 130) + 1
  lib <- as_OpenSpecy(
    wn,
    spectra = cbind(pe = pe, mineral = mineral),
    metadata = data.frame(
      x = 0:1,
      y = 0,
      sample_name = c("pe", "mineral"),
      material_class = c("poly(ethylene)", "mineral")
    )
  )
  map <- as_OpenSpecy(
    wn,
    spectra = cbind(pe + 0.01, pe + 0.02, mineral + 0.01, rep(0, length(wn))),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )

  res <- automate_particle_analysis(
    map,
    lib,
    sn_threshold_min = 0.001,
    area_threshold = 0,
    outputs = c("details", "summary"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  expect_type(res, "list")
  expect_s3_class(res$particle_details_all_csv, "data.table")
  expect_s3_class(res$particle_summary_all_csv, "data.table")
  expect_gt(nrow(res$particle_details_all_csv), 0)
  expect_named(
    res$samples[[1]],
    c("sample_id", "particle_details_csv", "particle_summary_csv",
      "particles_raw_rds", "particles_rds", "particle_image_png",
      "particle_heatmap_png", "particle_heatmap_thresholded_jpg",
      "cor_heatmap_png", "time_rds")
  )
})

test_that("automate_particle_analysis() rejects removed legacy arguments", {
  wn <- 1:5
  os <- as_OpenSpecy(wn, spectra = matrix(seq_len(10), nrow = 5))
  expect_error(
    automate_particle_analysis(os, os, adj_map_baseline = TRUE),
    "Removed automate_particle_analysis"
  )
})

test_that("automate_particle_analysis() smooths in-memory maps when requested", {
  coords <- expand.grid(x = 0:2, y = 0:2)
  spectra <- matrix(0, nrow = 5, ncol = nrow(coords),
                    dimnames = list(NULL, paste0("cell_", seq_len(nrow(coords)))))
  spectra[3, 5] <- 100
  map <- as_OpenSpecy(1:5, spectra = spectra, metadata = coords)

  raw <- .read_particle_sample(map, spectral_smooth = FALSE,
                               sigma = c(1, 1, 1))
  smoothed <- .read_particle_sample(map, spectral_smooth = TRUE,
                                    sigma = c(1, 1, 1))

  expect_equal(dim(smoothed$spectra), dim(raw$spectra))
  expect_false(isTRUE(all.equal(smoothed$spectra, raw$spectra)))
})

test_that("automate_particle_analysis() keeps all-cell coordinates and visual colors", {
  wn <- seq(750, 2200, length.out = 30)
  map_wn <- wn + 0.37
  pe <- sin(wn / 100) + 1
  pp <- cos(wn / 90) + 1
  map_pe <- sin(map_wn / 100) + 1
  map_pp <- cos(map_wn / 90) + 1
  lib <- as_OpenSpecy(
    wn,
    spectra = cbind(pe = pe, pp = pp),
    metadata = data.frame(
      x = 0:1,
      y = 0,
      sample_name = c("pe", "pp"),
      material_class = c("poly(ethylene)", "poly(propylene)")
    )
  )
  map <- as_OpenSpecy(
    map_wn,
    spectra = stats::setNames(
      data.frame(map_pe, map_pp, map_pe + 0.01, map_pp + 0.01),
      paste0("cell_", 1:4)
    ),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  img <- array(1, dim = c(4, 4, 3))
  img[, , 1] <- 0.2
  img[, , 2] <- 0.4
  img[, , 3] <- 0.6
  map <- add_visual_image(map, img, bottom_left = c(1, 4),
                          top_right = c(4, 1))

  res <- automate_particle_analysis(
    map,
    lib,
    material_col = "material_class",
    particle_id_strategy = "all_cell_id",
    sn_threshold_min = -Inf,
    sn_threshold_max = Inf,
    area_threshold = 0,
    outputs = c("details", "summary", "processed"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  expect_gt(nrow(res$particle_details_all_csv), 0)
  expect_contains(names(res$particle_details_all_csv), c("r", "g", "b"))
  expect_true(all(stats::complete.cases(res$particle_details_all_csv[, c("r", "g", "b")])))
  expect_false(any(grepl("\\.x$|\\.y$", names(res$samples[[1]]$particles_rds$metadata))))
  expect_true(all(c("x", "y") %in% names(res$samples[[1]]$particles_rds$metadata)))
  expect_true(all(res$samples[[1]]$particles_rds$wavenumber %in% lib$wavenumber))
})

test_that("automate_particle_analysis() returns and writes image outputs", {
  wn <- seq(750, 1800, length.out = 40)
  pe <- sin(wn / 120) + 1
  mineral <- cos(wn / 130) + 1
  lib <- as_OpenSpecy(
    wn,
    spectra = cbind(pe = pe, mineral = mineral),
    metadata = data.frame(
      sample_name = c("pe", "mineral"),
      material_class = c("poly(ethylene)", "mineral")
    )
  )
  map <- as_OpenSpecy(
    wn,
    spectra = cbind(pe + 0.01, pe + 0.02, mineral + 0.01, rep(0, length(wn))),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  out_dir <- tempfile("apa-images-")

  res <- automate_particle_analysis(
    list(small = map),
    lib,
    output_dir = out_dir,
    sn_threshold_min = 0.001,
    area_threshold = 0,
    outputs = c("details", "summary", "raw", "processed",
                "particle_image", "heatmap", "thresholded", "correlation"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  sample <- res$samples$small
  expect_s3_class(sample$particle_image_png, "recordedplot")
  expect_s3_class(sample$particle_heatmap_png, "recordedplot")
  expect_s3_class(sample$particle_heatmap_thresholded_jpg, "recordedplot")
  expect_s3_class(sample$cor_heatmap_png, "recordedplot")
  expect_true(file.exists(file.path(out_dir, "particle_image_small.png")))
  expect_true(file.exists(file.path(out_dir, "particle_heatmap_small.png")))
  expect_true(file.exists(file.path(out_dir,
                                   "particle_heatmap_thresholdedsmall.jpg")))
  expect_true(file.exists(file.path(out_dir, "cor_heatmap_small.png")))
})
