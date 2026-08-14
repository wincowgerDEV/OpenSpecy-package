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
  expect_s3_class(res, "OpenSpecyParticleAnalysis")
  expect_s3_class(res$particle_details_all_csv, "data.table")
  expect_s3_class(res$particle_summary_all_csv, "data.table")
  expect_gt(nrow(res$particle_details_all_csv), 0)
  expect_named(
    res$samples[[1]],
    c("sample_id", "particle_details_csv", "particle_summary_csv",
      "particles_raw_rds", "particles_rds", "particle_image",
      "particle_heatmap", "particle_heatmap_thresholded",
      "cor_heatmap", "sn_histogram", "cor_histogram",
      "time_rds")
  )
})

test_that(".normalize_particle_samples() expands a file-path vector", {
  normalized <- OpenSpecy:::.normalize_particle_samples(c("a.h5", "b.h5"))
  expect_type(normalized, "list")
  expect_length(normalized, 2L)
  expect_identical(normalized[[1]], "a.h5")
  expect_identical(normalized[[2]], "b.h5")
  expect_identical(names(normalized), c("a", "b"))

  single <- OpenSpecy:::.normalize_particle_samples("a.h5")
  expect_length(single, 1L)
  expect_identical(single[[1]], "a.h5")

  object <- as_OpenSpecy(1:2, spectra = data.frame(a = 1:2))
  wrapped <- OpenSpecy:::.normalize_particle_samples(object)
  expect_length(wrapped, 1L)
  expect_identical(wrapped[[1]], object)
})

test_that("automate_particle_analysis() reads a vector of file paths one at a time", {
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

  paths <- c(
    tempfile("particle-sample-a-", fileext = ".rds"),
    tempfile("particle-sample-b-", fileext = ".rds")
  )
  on.exit(unlink(paths), add = TRUE)
  write_spec(map, paths[[1L]])
  write_spec(map, paths[[2L]])

  res <- automate_particle_analysis(
    paths,
    lib,
    sn_threshold_min = 0.001,
    area_threshold = 0,
    outputs = c("details", "summary"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  expect_s3_class(res, "OpenSpecyParticleAnalysis")
  expect_length(res$samples, 2L)
  expect_identical(
    names(res$samples),
    tools::file_path_sans_ext(basename(paths))
  )
  expect_gt(nrow(res$particle_details_all_csv), 0)
  expect_setequal(
    unique(res$particle_details_all_csv$sample_id), names(res$samples)
  )
})

test_that("automate_particle_analysis() rejects removed legacy arguments", {
  wn <- 1:5
  os <- as_OpenSpecy(wn, spectra = matrix(seq_len(10), nrow = 5))
  expect_error(
    automate_particle_analysis(os, os, adj_map_baseline = TRUE),
    "Removed automate_particle_analysis"
  )
  expect_error(
    automate_particle_analysis(os, os, spatial_smooth = TRUE),
    "spatial_smooth"
  )
  expect_error(
    automate_particle_analysis(os, os, top_n = 2L),
    "top_n"
  )
  expect_error(
    automate_particle_analysis(
      os, os, sn_threshold_min = 2, sn_threshold_max = 1
    ),
    "sn_threshold_min"
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

test_that("automate_particle_analysis() explains all-pixel thresholds", {
  map <- as_OpenSpecy(
    c(800, 1200, 2500, 3000),
    spectra = matrix(rep(c(1, 3, 2, 4), 4), ncol = 4),
    metadata = expand.grid(x = 0:1, y = 0:1)
  )
  expect_error(
    automate_particle_analysis(
      map, map, sn_threshold_min = -Inf, sn_threshold_max = Inf,
      metric = "tot_sig"
    ),
    "retained every map pixel"
  )
})

test_that("particle preprocessing preserves an explicit target axis", {
  source <- as_OpenSpecy(
    seq(700, 1300, by = 10),
    spectra = matrix(seq_len(61), ncol = 1)
  )
  target <- c(721.2, 805.8, 934.6, 1102.1, 1277.7)
  library <- as_OpenSpecy(target, spectra = matrix(seq_along(target), ncol = 1))
  processed <- OpenSpecy:::.process_for_particle_match(
    source, library,
    list(
      conform_spec = TRUE,
      conform_spec_args = list(range = target, res = NULL, type = "roll"),
      restrict_range = FALSE, smooth_intens = FALSE, make_rel = FALSE
    )
  )
  expect_identical(processed$wavenumber, target)
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
                "particle_image", "heatmap", "thresholded", "correlation",
                "sn_histogram", "cor_histogram"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  sample <- res$samples$small
  expect_identical(sample$particle_image$type, "heatmap_categorical")
  expect_identical(sample$particle_heatmap$type, "heatmap")
  expect_identical(sample$particle_heatmap_thresholded$type, "heatmap_binary")
  expect_identical(sample$cor_heatmap$type, "heatmap_categorical")
  expect_identical(sample$sn_histogram$type, "histogram")
  expect_identical(sample$cor_histogram$type, "histogram")
  expect_true(all(c("x", "y", "z") %in% names(sample$particle_heatmap)))
  expect_equal(sample$sn_histogram$thresholds, 0.001)
  expect_equal(sample$cor_histogram$thresholds, 0.7)
  expect_true(file.exists(file.path(out_dir, "particle_image_small.png")))
  expect_true(file.exists(file.path(out_dir, "particle_heatmap_small.png")))
  expect_true(file.exists(file.path(out_dir,
                                   "particle_heatmap_thresholdedsmall.jpg")))
  expect_true(file.exists(file.path(out_dir, "cor_heatmap_small.png")))
  expect_true(file.exists(file.path(out_dir, "sn_histogram_small.png")))
  expect_true(file.exists(file.path(out_dir, "cor_histogram_small.png")))

  replay <- tempfile(fileext = ".png")
  grDevices::png(replay, width = 600, height = 600)
  grDevices::dev.control(displaylist = "enable")
  expect_invisible(plot(res, sample = "small", which = "sn_histogram"))
  grDevices::dev.off()
  expect_gt(file.info(replay)$size, 0)
})

test_that("particle heatmap scale includes the finite endpoints", {
  scale <- .particle_continuous_scale(c(NA, -2.5, 0, 7.25, Inf))
  expect_equal(scale$range, c(-2.5, 7.25))
  expect_equal(range(scale$ticks), scale$range)
})
test_that("removed particle arguments are absent from the public methods", {
  for (fun in list(
    automate_particle_analysis,
    OpenSpecy:::automate_particle_analysis.default,
    OpenSpecy:::automate_particle_analysis.FileSpecs
  )) {
    argument_names <- names(formals(fun))
    expect_identical(tail(argument_names, 2L), c("specs_centers", "..."))
    expect_false(any(c("spatial_smooth", "top_n") %in% argument_names))
  }
})

particle_partition_fixture <- function() {
  coords <- expand.grid(x = 0:4, y = 0:1)
  family_a <- c(1, 2, 5, 2, 1, 3)
  family_b <- c(5, 2, 1, 2, 5, 1)
  family <- rep(c("a", "b", "a", "a", "b"), 2)
  spectra <- vapply(family, function(value) {
    if (identical(value, "a")) family_a else family_b
  }, numeric(length(family_a)))
  colnames(spectra) <- paste0("pixel_", seq_len(nrow(coords)))
  map <- as_OpenSpecy(
    seq(800, 1300, length.out = nrow(spectra)), spectra = spectra,
    metadata = coords
  )
  attr(map, "partition_fixture") <- "preserve-me"
  list(
    map = map,
    eligible = coords$x != 2,
    family = family
  )
}

test_that("connected particle partitions return complete stable membership", {
  fixture <- particle_partition_fixture()
  result <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "collapse",
    collapse_function = "mean",
    area_threshold = 4
  )

  expect_s3_class(result$analysis_units, "OpenSpecy")
  expect_s3_class(result$pixel_to_unit, "data.table")
  expect_identical(result$pixel_to_unit$pixel_index, seq_len(10L))
  expect_identical(result$pixel_to_unit$pixel_id,
                   colnames(fixture$map$spectra))
  expect_identical(result$pixel_to_unit$eligible, fixture$eligible)
  expect_equal(data.table::uniqueN(na.omit(result$pixel_to_unit$region_id)), 2L)
  expect_equal(data.table::uniqueN(na.omit(result$pixel_to_unit$unit_id)), 2L)
  expect_true(all(result$pixel_to_unit$area[fixture$eligible] == 4L))
  expect_true(all(result$pixel_to_unit$kept[fixture$eligible]))
  expect_true(all(is.na(result$pixel_to_unit$unit_id[!fixture$eligible])))
  expect_true(all(result$pixel_to_unit$rejection_reason[!fixture$eligible] ==
                    "threshold"))
  expect_identical(colnames(result$analysis_units$spectra),
                   result$analysis_units$metadata$unit_id)
  expect_equal(ncol(result$analysis_units$spectra),
               nrow(result$analysis_units$metadata))
  expect_identical(attr(result$analysis_units, "partition_fixture"),
                   "preserve-me")
  expect_identical(colnames(result$analysis_units$spectra),
                   result$analysis_units$metadata$col_id)
  expect_identical(result$analysis_units$metadata$col_id,
                   result$analysis_units$metadata$unit_id)
})

test_that("particle partitions scope repeated coordinates to source maps", {
  wave <- seq(800, 1300, length.out = 6)
  family_a <- c(1, 2, 5, 2, 1, 3)
  family_b <- c(5, 2, 1, 2, 5, 1)
  spectra <- cbind(family_a, family_a, family_b, family_b)
  colnames(spectra) <- paste0("pixel_", seq_len(ncol(spectra)))
  map <- as_OpenSpecy(
    wave,
    spectra = spectra,
    metadata = data.frame(
      x = c(0, 1, 0, 1), y = c(0, 0, 0, 0),
      file_id = c("map-a", "map-a", "map-b", "map-b"),
      file_name = c("a.h5", "a.h5", "b.h5", "b.h5"),
      spectrum_identity = paste0("pixel-identity-", seq_len(4)),
      snr = c(1, 3, 2, 6), max_cor_val = c(0.2, 0.6, 0.3, 0.9),
      r = c(3, 4, 5, 12), g = c(0, 0, 6, 8), b = c(1, 1, 2, 2),
      mean_snr = 999, mean_cor = 999,
      mean_r = 999L, mean_g = 999L, mean_b = 999L,
      perimeter = rep(999, 4), feret_max = rep(999, 4),
      convex_hull_area = rep(999, 4), first_x = rep(999, 4)
    )
  )
  attr(map, "source_fixture") <- "preserve-me"

  connected <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", collapse_function = "mean",
    area_threshold = 1
  )
  expect_equal(data.table::uniqueN(connected$pixel_to_unit$source_id), 2L)
  expect_equal(data.table::uniqueN(connected$pixel_to_unit$region_id), 2L)
  expect_equal(data.table::uniqueN(connected$pixel_to_unit$unit_id), 2L)
  expect_true(all(connected$pixel_to_unit$area == 2L))
  expect_true(all(
    connected$pixel_to_unit[kept == TRUE,
      data.table::uniqueN(source_id), by = unit_id]$V1 == 1L
  ))
  expect_identical(colnames(connected$analysis_units$spectra),
                   connected$analysis_units$metadata$col_id)
  expect_identical(connected$analysis_units$metadata$col_id,
                   connected$analysis_units$metadata$unit_id)
  expect_setequal(connected$analysis_units$metadata$file_id,
                  c("map-a", "map-b"))
  expect_setequal(connected$analysis_units$metadata$file_name,
                  c("a.h5", "b.h5"))
  expect_true(all(is.na(connected$analysis_units$metadata$spectrum_identity)))
  expect_equal(connected$analysis_units$metadata$x, c(0.5, 0.5))
  expect_equal(connected$analysis_units$metadata$y, c(0, 0))
  expect_equal(connected$analysis_units$metadata$perimeter, c(2, 2))
  expect_equal(connected$analysis_units$metadata$feret_max, c(2, 2))
  expect_equal(connected$analysis_units$metadata$feret_min, c(1, 1))
  expect_equal(connected$analysis_units$metadata$convex_hull_area, c(0, 0))
  expect_equal(connected$analysis_units$metadata$first_x, c(0, 0))
  expect_equal(connected$analysis_units$metadata$mean_snr, c(2, 4))
  expect_equal(connected$analysis_units$metadata$mean_cor, c(0.4, 0.6))
  expect_equal(connected$analysis_units$metadata$mean_r, c(3, 9))
  expect_equal(connected$analysis_units$metadata$mean_g, c(0, 7))
  expect_equal(connected$analysis_units$metadata$mean_b, c(1, 2))
  expect_identical(attr(connected$analysis_units, "source_fixture"),
                   "preserve-me")

  nonspatial <- OpenSpecy:::.partition_particle_map(
    map, strategy = "nonspatial_collapse", pca_components = 3,
    centers = 10, collapse_function = "mean", area_threshold = 1
  )
  expect_equal(data.table::uniqueN(nonspatial$pixel_to_unit$unit_id), 2L)
  expect_true(all(
    nonspatial$pixel_to_unit[kept == TRUE,
      data.table::uniqueN(source_id), by = unit_id]$V1 == 1L
  ))
  expect_setequal(nonspatial$analysis_units$metadata$file_id,
                  c("map-a", "map-b"))
  expect_identical(colnames(nonspatial$analysis_units$spectra),
                   nonspatial$analysis_units$metadata$col_id)
  expect_equal(nonspatial$analysis_units$metadata$mean_snr, c(2, 4))
  expect_equal(nonspatial$analysis_units$metadata$mean_cor, c(0.4, 0.6))
  expect_equal(nonspatial$analysis_units$metadata$mean_r, c(3, 9))
  expect_equal(nonspatial$analysis_units$metadata$mean_g, c(0, 7))
  expect_equal(nonspatial$analysis_units$metadata$mean_b, c(1, 2))
  expect_false(any(c("perimeter", "feret_min", "feret_max",
                     "convex_hull_area", "first_x", "first_y") %in%
                   names(nonspatial$analysis_units$metadata)))
})

test_that("H5 regions scope repeated coordinates within one file", {
  spectra <- matrix(
    rep(seq_len(6), 4), nrow = 6,
    dimnames = list(NULL, paste0("region_pixel_", seq_len(4)))
  )
  map <- as_OpenSpecy(
    seq(800, 1300, length.out = 6),
    spectra = spectra,
    metadata = data.frame(
      x = c(0, 1, 0, 1), y = 0,
      file_id = "shared-file", file_name = "map.h5",
      region = c("region-a", "region-a", "region-b", "region-b")
    )
  )

  result <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", collapse_function = "mean",
    area_threshold = 1
  )
  expect_equal(data.table::uniqueN(result$pixel_to_unit$source_id), 2L)
  expect_equal(data.table::uniqueN(result$pixel_to_unit$unit_id), 2L)
  expect_setequal(result$analysis_units$metadata$region,
                  c("region-a", "region-b"))
  expect_true(all(
    result$pixel_to_unit[kept == TRUE,
      data.table::uniqueN(source_id), by = unit_id]$V1 == 1L
  ))
  expect_identical(colnames(result$analysis_units$spectra),
                   result$analysis_units$metadata$col_id)
})

test_that("mixed uploads encode missing source fields row by row", {
  spectra <- matrix(
    rep(seq_len(6), 6), nrow = 6,
    dimnames = list(NULL, paste0("mixed_pixel_", seq_len(6)))
  )
  map <- as_OpenSpecy(
    seq(800, 1300, length.out = 6),
    spectra = spectra,
    metadata = data.frame(
      x = rep(c(0, 1), 3), y = 0,
      file_id = c(rep("h5-file", 4), rep("csv-file", 2)),
      file_name = c(rep("map.h5", 4), rep("map.csv", 2)),
      region = c("region-a", "region-a", "region-b", "region-b", NA, NA)
    )
  )

  result <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", collapse_function = "mean",
    area_threshold = 1
  )
  expect_equal(data.table::uniqueN(result$pixel_to_unit$source_id), 3L)
  expect_equal(data.table::uniqueN(result$pixel_to_unit$unit_id), 3L)
  expect_true(all(
    result$pixel_to_unit[kept == TRUE,
      data.table::uniqueN(source_id), by = unit_id]$V1 == 1L
  ))
  expect_setequal(na.omit(result$analysis_units$metadata$region),
                  c("region-a", "region-b"))
  expect_equal(sum(is.na(result$analysis_units$metadata$region)), 1L)
  expect_identical(colnames(result$analysis_units$spectra),
                   result$analysis_units$metadata$col_id)
  expect_identical(result$analysis_units$metadata$col_id,
                   result$analysis_units$metadata$unit_id)
})

test_that("connected collapse recomputes geometry used by particle details", {
  spectra <- matrix(
    rep(seq_len(6), 4), nrow = 6,
    dimnames = list(NULL, paste0("square_pixel_", seq_len(4)))
  )
  map <- as_OpenSpecy(
    seq(800, 1300, length.out = 6),
    spectra = spectra,
    metadata = data.frame(
      x = c(0, 1, 0, 1), y = c(0, 0, 1, 1),
      perimeter = 999, feret_min = 999, feret_max = 999,
      convex_hull_area = 999, first_x = 999, first_y = 999
    )
  )

  result <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", collapse_function = "mean",
    area_threshold = 1
  )
  md <- result$analysis_units$metadata
  expected_max <- sqrt(2) + 1
  expect_equal(md$area, 4L)
  expect_equal(md$centroid_x, 0.5)
  expect_equal(md$centroid_y, 0.5)
  expect_equal(md$first_x, 0)
  expect_equal(md$first_y, 0)
  expect_equal(md$perimeter, 4)
  expect_equal(md$feret_max, expected_max)
  expect_equal(md$feret_min, 4 / expected_max)
  expect_equal(md$convex_hull_area, 1)

  details <- OpenSpecy:::.particle_details_table(
    result$analysis_units, "sample", "material_class",
    cor_threshold = 0.7, pixel_length = 2, origin = c(10, 20)
  )
  expect_equal(details$area_um2, 16)
  expect_equal(details$perimeter_um, 8)
  expect_equal(details$max_length_um, 2 * expected_max)
  expect_equal(details$min_length_um, 8 / expected_max)
  expect_equal(details$aspect_ratio, expected_max^2 / 4)
  expect_equal(details$circularity, 1 / pi)
  expect_equal(details$centroid_x, 11)
  expect_equal(details$centroid_y, 21)
  expect_equal(details$first_x, 10)
  expect_equal(details$first_y, 20)
})

test_that("many particle units share one stable membership index", {
  n_units <- 400L
  unit_id <- rep(sprintf("unit_%06d", seq_len(n_units)), each = 2L)
  n_pixels <- length(unit_id)
  spectra <- outer(seq_len(4L), seq_len(n_pixels), `+`)
  colnames(spectra) <- paste0("bulk_pixel_", seq_len(n_pixels))
  display <- as_OpenSpecy(
    seq(800, 1100, length.out = nrow(spectra)),
    spectra = spectra,
    metadata = data.frame(
      x = seq_len(n_pixels) - 1L, y = 0,
      unit_id = unit_id,
      constant_tag = unit_id,
      varying_tag = rep(c("left", "right"), n_units),
      snr = rep(seq_len(n_units), each = 2L) + rep(c(0, 1), n_units),
      max_cor_val = rep(seq_len(n_units) / n_units, each = 2L),
      r = rep(c(3, 4), n_units),
      g = rep(c(0, 0), n_units),
      b = rep(c(1, 1), n_units)
    )
  )
  mapping <- data.table::data.table(
    kept = TRUE, unit_id = unit_id, source_id = "bulk-map",
    x = seq_len(n_pixels) - 1L, y = 0,
    cluster_id = NA_character_
  )

  rows <- OpenSpecy:::.particle_membership_rows(unit_id, unique(unit_id))
  expect_length(rows, n_units)
  expect_identical(lengths(rows), rep(2L, n_units))
  expect_identical(unlist(rows, use.names = FALSE), seq_len(n_pixels))

  collapsed <- OpenSpecy:::.collapse_particle_units(
    display, mapping, base::mean, geometric = FALSE
  )
  expected_spectra <- vapply(seq_len(n_units), function(i) {
    rowMeans(spectra[, unit_id == sprintf("unit_%06d", i), drop = FALSE])
  }, numeric(nrow(spectra)))
  expect_equal(unname(collapsed$spectra), unname(expected_spectra))
  expect_identical(colnames(collapsed$spectra), unique(unit_id))
  expect_identical(collapsed$metadata$constant_tag, unique(unit_id))
  expect_true(all(is.na(collapsed$metadata$varying_tag)))
  expect_equal(collapsed$metadata$mean_snr, seq_len(n_units) + 0.5)
  expect_equal(collapsed$metadata$mean_cor, seq_len(n_units) / n_units)
  expect_equal(collapsed$metadata$mean_r, rep(3, n_units))
  expect_equal(collapsed$metadata$mean_g, rep(0, n_units))
  expect_equal(collapsed$metadata$mean_b, rep(1, n_units))
  expect_identical(collapsed$metadata$area, rep(2L, n_units))
  expect_equal(collapsed$metadata$perimeter, rep(2, n_units))
})

test_that("split-based spectral reducers match collapse_spec semantics", {
  spectra <- matrix(
    c(1, 3, 2, 4, 5, 7,
      2, 4, 3, 5, NA, 8,
      4, 8, 6, 10, 12, 14),
    nrow = 3L, byrow = TRUE,
    dimnames = list(NULL, paste0("reducer_pixel_", seq_len(6L)))
  )
  ids <- rep(c("unit_a", "unit_b", "unit_c"), each = 2L)
  map <- as_OpenSpecy(
    c(800, 900, 1000), spectra = spectra,
    metadata = data.frame(unit_id = ids)
  )
  rows <- OpenSpecy:::.particle_membership_rows(ids, unique(ids))
  reducers <- list(
    mean = base::mean,
    median = stats::median,
    sum = base::sum,
    custom = function(x) max(x) - min(x)
  )

  for (FUN in reducers) {
    expected <- collapse_spec(map, fun = FUN, column = "unit_id")$spectra
    actual <- OpenSpecy:::.particle_reduce_unit_spectra(
      map$spectra, rows, FUN
    )
    expect_equal(actual, unname(expected), ignore_attr = TRUE)
  }
})

test_that("visual RGB is registered once without feature relabeling", {
  spectra <- matrix(
    rep(seq_len(6), 4), nrow = 6,
    dimnames = list(NULL, paste0("visual_pixel_", seq_len(4)))
  )
  map <- as_OpenSpecy(
    seq(800, 1300, length.out = 6), spectra = spectra,
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  image <- array(0, dim = c(4, 4, 3))
  image[, , 1] <- 0.2
  image[, , 2] <- 0.4
  image[, , 3] <- 0.6
  map <- add_visual_image(map, image, bottom_left = c(1, 4),
                          top_right = c(4, 1))
  feature_calls <- 0L
  local_mocked_bindings(
    def_features = function(...) {
      feature_calls <<- feature_calls + 1L
      stop("legacy feature relabeling should not run")
    },
    .package = "OpenSpecy"
  )

  result <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", collapse_function = "mean",
    area_threshold = 1
  )
  expect_identical(feature_calls, 0L)
  expect_equal(result$display$metadata$r, rep(51L, 4L))
  expect_equal(result$display$metadata$g, rep(102L, 4L))
  expect_equal(result$display$metadata$b, rep(153L, 4L))
  expect_equal(result$analysis_units$metadata$mean_r, 51)
  expect_equal(result$analysis_units$metadata$mean_g, 102)
  expect_equal(result$analysis_units$metadata$mean_b, 153)
})

test_that("many K-means groups retain first-seen order and row alignment", {
  groups <- sprintf("group_%03d", c(80:1))
  grouping <- rep(groups, each = 4L)
  scores <- cbind(
    rep(c(-2, -1.9, 2, 2.1), length(groups)),
    rep(c(0, 0.1, 0, 0.1), length(groups))
  )
  clustered <- OpenSpecy:::.particle_kmeans_groups(
    scores, pixel_index = seq_len(nrow(scores)), grouping = grouping,
    centers = 2L, seed = 17L
  )

  expect_identical(names(clustered$effective_centers), groups)
  expect_identical(unname(clustered$effective_centers),
                   rep(2L, length(groups)))
  expect_true(all(vapply(split(clustered$cluster_id, grouping), function(x) {
    identical(unique(x), c("cluster_000001", "cluster_000002"))
  }, logical(1))))
})

test_that("public clustering steps are explicit rather than silently ignored", {
  expect_invisible(OpenSpecy:::.validate_particle_specs_steps(
    c("pca", "kmeans")
  ))
  expect_error(
    OpenSpecy:::.validate_particle_specs_steps("kmeans"),
    "require c\\(\\\"pca\\\", \\\"kmeans\\\"\\)"
  )
  expect_error(
    OpenSpecy:::.validate_particle_specs_steps(c("kmeans", "pca")),
    "retained for compatibility"
  )
})

test_that("material identities define connected regions without matching", {
  fixture <- particle_partition_fixture()
  material <- ifelse(fixture$family == "a", "polymer-a", "polymer-b")
  material[[1L]] <- NA_character_
  result <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "collapse",
    material = material,
    collapse_function = stats::median,
    area_threshold = 1
  )

  expect_false(result$pixel_to_unit$eligible[[1L]])
  expect_identical(result$pixel_to_unit$rejection_reason[[1L]],
                   "missing material identity")
  kept <- result$pixel_to_unit[result$pixel_to_unit$kept]
  expect_true(all(vapply(split(kept$material, kept$unit_id), function(x) {
    length(unique(x)) == 1L
  }, logical(1))))
  expect_false(any(grepl("polymer", kept$unit_id, fixed = TRUE)))
})

test_that("spatial-cluster PCA/K-means is source-scoped and deterministic", {
  fixture <- particle_partition_fixture()
  set.seed(812)
  seed_before <- .Random.seed
  first <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "partial_collapse",
    pca_components = 100,
    centers = 100,
    collapse_function = "mean",
    area_threshold = 1,
    seed = 44
  )
  seed_after <- .Random.seed
  second <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "partial_collapse",
    pca_components = 100,
    centers = 100,
    collapse_function = "mean",
    area_threshold = 1,
    seed = 44
  )

  expect_identical(seed_after, seed_before)
  expect_identical(first$pixel_to_unit, second$pixel_to_unit)
  expect_equal(first$settings$pca_components, 6L)
  expect_equal(unname(first$settings$centers), 2L)
  expect_equal(data.table::uniqueN(na.omit(first$pixel_to_unit$region_id)), 0L)
  expect_equal(data.table::uniqueN(na.omit(first$pixel_to_unit$unit_id)), 2L)
  expect_equal(ncol(first$analysis_units$spectra), 2L)
  by_unit <- first$pixel_to_unit[first$pixel_to_unit$kept,
                                 .(families = data.table::uniqueN(
                                   fixture$family[pixel_index]
                                 )),
                                 by = unit_id]
  expect_true(all(by_unit$families == 1L))
})

test_that("nonspatial PCA/K-means clusters globally with stable mapping", {
  fixture <- particle_partition_fixture()
  first <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "nonspatial_collapse",
    pca_components = 3,
    centers = 10,
    collapse_function = "median",
    area_threshold = 1,
    seed = 9
  )
  second <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "nonspatial_collapse",
    pca_components = 3,
    centers = 10,
    collapse_function = "median",
    area_threshold = 1,
    seed = 9
  )

  expect_identical(first$pixel_to_unit$unit_id,
                   second$pixel_to_unit$unit_id)
  expect_true(all(is.na(first$pixel_to_unit$region_id)))
  expect_equal(first$settings$centers[["global"]], 2L)
  expect_equal(data.table::uniqueN(na.omit(first$pixel_to_unit$unit_id)), 2L)
  expect_equal(sort(first$analysis_units$metadata$area), c(4L, 4L))
  expect_equal(nrow(first$pixel_to_unit), ncol(fixture$map$spectra))
})

test_that("spatial cluster composition joins only connected equal materials", {
  wave <- 1:6
  spectra <- cbind(
    p1 = c(1, 2, 6, 2, 1, 1),
    p2 = c(1, 2, 6, 2, 1, 1),
    p3 = c(6, 2, 1, 2, 6, 1),
    p4 = c(6, 2, 1, 2, 6, 1)
  )
  map <- as_OpenSpecy(
    wave, spectra = spectra,
    metadata = data.frame(x = 0:3, y = 0)
  )
  clusters <- OpenSpecy:::.partition_particle_map(
    map, strategy = "nonspatial_collapse", pca_components = 2,
    centers = 2, collapse_function = "mean", area_threshold = 1,
    seed = 4
  )
  expect_equal(data.table::uniqueN(clusters$pixel_to_unit$unit_id), 2L)

  same_material <- rep("polymer", 4L)
  spatial_joined <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", material = same_material,
    collapse_function = "mean", area_threshold = 1
  )
  expect_equal(data.table::uniqueN(spatial_joined$pixel_to_unit$unit_id), 1L)

  different_material <- ifelse(
    clusters$pixel_to_unit$unit_index == 1L, "polymer-a", "polymer-b"
  )
  spatial_separate <- OpenSpecy:::.partition_particle_map(
    map, strategy = "collapse", material = different_material,
    collapse_function = "mean", area_threshold = 1
  )
  expect_equal(data.table::uniqueN(spatial_separate$pixel_to_unit$unit_id), 2L)
})

test_that("particle partition area and geometric-mean validation are explicit", {
  fixture <- particle_partition_fixture()
  dropped <- OpenSpecy:::.partition_particle_map(
    fixture$map,
    eligible = fixture$eligible,
    strategy = "collapse",
    collapse_function = "mean",
    area_threshold = 5
  )
  expect_null(dropped$analysis_units)
  expect_false(any(dropped$pixel_to_unit$kept))
  expect_true(all(dropped$pixel_to_unit$rejection_reason[fixture$eligible] ==
                    "area"))

  nonpositive <- fixture$map
  nonpositive$spectra[[1L, 1L]] <- 0
  expect_error(
    OpenSpecy:::.partition_particle_map(
      nonpositive,
      eligible = fixture$eligible,
      strategy = "collapse",
      collapse_function = "geometric mean",
      area_threshold = 1
    ),
    "strictly positive"
  )

  custom <- function(x) mean(x) + log(exp(1))
  expect_false(OpenSpecy:::.particle_collapse_function(custom)$geometric)
  expect_no_error({
    custom_result <- OpenSpecy:::.partition_particle_map(
      nonpositive,
      eligible = fixture$eligible,
      strategy = "collapse",
      collapse_function = custom,
      area_threshold = 1
    )
  })
  expect_s3_class(custom_result$analysis_units, "OpenSpecy")
  expect_identical(custom_result$settings$collapse, "custom")
})

test_that("partial and nonspatial public results retain pixel-to-unit joins", {
  wn <- seq(750, 1800, length.out = 40)
  pe <- sin(wn / 120) + 1
  mineral <- cos(wn / 130) + 1
  library <- as_OpenSpecy(
    wn,
    spectra = cbind(pe = pe, mineral = mineral),
    metadata = data.frame(
      sample_name = c("pe", "mineral"),
      material_class = c("poly(ethylene)", "mineral")
    )
  )
  map <- as_OpenSpecy(
    wn,
    spectra = cbind(pe + 0.01, pe + 0.02, mineral + 0.01,
                    rep(0, length(wn))),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )

  for (strategy in c("partial_collapse", "nonspatial_collapse")) {
    result <- automate_particle_analysis(
      map,
      library,
      particle_id_strategy = strategy,
      specs_centers = 2,
      sn_threshold_min = 0.001,
      area_threshold = 1,
      collapse_function = mean,
      outputs = c("details", "summary", "raw", "processed",
                  "particle_image", "correlation"),
      process_args = list(smooth_intens = FALSE, make_rel = TRUE)
    )
    sample <- result$samples[[1L]]
    expect_s3_class(result, "OpenSpecyParticleAnalysis")
    expect_s3_class(sample$particles_rds, "OpenSpecy")
    expect_true("unit_id" %in% names(sample$particles_raw_rds$metadata))
    expect_true("material_class" %in% names(sample$particles_raw_rds$metadata))
    expect_true(any(!is.na(sample$particles_raw_rds$metadata$unit_id)))
    expect_identical(sample$particle_image$type, "heatmap_categorical")
    expect_identical(sample$cor_heatmap$type, "heatmap_categorical")
  }
})
