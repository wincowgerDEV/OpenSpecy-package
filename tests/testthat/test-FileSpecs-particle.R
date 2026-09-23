.make_particle_filespec_envi <- function(directory) {
  header <- file.path(directory, "particle-map.hdr")
  binary <- file.path(directory, "particle-map.dat")
  axis <- c(800, 1200, 2500, 3000)
  writeLines(c(
    "ENVI",
    "samples = 4",
    "lines = 4",
    "bands = 4",
    "header offset = 0",
    "data type = 4",
    "interleave = bip",
    "byte order = 0",
    paste0("wavelength = {", paste(axis, collapse = ", "), "}")
  ), header)

  particle <- c(1, 3, 2, 4)
  values <- unlist(lapply(0:3, function(row) {
    unlist(lapply(0:3, function(col) {
      if (row %in% 1:2 && col %in% 1:2) particle else rep(0, 4)
    }))
  }))
  con <- file(binary, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.numeric(values), con, size = 4L, endian = "little")
  list(header = header, binary = binary, axis = axis,
       particle = particle)
}

test_that("FileSpecs particle automation is bounded, exact, and reusable", {
  directory <- tempfile("filespec-particle-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  cache <- file.path(directory, "cache")
  source_before <- list(
    header = digest::digest(fixture$header, algo = "sha256", file = TRUE),
    binary = digest::digest(fixture$binary, algo = "sha256", file = TRUE),
    info = file.info(c(fixture$header, fixture$binary))[, c("size", "mtime")]
  )
  specs <- open_specs(fixture$header, cache_dir = cache)
  library <- as_OpenSpecy(
    fixture$axis,
    spectra = cbind(particle = fixture$particle,
                    other = c(4, 2, 3, 1)),
    metadata = data.frame(sample_name = c("particle", "other"),
                          material_class = c("polymer", "other"))
  )
  args <- list(
    library = library,
    particle_id_strategy = "collapse",
    sn_threshold_min = 5,
    sn_threshold_max = Inf,
    cor_threshold = 0.7,
    area_threshold = 0,
    metric = "tot_sig",
    collapse_function = mean,
    outputs = c("details", "summary", "raw", "processed", "heatmap",
                "thresholded", "correlation", "sn_histogram",
                "cor_histogram"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  old_chunk <- getOption("OpenSpecy.filespec.chunk_size")
  options(OpenSpecy.filespec.chunk_size = 3L)
  on.exit(options(OpenSpecy.filespec.chunk_size = old_chunk), add = TRUE)
  result <- do.call(automate_particle_analysis, c(list(x = specs), args))

  expect_s3_class(result, "OpenSpecyParticleAnalysis")
  expect_named(result$samples, "Region1")
  expect_equal(nrow(result$particle_details_all_csv), 1)
  expect_equal(result$particle_details_all_csv$area_um2, 4 * 25^2)
  expect_equal(result$particle_details_all_csv$material_class, "polymer")
  expect_equal(result$samples$Region1$particles_rds$metadata$max_cor_name,
               "particle")
  expect_s3_class(result$samples$Region1$particles_raw_rds, "FileSpecs")
  expect_s3_class(result$samples$Region1$particles_rds, "OpenSpecy")
  expect_identical(result$samples$Region1$sn_histogram$type, "histogram")
  expect_identical(result$samples$Region1$cor_histogram$type, "histogram")

  eager <- decompress_spec(specs, region = "Region1")
  eager_result <- do.call(automate_particle_analysis,
                          c(list(x = eager), args))
  file_details <- result$particle_details_all_csv
  eager_details <- eager_result$particle_details_all_csv
  compare <- c("max_cor_val", "area_um2", "perimeter_um",
               "max_length_um", "min_length_um", "material_class")
  expect_equal(file_details[, compare, with = FALSE],
               eager_details[, compare, with = FALSE], tolerance = 1e-10)
  expect_equal(result$samples$Region1$particles_rds$spectra,
               eager_result$samples[[1]]$particles_rds$spectra,
               tolerance = 1e-10, ignore_attr = TRUE)

  cache_files <- list.files(cache, recursive = TRUE, full.names = TRUE)
  cache_mtime <- file.info(cache_files)$mtime
  warm <- do.call(automate_particle_analysis, c(list(x = specs), args))
  expect_equal(warm$particle_details_all_csv,
               result$particle_details_all_csv)
  expect_identical(file.info(cache_files)$mtime, cache_mtime)

  source_after <- list(
    header = digest::digest(fixture$header, algo = "sha256", file = TRUE),
    binary = digest::digest(fixture$binary, algo = "sha256", file = TRUE),
    info = file.info(c(fixture$header, fixture$binary))[, c("size", "mtime")]
  )
  expect_identical(source_after, source_before)
})

test_that("file-backed connected means equal eager connected collapse", {
  directory <- tempfile("filespec-connected-mean-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$header, cache_dir = file.path(directory, "cache"))
  eligible <- rep(FALSE, 16L)
  eligible[c(6L, 7L, 10L, 11L)] <- TRUE

  streamed <- OpenSpecy:::.filespec_collapse_connected_mean(
    specs, eligible = eligible, area_threshold = 1L, chunk_size = 2L
  )
  eager_source <- decompress_spec(specs, region = "Region1")
  eager <- OpenSpecy:::.partition_particle_map(
    eager_source, eligible = eligible, strategy = "collapse",
    collapse_function = mean, area_threshold = 1L
  )

  mapping_columns <- setdiff(names(streamed$pixel_to_unit), "source_id")
  expect_equal(streamed$pixel_to_unit[, mapping_columns, with = FALSE],
               eager$pixel_to_unit[, mapping_columns, with = FALSE])
  expect_equal(data.table::uniqueN(streamed$pixel_to_unit$source_id), 1L)
  expect_equal(streamed$analysis_units$spectra,
               eager$analysis_units$spectra,
               tolerance = 1e-10, ignore_attr = TRUE)
  geometry <- c(
    "first_x", "first_y", "perimeter", "rectangular_min", "feret_min",
    "feret_max",
    "convex_hull_area"
  )
  expect_true(all(geometry %in% names(streamed$analysis_units$metadata)))
  expect_equal(
    streamed$analysis_units$metadata[, geometry, with = FALSE],
    eager$analysis_units$metadata[, geometry, with = FALSE],
    tolerance = 1e-10
  )
  expect_true(isTRUE(streamed$settings$file_backed))
  expect_equal(streamed$settings$chunk_size, 2L)

  material <- rep(NA_character_, 16L)
  material[c(6L, 7L)] <- "polymer-a"
  material[c(10L, 11L)] <- "polymer-b"
  streamed_by_material <- OpenSpecy:::.filespec_collapse_connected_mean(
    specs, eligible = eligible, material = material,
    area_threshold = 1L, chunk_size = 2L
  )
  eager_by_material <- OpenSpecy:::.partition_particle_map(
    eager_source, eligible = eligible, strategy = "collapse",
    material = material, collapse_function = mean, area_threshold = 1L
  )
  expect_identical(ncol(streamed_by_material$analysis_units$spectra), 2L)
  expect_equal(
    streamed_by_material$analysis_units$spectra,
    eager_by_material$analysis_units$spectra,
    tolerance = 1e-10, ignore_attr = TRUE
  )
  expect_error(
    OpenSpecy:::.filespec_collapse_connected_mean(
      specs, eligible = eligible, material = material[-1L]
    ),
    "one value per file-backed spectrum"
  )
})

test_that("file-backed particle blocks and retained means enforce memory bounds", {
  max_block <- 64 * 1024^2
  bands <- 427L
  expected <- floor(max_block / (bands * 8))
  expect_equal(
    OpenSpecy:::.filespec_bounded_chunk_size(bands, 100000L, max_block),
    as.integer(expected)
  )
  expect_error(
    OpenSpecy:::.filespec_bounded_chunk_size(bands, 1L, bands * 8 - 1),
    "cannot fit one spectrum"
  )

  capacity <- OpenSpecy:::.filespec_retained_mean_capacity(
    n_bands = bands, n_features = 1000L, max_bytes = 8 * 1024^2
  )
  expect_equal(capacity$bytes, bands * 1000 * 8 * 2)
  expect_error(
    OpenSpecy:::.filespec_retained_mean_capacity(
      n_bands = bands, n_features = 2000L, max_bytes = 8 * 1024^2
    ),
    "exceed the file-backed live-memory bound"
  )
})

test_that("file-backed S/N applies bounded preprocessing before measurement", {
  directory <- tempfile("filespec-snr-process-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$header, cache_dir = file.path(directory, "cache"))
  index <- OpenSpecy:::.filespec_index(specs)
  bands <- seq_along(fixture$axis)
  adjust <- function(block) adj_intens(
    block, type = "transmittance", make_rel = FALSE
  )

  streamed <- OpenSpecy:::.filespec_particle_snr(
    specs, index = index, bands = bands, metric = "sig", abs = FALSE,
    spectral_smooth = FALSE, sigma1 = c(1, 1, 1), chunk_size = 3L,
    process = adjust
  )
  eager <- decompress_spec(specs, region = "Region1") |>
    adjust() |>
    sig_noise(metric = "sig", abs = FALSE)

  expect_equal(streamed, unname(eager), tolerance = 1e-12)

  fully_process <- function(block) process_spec(
    block, adj_intens = TRUE,
    adj_intens_args = list(type = "transmittance"),
    conform_spec = FALSE, restrict_range = FALSE, flatten_range = FALSE,
    subtr_baseline = FALSE, smooth_intens = FALSE, make_rel = TRUE
  )
  streamed_fully <- OpenSpecy:::.filespec_particle_snr(
    specs, index = index, bands = bands, metric = "sig_times_noise",
    abs = FALSE, spectral_smooth = TRUE, sigma1 = c(1, 1, 1),
    chunk_size = 8L, process = fully_process
  )
  smooth_values <- OpenSpecy:::.filespec_smoothed_values(
    specs, index, seq_len(nrow(index)), bands = bands, sigma1 = c(1, 1, 1)
  )
  eager_fully <- OpenSpecy:::.filespec_values_to_OpenSpecy(
    specs, smooth_values
  ) |>
    fully_process() |>
    sig_noise(metric = "sig_times_noise", abs = FALSE)
  expect_equal(streamed_fully, unname(eager_fully), tolerance = 1e-12)
  expect_error(
    OpenSpecy:::.filespec_particle_snr(
      specs, index = index, bands = bands, metric = "sig", abs = FALSE,
      spectral_smooth = FALSE, sigma1 = c(1, 1, 1), chunk_size = 3L,
      process = "invalid"
    ),
    "process"
  )
})

test_that("FileSpecs particle automation accepts both threshold extremes", {
  directory <- tempfile("filespec-particle-extremes-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$header, cache_dir = file.path(directory, "cache"))
  library <- as_OpenSpecy(
    fixture$axis,
    spectra = matrix(fixture$particle, ncol = 1,
                     dimnames = list(NULL, "particle")),
    metadata = data.frame(sample_name = "particle",
                          material_class = "polymer")
  )
  args <- list(
    x = specs, library = library, collapse_function = mean,
    metric = "tot_sig", area_threshold = 0,
    outputs = c("details", "summary", "processed"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  retained <- NULL
  expect_message(
    retained <- do.call(
      automate_particle_analysis,
      c(args, list(sn_threshold_min = -Inf, sn_threshold_max = Inf))
    ),
    "retained every map pixel"
  )
  expect_equal(ncol(retained$samples$Region1$particles_rds$spectra), 1L)
  expect_equal(retained$samples$Region1$particles_rds$metadata$area, 16L)

  removed <- NULL
  expect_message(
    removed <- do.call(
      automate_particle_analysis,
      c(args, list(sn_threshold_min = 1e12, sn_threshold_max = Inf))
    ),
    "removed every map pixel"
  )
  expect_null(removed$samples$Region1$particles_rds)
  expect_equal(removed$particle_summary_all_csv$count, 0L)
})

test_that("FileSpecs all-cell identification matches the eager workflow", {
  directory <- tempfile("filespec-all-cell-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$header, cache_dir = file.path(directory, "cache"))
  library <- as_OpenSpecy(
    fixture$axis,
    spectra = cbind(particle = fixture$particle,
                    other = c(4, 2, 3, 1)),
    metadata = data.frame(sample_name = c("particle", "other"),
                          material_class = c("polymer", "other"))
  )
  args <- list(
    library = library, particle_id_strategy = "all_cell_id",
    sn_threshold_min = 5, sn_threshold_max = Inf,
    cor_threshold = 0.7, area_threshold = 0, metric = "tot_sig",
    collapse_function = mean, outputs = c("details", "processed"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  old_chunk <- getOption("OpenSpecy.filespec.chunk_size")
  options(OpenSpecy.filespec.chunk_size = 3L)
  on.exit(options(OpenSpecy.filespec.chunk_size = old_chunk), add = TRUE)
  streamed <- do.call(automate_particle_analysis, c(list(x = specs), args))
  eager <- do.call(
    automate_particle_analysis,
    c(list(x = decompress_spec(specs, region = "Region1")), args)
  )

  compare <- setdiff(names(streamed$particle_details_all_csv), "sample_id")
  expect_equal(streamed$particle_details_all_csv[, ..compare],
               eager$particle_details_all_csv[, ..compare], tolerance = 1e-10)
  expect_equal(streamed$samples$Region1$particles_rds$spectra,
               eager$samples[[1L]]$particles_rds$spectra,
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("FileSpecs particle automation rejects unsupported whole-map paths", {
  directory <- tempfile("filespec-particle-errors-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$binary, cache_dir = file.path(directory, "cache"))
  library <- as_OpenSpecy(
    fixture$axis,
    spectra = matrix(fixture$particle, ncol = 1,
                     dimnames = list(NULL, "particle"))
  )

  expect_error(automate_particle_analysis(specs, library),
               "collapse_function = mean")
  expect_error(automate_particle_analysis(
    specs, library, collapse_function = mean, particle_id_strategy = "raw"
  ), "currently supports")
  expect_error(automate_particle_analysis(
    specs, library, collapse_function = mean, metric = "entropy"
  ), "explicit global breaks")
  expect_error(automate_particle_analysis(
    specs, library, collapse_function = mean, top_n = 2L
  ), "top_n")
})

test_that("FileSpecs spectral_smooth matches the eager mmand::gaussianSmooth reader", {
  directory <- tempfile("filespec-particle-smooth-")
  dir.create(directory)
  fixture <- .make_particle_filespec_envi(directory)
  specs <- open_specs(fixture$header, cache_dir = file.path(directory, "cache"))
  library <- as_OpenSpecy(
    fixture$axis,
    spectra = cbind(particle = fixture$particle, other = c(4, 2, 3, 1)),
    metadata = data.frame(sample_name = c("particle", "other"),
                          material_class = c("polymer", "other"))
  )
  args <- list(
    library = library, particle_id_strategy = "collapse",
    spectral_smooth = TRUE, sigma1 = c(1, 1, 1),
    sn_threshold_min = 2, sn_threshold_max = Inf, cor_threshold = 0.7,
    area_threshold = 0, metric = "tot_sig", collapse_function = mean,
    outputs = c("details", "processed"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  old_chunk <- getOption("OpenSpecy.filespec.chunk_size")
  options(OpenSpecy.filespec.chunk_size = 3L)
  on.exit(options(OpenSpecy.filespec.chunk_size = old_chunk), add = TRUE)
  streamed <- do.call(automate_particle_analysis, c(list(x = specs), args))

  # Compare against the eager reader on the raw ENVI file (not
  # decompress_spec()), so both paths smooth via the same
  # mmand::gaussianSmooth() call rather than spatial_smooth().
  eager <- do.call(automate_particle_analysis,
                   c(list(x = fixture$binary), args))

  expect_gt(nrow(streamed$particle_details_all_csv), 0)
  expect_equal(
    streamed$particle_details_all_csv$area_um2,
    eager$particle_details_all_csv$area_um2
  )
  expect_equal(
    streamed$samples$Region1$particles_rds$spectra,
    eager$samples[[1]]$particles_rds$spectra,
    tolerance = 1e-10, ignore_attr = TRUE
  )
})

test_that("FileSpecs particle image identities include image content", {
  first <- matrix(c(0, 1, 2, 3), nrow = 2)
  second <- first
  second[[1L]] <- 9
  first_id <- OpenSpecy:::.filespec_image_identity(first, c(0, 0), c(1, 1))
  second_id <- OpenSpecy:::.filespec_image_identity(second, c(0, 0), c(1, 1))

  expect_identical(first_id$image$dim, second_id$image$dim)
  expect_false(identical(first_id$image$sha256, second_id$image$sha256))

  directory <- tempfile("filespec-image-identity-")
  dir.create(directory)
  path <- file.path(directory, "image.bin")
  writeBin(as.raw(c(1, 2, 3, 4)), path)
  original_time <- file.info(path)$mtime
  file_first <- OpenSpecy:::.filespec_image_identity(path, NULL, NULL)
  writeBin(as.raw(c(4, 3, 2, 1)), path)
  Sys.setFileTime(path, original_time)
  file_second <- OpenSpecy:::.filespec_image_identity(path, NULL, NULL)

  expect_identical(file_first$image$size, file_second$image$size)
  expect_false(identical(file_first$image$sha256,
                         file_second$image$sha256))
})
