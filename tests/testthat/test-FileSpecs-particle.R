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
    top_n = 2L,
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
  expect_equal(result$particle_details_all_csv$match_rank_1_name, "particle")
  expect_equal(result$particle_details_all_csv$match_rank_2_name, "other")
  expect_true(all(c("match_rank_1_value", "match_rank_2_value") %in%
                    names(result$samples$Region1$particles_rds$metadata)))
  expect_s3_class(result$samples$Region1$particles_raw_rds, "FileSpecs")
  expect_s3_class(result$samples$Region1$particles_rds, "OpenSpecy")
  expect_s3_class(result$samples$Region1$sn_histogram_png, "recordedplot")
  expect_s3_class(result$samples$Region1$cor_histogram_png, "recordedplot")

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
  ), "supports only")
  expect_error(automate_particle_analysis(
    specs, library, collapse_function = mean, spectral_smooth = TRUE
  ), "region-halo")
  expect_error(automate_particle_analysis(
    specs, library, collapse_function = mean, metric = "entropy"
  ), "explicit global breaks")
  expect_error(automate_particle_analysis(
    specs, 1, collapse_function = mean, top_n = 2L
  ), "requires an OpenSpecy library")
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
