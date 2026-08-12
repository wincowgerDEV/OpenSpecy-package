.make_filespec_h5 <- function(path,
                              region_names = c("Region1", "Region2")) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  info <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>",
    "<VAR TYPE=\"System.Int32\" NAME=\"SpectrumPoints\">4</VAR>"
  )
  info[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  region1 <- regions$create_group(region_names[[1L]])
  region1[["Dataset"]] <- array(as.numeric(seq_len(16)), dim = c(4, 2, 2))
  region1[["-StagePosXYZ"]] <- c(0, 0, 0, 1, 1, 0)
  region2 <- regions$create_group(region_names[[2L]])
  region2[["Dataset"]] <- array(as.numeric(101:116), dim = c(4, 2, 2))
  region2[["-StagePosXYZ"]] <- c(10, 0, 0, 11, 1, 0)
  mosaic <- h5$create_group("Mosaic")
  mosaic[["Centers"]] <- matrix(c(-1, 2, 0.5, 5.5, -1, 12), nrow = 1)
  mosaic[["Image0"]] <- as.integer(0)
  invisible(path)
}

.make_filespec_envi <- function(directory, interleave) {
  stem <- file.path(directory, paste0("tiny-", interleave))
  header <- paste0(stem, ".hdr")
  binary <- paste0(stem, ".dat")
  writeLines(c(
    "ENVI",
    "samples = 2",
    "lines = 2",
    "bands = 3",
    "header offset = 4",
    "data type = 4",
    paste0("interleave = ", interleave),
    "byte order = 0",
    "wavelength = {100, 200, 300}"
  ), header)

  cube <- array(NA_real_, dim = c(3, 2, 2))
  for (band in 1:3) {
    for (row in 1:2) {
      for (col in 1:2) cube[band, row, col] <- band * 100 + row * 10 + col
    }
  }
  values <- switch(
    interleave,
    bsq = unlist(lapply(1:3, function(band) {
      unlist(lapply(1:2, function(row) cube[band, row, 1:2]))
    })),
    bil = unlist(lapply(1:2, function(row) {
      unlist(lapply(1:3, function(band) cube[band, row, 1:2]))
    })),
    bip = unlist(lapply(1:2, function(row) {
      unlist(lapply(1:2, function(col) cube[1:3, row, col]))
    }))
  )
  con <- file(binary, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(rep(0, 4)), con)
  writeBin(as.numeric(values), con, size = 4L, endian = "little")
  list(header = header, binary = binary, cube = cube)
}

.make_filespec_envi_precision <- function(directory) {
  header <- file.path(directory, "precision.hdr")
  binary <- file.path(directory, "precision.dat")
  axis <- c(100.12345678901235, 200.23456789012346,
            300.34567890123457)
  cube <- array(c(
    pi, sqrt(2), log(2),
    exp(1), 1 / 7, 1 / 11,
    1 / 13, 1 / 17, 1 / 19,
    1 / 23, 1 / 29, 1 / 31
  ), dim = c(3, 2, 2))
  writeLines(c(
    "ENVI",
    "samples = 2",
    "lines = 2",
    "bands = 3",
    "header offset = 0",
    "data type = 5",
    "interleave = bip",
    "byte order = 0",
    paste0("wavelength = {",
           paste(trimws(formatC(axis, digits = 17L, format = "g",
                                flag = "#")), collapse = ", "), "}")
  ), header)
  values <- unlist(lapply(1:2, function(row) {
    unlist(lapply(1:2, function(col) cube[1:3, row, col]))
  }))
  con <- file(binary, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.numeric(values), con, size = 8L, endian = "little")
  list(header = header, binary = binary, axis = axis, cube = cube)
}

test_that("FileSpecs H5 descriptors read bounded selections without handles", {
  skip_if_not_installed("hdf5r")
  directory <- tempfile("filespec-h5-")
  dir.create(directory)
  file <- .make_filespec_h5(file.path(directory, "tiny.h5"))
  cache <- file.path(directory, "cache")

  before <- list(
    hash = digest::digest(file, algo = "sha256", file = TRUE),
    size = file.info(file)$size,
    mtime = file.info(file)$mtime
  )
  specs <- open_specs(file, cache_dir = cache)

  expect_s3_class(specs, "FileSpecs")
  expect_s3_class(specs, "Specs")
  expect_identical(class(specs), c("FileSpecs", "Specs", "list"))
  expect_identical(names(specs), c("source", "index", "view", "cache",
                                    "recipe"))
  expect_false("values" %in% names(specs))
  expect_equal(specs$source$axis, seq(100, 400, length.out = 4))
  expect_true(check_Specs(specs))
  expect_equal(OpenSpecy:::.filespec_n_spectra(specs), 8)
  expect_identical(OpenSpecy:::.filespec_regions(specs),
                   c("Region1", "Region2"))
  expect_contains(names(OpenSpecy:::.filespec_index(specs)),
                  c("index", "region", "row", "col", "x", "y", "col_id",
                    "stage_x_nm", "stage_y_nm", "stage_units"))
  expect_equal(specs$source$visual$image_datasets, "/Mosaic/Image0")
  expect_named(specs$source$visual$regions, c("Region1", "Region2"))
  expect_identical(attr(specs, "visual_image"), specs$source$visual)
  expect_match(capture.output(print(specs))[[1L]], "FileSpecs")

  selected <- OpenSpecy:::.filespec_read(specs, c(1, 4, 5), bands = c(1, 3))
  eager <- read_h5(file, read_visual = FALSE)
  expect_s3_class(selected, "OpenSpecy")
  expect_true(check_OpenSpecy(selected))
  expect_equal(selected$wavenumber, eager$wavenumber[c(1, 3)])
  expect_equal(selected$spectra,
               eager$spectra[c(1, 3), c(1, 4, 5), drop = FALSE],
               ignore_attr = TRUE)
  expect_equal(selected$metadata$region,
               eager$metadata$region[c(1, 4, 5)])
  expect_equal(selected$metadata$stage_pos_1, c(0, 0, 10))
  expect_error(decompress_spec(specs), "whole-source")
  expect_equal(ncol(decompress_spec(specs, region = "Region2")$spectra), 4)
  expect_equal(ncol(decompress_spec(specs, roi = c(1, 1, 0, 1))$spectra), 4)
  expect_error(decompress_spec(specs, index = c(1, 1)), "duplicate")

  views <- split_spec(specs, by = "region")
  expect_named(views, c("Region1", "Region2"))
  expect_equal(OpenSpecy:::.filespec_n_spectra(views$Region1), 4)
  expect_equal(OpenSpecy:::.filespec_regions(views$Region2), "Region2")
  expect_equal(visual_image(views$Region1)$region, "Region1")
  expect_equal(OpenSpecy:::.infer_visual_map_dim(views$Region1), c(2, 2))
  expect_equal(ncol(decompress_spec(views$Region2, index = 1)$spectra), 1)

  descriptor <- file.path(directory, "tiny.filespec.rds")
  expect_invisible(write_specs(specs, descriptor))
  restored <- read_specs(descriptor)
  expect_s3_class(restored, "FileSpecs")
  expect_equal(OpenSpecy:::.filespec_read(restored, 2)$spectra,
               eager$spectra[, 2, drop = FALSE], ignore_attr = TRUE)
  expect_error(write_specs(specs, descriptor), "already exists")
  expect_error(write_specs(specs, file), "source member")
  expect_silent(serialize(specs, NULL))

  after <- list(
    hash = digest::digest(file, algo = "sha256", file = TRUE),
    size = file.info(file)$size,
    mtime = file.info(file)$mtime
  )
  expect_identical(after, before)
})

test_that("FileSpecs retains raw collision-prone H5 region names", {
  skip_if_not_installed("hdf5r")
  directory <- tempfile("filespec-h5-names-")
  dir.create(directory)
  file <- .make_filespec_h5(file.path(directory, "names.h5"),
                            region_names = c("Area1", "Region1"))
  specs <- open_specs(file, cache_dir = file.path(directory, "cache"))
  index <- OpenSpecy:::.filespec_index(specs)

  expect_true(check_Specs(specs))
  expect_setequal(index$region, c("Area1", "Region1"))
  expect_identical(as.character(index$particle_id),
                   as.character(index$region))
  expect_identical(anyDuplicated(index$col_id), 0L)
  expect_true(all(startsWith(index$col_id,
                             paste0(index$region, "_r"))))

  materialized <- decompress_spec(specs, index = seq_len(nrow(index)))
  eager <- read_h5(file, read_visual = FALSE)
  expect_equal(materialized$spectra, eager$spectra, ignore_attr = TRUE)
  expect_identical(colnames(materialized$spectra), colnames(eager$spectra))
  expect_identical(as.character(materialized$metadata$region),
                   as.character(eager$metadata$region))
})

test_that("FileSpecs materializes metadata for every region safely", {
  skip_if_not_installed("hdf5r")
  directory <- tempfile("filespec-h5-metadata-")
  dir.create(directory)
  file <- .make_filespec_h5(file.path(directory, "metadata.h5"))
  specs <- open_specs(file, cache_dir = file.path(directory, "cache"))
  source <- specs$source
  source$region_metadata$Region1$region_label <- "first"
  source$region_metadata$Region1$row <- 999L
  source$region_metadata$Region1$region <- "tampered"
  source$region_metadata$Region2$region_label <- "second"
  source$region_metadata$Region2$row <- 888L
  source$region_metadata$Region2$region <- "tampered"
  selected <- OpenSpecy:::.filespec_index(specs)[c(1L, 5L)]

  metadata <- OpenSpecy:::.filespec_materialized_metadata(source, selected)

  expect_identical(metadata$region, c("Region1", "Region2"))
  expect_identical(metadata$row, c(1L, 1L))
  expect_identical(metadata$region_label, c("first", "second"))
  expect_equal(metadata$stage_pos_1, c(0, 10))
})

test_that("FileSpecs ENVI adapter reads BSQ, BIL, and BIP windows", {
  directory <- tempfile("filespec-envi-")
  dir.create(directory)

  for (interleave in c("bsq", "bil", "bip")) {
    fixture <- .make_filespec_envi(directory, interleave)
    specs <- open_specs(fixture$header,
                        cache_dir = file.path(directory, paste0("cache-",
                                                                interleave)))
    expect_true(check_Specs(specs))
    expect_equal(specs$source$layout$interleave, interleave)
    expect_equal(specs$source$members$path,
                 normalizePath(c(fixture$header, fixture$binary),
                               winslash = "/"))

    materialized <- decompress_spec(specs, index = c(4, 1), bands = c(1, 3))
    expected <- cbind(fixture$cube[c(1, 3), 2, 2],
                      fixture$cube[c(1, 3), 1, 1])
    expect_equal(materialized$wavenumber, c(100, 300))
    expect_equal(materialized$spectra, expected, ignore_attr = TRUE)
    expect_equal(materialized$metadata$row, c(2, 1))
    expect_equal(materialized$metadata$col, c(2, 1))

    roi <- decompress_spec(specs, roi = list(x = c(1, 1), y = c(0, 1)))
    expect_equal(ncol(roi$spectra), 2)
    expect_equal(roi$metadata$col, c(2, 2))
  }
})

test_that("FileSpecs exports a new atomic ENVI pair without changing sources", {
  directory <- tempfile("filespec-envi-write-")
  dir.create(directory)
  fixture <- .make_filespec_envi_precision(directory)
  specs <- open_specs(fixture$header,
                      cache_dir = file.path(directory, "cache"))
  source_before <- lapply(c(fixture$header, fixture$binary), function(path) {
    list(hash = digest::digest(path, algo = "sha256", file = TRUE),
         size = file.info(path)$size, mtime = file.info(path)$mtime)
  })

  target <- file.path(directory, "export.hdr")
  result <- write_spec(specs, target, chunk_size = 2L)
  expect_equal(result$header, normalizePath(target, winslash = "/",
                                             mustWork = FALSE))
  expect_true(file.exists(result$header))
  expect_true(file.exists(result$binary))

  restored <- open_specs(result$header,
                         cache_dir = file.path(directory, "export-cache"))
  original_values <- decompress_spec(specs, region = "Region1")
  restored_values <- decompress_spec(restored, region = "Region1")
  expect_equal(restored_values$wavenumber, original_values$wavenumber)
  expect_equal(restored_values$spectra, original_values$spectra,
               tolerance = 0, ignore_attr = TRUE)
  expect_match(paste(readLines(result$header), collapse = "\n"),
               "data type = 5", fixed = TRUE)
  expect_equal(file.info(result$binary)$size,
               length(original_values$spectra) * 8)
  expect_error(write_spec(specs, target), "overwrite")
  expect_error(write_spec(specs, fixture$header), "source member")
  custom_called <- FALSE
  expect_error(write_spec(specs, fixture$header,
                          method = function(...) custom_called <<- TRUE),
               "custom 'method' writers are disabled", fixed = TRUE)
  expect_false(custom_called)

  source_after <- lapply(c(fixture$header, fixture$binary), function(path) {
    list(hash = digest::digest(path, algo = "sha256", file = TRUE),
         size = file.info(path)$size, mtime = file.info(path)$mtime)
  })
  expect_identical(source_after, source_before)
})

test_that("FileSpecs cache publication is contained, locked, and immutable", {
  directory <- tempfile("filespec-cache-")
  dir.create(directory)
  fixture <- .make_filespec_envi(directory, "bip")
  specs <- open_specs(fixture$binary, cache_dir = file.path(directory, "cache"))

  path <- OpenSpecy:::.filespec_cache_path(specs, "particle", "result.rds")
  saved <- OpenSpecy:::.filespec_atomic_save_rds(list(value = 1), path)
  expect_true(file.exists(saved))
  expect_equal(readRDS(saved)$value, 1)
  reused <- OpenSpecy:::.filespec_atomic_save_rds(list(value = 2), path)
  expect_identical(reused, saved)
  expect_equal(readRDS(reused)$value, 1)
  expect_error(OpenSpecy:::.filespec_cache_path(specs, "..", "escape.rds"),
               "escapes")
  expect_error(OpenSpecy:::.filespec_cache_path(
    specs, "particle/../escape.rds"
  ), "escapes")
  expect_error(OpenSpecy:::.filespec_cache_path(
    specs, "particle\\..\\escape.rds"
  ), "escapes")

  lock <- OpenSpecy:::.filespec_acquire_lock(specs, "shared")
  on.exit(OpenSpecy:::.filespec_release_lock(specs, lock), add = TRUE)
  expect_error(OpenSpecy:::.filespec_acquire_lock(specs, "shared"), "locked")
  OpenSpecy:::.filespec_release_lock(specs, lock)

  expect_error(OpenSpecy:::.filespec_cache_commit(
    specs, "failed", function(stage) {
      saveRDS(1, file.path(stage, "partial.rds"))
      stop("cancelled")
    }
  ), "cancelled")
  expect_false(dir.exists(OpenSpecy:::.filespec_cache_path(
    specs, "generations",
    OpenSpecy:::.filespec_cache_key(specs, "failed")
  )))
})

test_that("FileSpecs detects changed sources and guards matrix-only methods", {
  directory <- tempfile("filespec-change-")
  dir.create(directory)
  fixture <- .make_filespec_envi(directory, "bsq")
  specs <- open_specs(fixture$binary, cache_dir = file.path(directory, "cache"))

  expect_identical(as_Specs(specs), specs)
  expect_error(cor_spec(specs), "unsafe whole-source")
  expect_error(match_spec(specs), "unsafe whole-source")
  expect_error(def_features(specs), "unsafe whole-source")
  expect_error(collapse_spec(specs), "unsafe whole-source")
  expect_error(fit_specs_pca(specs, 1), "unsafe whole-source")
  expect_error(encode_specs_hilbert(specs), "unsafe whole-source")
  expect_error(decode_specs_hilbert(specs), "unsafe whole-source")
  expect_error(particle_image(specs), "unsafe whole-source")
  expect_error(make_lib_lookup_template(specs, "region"),
               "unsafe whole-source")
  expect_error(join_lib_metadata(specs, data.frame(region = "Region1"),
                                 by = "region"), "unsafe whole-source")
  expect_error(join_material_hierarchy(
    specs, data.frame(material = "x", material_class = "x",
                      material_type = "x")
  ), "unsafe whole-source")

  con <- file(fixture$binary, open = "ab")
  writeBin(as.raw(0), con)
  close(con)
  expect_false(suppressWarnings(check_Specs(specs)))
  expect_error(OpenSpecy:::.filespec_read(specs, 1), "changed")
})

test_that("legacy Specs validation and serialization contracts remain intact", {
  values <- matrix(1:6, nrow = 2,
                   dimnames = list(c("a", "b"), c("one", "two", "three")))
  specs <- Specs(c("a", "b"), values)
  file <- tempfile(fileext = ".rds")

  expect_identical(names(specs), c("variables", "values", "coords", "metadata"))
  expect_true(check_Specs(specs))
  expect_invisible(write_specs(specs, file))
  expect_identical(read_specs(file), specs)
  expect_false(suppressWarnings(check_Specs(list())))
})
