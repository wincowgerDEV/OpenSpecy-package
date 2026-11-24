test_that("read_h5() keeps raw region spectra by default", {
  skip_if_not_installed("hdf5r")

  file <- tempfile(fileext = ".h5")
  h5 <- hdf5r::H5File$new(file, mode = "w")
  fi <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>"
  )
  fi[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  reg <- regions$create_group("Region1")
  reg[["Dataset"]] <- array(as.numeric(seq_len(24)), dim = c(4, 2, 3))
  reg[["-StagePosXYZ"]] <- as.numeric(1:6)
  h5$close_all()

  os <- read_h5(file, read_visual = FALSE)
  expect_s3_class(os, "OpenSpecy")
  expect_true(check_OpenSpecy(os))
  expect_equal(length(os$wavenumber), 4)
  expect_equal(range(os$wavenumber), c(100, 400))
  expect_equal(ncol(os$spectra), 6)
  expect_false(OpenSpecy:::.is_particle_centroid_object(os))
  expect_contains(names(os$metadata),
                  c("region", "particle_id", "subpixel", "row", "col",
                    "stage_pos_1"))
})

.make_particle_h5 <- function(marker = "mode", stage = TRUE, axis = TRUE) {
  file <- tempfile(fileext = ".hdf5")
  h5 <- hdf5r::H5File$new(file, mode = "w")
  on.exit(h5$close_all(), add = TRUE)
  info <- h5$create_group("FileInfo")
  xml <- paste0(
    '<VAR NAME="SpectrumPoints">4</VAR>',
    if (axis) paste0(
      '<VAR NAME="m_StartFrequency">400</VAR>',
      '<VAR NAME="m_EndFrequency">100</VAR>'
    ),
    if (identical(marker, "mode")) {
      '<VAR NAME="AutoRegionCollectType">ParticleAnalysisCollection</VAR>'
    },
    paste(rep('<VAR NAME="IgnoredRegionField">2</VAR>', 100L), collapse = "")
  )
  info[["MetaData"]] <- as.integer(charToRaw(xml))
  info$create_attr("SpectralType", robj = "Raman Shift")
  info$create_attr("SpatialUnitOfMeter", robj = 2e-9)
  if (marker %in% c("image", "empty")) {
    group <- h5$create_group("ParticleImages")
    if (identical(marker, "image")) group[["ParticleImage0"]] <- 0L
  }
  regions <- h5$create_group("Regions")
  first <- regions$create_group("Region1")
  first[["Dataset"]] <- array(as.numeric(seq_len(36)), dim = c(4, 3, 3))
  if (stage) first[["-StagePosXYZ"]] <- c(20, -100, 5, 10, -80, 5)
  second <- regions$create_group("Region2")
  second[["Dataset"]] <- array(as.numeric(101:116), dim = c(2, 2, 4))
  second[["-StagePosXYZ"]] <- c(100, -30, 5, 140, -20, 5)
  file
}

test_that("H5 acquisition metadata selects bounded particle means and centroids", {
  skip_if_not_installed("hdf5r")
  file <- .make_particle_h5()
  pixels <- read_h5(file, collapse = FALSE, read_visual = FALSE)
  expected <- vapply(c("Region1", "Region2"), function(region) {
    rowMeans(pixels$spectra[, pixels$metadata$region == region, drop = FALSE])
  }, numeric(4))
  particles <- read_h5(
    file, representation = "Specs", spectral_smooth = TRUE,
    background_filter = specs_background_filter("sig", 1e6, step = 1)
  )
  expect_true(check_OpenSpecy(particles))
  expect_identical(particles$wavenumber, pixels$wavenumber)
  expect_identical(particles$spectra, expected)
  expect_identical(particles$metadata$col_id, colnames(expected))
  expect_equal(particles$metadata$source_spectrum_count, c(9, 4))
  expect_equal(particles$metadata$x, c(30, 240))
  expect_equal(particles$metadata$y, c(-180, -50))
  expect_equal(particles$metadata$centroid_x, particles$metadata$x)
  expect_equal(particles$metadata$centroid_y, particles$metadata$y)
  expect_identical(attr(particles, "openspecy_object_type"), "particle_centroids")
  expect_identical(attr(particles, "openspecy_spatial_unit"), "nm")
  expect_identical(attr(particles, "spectra_type"), "raman")
  expect_false(any(grepl("IgnoredRegionField|subpixel", names(particles$metadata))))
  expect_null(attr(particles, "visual_image"))
  expect_false(OpenSpecy:::.is_particle_centroid_object(pixels))
  expect_equal(ncol(pixels$spectra), 13)
})

test_that("H5 particle labels distinguish acquisitions from similar map cubes", {
  skip_if_not_installed("hdf5r")
  particles <- read_any(.make_particle_h5("image"))
  pixels <- read_h5(.make_particle_h5("empty"), representation = "Specs")
  expect_true(OpenSpecy:::.is_particle_centroid_object(particles))
  expect_equal(ncol(particles$spectra), 2)
  expect_true(check_Specs(pixels))
  expect_equal(specs_source_count(pixels), 13)
  expect_error(read_h5(.make_particle_h5(stage = FALSE)), "stage bounds: Region1")
  expect_error(read_h5(.make_particle_h5(axis = FALSE)), "valid spectral range")
})

test_that("read_h5() preserves cube order while assembling regions", {
  skip_if_not_installed("hdf5r")

  file <- tempfile(fileext = ".h5")
  h5 <- hdf5r::H5File$new(file, mode = "w")
  fi <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>"
  )
  fi[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  first <- array(as.numeric(seq_len(24)), dim = c(2, 3, 4))
  second <- array(as.numeric(101:124), dim = c(2, 3, 4))
  region1 <- regions$create_group("Region1")
  region2 <- regions$create_group("Region2")
  region1[["Dataset"]] <- first
  region2[["Dataset"]] <- second
  h5$close_all()

  os <- read_h5(file, read_visual = FALSE)
  expected <- cbind(
    matrix(aperm(first, c(3, 1, 2)), nrow = 4),
    matrix(aperm(second, c(3, 1, 2)), nrow = 4)
  )
  colnames(expected) <- c(
    paste0("Region1_r", rep(1:2, 3), "c", rep(1:3, each = 2)),
    paste0("Region2_r", rep(1:2, 3), "c", rep(1:3, each = 2))
  )

  expect_identical(os$spectra, expected)
  expect_identical(as.character(os$metadata$region),
                   rep(c("Region1", "Region2"), each = 6))
  expect_identical(os$metadata$col_id, colnames(expected))
})

test_that("read_h5() can return compact Specs with background sentinel zero", {
  skip_if_not_installed("hdf5r")

  file <- tempfile(fileext = ".h5")
  h5 <- hdf5r::H5File$new(file, mode = "w")
  fi <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>"
  )
  fi[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  cube <- array(as.numeric(seq_len(24)), dim = c(2, 3, 4))
  region <- regions$create_group("Region1")
  region[["Dataset"]] <- cube
  h5$close_all()

  eager <- read_h5(file, read_visual = FALSE)
  exact <- read_h5(file, read_visual = FALSE, representation = "Specs")
  threshold <- stats::median(sig_noise(eager, metric = "sig", abs = FALSE))
  policy <- specs_background_filter("sig", threshold, step = 1)
  filtered <- read_h5(
    file, read_visual = FALSE, representation = "Specs",
    background_filter = policy
  )

  expect_true(check_Specs(exact))
  expect_identical(unname(specs_source_values(exact)),
                   unname(eager$spectra))
  expect_equal(specs_metadata(exact)$region, eager$metadata$region)
  expected_keep <- sig_noise(eager, metric = "sig", abs = FALSE) > threshold
  expect_equal(specs_background_mask(filtered), unname(!expected_keep))
  expect_equal(ncol(filtered$values), sum(expected_keep))
  expect_true(all(specs_source_values(filtered, which(!expected_keep)) == 0))
})

test_that("read_h5() attaches mosaic coregistration when stage metadata are present", {
  skip_if_not_installed("hdf5r")

  bmp <- as.raw(c(
    0x42, 0x4d, 0x46, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x36, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x18, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x13, 0x0b,
    0x00, 0x00, 0x13, 0x0b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0xff, 0xff, 0xff,
    0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00
  ))
  file <- tempfile(fileext = ".h5")
  h5 <- hdf5r::H5File$new(file, mode = "w")
  fi <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_LL_X\">100000</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_LL_Y\">-100000</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_WidthInNM\">2000</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_HeightInNM\">2000</VAR>"
  )
  fi[["MetaData"]] <- as.integer(charToRaw(xml))
  regions <- h5$create_group("Regions")
  reg <- regions$create_group("Region1")
  reg[["Dataset"]] <- array(as.numeric(seq_len(16)), dim = c(4, 2, 2))
  mosaic <- h5$create_group("Mosaic")
  mosaic[["Centers"]] <- matrix(
    c(-97000, -102000, -99500, 101000, 99000, 103000),
    nrow = 1
  )
  mosaic[["Image0"]] <- as.integer(bmp)
  h5$close_all()

  os <- read_h5(file)
  vi <- visual_image(os, require = TRUE)
  expect_equal(vi$source, "/Mosaic/Image0")
  expect_equal(vi$transform$method, "h5_mosaic_centers")
  expect_equal(vi$bottom_left, c(1.25, 1.6), tolerance = 0.001)
  expect_equal(vi$top_right, c(1.75, 1.2), tolerance = 0.001)
})
