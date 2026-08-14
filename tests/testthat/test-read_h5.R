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
  expect_contains(names(os$metadata),
                  c("region", "particle_id", "subpixel", "row", "col",
                    "stage_pos_1"))
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
