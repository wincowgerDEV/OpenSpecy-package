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
