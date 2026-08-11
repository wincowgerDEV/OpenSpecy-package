.h5_registration_le <- function(x, n) {
  as.raw(floor(x / 256^(seq_len(n) - 1L)) %% 256)
}

.h5_registration_bmp <- function(color, width = 2L, height = 2L) {
  rgb <- array(rep(as.integer(color), each = width * height),
               dim = c(height, width, 3L))
  row_stride <- as.integer(ceiling(width * 3L / 4L) * 4L)
  pixel_data <- raw()
  for (row in rev(seq_len(height))) {
    row_data <- raw()
    for (column in seq_len(width)) {
      row_data <- c(row_data, as.raw(rgb[row, column, c(3L, 2L, 1L)]))
    }
    pixel_data <- c(pixel_data, row_data,
                    as.raw(rep(0L, row_stride - width * 3L)))
  }
  image_size <- length(pixel_data)
  c(
    charToRaw("BM"), .h5_registration_le(54L + image_size, 4L),
    as.raw(rep(0L, 4L)), .h5_registration_le(54L, 4L),
    .h5_registration_le(40L, 4L), .h5_registration_le(width, 4L),
    .h5_registration_le(height, 4L), .h5_registration_le(1L, 2L),
    .h5_registration_le(24L, 2L), .h5_registration_le(0L, 4L),
    .h5_registration_le(image_size, 4L), as.raw(rep(0L, 16L)),
    pixel_data
  )
}

test_that("read_h5 registers every intersecting tile by region and stage", {
  skip_if_not_installed("hdf5r")

  file <- tempfile(fileext = ".h5")
  on.exit(unlink(file), add = TRUE)
  h5 <- hdf5r::H5File$new(file, mode = "w")
  fi <- h5$create_group("FileInfo")
  xml <- paste0(
    "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
    "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">300</VAR>"
  )
  fi[["MetaData"]] <- as.integer(charToRaw(xml))

  regions <- h5$create_group("Regions")
  region_a <- regions$create_group("RegionA")
  region_a[["Dataset"]] <- array(as.numeric(seq_len(12)), dim = c(3, 2, 2))
  region_a[["-StagePosXYZ"]] <- c(2, 2, 500, 18, 18, 500)
  region_b <- regions$create_group("RegionB")
  region_b[["Dataset"]] <- array(as.numeric(seq_len(12)), dim = c(3, 2, 2))
  region_b[["-StagePosXYZ"]] <- c(102, 102, 600, 118, 118, 600)

  mosaic <- h5$create_group("Mosaic")
  # Centers use [y edge, y edge, y center, x center, x edge, x edge].
  mosaic[["Centers"]] <- rbind(
    c(10, 0, 5, 5, 0, 10),
    c(10, 0, 5, 15, 10, 20),
    c(20, 10, 15, 5, 0, 10),
    c(20, 10, 15, 15, 10, 20),
    c(120, 100, 110, 105, 100, 110),
    c(120, 100, 110, 115, 110, 120)
  )
  colors <- list(c(255, 0, 0), c(0, 255, 0), c(0, 0, 255),
                 c(255, 255, 0), c(0, 255, 255), c(255, 0, 255))
  for (i in seq_along(colors)) {
    mosaic[[paste0("Image", i - 1L)]] <-
      as.integer(.h5_registration_bmp(colors[[i]]))
  }
  h5$close_all()

  os <- read_h5(file)
  expect_s3_class(os, "OpenSpecy")
  expect_true(check_OpenSpecy(os))
  region_counts <- table(os$metadata$region)
  expect_equal(names(region_counts), c("RegionA", "RegionB"))
  expect_equal(as.integer(region_counts), c(4L, 4L))
  expect_equal(anyDuplicated(os$metadata$id), 0L)
  expect_identical(colnames(os$spectra), os$metadata$id)

  a_md <- os$metadata[region == "RegionA"]
  expect_equal(sort(unique(a_md$stage_x_nm)), c(2, 18))
  expect_equal(sort(unique(a_md$stage_y_nm)), c(2, 18))
  expect_equal(unique(a_md$stage_z_nm), 500)
  expect_identical(unique(a_md$stage_units), "nm")
  expect_equal(
    a_md[row == 1L & col == 1L,
         c("stage_x_nm", "stage_y_nm"), with = FALSE],
    data.table::data.table(stage_x_nm = 2, stage_y_nm = 2)
  )
  expect_equal(a_md[, sort(unique(x))], 0:1)
  expect_equal(a_md[, sort(unique(y))], 0:1)

  full_vi <- visual_image(os, require = TRUE)
  expect_null(full_vi$image)
  expect_setequal(names(full_vi$regions), c("RegionA", "RegionB"))
  expect_equal(full_vi$diagnostics$tile_counts,
               c(RegionA = 4L, RegionB = 2L))

  region_a_os <- filter_spec(os, os$metadata$region == "RegionA")
  a_vi <- visual_image(region_a_os, require = TRUE)
  expect_equal(nrow(a_vi$tiles), 4L)
  expect_setequal(a_vi$source, paste0("/Mosaic/Image", 0:3))
  expect_equal(dim(a_vi$image), c(3L, 3L, 3L))
  expect_equal(as.integer(a_vi$image[1, 1, ]), c(0, 0, 255))
  expect_equal(as.integer(a_vi$image[1, 3, ]), c(255, 255, 0))
  expect_equal(as.integer(a_vi$image[3, 1, ]), c(255, 0, 0))
  expect_equal(as.integer(a_vi$image[3, 3, ]), c(0, 255, 0))
  expect_equal(a_vi$bottom_left, c(1.2, 2.8), tolerance = 1e-8)
  expect_equal(a_vi$top_right, c(2.8, 1.2), tolerance = 1e-8)
  expect_equal(a_vi$map_dim, c(2, 2))
  expect_identical(a_vi$transform$image_col_axis, "stage_x_increasing")
  expect_identical(a_vi$transform$image_row_axis, "stage_y_decreasing")

  region_b_os <- filter_spec(os, os$metadata$region == "RegionB")
  b_vi <- visual_image(region_b_os, require = TRUE)
  expect_setequal(b_vi$source, paste0("/Mosaic/Image", 4:5))

  file_specs <- open_specs(file, cache_dir = tempfile("h5-visual-cache-"))
  region_a_specs <- split_spec(file_specs, by = "region")$RegionA
  descriptor <- visual_image(region_a_specs, require = TRUE)
  expect_null(descriptor$image)
  materialized <- OpenSpecy:::.filespec_materialize_visual(region_a_specs)
  expect_equal(materialized$image, a_vi$image)
  expect_equal(materialized$bottom_left, a_vi$bottom_left)
  expect_equal(materialized$top_right, a_vi$top_right)
  expect_equal(nrow(materialized$tiles), 4L)
  expect_identical(
    OpenSpecy:::.filespec_materialize_visual(region_a_specs), materialized
  )
})
