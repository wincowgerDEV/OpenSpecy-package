test_that("visual images attach and red box origins are detected", {
  img <- array(1, dim = c(20, 30, 3))
  img[c(3, 18), 5:25, 1] <- 1
  img[c(3, 18), 5:25, 2:3] <- 0
  img[3:18, c(5, 25), 1] <- 1
  img[3:18, c(5, 25), 2:3] <- 0

  origin <- detect_image_origin(img)
  expect_equal(origin$bottom_left, c(5, 18))
  expect_equal(origin$top_right, c(25, 3))

  os <- as_OpenSpecy(
    1:3,
    spectra = matrix(seq_len(12), nrow = 3),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  os <- add_visual_image(os, img, bottom_left = origin$bottom_left,
                         top_right = origin$top_right)
  expect_equal(visual_image(os)$bottom_left, c(5, 18))
  expect_equal(visual_image(os)$map_dim, c(2, 2))

  id <- def_features(os, c(TRUE, FALSE, FALSE, FALSE))
  expect_true(check_OpenSpecy(id))
  expect_contains(names(id$metadata), c("r", "g", "b"))
  collapsed <- collapse_spec(def_features(os, c("a", "a", "b", "b")))
  expect_equal(visual_image(collapsed)$map_dim, c(2, 2))
})

test_that("visual color extraction tolerates one map-pixel edge drift", {
  img <- array(0, dim = c(5, 10, 3))
  img[, , 1] <- 1
  os <- as_OpenSpecy(
    1:3,
    spectra = matrix(seq_len(6), nrow = 3),
    metadata = data.frame(x = c(0, 1), y = c(0, 0))
  )
  os <- add_visual_image(os, img, bottom_left = c(8, 4),
                         top_right = c(14, 2))

  id <- def_features(os, c("particle", "particle"))

  expect_true(all(stats::complete.cases(id$metadata[, c("r", "g", "b")])))
  expect_equal(id$metadata$r, c(255L, 255L))
})

test_that("visual images read uncompressed BMP bytes without grDevices readbitmap", {
  bmp <- as.raw(c(
    0x42, 0x4d, 0x46, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x36, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x18, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x13, 0x0b,
    0x00, 0x00, 0x13, 0x0b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0xff, 0xff, 0xff,
    0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00
  ))

  img <- .read_visual_bmp_bytes(bmp)
  expect_equal(dim(img), c(2L, 2L, 3L))
  expect_equal(img[1, 1, ], c(1, 0, 0))
  expect_equal(img[2, 1, ], c(0, 0, 1))
})

test_that("visual image attributes survive Specs conversion and decompression", {
  os <- as_OpenSpecy(
    1:4,
    spectra = matrix(seq_len(16), nrow = 4),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  img <- array(1, dim = c(5, 5, 3))
  os <- add_visual_image(os, img, bottom_left = c(1, 5),
                         top_right = c(5, 1))

  specs <- as_Specs(os, steps = character())
  expect_false(is.null(visual_image(specs)))
  expanded <- decompress_spec(specs)
  expect_false(is.null(visual_image(expanded)))
})
