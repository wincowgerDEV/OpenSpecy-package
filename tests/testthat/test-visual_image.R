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

  id <- def_features(os, c(TRUE, FALSE, FALSE, FALSE))
  expect_true(check_OpenSpecy(id))
  expect_contains(names(id$metadata), c("r", "g", "b"))
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
