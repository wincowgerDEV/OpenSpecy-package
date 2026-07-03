test_that("particle_image() draws with base graphics", {
  expect_false(formals(particle_image)$labels)
  particles <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0, 0, 1, 1),
    material_class = c("poly(ethylene)", "mineral", "NA", "new material"),
    feature_id = c("a", "b", "c", "d")
  )
  file <- tempfile(fileext = ".png")
  png(file)
  plotted <- particle_image(particles, legend = TRUE)
  dev.off()

  expect_s3_class(plotted, "data.table")
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("particle_image() raster grid preserves full cells and background", {
  particles <- data.frame(
    x = c(0, 1, 2, 0, 1, 2),
    y = c(0, 0, 0, 1, 1, 1),
    material_class = c("poly(ethylene)", "background", "mineral",
                       NA, "mineral", "poly(ethylene)")
  )
  material <- as.character(particles$material_class)
  background <- .particle_background_material(material)
  material[background] <- NA_character_
  grid <- .particle_material_grid(particles, material, background)
  pal <- .resolve_particle_palette(material[!background])
  raster <- .particle_grid_raster(grid, pal)

  expect_equal(dim(grid$z), c(3, 2))
  expect_equal(grid$z[1, 1], "poly(ethylene)")
  expect_true(is.na(grid$z[2, 1]))
  expect_true(is.na(grid$z[1, 2]))
  expect_s3_class(raster, "raster")
  expect_equal(dim(raster), c(2, 3))
})

test_that("particle_image() rasterizes over attached visual images", {
  coords <- expand.grid(x = 0:1, y = 0:1)
  map <- as_OpenSpecy(
    1:3,
    spectra = matrix(seq_len(12), nrow = 3,
                     dimnames = list(NULL, paste0("cell_", 1:4))),
    metadata = data.frame(coords,
                          material_class = c("poly(ethylene)", "background",
                                             "mineral", "poly(ethylene)"))
  )
  img <- array(1, dim = c(6, 6, 3))
  map <- add_visual_image(map, img, bottom_left = c(1, 6),
                          top_right = c(6, 1))
  file <- tempfile(fileext = ".png")
  png(file)
  plotted <- particle_image(map, legend = TRUE)
  dev.off()

  expect_s3_class(plotted, "data.table")
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})
