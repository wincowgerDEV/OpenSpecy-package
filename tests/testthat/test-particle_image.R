test_that("particle_image() draws with base graphics", {
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
