map <- read_any(read_extdata("CA_tiny_map.zip"))
data("raman_hdpe")

test_that("plotly_spec() handles input errors correctly", {
  plotly_spec(1:1000) |> expect_error()
})

test_that("plotly_spec() generates 'plotly' object", {
  plotly_spec(x = raman_hdpe, x2 = raman_hdpe) |>
    expect_silent() |>
    expect_s3_class("plotly")
})

test_that("interactive_plot() generates 'plotly' object with single or multiple spectra from map", {
  interactive_plot(map, x2 = raman_hdpe, select = 2:3) |>
        expect_s3_class("plotly")
})

test_that("heatmap_spec() handles input errors correctly", {
  heatmap_spec(1:1000) |> expect_error()
})

test_that("heatmap_spec() generates 'plotly' object", {
  heatmap_spec(map, z = map$metadata$y) |>
    expect_silent() |>
    expect_s3_class("plotly")
})

test_that("heatmap_spec() generates static object", {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  heatmap_spec(map, z = map$metadata$y, type = "static") |>
    expect_silent()
})

test_that("heatmap_spec() draws typed particle centroids as points", {
  particles <- as_OpenSpecy(
    c(1000, 1100),
    spectra = matrix(1:6, nrow = 2,
                     dimnames = list(NULL, c("p1", "p2", "p3"))),
    metadata = data.frame(
      x = c(0.25, 4.5, 9.75), y = c(1, 7, 2), area = c(2, 4, 3)
    )
  )
  particles <- OpenSpecy:::.mark_particle_centroid_object(particles)
  expect_identical(attr(particles, "openspecy_object_type"),
                   "particle_centroids")
  built <- plotly::plotly_build(heatmap_spec(particles, z = c(0.2, 0.8, 0.5)))
  expect_identical(built$x$data[[1L]]$type, "scattergl")
  expect_equal(as.numeric(unlist(built$x$data[[1L]]$x)), particles$metadata$x)
  expect_equal(as.numeric(unlist(built$x$data[[1L]]$y)), particles$metadata$y)

  attr(particles, "openspecy_object_type") <- NULL
  expect_true(OpenSpecy:::.is_particle_centroid_object(particles))
  expect_identical(
    plotly::plotly_build(heatmap_spec(map, z = map$metadata$y))$x$data[[1L]]$type,
    "heatmap"
  )
})

test_that("spatial crop keeps inclusive centroids in dense and compact data", {
  dense <- as_OpenSpecy(
    1:2, spectra = matrix(1:8, nrow = 2),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  cropped_dense <- OpenSpecy:::.crop_spatial_spec(
    dense, c(xmin = 1, xmax = 2, ymin = 0, ymax = 1)
  )
  expect_equal(ncol(cropped_dense$spectra), 2L)
  expect_equal(cropped_dense$metadata$x, c(1, 1))
  expect_identical(attr(cropped_dense, "spatial_crop")$retained, 2L)

  compact <- Specs(
    dense$wavenumber, dense$spectra,
    coords = data.frame(
      x = dense$metadata$x, y = dense$metadata$y,
      source_id = colnames(dense$spectra),
      value_id = colnames(dense$spectra)
    )
  )
  cropped_compact <- OpenSpecy:::.crop_spatial_spec(
    compact, c(1, 2, 0, 1)
  )
  expect_s3_class(cropped_compact, "Specs")
  expect_equal(specs_source_count(cropped_compact), 2L)
  expect_equal(specs_coordinates(cropped_compact)$x, c(1, 1))
  expect_error(
    OpenSpecy:::.crop_spatial_spec(dense, c(2, 2, 0, 1)),
    "positive width"
  )
  expect_error(
    OpenSpecy:::.crop_spatial_spec(dense, c(10, 11, 10, 11)),
    "does not contain"
  )
})

test_that("plotly_spec() adds a quantitative logistic weight background", {
  coefficients <- data.table::data.table(
    dimensions_used = c(1L, 3L),
    dimension_units = c(-2, 1),
    variable = 1L,
    name = "ftir_test class",
    names = c(800, 1000)
  )
  model <- list(
    model_type = "logistic_regression",
    coefficients = coefficients,
    dimension_conversion = data.table::data.table(
      factor_num = 1L, name = "ftir_test class"
    ),
    all_variables = c(800, 900, 1000)
  )
  weights <- model_class_weights(model, "ftir_test class")
  expect_equal(weights$weight, c(-2, 0, 1))

  x <- as_OpenSpecy(
    c(800, 900, 1000),
    spectra = data.frame(sample = c(0, 1, 0))
  )
  plot <- plotly_spec(x, model = model, model_class = "ftir_test class")
  expect_s3_class(plot, "plotly")
  built <- plotly::plotly_build(plot)
  types <- vapply(built$x$data, function(trace) trace$type, character(1))
  expect_true("heatmap" %in% types)
  heat <- built$x$data[[which(types == "heatmap")[[1L]]]]
  expect_equal(heat$zmin, -2)
  expect_equal(heat$zmax, 2)
  expect_equal(heat$zmid, 0)
})

test_that("interactive_plot() generates 'plotly' object", {
  interactive_plot(map, x2 = raman_hdpe, select = 2) |>
        expect_s3_class("plotly")
})
