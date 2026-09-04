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
