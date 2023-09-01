map <- read_zip(read_extdata("CA_tiny_map.zip"))
data("raman_hdpe")

test_that("heatmap_spec() generates 'plotly' object", {
  heatmap_spec(map, z = map$metadata$y) |>
    expect_silent() |>
    expect_s3_class("plotly")
})

test_that("plotly_spec() generates 'plotly' object", {
  plotly_spec(x = raman_hdpe, x2 = raman_hdpe) |>
    expect_silent() |>
    expect_s3_class("plotly")
})

test_that("interactive_plot() generates 'plotly' object", {
  interactive_plot(map, x2 = raman_hdpe, select = 2) |>
    suppressWarnings() |>
    expect_s3_class("plotly")
})
