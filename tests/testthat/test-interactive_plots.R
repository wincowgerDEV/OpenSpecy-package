map <- read_zip(read_extdata("CA_tiny_map.zip"))
data("raman_hdpe")

test_that("plotly_spec() handles input errors correctly", {
  plotly_spec(1:1000) |> expect_error()
})

test_that("plotly_spec() generates 'plotly' object", {
  plotly_spec(raman_hdpe, select = 1) |>
    expect_silent() |>
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

test_that("interactive_plot() generates 'plotly' object", {
  interactive_plot(map, x2 = raman_hdpe, select = 2) |>
    suppressWarnings() |>
    expect_s3_class("plotly")
})
