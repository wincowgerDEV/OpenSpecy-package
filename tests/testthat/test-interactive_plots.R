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
    heatmap_spec(map, z = map$metadata$y, type = "static") |>
        expect_silent() 
})

test_that("interactive_plot() generates 'plotly' object", {
  interactive_plot(map, x2 = raman_hdpe, select = 2) |>
        expect_s3_class("plotly")
})
