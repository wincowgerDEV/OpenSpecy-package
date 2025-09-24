map <- read_any(read_extdata("CA_tiny_map.zip"))
data("raman_hdpe")


test_that("adding centroids no error", {

    map$metadata$snr <- sig_noise(map, metric = "sig_times_noise")
    
    id_map <- def_features(map, map$metadata$snr > 0.1)
    cent <- collapse_spec(id_map)
    
    heatmap_spec(centroids = cent)
    
    heatmap_spec(map, sn = map$metadata$snr) |> expect_silent()
    
    heatmap_spec(map, sn = map$metadata$snr, centroids = cent) |> expect_silent()
    
    heatmap_spec(id_map, z = id_map$metadata$feature_id, centroids = cent) |> expect_silent()
})

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
