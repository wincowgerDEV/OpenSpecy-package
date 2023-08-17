
test_that("heatmap_spec generates the correct heatmap", {
    # Create a test OpenSpecy object
    map <- read_zip(read_extdata("CA_tiny_map.zip"))
    heat_map <- heatmap_spec(map, z = map$metadata$y)
    
    # Test whether the heatmap object is a plotly object
    expect_true("plotly" %in% class(heat_map))
    
})

test_that("plotly_spec generates the correct spectra plot", {
    # Create a test OpenSpecy object
    data1 <- read_any(read_extdata("raman_hdpe.json"))
    spectra_plot <- plotly_spec(data1, select = 1)
    
    # Test whether the spectra_plot object is a plotly object
    expect_true("plotly" %in% class(spectra_plot))
    
})

test_that("interactive_plot generates the correct interactive plot", {
    # Create test OpenSpecy objects
    data1 <- read_any(read_extdata("raman_hdpe.json"))
    data2 <- read_zip(read_extdata("CA_tiny_map.zip"))
    interactive_plot <- suppressWarnings(interactive_plot(x2 = data1, select2 = 1, x = data2, select = 2))
    
    # Test whether the interactive_plot object is a subplot object
    expect_true("plotly" %in% class(interactive_plot))
    
})
