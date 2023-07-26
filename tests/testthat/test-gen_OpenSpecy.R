
# Define a setup before running the tests.
test_data <- read_any(read_extdata("raman_hdpe.csv"))

# Test: print.OpenSpecy() should print the OpenSpecy object.
test_that("print.OpenSpecy prints the OpenSpecy object", {
    expect_output(print.OpenSpecy(test_data))
})

# Test: head.OpenSpecy() should return the first few lines of the OpenSpecy object.
test_that("head.OpenSpecy returns the first few lines of the OpenSpecy object", {
    result <- head(test_data)
    expect_equal(nrow(result), 6)
})

# Test: plot.OpenSpecy() should return a ggplot object.
test_that("plot.OpenSpecy returns a plotly object", {
    result <- plot_OpenSpecy(test_data)
    expect_s3_class(result, "plotly")
})

# Test: sample.OpenSpecy() should return an OpenSpecy object with a subset of the spectra.
test_that("sample.OpenSpecy returns an OpenSpecy object with a subset of the spectra", {
    sample_OpenSpecy <- read_any(read_extdata("CA_tiny_map.zip"))
    result <- sample.OpenSpecy(sample_OpenSpecy, size = 5)
    expect_s3_class(result, "OpenSpecy")
    expect_equal(ncol(result$spectra), 5)
})
