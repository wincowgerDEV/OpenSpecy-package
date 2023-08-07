test_that("flatten_range.OpenSpecy() function test", {
    #spectrum <- read_any(read_extdata(file = "ftir_ps.0"))
    test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
    flattened_data <- flatten_range(object = test, min_range = c(4, 7), max_range = c(5, 10), make_rel = F)
    expect_equal(flattened_data$spectra$V1[4:5], c(4.5, 4.5))
    expect_equal(flattened_data$spectra$V1[7:10], c(8.5, 8.5, 8.5, 8.5))
})


test_that("flatten_range() error handling", {
    expect_error(flatten_range(test_data), "You need to specify a min and max range to flatten.")
    expect_error(flatten_range(test_data, min_range = c(1000), max_range = c(2000, 3000)), "min_range and max_range need to be the same length.")
    expect_error(flatten_range(test_data, min_range = c(2000), max_range = c(1000)), "all min_range values must be lower than corresponding max_range")
})
