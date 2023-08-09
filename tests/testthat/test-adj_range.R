test_that("restrict_range() provides correct range", {
  test_noise = as_OpenSpecy(x = seq(400,4000, by = 10), spectra = data.table(intensity = rnorm(361)))
  expect_silent(single_range <- restrict_range(test_noise, min_range = 1000, max_range = 2000))
  expect_silent(double_range <- restrict_range(test_noise, min_range = c(1000, 2000) , max_range = c(1500, 2500)))
  expect_true(is_OpenSpecy(single_range))
  expect_true(is_OpenSpecy(double_range))
  expect_identical(single_range$wavenumber, seq(1000,2000, by = 10))
  expect_identical(double_range$wavenumber, c(seq(1000,1500, by = 10), seq(2000,2500, by = 10)))
})

test_that("flatten_range() function test", {
    #spectrum <- read_any(read_extdata(file = "ftir_ps.0"))
    test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
    flattened_data <- flatten_range(test, min_range = c(4, 7), max_range = c(5, 10), make_rel = F)
    expect_equal(flattened_data$spectra$V1[4:5], c(4.5, 4.5))
    expect_equal(flattened_data$spectra$V1[7:10], c(8.5, 8.5, 8.5, 8.5))
})


test_that("flatten_range() error handling", {
  test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
    expect_error(flatten_range(test), "You need to specify a min and max range to flatten.")
    expect_error(flatten_range(test, min_range = c(1000), max_range = c(2000, 3000)), "min_range and max_range need to be the same length.")
    expect_error(flatten_range(test, min_range = c(2000), max_range = c(1000)), "all min_range values must be lower than corresponding max_range")
})
