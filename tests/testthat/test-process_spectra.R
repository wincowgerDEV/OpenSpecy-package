test_that("process returns expected values",{
    data <- read_any(read_extdata("CA_tiny_map.zip"))
    expect_silent(processed <- process_spec(data))
    expect_true(is_OpenSpecy(processed))
})

# Test: sample_spec() should return an OpenSpecy object with a subset of the spectra.
test_that("sample_spec returns an OpenSpecy object with a subset of the spectra", {
  tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
  sampled <- sample_spec(tiny_map, size = 5)
  expect_s3_class(result, "OpenSpecy")
  expect_equal(ncol(result$spectra), 5)
})
