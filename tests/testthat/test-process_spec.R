test_that("process_spec() returns expected values", {
  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()

  conf <- process_spec(tiny_map) |> expect_silent()
  expect_equal(conf, conform_spec(tiny_map))

  proc <- process_spec(raman_hdpe,
                       smooth_intens = TRUE,
                       smooth_intens_args = list(
                         polynomial = 3,
                         window = 11,
                         derivative = 1
                       )
  ) |> expect_silent()

  expect_equal(proc, conform_spec(raman_hdpe) |> smooth_intens(derivative = 1,
                                                               make_rel = F))
})

test_that("sample_spec() returns a subset of the spectra", {
  tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
  sampled <- sample_spec(tiny_map, size = 5)
  expect_s3_class(sampled, "OpenSpecy")
  expect_equal(ncol(sampled$spectra), 5)
})
