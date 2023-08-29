test_that("process_spec() returns expected values", {
  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()

  conf <- process_spec(tiny_map) |> expect_silent()
  expect_equal(conf, conform_spec(tiny_map, range = NULL, res = 5) |>
                 smooth_intens(polynomial = 3,
                               window = 11,
                               derivative = 1,
                               abs = T,
                               make_rel = T))


  proc <- process_spec(raman_hdpe,
                       smooth_intens = TRUE,
                       smooth_intens_args = list(
                         polynomial = 3,
                         window = 11,
                         derivative = 1
                       )
  ) |> expect_silent()

  expect_equal(proc, conform_spec(raman_hdpe) |>
                 smooth_intens(derivative = 1, make_rel = T))
})
