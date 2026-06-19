data("raman_hdpe")
tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()

test_that("process_spec() handles input errors corretly", {
  process_spec(1:1000) |> expect_error()
})

test_that("process_spec() returns expected values", {
  process_spec(raman_hdpe, adj_intens = T, conform_spec = T, restrict_range = T,
               flatten_range = T, subtr_baseline = T, smooth_intens = T) |>
    expect_silent()

  conf <- process_spec(tiny_map) |> expect_silent()

  expected_conf <- conform_spec(tiny_map, range = NULL, res = 5) |>
    smooth_intens(polynomial = 3,
                  window = 11,
                  derivative = 1,
                  abs = T,
                  make_rel = T)
  attr(expected_conf, "derivative_order") <- "1"
  expect_equal(conf, expected_conf)
  expect_true(check_OpenSpecy(conf))
  
  proc <- process_spec(raman_hdpe,
                       smooth_intens = TRUE,
                       smooth_intens_args = list(
                         polynomial = 3,
                         window = 11,
                         derivative = 1
                       )) |> expect_silent()
  
  expect_true(check_OpenSpecy(proc))
  
  expected_proc <- conform_spec(raman_hdpe) |>
    smooth_intens(derivative = 1, make_rel = T)
  attr(expected_proc, "derivative_order") <- "1"
  expect_equal(proc, expected_proc)
})

test_that("process_spec() automatically manages NA and updates attributes", {
  data(raman_hdpe)
  os <- raman_hdpe
  os$spectra[1:10, 1] <- NA

  derivative <- process_spec(
    os,
    conform_spec = FALSE,
    smooth_intens_args = list(window = 15, derivative = 1),
    make_rel = TRUE
  )
  expect_true(all(is.na(derivative$spectra[1:10, 1])))
  expect_true(any(is.finite(derivative$spectra[-(1:10), 1])))
  expect_equal(attr(derivative, "derivative_order"), "1")

  baseline <- process_spec(
    os,
    conform_spec = FALSE,
    smooth_intens = FALSE,
    subtr_baseline = TRUE,
    make_rel = TRUE
  )
  expect_equal(attr(baseline, "baseline"), "nobaseline")
})
