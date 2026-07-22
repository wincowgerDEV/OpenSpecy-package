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

test_that("process_spec() bulk-processes complete columns beside NA columns", {
  data(raman_hdpe)
  spectra <- cbind(
    complete = raman_hdpe$spectra[, 1],
    missing = raman_hdpe$spectra[, 1]
  )
  spectra[1:10, "missing"] <- NA
  os <- as_OpenSpecy(raman_hdpe$wavenumber, spectra)

  processed <- process_spec(
    os,
    conform_spec = FALSE,
    smooth_intens_args = list(window = 15, derivative = 1),
    make_rel = TRUE
  )

  complete_expected <- process_spec(
    filter_spec(os, "complete"),
    conform_spec = FALSE,
    smooth_intens_args = list(window = 15, derivative = 1),
    make_rel = TRUE
  )
  missing_expected <- manage_na(
    filter_spec(os, "missing"),
    fun = function(x) {
      process_spec(
        x,
        conform_spec = FALSE,
        smooth_intens_args = list(window = 15, derivative = 1),
        make_rel = TRUE
      )
    }
  )

  expect_equal(processed$spectra[, "complete"],
               complete_expected$spectra[, "complete"])
  expect_equal(processed$spectra[, "missing"],
               missing_expected$spectra[, "missing"])
  expect_equal(attr(processed, "derivative_order"), "1")
})

test_that("process_spec() passes automated range policies through", {
  wavenumber <- seq(1000, 2500, by = 10)
  values <- rep(0.1, length(wavenumber))
  values[wavenumber == 1200] <- 1
  values[seq_len(2L)] <- 4
  values[wavenumber >= 2200 & wavenumber <= 2400] <- 4
  os <- as_OpenSpecy(wavenumber, data.frame(sample = values))
  processed <- process_spec(
    os,
    conform_spec = FALSE,
    restrict_range = TRUE,
    restrict_range_args = list(automate = TRUE),
    flatten_range = TRUE,
    flatten_range_args = list(automate = TRUE),
    smooth_intens = FALSE,
    make_rel = FALSE
  )

  expect_lt(nrow(processed$spectra), nrow(os$spectra))
  expect_true(attr(processed, "automatic_tail")$applied)
  expect_true(attr(processed, "automatic_flatten")$applied)
  expect_true(check_OpenSpecy(processed))
})
