test_that("flatten_range() error handling", {
  test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))

  expect_s3_class(test, "OpenSpecy")
  expect_true(check_OpenSpecy(test))

  expect_error(flatten_range(test))
  expect_error(flatten_range(test, min = c(1000),
                             max = c(2000, 3000)))
  expect_error(flatten_range(test, min = c(2000), max = c(1000)))
})

test_that("restrict_range() provides correct range", {
  test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
                             spectra = data.table(intensity = rnorm(361)))
  single_range <- restrict_range(test_noise, min = 1000,
                                 max = 2000) |>
    expect_silent()

  double_range <- restrict_range(test_noise, min = c(1000, 2000),
                                 max = c(1500, 2500)) |>
    expect_silent()

  check_OpenSpecy(single_range) |> expect_true()
  check_OpenSpecy(double_range) |> expect_true()

  expect_identical(single_range$wavenumber, seq(1000,2000, by = 10))
  expect_identical(double_range$wavenumber, c(seq(1000,1500, by = 10),
                                              seq(2000,2500, by = 10)))
})

test_that("flatten_range() function test", {
  sam <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
  flat_sam <- flatten_range(sam, min = c(4, 7), max = c(5, 10),
                             make_rel = F) |>
    expect_silent()

  expect_true(check_OpenSpecy(flat_sam))

  expect_equal(flat_sam$spectra[4:5, "V1"], c(4.5, 4.5))
  expect_equal(flat_sam$spectra[7:10, "V1"], c(8.5, 8.5, 8.5, 8.5))

  data("raman_hdpe")
  flat_hdpe <- flatten_range(raman_hdpe, min = c(500, 1000),
                             max = c(700, 1500)) |>
    expect_silent()
  expect_true(check_OpenSpecy(flat_hdpe))

  expect_equal(flat_hdpe$spectra[1:50, "intensity"],
               make_rel(raman_hdpe$spectra[, "intensity"])[1:50])
  expect_equal(flat_hdpe$spectra[60:100, "intensity"] |> unique() |> round(6),
               0.036709)

  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
  flat_map <- flatten_range(tiny_map, min = c(1000, 2000),
                            max = c(1200, 2400), make_rel = F) |>
    expect_silent()
  expect_true(check_OpenSpecy(flat_map))

  expect_false(all.equal(flat_map$spectra, tiny_map$spectra) |> isTRUE())
  expect_equal(flat_map$spectra[1:20, ], tiny_map$spectra[1:20, ])

  flat_map$spectra[40:60, 1:5] |> unique() |> round(2) |> as.numeric() |>
    expect_equal(c(-0.87, -1.25, -0.83, -1.19, -0.79))
})

make_automatic_range_spec <- function(left = 0L, right = 0L, co2 = FALSE,
                                      second = FALSE) {
  wavenumber <- seq(1000, 2500, by = 10)
  one <- rep(0, length(wavenumber))
  one[wavenumber == 1200] <- 2
  if (left > 0L) one[seq_len(left)] <- 10
  if (right > 0L) one[seq.int(length(one) - right + 1L, length(one))] <- 10
  if (co2) one[wavenumber == 2300] <- 10
  spectra <- data.frame(one = one)
  if (second) {
    two <- rep(0, length(wavenumber))
    two[wavenumber == 1200] <- 2
    two[(length(two) - 2L):length(two)] <- 10
    spectra$two <- two
  }
  as_OpenSpecy(x = wavenumber, spectra = spectra)
}

test_that("automated range functions are exact no-ops when checks pass", {
  clean <- make_automatic_range_spec()

  expect_identical(
    restrict_range(clean, automate = TRUE, make_rel = FALSE),
    clean
  )
  expect_identical(
    flatten_range(clean, automate = TRUE, make_rel = FALSE),
    clean
  )
})

test_that("automated flattening only corrects a flagged CO2 region", {
  os <- make_automatic_range_spec(co2 = TRUE)
  attr(os, "example_attribute") <- "preserved"
  corrected <- flatten_range(os, automate = TRUE, make_rel = FALSE)

  expect_false(identical(corrected$spectra, os$spectra))
  expect_true(attr(corrected, "automatic_flatten")$applied)
  expect_identical(attr(corrected, "example_attribute"), "preserved")
  expect_equal(
    nrow(assess_spec(corrected, checks = "co2_region")),
    0L
  )
})

test_that("automated restriction finds minimal shared batch bounds", {
  os <- make_automatic_range_spec(left = 2L, second = TRUE)
  corrected <- restrict_range(os, automate = TRUE, make_rel = FALSE)

  expect_equal(min(corrected$wavenumber), 1020)
  expect_equal(max(corrected$wavenumber), 2470)
  expect_true(attr(corrected, "automatic_tail")$applied)
  expect_equal(ncol(corrected$spectra), 2L)
  expect_equal(nrow(corrected$metadata), 2L)
  expect_equal(nrow(assess_spec(corrected, checks = "high_tail")), 0L)
})

test_that("automated restriction is transactional at 20 percent", {
  os <- make_automatic_range_spec()
  os$spectra[seq_len(32L), "one"] <- 4^(32:1)
  corrected <- restrict_range(os, automate = TRUE, make_rel = FALSE)

  expect_identical(corrected$wavenumber, os$wavenumber)
  expect_identical(corrected$spectra, os$spectra)
  expect_false(attr(corrected, "automatic_tail")$applied)
  expect_identical(attr(corrected, "automatic_tail")$reason,
                   "max_crop_exceeded")
})

test_that("automated range arguments are validated", {
  os <- make_automatic_range_spec(left = 2L)
  expect_error(restrict_range(os, min = 1000, max = 2000,
                              automate = TRUE), "either")
  expect_error(restrict_range(os, automate = TRUE, artifact_ratio = 1),
               "greater than 1")
  expect_error(flatten_range(os, min = c(1000, 2200),
                             max = c(1100, 2400), automate = TRUE),
               "one flattening range")
})
