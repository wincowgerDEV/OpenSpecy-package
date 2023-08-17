test_that("restrict_range() provides correct range", {
  test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
                             spectra = data.table(intensity = rnorm(361)))
  single_range <- restrict_range(test_noise, min = 1000,
                                 max = 2000) |>
    expect_silent()

  double_range <- restrict_range(test_noise, min = c(1000, 2000),
                                 max = c(1500, 2500)) |>
    expect_silent()

  is_OpenSpecy(single_range) |> expect_true()
  expect_identical(single_range$wavenumber, seq(1000,2000, by = 10))
  expect_identical(double_range$wavenumber, c(seq(1000,1500, by = 10),
                                              seq(2000,2500, by = 10)))
})

test_that("flatten_range() function test", {
  sam <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))
  flat_sam <- flatten_range(sam, min = c(4, 7), max = c(5, 10),
                             make_rel = F) |>
    expect_silent()

  expect_equal(flat_sam$spectra$V1[4:5], c(4.5, 4.5))
  expect_equal(flat_sam$spectra$V1[7:10], c(8.5, 8.5, 8.5, 8.5))

  data("raman_hdpe")
  flat_hdpe <- flatten_range(raman_hdpe, min = c(500, 1000),
                             max = c(700, 1500)) |>
    expect_silent()
  expect_equal(flat_hdpe$spectra$intensity[1:50],
               make_rel(raman_hdpe$spectra$intensity)[1:50])
  expect_equal(flat_hdpe$spectra$intensity[60:100] |> unique() |> round(6),
               0.036709)

  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_zip()
  flat_map <- flatten_range(tiny_map, min = c(1000, 2000),
                            max = c(1200, 2400), make_rel = F) |>
    expect_silent()

  expect_false(all.equal(flat_map$spectra, tiny_map$spectra) |> isTRUE())
  expect_equal(flat_map$spectra[1:20], tiny_map$spectra[1:20])

  expect_equal(flat_map$spectra[40:60, 1:5] |> unique() |> round(4),
               data.table(-0.8694, -0.7769, -0.5828, -0.292, 0.1916))
})

test_that("flatten_range() error handling", {
  test <- as_OpenSpecy(x = 1:10, spectra = data.table(V1 = 1:10))

  expect_error(flatten_range(test))
  expect_error(flatten_range(test, min = c(1000),
                             max = c(2000, 3000)))
  expect_error(flatten_range(test, min = c(2000), max = c(1000)))
})
