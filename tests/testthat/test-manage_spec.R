data("raman_hdpe")

specs <- lapply(c(read_extdata("raman_hdpe.yml"),
                  read_extdata("ftir_ldpe_soil.asp")), read_any)
no_overlap <- specs
no_overlap[[1]] <- restrict_range(no_overlap[[1]], 300, 500)
no_overlap[[2]] <- restrict_range(no_overlap[[2]], 700, 1000)

test_that("c_spec() handles input errors correctly", {
  c_spec(1:1000) |> expect_error()
  c_spec(list(1:1000, 1000:2000)) |> expect_error()
  c_spec(raman_hdpe) |> expect_message()
  c_spec(no_overlap) |> expect_error()
})

test_that("c_spec() merges identical files without range specification", {
  specs <- lapply(c(read_extdata("raman_hdpe.yml"),
                    read_extdata("raman_hdpe.yml")), read_spec)
  same <- c_spec(specs) |> expect_silent()
  expect_true(check_OpenSpecy(same))
  
  expect_equal(same$wavenumber, raman_hdpe$wavenumber)
  expect_equal(same$spectra$intensity, raman_hdpe$spectra$intensity)
})

test_that("c_spec() merges different files with common range", {
  diff <- c_spec(specs, range = "common", res = 5) |>
    expect_silent()
  expect_true(check_OpenSpecy(diff))

  diff$wavenumber[1:2] |> expect_equal(c(655, 660))
  diff$spectra$intensity[1:2] |> round(2) |> expect_equal(c(53.87, 59.00))
  diff$spectra$intensity.1[1:2] |> round(2) |> expect_equal(c(0.03, 0.03))
})

test_that("c_spec() merges different files with specified range", {
  spec <- c_spec(specs, range = c(1000, 2000), res = 5) |>
    expect_silent()

  expect_true(check_OpenSpecy(spec))
  
  spec$wavenumber |> expect_equal(seq(1000, 2000, 5))
  spec$spectra$intensity |>
    expect_equal(
      conform_spec(raman_hdpe, c(1000, 2000), res = 5)$spectra$intensity
      )
})

test_that("sample_spec() returns a subset of the spectra", {
  tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
  sampled <- sample_spec(tiny_map, size = 5)
  expect_s3_class(sampled, "OpenSpecy")
  expect_true(check_OpenSpecy(sampled))
  expect_equal(ncol(sampled$spectra), 5)
})

