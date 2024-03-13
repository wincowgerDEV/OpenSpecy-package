# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("ENVI files are read", {
  tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any() |>
    expect_silent()
  read_extdata("CA_tiny_map.zip") |> read_any(share = tmp) |>
    expect_message() |> expect_warning()

  expect_s3_class(tiny_map, "OpenSpecy")
  expect_true(check_OpenSpecy(tiny_map))

  expect_equal(ncol(tiny_map$spectra), 208)
  expect_length(tiny_map$wavenumber, 427)

  range(tiny_map$wavenumber) |> round(1) |>
    expect_equal(c(717.4, 4003.7))
  range(tiny_map$spectra) |> round(2) |>
    expect_equal(c(-1.32, 1.17))
  tiny_map$spectra[c(1,427), c(1,45)] |> round(2) |> unlist() |> as.numeric() |>
    expect_equal(c(-0.86, -0.88, -0.62, -0.64))

  names(tiny_map$metadata) |>
    expect_contains(c("x", "y", "file_name", "file_id", "description",
                      "pixel size"))
})

# Tidy up
unlink(tmp, recursive = T)
