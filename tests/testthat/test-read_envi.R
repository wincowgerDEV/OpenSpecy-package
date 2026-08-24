# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("ENVI files are read", {
  tiny_map <- read_extdata("CA_tiny_map.zip") |> 
      read_any() |>
      expect_silent()
  read_extdata("CA_tiny_map.zip") |> read_any() |>
    expect_silent()

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

test_that("ENVI interleaves keep the same band-by-pixel format", {
  nx <- 3L
  ny <- 2L
  n_bands <- 2L
  expected <- matrix(as.numeric(seq_len(nx * ny * n_bands)),
                     nrow = n_bands)

  binary_values <- list(
    bip = as.vector(expected),
    bil = unlist(lapply(seq_len(ny), function(row) {
      columns <- ((row - 1L) * nx + 1L):(row * nx)
      as.vector(t(expected[, columns, drop = FALSE]))
    }), use.names = FALSE),
    bsq = as.vector(t(expected))
  )

  for (interleave in names(binary_values)) {
    binary <- tempfile(fileext = ".dat")
    header <- tempfile(fileext = ".hdr")
    on.exit(unlink(c(binary, header)), add = TRUE)
    connection <- file(binary, "wb")
    writeBin(binary_values[[interleave]], connection, size = 4L,
             endian = "little")
    close(connection)
    writeLines(c(
      "ENVI",
      paste("samples =", nx),
      paste("lines =", ny),
      paste("bands =", n_bands),
      "header offset = 0",
      "data type = 4",
      paste("interleave =", interleave),
      "byte order = 0",
      "wavelength = {1000, 1001}"
    ), header)

    object <- read_envi(binary, header, metadata = list(file_name = "map.dat"))
    expect_s3_class(object, "OpenSpecy")
    expect_true(check_OpenSpecy(object))
    expect_identical(unname(object$spectra), expected)
    expect_identical(object$wavenumber, c(1000, 1001))
    expect_identical(object$metadata$x, c(0, 1, 2, 0, 1, 2))
    expect_identical(object$metadata$y, c(0, 0, 0, 1, 1, 1))
    expect_identical(colnames(object$spectra),
                     c("0_0", "0_1", "0_2", "1_0", "1_1", "1_2"))
  }
})

# Tidy up
unlink(tmp, recursive = T)
