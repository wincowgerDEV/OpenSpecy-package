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

test_that("ENVI ZIP members stream without a full extraction directory", {
  archive <- read_extdata("CA_tiny_map.zip")
  legacy_extraction <- file.path(tempdir(), "OpenSpecy-unzip")
  unlink(legacy_extraction, recursive = TRUE, force = TRUE)
  dir.create(legacy_extraction)
  sentinel <- file.path(legacy_extraction, "streaming-sentinel")
  writeLines("keep", sentinel)
  on.exit(unlink(legacy_extraction, recursive = TRUE, force = TRUE), add = TRUE)

  streamed <- read_zip(archive)
  expect_true(file.exists(sentinel))

  extracted <- tempfile("OpenSpecy-envi-reference-")
  dir.create(extracted)
  on.exit(unlink(extracted, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(archive, exdir = extracted)
  dat <- list.files(extracted, pattern = "\\.dat$", full.names = TRUE,
                    ignore.case = TRUE)
  hdr <- list.files(extracted, pattern = "\\.hdr$", full.names = TRUE,
                    ignore.case = TRUE)
  reference <- read_envi(dat, hdr)

  expect_identical(streamed$wavenumber, reference$wavenumber)
  expect_identical(streamed$spectra, reference$spectra)
  expect_identical(streamed$metadata, reference$metadata)
  expect_identical(attributes(streamed$spectra),
                   attributes(reference$spectra))

  policy <- specs_background_filter(
    metric = "run_sig_over_noise", minimum = -Inf, maximum = Inf,
    sigma = c(0.5, 0.5, 0.5)
  )
  compact <- read_zip(
    archive, representation = "Specs", background_filter = policy,
    spectral_smooth = TRUE, sigma = c(0.5, 0.5, 0.5)
  )
  expect_s3_class(compact, "Specs")
  expect_true(check_Specs(compact))
  expect_true(file.exists(sentinel))
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

    bytes <- readBin(binary, what = "raw", n = file.info(binary)$size)
    binary_connection <- rawConnection(bytes, open = "rb")
    streamed <- read_envi(
      binary_connection, header, metadata = list(file_name = "map.dat")
    )
    close(binary_connection)
    expect_identical(streamed, object)
  }
})

test_that("compact ENVI background filtering matches whole-cube smoothing", {
  nx <- 4L
  ny <- 70L
  n_bands <- 5L
  pixels <- nx * ny
  spectra <- vapply(seq_len(pixels), function(i) {
    x <- (i - 1L) %% nx
    y <- (i - 1L) %/% nx
    seq_len(n_bands) / 10 + x / 5 + y / 50
  }, numeric(n_bands))
  binary <- tempfile(fileext = ".dat")
  header <- tempfile(fileext = ".hdr")
  on.exit(unlink(c(binary, header)), add = TRUE)
  connection <- file(binary, "wb")
  writeBin(as.vector(spectra), connection, size = 4L, endian = "little")
  close(connection)
  writeLines(c(
    "ENVI", paste("samples =", nx), paste("lines =", ny),
    paste("bands =", n_bands), "header offset = 0", "data type = 4",
    "interleave = bip", "byte order = 0",
    "wavelength = {1000, 1001, 1002, 1003, 1004}"
  ), header)

  cube <- aperm(array(spectra, dim = c(n_bands, nx, ny)), c(3, 2, 1))
  smoothed <- mmand::gaussianSmooth(cube, sigma = c(1, 1, 1))
  smooth_matrix <- matrix(aperm(smoothed, c(3, 2, 1)), nrow = n_bands)
  reference <- as_OpenSpecy(
    1000:1004, spectra = smooth_matrix,
    metadata = data.frame(x = rep(0:(nx - 1L), times = ny),
                          y = rep(0:(ny - 1L), each = nx))
  )
  metric <- sig_noise(reference, metric = "sig", abs = FALSE)
  threshold <- stats::median(metric)
  policy <- specs_background_filter(
    metric = "sig", minimum = threshold, sigma = c(1, 1, 1), step = 1
  )
  compact <- read_envi(
    binary, header, representation = "Specs", background_filter = policy,
    metadata = list(file_name = "map.dat")
  )

  expect_s3_class(compact, "Specs")
  expect_true(check_Specs(compact))
  expect_equal(specs_background_mask(compact),
               unname(!(is.finite(metric) & metric > threshold)))
  expect_equal(attr(compact, "background")$signal_to_noise, unname(metric),
               tolerance = 1e-7)
  expect_equal(ncol(compact$values), sum(metric > threshold))
  expect_equal(unname(specs_source_values(compact, c(1, pixels))[, 2L]),
               spectra[, pixels], tolerance = 1e-7)
})

test_that("compact ENVI supports BIP, BIL, and BSQ without changing values", {
  nx <- 3L
  ny <- 2L
  n_bands <- 3L
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
      "ENVI", paste("samples =", nx), paste("lines =", ny),
      paste("bands =", n_bands), "header offset = 0", "data type = 4",
      paste("interleave =", interleave), "byte order = 0",
      "wavelength = {1000, 1001, 1002}"
    ), header)
    compact <- read_envi(
      binary, header, representation = "Specs",
      metadata = list(file_name = "map.dat")
    )
    expect_true(check_Specs(compact))
    expect_identical(unname(specs_source_values(compact)), expected)
  }
})

# Tidy up
unlink(tmp, recursive = T)
