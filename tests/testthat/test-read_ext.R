# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

library(data.table)

data("raman_hdpe")

test_that("extdata files are present", {
  ed <- read_extdata()
  expect_true(any(grepl("\\.asp$", ed)))
  expect_true(any(grepl("\\.csv$", ed)))
  expect_true(any(grepl("\\.0$", ed)))
  expect_true(any(grepl("\\.jdx$", ed)))
  expect_true(any(grepl("\\.spa$", ed)))
  expect_true(any(grepl("\\.spc$", ed)))
})

test_that("read_text() gives expected output", {
  csv <- read_extdata("raman_hdpe.csv") |> read_text() |>
    expect_silent()
  # expect_warning(
  #   expect_message(read_text(read_extdata("raman_hdpe.csv"), share = tmp))
  # )
  dtf <- read_extdata("raman_hdpe.csv") |> read_text(method = "fread") |>
    expect_silent()
  read_extdata("ftir_pva_without_header.csv") |> read_text() |>
    expect_warning() |> expect_warning()
  read_extdata("ftir_pva_without_header.csv") |> read_text(header = F) |>
    expect_warning() |> expect_warning()
  read_extdata("ftir_pva_without_header.csv") |> read_text(method = "fread") |>
    expect_warning() |> expect_warning()

  expect_s3_class(csv, "OpenSpecy")
  expect_equal(names(csv), c("wavenumber", "spectra", "metadata"))
  expect_equal(csv$wavenumber, raman_hdpe$wavenumber)
  expect_equal(csv$spectra, raman_hdpe$spectra)
  expect_equal(csv, dtf)
})

test_that("read_asp() gives expected output", {
  asp <- read_extdata("ftir_ldpe_soil.asp") |> read_asp() |>
    expect_silent()
  read_extdata("raman_hdpe.csv") |> read_asp() |>
    expect_error()

  expect_s3_class(asp, "OpenSpecy")
  expect_equal(names(asp), c("wavenumber", "spectra", "metadata"))
  expect_length(asp$wavenumber, 1798)
  range(asp$wavenumber) |> round(1) |>
    expect_equal(c(650.4, 3999.4))
  range(asp$spectra) |> round(4) |>
    expect_equal(c(0.0010, 0.5182))
})

test_that("read_spa() gives expected output", {
  spa <- read_extdata("ftir_polyethylene_reflectance_adjustment_not_working.spa") |>
    read_spa() |> expect_silent()
  read_extdata("raman_hdpe.csv") |> read_spa() |>
    expect_error()

  expect_s3_class(spa, "OpenSpecy")
  expect_equal(names(spa), c("wavenumber", "spectra", "metadata"))
  expect_length(spa$wavenumber, 1738)
  range(spa$wavenumber) |> round(1) |>
    expect_equal(c(649.9, 3999.8))
  range(spa$spectra) |> round(2) |>
    expect_equal(c(61.51, 102.88))
})

test_that("read_jdx() gives expected output", {
  suppressWarnings(jdx <- read_jdx(read_extdata("fitr_nitrocellulose.jdx"),
                                   encoding = "latin1")) |>
    capture_messages() |>
    expect_match("JDX file inconsistency.*")
  read_extdata("throws_error_raman_1000002.jdx") |> read_jdx() |>
    expect_error()
  read_extdata("raman_hdpe.csv") |> read_jdx() |>
    expect_error()

  expect_s3_class(jdx, "OpenSpecy")
  expect_equal(names(jdx), c("wavenumber", "spectra", "metadata"))
  expect_length(jdx$wavenumber, 7154)
  range(jdx$wavenumber) |> round(1) |>
    expect_equal(c(599.9, 7499.0))
  range(jdx$spectra) |> round(4) |>
    expect_equal(c(0.0106, 0.6989))
})

test_that("read_spc() gives expected output", {
  spc <- read_extdata("raman_atacamit.spc") |> read_spc() |>
    expect_silent()
  read_extdata("raman_hdpe.csv") |> read_spc() |>
    expect_error()

  expect_s3_class(spc, "OpenSpecy")
  expect_equal(names(spc), c("wavenumber", "spectra", "metadata"))
  expect_length(spc$wavenumber, 559)
  range(spc$wavenumber) |> round(1) |>
    expect_equal(c(117.8, 1050.0))
  range(spc$spectra) |> round(2) |>
    expect_equal(c(0.08, 585.51))
})

# Tidy up
unlink(tmp, recursive = T)
