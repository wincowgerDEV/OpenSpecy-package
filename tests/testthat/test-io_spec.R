# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

library(data.table)

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
  expect_silent(csv <- read_text(read_extdata("raman_hdpe.csv")))
  expect_warning(
    expect_message(read_text(read_extdata("raman_hdpe.csv"), share = tmp))
  )
  expect_silent(
    dtf <- read_text(read_extdata("raman_hdpe.csv"), method = "fread")
  )
  expect_error(read_text(read_extdata("ftir_pva_without_header.csv"),
                         method = "read.csv"))
  expect_warning(
    expect_warning(
      read_text(read_extdata("ftir_pva_without_header.csv"),
                           method = "read.csv", header = F)
      )
  )
  expect_warning(
    expect_warning(
      read_text(read_extdata("ftir_pva_without_header.csv"))
    )
  )
  expect_s3_class(csv, "OpenSpecy")
  expect_equal(names(csv), c("wavenumber", "spectra", "coords"))
  expect_equal(length(csv$wavenumber), 1095)
  expect_equal(round(range(csv$wavenumber), 1), c(150.9, 2998.5))
  expect_equal(round(range(csv$spectra[[1]]), 1), c(3264.2, 41238.9))
  expect_equal(csv, dtf)
})

test_that("read_asp() gives expected output", {
  expect_silent(asp <- read_asp(read_extdata("ftir_ldpe_soil.asp")))
  expect_error(read_asp(read_extdata("raman_hdpe.csv")))
  expect_s3_class(asp, "OpenSpecy")
  expect_equal(names(asp), c("wavenumber", "spectra", "coords"))
  expect_equal(length(asp$wavenumber), 1798)
  expect_equal(round(range(asp$wavenumber), 1), c(650.4, 3999.4))
  expect_equal(round(range(asp$spectra[[1]]), 4), c(0.0010, 0.5182))
})

test_that("read_spa() gives expected output", {
  expect_silent(spa <- read_spa(read_extdata("ftir_polyethylene_reflectance_adjustment_not_working.spa")))
  expect_error(read_spa(read_extdata("raman_hdpe.csv")))
  expect_s3_class(spa, "OpenSpecy")
  expect_equal(names(spa), c("wavenumber", "spectra", "coords"))
  expect_equal(length(spa$wavenumber), 1738)
  expect_equal(round(range(spa$wavenumber), 1), c(649.9, 3999.8))
  expect_equal(round(range(spa$spectra[[1]]), 2), c(61.51, 102.88))
})

test_that("read_jdx() gives expected output", {
  expect_match(capture_messages(
    suppressWarnings(jdx <- read_jdx(read_extdata("fitr_nitrocellulose.jdx"),
                                     encoding = "latin1"))
  ), "JDX file inconsistency.*"
  )
  expect_error(read_jdx(read_extdata("throws_error_raman_1000002.jdx")))
  expect_error(read_jdx(read_extdata("raman_hdpe.csv")))
  expect_s3_class(jdx, "OpenSpecy")
  expect_equal(names(jdx), c("wavenumber", "spectra", "coords"))
  expect_equal(length(jdx$wavenumber), 7154)
  expect_equal(round(range(jdx$wavenumber), 1), c(599.9, 7499.0))
  expect_equal(round(range(jdx$spectra[[1]]), 4), c(0.0106, 0.6989))
})

test_that("read_spc() gives expected output", {
  expect_silent(spc <- read_spc(read_extdata("raman_atacamit.spc")))
  expect_error(read_spc(read_extdata("raman_hdpe.csv")))
  expect_s3_class(spc, "OpenSpecy")
  expect_equal(names(spc), c("wavenumber", "spectra", "coords"))
  expect_equal(length(spc$wavenumber), 559)
  expect_equal(round(range(spc$wavenumber), 1), c(117.8, 1050.0))
  expect_equal(round(range(spc$spectra[[1]]), 2), c(0.08, 585.51))
})

test_that("read_opus() gives expected output", {
  expect_silent(opus <- read_opus(read_extdata("ftir_ps.0")))
  expect_error(read_opus(read_extdata("raman_hdpe.csv")))
  expect_s3_class(opus, "OpenSpecy")
  expect_equal(names(opus), c("wavenumber", "spectra", "coords"))
  expect_equal(length(opus), 2126)
  expect_equal(round(range(opus$wavenumber), 1), c(399.2, 4497.5))
  expect_equal(round(range(opus$spectra[[1]]), 4), c(0.0130, 0.6112))
})

# Tidy up
unlink(tmp, recursive = T)