test_that("extdata files are present", {
  ed <- read_extdata()
  expect_true(any(grepl("\\.asp$", ed)))
  expect_true(any(grepl("\\.csv$", ed)))
  expect_true(any(grepl("\\.0$", ed)))
  expect_true(any(grepl("\\.jdx$", ed)))
  expect_true(any(grepl("\\.spa$", ed)))
  expect_true(any(grepl("\\.spc$", ed)))
})

test_that("read_asp() gives expected output", {
  expect_silent(asp <- read_asp(read_extdata("ftir_ldpe_soil.asp")))
  expect_error(read_asp(read_extdata("raman_hdpe.csv")))
  expect_s3_class(asp, "data.frame")
  expect_equal(names(asp), c("wavenumber", "intensity"))
  expect_equal(nrow(asp), 1798)
  expect_equal(round(range(asp[1]), 1), c(650.4, 3999.4))
  expect_equal(round(range(asp[2]), 4), c(0.0010, 0.5182))
})

test_that("read_spa() gives expected output", {
  expect_silent(spa <- read_spa(read_extdata("ftir_polyethylene_reflectance_adjustment_not_working.spa")))
  expect_error(read_spa(read_extdata("raman_hdpe.csv")))
  expect_s3_class(spa, "data.frame")
  expect_equal(names(spa), c("wavenumber", "intensity"))
  expect_equal(nrow(spa), 1738)
  expect_equal(round(range(spa[1]), 1), c(649.9, 3999.8))
  expect_equal(round(range(spa[2]), 2), c(61.51, 102.88))
})

test_that("read_jdx() gives expected output", {
  expect_match(capture_messages(
    suppressWarnings(jdx <- read_jdx(read_extdata("fitr_nitrocellulose.jdx")))
    ), "JDX file inconsistency.*"
  )
  expect_error(read_jdx(read_extdata("throws_error_raman_1000002.jdx")))
  expect_error(read_jdx(read_extdata("raman_hdpe.csv")))
  expect_s3_class(jdx, "data.frame")
  expect_equal(names(jdx), c("wavenumber", "intensity"))
  expect_equal(nrow(jdx), 7154)
  expect_equal(round(range(jdx[1]), 1), c(599.9, 7499.0))
  expect_equal(round(range(jdx[2]), 4), c(0.0106, 0.6989))
})

test_that("read_spc() gives expected output", {
  expect_silent(spc <- read_spc(read_extdata("raman_atacamit.spc")))
  expect_error(read_spc(read_extdata("raman_hdpe.csv")))
  expect_s3_class(spc, "data.frame")
  expect_equal(names(spc), c("wavenumber", "intensity"))
  expect_equal(nrow(spc), 559)
  expect_equal(round(range(spc[1]), 1), c(117.8, 1050.0))
  expect_equal(round(range(spc[2]), 2), c(0.08, 585.51))
})

test_that("read_0() gives expected output", {
  expect_silent(f0 <- read_0(read_extdata("ftir_ps.0")))
  expect_error(read_0(read_extdata("raman_hdpe.csv")))
  expect_s3_class(f0, "data.frame")
  expect_equal(names(f0), c("wavenumber", "intensity"))
  expect_equal(nrow(f0), 2126)
  expect_equal(round(range(f0[1]), 1), c(399.2, 4497.5))
  expect_equal(round(range(f0[2]), 4), c(0.0130, 0.6112))
})

