# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("raman_hdpe")

test_that("reading in multi files doesn't throw error", {
  
  #Check new configuration
  expect_silent(multi <- read_extdata("testdata_zipped.zip") |> read_any(c_spec = F))
  expect_type(multi, "list")
  expect_false(is_OpenSpecy(multi))
  expect_true(all(vapply(multi, is_OpenSpecy, FUN.VALUE = logical(1))))
    
  expect_message(multi <- read_extdata("testdata_zipped.zip") |> read_any() |> c_spec())
  expect_silent(multi <- read_extdata("testdata_zipped.zip") |> read_any())
  expect_s3_class(multi, "OpenSpecy")

  expect_equal(multi$wavenumber, raman_hdpe$wavenumber)
  expect_equal(multi$spectra[, "intensity"], raman_hdpe$spectra[, "intensity"])
  expect_equal(multi$spectra[, "intensity.1"], raman_hdpe$spectra[, "intensity"])
  expect_equal(multi$spectra[, "intensity.2"], raman_hdpe$spectra[, "intensity"])
})

test_that("ordinary ZIP readers ignore compact map-only arguments", {
  policy <- specs_background_filter(minimum = 4)
  multi <- read_zip(
    read_extdata("testdata_zipped.zip"), representation = "Specs",
    background_filter = policy, spectral_smooth = TRUE, sigma = c(1, 1, 1)
  )

  expect_type(multi, "list")
  expect_true(all(vapply(multi, is_OpenSpecy, logical(1))))
})

test_that("read_any() dispatches multi-digit OPUS extensions", {
  opus_file <- NULL
  local_mocked_bindings(
    read_opus = function(file, ...) {
      opus_file <<- file
      "opus-result"
    },
    .package = "OpenSpecy"
  )

  expect_identical(read_any("sample.10"), "opus-result")
  expect_identical(opus_file, "sample.10")
})

# Tidy up
unlink(tmp, recursive = T)
