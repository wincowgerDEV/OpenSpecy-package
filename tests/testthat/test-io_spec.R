# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

# Loading test data
data(raman_hdpe)

test_that("extdata files are present", {
  ed <- read_extdata()
  any(grepl("\\.yml$", ed)) |> expect_true()
  any(grepl("\\.json$", ed)) |> expect_true()
  any(grepl("\\.rds$", ed)) |> expect_true()
  any(grepl("\\.csv$", ed)) |> expect_true()
})

test_that("write_spec() works without errors", {
  write_spec(raman_hdpe, file.path(tmp, "test.yml")) |> expect_silent()
  write_spec(raman_hdpe, file.path(tmp, "test.json")) |> expect_silent()
  write_spec(raman_hdpe, file.path(tmp, "test.rds")) |> expect_silent()
  write_spec(raman_hdpe, file.path(tmp, "test.csv")) |> expect_silent()
  
  write_spec(as.data.frame(raman_hdpe), file.path(tmp, "test.yml")) |>
    expect_error()
  write_spec(raman_hdpe, file.path(tmp, "test.flunk")) |> expect_error()
})

test_that("read_spec() gives expected output", {
  yml <- read_extdata("raman_hdpe.yml") |> read_spec() |> expect_silent()
  jsn <- read_extdata("raman_hdpe.json") |> read_spec() |> expect_silent()
  rds <- read_extdata("raman_hdpe.rds") |> read_spec() |> expect_silent()

  read_extdata("raman_hdpe.csv") |> read_spec() |> expect_error()

  read_extdata("raman_hdpe.yml") |> read_spec(share = tmp) |> expect_message()
  read_extdata("raman_hdpe.json") |> read_spec(share = tmp) |> expect_message()
  read_extdata("raman_hdpe.rds") |> read_spec(share = tmp) |> expect_message()

  expect_s3_class(yml, "OpenSpecy")
  expect_s3_class(jsn, "OpenSpecy")
  expect_s3_class(rds, "OpenSpecy")
  
  expect_true(check_OpenSpecy(yml))
  expect_true(check_OpenSpecy(jsn))
  expect_true(check_OpenSpecy(rds))

  jsn$metadata$file_name <- yml$metadata$file_name <-
    rds$metadata$file_name <- NULL
  expect_equal(jsn, yml)
  expect_equal(rds, raman_hdpe)
  expect_equal(jsn[1:2], raman_hdpe[1:2])
  expect_equal(yml[1:2], raman_hdpe[1:2])

  read_spec(read_extdata("raman_hdpe.yml"), share = tmp) |> expect_message()
})

test_that("read_spec() and write_spec() work nicely together", {
  yml <- read_spec(file.path(tmp, "test.yml")) |> expect_silent()
  jsn <- read_spec(file.path(tmp, "test.json")) |> expect_silent()
  rds <- read_spec(file.path(tmp, "test.rds")) |> expect_silent()

  jsn$metadata$file_name <- yml$metadata$file_name <-
    rds$metadata$file_name <- NULL
  expect_equal(jsn, yml)
  expect_equal(rds, raman_hdpe)
  expect_equal(jsn[1:2], raman_hdpe[1:2])
  expect_equal(yml[1:2], raman_hdpe[1:2])
})

test_that("as_hyperspec() function works", {
  hyperspec_object <- as_hyperSpec(raman_hdpe)

  # Verify the class of the output
  expect_s4_class(hyperspec_object, "hyperSpec")

  # Verify the equality of the content
  expect_equal(hyperspec_object@wavelength, raman_hdpe$wavenumber)
  expect_equal(c(hyperspec_object$spc), raman_hdpe$spectra$intensity)
})

# Tidy up
unlink(tmp, recursive = T)
