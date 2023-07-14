# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

library(data.table)

test_that("extdata files are present", {
  ed <- read_extdata()
  expect_true(any(grepl("\\.yml$", ed)))
  expect_true(any(grepl("\\.json$", ed)))
  expect_true(any(grepl("\\.rds$", ed)))
})

test_that("write_spec() works without errors", {
  expect_silent(write_spec(raman_hdpe, file.path(tmp, "test.yml")))
  expect_silent(write_spec(raman_hdpe, file.path(tmp, "test.json")))
  expect_silent(write_spec(raman_hdpe, file.path(tmp, "test.rds")))

  expect_error(write_spec(as.data.frame(raman_hdpe),
                          file.path(tmp, "test.yml")))
  expect_error(write_spec(raman_hdpe, file.path(tmp, "test.csv")))
})

test_that("read_spec() gives expected output", {
  expect_silent(yml <- read_spec(read_extdata("raman_hdpe.yml")))
  expect_silent(jsn <- read_spec(read_extdata("raman_hdpe.json")))
  expect_silent(rds <- read_spec(read_extdata("raman_hdpe.rds")))

  expect_error(read_spec(read_extdata("raman_hdpe.csv")))

  expect_s3_class(yml, "OpenSpecy")
  expect_s3_class(jsn, "OpenSpecy")
  expect_s3_class(rds, "OpenSpecy")

  expect_equal(jsn, yml)
  expect_equal(rds, raman_hdpe)
  expect_equal(jsn[1:2], raman_hdpe[1:2])
  expect_equal(yml[1:2], raman_hdpe[1:2])

  expect_message(read_spec(read_extdata("raman_hdpe.yml"), share = tmp))
})

test_that("read_spec() and write_spec() work nicely together", {
  expect_silent(yml <- read_spec(file.path(tmp, "test.yml")))
  expect_silent(jsn <- read_spec(file.path(tmp, "test.json")))
  expect_silent(rds <- read_spec(file.path(tmp, "test.rds")))

  expect_equal(jsn, yml)
  expect_equal(rds, raman_hdpe)
  expect_equal(jsn[1:2], raman_hdpe[1:2])
  expect_equal(yml[1:2], raman_hdpe[1:2])
})

# Tidy up
unlink(tmp, recursive = T)
