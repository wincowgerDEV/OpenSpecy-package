test_that("ENVI files are read", {
  tiny_map <- read_zip(read_extdata("CA_tiny_map.zip"))
  expect_s3_class(tiny_map, "OpenSpecy")

  expect_equal(ncol(tiny_map$spectra), 208)
  expect_length(tiny_map$wavenumber, 427)
  expect_identical(names(tiny_map$metadata), c("x", "y", "file", "file_id"))
})
