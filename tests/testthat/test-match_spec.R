# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

# Create test data for cor_spec function
data("test_lib")

unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) |>
  conform_spec(range= test_lib$wavenumber, res = spec_res(test_lib)) |>
  process_spec(smooth_intens = T, make_rel = T)

# Create a subset of test_lib for filtering
test_lib_extract <- filter_spec(test_lib,
                                logic = test_lib$metadata$polymer_class == "polycarbonates")

# Match_spec function with AI
test_that("match_spec returns correct structure with AI", {
  skip_on_cran()
  get_lib("model")
  expect_silent(check_lib("model"))
  lib <- load_lib(type = "model")
  fill <- as_OpenSpecy(as.numeric(unique(lib$variables_in)),
                       spectra = data.frame(runif(n = length(unique(lib$variables_in)))))
  matches <- match_spec(x = unknown, library = lib, na.rm = T, fill = fill)
  expect_true(nrow(matches) == 1)
  expect_true(all(c("x", "y", "z", "value", "name") %in% names(matches)))
})

# Match_spec function
test_that("match_spec returns correct structure", {
  matches <- match_spec(x = unknown, library = test_lib, na.rm = T, top_n = 5, add_library_metadata = "sample_name", add_object_metadata = "col_id")
  expect_true(nrow(matches) == 5)
  expect_true(all(c("object_id", "library_id", "match_val") %in% names(matches)))
})

# Write the tests for cor_spec function
test_that("cor_spec returns a data.table with correct columns", {
  matches <- cor_spec(unknown,library =  test_lib)
  unknown2 <- unknown
  unknown2$wavenumber[1:3] <- unknown2$wavenumber[1:3] +1
  expect_warning(matches2 <- cor_spec(unknown2,library =  test_lib))
  expect_true(inherits(matches, "matrix"))
  expect_identical(dim(matches), c(ncol(test_lib$spectra), ncol(unknown$spectra)))
  top_matches <- max_cor_named(cor_matrix = matches, na.rm = T)
  expect_true(length(top_matches) == 1)
  expect_true(ncol(filter_spec(test_lib, logic = names(top_matches))$spectra) == 1)
  test_lib$metadata$test <- NA
  test_metadata <- get_metadata(test_lib, logic = names(top_matches), remove_empty = T)
  expect_true(nrow(test_metadata) == 1)
  expect_true(!"test" %in% names(test_metadata))
  full_test <- ident_spec(matches, unknown, library = test_lib, top_n = 5, add_library_metadata = "sample_name")
  expect_true(nrow(full_test) == 5)
})

# Write the tests for filter_spec function
test_that("filter_spec returns OpenSpecy object with filtered spectra", {
  os_filtered <- filter_spec(test_lib, logic = rep(F,ncol(test_lib$spectra)))
  expect_equal(ncol(os_filtered$spectra), 0)
  expect_equal(nrow(os_filtered$metadata), 0)
})

# Write the tests for filter_spec function
test_that("filter_spec returns OpenSpecy object with filtered spectra", {
  logic <- rep(F,ncol(test_lib$spectra))
  logic[1] <- TRUE
  os_filtered <- filter_spec(test_lib, logic = logic)
  expect_equal(ncol(os_filtered$spectra), 1)
  expect_equal(nrow(os_filtered$metadata), 1)
})

# Tidy up
unlink(tmp, recursive = T)
