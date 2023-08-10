# Create test data for cor_spec function
data("test_lib")
unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) |>
    conform_spec(new_wavenumbers = test_lib$wavenumber, res = spec_res(test_lib)) |>
    process_spec()

# Create a subset of test_lib for filtering
test_lib_extract <- filter_spec(test_lib, logic = test_lib$metadata$polymer_class == "polycarbonates")

# Write the tests for cor_spec function
test_that("cor_spec returns a data.table with correct columns", {
    matches <- cor_spec(unknown,library =  test_lib)
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
