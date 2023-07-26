# Create test data for correlate_spectra function
data("test_lib")
unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) %>%
    conform_spec(., new_wavenumbers = test_lib$wavenumber, res = spec_res(test_lib)) %>%
    process_spectra(.)

# Create a subset of test_lib for filtering
test_lib_extract <- filter_spec(test_lib, logic = test_lib$metadata$polymer_class == "polycarbonates")

# Write the tests for correlate_spectra function
test_that("correlate_spectra returns a data.table with correct columns", {
    matches <- correlate_spectra(unknown, test_lib, top_n = 10, add_library_metadata = "sample_name")
    expect_s3_class(matches, "data.table")
    expect_true(all(c("object_id", "library_id", "match_val") %in% colnames(matches)))
})

test_that("correlate_spectra returns correct number of matches with top_n", {
    matches <- correlate_spectra(unknown, test_lib, top_n = 5)
    expect_equal(nrow(matches), 5)
})

test_that("correlate_spectra returns all matches if top_n is larger than library", {
    matches <- correlate_spectra(unknown, test_lib, top_n = 1000)
    expect_equal(nrow(matches), ncol(test_lib$spectra))
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
