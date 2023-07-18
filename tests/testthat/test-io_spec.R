
# Loading test data
data(raman_hdpe)

test_that("Test read_spec and write_spec functions", {
    # First write the test data to temporary files in different formats
    temp_file_yml <- tempfile(fileext = ".yml")
    temp_file_json <- tempfile(fileext = ".json")
    temp_file_rds <- tempfile(fileext = ".rds")
    
    write_spec(raman_hdpe, temp_file_yml)
    write_spec(raman_hdpe, temp_file_json)
    write_spec(raman_hdpe, temp_file_rds)
    
    # Now read the data back in and test equality
    spec_yml <- read_spec(temp_file_yml)
    spec_json <- read_spec(temp_file_json)
    spec_rds <- read_spec(temp_file_rds)
    
    # Verify the equality of the original and the read data
    expect_equal(raman_hdpe, spec_yml)
    expect_equal(raman_hdpe, spec_json)
    expect_equal(raman_hdpe, spec_rds)
    
    # Clean up temporary files
    file.remove(temp_file_yml)
    file.remove(temp_file_json)
    file.remove(temp_file_rds)
})

test_that("Test to_hyperspec function", {
    hyperspec_object <- to_hyperSpec(raman_hdpe)
    
    # Verify the class of the output
    expect_s4_class(hyperspec_object, "hyperSpec")
    
    # Verify the equality of the content
    expect_equal(hyperspec_object@wavelength, raman_hdpe$wavenumber)
    expect_equal(c(hyperspec_object$spc), raman_hdpe$spectra$intensity)
})
