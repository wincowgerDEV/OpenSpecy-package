test_that("process returns expected values"{
    data <- read_any(read_extdata("CA_tiny_map.zip"))
    expect_silent(processed <- process_spectra(data))
    expect_true(is_OpenSpecy(processed))
})