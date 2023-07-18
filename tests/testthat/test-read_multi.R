
test_that("reading in multi files doesn't throw error", {
    expect_no_error(map_file <- read_extdata("CA_tiny_map.zip") |> read_any())
    expect_true(length(map_file$wavenumber) == 427)
    expect_true(is_OpenSpecy(map_file))
    expect_true(ncol(map_file$spectra) == 208)
    expect_silent(multi_file <- read_extdata("testdata_zipped.zip") |> read_any())
    expect_true(is_OpenSpecy(multi_file))
    expect_true(length(multi_file$wavenumber) == 964)
    expect_true(ncol(multi_file$spectra) == 3)
})
