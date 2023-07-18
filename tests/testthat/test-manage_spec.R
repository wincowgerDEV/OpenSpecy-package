

test_that("Merge multiple of same file without range specification", {
    spectra <- lapply(c(read_extdata("raman_hdpe.csv"), read_extdata("raman_hdpe.csv"), read_extdata("raman_hdpe.csv")), read_text)
    expect_silent(spectra2 <- c_spec(objects = spectra))
    expect_true(is_OpenSpecy(spectra2))
})

spectra <- lapply(c(read_extdata("raman_hdpe.csv"), read_extdata("ftir_ldpe_soil.asp")), read_any)

test_that("Merge different files", {
    expect_silent(spectra2 <- c_spec(objects = spectra, wavenumber_transform = "common_range", res = 5))
    expect_true(is_OpenSpecy(spectra2))
})

test_that("Merge different files with specified range", {
    expect_silent(spectra2 <- c_spec(objects = spectra, wavenumber_transform = c(1000, 2000), res = 5))
    expect_true(is_OpenSpecy(spectra2))
})
