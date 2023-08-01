test_that("Zip folder Raman batch analysis", {
    expect_silent(batch <- read_any(read_extdata(file = "testdata_zipped.zip")))
    plot_OpenSpecy(batch)
    expect_true(is_OpenSpecy(batch))
    expect_no_error(get_lib(types = c("nobaseline")))
    expect_silent(check_lib(types = c("nobaseline")))
    expect_silent(lib <- load_lib(types = "nobaseline") %>%
                      filter_spec(., .$metadata$SpectrumType == "Raman"))
    expect_silent(batch2 <- conform_spec(batch, new_wavenumbers = lib$wavenumber, res = spec_res(lib$wavenumber)))
    plot_OpenSpecy(batch2)
    expect_silent(batch3 <- process_spectra(batch2, baseline_decision = T, derivative_decision = F))
    plot_OpenSpecy(x = batch3, x2 = batch)
    expect_silent(matches <- correlate_spectra(batch3, library = lib))\
    expect_silent(test_max_cor <- max_cor_named(matches))

})
