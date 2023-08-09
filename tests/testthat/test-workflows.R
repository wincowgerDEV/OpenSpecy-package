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
    expect_silent(test_sn2 <- signal_noise(batch2, return = "run_signal_over_noise"))
    expect_silent(batch3 <- process_spec(batch2, baseline_decision = T, derivative_decision = F))
    plot_OpenSpecy(x = batch3, x2 = batch)
    expect_silent(matches <- cor_spec(batch3, library = lib))
    expect_silent(test_max_cor <- max_cor_named(matches))
    expect_silent(test_sn <- signal_noise(batch3, return = "run_signal_over_noise"))
    
    heatmap_OpenSpecy(object = batch3, 
                      sn = test_sn, 
                      cor = test_max_cor, 
                      min_sn = 4,
                      min_cor = 0.7,
                      selected_spectrum = 2,
                      source = "heatplot")
})
