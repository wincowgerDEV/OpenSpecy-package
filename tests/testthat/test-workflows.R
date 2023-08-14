# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("Raman batch analysis with test library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  expect_silent(batch <- read_any(read_extdata(file = "testdata_zipped.zip")))
  expect_true(is_OpenSpecy(batch))
  expect_silent(plot(batch))
  expect_silent(plotly_spec(batch))

  expect_no_error(get_lib(type = "test", path = tmp))
  expect_silent(check_lib(type = "test", path = tmp))
  expect_silent(lib <- load_lib(type = "test", path = tmp))

  expect_silent(filter_spec(lib, lib$metadata$SpectrumType == "Raman"))
  expect_silent(batch2 <- conform_spec(batch, new_wavenumbers = lib$wavenumber,
                                       res = spec_res(lib$wavenumber)))

  expect_silent(plotly_spec(batch2))

  expect_silent(test_sn2 <- signal_noise(batch2,
                                         return = "run_signal_over_noise"))
  expect_silent(batch3 <- process_spec(batch2, baseline_decision = T,
                                       derivative_decision = F))
  expect_silent(plotly_spec(x = batch3, x2 = batch))

  expect_silent(matches <- cor_spec(batch3, library = lib))
  expect_silent(test_max_cor <- max_cor_named(matches))
  expect_silent(test_sn <- signal_noise(batch3,
                                        return = "run_signal_over_noise"))
})

test_that("Raman batch analysis with complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  expect_silent(batch <- read_any(read_extdata(file = "testdata_zipped.zip")))
  expect_true(is_OpenSpecy(batch))
  expect_silent(plot(batch))
  expect_silent(plotly_spec(batch))

  expect_no_error(get_lib(type = "nobaseline", path = tmp))
  expect_silent(check_lib(type = "nobaseline", path = tmp))
  expect_silent(lib <- load_lib(type = "nobaseline", path = tmp))

  expect_silent(filter_spec(lib, lib$metadata$SpectrumType == "Raman"))
  expect_silent(batch2 <- conform_spec(batch, new_wavenumbers = lib$wavenumber,
                                       res = spec_res(lib$wavenumber)))

  expect_silent(plotly_spec(batch2))

  expect_silent(test_sn2 <- signal_noise(batch2,
                                         return = "run_signal_over_noise"))
  expect_silent(batch3 <- process_spec(batch2, baseline_decision = T,
                                       derivative_decision = F))
  expect_silent(plotly_spec(x = batch3, x2 = batch))

  expect_silent(matches <- cor_spec(batch3, library = lib))
  expect_silent(test_max_cor <- max_cor_named(matches))
  expect_silent(test_sn <- signal_noise(batch3, return = "run_signal_over_noise"))

  expect_silent(heatmap_spec(batch3, sn = test_sn, cor = test_max_cor,
                             min_sn = 4, min_cor = 0.7, selected_spectrum = 2,
                             source = "heatplot"))
})

# Tidy up
unlink(tmp, recursive = T)
