# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("Raman batch analysis with test library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  batch <- read_extdata("testdata_zipped.zip") |> read_any() |>
    expect_silent()
  is_OpenSpecy(batch) |> expect_true()
  plot(batch) |> expect_silent()
  plotly_spec(batch) |> expect_silent()

  get_lib(type = "test", path = tmp) |> expect_no_error()
  check_lib(type = "test", path = tmp) |> expect_silent()
  lib <- load_lib(type = "test", path = tmp) |> expect_silent()

  filter_spec(lib, lib$metadata$SpectrumType == "Raman") |> expect_silent()
  batch2 <- conform_spec(batch, lib$wavenumber, res = spec_res(lib$wavenumber)) |>
    expect_silent()

  plotly_spec(batch2) |> expect_silent()

  sig_noise(batch2, metric = "run_sig_over_noise") |>
    expect_silent()
  batch3 <- process_spec(batch2, subtr_baseline = T) |> expect_silent()
  plotly_spec(x = batch3, x2 = batch) |> expect_silent()

  matches <- cor_spec(batch3, library = lib) |> expect_silent()
  test_max_cor <- max_cor_named(matches) |> expect_silent()
  sig_noise(batch3, metric = "run_sig_over_noise") |>
    expect_silent()
})

test_that("Raman batch analysis with complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  batch <- read_extdata(file = "testdata_zipped.zip") |> read_any() |>
    expect_silent()
  is_OpenSpecy(batch) |> expect_true()
  plot(batch) |> expect_silent()
  plotly_spec(batch) |> expect_silent()

  get_lib(type = "nobaseline", path = tmp) |> expect_no_error()
  check_lib(type = "nobaseline", path = tmp) |> expect_silent()
  lib <- load_lib(type = "nobaseline", path = tmp) |> expect_silent()

  filter_spec(lib, lib$metadata$SpectrumType == "Raman") |> expect_silent()
  batch2 <- conform_spec(batch, range = lib$wavenumber,
                         res = spec_res(lib$wavenumber)) |>
    expect_silent()

  plotly_spec(batch2) |> expect_silent()

  test_sn2 <- sig_noise(batch2, metric = "run_sig_over_noise") |>
    expect_silent()
  batch3 <- process_spec(batch2, subtr_baseline = T) |> expect_silent()
  plotly_spec(x = batch3, x2 = batch) |> expect_silent()

  matches <- cor_spec(batch3, library = lib) |> expect_silent()
  test_max_cor <- max_cor_named(matches) |> expect_silent()
  test_sn <- sig_noise(batch3, metric = "run_sig_over_noise") |>
    expect_silent()

  heatmap_spec(batch3, sn = test_sn, cor = test_max_cor, min_sn = 4,
               min_cor = 0.7, select = 2, source = "heatplot") |>
    expect_silent()
})

# Tidy up
unlink(tmp, recursive = T)
