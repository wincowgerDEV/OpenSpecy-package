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
  expect_true(check_OpenSpecy(batch))

  get_lib(type = "test", path = tmp) |> expect_no_error()
  check_lib(type = "test", path = tmp) |> expect_silent()
  lib <- load_lib(type = "test", path = tmp) |> expect_silent()

  filter_spec(lib, lib$metadata$SpectrumType == "Raman") |> expect_silent()
  batch2 <- conform_spec(batch, lib$wavenumber, res = spec_res(lib$wavenumber)) |>
    expect_silent()

  expect_true(check_OpenSpecy(batch2))

  plotly_spec(batch2) |> expect_silent()

  sig_noise(batch2, metric = "run_sig_over_noise") |>
    expect_silent()
  batch3 <- process_spec(batch2, subtr_baseline = T) |> expect_silent()
  plotly_spec(x = batch3, x2 = batch) |> expect_silent()

  expect_true(check_OpenSpecy(batch3))

  matches <- cor_spec(batch3, library = lib) |> expect_silent()
  test_max_cor <- max_cor_named(matches) |> expect_silent()
  sig_noise(batch3, metric = "run_sig_over_noise") |>
    expect_silent()
})

#test_that("Raman batch analysis with complete library", {
#  skip_on_cran()
#  skip_if_offline(host = "api.osf.io")
#  skip_if_not(testthat:::on_ci(), "Not on CI")#

#  batch <- read_extdata(file = "testdata_zipped.zip") |> read_any() |>
#    expect_silent()
#  is_OpenSpecy(batch) |> expect_true()
#  plot(batch) |> expect_silent()
#  plotly_spec(batch) |> expect_silent()

#  get_lib(type = "nobaseline", path = tmp) |> expect_no_error()
#  check_lib(type = "nobaseline", path = tmp) |> expect_silent()
#  lib <- load_lib(type = "nobaseline", path = tmp) |> expect_silent()

#  filter_spec(lib, lib$metadata$spectrum_type == "raman") |> expect_silent()
#  batch2 <- conform_spec(batch, range = lib$wavenumber,
#                         res = spec_res(lib$wavenumber)) |>
#    expect_silent()
#  expect_true(check_OpenSpecy(batch2))

#  plotly_spec(batch2) |> expect_silent()

#  test_sn2 <- sig_noise(batch2, metric = "run_sig_over_noise") |>
#    expect_silent()
#  batch3 <- process_spec(batch2, subtr_baseline = T) |> expect_silent()
#  plotly_spec(x = batch3, x2 = batch) |> expect_silent()

#  expect_true(check_OpenSpecy(batch3))

#  matches <- cor_spec(batch3, library = lib) |> expect_silent()
#  test_max_cor <- max_cor_named(matches) |> expect_silent()
#  test_sn <- sig_noise(batch3, metric = "run_sig_over_noise") |>
#    expect_silent()

#  heatmap_spec(batch3, sn = test_sn, cor = test_max_cor, min_sn = 4,
#               min_cor = 0.7, select = 2, source = "heatplot") |>
#    expect_silent()
#})

test_that("One particle is identified with standard workflow in map", {
  skip_on_cran()

  map <- read_extdata("CA_tiny_map.zip") |> read_any()
  signal_noise <- sig_noise(map, metric = "sig_times_noise", abs = F)

  id_map <- def_features(map,signal_noise > 0.01)
  unique(id_map$metadata$feature_id) |> length() |> expect_equal(4)

  test_collapsed <- collapse_spec(id_map)

  test_collapsed$metadata |> nrow() |>
    expect_equal(4)
  test_collapsed$metadata$feret_max |> round(2) |>
    expect_equal(c(NA, 8, 12.31, 4.00))
  test_collapsed$metadata$centroid_x |> round(2) |>
    expect_equal(c(7.87, 2.00, 7.9, 0.00))
})

# Tidy up
unlink(tmp, recursive = T)
