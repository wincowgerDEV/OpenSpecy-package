# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("opus files are read correctly", {
  single <- read_extdata("ftir_ps.0") |> read_opus() |>
    expect_silent()

  multi <- c(read_extdata("ftir_ps.0"), read_extdata("ftir_ps.0")) |>
    read_opus() |> expect_silent()

  read_extdata("raman_hdpe.csv") |> read_opus() |>
    expect_error()

  expect_s3_class(single, "OpenSpecy")
  expect_s3_class(multi, "OpenSpecy")
  expect_true(check_OpenSpecy(single))
  expect_true(check_OpenSpecy(multi))

  expect_equal(names(single), c("wavenumber", "spectra", "metadata"))
  expect_equal(names(multi), c("wavenumber", "spectra", "metadata"))
  expect_length(single$wavenumber, 2126)
  range(single$wavenumber) |> round(1) |>
    expect_equal(c(399.2, 4497.5))
  range(single$spectra) |> round(4) |>
    expect_equal(c(0.0130, 0.6112))
  expect_equal(multi$wavenumber, single$wavenumber |> round(1))
  range(multi$spectra, na.rm = T) |> round(4) |>
    expect_equal(c(0.0130, 0.6103))

  names(single$metadata) |>
    expect_contains(c("x", "y", "unique_id", "sample_id", "date_time_sm",
                      "date_time_rf", "sample_name", "instr_name_range",
                      "resolution_wn", "result_spc", "beamspl", "laser_wn",
                      "spc_in_file", "zero_filling", "temp_scanner_sm",
                      "temp_scanner_rf", "hum_rel_sm", "hum_rel_rf",
                      "hum_abs_sm", "hum_abs_rf", "file_id"))
  expect_identical(names(multi$metadata), names(single$metadata))
})

# Tidy up
unlink(tmp, recursive = T)
