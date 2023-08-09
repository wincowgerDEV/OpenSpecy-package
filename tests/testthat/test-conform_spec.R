spectrum <- read_extdata("raman_hdpe.csv") |> read_any()

test_that("check that conforming spectra doesn't produce errors", {
    expect_true(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10), max(spectrum$wavenumber)))))
    expect_equal(conform_spec(spectrum, c(1000, 2000))$wavenumber, seq(1000, 2000, by = 5))
    expect_error(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10)))))
    expect_error(is_OpenSpecy(conform_spec(spectrum)))
    expect_error(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10), max(spectrum$wavenumber)), res = 0)))
})

test_that("conform_spec.default throws an error for non-OpenSpecy objects", {
  # Create a non-OpenSpecy object
  non_open_specy_object <- data.frame(wavenumber = c(1000, 1100, 1200),
                                      intensity = c(0.1, 0.2, 0.3))

  # Test if the function throws an error for non-OpenSpecy object
  expect_error(conform_spec(non_open_specy_object, c(1000, 2000)))
})

test_that("conform_spec.OpenSpecy conforms wavenumbers correctly", {
  # Create a sample OpenSpecy object
  wavenumber <- seq(1000, 2000, 5)
  intensity <- rnorm(length(wavenumber))
  open_specy_object <- as_OpenSpecy(wavenumber, data.table(intensity = intensity))

  # Conform the wavenumbers to a new range
  new_wavenumbers <- c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900)
  conform_result <- conform_spec(open_specy_object, new_wavenumbers)

  # Test if the wavenumber and intensity vectors have the same length after conforming
  expect_equal(length(conform_result$wavenumber), length(conform_result$spectra[[1]]))

  # Test if the wavenumber range matches the new_wavenumbers vector
  expect_equal(min(conform_result$wavenumber), min(new_wavenumbers))
  expect_equal(max(conform_result$wavenumber), max(new_wavenumbers))
})
