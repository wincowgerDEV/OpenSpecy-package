spectrum <- read_extdata("raman_hdpe.csv") |> read_any()

test_that("check that conforming spectra doesn't produce errors", {
    expect_true(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10), max(spectrum$wavenumber)))))
    expect_equal(conform_spec(spectrum, c(1000, 2000))$wavenumber, seq(1000, 2000, by = 5))
    expect_error(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10)))))
    expect_error(is_OpenSpecy(conform_spec(spectrum)))
    expect_error(is_OpenSpecy(conform_spec(spectrum, c(min(spectrum$wavenumber + 10), max(spectrum$wavenumber)), res = 0)))
})
