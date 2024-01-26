data("raman_hdpe")

test_that("conform_spec() throws an error for non-OpenSpecy objects", {
  # Create a non-OpenSpecy object
  non_OpenSpecy <- data.frame(wavenumber = c(1000, 1100, 1200),
                              intensity = c(0.1, 0.2, 0.3))

  conform_spec(non_OpenSpecy, c(1000, 2000)) |>
    expect_error()
})

test_that("conform_spec() handles errors correctly", {
  conform_spec(raman_hdpe, range = c(min(raman_hdpe$wavenumber + 10),
                                     max(raman_hdpe$wavenumber))) |>
    expect_silent()
  conform_spec(raman_hdpe, range = c(0,5000)) |>
    expect_silent()
  expect_equal(conform_spec(raman_hdpe, c(1000, 2000))$wavenumber,
               seq(1000, 2000, by = 5))
  conform_spec(raman_hdpe, c(min(raman_hdpe$wavenumber + 10))) |>
    expect_error()
  conform_spec(raman_hdpe, c(min(raman_hdpe$wavenumber + 10),
                             max(raman_hdpe$wavenumber)), res = 0) |>
    expect_error()
})

test_that("conform_spec() conforms wavenumbers correctly", {
  wn <- seq(1000, 2000, 5)
  sam <- as_OpenSpecy(x = wn,
                      data.table(intensity = rnorm(length(wn))))
  expect_true(check_OpenSpecy(sam))
  
  new_wavenumbers <- c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900)
  wider_wavenumbers <- seq(800, 2500, by = 100)
  conf_new <- conform_spec(sam, new_wavenumbers) |>
    expect_silent()
  expect_true(check_OpenSpecy(conf_new))
  
  conf_roll <- conform_spec(sam, new_wavenumbers,res = NULL, type = "roll") |>
    expect_silent()
  expect_true(check_OpenSpecy(conf_roll))
  
  conf_mean_up <- conform_spec(sam, new_wavenumbers,res = NULL, type = "mean_up") |>
      expect_silent()
  expect_true(check_OpenSpecy(conf_mean_up))
  
  conf_wider <- conform_spec(x = sam,range = wider_wavenumbers, res = NULL) |>
      expect_silent()
  expect_true(check_OpenSpecy(conf_wider))

  expect_equal(length(conf_new$wavenumber), length(conf_new$spectra[[1]]))
  expect_equal(range(conf_new$wavenumber), range(new_wavenumbers))

  expect_identical(conf_roll$wavenumber, new_wavenumbers)

  expect_s3_class(conf_new, "OpenSpecy")
  expect_s3_class(conf_roll, "OpenSpecy")

  conform_spec(raman_hdpe)$spectra$intensity[c(63, 143, 283, 325, 402)] |>
    round(2) |>
    expect_equal(c(78.84, 65.00, 105.73, 109.41, 116.00))
  
  expect_true(all(conf_wider$wavenumber <= max(sam$wavenumber)) & all(conf_wider$wavenumber >= min(sam$wavenumber)))
  
  
})
