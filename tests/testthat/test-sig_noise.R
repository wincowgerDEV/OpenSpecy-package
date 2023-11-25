data("raman_hdpe")

test_that("sig_noise() handles input errors corretly", {
  sig_noise(1:1000) |> expect_error()
  restrict_range(raman_hdpe, 300, 320) |> sig_noise() |>
    expect_warning()
})

test_that("sig_noise() can restrict the range correctly", {
  big_peak <- sig_noise(raman_hdpe, metric = "sig", sig_max = 3000,
                        sig_min = 2500)
  low_region <- sig_noise(raman_hdpe, metric = "sig", sig_max = 1000,
                          sig_min = 500)
  expect_true(big_peak > low_region)
  restric_peak <- restrict_range(raman_hdpe, min = 2500, max = 3000,
                                 make_rel = F) |>
    sig_noise(metric = "sig")
  expect_identical(big_peak, restric_peak)
})

test_that("sig_noise() returns correct values", {
  sig_noise(raman_hdpe, metric = "sig") |> round(2) |> unname() |>
    expect_equal(101.17)

  sig_noise(raman_hdpe, metric = "noise") |> round(2) |> unname() |>
    expect_equal(61.01)

  sig_noise(raman_hdpe, metric = "sig_times_noise") |> round(2) |> unname() |>
    expect_equal(6172.1)

  sig_noise(raman_hdpe, metric = "sig_over_noise") |> round(2) |> unname() |>
    expect_equal(1.66)

  sig_noise(raman_hdpe, metric = "tot_sig") |> round(2) |> unname() |>
    expect_equal(97527)
})

