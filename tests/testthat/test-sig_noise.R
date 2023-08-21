# Load your OpenSpecy object
data("raman_hdpe")

test_that("sig_noise returns correct values", {
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
