data("raman_hdpe")

test_that("share_text() gives expected output", {
  expect_message(
    expect_message(share_spectrum(raman_hdpe))
    )
  expect_message(share_spectrum(raman_hdpe,
                                metadata = c(user_name = "Win Cowger",
                                             spectrum_type = "FTIR",
                                             spectrum_identity = "PE")
                                ))
  expect_error(share_spectrum(raman_hdpe, metadata = c("a", "b", "c")))
})
