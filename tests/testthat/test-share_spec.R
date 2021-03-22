data("raman_hdpe")

test_that("share_text() gives expected output", {
  expect_message(share_spec(raman_hdpe,
                                metadata = c(user_name = "Win Cowger",
                                             spectrum_type = "FTIR",
                                             spectrum_identity = "PE")
  ))
  expect_warning(
    expect_message(share_spec(raman_hdpe))
    )
  expect_error(share_spec(raman_hdpe, metadata = c("a", "b", "c")))
})
