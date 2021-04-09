data("raman_hdpe")

# Create temp dir for testthat
tmp <- file.path(tempdir(), "testthat")
dir.create(tmp, showWarnings = F)

test_that("share_text() gives expected output", {
  expect_message(share_spec(raman_hdpe,
                            metadata = c(user_name = "Win Cowger",
                                         spectrum_type = "FTIR",
                                         spectrum_identity = "PE",
                                         license = "CC BY-NC"),
                            share = tmp
  ))
  expect_warning(
    expect_message(share_spec(raman_hdpe, share = tmp))
  )
  expect_error(share_spec(raman_hdpe, metadata = c("a", "b", "c"),
                          share = tmp))
})

# Tidy up
unlink(tmp, recursive = T)
