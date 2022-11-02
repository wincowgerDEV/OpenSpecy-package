data("raman_hdpe")

# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

wo_meta <- raman_hdpe
wo_meta$metadata$spectrum_identity <- NULL

test_that("share_text() gives expected output", {
  expect_message(share_spec(raman_hdpe, share = tmp))
  expect_warning(
    expect_message(share_spec(wo_meta, share = tmp))
  )
})

# Tidy up
unlink(tmp, recursive = T)
