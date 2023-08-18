data("raman_hdpe")

# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

wo_meta <- raman_hdpe
wo_meta$metadata$spectrum_identity <- NULL

test_that("share_text() gives expected output", {
  share_spec(raman_hdpe, share = tmp) |>
    expect_message()
  file.path(tmp, raman_hdpe$metadata$session_id,
            paste0(raman_hdpe$metadata$file_id, ".yml")) |>
    file.exists() |>
    expect_true()

  share_spec(wo_meta, share = tmp) |>
    expect_message() |>
    expect_warning()
})

# Tidy up
unlink(tmp, recursive = T)
