# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("raman_hdpe")

wo_meta <- raman_hdpe
wo_meta$metadata$spectrum_identity <- NULL

test_that("share_text() gives expected output", {
  share_spec(raman_hdpe, file = read_extdata("raman_hdpe.csv"), share = tmp) |>
    expect_message()
  read_extdata("testdata_zipped.zip") |> read_zip() |>
    share_spec(share = tmp) |>
    expect_message() |> expect_warning()

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
