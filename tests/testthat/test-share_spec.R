# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("raman_hdpe")

wo_meta <- raman_hdpe
wo_meta$metadata$spectrum_identity <- NULL

test_that("share_text() works locally", {
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

test_that("share_text() uploads to the cloud", {
  skip_on_cran()
  skip_if_not(testthat:::on_ci(), "Not on CI")
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "" ||
            Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", "No credentials")

  share_spec(raman_hdpe, file = read_extdata("raman_hdpe.csv"), share = "cloud",
             credentials = list(
               s3_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
               s3_secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
               s3_region = "us-east-2", s3_bucket = "openspecy")) |>
    expect_message()
})

# Tidy up
unlink(tmp, recursive = T)
