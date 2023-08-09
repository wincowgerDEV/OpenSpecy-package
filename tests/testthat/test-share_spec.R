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

# TODO: Need to think about config file
# test_that("check that data will upload", {
#     skip_on_cran()
#     library(config)
#     conf <- config::get()
#     expect_no_error(share_spec(object = raman_hdpe, file = read_extdata("raman_hdpe.csv"), share = "cloud", s3_key_id = conf$s3_key_id, s3_secret_key = conf$s3_secret_key, s3_region = conf$s3_region, s3_bucket = conf$s3_bucket))
# })


# Tidy up
unlink(tmp, recursive = T)
