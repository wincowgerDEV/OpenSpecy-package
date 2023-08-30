# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("run_app() wrapper doesn't produce errors", {
  run_app(path = tmp, test_mode = T) |>
    expect_silent()
})

# Tidy up
unlink(tmp, recursive = T)
