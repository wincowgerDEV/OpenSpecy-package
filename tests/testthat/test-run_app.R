# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("run_app() wrapper doesn't produce errors", {
  run_app(path = tmp, test_mode = T) |>
    expect_silent()
})

test_that("run_app() uses local copy when requested", {
  local_app <- file.path(tmp, "local_app")
  dir.create(local_app, showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(local_app, c("server.R", "ui.R")))

  run_app(path = tmp, check_local = TRUE, test_mode = T) |>
    expect_message("Running local OpenSpecy Shiny app")
})

# Tidy up
unlink(tmp, recursive = T)
