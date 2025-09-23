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

  metadata <- list(
    commit = "1234567890abcdef1234567890abcdef12345678",
    owner = "wincowgerDEV",
    repo = "OpenSpecy-shiny"
  )
  saveRDS(metadata, file.path(local_app, ".openspecy-shiny-metadata.rds"))

  run_app(path = tmp, check_local = TRUE, test_mode = T) |>
    expect_message(
      "Local app was downloaded from commit 1234567890abcdef1234567890abcdef12345678. View commit: https://github.com/wincowgerDEV/OpenSpecy-shiny/commit/1234567890abcdef1234567890abcdef12345678",
      fixed = TRUE
    )
})

# Tidy up
unlink(tmp, recursive = T)
