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

test_that("run_app() selects matching local commit when requested", {
  commit_one <- "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  commit_two <- "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

  local_old <- file.path(tmp, "local_old")
  local_new <- file.path(tmp, "local_new")

  dir.create(local_old, showWarnings = FALSE, recursive = TRUE)
  dir.create(local_new, showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(local_old, c("server.R", "ui.R")))
  file.create(file.path(local_new, c("server.R", "ui.R")))

  metadata_old <- list(
    commit = commit_one,
    ref = substr(commit_one, 1, 12),
    downloaded_at = "2020-01-01 00:00:00 UTC"
  )
  metadata_new <- list(
    commit = commit_two,
    ref = substr(commit_two, 1, 12),
    downloaded_at = "2021-01-01 00:00:00 UTC"
  )

  saveRDS(metadata_old, file.path(local_old, ".openspecy-shiny-metadata.rds"))
  saveRDS(metadata_new, file.path(local_new, ".openspecy-shiny-metadata.rds"))

  selected <- expect_message(
    run_app(
      path = tmp,
      check_local = TRUE,
      ref = substr(commit_one, 1, 10),
      test_mode = TRUE
    ),
    "Local app was downloaded from commit aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa. View commit: https://github.com/wincowgerDEV/OpenSpecy-shiny/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    fixed = TRUE
  )

  expect_equal(selected, local_old)
})

test_that("run_app() parses timezone metadata for local apps", {
  commit_three <- "cccccccccccccccccccccccccccccccccccccccc"

  legacy_dir <- file.path(tmp, "legacy_mid_tz")
  dir.create(legacy_dir, showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(legacy_dir, c("server.R", "ui.R")))

  metadata_legacy <- list(
    commit = commit_three,
    ref = commit_three,
    downloaded_at = "Tue May 21 11:22:33 UTC 2024"
  )

  saveRDS(metadata_legacy, file.path(legacy_dir, ".openspecy-shiny-metadata.rds"))

  selected <- expect_message(
    run_app(
      path = tmp,
      check_local = TRUE,
      ref = commit_three,
      test_mode = TRUE
    ),
    "Local app was downloaded from commit cccccccccccccccccccccccccccccccccccccccc. View commit: https://github.com/wincowgerDEV/OpenSpecy-shiny/commit/cccccccccccccccccccccccccccccccccccccccc",
    fixed = TRUE
  )

  expect_equal(selected, legacy_dir)
})

# Tidy up
unlink(tmp, recursive = T)
