test_that("run_app() resolves the bundled app", {
  app_path <- run_app(test_mode = TRUE)

  expect_true(dir.exists(app_path))
  expect_true(all(file.exists(file.path(
    app_path,
    .openspecy_bundled_shiny_files()
  ))))
})

test_that("run_app() supports explicit local app directories", {
  tmp <- file.path(tempdir(), "OpenSpecy-testthat-run-app")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  local_app <- file.path(tmp, "local_app")
  dir.create(local_app, showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(local_app, c("server.R", "ui.R")))

  expect_equal(
    run_app(path = tmp, test_mode = TRUE),
    normalizePath(local_app, winslash = "/", mustWork = TRUE)
  )
})

test_that("run_app() rejects directories without a Shiny app", {
  tmp <- file.path(tempdir(), "OpenSpecy-testthat-missing-app")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

  expect_error(
    run_app(path = tmp, test_mode = TRUE),
    "Unable to locate a Shiny app"
  )
})

test_that("run_app() reports ignored historical remote arguments", {
  expect_warning(
    run_app(ref = "main", test_mode = TRUE),
    "`ref` is ignored",
    fixed = TRUE
  )

  expect_warning(
    run_app(check_local = FALSE, test_mode = TRUE),
    "`check_local` is ignored",
    fixed = TRUE
  )
})

test_that("bundled Shiny app source files parse", {
  app_path <- run_app(test_mode = TRUE)

  for(file in c("global.R", "server.R", "ui.R")) {
    expect_error(parse(file.path(app_path, file)), NA)
  }
})

test_that("bundled Shiny app no longer advertises YAML uploads", {
  app_path <- run_app(test_mode = TRUE)
  app_source <- unlist(lapply(
    file.path(app_path, c("server.R", "ui.R")),
    readLines,
    warn = FALSE
  ))

  expect_false(any(grepl("\\.ya?ml|yml", app_source, ignore.case = TRUE)))
})

test_that("bundled Shiny app avoids app-local library data assumptions", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- readLines(file.path(app_path, "server.R"), warn = FALSE)

  expect_false(any(grepl("data/.*\\.rds", server_source)))
  expect_true(any(grepl("load_app_library", server_source, fixed = TRUE)))
})

test_that("bundled Shiny app prunes imported orphan assets", {
  app_path <- run_app(test_mode = TRUE)
  assets <- list.files(file.path(app_path, "www"), recursive = TRUE)

  expect_false(any(grepl(
    "jumbotron\\.png|dancing\\.jpg|jqfp\\.js|md5\\.js|shinyBindings\\.js",
    assets
  )))
})

test_that("bundled Shiny app helpers can be sourced when app packages exist", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:",
    paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)

  expect_error(sys.source(file.path(app_path, "global.R"), envir = env), NA)
  expect_true(is.function(env$load_app_library))
  expect_true(is.function(env$app_library_dir))
  expect_match(env$app_version_display$text, "^OpenSpecy ")

  local({
    env$load_data()
    expect_s3_class(testdata, "data.table")
    expect_true(all(c("wavenumber", "intensity") %in% names(testdata)))
    expect_gt(nrow(testdata), 0)
  })
})
