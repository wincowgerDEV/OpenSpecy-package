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
  expect_true(any(grepl("apply\\(library\\$spectra, 2", server_source)))
  expect_false(any(grepl("vapply\\(\\.\\$spectra", server_source)))
  expect_true(any(grepl("colnames\\(library_filtered\\(\\)\\$spectra\\)", server_source)))
  expect_true(any(grepl("colnames\\(DataR\\(\\)\\$spectra\\)", server_source)))
  expect_false(any(grepl("\\bnames\\(library_filtered\\(\\)\\$spectra\\)", server_source)))
  expect_false(any(grepl("\\bnames\\(DataR\\(\\)\\$spectra\\)", server_source)))
})

test_that("bundled Shiny app only watches heatmap clicks when heatmap exists", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- readLines(file.path(app_path, "server.R"), warn = FALSE)

  expect_true(any(grepl("ncol\\(preprocessed\\$data\\$spectra\\) > 1", server_source)))
  expect_true(any(grepl("event_data\\(\"plotly_click\"", server_source)))
})

test_that("bundled Shiny app does not block startup or auto-load remote images", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- readLines(file.path(app_path, "ui.R"), warn = FALSE)

  expect_false(any(grepl("modalDialog\\(", ui_source)))
  expect_false(any(grepl("img\\(src = \"https?://", ui_source)))
  expect_false(any(grepl("<iframe", ui_source, fixed = TRUE)))
  expect_false(any(grepl("width: 15vw", ui_source, fixed = TRUE)))
  expect_true(any(grepl("object-fit:contain", ui_source, fixed = TRUE)))
  expect_true(any(grepl("html.openspecy-busy-visible", ui_source,
                        fixed = TRUE)))
  bridge <- readLines(file.path(app_path, "www", "parent-frame.js"),
                      warn = FALSE)
  expect_true(any(grepl("shiny:busy.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("busyDelay = 650", bridge, fixed = TRUE)))
  expect_true(any(grepl("shiny:value.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("resultQuietPeriod = 2000", bridge, fixed = TRUE)))
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

test_that("bundled Shiny app uses package-downloaded libraries before network", {
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
  sys.source(file.path(app_path, "global.R"), envir = env)

  expected_library <- structure(list(source = "package"), class = "fake_lib")
  env$load_lib <- function(type, path = "system") {
    expect_identical(type, "medoid_derivative")
    if(identical(path, "system")) {
      return(expected_library)
    }
    stop("cache should not be checked when package library exists")
  }
  env$get_lib <- function(...) {
    stop("get_lib() should not run when package library exists")
  }

  expect_identical(env$load_app_library("medoid_derivative"), expected_library)
})

test_that("bundled Shiny app can match with a local cached library", {
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
  old_library_path <- Sys.getenv("OPENSPECY_SHINY_LIBRARY_PATH", unset = NA)
  on.exit(setwd(old_wd), add = TRUE)
  on.exit({
    if(is.na(old_library_path)) {
      Sys.unsetenv("OPENSPECY_SHINY_LIBRARY_PATH")
    } else {
      Sys.setenv(OPENSPECY_SHINY_LIBRARY_PATH = old_library_path)
    }
  }, add = TRUE)

  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  library_path <- file.path(tempdir(), "OpenSpecy-shiny-library-test")
  dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)
  Sys.setenv(OPENSPECY_SHINY_LIBRARY_PATH = library_path)

  x <- read_any(read_extdata("raman_hdpe.csv"))
  processed <- process_spec(
    x,
    active = TRUE,
    conform_spec = TRUE,
    conform_spec_args = list(range = NULL, res = 8, type = "interp"),
    smooth_intens = TRUE,
    smooth_intens_args = list(
      polynomial = 3,
      window = calc_window_points(seq(100, 4000, by = 8), 90),
      derivative = 1,
      abs = TRUE
    ),
    make_rel = TRUE
  )
  processed$metadata$organization <- "OpenSpecy smoke"
  processed$metadata$spectrum_type <- "raman"
  processed$metadata$material_class <- "HDPE"
  processed$metadata$spectrum_identity <- "HDPE smoke"
  processed$metadata$sample_name <- colnames(processed$spectra)
  saveRDS(processed, file.path(library_path, "derivative.rds"))

  real_load_lib <- load_lib
  download_attempted <- FALSE
  env$load_lib <- function(type, path = "system") {
    if(identical(path, "system")) {
      stop("simulate missing installed library")
    }
    real_load_lib(type, path = path)
  }
  env$get_lib <- function(...) {
    download_attempted <<- TRUE
    stop("network download should not be attempted")
  }

  local_library <- env$load_app_library("derivative")
  cors <- cor_spec(processed, library = local_library, conform = TRUE,
                   type = "roll")
  top_match <- max_cor_named(cors)

  expect_false(download_attempted)
  expect_identical(names(top_match)[[1]], colnames(local_library$spectra)[[1]])
  expect_gt(unname(top_match[[1]]), 0.99)
})
