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
  expect_true(any(grepl('id = "openspecy_busy_overlay"', ui_source,
                        fixed = TRUE)))
  expect_true(any(grepl('id = "openspecy_busy_elapsed"', ui_source,
                        fixed = TRUE)))
  expect_true(any(grepl('id = "openspecy_busy_progress"', ui_source,
                        fixed = TRUE)))
  expect_false(any(grepl("openspecy_busy_eta", ui_source, fixed = TRUE)))
  bridge <- readLines(file.path(app_path, "www", "parent-frame.js"),
                      warn = FALSE)
  expect_true(any(grepl("shiny:busy.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("busyDelay = 650", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy-analysis-phase", bridge, fixed = TRUE)))
  expect_true(any(grepl("elapsedTimer", bridge, fixed = TRUE)))
  expect_true(any(grepl("aria-valuenow", bridge, fixed = TRUE)))
  expect_true(any(grepl("busyState.progress", bridge, fixed = TRUE)))
  expect_false(any(grepl("Estimated remaining|state.eta", bridge)))
  expect_false(any(grepl("shiny:value.openspecyBusy", bridge, fixed = TRUE)))

  server_source <- readLines(file.path(app_path, "server.R"), warn = FALSE)
  expect_false(any(grepl("withProgress\\(", server_source)))
})

test_that("bundled app defaults to automated range processing and identification", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, 'app_section_switch("active_preprocessing"',
               fixed = TRUE)
  expect_match(ui_source, 'app_section_switch("active_identification"',
               fixed = TRUE)
  expect_match(ui_source, '"range_automate"', fixed = TRUE)
  expect_match(ui_source, '"co2_automate"', fixed = TRUE)
  expect_match(server_source, "restrict_range = FALSE", fixed = TRUE)
  expect_match(server_source, "flatten_range = FALSE", fixed = TRUE)
  expect_match(server_source, "app_apply_range_automation", fixed = TRUE)
  expect_gt(regexpr("process_spec(", server_source, fixed = TRUE)[[1]], 0)
  expect_gt(regexpr("app_apply_range_automation(", server_source,
                    fixed = TRUE)[[1]],
            regexpr("process_spec(", server_source, fixed = TRUE)[[1]])
  expect_match(server_source, "req(!is.null(preprocessed$data))", fixed = TRUE)
  expect_false(grepl("Library Spectra", server_source, fixed = TRUE))
  expect_match(ui_source, 'plotlyOutput("MyPlotC", height = "45vh")',
               fixed = TRUE)
  expect_match(server_source, "app_empty_spectrum_plot()", fixed = TRUE)
})

test_that("bundled app namespaces dashboard boxes", {
  app_path <- run_app(test_mode = TRUE)
  sources <- unlist(lapply(c("ui.R", "server.R"), function(file) {
    readLines(file.path(app_path, file), warn = FALSE)
  }))

  expect_true(any(grepl("bs4Dash::box\\(", sources)))
  expect_false(any(grepl("(?<![:[:alnum:]_])box\\(", sources,
                         perl = TRUE)))
})

test_that("bundled Shiny app prunes imported orphan assets", {
  app_path <- run_app(test_mode = TRUE)
  assets <- list.files(file.path(app_path, "www"), recursive = TRUE)

  expect_false(any(grepl(
    paste0("jumbotron\\.png|dancing\\.jpg|jqfp\\.js|md5\\.js|",
           "shinyBindings\\.js|googletranslate\\.html"),
    assets
  )))
})

test_that("bundled app presents one analysis workspace with advanced controls", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, "dashboardSidebar(disable = TRUE)", fixed = TRUE)
  expect_false(grepl("sidebarMenu(", ui_source, fixed = TRUE))
  expect_false(grepl("googletranslate|uiOutput(\"translate\")", ui_source))
  expect_false(grepl("output$translate", server_source, fixed = TRUE))
  expect_match(ui_source, 'tabPanel(\n              "Preprocessing"', fixed = TRUE)
  expect_match(ui_source, 'tabPanel(\n              "Identification"', fixed = TRUE)
  expect_match(ui_source, 'tabPanel(\n              "Advanced"', fixed = TRUE)
  expect_true(all(vapply(
    c("threshold_decision", "cor_threshold_decision", "spatial_decision",
      "xy_grid", "collapse_decision"),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
  expect_match(server_source, 'tags$summary("Top Match options")', fixed = TRUE)
})

test_that("bundled app keeps a stable native download link", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  global_source <- paste(readLines(file.path(app_path, "global.R"),
                                   warn = FALSE), collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, 'shiny::downloadButton(\n              "download_data"',
               fixed = TRUE)
  expect_false(grepl("downloadButton <- function", global_source, fixed = TRUE))
  expect_false(grepl('label = downloadButton("download_data"', server_source,
                     fixed = TRUE))
  expect_match(server_source, "overwrite = TRUE", fixed = TRUE)
  expect_match(server_source, "did not create a nonempty download", fixed = TRUE)
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
  expect_true(is.function(env$app_download_choices))
  expect_true(is.function(env$app_apply_range_automation))
  expect_true(is.function(env$app_style_plotly))
  expect_true(is.function(env$app_empty_spectrum_plot))
  expect_match(env$app_version_display$text, "^OpenSpecy ")

  expect_s3_class(env$app_empty_spectrum_plot(), "plotly")
  empty_plot <- plotly::plotly_build(env$app_empty_spectrum_plot())
  expect_identical(empty_plot$x$layout$paper_bgcolor,
                   env$app_plot_palette$panel)
  expect_identical(empty_plot$x$layout$xaxis$gridcolor,
                   env$app_plot_palette$grid)

  local({
    env$load_data()
    expect_s3_class(testdata, "data.table")
    expect_true(all(c("wavenumber", "intensity") %in% names(testdata)))
    expect_gt(nrow(testdata), 0)
  })
})

test_that("bundled app accepts only improving post-processing corrections", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  wavenumber <- seq(1000, 3000, by = 10)
  spectra <- matrix(
    0.1,
    nrow = length(wavenumber), ncol = 3,
    dimnames = list(NULL, c("co2", "tail", "clean"))
  )
  spectra[wavenumber == 1600, ] <- 0.35
  spectra[wavenumber > 2200 & wavenumber < 2400, "co2"] <- 1
  spectra[seq_len(5), "tail"] <- 1
  batch <- as_OpenSpecy(wavenumber, as.data.frame(spectra))
  batch$metadata$group <- c("a", "b", "c")
  attr(batch, "source_tag") <- "post-processing fixture"

  real_flatten <- OpenSpecy::flatten_range
  real_restrict <- OpenSpecy::restrict_range

  clean <- filter_spec(batch, logic = "clean")
  flatten_calls <- restrict_calls <- 0L
  env$flatten_range <- function(...) {
    flatten_calls <<- flatten_calls + 1L
    stop("clean CO2 spectra must not be corrected")
  }
  env$restrict_range <- function(...) {
    restrict_calls <<- restrict_calls + 1L
    stop("clean spectral tails must not be corrected")
  }
  clean_result <- env$app_apply_range_automation(clean)
  expect_identical(clean_result$data, clean)
  expect_identical(c(flatten_calls, restrict_calls), c(0L, 0L))
  expect_identical(clean_result$diagnostics$reason,
                   c("no_failures", "no_failures"))
  expect_false(any(clean_result$diagnostics$attempted))

  co2_only <- filter_spec(batch, logic = "co2")
  env$flatten_range <- function(x, ...) {
    flatten_calls <<- flatten_calls + 1L
    attr(x, "candidate_only") <- TRUE
    x
  }
  rejected <- env$app_apply_range_automation(
    co2_only, flatten = TRUE, restrict = FALSE
  )
  expect_identical(rejected$data, co2_only)
  expect_identical(rejected$diagnostics$reason,
                   c("not_improved", "disabled"))
  expect_true(rejected$diagnostics$attempted[[1]])
  expect_false(rejected$diagnostics$accepted[[1]])
  expect_identical(rejected$diagnostics$before_passes[[1]], 0L)
  expect_identical(rejected$diagnostics$after_passes[[1]], 0L)

  env$flatten_range <- real_flatten
  flattened <- env$app_apply_range_automation(
    batch, flatten = TRUE, restrict = FALSE
  )
  expect_true(flattened$diagnostics$accepted[[1]])
  expect_identical(flattened$diagnostics$before_passes[[1]], 2L)
  expect_identical(flattened$diagnostics$after_passes[[1]], 3L)

  restrict_received_flattened <- FALSE
  env$restrict_range <- function(x, ...) {
    restrict_received_flattened <<-
      !is.null(attr(x, "automatic_flatten"))
    x
  }
  staged <- env$app_apply_range_automation(batch)
  expect_true(restrict_received_flattened)
  expect_identical(staged$data, flattened$data)
  expect_identical(staged$diagnostics$reason,
                   c("improved", "not_improved"))

  env$restrict_range <- real_restrict
  corrected <- env$app_apply_range_automation(batch)
  expect_identical(corrected$diagnostics$reason,
                   c("improved", "improved"))
  expect_true(all(corrected$diagnostics$accepted))
  expect_identical(corrected$diagnostics$before_passes, c(2L, 2L))
  expect_identical(corrected$diagnostics$after_passes, c(3L, 3L))
  expect_equal(ncol(corrected$data$spectra), ncol(batch$spectra))
  expect_identical(colnames(corrected$data$spectra),
                   colnames(batch$spectra))
  expect_identical(corrected$data$metadata, batch$metadata)
  expect_identical(attr(corrected$data, "source_tag"),
                   attr(batch, "source_tag"))
  expect_false(is.null(attr(corrected$data, "automatic_flatten")))
  expect_false(is.null(attr(corrected$data, "automatic_tail")))
  expect_equal(nrow(assess_spec(
    corrected$data, checks = c("co2_region", "high_tail")
  )), 0L)
})

test_that("bundled Test Map exercises both post-processing corrections", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  test_map <- suppressWarnings(
    read_any(read_extdata("CA_tiny_map.zip")) |>
      c_spec(range = "common", res = 6) |>
      manage_na(ig = c(NA, 0), type = "remove")
  )
  processed <- process_spec(
    test_map,
    active = TRUE,
    adj_intens = FALSE,
    conform_spec = TRUE,
    conform_spec_args = list(range = NULL, res = 6, type = "interp"),
    restrict_range = FALSE,
    flatten_range = FALSE,
    subtr_baseline = FALSE,
    smooth_intens = TRUE,
    smooth_intens_args = list(
      polynomial = 3,
      window = calc_window_points(seq(100, 4000, by = 6), 90),
      derivative = 1,
      abs = TRUE
    ),
    make_rel = TRUE
  )

  result <- env$app_apply_range_automation(processed)
  expect_identical(result$diagnostics$step,
                   c("flatten_range", "restrict_range"))
  expect_true(all(result$diagnostics$attempted))
  expect_true(all(result$diagnostics$accepted))
  expect_true(all(result$diagnostics$after_passes >
                    result$diagnostics$before_passes))
  expect_identical(colnames(result$data$spectra),
                   colnames(processed$spectra))
  expect_identical(result$data$metadata, processed$metadata)
  expect_equal(nrow(assess_spec(
    result$data, checks = c("co2_region", "high_tail")
  )), 0L)
})

test_that("bundled app orders downloads from the current analysis state", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  expect_identical(env$app_download_choices(FALSE, TRUE),
                   c("Test Data", "Test Map"))
  expect_identical(env$app_download_choices(TRUE, FALSE),
                   c("Processed Spectra", "Test Data", "Test Map"))
  expect_identical(env$app_download_choices(TRUE, TRUE),
                   c("Top Matches", "Processed Spectra", "Test Data", "Test Map"))
  expect_identical(env$app_download_choices(TRUE, TRUE, collapse = TRUE),
                   c("Top Matches", "Processed Spectra", "Thresholded Particles",
                     "Test Data", "Test Map"))
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
