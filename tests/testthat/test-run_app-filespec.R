source_filespec_app_global <- function() {
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
  setwd(app_path)
  on.exit(setwd(old_wd), add = TRUE)
  sys.source(file.path(app_path, "global.R"), envir = env)
  list(path = app_path, env = env)
}

test_that("large FileSpecs controls are local-only and app sources parse", {
  app_path <- run_app(test_mode = TRUE)
  for(file in c("global.R", "server.R", "ui.R")) {
    expect_error(parse(file.path(app_path, file)), NA)
  }

  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  expect_match(ui_source, "if(app_local_file_mode()) fluidRow", fixed = TRUE)
  expect_match(ui_source, 'id = "filespec_source_box"', fixed = TRUE)
  expect_match(ui_source, 'plotOutput(\n                "filespec_map"',
               fixed = TRUE)
  expect_match(ui_source, "output.filespec_active === true", fixed = TRUE)
  expect_match(ui_source, 'id = "filespec_map_brush"', fixed = TRUE)
  expect_match(ui_source, '"filespec_view_reset"', fixed = TRUE)
  expect_match(ui_source, '"filespec_close"', fixed = TRUE)
  expect_match(ui_source, "read-only", fixed = TRUE)
  expect_match(ui_source, "app_upload_guidance()", fixed = TRUE)
})

test_that("local filesystem access requires an explicit non-wasm opt-in", {
  app <- source_filespec_app_global()
  env <- app$env
  option_names <- c(
    "openspecy.shiny.local_files", "openspecy.shiny.wasm"
  )
  old_options <- options(option_names)
  old_local <- Sys.getenv("OPENSPECY_SHINY_LOCAL_FILES", unset = NA_character_)
  old_wasm <- Sys.getenv("OPENSPECY_SHINY_WASM", unset = NA_character_)
  on.exit({
    options(old_options)
    if(is.na(old_local)) Sys.unsetenv("OPENSPECY_SHINY_LOCAL_FILES") else
      Sys.setenv(OPENSPECY_SHINY_LOCAL_FILES = old_local)
    if(is.na(old_wasm)) Sys.unsetenv("OPENSPECY_SHINY_WASM") else
      Sys.setenv(OPENSPECY_SHINY_WASM = old_wasm)
  }, add = TRUE)

  options(openspecy.shiny.local_files = NULL, openspecy.shiny.wasm = NULL)
  Sys.unsetenv(c("OPENSPECY_SHINY_LOCAL_FILES", "OPENSPECY_SHINY_WASM"))
  expect_false(env$app_local_file_mode())

  options(openspecy.shiny.local_files = TRUE)
  expect_true(env$app_local_file_mode())
  options(openspecy.shiny.wasm = TRUE)
  expect_false(env$app_local_file_mode())

  options(openspecy.shiny.local_files = NULL, openspecy.shiny.wasm = NULL)
  Sys.setenv(OPENSPECY_SHINY_LOCAL_FILES = "yes")
  expect_true(env$app_local_file_mode())
  Sys.setenv(OPENSPECY_SHINY_WASM = "true")
  expect_false(env$app_local_file_mode())
})

test_that("the app FileSpecs cache uses an override or process temp storage", {
  app <- source_filespec_app_global()
  env <- app$env
  old_cache <- Sys.getenv("OPENSPECY_FILE_SPECS_CACHE", unset = NA_character_)
  on.exit({
    if(is.na(old_cache)) Sys.unsetenv("OPENSPECY_FILE_SPECS_CACHE") else
      Sys.setenv(OPENSPECY_FILE_SPECS_CACHE = old_cache)
  }, add = TRUE)

  override <- tempfile("openspecy-app-filespec-cache-")
  on.exit(unlink(override, recursive = TRUE), add = TRUE)
  Sys.setenv(OPENSPECY_FILE_SPECS_CACHE = override)
  expect_identical(
    env$app_filespec_cache_dir(),
    normalizePath(override, winslash = "/", mustWork = TRUE)
  )
  expect_identical(unname(file.access(override, mode = 2L)), 0L)

  Sys.unsetenv("OPENSPECY_FILE_SPECS_CACHE")
  default <- env$app_filespec_cache_dir()
  expect_true(startsWith(
    default, normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  ))
})

test_that("ordinary uploads are bounded with local and hosted guidance", {
  app <- source_filespec_app_global()
  env <- app$env

  expect_true(env$app_validate_upload_size(
    data.frame(size = 1024), wasm = FALSE
  )$ok)
  local <- env$app_validate_upload_size(
    data.frame(size = 513 * 1024^2), wasm = FALSE
  )
  expect_false(local$ok)
  expect_match(local$message, "Large local H5 / ENVI source", fixed = TRUE)
  hosted <- env$app_validate_upload_size(
    data.frame(size = 101 * 1024^2), wasm = TRUE
  )
  expect_false(hosted$ok)
  expect_match(hosted$message, "hosted app", fixed = TRUE)
  expect_match(hosted$message, "Run the local OpenSpecy app", fixed = TRUE)
  expect_match(env$app_upload_guidance(TRUE), "100 MB", fixed = TRUE)
  expect_match(
    env$app_upload_guidance(TRUE), "Run the local OpenSpecy app", fixed = TRUE
  )
})

test_that("FileSpecs index previews and selections remain bounded", {
  app <- source_filespec_app_global()
  env <- app$env
  index <- data.frame(
    region = rep(c("R1", "R2"), each = 1000L),
    x = rep(0:999, 2L), y = rep(999:0, 2L),
    stage_x_nm = rep(10000 + 0:999, 2L),
    stage_y_nm = rep(20000 + 999:0, 2L),
    stringsAsFactors = FALSE
  )

  preview <- env$app_filespec_preview(
    index, "R1", max_width = 128L, max_height = 96L
  )
  expect_lte(ncol(preview$counts), 128L)
  expect_lte(nrow(preview$counts), 96L)
  expect_lte(as.numeric(object.size(preview$counts)), 2 * 1024^2)
  expect_identical(preview$spectra, 1000L)
  expect_identical(preview$total_spectra, 1000L)
  expect_identical(preview$xlab, "Stage X (nm)")
  expect_equal(sum(preview$counts), 1000L)
  expect_identical(
    env$app_filespec_nearest_position(index, "R2", 10025, 20974),
    1026L
  )
  roi <- c(10010, 10020, 20979, 20989)
  roi_preview <- env$app_filespec_preview(index, "R2", roi = roi)
  expect_identical(roi_preview$spectra, 11L)
  expect_identical(roi_preview$total_spectra, 1000L)
  expect_equal(unname(roi_preview$viewport), roi)
  expect_equal(sum(roi_preview$counts), 11L)
  expect_identical(
    env$app_filespec_nearest_position(
      index, "R2", 10025, 20974, roi = roi
    ),
    1021L
  )
  expect_error(
    env$app_filespec_preview(index, "R2", roi = c(-4, -2, -8, -6)),
    "does not overlap"
  )
  expect_length(
    env$app_filespec_nearest_position(index, "R2", NA_real_, 1), 0L
  )
})

test_that("large source server path materializes only one selection", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- paste(
    readLines(file.path(app_path, "server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server_source, "final_specs <- reactiveVal(NULL)", fixed = TRUE)
  expect_match(server_source, "final_selection <- reactiveVal(NULL)",
               fixed = TRUE)
  expect_match(server_source,
               "OpenSpecy:::.filespec_read(specs, position)", fixed = TRUE)
  expect_match(server_source, "final_selection(selected)", fixed = TRUE)
  expect_match(server_source, "preprocessed$data <- selected", fixed = TRUE)
  expect_match(server_source, "OpenSpecy:::.filespec_index(opened)",
               fixed = TRUE)
  expect_match(server_source, "OpenSpecy:::.filespec_regions(opened)",
               fixed = TRUE)
  expect_match(server_source, "selected <- final_selection()", fixed = TRUE)
  expect_match(server_source, "da <- active_source()", fixed = TRUE)
  expect_match(server_source, "filespec_viewport_state <- reactiveVal(NULL)",
               fixed = TRUE)
  expect_match(server_source, "input$filespec_map_brush", fixed = TRUE)
  expect_match(server_source, 'shift_filespec_view("right")', fixed = TRUE)
  expect_match(server_source, "req(app_local_file_mode())", fixed = TRUE)
  expect_match(server_source, "cache_dir = app_filespec_cache_dir()",
               fixed = TRUE)
  expect_match(server_source, "session$onSessionEnded(function()", fixed = TRUE)
  expect_match(server_source,
               "upload_size <- app_validate_upload_size", fixed = TRUE)
  expect_match(
    server_source,
    "options(shiny.maxRequestSize = app_upload_limit_bytes())",
    fixed = TRUE
  )
  expect_false(grepl("10000*1024^2", server_source, fixed = TRUE))
  expect_false(grepl("req(input$file)", server_source, fixed = TRUE))

  global_source <- paste(
    readLines(file.path(app_path, "global.R"), warn = FALSE), collapse = "\n"
  )
  expect_false(grepl("escape = FALSE", global_source, fixed = TRUE))
  expect_false(grepl("escape = FALSE", server_source, fixed = TRUE))

  opener <- sub(".*observeEvent\\(input\\$filespec_open", "", server_source)
  opener <- sub("observeEvent\\(input\\$support_openspecy.*", "", opener)
  expect_false(grepl("plotly", opener, ignore.case = TRUE))
  expect_false(grepl("decompress_spec", opener, fixed = TRUE))
})
