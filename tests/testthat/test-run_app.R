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

test_that("bundled app uses one 10 GiB total upload ceiling", {
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

  limit <- 10 * 1024^3
  expect_identical(env$app_upload_limit_bytes(), limit)
  expect_identical(env$app_max_request_size_bytes(), limit)
  expect_true(env$app_validate_upload_size(data.frame(size = limit))$ok)
  expect_false(env$app_validate_upload_size(
    data.frame(size = limit + 1)
  )$ok)
  expect_true(env$app_validate_upload_size(
    data.frame(size = c(limit - 1, 1))
  )$ok)
  multi_over <- env$app_validate_upload_size(
    data.frame(size = c(limit - 1, 2))
  )
  expect_false(multi_over$ok)
  expect_match(multi_over$message, "10 GiB", fixed = TRUE)
  expect_false(env$app_validate_upload_size(data.frame(size = NA_real_))$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = Inf))$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = -1))$ok)

  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  bridge_source <- paste(readLines(
    file.path(app_path, "www", "parent-frame.js"), warn = FALSE
  ), collapse = "\n")
  expect_match(
    server_source,
    "options(shiny.maxRequestSize = app_max_request_size_bytes())",
    fixed = TRUE
  )
  expect_match(bridge_source,
               "10 * 1024 * 1024 * 1024", fixed = TRUE)
  expect_match(bridge_source, "setUploadStatus(", fixed = TRUE)
  expect_false(grepl("showUploadLimitPopup", bridge_source, fixed = TRUE))
})

test_that("bundled app has one in-memory upload route", {
  app_path <- run_app(test_mode = TRUE)
  source_paths <- file.path(app_path, c(
    "global.R", "server.R", "ui.R", file.path("www", "parent-frame.js")
  ))
  app_source <- paste(unlist(lapply(source_paths, readLines, warn = FALSE)),
                      collapse = "\n")
  prohibited <- c(
    "app_local_file_mode", "app_filespec_", "open_via_filespecs",
    "openspecy.shiny.local_files", "OPENSPECY_SHINY_LOCAL_FILES",
    "FileSpecs", "file-backed", "Local H5 / ENVI"
  )
  expect_false(any(vapply(
    prohibited, grepl, logical(1), x = app_source, fixed = TRUE
  )))

  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  expect_match(server_source, "readRDS(as.character(input$file$datapath[[1L]]))",
               fixed = TRUE)
  expect_match(server_source, "compute_file_id = FALSE", fixed = TRUE)
  expect_match(server_source,
               "read_any(\n              file = as.character(input$file$datapath), c_spec = FALSE",
               fixed = TRUE)
  expect_match(server_source, "combined <- if(is_OpenSpecy(members))",
               fixed = TRUE)
  expect_match(server_source, "upload_status_state(upload_size$message)",
               fixed = TRUE)
  expect_match(server_source,
               "candidates <- which(distance == min(distance))",
               fixed = TRUE)
  expect_match(server_source, "candidates[[length(candidates)]]",
               fixed = TRUE)
})

test_that("particle archives contain only canonical requested artifacts", {
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

  directory <- tempfile("openspecy-particle-outputs-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  expected <- c("particle_details.csv", "particles_processed.rds",
                "pixel_to_unit.csv", "top_matches.csv")
  file.create(file.path(directory, expected))

  archive <- tempfile(fileext = ".zip")
  env$app_write_particle_archive(
    file.path(directory, expected[c(1, 3, 4)]), archive, directory
  )
  expect_setequal(utils::unzip(archive, list = TRUE)$Name,
                  expected[c(1, 3, 4)])
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
  expect_false(any(grepl("apply\\(library\\$spectra, 2", server_source)))
  expect_true(any(grepl("OpenSpecy:::.match_spec_blockwise", server_source,
                        fixed = TRUE)))
  expect_false(any(grepl("vapply\\(\\.\\$spectra", server_source)))
  expect_true(any(grepl("colnames\\(library_filtered\\(\\)\\$spectra\\)", server_source)))
  expect_true(any(grepl("colnames\\(DataR\\(\\)\\$spectra\\)", server_source)))
  expect_false(any(grepl("\\bnames\\(library_filtered\\(\\)\\$spectra\\)", server_source)))
  expect_false(any(grepl("\\bnames\\(DataR\\(\\)\\$spectra\\)", server_source)))
})

test_that("bundled app updates map selection without full heatmap or spectrum redraws", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")

  expect_match(server_source, "ncol(preprocessed$data$spectra) > 1",
               fixed = TRUE)
  expect_match(server_source, "output$heatmapA <- plotly::renderPlotly({",
               fixed = TRUE)
  expect_false(grepl("app_draw_server_heatmap(", server_source, fixed = TRUE))
  expect_false(grepl("observeEvent(input$heatmap_click, {", server_source,
                     fixed = TRUE))
  expect_false(grepl("heatmap_popover_info", server_source, fixed = TRUE))
  expect_false(grepl("output$heatmap_popover <- renderUI({", server_source,
                     fixed = TRUE))
  expect_match(server_source, 'event_data("plotly_click", source = "heat_plot"',
               fixed = TRUE)
  # A cheap marker restyle (not a full heatmap redraw) syncs the selection
  # marker on click.
  expect_true(grepl('plotlyProxy("heatmapA", session)', server_source,
                    fixed = TRUE))
  expect_match(ui_source, 'plotly::plotlyOutput("heatmapA"', fixed = TRUE)
  expect_false(grepl('plotOutput(\n                  "heatmapA"', ui_source,
                     fixed = TRUE))
  expect_false(grepl('plotly::plotlyOutput("heatmapB"', ui_source,
                     fixed = TRUE))
  expect_match(server_source, "selected_match <- reactive({", fixed = TRUE)
  expect_match(server_source, "selected_match()", fixed = TRUE)
  expect_false(grepl("selected_match_cache", server_source, fixed = TRUE))

  spectrum_block <- sub(
    ".*output\\$MyPlotC <- renderPlotly\\(\\{", "", server_source
  )
  spectrum_block <- sub("#Heatmap ----.*", "", spectrum_block)
  expect_false(grepl("analysis_phase(", spectrum_block, fixed = TRUE))

  expect_match(
    ui_source,
    'choices = app_library_type_choices(), selected = "medoid"',
    fixed = TRUE
  )
  quantification_ui <- sub(
    ".*quantification_controls <- tagList\\(", "", ui_source
  )
  quantification_ui <- sub("# UI ----.*", "", quantification_ui)
  expect_false(grepl("sliderInput(", quantification_ui, fixed = TRUE))
})

test_that("run_app() launches without app-only local-file state", {
  tmp <- file.path(tempdir(), "OpenSpecy-testthat-run-app-launch")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(tmp, c("server.R", "ui.R")))

  local_mocked_bindings(
    .openspecy_require_shiny_packages = function() invisible(TRUE),
    runApp = function(...) "app-returned",
    .package = "OpenSpecy"
  )

  expect_identical(
    run_app(path = tmp, launch.browser = FALSE), "app-returned"
  )

  expect_identical(run_app(path = tmp, test_mode = TRUE),
                   normalizePath(tmp, winslash = "/", mustWork = TRUE))
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
  expect_true(any(grepl("analysisPhaseActive", bridge, fixed = TRUE)))
  expect_true(any(grepl("if (!analysisPhaseActive) return", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("elapsedTimer", bridge, fixed = TRUE)))
  expect_true(any(grepl("aria-valuenow", bridge, fixed = TRUE)))
  expect_true(any(grepl("busyState.progress", bridge, fixed = TRUE)))
  expect_false(any(grepl("Estimated remaining|state.eta", bridge)))
  expect_false(any(grepl("shiny:value.openspecyBusy", bridge, fixed = TRUE)))

  server_source <- readLines(file.path(app_path, "server.R"), warn = FALSE)
  expect_false(any(grepl("withProgress\\(", server_source)))
  expect_true(any(grepl("observeEvent(input$support_openspecy", server_source,
                        fixed = TRUE)))
  expect_true(any(grepl("showModal(modalDialog", server_source,
                        fixed = TRUE)))
})

test_that("bundled app runs corrections and identification unconditionally", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  expect_false(grepl("active_preprocessing", ui_source, fixed = TRUE))
  expect_false(grepl("active_identification", ui_source, fixed = TRUE))
  expect_false(grepl("active_advanced", ui_source, fixed = TRUE))
  expect_false(grepl("active_quantification", ui_source, fixed = TRUE))
  expect_match(ui_source, '"spike_decision", "Remove Isolated Spikes", TRUE',
               fixed = TRUE)
  expect_match(ui_source,
               '"saturation_decision", "Remove Saturated Ranges", FALSE',
               fixed = TRUE)
  expect_lt(regexpr('"co2_decision"', ui_source, fixed = TRUE)[[1L]],
            regexpr('"spike_decision"', ui_source, fixed = TRUE)[[1L]])
  expect_lt(regexpr('"spike_decision"', ui_source, fixed = TRUE)[[1L]],
            regexpr('"saturation_decision"', ui_source, fixed = TRUE)[[1L]])
  expect_match(ui_source, "Robust Residual Threshold is the prediction error",
               fixed = TRUE)
  expect_match(ui_source, "Neighbor Points per Side is the number",
               fixed = TRUE)
  expect_match(ui_source, "Detector Ceiling is expressed in the uploaded intensity units",
               fixed = TRUE)
  expect_match(ui_source, "0.10 means 10%", fixed = TRUE)
  expect_match(ui_source, '"range_automate"', fixed = TRUE)
  expect_match(ui_source, '"co2_automate"', fixed = TRUE)
  expect_match(server_source, "restrict_range = FALSE", fixed = TRUE)
  expect_match(server_source, "flatten_range = FALSE", fixed = TRUE)
  expect_match(server_source, "app_apply_range_automation", fixed = TRUE)
  expect_match(server_source, "app_apply_spectral_corrections", fixed = TRUE)
  expect_gt(regexpr("process_spec(", server_source, fixed = TRUE)[[1]], 0)
  expect_lt(
    regexpr("app_apply_spectral_corrections(", server_source,
            fixed = TRUE)[[1]],
    regexpr("process_spec(", server_source, fixed = TRUE)[[1]]
  )
  expect_gt(regexpr("app_apply_range_automation(", server_source,
                    fixed = TRUE)[[1]],
            regexpr("process_spec(", server_source, fixed = TRUE)[[1]])
  expect_match(server_source, "req(!is.null(preprocessed$data))", fixed = TRUE)
  expect_false(grepl("Library Spectra", server_source, fixed = TRUE))
  expect_match(ui_source, 'plotlyOutput("MyPlotC", height = "45vh")',
               fixed = TRUE)
  expect_match(
    ui_source,
    'shinyjs::disabled(\n        numericInput("MinRange"',
    fixed = TRUE
  )
  expect_match(
    ui_source,
    'shinyjs::disabled(\n        numericInput("MaxRange"',
    fixed = TRUE
  )
  expect_match(ui_source, "scans the full processed wavenumber axis",
               fixed = TRUE)
  expect_match(
    ui_source,
    "Manual bounds are ignored and locked while automatic mode is on",
    fixed = TRUE
  )
  expect_match(server_source, "observeEvent(input$range_automate, {",
               fixed = TRUE)
  expect_match(server_source,
               "manual_range <- !isTRUE(input$range_automate)",
               fixed = TRUE)
  expect_match(server_source,
               'shinyjs::toggleState("MinRange", condition = manual_range)',
               fixed = TRUE)
  expect_match(server_source,
               'shinyjs::toggleState("MaxRange", condition = manual_range)',
               fixed = TRUE)
  expect_match(server_source, "app_empty_spectrum_plot()", fixed = TRUE)
  expect_match(server_source, "active_ratio_definitions", fixed = TRUE)
  expect_match(server_source, "quantified_data", fixed = TRUE)
  expect_match(server_source, "app_attach_quantification", fixed = TRUE)
  expect_match(server_source, "RawR_plot <- reactive({", fixed = TRUE)
  expect_match(server_source, "reference <- selected_match()", fixed = TRUE)
  expect_match(server_source, "app_spectrum_plot(", fixed = TRUE)
  expect_match(server_source, 'report = "all"', fixed = TRUE)
  expect_match(server_source, "quality_findings <- reactive({", fixed = TRUE)
  expect_match(server_source, "app_threshold_quality_report(", fixed = TRUE)
  expect_match(server_source, "canonical_state <- reactive({", fixed = TRUE)
  expect_match(server_source, "canonical_final <- reactive({", fixed = TRUE)
  expect_match(server_source, "particle_pipeline_enabled <- reactive({",
               fixed = TRUE)
  expect_match(server_source,
               "isTRUE(input$collapse_decision) && !is.null(preprocessed$data)",
               fixed = TRUE)
  expect_match(server_source, "current_heatmap_data <- reactive({",
               fixed = TRUE)
  expect_match(server_source, "OpenSpecy:::.match_spec_blockwise(",
               fixed = TRUE)
  expect_match(server_source, "conform = FALSE, type = \"roll\"",
               fixed = TRUE)
  expect_match(server_source, "app_reference_for_query(", fixed = TRUE)
  expect_match(ui_source, 'role = "group"', fixed = TRUE)
  expect_match(ui_source, '"quality_automatic_details"', fixed = TRUE)
  expect_match(ui_source, '"quality_warning_details"', fixed = TRUE)
  expect_match(ui_source, '"quality_success_details"', fixed = TRUE)
  expect_false(grepl('"quality_error_details"', ui_source, fixed = TRUE))
  expect_false(grepl('"quality_pass_details"', ui_source, fixed = TRUE))
  expect_false(grepl("correlation_head", ui_source, fixed = TRUE))
  expect_false(grepl("output$correlation_head", server_source, fixed = TRUE))
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

test_that("bundled app presents one analysis workspace with advanced and quantification controls", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  global_source <- paste(readLines(file.path(app_path, "global.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, "dashboardSidebar(disable = TRUE)", fixed = TRUE)
  expect_false(grepl("sidebarMenu(", ui_source, fixed = TRUE))
  expect_false(grepl("googletranslate|uiOutput(\"translate\")", ui_source))
  expect_false(grepl("output$translate", server_source, fixed = TRUE))
  expect_match(ui_source, 'tabPanel(\n              "Preprocessing"', fixed = TRUE)
  expect_match(ui_source, 'tabPanel(\n              "Identification"', fixed = TRUE)
  expect_match(ui_source, 'tabPanel(\n              "Advanced"', fixed = TRUE)
  expect_match(ui_source, 'tabPanel(\n              "Quantification"', fixed = TRUE)
  expect_true(all(vapply(
    c("threshold_decision", "cor_threshold_decision", "spatial_decision",
      "xy_grid", "collapse_decision"),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
  expect_match(server_source, 'tags$summary("Top Matches columns")',
               fixed = TRUE)
  expect_match(ui_source, '"baseline_method", "Baseline Method"',
               fixed = TRUE)
  expect_match(ui_source, '"Fill Peaks (4S)" = "fill_peaks"',
               fixed = TRUE)
  expect_true(all(vapply(
    c(
      "quant_ratio_name", "quant_ratio_type",
      "quant_numerator_area_min", "quant_numerator_area_max",
      "quant_denominator_area_min", "quant_denominator_area_max",
      "quant_numerator_peak", "quant_denominator_peak",
      "quant_ratio_add", "quant_saved_ratios",
      "quant_measurement_name", "quant_measurement_type",
      "quant_measurement_area_min",
      "quant_measurement_area_max", "quant_measurement_wavenumber",
      "quant_measurement_add", "quant_measurement_remove",
      "quant_measurement_clear", "quant_measurement_definitions"
    ),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
  expect_false(grepl("quant_measurement_enabled", ui_source, fixed = TRUE))
  expect_false(grepl('"quant_ratio_bounds"', ui_source, fixed = TRUE))
  expect_false(grepl("app_quantification_indices", global_source,
                     fixed = TRUE))
  expect_false(grepl("quant_carbonyl_saub", ui_source, fixed = TRUE))
  expect_match(ui_source, '"run_analysis", "Run"', fixed = TRUE)
  expect_match(ui_source, "shinyjs::disabled(", fixed = TRUE)
  expect_match(server_source,
               'shinyjs::toggleState("run_analysis", condition = !is.null(preprocessed$data))',
               fixed = TRUE)
  expect_true(all(vapply(
    c("range_artifact_ratio", "co2_artifact_ratio"),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
  expect_false(grepl("range_automation_status", ui_source, fixed = TRUE))
  expect_false(grepl("co2_automation_status", ui_source, fixed = TRUE))
  expect_match(
    ui_source,
    "Every ratio uses exactly the final processed uploaded spectrum visible",
    fixed = TRUE
  )
  expect_false(grepl("quant_treatment|quant_fill_|quant_poly_", ui_source))
  expect_false(grepl("app_prepare_quantification_source", global_source,
                     fixed = TRUE))
  expect_false(grepl("quantification_treatment", server_source,
                     fixed = TRUE))
  expect_match(server_source,
               "app_attach_quantification(processed, definitions, measurements)",
               fixed = TRUE)
})

test_that("bundled app exposes the on-demand historical donation choices", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, '"Support Open Source Software"', fixed = TRUE)
  expect_match(ui_source, 'icon("donate")', fixed = TRUE)
  expect_match(server_source, 'title = tagList(icon("donate")', fixed = TRUE)
  expect_match(server_source, '"$25" = "https://www.paypal.com/donate/',
               fixed = TRUE)
  expect_match(server_source, '"Other" = "https://www.paypal.com/donate/',
               fixed = TRUE)
  expect_match(server_source, "easyClose = TRUE", fixed = TRUE)
})

test_that("bundled app uses collapsed responsive panels and one shared theme", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")

  expect_match(ui_source, 'id = "analysis_settings"', fixed = TRUE)
  expect_match(ui_source, 'id = "download_panel_box"', fixed = TRUE)
  expect_match(ui_source, 'id = "spectra_box"', fixed = TRUE)
  expect_match(ui_source, "dark = NULL", fixed = TRUE)
  expect_match(ui_source, "help = NULL", fixed = TRUE)
  expect_match(ui_source, "rightUi = tagList(", fixed = TRUE)
  expect_lt(
    regexpr("openspecy-version-item", ui_source, fixed = TRUE)[[1L]],
    regexpr("openspecy-support-item", ui_source, fixed = TRUE)[[1L]]
  )
  expect_match(ui_source, "collapsed = TRUE", fixed = TRUE)
  expect_match(ui_source, "title = shiny::downloadButton(", fixed = TRUE)
  expect_match(ui_source, "app_theme_css()", fixed = TRUE)
  expect_match(ui_source, "width: 100% !important",
               fixed = TRUE)
  expect_match(ui_source, "flex: 0 0 calc(100% - 44px)", fixed = TRUE)
  expect_match(ui_source, "gap: .65rem", fixed = TRUE)
  expect_match(ui_source, "white-space: nowrap", fixed = TRUE)
  expect_match(ui_source,
               ".selectize-control.dropdown-active { z-index: 1100; }",
               fixed = TRUE)
  expect_match(ui_source,
               ".selectize-dropdown { z-index: 1101 !important; }",
               fixed = TRUE)
  expect_match(ui_source,
               "#choice_names { position: relative; z-index: 20; }",
               fixed = TRUE)
  expect_match(ui_source, "background: var(--openspecy-success)",
               fixed = TRUE)
  expect_match(
    ui_source,
    ".btn.openspecy-quality-success {\n          border-color: var(--openspecy-success)",
    fixed = TRUE
  )
  expect_match(
    ui_source,
    ".openspecy-quality-icon-success {\n          color: var(--openspecy-success)",
    fixed = TRUE
  )
  expect_match(ui_source, "background: #FFFFFF", fixed = TRUE)
  expect_match(ui_source, 'class = "openspecy-summary-column"', fixed = TRUE)
  expect_match(ui_source, '#spectra_box,\n        #analysis_summary_box',
               fixed = TRUE)
  expect_false(grepl("bs4Dash::popover", ui_source, fixed = TRUE))
  expect_false(grepl('data-toggle="popover"', ui_source, fixed = TRUE))
  expect_match(
    ui_source,
    "#spectra_box .direct-chat-contacts {\n          z-index: 40;",
    fixed = TRUE
  )
  expect_match(ui_source, "#sidebar_tables {", fixed = TRUE)
  expect_match(ui_source, "#spectra_box #mycardsidebar {", fixed = TRUE)
  expect_match(
    ui_source,
    "#spectra_box.direct-chat-contacts-open #mycardsidebar",
    fixed = TRUE
  )
  expect_match(
    ui_source,
    "#spectra_box.direct-chat-contacts-open #choice_names {",
    fixed = TRUE
  )
  expect_match(ui_source, "pointer-events: none;", fixed = TRUE)
  expect_match(ui_source,
               "background: var(--openspecy-panel) !important;",
               fixed = TRUE)

  expect_match(server_source, 'id = "analysis_summary_box"', fixed = TRUE)
  expect_match(server_source, "app_spectrum_plot(", fixed = TRUE)
  expect_match(server_source, "app_ratio_metadata_columns",
               fixed = TRUE)
  expect_match(server_source, "quantified_data()$metadata", fixed = TRUE)
  expect_match(server_source, "app_summary_row(metric_items)", fixed = TRUE)
  expect_match(server_source, "app_summary_row(plot_items)", fixed = TRUE)
  expect_match(server_source, "automatic_report <- reactive({", fixed = TRUE)
  expect_match(server_source, "app_automatic_modal_content(automatic_report())",
               fixed = TRUE)
  expect_match(server_source, '"quality_automatic_details",', fixed = TRUE)
  expect_match(server_source, '"openspecy-automatic-applied",', fixed = TRUE)
  expect_false(grepl("automation_status_ui <- function", server_source,
                     fixed = TRUE))
  expect_false(grepl("range_automation_status", server_source, fixed = TRUE))
  expect_false(grepl("co2_automation_status", server_source, fixed = TRUE))
  expect_match(server_source,
               "artifact_ratio = co2_artifact_ratio", fixed = TRUE)
  expect_match(server_source,
               "artifact_ratio = range_artifact_ratio", fixed = TRUE)
  expect_match(server_source, "min = input$MinFlat", fixed = TRUE)
  expect_match(server_source, "max = input$MaxFlat", fixed = TRUE)
  expect_match(server_source,
               'updateNumericInput(\n            session, "MinRange"',
               fixed = TRUE)
  expect_match(server_source,
               'updateNumericInput(\n            session, "MaxRange"',
               fixed = TRUE)
  expect_match(server_source,
               'result$diagnostics$check == "high_tail"', fixed = TRUE)
  expect_match(
    server_source,
    'outputOptions(output, "download_ui", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
  download_ui <- sub(
    ".*output\\$download_ui <- renderUI\\(\\{",
    "",
    server_source
  )
  download_ui <- sub("observeEvent\\(input\\$download_selection,.*", "",
                     download_ui)
  expect_match(download_ui, "selectInput(", fixed = TRUE)
  expect_false(grepl("footnote(", download_ui, fixed = TRUE))
})

test_that("bundled app keeps disabled child controls out of analysis dependencies", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  bridge_source <- paste(readLines(
    file.path(app_path, "www", "parent-frame.js"), warn = FALSE
  ), collapse = "\n")

  expect_match(server_source,
               "intensity_args <- if(intensity_enabled)", fixed = TRUE)
  expect_match(server_source,
               "conform_args <- if(conform_enabled)", fixed = TRUE)
  expect_match(server_source,
               "baseline_args <- if(baseline_enabled)", fixed = TRUE)
  expect_match(server_source,
               "smooth_args <- if(smooth_enabled)", fixed = TRUE)
  expect_match(server_source, "effective_signal_selection", fixed = TRUE)
  # A single Run button, gated only on upload completion, is the sole
  # trigger for the expensive analysis tranche; there are no per-tab
  # owner switches left to keep child controls inert while disabled.
  expect_false(grepl("set_advanced_child_state <- function(enabled)",
                     server_source, fixed = TRUE))
  expect_false(grepl(
    "enabled ? el.selectize.enable() : el.selectize.disable()",
    server_source, fixed = TRUE
  ))
  expect_match(server_source,
               'shinyjs::toggleState("run_analysis", condition = !is.null(preprocessed$data))',
               fixed = TRUE)
  expect_false(grepl("active_preprocessing|active_identification|active_advanced|active_quantification",
                     server_source))
  expect_false(grepl("list(DataR(), input$signal_selection)", server_source,
                     fixed = TRUE))
  expect_match(server_source, "active_ratio_definitions", fixed = TRUE)
  expect_match(server_source, "active_measurement_definitions", fixed = TRUE)
  expect_false(grepl("quant_measurement_enabled", server_source,
                     fixed = TRUE))
  expect_match(server_source, "isolate(input$quant_ratio_name)",
               fixed = TRUE)
  expect_match(server_source,
               paste0(
                 "processed <- DataR()\n",
                 "    definitions <- active_ratio_definitions()\n",
                 "    measurements <- active_measurement_definitions()"
               ),
               fixed = TRUE)
  expect_match(server_source,
               "defaults <- app_quantification_defaults(",
               fixed = TRUE)
  expect_match(server_source, "quantification_axis <- reactiveVal(NULL)",
               fixed = TRUE)
  expect_match(
    server_source,
    "observeEvent(list(quantification_axis(), input$quant_ratio_type)",
    fixed = TRUE
  )
  expect_false(grepl(
    "observeEvent(list(input$file, input$quant_ratio_type)",
    server_source, fixed = TRUE
  ))
  expect_match(server_source, "axis = processed$wavenumber", fixed = TRUE)
  expect_false(grepl("quantification_source <- reactive", server_source,
                     fixed = TRUE))
  expect_match(bridge_source,
               "if (!analysisPhaseActive || !shinyIsBusy) return;",
               fixed = TRUE)
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
  expect_match(
    server_source,
    'return(paste0("os_metadata_", human_ts(), ".csv"))',
    fixed = TRUE
  )
  expect_match(
    server_source,
    'fwrite(data.table::as.data.table(user_metadata()), file)',
    fixed = TRUE
  )
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
  expect_true(is.function(env$app_download_label))
  expect_true(is.function(env$app_apply_range_automation))
  expect_true(is.function(env$app_apply_spectral_corrections))
  expect_true(is.function(env$app_attach_correction_metadata))
  expect_true(is.function(env$app_conform_axis))
  expect_true(is.function(env$app_quality_ui_report))
  expect_true(is.function(env$app_quality_status_report))
  expect_true(is.function(env$app_threshold_quality_report))
  expect_true(is.function(env$app_quality_counts))
  expect_true(is.function(env$app_quality_success_description))
  expect_true(is.function(env$app_quality_modal_content))
  expect_true(is.function(env$app_automatic_report))
  expect_true(is.function(env$app_automatic_modal_content))
  expect_true(is.function(env$app_category_palette))
  expect_true(is.function(env$app_category_colorscale))
  expect_true(is.function(env$app_heatmap_legend_layout))
  expect_true(is.function(env$app_add_measurement_definition))
  expect_true(is.function(env$app_measurement_definition_label))
  expect_true(is.function(env$app_spectrum_legend_layout))
  expect_true(is.function(env$app_theme_css))
  expect_true(is.function(env$app_summary_row))
  expect_true(is.function(env$app_style_plotly))
  expect_true(is.function(env$app_spectrum_plot))
  expect_true(is.function(env$app_empty_spectrum_plot))
  expect_match(env$app_version_display$text, "^OpenSpecy ")

  expect_true(all(c(
    "canvas", "panel", "panel_2", "border", "accent", "success", "text",
    "muted", "grid", "axis", "raw", "reference", "spectrum"
  ) %in% names(env$app_theme)))
  theme_css <- env$app_theme_css()
  expect_match(theme_css, "--openspecy-canvas:", fixed = TRUE)
  expect_match(theme_css, "--openspecy-panel-2:", fixed = TRUE)
  expect_match(theme_css, "--openspecy-accent:", fixed = TRUE)
  expect_match(theme_css, "--openspecy-success:", fixed = TRUE)
  expect_identical(env$app_plot_palette$primary, env$app_theme$accent)
  expect_identical(env$app_plot_palette$raw, env$app_theme$raw)
  expect_identical(env$app_plot_palette$reference, env$app_theme$reference)
  expect_identical(env$app_plot_palette$spectrum, "#FFFFFF")
  expect_error(env$app_theme_css(list()), "required color tokens")

  info <- htmltools::renderTags(env$footnote(
    "More information", "A substantive explanation."
  ))$html
  expect_match(paste(info, collapse = "\n"), "A substantive explanation.",
               fixed = TRUE)
  expect_error(env$footnote("More information"), "substantive details")

  one_item <- as.character(env$app_summary_row(list(tags$span("one"))))
  two_items <- as.character(env$app_summary_row(list(
    tags$span("one"), tags$span("two")
  )))
  three_items <- as.character(env$app_summary_row(list(
    tags$span("one"), tags$span("two"), tags$span("three")
  )))
  expect_match(one_item, "row openspecy-summary-grid", fixed = TRUE)
  expect_match(one_item, "col-sm-12 openspecy-summary-panel", fixed = TRUE)
  expect_equal(lengths(regmatches(
    two_items, gregexpr("col-sm-6", two_items, fixed = TRUE)
  )), 2L)
  expect_equal(lengths(regmatches(
    three_items, gregexpr("col-sm-4", three_items, fixed = TRUE)
  )), 3L)
  expect_null(env$app_summary_row(list(NULL, NULL)))

  expect_s3_class(env$app_empty_spectrum_plot(), "plotly")
  empty_plot <- plotly::plotly_build(env$app_empty_spectrum_plot())
  expect_identical(empty_plot$x$layout$paper_bgcolor,
                   env$app_plot_palette$panel)
  expect_identical(empty_plot$x$layout$xaxis$gridcolor,
                   env$app_plot_palette$grid)

  axis <- c(1000, 1100, 1200)
  active <- as_OpenSpecy(axis, data.frame(active = c(0.2, 0.8, 0.3)))
  raw <- as_OpenSpecy(axis, data.frame(raw = c(10, 20, 12)))
  reference <- as_OpenSpecy(axis, data.frame(reference = c(0.1, 0.7, 0.4)))
  spectrum_plot <- plotly::plotly_build(env$app_spectrum_plot(
    active = active, raw = raw, reference = reference
  ))
  spectrum_traces <- spectrum_plot$x$data
  expect_identical(
    vapply(spectrum_traces, `[[`, character(1), "name"),
    c("Raw spectrum", "Active spectrum", "Identification match")
  )
  expect_identical(
    vapply(spectrum_traces, function(trace) trace$line$color, character(1)),
    c("rgba(203, 213, 225, 0.24)", env$app_plot_palette$spectrum,
      env$app_plot_palette$reference)
  )
  expect_identical(
    vapply(spectrum_traces, function(trace) trace$line$dash, character(1)),
    c("solid", "solid", "dot")
  )
  expect_true(all(vapply(
    spectrum_traces, function(trace) isTRUE(trace$showlegend), logical(1)
  )))
  expect_identical(spectrum_plot$x$layout$legend$orientation, "v")
  expect_gt(spectrum_plot$x$layout$legend$x, 1)
  expect_gte(spectrum_plot$x$layout$margin$r, 180)

  mobile_plot <- plotly::plotly_build(env$app_spectrum_plot(
    active, raw = raw, reference = reference, plot_width = 390
  ))
  expect_identical(mobile_plot$x$layout$legend$orientation, "h")
  expect_lt(mobile_plot$x$layout$legend$y, 0)
  expect_gte(mobile_plot$x$layout$margin$b, 100)

  active_only <- plotly::plotly_build(env$app_spectrum_plot(active))
  expect_identical(
    vapply(active_only$x$data, `[[`, character(1), "name"),
    "Active spectrum"
  )
  raw_overlay <- plotly::plotly_build(env$app_spectrum_plot(active, raw = raw))
  expect_identical(
    vapply(raw_overlay$x$data, `[[`, character(1), "name"),
    c("Raw spectrum", "Active spectrum")
  )
  reference_overlay <- plotly::plotly_build(env$app_spectrum_plot(
    active, reference = reference
  ))
  expect_identical(
    vapply(reference_overlay$x$data, `[[`, character(1), "name"),
    c("Active spectrum", "Identification match")
  )

  normalized_overlays <- plotly::plotly_build(env$app_spectrum_plot(
    active, raw = raw, reference = reference, make_rel = TRUE
  ))
  expect_equal(
    as.numeric(normalized_overlays$x$data[[2L]]$y),
    as.numeric(as.matrix(active$spectra)[, 1L])
  )
  expect_equal(range(normalized_overlays$x$data[[1L]]$y), c(0, 1))
  expect_equal(range(normalized_overlays$x$data[[3L]]$y), c(0, 1))

  data_table_active <- active
  data_table_active$spectra <- data.table::as.data.table(active$spectra)
  data_table_plot <- plotly::plotly_build(
    env$app_spectrum_plot(data_table_active)
  )
  expect_identical(
    as.numeric(data_table_plot$x$data[[1L]]$y),
    c(0.2, 0.8, 0.3)
  )

  local({
    env$load_data()
    expect_s3_class(testdata, "data.table")
    expect_true(all(c("wavenumber", "intensity") %in% names(testdata)))
    expect_gt(nrow(testdata), 0)
  })
})

test_that("bundled app correction and quality helpers preserve auditable state", {
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

  plateau <- cbind(first = seq_len(40), second = seq_len(40) / 2)
  plateau[20:21, 1] <- 100
  saturated <- as_OpenSpecy(seq_len(40), as.data.frame(plateau))
  restricted <- env$app_apply_spectral_corrections(
    saturated, spike = FALSE, saturation = "auto",
    saturation_args = list(saturation_guard = 1L)
  )
  diagnostic <- attr(restricted, "saturation_restriction")
  expect_true(diagnostic$applied)
  expect_identical(
    attr(restricted, "app_automatic_correction_state"),
    c(spike = FALSE, saturation = TRUE)
  )
  rebuilt <- as_OpenSpecy(
    restricted$wavenumber,
    as.data.frame(restricted$spectra),
    metadata = restricted$metadata
  )
  rebuilt <- env$app_copy_correction_history(restricted, rebuilt)
  expect_identical(
    attr(rebuilt, "app_automatic_correction_state"),
    c(spike = FALSE, saturation = TRUE)
  )
  expect_false(any(env$app_conform_axis(restricted, 1) %in% 19:22))
  annotated <- env$app_attach_correction_metadata(restricted)
  expect_true(all(c(
    "saturation_restriction_applied",
    "saturation_restriction_reason",
    "saturation_loss_fraction",
    "saturation_excluded_ranges",
    "saturation_proposed_loss_fraction",
    "saturation_proposed_excluded_ranges",
    "saturation_detected_spectra"
  ) %in% names(annotated$metadata)))
  expect_true(all(annotated$metadata$saturation_restriction_applied))

  rejected_values <- c(0, rep(10, 9), 0)
  rejected_source <- as_OpenSpecy(
    0:10, data.frame(sample = rejected_values)
  )
  rejected <- env$app_apply_spectral_corrections(
    rejected_source, spike = FALSE, saturation = 10,
    saturation_args = list(
      saturation_guard = 0L, max_saturation_loss = 0.70
    )
  )
  rejected_metadata <- env$app_attach_correction_metadata(rejected)$metadata
  expect_false(rejected_metadata$saturation_restriction_applied)
  expect_equal(rejected_metadata$saturation_loss_fraction, 0)
  expect_gt(rejected_metadata$saturation_proposed_loss_fraction, 0.70)
  expect_true(is.na(rejected_metadata$saturation_excluded_ranges))
  expect_false(is.na(
    rejected_metadata$saturation_proposed_excluded_ranges
  ))

  report <- data.frame(
    status = c("error", "warning", "pass", "warning"),
    test_id = c(
      "spectrum:a:missing_values", "spectrum:a:low_snr",
      "spectrum:a:flat_spectrum", "spectrum:a:spike"
    ),
    check = c("missing_values", "low_snr", "flat_spectrum", "spike"),
    description = c(
      "Missing values found", "Low signal found", "Flat check passed",
      "Spike detected"
    ),
    likely_cause = c("Import problem", "Weak signal", NA, "Impulse"),
    potential_fix = c(
      "Repair the import", "Recollect", "No action required.", "Correct it"
    ),
    metric = c("non_finite_count", "snr", NA, "score"),
    value = c(1, 2, NA, 9), threshold = c(0, 4, NA, 8),
    region_min = c(NA, NA, NA, 60),
    region_max = c(NA, NA, NA, 60),
    stringsAsFactors = FALSE
  )
  ui_report <- env$app_quality_ui_report(report)
  expect_false("low_snr" %in% env$app_quality_checks)
  expect_identical(ui_report$check,
                   c("missing_values", "low_snr", "flat_spectrum"))
  expect_identical(ui_report$status, c("warning", "warning", "success"))
  expect_identical(
    env$app_quality_counts(report),
    c(warning = 2L, success = 1L)
  )
  warning_html <- paste(as.character(
    env$app_quality_modal_content(report, "warning")
  ), collapse = "")
  success_html <- paste(as.character(
    env$app_quality_modal_content(report, "success")
  ), collapse = "")
  expect_match(warning_html, "Finding:", fixed = TRUE)
  expect_match(warning_html, "Evidence:", fixed = TRUE)
  expect_match(warning_html, "Interpretation:", fixed = TRUE)
  expect_match(warning_html, "Action:", fixed = TRUE)
  expect_match(warning_html, "Missing values found", fixed = TRUE)
  expect_match(warning_html, "Low signal found", fixed = TRUE)
  expect_false(grepl("Flat check passed", warning_html, fixed = TRUE))
  expect_false(grepl("Spike detected", warning_html, fixed = TRUE))
  expect_match(success_html, "Finding:", fixed = TRUE)
  expect_match(success_html, "Evidence:", fixed = TRUE)
  expect_match(
    success_html,
    "finite intensity range exceeds the configured flat-spectrum tolerance",
    fixed = TRUE
  )
  expect_false(grepl("check passed", success_html, fixed = TRUE))
  expect_false(grepl("Interpretation:", success_html, fixed = TRUE))
  expect_false(grepl("Action:", success_html, fixed = TRUE))
  expect_false(grepl("No likely cause was recorded", success_html,
                     fixed = TRUE))
  expect_false(grepl("No action required", success_html, fixed = TRUE))
  expect_false(grepl("Missing values found", success_html, fixed = TRUE))
  expect_false(grepl("Low signal found", success_html, fixed = TRUE))
  expect_false(grepl("Automatic correction:", warning_html, fixed = TRUE))
  expect_false(grepl("Automatic correction:", success_html, fixed = TRUE))

  empty_warning_html <- paste(as.character(
    env$app_quality_modal_content(data.frame(), "warning")
  ), collapse = "")
  empty_success_html <- paste(as.character(
    env$app_quality_modal_content(data.frame(), "success")
  ), collapse = "")
  expect_match(empty_warning_html, "No warning findings", fixed = TRUE)
  expect_match(empty_success_html, "No success findings", fixed = TRUE)
  expect_false(grepl("Upload a spectrum", empty_warning_html, fixed = TRUE))
  expect_false(grepl("Upload a spectrum", empty_success_html, fixed = TRUE))

  threshold_report <- env$app_threshold_quality_report(
    "a", snr_value = 5, snr_threshold = 4,
    correlation_value = 0.7, correlation_threshold = 0.7
  )
  expect_identical(threshold_report$check,
                   c("snr_threshold", "correlation_threshold"))
  expect_identical(threshold_report$status, c("success", "warning"))
  expect_match(threshold_report$description[[1L]], "is above", fixed = TRUE)
  expect_match(threshold_report$description[[2L]], "does not exceed",
               fixed = TRUE)

  below <- env$app_threshold_quality_report(
    "a", snr_value = 3, snr_threshold = 4
  )
  unavailable <- env$app_threshold_quality_report(
    "a", correlation_value = NA_real_, correlation_threshold = 0.7
  )
  expect_identical(below$status, "warning")
  expect_match(below$description, "is below", fixed = TRUE)
  expect_identical(unavailable$status, "warning")
  expect_match(unavailable$description, "could not be evaluated",
               fixed = TRUE)
  infinite_snr <- env$app_threshold_quality_report(
    "a", snr_value = Inf, snr_threshold = 4
  )
  expect_identical(infinite_snr$status, "success")
  expect_match(infinite_snr$description, "Inf is above", fixed = TRUE)
  alternate_signal <- env$app_threshold_quality_report(
    "a", snr_value = 5, snr_threshold = 4,
    signal_metric = "sig_times_noise"
  )
  expect_identical(alternate_signal$metric, "signal_times_noise")
  expect_match(alternate_signal$description, "Signal times noise",
               fixed = TRUE)
  expect_error(
    env$app_threshold_quality_report(
      "a", snr_value = 5, snr_threshold = Inf
    ),
    "finite number"
  )

  combined_report <- data.table::rbindlist(
    list(report, threshold_report), use.names = TRUE, fill = TRUE
  )
  warning_rows <- env$app_quality_status_report(combined_report, "warning")
  success_rows <- env$app_quality_status_report(combined_report, "success")
  expect_true(nrow(warning_rows) > 0L)
  expect_true(nrow(success_rows) > 0L)
  expect_true(all(warning_rows$status == "warning"))
  expect_true(all(success_rows$status == "success"))
  expect_length(intersect(warning_rows$test_id, success_rows$test_id), 0L)
  expect_true("correlation_threshold" %in% warning_rows$check)
  expect_false("snr_threshold" %in% warning_rows$check)
  expect_true("snr_threshold" %in% success_rows$check)
  expect_false("correlation_threshold" %in% success_rows$check)

  automatic_source <- saturated
  attr(automatic_source, "automatic_spike") <- list(
    applied = TRUE,
    corrected_regions = data.frame(
      spectrum_id = "first", region_min = 20, region_max = 20
    ),
    affected_spectra = "first"
  )
  range_diagnostics <- data.frame(
    check = c("co2_region", "high_tail"),
    accepted = c(TRUE, FALSE),
    total_spectra = c(2L, 2L),
    before_passes = c(1L, 2L),
    after_passes = c(2L, 2L),
    reason = c("improved", "no_failures"),
    message = c("", ""),
    original_range_min = c(400, 400),
    original_range_max = c(4000, 4000),
    applied_range_min = c(2200, 400),
    applied_range_max = c(2400, 4000),
    stringsAsFactors = FALSE
  )
  automatic <- env$app_automatic_report(
    automatic_source,
    diagnostics = range_diagnostics,
    enabled = c(spike = TRUE, saturation = TRUE,
                flatten = TRUE, tails = TRUE)
  )
  expect_identical(automatic$step,
                   c("spike", "saturation", "flatten", "tails"))
  expect_identical(sum(automatic$applied), 2L)
  expect_match(
    automatic$summary[automatic$step == "spike"],
    "at 20 cm^-1", fixed = TRUE
  )
  expect_match(
    automatic$summary[automatic$step == "flatten"],
    "Flattened 2200-2400 cm^-1", fixed = TRUE
  )
  expect_false(grepl(
    "disabled", automatic$summary[automatic$step == "saturation"],
    fixed = TRUE
  ))
  automatic_html <- paste(as.character(
    env$app_automatic_modal_content(automatic)
  ), collapse = "")
  expect_identical(
    lengths(regmatches(
      automatic_html,
      gregexpr("openspecy-quality-finding-automatic", automatic_html,
               fixed = TRUE)
    )),
    4L
  )
  expect_identical(
    lengths(regmatches(
      automatic_html,
      gregexpr("openspecy-automatic-applied", automatic_html, fixed = TRUE)
    )),
    2L
  )

  canonical_state <- saturated
  attr(canonical_state, "app_automatic_correction_state") <- c(
    spike = FALSE, saturation = TRUE
  )
  state_report <- env$app_automatic_report(
    canonical_state,
    enabled = c(spike = TRUE, saturation = FALSE,
                flatten = FALSE, tails = FALSE)
  )
  expect_identical(
    state_report$outcome[state_report$step == "saturation"], "not_needed"
  )
  expect_identical(
    state_report$outcome[state_report$step == "spike"], "disabled"
  )

  colors <- vapply(env$app_heatmap_colorscale, `[[`, character(1), 2L)
  rgb <- grDevices::col2rgb(colors)
  expect_true(all(colMeans(rgb) > 85))
  expect_false(any(tolower(colors) %in% c("#000000", "#440154")))
})

test_that("bundled app renders scalable numeric and class heatmaps", {
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

  map <- as_OpenSpecy(
    c(1000, 1100),
    data.frame(a = c(1, 2), b = c(2, 3), c = c(3, 4), d = c(4, 5)),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )
  numeric_data <- env$app_ordinary_heatmap_data(
    map$metadata, c(0.2, 0.4, 0.6, 0.8), categorical = FALSE,
    legend_title = "Match Value"
  )
  widget <- expect_silent(env$app_particle_plotly(numeric_data))
  expect_s3_class(widget, "plotly")

  categories <- c("PET", "PE", "PP", "PE")
  palette <- env$app_category_palette(categories)
  colorscale <- env$app_category_colorscale(categories)
  expect_identical(names(palette), sort(unique(categories)))
  expect_identical(
    vapply(colorscale[seq.int(1L, length(colorscale), by = 2L)],
           `[[`, character(1), 2L),
    unname(palette)
  )
  masked_categories <- factor(
    c("PE", NA, "PP"), levels = c("PE", "PET", "PP")
  )
  expect_identical(
    names(env$app_category_palette(masked_categories)),
    levels(masked_categories)
  )

  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  expect_match(server_source,
               "match_name_palette <- reactive({", fixed = TRUE)
  expect_match(server_source,
               "app_category_palette(pixel_projection()$material)",
               fixed = TRUE)
  expect_match(server_source, "keep[is.na(keep)] <- FALSE", fixed = TRUE)
  expect_match(
    server_source,
    "rejected = projection$rejected",
    fixed = TRUE
  )
  expect_match(server_source, "app_material_summary_plot(", fixed = TRUE)
  expect_match(server_source, "map_color_choices <- reactive({", fixed = TRUE)
  expect_match(server_source, "resolved_map_color <- reactive({", fixed = TRUE)
  expect_match(server_source,
               'identical(selection, "Thresholded Particles")',
               fixed = TRUE)
  expect_false(grepl("app_draw_server_heatmap(", server_source, fixed = TRUE))
  expect_match(
    server_source, "heatmap_state <- reactive({", fixed = TRUE
  )
  expect_match(
    server_source, "app_particle_plotly(current_heatmap_data()", fixed = TRUE
  )
})

test_that("bundled app applies spike correction through the registered API", {
  app_path <- run_app(test_mode = TRUE)
  namespace_path <- normalizePath(
    file.path(app_path, "..", "..", "NAMESPACE"),
    winslash = "/", mustWork = FALSE
  )
  registered <- file.exists(namespace_path) && any(grepl(
    "^S3method\\(correct_spike,OpenSpecy\\)$",
    readLines(namespace_path, warn = FALSE)
  ))
  skip_if(!registered,
          "correct_spike.OpenSpecy is registered when documentation is regenerated")
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  axis <- seq_len(121)
  baseline <- sin(axis / 15)
  values <- baseline
  values[61] <- values[61] + 40
  spiked <- as_OpenSpecy(axis, data.frame(sample = values))
  corrected <- env$app_apply_spectral_corrections(
    spiked,
    spike_args = list(interpolation_points = 5L),
    saturation = NULL
  )
  expect_true(attr(corrected, "automatic_spike")$applied)
  expect_equal(
    unname(corrected$spectra[61, 1]), baseline[61], tolerance = 0.02
  )
})

test_that("bundled app quantifies the displayed processed spectra", {
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

  wavenumber <- seq(900, 3400, by = 10)
  spectra <- cbind(
    first = 1 + wavenumber / max(wavenumber),
    second = 2 + wavenumber / max(wavenumber)
  )
  processed <- as_OpenSpecy(wavenumber, as.data.frame(spectra))
  processed$spectra <- processed$spectra^2
  expect_identical(
    env$app_quantification_source_value,
    "displayed_processed_spectra"
  )

  definitions <- env$app_empty_ratio_definitions()
  expect_identical(
    names(definitions),
    c(
      "id", "name", "column", "type", "numerator_min", "numerator_max",
      "denominator_min", "denominator_max"
    )
  )
  expect_identical(nrow(definitions), 0L)

  area_defaults <- env$app_quantification_defaults(wavenumber, "area")
  expect_equal(area_defaults$min, 900)
  expect_equal(area_defaults$max, 3400)
  expect_equal(area_defaults$step, 1)
  expect_equal(area_defaults$numerator, c(1650, 1850))
  expect_equal(area_defaults$denominator, c(1420, 1500))
  expect_true(all(vapply(area_defaults, function(value) {
    is.numeric(value)
  }, logical(1))))

  peak_defaults <- env$app_quantification_defaults(wavenumber, "peak")
  expect_equal(peak_defaults$numerator, 1710)
  expect_equal(peak_defaults$denominator, 1460)
  expect_true(all(vapply(peak_defaults, function(value) {
    is.numeric(value)
  }, logical(1))))

  fractional_defaults <- env$app_quantification_defaults(
    seq(900.25, 3400.25, by = 10), "area"
  )
  expect_equal(fractional_defaults$min, 900.25)
  expect_equal(fractional_defaults$max, 3400.25)
  expect_equal(fractional_defaults$step, 1)
  expect_true(all(vapply(fractional_defaults, function(value) {
    is.numeric(value)
  }, logical(1))))

  definitions <- env$app_add_ratio_definition(
    definitions,
    name = "Custom Carbonyl",
    type = "area",
    numerator = c(1650, 1850),
    denominator = c(1420, 1500),
    axis = wavenumber
  )
  definitions <- env$app_add_ratio_definition(
    definitions,
    name = "Point Check",
    type = "peak",
    numerator = 1710,
    denominator = 1460,
    axis = wavenumber
  )
  expect_identical(
    definitions$column,
    c("area_ratio_custom_carbonyl", "peak_ratio_point_check")
  )

  measurements <- env$app_empty_measurement_definitions()
  expect_identical(
    names(measurements),
    c("id", "name", "column", "type", "minimum", "maximum")
  )
  measurements <- env$app_add_measurement_definition(
    measurements,
    name = "Carbonyl Area",
    type = "area",
    values = c(1650, 1850),
    axis = wavenumber
  )
  measurements <- env$app_add_measurement_definition(
    measurements,
    name = "Typed Point",
    type = "point",
    values = 1715,
    axis = wavenumber
  )
  expect_identical(
    measurements$column,
    c("area_under_band_carbonyl_area", "point_intensity_typed_point")
  )
  expect_identical(measurements$type, c("area", "point"))

  expected_area <- area_under_band(
    processed, min = 1650, max = 1850
  ) / area_under_band(
    processed, min = 1420, max = 1500
  )
  expect_equal(
    env$app_area_ratio(
      processed, numerator = c(1650, 1850),
      denominator = c(1420, 1500)
    ),
    expected_area
  )
  expected_peak <- peak_ratio(
    processed, numerator = 1710, denominator = 1460
  )
  expected_measurement_area <- area_under_band(
    processed, min = 1650, max = 1850
  )
  expect_warning(
    uncovered_area <- env$app_area_measurement(
      processed, bounds = c(1650, max(processed$wavenumber) + 1)
    ),
    "does not fully cover"
  )
  expect_true(all(is.na(uncovered_area)))
  expected_point <- point_intensity(
    processed, wavenumber = 1715, method = "nearest"
  )

  quantified <- env$app_attach_quantification(
    processed, definitions, measurements
  )

  expect_s3_class(quantified, "OpenSpecy")
  expect_identical(quantified$spectra, processed$spectra)
  expect_false("quantification_source" %in% names(processed$metadata))
  expect_false("quantification_treatment" %in% names(quantified$metadata))
  expect_identical(
    quantified$metadata$quantification_source,
    rep("displayed_processed_spectra", ncol(processed$spectra))
  )
  expect_true(all(grepl(
    paste0(
      "Ratios: Custom Carbonyl (area: 1650-1850 / 1420-1500 cm^-1); ",
      "Point Check (peak: 1710 / 1460 cm^-1)"
    ),
    quantified$metadata$quantification_definitions,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    paste0(
      "Measurements: Carbonyl Area (area: 1650-1850 cm^-1); ",
      "Typed Point (intensity: 1715 cm^-1)"
    ),
    quantified$metadata$quantification_definitions,
    fixed = TRUE
  )))
  expect_equal(
    quantified$metadata$area_ratio_custom_carbonyl,
    as.numeric(expected_area)
  )
  expect_equal(
    quantified$metadata$peak_ratio_point_check,
    as.numeric(expected_peak)
  )
  expect_equal(
    quantified$metadata$area_under_band_carbonyl_area,
    as.numeric(expected_measurement_area)
  )
  expect_equal(
    quantified$metadata$point_intensity_typed_point,
    as.numeric(expected_point)
  )
  expect_true(all(env$app_ratio_metadata_columns(
    definitions, measurements
  ) %in% names(quantified$metadata)))
  expect_identical(
    env$app_attach_quantification(
      processed, env$app_empty_ratio_definitions(),
      env$app_empty_measurement_definitions()
    ),
    processed
  )

  expect_error(
    env$app_add_ratio_definition(
      definitions,
      name = "Custom Carbonyl",
      type = "area",
      numerator = c(1650, 1850),
      denominator = c(1420, 1500),
      axis = wavenumber
    ),
    "same metadata name"
  )
  expect_error(
    env$app_add_ratio_definition(
      definitions,
      name = "Outside",
      type = "area",
      numerator = c(800, 850),
      denominator = c(1420, 1500),
      axis = wavenumber
    ),
    "within the displayed processed wavenumber range"
  )
  expect_warning(
    outside <- env$app_area_ratio(
      processed, numerator = c(800, 850),
      denominator = c(1420, 1500)
    ),
    "does not fully cover"
  )
  expect_true(all(is.na(outside)))
  expect_named(outside, colnames(processed$spectra))
})

test_that("bundled informational disclosures all contain detail content", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), "Missing Shiny app packages")

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)
  ui <- source(file.path(app_path, "ui.R"), local = env)$value
  html <- paste(htmltools::renderTags(ui)$html, collapse = "\n")
  blocks <- regmatches(
    html,
    gregexpr(
      '(?s)<details class="openspecy-info-details">.*?</details>',
      html, perl = TRUE
    )
  )[[1]]

  expect_gte(length(blocks), 15L)
  expect_true(all(grepl("openspecy-info-details-body", blocks,
                        fixed = TRUE)))
  expect_true(all(nchar(trimws(gsub("<[^>]+>", " ", blocks))) > 30L))
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
  expect_equal(
    unlist(flattened$diagnostics[1, c(
      "applied_range_min", "applied_range_max"
    )], use.names = FALSE),
    c(2200, 2400)
  )

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
  tail_detail <- attr(corrected$data, "automatic_tail")
  expect_equal(
    unlist(corrected$diagnostics[2, c(
      "original_range_min", "original_range_max"
    )], use.names = FALSE),
    tail_detail$original_range
  )
  expect_equal(
    unlist(corrected$diagnostics[2, c(
      "applied_range_min", "applied_range_max"
    )], use.names = FALSE),
    tail_detail$corrected_range
  )
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

test_that("bundled Test Map metadata renders and keeps spectrum alignment", {
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

  test_map <- suppressWarnings(read_any(read_extdata("CA_tiny_map.zip")))
  expect_identical(ncol(test_map$spectra), 208L)
  expect_identical(nrow(test_map$metadata), 208L)
  spectrum_ids <- colnames(test_map$spectra)
  signal_to_noise <- seq_along(spectrum_ids)
  original_metadata <- data.table::copy(test_map$metadata)

  cache <- env$app_uploaded_metadata_cache(test_map, signal_to_noise)
  expect_identical(test_map$metadata, original_metadata)
  expect_identical(cache$.openspecy_index, seq_len(208L))
  expect_identical(cache$signal_to_noise, signal_to_noise)

  table <- env$app_uploaded_metadata_table(cache)
  expect_s3_class(table, "datatables")
  expect_identical(nrow(table$x$data), 208L)
  expect_identical(table$x$filter, "top")
  expect_identical(table$x$options$pageLength, 5)
  expect_match(table$x$options$sDom, "ip", fixed = TRUE)
  expect_identical(attr(table$x$options, "escapeIdx"), "true")

  reordered <- test_map
  reordered$metadata <- data.table::copy(test_map$metadata[208:1])
  reordered_before <- data.table::copy(reordered$metadata)
  reordered_cache <- env$app_uploaded_metadata_cache(
    reordered, signal_to_noise
  )
  selected_row <- 37L
  expected_spectrum <- match(
    reordered$metadata$col_id[[selected_row]], spectrum_ids
  )
  expect_identical(
    env$app_uploaded_metadata_spectrum(reordered_cache, selected_row),
    as.integer(expected_spectrum)
  )
  expect_identical(
    env$app_uploaded_metadata_row(reordered_cache, expected_spectrum),
    selected_row
  )

  selected_match <- data.table::data.table(
    object_id = spectrum_ids[[137L]],
    material_class = "test material",
    spectrum_identity = "test reference",
    match_val = 0.91
  )
  selected <- env$app_selected_metadata(
    reordered, selected_match, signal_to_noise
  )
  expect_identical(reordered$metadata, reordered_before)
  expect_identical(selected$col_id, spectrum_ids[[137L]])
  expect_identical(selected$signal_to_noise, 137L)
  expect_identical(
    selected$x,
    test_map$metadata[col_id == spectrum_ids[[137L]], x]
  )
  selected_without_match <- env$app_selected_metadata(
    reordered,
    selected_match[, .(object_id, material_class, spectrum_identity)],
    signal_to_noise
  )
  expect_false("match_val" %in% names(selected_without_match))
  expect_identical(selected_without_match$col_id, spectrum_ids[[137L]])

  server_source <- paste(
    readLines(file.path(app_path, "server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    server_source,
    'outputOptions(output, "sidebar_metadata", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
  expect_match(server_source, "}, server = FALSE)", fixed = TRUE)
  expect_false(grepl("setkey(dataR_metadata", server_source, fixed = TRUE))
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
                   c("Test Data", "Test Map", "User Metadata"))
  expect_identical(env$app_download_choices(TRUE, FALSE),
                   c("Processed Spectra", "Test Data", "Test Map",
                     "User Metadata"))
  expect_identical(env$app_download_choices(TRUE, TRUE),
                   c("Top Matches", "Processed Spectra", "Test Data", "Test Map",
                     "User Metadata"))
  expect_identical(env$app_download_choices(TRUE, TRUE, collapse = TRUE),
                   c("Top Matches", "Processed Spectra", "Thresholded Particles",
                     "Test Data", "Test Map", "User Metadata"))

  expected_labels <- c(
    "Test Data" = "Download Test Data",
    "Test Map" = "Download Test Map",
    "Processed Spectra" = "Download Processed Spectra",
    "Top Matches" = "Download Top Matches",
    "Thresholded Particles" = "Download Thresholded Particles",
    "User Metadata" = "Download User Metadata"
  )
  expect_identical(
    vapply(names(expected_labels), env$app_download_label, character(1)),
    expected_labels
  )
  expect_identical(env$app_download_label(NULL), "Download selected")
  expect_identical(env$app_download_label("unsupported"), "Download selected")
})

test_that("top matches table guards against a rejected pixel selection", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  server_source <- paste(
    readLines(file.path(app_path, "server.R"), warn = FALSE),
    collapse = "\n"
  )

  block <- regmatches(
    server_source,
    regexpr("(?s)top_matches <- reactive\\(\\{.*?\\n  \\}\\)",
            server_source, perl = TRUE)
  )
  expect_length(block, 1L)
  expect_match(block, "req(!is.na(selected_unit_index()))", fixed = TRUE)

  # The guard must run before matches_to_single() is piped into
  # dplyr::select(), so a rejected (NA) selection halts the reactive instead
  # of reaching the select() call with matches_to_single()'s two-column
  # "nothing selected" stub, which is missing "material_class" and crashes.
  guard_pos <- regexpr("req(!is.na(selected_unit_index()))", block,
                        fixed = TRUE)
  select_pos <- regexpr("matches_to_single()", block, fixed = TRUE)
  expect_true(guard_pos < select_pos)
})

test_that("bundled app exports one-row metadata snapshots without restoring them", {
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

  expected_input_ids <- c(
    "spike_decision", "spike_direction",
    "spike_residual_threshold", "spike_residual_window",
    "saturation_decision", "saturation_mode", "saturation_ceiling",
    "saturation_max_loss", "make_rel_decision", "smooth_decision",
    "smoother", "derivative_order", "smoother_window", "derivative_abs",
    "conform_decision", "conform_selection", "conform_res",
    "intensity_decision", "intensity_corr", "baseline_decision",
    "baseline_method", "baseline", "refit", "baseline_lambda",
    "baseline_hwi", "iterations", "range_decision", "range_automate",
    "range_artifact_ratio", "MinRange", "MaxRange", "co2_decision",
    "co2_automate", "co2_artifact_ratio", "MinFlat", "MaxFlat",
    "id_spec_type", "id_strategy", "lib_type",
    "top_n_input", "filter_lib", "lib_org", "threshold_decision",
    "MinSNR", "MaxSNR", "signal_selection", "cor_threshold_decision", "MinCor",
    "spatial_decision", "sigma", "xy_grid", "preserve_uploaded_axis",
    "collapse_decision",
    "collapse_type", "particle_id_strategy", "particle_pca_components",
    "particle_cluster_k", "particle_area_threshold",
    "quant_ratio_name", "quant_ratio_type", "quant_numerator_area_min",
    "quant_numerator_area_max", "quant_denominator_area_min",
    "quant_denominator_area_max", "quant_numerator_peak",
    "quant_denominator_peak", "quant_measurement_name",
    "quant_measurement_type",
    "quant_measurement_area_min", "quant_measurement_area_max",
    "quant_measurement_wavenumber"
  )
  expect_identical(env$app_user_metadata_input_ids, expected_input_ids)

  settings <- stats::setNames(as.list(seq_along(expected_input_ids)),
                              expected_input_ids)
  settings$lib_org <- c("polymer", "fiber")
  snapshot <- env$app_user_metadata_snapshot(
    settings = settings,
    definitions = env$app_empty_ratio_definitions(),
    recorded_at = "2026-07-23 12:34:56 -0700",
    app_version = "1.2.3",
    session_id = "session-test"
  )
  provenance <- c(
    "recorded_at", "app_version", "session_id", "data_uploaded",
    "data_file_name", "data_file_size_bytes", "data_file_type",
    "data_file_last_modified", "data_digest_md5", "data_spectrum_count",
    "data_wavenumber_count", "data_wavenumber_min", "data_wavenumber_max"
  )
  expect_identical(
    names(snapshot),
    c(provenance, expected_input_ids, "quant_saved_ratio_count",
      "quant_saved_ratio_definitions", "quant_saved_measurement_count",
      "quant_saved_measurement_definitions")
  )
  expect_true(all(lengths(snapshot) == 1L))
  expect_false(snapshot$data_uploaded)
  expect_true(is.na(snapshot$data_digest_md5))
  expect_identical(snapshot$lib_org, "polymer | fiber")
  expect_equal(nrow(data.table::as.data.table(snapshot)), 1L)

  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                  warn = FALSE), collapse = "\n")
  expect_false(grepl(
    'fileInput\\s*\\(\\s*["\'](?:user_)?metadata(?:_file|_upload)?',
    ui_source, ignore.case = TRUE, perl = TRUE
  ))
  expect_false(grepl(
    "observeEvent\\s*\\(\\s*input\\$(?:user_)?metadata(?:_file|_upload)?",
    server_source, ignore.case = TRUE, perl = TRUE
  ))
})

test_that("bundled app updates the native download label without replacing it", {
  app_path <- run_app(test_mode = TRUE)
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  script_source <- paste(readLines(
    file.path(app_path, "www", "parent-frame.js"), warn = FALSE
  ), collapse = "\n")

  expect_match(server_source, '"openspecy-download-label"', fixed = TRUE)
  expect_match(server_source, "app_download_label(input$download_selection)",
               fixed = TRUE)
  expect_match(script_source,
               'addCustomMessageHandler("openspecy-download-label"',
               fixed = TRUE)
  expect_match(script_source, 'document.getElementById(state.id || "download_data")',
               fixed = TRUE)
  expect_match(script_source, 'button.setAttribute("aria-label", label)',
               fixed = TRUE)
  expect_match(script_source, "button.appendChild(icon)", fixed = TRUE)
  expect_match(script_source, '"#analysis_settings .nav-link"', fixed = TRUE)
  expect_match(script_source, 'this.closest("#analysis_settings_box")',
               fixed = TRUE)
  expect_match(script_source, '[data-card-widget="collapse"]', fixed = TRUE)
  expect_false(grepl('output$download_data <- renderUI', server_source,
                     fixed = TRUE))
})

test_that("bundled app bridges downloads only inside WebAssembly", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  bridge <- paste(readLines(
    file.path(app_path, "www", "parent-frame.js"), warn = FALSE
  ), collapse = "\n")

  expect_match(ui_source, 'name = "openspecy-wasm-mode"', fixed = TRUE)
  expect_match(ui_source, 'if(app_wasm_mode()) "true" else "false"',
               fixed = TRUE)
  expect_match(bridge, "function bindWasmDownloads()", fixed = TRUE)
  expect_match(bridge, "if (!isWasmMode()) return", fixed = TRUE)
  expect_match(bridge, "event.preventDefault()", fixed = TRUE)
  expect_match(bridge, "window.fetch(href", fixed = TRUE)
  expect_match(bridge, "response.blob()", fixed = TRUE)
  expect_match(bridge, "window.URL.createObjectURL(blob)", fixed = TRUE)
  expect_match(bridge, "localLink.download = safeFilename(filename)",
               fixed = TRUE)
  expect_match(bridge, "HTML page instead of the file", fixed = TRUE)
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
