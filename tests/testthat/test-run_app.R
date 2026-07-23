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

test_that("bundled app defaults artifact automation and identification but not quantification", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
                     collapse = "\n")
  server_source <- paste(readLines(file.path(app_path, "server.R"),
                                   warn = FALSE), collapse = "\n")
  expect_match(ui_source, '"active_preprocessing", "Preprocessing", TRUE',
               fixed = TRUE)
  expect_match(ui_source, '"active_identification", "Identification", TRUE',
               fixed = TRUE)
  expect_match(
    ui_source,
    'app_section_switch("active_quantification", "Quantification", FALSE)',
    fixed = TRUE
  )
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
  expect_match(
    server_source,
    "if(!isTRUE(input$active_preprocessing)) return(NULL)",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "reference <- if(isTRUE(input$active_identification))",
    fixed = TRUE
  )
  expect_match(server_source, "app_spectrum_plot(", fixed = TRUE)
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
  expect_match(server_source, 'tags$summary("Top Match options")', fixed = TRUE)
  expect_match(ui_source, '"baseline_method", "Baseline Method"',
               fixed = TRUE)
  expect_match(ui_source, '"Fill Peaks (4S)" = "fill_peaks"',
               fixed = TRUE)
  expect_true(all(vapply(
    c("quant_ratio_name", "quant_ratio_type", "quant_ratio_bounds",
      "quant_ratio_add", "quant_saved_ratios"),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
  expect_false(grepl("app_quantification_indices", global_source,
                     fixed = TRUE))
  expect_false(grepl("quant_carbonyl_saub", ui_source, fixed = TRUE))
  expect_match(
    ui_source,
    "Transforms uploaded spectra before artifact checks and identification.",
    fixed = TRUE
  )
  expect_match(
    ui_source,
    "Matches processed spectra to references and displays the best results.",
    fixed = TRUE
  )
  expect_match(
    ui_source,
    "These settings operate independently of both Preprocessing and Identification.",
    fixed = TRUE
  )
  expect_match(ui_source, "openspecy-section-description", fixed = TRUE)
  expect_true(all(vapply(
    c("range_artifact_ratio", "range_automation_status",
      "co2_artifact_ratio", "co2_automation_status"),
    function(id) grepl(paste0('"', id, '"'), ui_source, fixed = TRUE),
    logical(1)
  )))
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
               "app_attach_quantification(processed, definitions)",
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
  expect_match(ui_source, "width: 20rem !important",
               fixed = TRUE)
  expect_match(ui_source, "gap: .65rem", fixed = TRUE)
  expect_match(ui_source, "white-space: nowrap", fixed = TRUE)
  expect_match(ui_source, "background: var(--openspecy-success)",
               fixed = TRUE)
  expect_match(ui_source, "background: #FFFFFF", fixed = TRUE)
  expect_match(ui_source, 'class = "openspecy-summary-column"', fixed = TRUE)
  expect_match(ui_source, '#spectra_box,\n        #analysis_summary_box',
               fixed = TRUE)
  expect_false(grepl("bs4Dash::popover", ui_source, fixed = TRUE))
  expect_false(grepl('data-toggle="popover"', ui_source, fixed = TRUE))
  expect_match(ui_source, "#spectra_box .direct-chat-contacts {",
               fixed = TRUE)
  expect_match(ui_source, "#sidebar_tables {", fixed = TRUE)
  expect_match(ui_source, "#spectra_box #mycardsidebar {", fixed = TRUE)
  expect_match(
    ui_source,
    "#spectra_box.direct-chat-contacts-open #mycardsidebar",
    fixed = TRUE
  )
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
  expect_match(server_source, "automation_status_ui <- function", fixed = TRUE)
  expect_match(server_source, "Problematic spectra:", fixed = TRUE)
  expect_match(
    server_source,
    'outputOptions(output, "range_automation_status", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
  expect_match(
    server_source,
    'outputOptions(output, "co2_automation_status", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
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
  expect_false(grepl("list(DataR(), input$signal_selection)", server_source,
                     fixed = TRUE))
  expect_match(server_source,
               "if(!isTRUE(input$active_quantification))", fixed = TRUE)
  expect_match(server_source, "active_ratio_definitions", fixed = TRUE)
  expect_match(server_source, "isolate(input$quant_ratio_name)",
               fixed = TRUE)
  expect_match(server_source,
               "processed <- DataR()\n    definitions <- active_ratio_definitions()",
               fixed = TRUE)
  expect_match(server_source,
               "defaults <- app_ratio_slider_defaults(\n      processed$wavenumber",
               fixed = TRUE)
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
  expect_identical(spectrum_plot$x$layout$legend$orientation, "h")

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

  area_defaults <- env$app_ratio_slider_defaults(wavenumber, "area")
  expect_equal(area_defaults$min, 900)
  expect_equal(area_defaults$max, 3400)
  expect_equal(area_defaults$step, 1)
  expect_equal(area_defaults$numerator, c(1650, 1850))
  expect_equal(area_defaults$denominator, c(1420, 1500))
  expect_true(all(vapply(area_defaults, function(value) {
    is.integer(value)
  }, logical(1))))

  peak_defaults <- env$app_ratio_slider_defaults(wavenumber, "peak")
  expect_equal(peak_defaults$numerator, 1710)
  expect_equal(peak_defaults$denominator, 1460)
  expect_true(all(vapply(peak_defaults, function(value) {
    is.integer(value)
  }, logical(1))))

  fractional_defaults <- env$app_ratio_slider_defaults(
    seq(900.25, 3400.25, by = 10), "area"
  )
  expect_identical(fractional_defaults$min, 901L)
  expect_identical(fractional_defaults$max, 3400L)
  expect_identical(fractional_defaults$step, 1L)
  expect_true(all(vapply(fractional_defaults, function(value) {
    is.integer(value)
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

  quantified <- env$app_attach_quantification(
    processed, definitions
  )

  expect_s3_class(quantified, "OpenSpecy")
  expect_identical(quantified$spectra, processed$spectra)
  expect_false("quantification_source" %in% names(processed$metadata))
  expect_false("quantification_treatment" %in% names(quantified$metadata))
  expect_identical(
    quantified$metadata$quantification_source,
    rep("displayed_processed_spectra", ncol(processed$spectra))
  )
  expect_identical(
    quantified$metadata$quantification_definitions,
    rep(
      paste0(
        "Custom Carbonyl (area: 1650-1850 / 1420-1500 cm^-1); ",
        "Point Check (peak: 1710 / 1460 cm^-1)"
      ),
      ncol(processed$spectra)
    )
  )
  expect_equal(
    quantified$metadata$area_ratio_custom_carbonyl,
    as.numeric(expected_area)
  )
  expect_equal(
    quantified$metadata$peak_ratio_point_check,
    as.numeric(expected_peak)
  )
  expect_identical(
    env$app_attach_quantification(
      processed, env$app_empty_ratio_definitions()
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
    "active_preprocessing", "make_rel_decision", "smooth_decision",
    "smoother", "derivative_order", "smoother_window", "derivative_abs",
    "conform_decision", "conform_selection", "conform_res",
    "intensity_decision", "intensity_corr", "baseline_decision",
    "baseline_method", "baseline", "refit", "baseline_lambda",
    "baseline_hwi", "iterations", "range_decision", "range_automate",
    "range_artifact_ratio", "MinRange", "MaxRange", "co2_decision",
    "co2_automate", "co2_artifact_ratio", "MinFlat", "MaxFlat",
    "active_identification", "id_spec_type", "id_strategy", "lib_type",
    "filter_lib", "lib_org", "threshold_decision", "MinSNR",
    "signal_selection", "cor_threshold_decision", "MinCor",
    "spatial_decision", "sigma", "xy_grid", "collapse_decision",
    "collapse_type", "collapse_log_type", "active_quantification",
    "quant_ratio_name", "quant_ratio_type", "quant_numerator_area",
    "quant_denominator_area", "quant_numerator_peak",
    "quant_denominator_peak"
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
      "quant_saved_ratio_definitions")
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
