.onboarding_helpers <- NULL

.source_onboarding_helpers <- function() {
  if(!is.null(.onboarding_helpers)) return(.onboarding_helpers)
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
  .onboarding_helpers <<- env
  env
}

test_that("in-place help exposes complete guidance topics", {
  env <- .source_onboarding_helpers()
  expect_true(length(env$app_guidance_registry) > 0L)
  for(topic in names(env$app_guidance_registry)) {
    guidance <- env$app_guidance_topic(topic)
    expect_true(nzchar(guidance$title))
    expect_true(length(guidance$controls) > 0L)
    expect_true(all(nzchar(guidance$body)))
  }
  expect_error(env$app_guidance_topic("unknown"), "Unknown app guidance topic")
})

test_that("tab-wide actions have an all-off-only model", {
  env <- .source_onboarding_helpers()
  ids <- env$app_tab_switch_ids()

  expect_named(ids, c("preprocessing", "identification", "advanced"))
  expect_false(any(c(
    "derivative_abs", "refit", "range_automate", "co2_automate"
  ) %in% ids$preprocessing))
  expect_false("top_n_per_organization" %in% ids$identification)
  expect_true(all(c(
    "make_rel_decision", "smooth_decision", "baseline_decision"
  ) %in% ids$preprocessing))
  expect_true("load_entire_map" %in% ids$advanced)
  for(tab in names(ids)) {
    values <- env$app_tab_all_off_values(tab)
    expect_named(values, ids[[tab]])
    expect_true(all(!values))
  }
})

test_that("identification compatibility warnings cover both library recipes", {
  env <- .source_onboarding_helpers()
  settings <- list(
    identification_active = FALSE, id_strategy = "deriv",
    smooth_decision = FALSE, derivative_order = 0L,
    derivative_abs = FALSE, baseline_decision = FALSE
  )
  expect_length(env$app_identification_compatibility_warnings(settings), 0L)

  settings$identification_active <- TRUE
  derivative_warning <- env$app_identification_compatibility_warnings(settings)
  expect_length(derivative_warning, 1L)
  expect_match(derivative_warning, "Smoothing / Derivative", fixed = TRUE)
  expect_match(derivative_warning, "Derivative Order to 1", fixed = TRUE)
  expect_match(derivative_warning, "Absolute Value", fixed = TRUE)
  expect_match(derivative_warning, "deliberately preprocessed", fixed = TRUE)

  settings$smooth_decision <- TRUE
  settings$derivative_order <- 1L
  settings$derivative_abs <- TRUE
  expect_length(env$app_identification_compatibility_warnings(settings), 0L)

  settings$id_strategy <- "nobaseline"
  no_baseline_warning <- env$app_identification_compatibility_warnings(settings)
  expect_match(no_baseline_warning, "Baseline Correction", fixed = TRUE)
  expect_match(no_baseline_warning, "Derivative Order 0", fixed = TRUE)

  settings$baseline_decision <- TRUE
  settings$smooth_decision <- FALSE
  expect_length(env$app_identification_compatibility_warnings(settings), 0L)

  settings$smooth_decision <- TRUE
  settings$derivative_order <- 0L
  expect_length(env$app_identification_compatibility_warnings(settings), 0L)
})

test_that("initial result selection is bounded", {
  env <- .source_onboarding_helpers()
  path <- OpenSpecy::read_extdata("raman_hdpe.csv")

  object <- OpenSpecy::read_any(path)
  mapping <- data.table::data.table(
    pixel_index = c(8L, 3L), unit_index = c(1L, 1L),
    kept = c(FALSE, TRUE)
  )
  selected <- env$app_initial_result_selection(object, mapping)
  expect_identical(selected, list(plot = 1L, pixel = 3L, table = 1L))
  expect_true(is.na(env$app_initial_result_selection(NULL)$plot))
})

test_that("rank changes remain selectable after one fresh result", {
  env <- .source_onboarding_helpers()
  rows <- data.table::data.table(
    sample_name = c("rank-1", "rank-2"), match_val = c(0.98, 0.91)
  )
  server <- function(input, output, session) {
    run_count <- shiny::reactiveVal(0L)
    result <- shiny::reactiveVal(NULL)
    selected_rank <- shiny::reactiveVal(1L)
    shiny::observeEvent(input$run_analysis, {
      run_count(run_count() + 1L)
      result(rows)
      selected_rank(1L)
    })
    shiny::observeEvent(input$event_rows_selected, {
      selected_rank(env$app_selected_rank_index(
        input$event_rows_selected, nrow(result())
      ))
    }, ignoreInit = TRUE)
    selected_match <- shiny::reactive({
      shiny::req(!is.null(result()))
      result()[selected_rank()]
    })
    session$userData$run_count <- run_count
    session$userData$selected_match <- selected_match
  }

  suppressWarnings(shiny::testServer(server, {
    session$setInputs(run_analysis = 1L)
    expect_identical(session$userData$run_count(), 1L)
    expect_identical(session$userData$selected_match()$sample_name, "rank-1")

    session$setInputs(event_rows_selected = 2L)
    expect_identical(session$userData$selected_match()$sample_name, "rank-2")
    expect_identical(session$userData$run_count(), 1L)
  }))
})

test_that("the real server exposes metadata and rank 2 on its first Run", {
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

  query_path <- OpenSpecy::read_extdata("raman_hdpe.csv")
  query <- OpenSpecy::read_any(query_path)
  reference_spectra <- data.frame(
    `hdpe-reference` = query$spectra[, 1L],
    `alternate-reference` = rev(query$spectra[, 1L]),
    check.names = FALSE
  )
  reference <- OpenSpecy::as_OpenSpecy(
    query$wavenumber, spectra = reference_spectra,
    metadata = data.frame(
      sample_name = c("hdpe-reference", "alternate-reference"),
      material_class = c("polyethylene", "alternate"),
      spectrum_type = "raman", organization = "test",
      stringsAsFactors = FALSE
    )
  )
  env$app_wasm_mode <- function() TRUE
  env$load_app_library <- function(type) reference
  server <- sys.source(file.path(app_path, "server.R"), envir = env)$value

  suppressWarnings(shiny::testServer(server, {
    session$setInputs(
      spike_decision = FALSE, spike_direction = "both",
      spike_residual_threshold = 8, spike_residual_window = 5,
      saturation_decision = FALSE, saturation_mode = "auto",
      saturation_ceiling = 65535, saturation_max_loss = 0.7,
      make_rel_decision = TRUE, smooth_decision = FALSE, smoother = 3,
      derivative_order = 0, smoother_window = 90, derivative_abs = TRUE,
      conform_decision = FALSE, conform_selection = "mean_up", conform_res = 6,
      intensity_decision = FALSE, intensity_corr = "none",
      baseline_decision = FALSE, baseline_method = "polynomial", baseline = 8,
      refit = FALSE, baseline_lambda = 4, baseline_hwi = 50, iterations = 10,
      range_decision = FALSE, range_automate = TRUE,
      range_artifact_ratio = 2, MinRange = 300, MaxRange = 2000,
      co2_decision = FALSE, co2_automate = TRUE, co2_artifact_ratio = 2,
      MinFlat = 2200, MaxFlat = 2420,
      identification_active = TRUE, id_spec_type = "raman",
      id_strategy = "deriv", lib_type = "medoid", top_n_input = 2,
      filter_lib = FALSE, lib_org = "test",
      threshold_decision = FALSE, signal_basis = "raw_smoothed",
      MinSNR = 4, MaxSNR = 1e12, signal_selection = "run_sig_over_noise",
      cor_threshold_decision = FALSE, MinCor = 0.6,
      spatial_decision = FALSE, sigma = 1, xy_grid = FALSE,
      collapse_decision = FALSE, collapse_type = "Mean",
      particle_id_strategy = "collapse", particle_pca_components = 10,
      particle_cluster_k = 10, particle_area_threshold = 1,
      pixel_size = 1, pixel_unit = "pixel", simple_metadata = TRUE,
      show_peak_positions = TRUE, peak_count = 7,
      quant_ratio_type = "area", quant_ratio_name = "",
      quant_numerator_area_min = 1650, quant_numerator_area_max = 1850,
      quant_denominator_area_min = 1420, quant_denominator_area_max = 1500,
      quant_measurement_type = "area", quant_measurement_name = "",
      quant_measurement_area_min = 1650, quant_measurement_area_max = 1850,
      quant_measurement_wavenumber = 1715
    )
    query_info <- data.frame(
      name = basename(query_path),
      size = unname(file.info(query_path)$size),
      type = "text/csv",
      datapath = normalizePath(query_path, winslash = "/", mustWork = TRUE),
      stringsAsFactors = FALSE
    )
    stage_selected_files(query_info, mounted = FALSE)
    session$setInputs(run_analysis = 1L)

    expect_s3_class(canonical_state()$object, "OpenSpecy")
    expect_identical(selected_unit_index(), 1L)
    expect_true(nrow(meta_cache()) == 1L)
    expect_true(nrow(match_metadata()) == 1L)
    expect_identical(data_click$table, 1L)
    expect_true(nrow(top_matches()) >= 2L)

    session$setInputs(event_rows_selected = 2L)
    expect_identical(data_click$table, 2L)
    expect_identical(
      colnames(match_selected()$spectra), matches_to_single()$sample_name[[2L]]
    )
    expect_identical(input$run_analysis, 1L)

    # A second, scientifically identical Run is still a distinct readiness
    # boundary and must restore rank 1 without waiting for a changed result.
    session$setInputs(run_analysis = 2L)
    expect_identical(selection_ready_run(), 2L)
    expect_identical(data_click$table, 1L)
    expect_true(nrow(meta_cache()) == 1L)
    session$setInputs(event_rows_selected = 2L)
    expect_identical(data_click$table, 2L)
    expect_identical(input$run_analysis, 2L)
  }))
})

test_that("file-backed threshold inspection keeps the map bounded and selectable", {
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
  server <- sys.source(file.path(app_path, "server.R"), envir = env)$value
  source_dir <- tempfile("openspecy-file-backed-map-")
  dir.create(source_dir)
  utils::unzip(read_extdata("CA_tiny_map.zip"), exdir = source_dir)
  paths <- list.files(source_dir, full.names = TRUE)
  paths <- paths[grepl("\\.(dat|hdr)$", paths, ignore.case = TRUE)]

  suppressWarnings(shiny::testServer(server, {
    session$setInputs(
      spike_decision = FALSE, spike_direction = "both",
      spike_residual_threshold = 8, spike_residual_window = 5,
      saturation_decision = FALSE, saturation_mode = "auto",
      saturation_ceiling = 65535, saturation_max_loss = 0.7,
      make_rel_decision = FALSE, smooth_decision = FALSE, smoother = 3,
      derivative_order = 1, smoother_window = 90, derivative_abs = TRUE,
      conform_decision = FALSE, conform_selection = "mean_up", conform_res = 6,
      intensity_decision = FALSE, intensity_corr = "none",
      baseline_decision = FALSE, baseline_method = "polynomial", baseline = 8,
      refit = FALSE, baseline_lambda = 4, baseline_hwi = 50, iterations = 10,
      range_decision = FALSE, range_automate = TRUE,
      range_artifact_ratio = 2, MinRange = 300, MaxRange = 2000,
      co2_decision = FALSE, co2_automate = TRUE, co2_artifact_ratio = 2,
      MinFlat = 2200, MaxFlat = 2420,
      identification_active = FALSE, id_spec_type = "all",
      id_strategy = "deriv", lib_type = "medoid", top_n_input = 1,
      top_n_per_organization = TRUE, filter_lib = FALSE,
      threshold_decision = TRUE, signal_basis = "raw_smoothed",
      MinSNR = 0.01, MaxSNR = 1e12,
      signal_selection = "sig_times_noise",
      cor_threshold_decision = FALSE, MinCor = 0.7,
      spatial_decision = FALSE, sigma = 1, xy_grid = FALSE,
      load_entire_map = FALSE,
      collapse_decision = FALSE, collapse_type = "Mean",
      particle_id_strategy = "collapse", particle_pca_components = 10,
      particle_cluster_k = 10, particle_area_threshold = 1,
      pixel_size = 1, pixel_unit = "pixel", simple_metadata = TRUE,
      show_peak_positions = TRUE, peak_count = 7,
      quant_ratio_type = "area", quant_ratio_name = "",
      quant_numerator_area_min = 1650, quant_numerator_area_max = 1850,
      quant_denominator_area_min = 1420, quant_denominator_area_max = 1500,
      quant_measurement_type = "area", quant_measurement_name = "",
      quant_measurement_area_min = 1650, quant_measurement_area_max = 1850,
      quant_measurement_wavenumber = 1715
    )
    info <- data.frame(
      name = basename(paths), size = unname(file.info(paths)$size), type = "",
      datapath = paths, stringsAsFactors = FALSE
    )
    stage_selected_files(info, mounted = FALSE)
    session$setInputs(run_analysis = 1L)

    state <- canonical_state()
    mapping <- data.table::as.data.table(state$pixel_to_unit)
    expect_s3_class(inspection_source_gate(), "FileSpecs")
    expect_true(state$settings$file_backed_selection)
    expect_identical(ncol(state$object$spectra), 1L)
    expect_identical(nrow(mapping), 208L)
    expect_identical(nrow(pixel_projection()$metadata), 208L)
    expect_identical(nrow(meta_cache()), 208L)
    expect_identical(data_click$pixel, env$app_first_retained_pixel(mapping))
    expect_identical(selected_unit_index(), 1L)
    expect_identical(names(match_metadata()), c("Signal Times Noise", "File Name"))

    rejected <- mapping[kept == FALSE, pixel_index][[1L]]
    data_click$pixel <- rejected
    data_click$plot <- NA_integer_
    session$flushReact()
    expect_true(is.na(selected_unit_index()))
    expect_identical(
      attr(active_spectrum_view(), "openspecy_selection_status"),
      "rejected_pixel"
    )
    expect_identical(specs_source_count(inspection_source_gate()), 208L)

    session$setInputs(
      make_rel_decision = TRUE,
      smooth_decision = TRUE, smoother = 3, derivative_order = 1,
      smoother_window = 90, derivative_abs = TRUE,
      conform_decision = TRUE, conform_selection = "mean_up", conform_res = 6,
      identification_active = TRUE, id_spec_type = "all",
      id_strategy = "deriv", lib_type = "medoid", top_n_input = 1,
      top_n_per_organization = FALSE, filter_lib = FALSE,
      threshold_decision = FALSE,
      cor_threshold_decision = TRUE, MinCor = -1,
      collapse_decision = TRUE, collapse_type = "Mean",
      particle_id_strategy = "collapse", particle_area_threshold = 1
    )
    session$setInputs(run_analysis = 2L)
    state <- canonical_state()
    streamed_matches <- data.table::as.data.table(state$pixel_matches)
    streamed_mapping <- data.table::as.data.table(state$pixel_to_unit)
    expect_s3_class(inspection_source_gate(), "FileSpecs")
    expect_false(state$settings$load_entire_map)
    expect_null(state$diagnostic)
    expect_s3_class(state$object, "OpenSpecy")
    expect_identical(nrow(streamed_matches), 208L)
    expect_true(all(streamed_matches[, .N, by = object_id]$N == 1L))
    expect_true(all(c(
      "threshold_match_val", "threshold_match_id", "threshold_material"
    ) %in% names(streamed_mapping)))
    expect_identical(nrow(streamed_mapping), 208L)

    session$setInputs(
      load_entire_map = TRUE,
      identification_active = FALSE, cor_threshold_decision = FALSE,
      collapse_decision = FALSE,
      make_rel_decision = FALSE, smooth_decision = FALSE,
      conform_decision = FALSE
    )
    session$setInputs(run_analysis = 3L)
    expect_s3_class(preprocessed$data, "OpenSpecy")
    expect_false(inherits(preprocessed$data, "Specs"))
    expect_identical(ncol(preprocessed$data$spectra), 208L)
    expect_true(canonical_state()$settings$load_entire_map)
  }))
})

test_that("startup and heatmap event guards are explicit in app sources", {
  app_path <- run_app(test_mode = TRUE)
  server_lines <- readLines(file.path(app_path, "server.R"), warn = FALSE)
  server <- paste(server_lines, collapse = "\n")
  ui <- paste(readLines(file.path(app_path, "ui.R"), warn = FALSE),
              collapse = "\n")
  bridge <- paste(readLines(
    file.path(app_path, "www", "parent-frame.js"), warn = FALSE
  ), collapse = "\n")

  selected_start <- grep(
    "selected_unit_index <- reactive({", server_lines, fixed = TRUE
  )
  selected_end <- grep(
    "active_spectrum_view <- reactive({", server_lines, fixed = TRUE
  )
  expect_length(selected_start, 1L)
  expect_length(selected_end, 1L)
  selected_block <- paste(
    server_lines[selected_start:(selected_end - 1L)], collapse = "\n"
  )
  expect_match(selected_block, "state <- canonical_state()", fixed = TRUE)
  expect_match(selected_block, "object <- state$object", fixed = TRUE)
  expect_match(selected_block, "file_backed_selection", fixed = TRUE)
  expect_false(grepl("ncol(DataR()$spectra)", selected_block, fixed = TRUE))
  expect_match(server, "selection_ready_run", fixed = TRUE)
  expect_match(server, "Every successful Run owns a fresh rank-1", fixed = TRUE)
  expect_match(server, "priority = -10L, ignoreInit = TRUE", fixed = TRUE)
  expect_false(grepl(
    "observeEvent(list(selected_unit_index(), top_matches())", server,
    fixed = TRUE
  ))
  expect_match(server, "last_rank_unit <- reactiveVal(NA_integer_)",
               fixed = TRUE)
  expect_match(server, "observeEvent(selected_unit_index(), {", fixed = TRUE)
  expect_match(server, 'outputOptions(output, "event", suspendWhenHidden = FALSE)',
               fixed = TRUE)
  expect_match(server, "req(isTRUE(heatmap_events_ready()))", fixed = TRUE)
  expect_match(server, "app_has_clickable_heatmap", fixed = TRUE)
  expect_match(server, 'plotly::event_data("plotly_click", source = "heat_plot")',
               fixed = TRUE)

  expect_false(grepl("walkthrough", ui, ignore.case = TRUE))
  expect_false(grepl("walkthrough|openspecy-tutorial", server,
                     ignore.case = TRUE))
  expect_match(server, '"Turn All Off"', fixed = TRUE)
  expect_false(grepl("Turn All On", server, fixed = TRUE))
  expect_match(ui, 'uiOutput("preprocessing_all_toggle_ui")', fixed = TRUE)
  expect_match(server, 'output_id <- paste0(tab, "_all_toggle_ui")',
               fixed = TRUE)
  expect_false(grepl("walkthrough|openspecy-tutorial|tutorialRunGeneration",
                     bridge, ignore.case = TRUE))
  expect_match(bridge, '"openspecy-clear-heatmap-click"', fixed = TRUE)
  expect_match(bridge, '"plotly_click-heat_plot", null', fixed = TRUE)
})

test_that("heatmap events are enabled only for a real multi-spectrum map", {
  env <- .source_onboarding_helpers()

  expect_false(env$app_has_clickable_heatmap(NULL, 2L))
  expect_false(env$app_has_clickable_heatmap(list(type = "empty"), 2L))
  expect_false(env$app_has_clickable_heatmap(list(type = "heatmap"), 1L))
  expect_true(env$app_has_clickable_heatmap(list(type = "heatmap"), 2L))
})
