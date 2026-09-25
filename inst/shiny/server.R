function(input, output, session) {
    
  #Setup ----
    options(shiny.maxRequestSize = app_max_request_size_bytes())
    
    #URL Query
    # observeEvent(session$clientData$url_search, {
    #     query <- parseQueryString(session$clientData$url_search)
    #     
    #     for (i in 1:(length(reactiveValuesToList(input)))) {
    #         nameval = names(reactiveValuesToList(input)[i])
    #         valuetoupdate = query[[nameval]]
    #         
    #         if (!is.null(query[[nameval]])) {
    #             if (is.na(as.numeric(valuetoupdate))) {
    #                 updateTextInput(session, nameval, value = valuetoupdate)
    #             }
    #             else {
    #                 updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
    #             }
    #         }
    #         
    #     }
    #     
    # })

  #create a random session id
  session_id <- digest(runif(10))
  particle_output_root <- file.path(
    tempdir(), paste0("OpenSpecy-shiny-particles-", session_id)
  )

  # Session state
  load_data()

  preprocessed <- reactiveValues(data = NULL)
  upload_status_state <- reactiveVal(NULL)
  active_file_info <- reactiveVal(NULL)
  data_click <- reactiveValues(plot = NULL, pixel = NULL, table = NULL)
  meta_cache <- reactiveVal(NULL)
  correction_diagnostics <- reactiveVal(data.frame())
  ratio_definitions <- reactiveVal(app_empty_ratio_definitions())
  measurement_definitions <- reactiveVal(app_empty_measurement_definitions())
  settings_defaults <- reactiveVal(NULL)
  settings_import_status <- reactiveVal(NULL)
  quantification_axis <- reactiveVal(NULL)
  inspection_source_gate <- reactiveVal(NULL)
  # The reference artifact is committed by Run alongside canonical_state().
  # Downstream plots/tables must never pair a completed medoid result with a
  # newly selected (but not yet run) model library, or vice versa.
  analysis_library <- reactiveVal(NULL)
  heatmap_events_ready <- reactiveVal(FALSE)
  selection_ready_run <- reactiveVal(NULL)
  quality_modal_observers <- new.env(parent = emptyenv())

  identify_batch_size <- reactive({
    value <- suppressWarnings(as.integer(input$identify_batch_size))
    if(length(value) != 1L || is.na(value) || value < 1L) 1000L else value
  })

  # The Run button is the single trigger for the full analysis tranche; it is
  # enabled purely by upload completion (preprocessed$data becoming non-NULL)
  # and is not gated by any other setting.
  observe({
    shinyjs::toggleState("run_analysis", condition = !is.null(active_file_info()))
  })

  # Each Run-gated result below is a reactiveVal cache populated only when
  # Run is clicked, rather than a plain bindEvent()-wrapped reactive, so that
  # a fresh upload can explicitly clear it back to "not yet analyzed" instead
  # of continuing to show the previous dataset's results until the next Run.
  #
  # Every gate below registers its own observeEvent(input$run_analysis, ...).
  # Shiny gives no ordering guarantee between independent observers bound to
  # the same input, but downstream gates (quantified_data_gate and friends)
  # call canonical_final()/DataR(), which reads canonical_state_gate's cache
  # and the reset flag cleared below. `priority` makes that dependency
  # explicit and deterministic instead of racing: higher priority runs
  # first. RUN_GATE_PRIORITY_RESET (highest) clears the reset flag before any
  # gate computes; RUN_GATE_PRIORITY_CANONICAL populates canonical_state
  # before any gate that reads it.
  RUN_GATE_PRIORITY_ANNOUNCE <- 25L
  RUN_GATE_PRIORITY_MATERIALIZE <- 24L
  RUN_GATE_PRIORITY_RESET <- 20L
  RUN_GATE_PRIORITY_CANONICAL <- 10L
  RUN_GATE_PRIORITY_DEFAULT <- 0L
  run_gated_reactive <- function(compute, priority = RUN_GATE_PRIORITY_DEFAULT) {
    cache <- reactiveVal(NULL)
    observeEvent(input$run_analysis, {
      cache(compute())
    }, priority = priority)
    structure(list(read = function() cache(), clear = function() cache(NULL)),
              class = "openspecy_run_gate")
  }

  # Bright green ("dirty") whenever there is a new upload or a settings
  # change the current results don't reflect yet; the app's normal accent
  # color once Run has been clicked for the current upload and settings.
  analysis_dirty <- reactiveVal(FALSE)
  analysis_needs_reset <- reactiveVal(FALSE)
  session$onFlushed(function() {
    defaults <- shiny::isolate(
      stats::setNames(
        lapply(app_user_metadata_input_ids, function(id) input[[id]]),
        app_user_metadata_input_ids
      )
    )
    if(is.null(defaults$visual_overlay)) defaults$visual_overlay <- TRUE
    if(is.null(defaults$overlay_transparency)) {
      defaults$overlay_transparency <- 20
    }
    settings_defaults(defaults)
  }, once = TRUE)
  settings_signature <- reactive({
    analysis_ids <- setdiff(
      app_user_metadata_input_ids, app_live_display_input_ids
    )
    lapply(analysis_ids, function(id) input[[id]])
  })
  observeEvent(settings_signature(), {
    analysis_dirty(TRUE)
  }, ignoreInit = TRUE)
  # Sends the first busy-overlay signal immediately on click, before any
  # other Run-triggered observer (including the reset below and every
  # run_gated_reactive()) runs -- otherwise the overlay has nothing to react
  # to until whichever gate happens to reach its own first analysis_phase()
  # call, which can be seconds away (e.g. recalculate_snr_preview()'s
  # whole-map scan has none at all). Message-only: reads nothing, writes
  # nothing any other observer depends on, so it cannot affect gate ordering.
  observeEvent(input$run_analysis, {
    analysis_phase("Starting analysis", "Preparing to run.", 1)
  }, priority = RUN_GATE_PRIORITY_ANNOUNCE)

  observeEvent(input$run_analysis, {
    analysis_dirty(FALSE)
    analysis_needs_reset(FALSE)
  }, priority = RUN_GATE_PRIORITY_RESET)
  observe({
    shinyjs::toggleClass(
      "run_analysis", "openspecy-run-dirty", condition = isTRUE(analysis_dirty())
    )
  })

  # The tab-wide action is intentionally one-way. "All on" combines mutually
  # unsuitable scientific choices, so the safe convenience is always Reset
  # all switches to off and users then opt into the few steps they need.
  tab_switch_ids <- app_tab_switch_ids()
  app_render_tab_all_toggle <- function(tab) {
    actionButton(
      paste0(tab, "_all_toggle"),
      "Turn All Off",
      icon = icon("toggle-off"),
      class = "btn-sm openspecy-tab-all-toggle",
      title = "Turn off every switch in this tab."
    )
  }
  lapply(names(tab_switch_ids), function(tab) {
    output_id <- paste0(tab, "_all_toggle_ui")
    output[[output_id]] <- renderUI(
      app_render_tab_all_toggle(tab)
    )
    outputOptions(output, output_id, suspendWhenHidden = FALSE)
    observeEvent(input[[paste0(tab, "_all_toggle")]], {
      for(id in tab_switch_ids[[tab]]) {
        shinyWidgets::updatePrettySwitch(session, id, value = FALSE)
      }
    }, ignoreInit = TRUE)
  })

  output$quantification_all_toggle_ui <- renderUI(
    actionButton(
      "quantification_remove_all", "Remove All",
      icon = icon("trash"), class = "btn-sm openspecy-tab-all-toggle",
      title = "Remove every saved ratio and measurement."
    )
  )
  outputOptions(
    output, "quantification_all_toggle_ui", suspendWhenHidden = FALSE
  )

  observeEvent(input$quantification_remove_all, {
    ratio_definitions(app_empty_ratio_definitions())
    measurement_definitions(app_empty_measurement_definitions())
    analysis_dirty(TRUE)
  }, ignoreInit = TRUE)

  observe({
    values <- stats::setNames(
      lapply(unique(unlist(tab_switch_ids, use.names = FALSE)), function(id) {
        input[[id]]
      }),
      unique(unlist(tab_switch_ids, use.names = FALSE))
    )
    states <- app_tab_active_states(
      values, ratio_definitions(), measurement_definitions()
    )
    session$sendCustomMessage(
      "openspecy-tab-active-state",
      as.list(states)
    )
  })

  observe({
    active <- isTRUE(input$identification_active)
    shinyjs::toggleState("id_spec_type", condition = active)
    shinyjs::toggleState("id_strategy", condition = active)
    shinyjs::toggleState("lib_type", condition = active)
    shinyjs::toggleState("top_n_input", condition = active)
    shinyjs::toggleState(
      "top_n_per_organization",
      condition = active && !identical(input$lib_type, "model")
    )
  })

  observeEvent(input$range_automate, {
    manual_range <- !isTRUE(input$range_automate)
    shinyjs::toggleState("MinRange", condition = manual_range)
    shinyjs::toggleState("MaxRange", condition = manual_range)
    shinyjs::toggleClass(
      "manual_range_bounds", "openspecy-inputs-disabled",
      condition = !manual_range
    )
  }, ignoreInit = FALSE)

  analysis_phase <- function(message, detail, progress = 4) {
    progress <- max(0, min(100, as.numeric(progress)[[1L]]))
    session$sendCustomMessage(
      "openspecy-analysis-phase",
      list(message = message, detail = detail, progress = progress)
    )
  }

  set_upload_status <- function(message = NULL, type = "message") {
    upload_status_state(message)
    if(app_wasm_mode() && !is.null(message)) {
      session$sendCustomMessage(
        "openspecy-upload-status",
        list(message = as.character(message), type = type)
      )
    }
  }

  session$onSessionEnded(function() {
    preprocessed$data <- NULL
    if(dir.exists(particle_output_root)) {
      unlink(particle_output_root, recursive = TRUE, force = TRUE)
    }
  })

  observeEvent(input$support_openspecy, {
    donation_links <- c(
      "$25" = "https://www.paypal.com/donate/?hosted_button_id=F2CAABAZ6JQTJ",
      "$50" = "https://www.paypal.com/donate/?hosted_button_id=MW8NUFBH7JX2W",
      "$75" = "https://www.paypal.com/donate/?hosted_button_id=M59EWJTJWHZBA",
      "$100" = "https://www.paypal.com/donate/?hosted_button_id=WZPE5LCF4FSNE",
      "$1,000" = "https://www.paypal.com/donate/?hosted_button_id=MCZ2D4TQGYVKC",
      "Other" = "https://www.paypal.com/donate/?hosted_button_id=PZHG44PX5C89C"
    )
    showModal(modalDialog(
      title = tagList(icon("donate"), "Help Support Us!"),
      easyClose = TRUE,
      size = "l",
      tags$p("Thanks to users like you, Open Specy remains free and open."),
      tags$p(
        "Open Specy is a free and open-source platform dedicated to advancing ",
        "spectroscopy and microplastic research. Maintaining it takes time and ",
        "resources; donations support continued development and access."
      ),
      tags$h4("Donate Today!"),
      tags$div(
        class = "openspecy-donation-options",
        lapply(names(donation_links), function(amount) {
          tags$a(
            icon("paypal"), amount,
            href = unname(donation_links[[amount]]),
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-primary openspecy-donation-link"
          )
        })
      ),
      footer = modalButton("Close")
    ))
  })


  #Read Data ----
# Local native/shinyFiles direct paths and hosted WORKERFS mounts enter this
# function as the same four-column file table and converge before read_any().
read_uploaded_files <- function(file_info, mounted = FALSE) {
  started <- proc.time()[["elapsed"]]
  data_click$plot <- 1
  data_click$pixel <- 1
  data_click$table <- 1
  preprocessed$data <- NULL
  inspection_source_gate(NULL)
  heatmap_events_ready(FALSE)
  session$sendCustomMessage("openspecy-heatmap-pending", list())
  session$sendCustomMessage("openspecy-clear-heatmap-click", list())
  attr(file_info, "mounted") <- isTRUE(mounted)
  active_file_info(file_info)
  set_upload_status(NULL)
  meta_cache(NULL)
  correction_diagnostics(data.frame())
  quantification_axis(NULL)

  reset_upload_control <- function() {
    if(isTRUE(mounted)) {
      session$sendCustomMessage("openspecy-mounted-reset", list())
    } else {
      active_file_info(NULL)
    }
  }

  upload_size <- app_validate_upload_size(file_info)
  if(!isTRUE(upload_size$ok)) {
    set_upload_status(upload_size$message, "error")
    reset_upload_control()
    active_file_info(NULL)
    return(NULL)
  }

  if (!all(grepl("(\\.tsv$)|(\\.h5$)|(\\.txt$)|(\\.img$)|(\\.dat$)|(\\.hdr$)|(\\.jpg$)|(\\.jpeg$)|(\\.png$)|(\\.json$)|(\\.rds$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.dx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = TRUE, as.character(file_info$name)))) {
    set_upload_status(paste(
      "Uploaded data type is not supported. Check the upload guidance for",
      "the accepted file extensions."
    ), "error")
    reset_upload_control()
    active_file_info(NULL)
    return(NULL)
  }

  load_entire_map <- isTRUE(input$load_entire_map)
  analysis_phase(
    if(load_entire_map) "Reading entire map into memory" else
      "Reading spectra",
    paste0(
      "Reading and validating ", nrow(file_info), " ",
      if(isTRUE(mounted)) "browser-mounted" else "uploaded", " file",
      if(nrow(file_info) == 1L) "." else "s.",
      if(load_entire_map) {
        " Full-memory mode is enabled."
      } else {
        " Supported maps remain file-backed."
      }
    ),
    8
  )
      
      rout <- tryCatch(expr = {
          serialized_rds <- nrow(file_info) == 1L &&
            grepl("\\.rds$", file_info$name[[1L]], ignore.case = TRUE)
          # Serialized OpenSpecy objects are already materialized. Their S/N
          # is calculated by the canonical analysis pipeline after import, so
          # constructing a compact-reader policy here is both unnecessary and
          # made particle-RDS round trips depend on the installed reader API.
          background_policy <- if(isTRUE(input$threshold_decision) &&
                                     !serialized_rds) {
            specs_background_filter(
              metric = effective_signal_selection(), minimum = MinSNR(),
              maximum = MaxSNR(),
              sigma = if(isTRUE(input$spatial_decision)) {
                rep(as.numeric(input$sigma), 3L)
              } else NULL,
              step = 10,
              intensity_type = if(isTRUE(input$intensity_decision)) {
                input$intensity_corr
              } else NULL
            )
          } else NULL
          reader_background_policy <- if(
            identical(input$signal_basis, "fully_processed")
          ) NULL else background_policy
          # RDS maps are already serialized OpenSpecy objects. Reading a lone
          # RDS directly avoids dispatch and, critically for gigabyte maps,
          # avoids hashing/copying the full spectra matrix merely to add a
          # provenance ID. Existing IDs in the serialized object are retained.
          members <- if(serialized_rds) {
            serialized <- readRDS(as.character(file_info$datapath[[1L]]))
            if(is_Specs(serialized)) {
              check_Specs(serialized)
              serialized
            } else {
              app_restore_spatial_coordinates(
                as_OpenSpecy(serialized, compute_file_id = FALSE)
              )
            }
          } else {
            app_read_uploaded_members(
              paths = file_info$datapath, mounted = mounted,
              representation = if(load_entire_map) "OpenSpecy" else "Specs",
              background_filter = reader_background_policy,
              spectral_smooth = isTRUE(input$spatial_decision),
              sigma = rep(as.numeric(input$sigma), 3L)
            )
          }
          combined <- if(is_OpenSpecy(members) || is_Specs(members)) {
            members
          } else {
            c_spec(
              members, range = "common",
              res = if(input$conform_decision) input$conform_res else 8
            )
          }
          if(load_entire_map && is_Specs(combined)) {
            analysis_phase(
              "Materializing the complete map",
              paste(
                "Reading every spectrum into the ordinary in-memory workflow",
                "as explicitly requested."
              ),
              12
            )
            combined <- if(inherits(combined, "FileSpecs")) {
              decompress_spec(
                combined,
                index = seq_len(OpenSpecy:::.filespec_n_spectra(combined))
              )
            } else {
              decompress_spec(combined, expand = TRUE)
            }
          }
          if(is_Specs(combined) && !inherits(combined, "FileSpecs") &&
             !is.null(background_policy) &&
             identical(input$signal_basis, "fully_processed")) {
            analysis_phase(
              "Classifying fully processed signal/noise",
              paste(
                "Applying the committed preprocessing to active map spectra",
                "before assigning the background sentinel."
              ),
              13
            )
            classification_basis <- ordinary_process(
              combined,
              settings = app_snr_processing_settings(
                current_processing_settings()
              ),
              view_only = TRUE
            )
            classification_snr <- sig_noise(
              classification_basis, metric = background_policy$metric,
              step = background_policy$step, spatial_smooth = FALSE,
              abs = FALSE
            )
            combined <- OpenSpecy:::.apply_specs_background_result(
              combined, background_policy, classification_snr,
              basis = "fully_processed"
            )
          }
          if(is_Specs(combined)) combined else
            manage_na(combined, ig = c(NA, 0), type = "remove")},
          error = function(e){
              class(e$message) <- "simpleError"
              e$message
          }#,
          #warning = function(w){
          #class(w$message) <- "simpleWarning"
          #    w$message
          #}
      )
      #print(rout)
      
      if(!inherits(rout, "simpleError") && is_OpenSpecy(rout) &&
         all(!grepl("(\\.hdr$)|(\\.dat$)|(\\.zip$)", file_info$name))){
          rout$metadata$file_name <- file_info$name
      }
      
      if(!inherits(rout, "simpleError")){
          checkit <- tryCatch(expr = {
            if(is_Specs(rout)) check_Specs(rout) else check_OpenSpecy(rout)
          },
                              error = function(e){
                                  class(e$message) <- "simpleError"
                                  e$message
                              },
                              warning = function(w){
                                  class(w$message) <- "simpleWarning"
                                  w$message
                              })          
      }
      else{
          checkit <- NA
      }
      
    #print(checkit)
    if (inherits(rout, "simpleError") || inherits(checkit, "simpleError")) {
      elapsed <- proc.time()[["elapsed"]] - started
      failure_detail <- paste0(
        if(inherits(rout, "simpleError")) {
          paste0("Data loading reported: ", rout, ".")
        } else "",
        if(inherits(checkit, "simpleError")) {
          paste0(" Data checking reported: ", checkit, ".")
        } else ""
      )
      failure_message <- paste(
        failure_detail, app_upload_failure_guidance(elapsed, mounted)
      )
      show_alert(
        title = "Something went wrong with reading the data :-(",
        text = paste0(
          failure_detail,
          " If you uploaded a text/csv file, make sure that the columns are ",
          "numeric and named 'wavenumber' and 'intensity'. ",
          app_upload_failure_guidance(elapsed, mounted)
        ),
        type =  "error"
      )
      set_upload_status(failure_message, "error")
      reset_upload_control()
      active_file_info(NULL)
      preprocessed$data <- NULL
    }
    else if(inherits(checkit, "simpleWarning")) {
      set_upload_status(paste(
        "The uploaded spectra need attention:", as.character(checkit)
      ), "warning")
      reset_upload_control()
      active_file_info(NULL)
      preprocessed$data <- NULL
    }
      
    else {
        analysis_phase(
          "Preparing uploaded spectra",
          "Checking spectral structure and preparing the shared wavenumber axis.",
          15
        )
        # A newly uploaded dataset invalidates every previous Run's results;
        # clear them before publishing preprocessed$data. Publishing first let
        # the heatmap observer briefly reveal the prior/empty plot during the
        # same reactive flush, which produced the first-upload blink.
        analysis_dirty(TRUE)
        analysis_needs_reset(TRUE)
        canonical_state_gate$clear()
        analysis_library(NULL)
        quantified_data_gate$clear()
        automatic_report_gate$clear()
        ai_output_gate$clear()
        pixel_projection_gate$clear()
        preprocessed$data <- rout
        spatial_unit <- attr(rout, "openspecy_spatial_unit", exact = TRUE)
        if(isTruthy(spatial_unit)) {
          updateNumericInput(session, "pixel_size", value = 1)
          updateTextInput(session, "pixel_unit", value = spatial_unit)
        }
        set_upload_status(NULL)
        session$sendCustomMessage(
          "openspecy-upload-materialized",
          list(
            transport = if(isTRUE(mounted)) "workerfs" else "native",
            files = as.character(file_info$name)
          )
        )
        #print(preprocessed$data)
    }
}

stage_selected_files <- function(file_info, mounted = FALSE) {
  attr(file_info, "mounted") <- isTRUE(mounted)
  active_file_info(file_info)
  preprocessed$data <- NULL
  inspection_source_gate(NULL)
  data_click$plot <- NULL
  data_click$pixel <- NULL
  data_click$table <- NULL
  meta_cache(NULL)
  correction_diagnostics(data.frame())
  snr_preview(NULL)
  snr_preview_signature(NULL)
  heatmap_events_ready(FALSE)
  session$sendCustomMessage("openspecy-heatmap-pending", list())
  session$sendCustomMessage("openspecy-clear-heatmap-click", list())
  quantification_axis(NULL)
  set_upload_status(paste0(
    nrow(file_info), " file", if(nrow(file_info) == 1L) "" else "s",
    " selected. Click Run to read and analyze."
  ))
  analysis_dirty(TRUE)
  analysis_needs_reset(TRUE)
  canonical_state_gate$clear()
  analysis_library(NULL)
  quantified_data_gate$clear()
  automatic_report_gate$clear()
  ai_output_gate$clear()
  pixel_projection_gate$clear()
  if(isTRUE(mounted) && app_wasm_mode()) {
    session$sendCustomMessage(
      "openspecy-run-ready", list(enabled = TRUE)
    )
  }
}

if(!app_wasm_mode()) {
  local_roots <- app_local_roots()
  app_shiny_files("shinyFileChoose")(
    input, "local_files", roots = local_roots, session = session,
    filetypes = app_local_file_extensions()
  )
  observeEvent(input$local_files, {
    parsed <- app_shiny_files("parseFilePaths")(local_roots, input$local_files)
    file_info <- tryCatch(app_local_file_info(parsed, local_roots),
                          error = identity)
    if(inherits(file_info, "error")) {
      set_upload_status(conditionMessage(file_info), "error")
      return(NULL)
    }
    stage_selected_files(file_info, mounted = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$local_native_files, {
    paths <- tryCatch(app_choose_local_paths(), error = identity)
    if(inherits(paths, "error")) {
      set_upload_status(conditionMessage(paths), "error")
      return(NULL)
    }
    if(!length(paths)) return(NULL)
    file_info <- tryCatch(app_direct_file_info(paths), error = identity)
    if(inherits(file_info, "error")) {
      set_upload_status(conditionMessage(file_info), "error")
      return(NULL)
    }
    stage_selected_files(file_info, mounted = FALSE)
  }, ignoreInit = TRUE)
}

observeEvent(input$mounted_files, {
  file_info <- tryCatch(
    app_mounted_file_info(input$mounted_files), error = identity
  )
  if(inherits(file_info, "error")) {
    set_upload_status(
      paste("Mounted file metadata was rejected:", conditionMessage(file_info)),
      "error"
    )
    session$sendCustomMessage("openspecy-mounted-reset", list())
    return(NULL)
  }
  stage_selected_files(file_info, mounted = TRUE)
}, ignoreInit = TRUE)

update_imported_setting <- function(id, value) {
  if(id %in% app_logical_setting_ids) {
    shinyWidgets::updatePrettySwitch(session, id, value = isTRUE(value))
  } else if(id %in% c("id_spec_type", "id_strategy", "lib_type", "lib_org",
                      "collapse_type", "particle_id_strategy")) {
    shinyWidgets::updatePickerInput(session, id, selected = value)
  } else if(id %in% c("intensity_corr", "quant_ratio_type",
                      "quant_measurement_type")) {
    updateRadioButtons(session, id, selected = value)
  } else if(id %in% c("smoother", "derivative_order", "smoother_window",
                      "conform_res", "baseline", "baseline_lambda",
                      "iterations", "saturation_max_loss", "peak_count",
                      "overlay_transparency")) {
    updateSliderInput(session, id, value = value)
  } else if(id %in% c("pixel_unit", "quant_ratio_name",
                      "quant_measurement_name")) {
    updateTextInput(session, id, value = value)
  } else if(id %in% app_numeric_setting_ids) {
    updateNumericInput(session, id, value = value)
  } else {
    updateSelectInput(session, id, selected = value)
  }
}

output$settings_import_status <- renderUI({
  status <- settings_import_status()
  if(is.null(status)) return(NULL)
  tags$p(class = if(isTRUE(status$ok)) "text-success" else "text-danger",
         status$message)
})

observeEvent(input$settings_csv, {
  upload <- input$settings_csv
  req(!is.null(upload), nrow(upload) == 1L)
  parsed <- tryCatch({
    snapshot <- data.table::fread(
      upload$datapath[[1L]], data.table = FALSE, check.names = FALSE,
      na.strings = c("NA", "")
    )
    app_user_metadata_import(snapshot, settings_defaults())
  }, error = identity)
  if(inherits(parsed, "error")) {
    settings_import_status(list(ok = FALSE, message = conditionMessage(parsed)))
    return(NULL)
  }
  for(id in app_user_metadata_input_ids) {
    update_imported_setting(id, parsed$settings[[id]])
  }
  ratio_definitions(parsed$ratios)
  measurement_definitions(parsed$measurements)
  analysis_dirty(TRUE)
  analysis_needs_reset(TRUE)
  canonical_state_gate$clear()
  quantified_data_gate$clear()
  automatic_report_gate$clear()
  ai_output_gate$clear()
  pixel_projection_gate$clear()
  warning_text <- if(length(parsed$unknown)) {
    paste0(" Unknown columns ignored: ", paste(parsed$unknown, collapse = ", "), ".")
  } else ""
  settings_import_status(list(
    ok = TRUE,
    message = paste0("Settings restored. Click Run to apply them.", warning_text)
  ))
}, ignoreInit = TRUE)

observeEvent(input$run_analysis, {
  files <- active_file_info()
  req(!is.null(files))
  read_uploaded_files(files, mounted = isTRUE(attr(files, "mounted")))
}, priority = RUN_GATE_PRIORITY_MATERIALIZE)

  output$upload_status <- renderUI({
    message <- upload_status_state()
    if(is.null(message)) return(NULL)
    tags$span(message)
  })
  outputOptions(output, "upload_status", suspendWhenHidden = FALSE)
  
  # Load the selected library independently of the processed data. Keeping this
  # expensive read in its own reactive prevents every preprocessing change from
  # re-reading the 42 MB full library.
  library_source <- reactive({
      analysis_phase(
        "Loading the reference library",
        paste0(
          "Loading the selected ", input$lib_type,
          " library. The first use can take longer if it must be downloaded."
        ),
        52
      )
      artifact_name <- if(input$lib_type == "medoid") {
        paste0("medoid_", if(input$id_strategy == "deriv") {
          "derivative"
        } else "nobaseline")
      } else if(input$lib_type == "model") {
        paste0("model_", if(input$id_strategy == "deriv") {
          "derivative"
        } else "nobaseline")
      } else if(grepl("nobaseline$", input$id_strategy)) {
        "nobaseline"
      } else "derivative"
      app_select_library_spectrum_type(
        load_app_library(artifact_name), input$id_spec_type
      )
  })

  #The matching library to use.
  libraryR <- reactive({
      req(!is.null(preprocessed$data))
      library <- library_source()
      if(identical(input$lib_type, "model")) return(library)

      if(grepl("^ftir", input$id_spec_type)) {
        library <- filter_spec(
          library, logic = library$metadata$spectrum_type == "ftir"
        )
      } else if(grepl("^raman", input$id_spec_type)) {
        library <- filter_spec(
          library, logic = library$metadata$spectrum_type == "raman"
        )
      } else if(grepl("^nir", input$id_spec_type)) {
        library <- filter_spec(
          library, logic = library$metadata$spectrum_type == "nir"
        )
      }
      library
  })

  observeEvent(libraryR(), {
      if(identical(input$lib_type, "model")) return()
      orgs <- sort(unique(libraryR()$metadata$organization))
      updatePickerInput(session, "lib_org", choices = orgs,
                        selected = orgs)
  })
  

  library_filtered <- reactive({
      library <- libraryR()
      library_type <- input$lib_type
      filter_enabled <- !identical(library_type, "model") &&
        isTRUE(input$filter_lib)
      if(!filter_enabled || !length(input$lib_org)) return(library)

      filter_spec(
        library,
        logic = library$metadata$organization %in% input$lib_org
      )
  })
  # Corrects spectral intensity units using the user specified correction

 data <- reactive({
    req(!is.null(preprocessed$data))
      da <- preprocessed$data
      if(is_Specs(da)) {
        if(isTruthy(input$xy_grid)) {
          md <- specs_metadata(da)
          if(!all(diff(sort(md$y)) %in% c(0, 1)) ||
             !all(diff(sort(md$x)) %in% c(0, 1))) {
            grid <- gen_grid(nrow(md))
            coords <- specs_coordinates(da)
            coords$x <- grid$x
            coords$y <- grid$y
            da$coords <- coords
            attr(da, "source_metadata") <- OpenSpecy:::.encode_specs_metadata(md)
          }
        }
        return(da)
      }
      if(isTruthy(input$xy_grid) &&
         (!all(diff(sort(da$metadata$y)) %in% c(0,1)) ||
          !all(diff(sort(da$metadata$x)) %in% c(0,1)))){
          grid <- gen_grid(nrow(da$metadata))
          da$metadata$x <- grid$x
          da$metadata$y <- grid$y
      }
          da
    })

  source_count <- function(x) {
    if(is_Specs(x)) specs_source_count(x) else ncol(x$spectra)
  }
  source_metadata <- function(x) {
    if(is_Specs(x)) specs_metadata(x) else data.table::as.data.table(x$metadata)
  }

  # Preprocess ----
  ordinary_processing_input_ids <- c(
    "spike_decision", "spike_direction", "spike_residual_threshold",
    "spike_residual_window", "saturation_decision", "saturation_mode",
    "saturation_ceiling", "saturation_max_loss", "intensity_decision",
    "intensity_corr", "conform_decision", "conform_selection", "conform_res",
    "baseline_decision", "baseline_method", "baseline_lambda", "baseline_hwi",
    "iterations", "baseline", "refit", "smooth_decision", "smoother",
    "smoother_window", "derivative_order", "derivative_abs",
    "make_rel_decision", "co2_decision", "co2_automate",
    "co2_artifact_ratio", "MinFlat", "MaxFlat", "range_decision",
    "range_automate", "range_artifact_ratio", "MinRange", "MaxRange"
  )
  current_processing_settings <- function() {
    stats::setNames(
      lapply(ordinary_processing_input_ids, function(id) input[[id]]),
      ordinary_processing_input_ids
    )
  }

  # Compatibility advice is deliberately captured and shown only on Run. It
  # is nonblocking because advanced users may upload spectra that were already
  # transformed before entering Open Specy.
  observeEvent(input$run_analysis, {
    processing <- current_processing_settings()
    snapshot <- c(
      list(
        identification_active = isTRUE(input$identification_active),
        id_strategy = input$id_strategy
      ),
      processing[c(
        "smooth_decision", "derivative_order", "derivative_abs",
        "baseline_decision"
      )]
    )
    messages <- app_identification_compatibility_warnings(snapshot)
    lapply(messages, function(message) {
      showNotification(
        message, type = "warning", duration = 12, closeButton = TRUE
      )
    })
  }, priority = RUN_GATE_PRIORITY_MATERIALIZE - 1L)

  # Ordinary spectral processing is a pure operation over its input. Spatial
  # smoothing is deliberately kept outside this function so S/N and particle
  # partitioning always use the same spatial-only spectra.
  ordinary_process <- function(uploaded, settings = NULL, view_only = FALSE) {
    value <- function(id) {
      if(is.null(settings)) input[[id]] else settings[[id]]
    }
    report_phase <- function(...) {
      if(!isTRUE(view_only)) analysis_phase(...)
    }
    if(inherits(uploaded, "FileSpecs")) {
      stop(paste(
        "File-backed maps must be collapsed to retained particle means before",
        "ordinary spectral processing."
      ), call. = FALSE)
    }
    if(is_Specs(uploaded)) uploaded <- decompress_spec(uploaded, expand = FALSE)
    processed <- uploaded

    {
      spike_enabled <- isTRUE(value("spike_decision"))
      spike_args <- if(spike_enabled) {
        list(
          method = "residual",
          direction = if(is.null(value("spike_direction"))) {
            "both"
          } else value("spike_direction"),
          residual_threshold = if(is.null(value("spike_residual_threshold"))) {
            8
          } else value("spike_residual_threshold"),
          residual_window = if(is.null(value("spike_residual_window"))) {
            5L
          } else as.integer(value("spike_residual_window"))
        )
      } else {
        list()
      }
      saturation_enabled <- isTRUE(value("saturation_decision"))
      saturation <- if(saturation_enabled) {
        saturation_mode <- if(is.null(value("saturation_mode"))) {
          "auto"
        } else value("saturation_mode")
        ceiling <- if(identical(saturation_mode, "threshold")) {
          value("saturation_ceiling")
        } else {
          NULL
        }
        app_saturation_value(saturation_mode, ceiling)
      } else {
        NULL
      }
      saturation_args <- if(saturation_enabled) {
        list(
          max_saturation_loss = if(is.null(value("saturation_max_loss"))) {
            0.7
          } else value("saturation_max_loss")
        )
      } else {
        list()
      }
      if(spike_enabled || saturation_enabled) {
        correction_steps <- c(
          if(spike_enabled) "checking isolated spikes",
          if(saturation_enabled) "checking shared saturated ranges"
        )
        report_phase(
          "Correcting acquisition artifacts",
          paste0(
            paste(correction_steps, collapse = " and "),
            " before ordinary preprocessing."
          ),
          20
        )
        processed <- app_apply_spectral_corrections(
          processed,
          spike = spike_enabled,
          spike_args = spike_args,
          saturation = saturation,
          saturation_args = saturation_args
        )
      }
      corrected_source <- processed
      report_phase(
        "Preprocessing spectra",
        paste0(
          "Applying the selected preprocessing steps to ",
          format(ncol(uploaded$spectra), big.mark = ","), " spectrum",
          if(ncol(uploaded$spectra) == 1L) "." else "s."
        ),
        26
      )
      intensity_enabled <- isTRUE(value("intensity_decision"))
      intensity_args <- if(intensity_enabled) {
        list(type = value("intensity_corr"))
      } else {
        list()
      }

      preserve_uploaded_axis <- app_conform_preserve_axis(
        processed, value("conform_decision"), value("conform_selection"),
        value("conform_res")
      )
      conform_enabled <- isTRUE(value("conform_decision")) &&
        !preserve_uploaded_axis
      conform_args <- if(conform_enabled) {
        list(
          range = app_conform_axis(processed, value("conform_res")),
          res = NULL,
          # Mean Up only reaches this branch when the target resolution is
          # finer than the upload's native resolution, which calls for
          # interpolation (mean_up itself can only aggregate down).
          type = if(identical(value("conform_selection"), "mean_up")) "interp" else
            value("conform_selection")
        )
      } else {
        list()
      }

      baseline_enabled <- isTRUE(value("baseline_decision"))
      baseline_args <- if(baseline_enabled) {
        if(identical(value("baseline_method"), "fill_peaks")) {
          list(
            type = "fill_peaks",
            lambda = value("baseline_lambda"),
            hwi = value("baseline_hwi"),
            it = value("iterations"),
            make_rel = FALSE
          )
        } else {
          list(
            type = "polynomial",
            degree = value("baseline"),
            raw = FALSE,
            refit_at_end = value("refit"),
            iterations = value("iterations"),
            baseline = NULL,
            make_rel = FALSE
          )
        }
      } else {
        list()
      }

      smooth_enabled <- isTRUE(value("smooth_decision"))
      smooth_args <- if(smooth_enabled) {
        smoothing_axis <- if(conform_enabled) {
          conform_args$range
        } else {
          processed$wavenumber
        }
        list(
          polynomial = value("smoother"),
          window = calc_window_points(smoothing_axis, value("smoother_window")),
          derivative = value("derivative_order"),
          abs = value("derivative_abs")
        )
      } else {
        list()
      }

      processed <- process_spec(
        x = processed,
        active = TRUE,
        adj_intens = intensity_enabled,
        adj_intens_args = intensity_args,
        conform_spec = conform_enabled,
        conform_spec_args = conform_args,
        restrict_range = FALSE,
        flatten_range = FALSE,
        subtr_baseline = baseline_enabled,
        subtr_baseline_args = baseline_args,
        smooth_intens = smooth_enabled,
        smooth_intens_args = smooth_args,
        make_rel = value("make_rel_decision")
      )
      processed <- app_copy_correction_history(corrected_source, processed)
    }

    diagnostics <- list()
    if(isTRUE(value("co2_decision"))) {
      if(isTRUE(value("co2_automate"))) {
        co2_artifact_ratio <- value("co2_artifact_ratio")
        if(is.null(co2_artifact_ratio)) co2_artifact_ratio <- 2
        report_phase(
          "Checking the CO2 region",
          "Testing the processed spectra and keeping flattening only if more spectra pass.",
          38
        )
        result <- app_apply_range_automation(
          processed,
          flatten = TRUE,
          restrict = FALSE,
          # These bounds define both the assessed CO2 region and the region
          # flattened by an accepted automatic correction.
          flatten_args = list(
            min = value("MinFlat"),
            max = value("MaxFlat"),
            artifact_ratio = co2_artifact_ratio
          )
        )
        processed <- result$data
        diagnostics[[length(diagnostics) + 1L]] <-
          result$diagnostics[result$diagnostics$enabled, , drop = FALSE]
      } else {
        processed <- flatten_range(
          processed,
          min = value("MinFlat"),
          max = value("MaxFlat"),
          make_rel = FALSE
        )
      }
    }

    if(isTRUE(value("range_decision"))) {
      if(isTRUE(value("range_automate"))) {
        range_artifact_ratio <- value("range_artifact_ratio")
        if(is.null(range_artifact_ratio)) range_artifact_ratio <- 2
        report_phase(
          "Checking spectral tails",
          "Testing the processed batch and keeping shared-axis cropping only if more spectra pass.",
          43
        )
        result <- app_apply_range_automation(
          processed,
          flatten = FALSE,
          restrict = TRUE,
          restrict_args = list(artifact_ratio = range_artifact_ratio)
        )
        processed <- result$data
        high_tail_accepted <- any(
          result$diagnostics$check == "high_tail" &
            result$diagnostics$accepted
        )
        if(isTRUE(high_tail_accepted)) {
          accepted_bounds <- range(processed$wavenumber, na.rm = TRUE)
          if(!isTRUE(view_only)) updateNumericInput(
            session, "MinRange", value = accepted_bounds[[1L]]
          )
          if(!isTRUE(view_only)) updateNumericInput(
            session, "MaxRange", value = accepted_bounds[[2L]]
          )
        }
        diagnostics[[length(diagnostics) + 1L]] <-
          result$diagnostics[result$diagnostics$enabled, , drop = FALSE]
      } else {
        processed <- restrict_range(
          processed,
          min = value("MinRange"),
          max = value("MaxRange"),
          make_rel = FALSE
        )
      }
    }

    diagnostics <- if(length(diagnostics)) {
      do.call(rbind, diagnostics)
    } else {
      data.frame()
    }
    if(!isTRUE(view_only)) correction_diagnostics(diagnostics)
    if(nrow(diagnostics)) {
      accepted <- sum(diagnostics$accepted)
      skipped <- sum(diagnostics$reason == "no_failures")
      rejected <- nrow(diagnostics) - accepted - skipped
      report_phase(
        "Artifact checks complete",
        paste0(
          accepted, " automated correction", if(accepted == 1L) " was" else "s were",
          " retained; ", skipped, " clean check", if(skipped == 1L) " was" else "s were",
          " left unchanged; ", rejected, " candidate",
          if(rejected == 1L) " was" else "s were",
          " rejected because the batch did not improve."
        ),
        47
      )
    }

    result <- app_attach_correction_metadata(processed)
    # identify_blockwise() reads this back so its "conform the library
    # instead" decision always matches what actually happened to this
    # specific object's axis, regardless of which pipeline stage called
    # ordinary_process() (whole upload, cluster collapse, pixel subset, ...).
    attr(result, "preserve_uploaded_axis") <- preserve_uploaded_axis
    result
  }

  spatial_data <- reactive({
    req(!is.null(preprocessed$data))
    uploaded <- data()
    if(is_Specs(uploaded)) return(uploaded)
    # Spatial smoothing has no neighbors to smooth across for a single
    # uploaded spectrum; silently skip it rather than erroring.
    if(!isTRUE(input$spatial_decision) || ncol(uploaded$spectra) <= 1L) {
      return(uploaded)
    }
    analysis_phase(
      "Smoothing the spectral map",
      "Applying spatial smoothing before thresholds or particle grouping.",
      18
    )
    spatial_smooth(
      uploaded, sigma = c(input$sigma, input$sigma, input$sigma)
    )
  })

  update_quantification_inputs <- function(axis, type) {
    defaults <- app_quantification_defaults(axis, type = type)
    common <- list(
      session = session, min = defaults$min, max = defaults$max,
      step = defaults$step
    )
    update_value <- function(id, value) {
      do.call(updateNumericInput, c(common, list(inputId = id, value = value)))
    }
    if(identical(type, "area")) {
      update_value("quant_numerator_area_min", defaults$numerator[[1L]])
      update_value("quant_numerator_area_max", defaults$numerator[[2L]])
      update_value("quant_denominator_area_min", defaults$denominator[[1L]])
      update_value("quant_denominator_area_max", defaults$denominator[[2L]])
    } else {
      update_value("quant_numerator_peak", defaults$numerator[[1L]])
      update_value("quant_denominator_peak", defaults$denominator[[1L]])
    }
  }

  observe({
    req(!is.null(preprocessed$data))
    axis <- DataR()$wavenumber
    signature <- digest::digest(axis, algo = "md5")
    current <- isolate(quantification_axis())
    if(is.null(current) || !identical(current$signature, signature)) {
      quantification_axis(list(signature = signature, axis = axis))
    }
  })

  observeEvent(list(quantification_axis(), input$quant_ratio_type), {
    axis_state <- quantification_axis()
    req(!is.null(axis_state))
    type <- input$quant_ratio_type
    if(is.null(type)) type <- "area"
    update_quantification_inputs(axis_state$axis, type)
  }, ignoreInit = TRUE)

  observeEvent(input$quant_ratio_add, {
    result <- tryCatch({
      type <- isolate(input$quant_ratio_type)
      if(is.null(type)) type <- "area"
      numerator <- if(identical(type, "peak")) {
        isolate(input$quant_numerator_peak)
      } else {
        c(
          isolate(input$quant_numerator_area_min),
          isolate(input$quant_numerator_area_max)
        )
      }
      denominator <- if(identical(type, "peak")) {
        isolate(input$quant_denominator_peak)
      } else {
        c(
          isolate(input$quant_denominator_area_min),
          isolate(input$quant_denominator_area_max)
        )
      }
      axis_state <- isolate(quantification_axis())
      app_add_ratio_definition(
        ratio_definitions(),
        name = isolate(input$quant_ratio_name),
        type = type,
        numerator = numerator,
        denominator = denominator,
        axis = if(is.null(axis_state)) NULL else axis_state$axis
      )
    }, error = function(error) error)

    if(inherits(result, "error")) {
      show_alert(
        title = "Ratio not added",
        text = conditionMessage(result),
        type = "error"
      )
      return()
    }
    ratio_definitions(result)
    analysis_dirty(TRUE)
    updateTextInput(session, "quant_ratio_name", value = "")
  })

  output$quant_saved_ratios <- renderUI({
    definitions <- ratio_definitions()
    if(!nrow(definitions)) {
      return(tags$p(
        class = "text-muted openspecy-ratio-empty",
        "No ratios saved yet. Define a name and bounds, then choose Add Ratio."
      ))
    }
    labels <- vapply(seq_len(nrow(definitions)), function(i) {
      app_ratio_definition_label(definitions[i, , drop = FALSE])
    }, character(1))
    tagList(
      selectInput(
        "quant_remove_id", "Saved ratios",
        choices = stats::setNames(as.character(definitions$id), labels),
        selected = as.character(utils::tail(definitions$id, 1L))
      ),
      actionButton(
        "quant_remove_ratio", "Remove Selected",
        icon = icon("trash"), class = "btn-outline-danger"
      )
    )
  })
  outputOptions(output, "quant_saved_ratios", suspendWhenHidden = FALSE)

  observeEvent(input$quant_remove_ratio, {
    id <- suppressWarnings(as.integer(isolate(input$quant_remove_id)))
    if(is.na(id)) return()
    definitions <- ratio_definitions()
    ratio_definitions(definitions[definitions$id != id, , drop = FALSE])
    analysis_dirty(TRUE)
  })

  observeEvent(list(quantification_axis(), input$quant_measurement_type), {
    axis_state <- quantification_axis()
    req(!is.null(axis_state))
    ui_type <- input$quant_measurement_type
    type <- if(identical(ui_type, "intensity")) "peak" else "area"
    defaults <- app_quantification_defaults(axis_state$axis, type = type)
    common <- list(
      session = session, min = defaults$min, max = defaults$max,
      step = defaults$step
    )
    if(identical(type, "area")) {
      do.call(updateNumericInput, c(common, list(
        inputId = "quant_measurement_area_min",
        value = defaults$numerator[[1L]]
      )))
      do.call(updateNumericInput, c(common, list(
        inputId = "quant_measurement_area_max",
        value = defaults$numerator[[2L]]
      )))
    } else {
      do.call(updateNumericInput, c(common, list(
        inputId = "quant_measurement_wavenumber",
        value = defaults$numerator[[1L]]
      )))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$quant_measurement_add, {
    result <- tryCatch({
      ui_type <- isolate(input$quant_measurement_type)
      type <- if(identical(ui_type, "intensity")) "point" else "area"
      values <- if(identical(type, "point")) {
        isolate(input$quant_measurement_wavenumber)
      } else {
        c(
          isolate(input$quant_measurement_area_min),
          isolate(input$quant_measurement_area_max)
        )
      }
      axis_state <- isolate(quantification_axis())
      app_add_measurement_definition(
        measurement_definitions(),
        name = isolate(input$quant_measurement_name),
        type = type,
        values = values,
        axis = if(is.null(axis_state)) NULL else axis_state$axis
      )
    }, error = function(error) error)
    if(inherits(result, "error")) {
      show_alert(
        title = "Measurement not added",
        text = conditionMessage(result),
        type = "error"
      )
      return()
    }
    measurement_definitions(result)
    analysis_dirty(TRUE)
    updateTextInput(session, "quant_measurement_name", value = "")
  })

  output$quant_measurement_definitions <- renderUI({
    definitions <- measurement_definitions()
    if(!nrow(definitions)) {
      return(tags$p(
        class = "text-muted openspecy-measurement-empty",
        "No single measurements saved yet."
      ))
    }
    tags$ul(lapply(seq_len(nrow(definitions)), function(i) {
      tags$li(app_measurement_definition_label(
        definitions[i, , drop = FALSE]
      ))
    }))
  })
  outputOptions(
    output, "quant_measurement_definitions", suspendWhenHidden = FALSE
  )

  observe({
    definitions <- measurement_definitions()
    labels <- if(nrow(definitions)) {
      vapply(seq_len(nrow(definitions)), function(i) {
        app_measurement_definition_label(definitions[i, , drop = FALSE])
      }, character(1))
    } else character()
    updateSelectInput(
      session, "quant_measurement_remove_id",
      choices = stats::setNames(as.character(definitions$id), labels),
      selected = if(nrow(definitions)) {
        as.character(utils::tail(definitions$id, 1L))
      } else character()
    )
  })

  observeEvent(input$quant_measurement_remove, {
    id <- suppressWarnings(as.integer(
      isolate(input$quant_measurement_remove_id)
    ))
    if(is.na(id)) return()
    definitions <- measurement_definitions()
    measurement_definitions(
      definitions[definitions$id != id, , drop = FALSE]
    )
    analysis_dirty(TRUE)
  })

  observeEvent(input$quant_measurement_clear, {
    measurement_definitions(app_empty_measurement_definitions())
    analysis_dirty(TRUE)
  })

  active_ratio_definitions <- reactive({
    ratio_definitions()
  })

  active_measurement_definitions <- reactive({
    measurement_definitions()
  })

  top_n_value <- reactive({
    value <- suppressWarnings(as.integer(input$top_n_input))
    if(length(value) != 1L || is.na(value) || value < 1L) 1L else value
  })

  MinSNR <- reactive({
    if(!isTRUE(input$threshold_decision)) {
      return(-Inf)
    }
    value <- suppressWarnings(as.numeric(input$MinSNR))
    if(length(value) != 1L || is.na(value)) -Inf else value
  })

  MaxSNR <- reactive({
    if(!isTRUE(input$threshold_decision)) {
      return(Inf)
    }
    value <- suppressWarnings(as.numeric(input$MaxSNR))
    if(length(value) != 1L || is.na(value)) Inf else value
  })

  MinCor <- reactive({
    if(!isTRUE(input$cor_threshold_decision)) return(-Inf)
    value <- suppressWarnings(as.numeric(input$MinCor))
    if(length(value) != 1L || is.na(value)) -Inf else value
  })

  # S/N Basis defaults to only the uploaded spectra plus the optional spatial
  # smooth (fast; independent of baseline, derivative, range, normalization,
  # particle collapse, or identification settings). Signal/Noise Basis =
  # "Fully Processed" instead runs the complete enabled preprocessing recipe,
  # including Min-Max normalization when selected, at real cost on a large map
  # -- deliberately not the default. Raw/Spatial never applies Min-Max. Either
  # way, this decides collapse eligibility (signal_eligible() below), not what
  # data particles collapse from.
  signal_to_noise_basis <- reactive({
    req(!is.null(preprocessed$data))
    spatial <- spatial_data()
    settings <- current_processing_settings()
    if(identical(input$signal_basis, "fully_processed")) {
      ordinary_process(spatial, settings = settings, view_only = TRUE)
    } else {
      if(is_Specs(spatial)) spatial <- decompress_spec(spatial, expand = FALSE)
      app_intensity_snr_basis(spatial, settings)
    }
  })

  signal_to_noise <- reactive({
    source <- spatial_data()
    if(inherits(source, "FileSpecs")) {
      settings <- current_processing_settings()
      fully_processed <- identical(input$signal_basis, "fully_processed")
      if(fully_processed) {
        issues <- app_file_stream_processing_issues(
          settings, spatial_smooth = isTRUE(input$spatial_decision)
        )
      } else {
        issues <- character()
      }
      if(length(issues)) {
        stop(paste(
          "Fully Processed file-backed signal/noise requires chunk-stable",
          "processing;", paste(issues, collapse = ", "),
          "or enable Load Entire File into Memory."
        ), call. = FALSE)
      }
      index <- OpenSpecy:::.filespec_index(source)
      values <- OpenSpecy:::.filespec_particle_snr(
        source, index = index,
        bands = seq_along(OpenSpecy:::.filespec_axis(source)),
        metric = effective_signal_selection(), abs = FALSE,
        spectral_smooth = isTRUE(input$spatial_decision),
        sigma1 = rep(as.numeric(input$sigma), 3L),
        chunk_size = 8192L,
        process = if(fully_processed) {
          function(block) ordinary_process(
            block, settings = settings, view_only = TRUE
          )
        } else {
          function(block) app_intensity_snr_basis(block, settings)
        }
      )
      names(values) <- index$source_id
      return(values)
    }
    if(is_Specs(source)) {
      background <- attr(source, "background")
      if(!is.null(background) && length(background$signal_to_noise) ==
         specs_source_count(source)) {
        values <- background$signal_to_noise
        names(values) <- specs_coordinates(source)$source_id
        return(values)
      }
    }
    basis <- signal_to_noise_basis()
    if(is_Specs(basis)) {
      basis <- decompress_spec(basis, expand = FALSE)
    }
    values <- sig_noise(
      basis, step = 10,
      metric = effective_signal_selection(), abs = FALSE
    )
    names(values) <- colnames(basis$spectra)
    values
  })

  signal_eligible <- reactive({
    values <- signal_to_noise()
    keep <- values > MinSNR() & values < MaxSNR()
    keep[is.na(keep)] <- FALSE
    if(!isTRUE(input$threshold_decision)) keep[] <- TRUE
    keep
  })

  # The Signal/Noise histogram preview is expensive to keep live (it can run
  # the full "Fully Processed" basis, or a spatial smooth, over the whole
  # map) and re-triggering it on every settings change is exactly the
  # flicker-before-Run pattern the rest of this file avoids. It only
  # recomputes on Run or an explicit "Recalculate Preview" click, and dims
  # (via a signature comparison) whenever the settings it depends on have
  # since changed.
  snr_preview <- reactiveVal(NULL)
  snr_preview_signature <- reactiveVal(NULL)
  snr_relevant_signature <- reactive({
    list(
      signal_basis = input$signal_basis, spatial_decision = input$spatial_decision,
      sigma = input$sigma, signal_selection = input$signal_selection,
      processing = if(identical(input$signal_basis, "fully_processed")) {
        current_processing_settings()
      } else {
        current_processing_settings()[c("intensity_decision", "intensity_corr")]
      }
    )
  })
  recalculate_snr_preview <- function() {
    if(is.null(preprocessed$data)) {
      files <- active_file_info()
      if(is.null(files)) return(invisible(NULL))
      read_uploaded_files(
        files, mounted = isTRUE(attr(files, "mounted", exact = TRUE))
      )
    }
    if(is.null(preprocessed$data)) return(invisible(NULL))
    # First statement, before the signal_to_noise()/sig_noise() scan below:
    # in the default configuration this function previously had no progress
    # signal at all, so Recalculate Preview looked unresponsive until the
    # (potentially whole-map) scan finished.
    analysis_phase("Calculating signal/noise", "Scanning the uploaded data.", 2)
    snr_preview(signal_to_noise())
    snr_preview_signature(snr_relevant_signature())
  }
  # Priority matches canonical_state_gate: canonical_signal_noise() below
  # reads snr_preview() and is itself read by default-priority gates
  # (quantified_data_gate and friends), so it must be populated before
  # those run on the same Run click.
  observeEvent(
    input$run_analysis, recalculate_snr_preview(),
    priority = RUN_GATE_PRIORITY_CANONICAL
  )
  observeEvent(input$recalculate_snr, recalculate_snr_preview(), ignoreInit = TRUE)
  observeEvent(list(
    input$local_files, input$local_native_files, input$mounted_files
  ), {
    snr_preview(NULL)
    snr_preview_signature(NULL)
  }, ignoreInit = TRUE)
  snr_preview_stale <- reactive({
    is.null(snr_preview_signature()) ||
      !identical(snr_preview_signature(), snr_relevant_signature())
  })
  preview_signal_metric <- reactive({
    signature <- snr_preview_signature()
    metric <- if(is.null(signature)) NULL else signature$signal_selection
    if(is.null(metric)) metric <- effective_signal_selection()
    metric
  })
  preview_signal_label <- reactive({
    app_signal_metric_label(preview_signal_metric())
  })
  observe({
    shinyjs::toggleClass(
      id = "snr_preview_container", class = "openspecy-preview-stale",
      condition = isTRUE(snr_preview_stale())
    )
  })
  # Mirrors the main Run button's dirty/clean convention exactly (green =
  # clicking it would change the result; dark navy = it already matches).
  observe({
    shinyjs::toggleClass(
      id = "recalculate_snr", class = "openspecy-run-dirty",
      condition = isTRUE(snr_preview_stale())
    )
  })

  particle_pipeline_enabled <- reactive({
    # Collapsing particle spectra requires a map with more than one spectrum
    # to group; silently ignore the setting for a single uploaded spectrum
    # instead of erroring.
    count <- if(is_Specs(preprocessed$data)) {
      specs_source_count(preprocessed$data)
    } else if(!is.null(preprocessed$data)) ncol(preprocessed$data$spectra) else 0L
    isTRUE(input$collapse_decision) && count > 1L
  })

  particle_collapse_function <- reactive({
    switch(
      input$collapse_type,
      "Median" = stats::median,
      "Geometric Mean" = OpenSpecy:::.particle_geometric_mean,
      base::mean
    )
  })

  particle_pca_components <- reactive({
    value <- suppressWarnings(as.integer(input$particle_pca_components))
    if(length(value) != 1L || is.na(value) || value < 1L) 10L else value
  })

  particle_cluster_k <- reactive({
    value <- suppressWarnings(as.integer(input$particle_cluster_k))
    if(length(value) != 1L || is.na(value) || value < 1L) 10L else value
  })

  particle_area_threshold <- reactive({
    value <- suppressWarnings(as.numeric(input$particle_area_threshold))
    if(length(value) != 1L || is.na(value) || value < 0) 1 else value
  })

  pixel_calibration <- reactive({
    if(!isTRUE(input$collapse_decision)) {
      return(app_pixel_calibration(1, "pixel"))
    }
    app_pixel_calibration(input$pixel_size, input$pixel_unit)
  })

  identify_blockwise <- function(object, batch_size = identify_batch_size()) {
    preserve_axis <- isTRUE(attr(object, "preserve_uploaded_axis", exact = TRUE))
    library <- analysis_library()
    req(!is.null(library))
    reference <- app_reference_for_query(
      library, object, preserve_axis = preserve_axis
    )
    report_identification_progress <- function(completed_blocks = 0L,
                                               total_blocks = NULL,
                                               group = NULL,
                                               completed_groups = NULL,
                                               total_groups = NULL, ...) {
      state <- app_identification_block_progress(
        query_count = ncol(object$spectra),
        library_count = ncol(reference$spectra),
        block_size = batch_size,
        completed_blocks = completed_blocks,
        total_blocks = total_blocks
      )
      if(isTruthy(group) && !is.null(completed_groups) &&
         !is.null(total_groups)) {
        state$detail <- paste0(
          state$detail, " Organization ", completed_groups, " of ",
          total_groups, ": ", group, "."
        )
      }
      analysis_phase(state$message, state$detail, state$progress)
    }
    report_identification_progress()
    match_spec(
      object, reference, top_n = top_n_value(), batch_size = batch_size,
      top_n_by = if(isTRUE(input$top_n_per_organization)) {
        "organization"
      } else NULL,
      conform = FALSE, type = "roll",
      progress = function(completed_blocks, total_blocks, ...) {
        report_identification_progress(completed_blocks, total_blocks, ...)
      }
    )
  }

  identify_filespec_best <- function(source, eligible, settings, batch_size,
                                     library_override = NULL,
                                     spatial_smooth = FALSE,
                                     spatial_sigma = c(1, 1, 1)) {
    issues <- app_file_stream_processing_issues(
      settings, spatial_smooth = spatial_smooth
    )
    if(length(issues)) {
      stop(paste0(
        "File-backed per-pixel correlation requires chunk-stable processing; ",
        paste(issues, collapse = ", "),
        ", or enable Load Entire File into Memory."
      ), call. = FALSE)
    }
    library <- if(is.null(library_override)) analysis_library() else
      library_override
    req(!is.null(library), is_OpenSpecy(library))
    reference <- NULL
    prepared_reference <- NULL
    total_queries <- sum(eligible, na.rm = TRUE)
    app_stream_filespec_best_matches(
      source, eligible = eligible, chunk_size = batch_size,
      process = function(query) {
        ordinary_process(query, settings = settings, view_only = TRUE)
      },
      identify = function(query) {
        preserve_axis <- isTRUE(attr(
          query, "preserve_uploaded_axis", exact = TRUE
        ))
        if(is.null(reference)) {
          reference <<- app_reference_for_query(
            library, query, preserve_axis = preserve_axis
          )
          prepared_reference <<- app_prepare_correlation_reference(reference)
        } else if(!identical(reference$wavenumber, query$wavenumber)) {
          stop(
            "Streamed preprocessing produced inconsistent wavenumber axes.",
            call. = FALSE
          )
        }
        app_match_prepared_best(
          query, prepared_reference,
          library_block_size = batch_size
        )
      },
      progress = function(completed_blocks, total_blocks,
                          completed_spectra, total_spectra, chunk_size) {
        fraction <- completed_blocks / max(1L, total_blocks)
        analysis_phase(
          paste0(
            "Identifying file-backed pixels (",
            as.integer(floor(100 * fraction)), "% complete)"
          ),
          paste0(
            "Retained one winning correlation per pixel for ",
            format(completed_spectra, big.mark = ","), " of ",
            format(total_queries, big.mark = ","),
            " eligible spectra; chunk size ",
            format(chunk_size, big.mark = ","), "."
          ),
          38 + 34 * fraction
        )
      },
      spatial_smooth = spatial_smooth, sigma = spatial_sigma
    )
  }

  classify_filespec_best <- function(source, eligible, settings, batch_size,
                                     spatial_smooth = FALSE,
                                     spatial_sigma = c(1, 1, 1)) {
    issues <- app_file_stream_processing_issues(
      settings, spatial_smooth = spatial_smooth
    )
    if(length(issues)) {
      stop(paste0(
        "File-backed per-pixel model classification requires chunk-stable ",
        "processing; ", paste(issues, collapse = ", "),
        ", or enable Load Entire File into Memory."
      ), call. = FALSE)
    }
    model_library <- analysis_library()
    req(!is.null(model_library))
    total_queries <- sum(eligible, na.rm = TRUE)
    app_stream_filespec_best_matches(
      source, eligible = eligible, chunk_size = batch_size,
      process = function(query) ordinary_process(
        query, settings = settings, view_only = TRUE
      ),
      identify = function(query) {
        prediction <- data.table::as.data.table(
          app_classify_model_library(query, model_library, top_n = 1L)
        )
        data.table::data.table(
          object_id = colnames(query$spectra)[as.integer(prediction$x)],
          library_id = as.character(prediction$name),
          match_val = as.numeric(prediction$value)
        )
      },
      progress = function(completed_blocks, total_blocks,
                          completed_spectra, total_spectra, chunk_size) {
        fraction <- completed_blocks / max(1L, total_blocks)
        analysis_phase(
          paste0("Classifying file-backed pixels (",
                 as.integer(floor(100 * fraction)), "% complete)"),
          paste0(
            "Retained one winning probability per pixel for ",
            format(completed_spectra, big.mark = ","), " of ",
            format(total_queries, big.mark = ","),
            " eligible spectra; chunk size ",
            format(chunk_size, big.mark = ","), "."
          ),
          38 + 34 * fraction
        )
      },
      spatial_smooth = spatial_smooth, sigma = spatial_sigma
    )
  }

  processed_filespec_selection <- function(source, pixel, settings) {
    pixel <- suppressWarnings(as.integer(pixel)[[1L]])
    query <- if(isTRUE(settings$spatial_smooth)) {
      index <- OpenSpecy:::.filespec_index(source)
      values <- OpenSpecy:::.filespec_smoothed_values(
        source, index, pixel, bands = NULL, sigma1 = settings$spatial_sigma
      )
      OpenSpecy:::.filespec_values_to_OpenSpecy(source, values)
    } else {
      decompress_spec(source, index = pixel)
    }
    ordinary_process(query, settings = settings$processing, view_only = TRUE)
  }

  best_match_rows <- function(matches) {
    matches <- data.table::as.data.table(matches)
    if(!nrow(matches)) return(matches)
    matches[, .SD[1L], by = object_id]
  }

  match_material <- function(library_id) {
    library <- analysis_library()
    if(is.null(library) || !is_OpenSpecy(library)) {
      return(rep.int("unknown", length(library_id)))
    }
    metadata <- data.table::as.data.table(library$metadata)
    ids <- if("sample_name" %in% names(metadata)) metadata$sample_name else
      colnames(library$spectra)
    classes <- if("material_class" %in% names(metadata)) {
      metadata$material_class
    } else rep("unknown", nrow(metadata))
    classes[match(library_id, ids)]
  }

  attach_best_matches <- function(object, matches) {
    ids <- colnames(object$spectra)
    object$metadata <- data.table::as.data.table(object$metadata)
    if(nrow(object$metadata) == length(ids)) object$metadata$col_id <- ids
    if(!"file_name" %in% names(object$metadata)) {
      files <- active_file_info()
      source_name <- if(!is.null(files$name)) files$name[[1L]] else "uploaded"
      object$metadata$file_name <- rep(source_name, nrow(object$metadata))
    }
    if(is.null(matches) || !nrow(matches)) return(object)
    best <- best_match_rows(matches)
    index <- match(ids, best$object_id)
    object$metadata$max_cor_name <- best$library_id[index]
    object$metadata$max_cor_val <- best$match_val[index]
    object$metadata$material_class <- match_material(best$library_id[index])
    object
  }

  # `eligible` marks pixels that fail the enabled signal/noise threshold.
  # They remain real columns in `object` (nothing is filtered out here,
  # unlike the collapse paths), but a NA `unit_index` keeps a click on one
  # from resolving to a valid spectrum, so it flat-lines like a rejected
  # collapsed particle instead of silently ignoring the threshold.
  aggregate_unit_matches <- function(matches, mapping, unit_ids) {
    library <- analysis_library()
    req(!is.null(library), is_OpenSpecy(library))
    groups <- if(isTRUE(input$top_n_per_organization)) {
      as.character(library$metadata$organization)
    } else NULL
    app_aggregate_unit_matches(
      matches, mapping, unit_ids = unit_ids,
      library_ids = colnames(library$spectra),
      top_n = top_n_value(), library_groups = groups
    )
  }

  expand_pixel_mapping <- function(subset_mapping, full_object,
                                   signal_keep) {
    metadata <- if(is_Specs(full_object)) {
      specs_metadata(full_object)
    } else data.table::as.data.table(full_object$metadata)
    ids <- if(is_Specs(full_object)) {
      specs_coordinates(full_object)$source_id
    } else colnames(full_object$spectra)
    full <- data.table::data.table(
      pixel_index = seq_along(ids), pixel_id = ids,
      source_id = OpenSpecy:::.particle_source_vector(metadata, length(ids)),
      x = if("x" %in% names(metadata)) metadata$x else seq_along(ids) - 1,
      y = if("y" %in% names(metadata)) metadata$y else 0,
      eligible = FALSE, material = NA_character_, region_id = NA_character_,
      cluster_id = NA_character_, unit_id = NA_character_,
      unit_index = NA_integer_, area = NA_integer_, kept = FALSE,
      rejection_reason = ifelse(signal_keep, "correlation", "signal/noise")
    )
    columns <- intersect(
      c(
        "eligible", "material", "region_id", "cluster_id", "unit_id",
        "unit_index", "area", "kept", "rejection_reason"
      ),
      names(subset_mapping)
    )
    rows <- match(subset_mapping$pixel_id, full$pixel_id)
    full[rows, (columns) := subset_mapping[, columns, with = FALSE]]
    full[rejection_reason == "threshold", rejection_reason := "correlation"]
    full
  }

  # This state is the only expensive analysis owner. It returns one final
  # OpenSpecy object, its compact Top-N match table, and a complete full-pixel
  # mapping used only to project unit results back onto the map.
  canonical_state_gate <- run_gated_reactive(function() {
    req(!is.null(preprocessed$data))
    # Never let a failed replacement Run expose the prior Run's source pixel
    # through the rejected-pixel inspection lane.
    inspection_source_gate(NULL)
    # Captured once per Run so every consumer (heatmap, plots, download
    # list, summary panels) can tell what actually produced the current
    # result instead of re-reading these settings live and drifting out of
    # sync with canonical_state() until the next Run.
    run_settings <- list(
      collapse = particle_pipeline_enabled(),
      strategy = input$particle_id_strategy,
      identification_active = isTRUE(input$identification_active),
      model_library = isTRUE(input$identification_active) &&
        identical(input$lib_type, "model"),
      top_n = top_n_value(),
      top_n_per_organization = isTRUE(input$identification_active) &&
        !identical(input$lib_type, "model") &&
        isTRUE(input$top_n_per_organization),
      identify_batch_size = identify_batch_size(),
      threshold_active = isTRUE(input$threshold_decision),
      correlation_active = particle_pipeline_enabled() &&
        isTRUE(input$cor_threshold_decision),
      min_snr = MinSNR(), max_snr = MaxSNR(), min_cor = MinCor(),
      signal_metric = effective_signal_selection(),
      load_entire_map = isTRUE(input$load_entire_map),
      file_backed_selection = FALSE,
      spatial_smooth = isTRUE(input$spatial_decision),
      spatial_sigma = rep(as.numeric(input$sigma), 3L),
      processing = current_processing_settings()
    )
    result <- tryCatch({
      run_library <- if(run_settings$identification_active) {
        if(run_settings$model_library) libraryR() else library_filtered()
      } else NULL
      analysis_library(run_library)
      spatial <- spatial_data()
      inspection_source_gate(spatial)
      use_library <- run_settings$identification_active &&
        !run_settings$model_library
      collapse <- run_settings$collapse
      strategy <- run_settings$strategy
      clustered <- collapse && strategy %in%
        c("partial_collapse", "nonspatial_collapse")
      cluster_buster <- collapse && identical(strategy, "cluster_buster_1000")
      correlation_threshold <- run_settings$correlation_active

      unavailable <- function(message, mapping = NULL, partition = NULL,
                              pixel_matches = NULL) list(
        object = NULL, matches = NULL, pixel_matches = pixel_matches,
        pixel_to_unit = mapping, partition = partition, error = NULL,
        diagnostic = message, settings = run_settings
      )

      mapping_match_fields <- function(mapping, source_ids, matches) {
        mapping <- data.table::copy(data.table::as.data.table(mapping))
        best <- best_match_rows(matches)
        index <- match(source_ids, best$object_id)
        mapping$threshold_match_val <- best$match_val[index]
        mapping$threshold_match_id <- best$library_id[index]
        mapping$threshold_material <- match_material(best$library_id[index])
        mapping
      }

      cluster_buster_reference <- function(background) {
        original <- analysis_library()
        reference <- app_reference_for_query(
          original, background,
          preserve_axis = isTRUE(attr(
            background, "preserve_uploaded_axis", exact = TRUE
          ))
        )
        app_append_cluster_buster_background(reference, background)
      }

      cluster_buster_mapping <- function(source, signal_keep, pixel_matches) {
        mapping <- app_identity_pixel_mapping(source, signal_keep)
        decisions <- app_cluster_buster_decisions(
          pixel_matches, mapping$pixel_id, signal_keep,
          correlation_enabled = correlation_threshold,
          minimum = run_settings$min_cor
        )
        mapping$threshold_match_val <- decisions$match_val
        mapping$threshold_match_id <- decisions$library_id
        mapping$threshold_material <- match_material(decisions$library_id)
        background_rows <- !is.na(decisions$library_id) &
          decisions$library_id == "background"
        mapping$threshold_material[background_rows] <- "background"
        replace <- !is.na(decisions$rejection_reason)
        mapping$rejection_reason[replace] <- decisions$rejection_reason[replace]
        list(mapping = mapping, decisions = decisions)
      }

      overlay_cluster_buster_mapping <- function(partition, threshold_state) {
        mapping <- data.table::as.data.table(partition$pixel_to_unit)
        source_rows <- match(
          mapping$pixel_id, threshold_state$mapping$pixel_id
        )
        for(column in c(
            "threshold_match_val", "threshold_match_id",
            "threshold_material")) {
          mapping[[column]] <- threshold_state$mapping[[column]][source_rows]
        }
        decision_rows <- match(
          mapping$pixel_id, threshold_state$decisions$pixel_id
        )
        rejected <- !is.na(decision_rows) &
          !threshold_state$decisions$keep[decision_rows]
        mapping$rejection_reason[rejected] <-
          threshold_state$decisions$rejection_reason[decision_rows][rejected]
        partition$pixel_to_unit <- mapping
        partition
      }

      if(cluster_buster && (!run_settings$threshold_active || !use_library)) {
        return(unavailable(paste(
          "Cluster Buster 1000 requires Threshold Signal / Noise and",
          "Identification with a medoid or full reference library."
        )))
      }

      if((correlation_threshold || (clustered &&
          identical(strategy, "partial_collapse"))) && !use_library) {
        return(unavailable(paste(
          "Correlation thresholds and spatial spectral clusters need",
          "Identification with a medoid or full reference library."
        )))
      }

      if(inherits(spatial, "FileSpecs")) {
        signal_keep <- signal_eligible()
        if(!any(signal_keep)) {
          return(unavailable(
            "No pixels pass the enabled signal/noise threshold."
          ))
        }
        if(!collapse) {
          run_settings$file_backed_selection <- TRUE
          mapping <- app_identity_pixel_mapping(spatial, signal_keep)
          pixel_matches <- if(run_settings$identification_active) {
            if(run_settings$model_library) {
              classify_filespec_best(
                spatial, eligible = signal_keep,
                settings = run_settings$processing,
                batch_size = run_settings$identify_batch_size,
                spatial_smooth = run_settings$spatial_smooth,
                spatial_sigma = run_settings$spatial_sigma
              )
            } else {
              identify_filespec_best(
                spatial, eligible = signal_keep,
                settings = run_settings$processing,
                batch_size = run_settings$identify_batch_size,
                spatial_smooth = run_settings$spatial_smooth,
                spatial_sigma = run_settings$spatial_sigma
              )
            }
          } else NULL
          selected <- which(signal_keep)[[1L]]
          analysis_phase(
            "Preparing file-backed spectrum inspection",
            paste(
              "Keeping compact signal/noise and identification summaries",
              "selectable and reading only the initial retained spectrum."
            ),
            34
          )
          processed <- processed_filespec_selection(
            spatial, selected, run_settings
          )
          return(list(
            object = processed, matches = NULL, pixel_matches = pixel_matches,
            pixel_to_unit = mapping, partition = NULL,
            error = NULL, diagnostic = NULL, settings = run_settings
          ))
        }
        unsupported <- character()
        if(!strategy %in% c("collapse", "cluster_buster_1000")) unsupported <- c(
          unsupported, "use Connected Particle collapse"
        )
        if(!identical(input$collapse_type, "Mean")) unsupported <- c(
          unsupported, "use Mean collapse"
        )
        if(length(unsupported)) {
          return(unavailable(paste0(
            "This file-backed map is protected from full-map materialization; ",
            paste(unique(unsupported), collapse = ", "), "."
          )))
        }
        if(cluster_buster) {
          analysis_phase(
            "Building Cluster Buster background",
            paste(
              "Processing S/N-retained spectra in bounded blocks and retaining",
              "only their running mean."
            ), 24
          )
          background <- app_stream_filespec_processed_mean(
            spatial, eligible = signal_keep,
            chunk_size = run_settings$identify_batch_size,
            process = function(query) ordinary_process(
              query, settings = run_settings$processing, view_only = TRUE
            ),
            progress = function(completed_blocks, total_blocks,
                                completed_spectra, total_spectra, ...) {
              fraction <- completed_blocks / max(1L, total_blocks)
              analysis_phase(
                paste0("Building background (", floor(100 * fraction), "%)"),
                paste0(
                  "Accumulated ", format(completed_spectra, big.mark = ","),
                  " of ", format(total_spectra, big.mark = ","),
                  " processed retained spectra."
                ), 24 + 10 * fraction
              )
            },
            spatial_smooth = run_settings$spatial_smooth,
            sigma = run_settings$spatial_sigma
          )
          temporary_library <- cluster_buster_reference(background)
          pixel_matches <- identify_filespec_best(
            spatial, eligible = signal_keep,
            settings = run_settings$processing,
            batch_size = run_settings$identify_batch_size,
            library_override = temporary_library,
            spatial_smooth = run_settings$spatial_smooth,
            spatial_sigma = run_settings$spatial_sigma
          )
          threshold_state <- cluster_buster_mapping(
            spatial, signal_keep, pixel_matches
          )
          partition <- OpenSpecy:::.filespec_collapse_connected_mean(
            spatial, eligible = threshold_state$decisions$keep,
            area_threshold = particle_area_threshold(),
            spectral_smooth = run_settings$spatial_smooth,
            sigma = run_settings$spatial_sigma, chunk_size = 8192L
          )
          partition$settings$requested_strategy <- strategy
          partition <- overlay_cluster_buster_mapping(
            partition, threshold_state
          )
          if(is.null(partition$analysis_units)) {
            return(unavailable(
              "No Cluster Buster particles meet the active filters and minimum area.",
              partition$pixel_to_unit, partition, pixel_matches
            ))
          }
          processed <- ordinary_process(partition$analysis_units)
          matches <- identify_blockwise(
            processed, run_settings$identify_batch_size
          )
          processed <- attach_best_matches(processed, matches)
          return(list(
            object = processed, matches = matches,
            pixel_matches = pixel_matches,
            pixel_to_unit = partition$pixel_to_unit, partition = partition,
            error = NULL, diagnostic = NULL, settings = run_settings
          ))
        }
        pixel_matches <- if(correlation_threshold) {
          identify_filespec_best(
            spatial, eligible = signal_keep,
            settings = run_settings$processing,
            batch_size = run_settings$identify_batch_size,
            spatial_smooth = run_settings$spatial_smooth,
            spatial_sigma = run_settings$spatial_sigma
          )
        } else NULL
        threshold_mapping <- app_identity_pixel_mapping(spatial, signal_keep)
        if(correlation_threshold) {
          threshold_mapping <- mapping_match_fields(
            threshold_mapping, threshold_mapping$pixel_id, pixel_matches
          )
          correlation_keep <-
            is.finite(threshold_mapping$threshold_match_val) &
            threshold_mapping$threshold_match_val >= run_settings$min_cor
          collapse_keep <- signal_keep & correlation_keep
          collapse_material <- threshold_mapping$threshold_material
        } else {
          collapse_keep <- signal_keep
          collapse_material <- NULL
        }
        analysis_phase(
          "Collapsing retained particles",
          paste(
            "Streaming retained spectra into connected particle means without",
            "materializing or combining the full map."
          ),
          if(correlation_threshold) 74 else 34
        )
        partition <- OpenSpecy:::.filespec_collapse_connected_mean(
          spatial, eligible = collapse_keep, material = collapse_material,
          area_threshold = particle_area_threshold(),
           spectral_smooth = run_settings$spatial_smooth,
           sigma = run_settings$spatial_sigma, chunk_size = 8192L
        )
        if(is.null(partition$analysis_units)) {
          return(unavailable(
            "No connected particle regions meet the active thresholds and minimum area.",
            partition$pixel_to_unit, partition, pixel_matches
          ))
        }
        if(correlation_threshold) {
          mapping <- data.table::as.data.table(partition$pixel_to_unit)
          source_rows <- match(
            mapping$pixel_index, threshold_mapping$pixel_index
          )
          for(column in c(
              "threshold_match_val", "threshold_match_id",
              "threshold_material")) {
            mapping[[column]] <- threshold_mapping[[column]][source_rows]
          }
          mapping$rejection_reason[
            signal_keep & !collapse_keep
          ] <- "correlation"
          partition$pixel_to_unit <- mapping
        }
        processed <- ordinary_process(partition$analysis_units)
        matches <- if(use_library) identify_blockwise(
          processed, run_settings$identify_batch_size
        ) else NULL
        processed <- attach_best_matches(processed, matches)
        return(list(
          object = processed, matches = matches,
          pixel_matches = pixel_matches,
          pixel_to_unit = partition$pixel_to_unit, partition = partition,
          error = NULL, diagnostic = NULL, settings = run_settings
        ))
      }

      if(!collapse) {
        if(is_Specs(spatial)) {
          compact_keep <- signal_eligible()
          if(!any(compact_keep)) {
            return(unavailable(
              "No pixels pass the enabled signal/noise threshold."
            ))
          }
          selected <- which(compact_keep)
          active <- decompress_spec(spatial, index = selected)
          processed <- ordinary_process(active)
        } else {
          selected <- seq_len(ncol(spatial$spectra))
          processed <- ordinary_process(spatial)
        }
        matches <- if(use_library) identify_blockwise(
          processed, run_settings$identify_batch_size
        ) else NULL
        processed <- attach_best_matches(processed, matches)
        mapping <- app_identity_pixel_mapping(
          processed, rep(TRUE, ncol(processed$spectra))
        )
        if(is_Specs(spatial)) {
          mapping <- expand_pixel_mapping(mapping, spatial, compact_keep)
        }
        return(list(
          object = processed, matches = matches, pixel_matches = matches,
          pixel_to_unit = mapping,
          partition = NULL, error = NULL, diagnostic = NULL,
          settings = run_settings
        ))
      }

      signal_keep <- signal_eligible()
      if(!any(signal_keep)) {
        return(unavailable(
          "No pixels pass the enabled signal/noise threshold."
        ))
      }
      signal_subset <- if(is_Specs(spatial)) {
        decompress_spec(spatial, index = which(signal_keep))
      } else if(all(signal_keep)) spatial else {
        filter_spec(spatial, logic = signal_keep)
      }

      if(cluster_buster) {
        analysis_phase(
          "Building Cluster Buster background",
          paste(
            "Processing retained pixels, averaging their processed spectra,",
            "and running bounded Top-1 background comparison."
          ), 28
        )
        processed_pixels <- ordinary_process(signal_subset)
        background <- app_cluster_buster_background(processed_pixels)
        temporary_library <- cluster_buster_reference(background)
        pixel_matches <- app_match_bounded_best(
          processed_pixels, temporary_library,
          block_size = run_settings$identify_batch_size,
          progress = function(completed_blocks, total_blocks, ...) {
            fraction <- completed_blocks / max(1L, total_blocks)
            analysis_phase(
              paste0("Cluster Buster matching (", floor(100 * fraction), "%)"),
              paste0(
                "Completed ", completed_blocks, " of ", total_blocks,
                " query blocks; block size ",
                run_settings$identify_batch_size, "."
              ), 38 + 34 * fraction
            )
          }
        )
        threshold_state <- cluster_buster_mapping(
          spatial, signal_keep, pixel_matches
        )
        partition <- OpenSpecy:::.partition_particle_map(
          spatial, eligible = threshold_state$decisions$keep,
          strategy = "collapse",
          collapse_function = particle_collapse_function(),
          area_threshold = particle_area_threshold()
        )
        partition$settings$requested_strategy <- strategy
        partition <- overlay_cluster_buster_mapping(partition, threshold_state)
        if(is.null(partition$analysis_units)) {
          return(unavailable(
            "No Cluster Buster particles meet the active filters and minimum area.",
            partition$pixel_to_unit, partition, pixel_matches
          ))
        }
        processed <- ordinary_process(partition$analysis_units)
        matches <- identify_blockwise(
          processed, run_settings$identify_batch_size
        )
        processed <- attach_best_matches(processed, matches)
        return(list(
          object = processed, matches = matches,
          pixel_matches = pixel_matches,
          pixel_to_unit = partition$pixel_to_unit, partition = partition,
          error = NULL, diagnostic = NULL, settings = run_settings
        ))
      }

      if(clustered) {
        # PCA/K-means is the first reduction and is fitted once per source.
        # Both modes identify the same processed cluster spectra. Spatial mode
        # then projects those identities back to the original pixels and makes
        # a second, connected same-material collapse without re-identifying.
        cluster_partition <- OpenSpecy:::.partition_particle_map(
          signal_subset, eligible = rep(TRUE, ncol(signal_subset$spectra)),
          strategy = "nonspatial_collapse",
          pca_components = particle_pca_components(),
          centers = particle_cluster_k(),
          collapse_function = particle_collapse_function(),
          area_threshold = if(identical(strategy, "partial_collapse")) 1 else
            particle_area_threshold()
        )
        cluster_partition$settings$requested_strategy <- strategy
        if(is.null(cluster_partition$analysis_units)) {
          return(unavailable(
            "No spectral clusters meet the active minimum area.",
            cluster_partition$pixel_to_unit, cluster_partition
          ))
        }
        processed_clusters <- ordinary_process(cluster_partition$analysis_units)
        cluster_matches <- if(use_library) {
          identify_blockwise(
            processed_clusters, run_settings$identify_batch_size
          )
        } else NULL
        processed_clusters <- attach_best_matches(
          processed_clusters, cluster_matches
        )
        full_cluster_mapping <- expand_pixel_mapping(
          cluster_partition$pixel_to_unit, spatial, signal_keep
        )
        cluster_ids <- full_cluster_mapping$unit_id
        if(use_library) {
          full_cluster_mapping <- mapping_match_fields(
            full_cluster_mapping, cluster_ids, cluster_matches
          )
        }
        cluster_keep <- !is.na(cluster_ids)
        if(correlation_threshold) {
          cluster_keep <- cluster_keep &
            !is.na(full_cluster_mapping$threshold_match_val) &
            full_cluster_mapping$threshold_match_val >= MinCor()
        }

        if(identical(strategy, "nonspatial_collapse")) {
          keep_ids <- unique(cluster_ids[cluster_keep])
          if(!length(keep_ids)) {
            return(unavailable(
              "No spectral clusters meet the enabled thresholds and minimum area.",
              full_cluster_mapping, cluster_partition, cluster_matches
            ))
          }
          full_cluster_mapping$kept <- cluster_keep
          full_cluster_mapping$eligible <- signal_keep & cluster_keep
          full_cluster_mapping$unit_id[!cluster_keep] <- NA_character_
          full_cluster_mapping$unit_index <- match(
            full_cluster_mapping$unit_id, keep_ids
          )
          full_cluster_mapping$rejection_reason[signal_keep & !cluster_keep] <-
            if(correlation_threshold) "correlation" else "area"
          keep_columns <- colnames(processed_clusters$spectra) %in% keep_ids
          processed_clusters <- if(all(keep_columns)) processed_clusters else
            filter_spec(processed_clusters, logic = keep_columns)
          matches <- if(is.null(cluster_matches)) NULL else
            cluster_matches[object_id %in% keep_ids]
          processed_clusters <- attach_best_matches(processed_clusters, matches)
          return(list(
            object = processed_clusters, matches = matches,
            pixel_matches = cluster_matches,
            pixel_to_unit = full_cluster_mapping,
            partition = cluster_partition, error = NULL, diagnostic = NULL,
            settings = run_settings
          ))
        }

        final_partition <- OpenSpecy:::.partition_particle_map(
          spatial, eligible = signal_keep & cluster_keep,
          strategy = "collapse",
          material = full_cluster_mapping$threshold_material,
          collapse_function = particle_collapse_function(),
          area_threshold = particle_area_threshold()
        )
        final_mapping <- final_partition$pixel_to_unit
        final_mapping$spectral_cluster_id <- cluster_ids
        for(column in c("threshold_match_val", "threshold_match_id",
                        "threshold_material")) {
          final_mapping[[column]] <- full_cluster_mapping[[column]]
        }
        if(is.null(final_partition$analysis_units)) {
          return(unavailable(
            "No connected material particles meet the enabled thresholds and minimum area.",
            final_mapping, cluster_partition, cluster_matches
          ))
        }
        final_object <- ordinary_process(final_partition$analysis_units)
        membership <- unique(data.table::data.table(
          pixel_id = cluster_ids[final_mapping$kept],
          unit_id = final_mapping$unit_id[final_mapping$kept],
          pixel_index = final_mapping$pixel_index[final_mapping$kept],
          kept = TRUE
        ), by = c("pixel_id", "unit_id"))
        matches <- app_aggregate_unit_matches(
          cluster_matches, membership,
          unit_ids = colnames(final_object$spectra),
          library_ids = colnames(analysis_library()$spectra),
          top_n = top_n_value(),
          library_groups = if(isTRUE(input$top_n_per_organization)) {
            as.character(analysis_library()$metadata$organization)
          } else NULL
        )
        final_object <- attach_best_matches(final_object, matches)
        cluster_partition$final_partition <- final_partition
        return(list(
          object = final_object, matches = matches,
          pixel_matches = cluster_matches, pixel_to_unit = final_mapping,
          partition = cluster_partition, error = NULL, diagnostic = NULL,
          settings = run_settings
        ))
      }

      if(!correlation_threshold) {
        partition <- OpenSpecy:::.partition_particle_map(
          spatial, eligible = signal_keep, strategy = "collapse",
          collapse_function = particle_collapse_function(),
          area_threshold = particle_area_threshold()
        )
        if(is.null(partition$analysis_units)) {
          return(unavailable(
            "No connected particle regions meet the active thresholds and minimum area.",
            partition$pixel_to_unit, partition
          ))
        }
        processed <- ordinary_process(partition$analysis_units)
        matches <- if(use_library) identify_blockwise(
          processed, run_settings$identify_batch_size
        ) else NULL
        processed <- attach_best_matches(processed, matches)
        return(list(
          object = processed, matches = matches, pixel_matches = NULL,
          pixel_to_unit = partition$pixel_to_unit, partition = partition,
          error = NULL, diagnostic = NULL, settings = run_settings
        ))
      }

      # Correlation-connected regions use one processed pixel identification
      # pass, then collapse the spatial-only source and reprocess the final
      # particles. Their Top-N rows are projected from that same first pass.
      processed_pixels <- ordinary_process(signal_subset)
      pixel_matches <- identify_blockwise(
        processed_pixels, run_settings$identify_batch_size
      )
      subset_mapping <- app_identity_pixel_mapping(signal_subset)
      subset_mapping <- mapping_match_fields(
        subset_mapping, subset_mapping$pixel_id, pixel_matches
      )
      full_identity <- expand_pixel_mapping(subset_mapping, spatial, signal_keep)
      for(column in c("threshold_match_val", "threshold_match_id",
                      "threshold_material")) {
        full_identity[[column]] <- NA
        full_identity[[column]][match(subset_mapping$pixel_id,
                                      full_identity$pixel_id)] <-
          subset_mapping[[column]]
      }
      correlation_keep <- !is.na(full_identity$threshold_match_val) &
        full_identity$threshold_match_val >= MinCor()
      partition <- OpenSpecy:::.partition_particle_map(
        spatial, eligible = signal_keep & correlation_keep,
        strategy = "collapse", material = full_identity$threshold_material,
        collapse_function = particle_collapse_function(),
        area_threshold = particle_area_threshold()
      )
      final_mapping <- partition$pixel_to_unit
      for(column in c("threshold_match_val", "threshold_match_id",
                      "threshold_material")) {
        final_mapping[[column]] <- full_identity[[column]]
      }
      if(is.null(partition$analysis_units)) {
        return(unavailable(
          "No connected particle regions meet the enabled thresholds and minimum area.",
          final_mapping, partition, pixel_matches
        ))
      }
      processed <- ordinary_process(partition$analysis_units)
      matches <- aggregate_unit_matches(
        pixel_matches, final_mapping, colnames(processed$spectra)
      )
      processed <- attach_best_matches(processed, matches)
      list(
        object = processed, matches = matches, pixel_matches = pixel_matches,
        pixel_to_unit = final_mapping, partition = partition,
        error = NULL, diagnostic = NULL
      )
    }, error = identity)

    if(inherits(result, "error")) {
      return(list(
        object = NULL, matches = NULL, pixel_matches = NULL,
        pixel_to_unit = NULL, partition = NULL,
        error = conditionMessage(result), diagnostic = NULL,
        settings = run_settings
      ))
    }
    result$settings <- run_settings
    result
  }, priority = RUN_GATE_PRIORITY_CANONICAL)
  canonical_state <- reactive(canonical_state_gate$read())

  canonical_error_key <- reactiveVal(NULL)
  observeEvent(canonical_state()$error, {
    error <- canonical_state()$error
    if(is.null(error) || identical(error, canonical_error_key())) return()
    canonical_error_key(error)
    show_alert(
      title = "Analysis could not complete", text = error, type = "error"
    )
  }, ignoreNULL = TRUE)
  observeEvent(
    list(input$local_files, input$local_native_files, input$mounted_files),
    canonical_error_key(NULL),
    ignoreInit = TRUE
  )

  output$particle_partition_status <- renderUI({
    state <- canonical_state()
    if(is.null(state$partition)) return(NULL)
    if(!is.null(state$diagnostic)) {
      return(tags$p(class = "text-warning", state$diagnostic))
    }
    settings <- state$partition$settings
    if(is.null(settings)) return(NULL)
    strategy <- if(is.null(settings$requested_strategy)) {
      settings$strategy
    } else settings$requested_strategy
    if(strategy %in% c("collapse", "cluster_buster_1000")) {
      retained <- unique(state$pixel_to_unit$unit_id[
        state$pixel_to_unit$kept & !is.na(state$pixel_to_unit$unit_id)
      ])
      return(tags$p(
        class = "text-muted",
        paste0(
          if(identical(strategy, "cluster_buster_1000")) {
            "Cluster Buster 1000: "
          } else "",
          length(retained), " connected particle regions retained."
        )
      ))
    }
    centers <- settings$centers
    centers_text <- if(!length(centers)) {
      "0"
    } else if(length(centers) <= 8L) {
      paste(centers, collapse = ", ")
    } else {
      paste0(min(centers), "-", max(centers), " across ", length(centers),
             " groups")
    }
    tags$p(
      class = "text-muted",
      paste0(
        if(identical(strategy, "partial_collapse")) {
          "Spatial material-connected mode. "
        } else {
          "Non-spatial spectral-cluster mode. "
        },
        "Effective PCA components: ", settings$pca_components,
        "; source-scoped K: ", centers_text, "; final particles: ",
        ncol(state$object$spectra), "."
      )
    )
  })
  outputOptions(output, "particle_partition_status", suspendWhenHidden = FALSE)

  canonical_final <- reactive({
    validate(need(
      !isTRUE(analysis_needs_reset()),
      "A new dataset was uploaded. Click Run to analyze it."
    ))
    state <- canonical_state()
    reason <- state$diagnostic
    if(is.null(reason)) reason <- state$error
    if(is.null(reason)) reason <- "Analysis is not available."
    validate(need(!is.null(state$object), reason))
    state$object
  })

  canonical_signal_noise <- reactive({
    object <- canonical_final()
    mapping <- canonical_state()$pixel_to_unit
    # snr_preview() (Run-gated) instead of live signal_to_noise(): the final
    # object and its pixel_to_unit mapping only change on Run, so the S/N
    # values attached to it must come from that same Run, not whatever the
    # Signal/Noise Basis/thresholding inputs currently say.
    pixel_values <- snr_preview()
    req(!is.null(pixel_values))
    ids <- colnames(object$spectra)
    if(is.null(mapping)) {
      values <- pixel_values[match(ids, names(pixel_values))]
      names(values) <- ids
      return(values)
    }
    mapping <- data.table::as.data.table(mapping)
    mapping[, signal_to_noise := as.numeric(pixel_values[pixel_index])]
    by_unit <- mapping[kept == TRUE & !is.na(unit_id), .(
      signal_to_noise = mean(signal_to_noise, na.rm = TRUE)
    ), by = unit_id]
    values <- by_unit$signal_to_noise[match(ids, by_unit$unit_id)]
    names(values) <- ids
    values
  })

  # Compatibility alias for existing plot/quantification code. Every consumer
  # receives the same canonical final object.
  DataR <- reactive(canonical_final())

  quantified_data_gate <- run_gated_reactive(function() {
    processed <- DataR()
    definitions <- active_ratio_definitions()
    measurements <- active_measurement_definitions()
    if(!nrow(definitions) && !nrow(measurements)) return(processed)
    analysis_phase(
      "Calculating saved quantification",
      paste0(
        "Calculating ", nrow(definitions), " saved ratio",
        if(nrow(definitions) == 1L) "" else "s", " and ",
        nrow(measurements), " single measurement",
        if(nrow(measurements) == 1L) "" else "s",
        " from the displayed processed spectra."
      ),
      49
    )
    app_attach_quantification(processed, definitions, measurements)
  })
  quantified_data <- reactive(quantified_data_gate$read())

  #The data to use in the plot. 
  selected_unit_index <- reactive({
      value <- suppressWarnings(as.integer(data_click$plot))
      # canonical_state() is nullable before the first successful Run. Do not
      # enter canonical_final()/DataR() here: its validation message is useful
      # to outputs, but an always-on selection observer turned that message
      # into a server warning during a quiet startup flush.
      state <- canonical_state()
      object <- state$object
      if(is.null(object) || is.null(object$spectra)) return(NA_integer_)
      if(isTRUE(state$settings$file_backed_selection)) {
        pixel <- suppressWarnings(as.integer(data_click$pixel))
        mapping <- data.table::as.data.table(state$pixel_to_unit)
        kept <- mapping$kept[match(pixel, mapping$pixel_index)]
        if(length(kept) == 1L && isTRUE(kept)) return(1L)
        return(NA_integer_)
      }
      count <- ncol(object$spectra)
      if(length(value) != 1L || is.na(value) || value < 1L ||
         value > count) return(NA_integer_)
      value
  })

  active_spectrum_view <- reactive({
    state <- canonical_state()
    if(isTRUE(state$settings$file_backed_selection)) {
      source <- inspection_source_gate()
      pixel <- suppressWarnings(as.integer(data_click$pixel))
      source_count <- if(is_Specs(source)) specs_source_count(source) else 0L
      validate(need(
        !is.null(source) && length(pixel) == 1L && !is.na(pixel) &&
          pixel >= 1L && pixel <= source_count,
        "The selected source pixel is not available for inspection."
      ))
      viewed <- processed_filespec_selection(source, pixel, state$settings)
      mapping <- data.table::as.data.table(state$pixel_to_unit)
      retained <- mapping$kept[match(pixel, mapping$pixel_index)]
      pixel_id <- specs_coordinates(source, pixel)$source_id[[1L]]
      viewed$metadata$col_id <- pixel_id
      if(length(retained) == 1L && isTRUE(retained)) {
        colnames(viewed$spectra) <- pixel_id
        viewed$metadata$selection <- "Retained file-backed spectrum"
        attr(viewed, "openspecy_selection_status") <- "retained"
      } else {
        colnames(viewed$spectra) <- paste0("Rejected pixel: ", pixel_id)
        viewed$metadata$selection <- "Rejected pixel inspection"
        attr(viewed, "openspecy_selection_status") <- "rejected_pixel"
      }
      return(viewed)
    }
    final <- DataR()
    selected <- selected_unit_index()
    if(!is.na(selected)) {
      viewed <- filter_spec(
        final, logic = seq_len(ncol(final$spectra)) == selected
      )
      attr(viewed, "openspecy_selection_status") <- "retained"
      return(viewed)
    }

    source <- inspection_source_gate()
    pixel <- suppressWarnings(as.integer(data_click$pixel))
    source_count <- if(is_Specs(source)) specs_source_count(source) else
      if(is.null(source)) 0L else ncol(source$spectra)
    validate(need(
      !is.null(source) && length(pixel) == 1L && !is.na(pixel) &&
        pixel >= 1L && pixel <= source_count,
      "The selected source pixel is not available for inspection."
    ))
    viewed <- if(is_Specs(source)) {
      decompress_spec(source, index = pixel)
    } else filter_spec(
      source, logic = seq_len(ncol(source$spectra)) == pixel
    )
    viewed <- ordinary_process(
      viewed, settings = canonical_state()$settings$processing,
      view_only = TRUE
    )
    pixel_id <- if(is_Specs(source)) {
      specs_coordinates(source, pixel)$source_id[[1L]]
    } else colnames(source$spectra)[[pixel]]
    colnames(viewed$spectra) <- paste0("Rejected pixel: ", pixel_id)
    viewed$metadata$col_id <- colnames(viewed$spectra)
    viewed$metadata$selection <- "Rejected pixel inspection"
    attr(viewed, "openspecy_selection_status") <- "rejected_pixel"
    viewed
  })

  DataR_plot <- reactive(active_spectrum_view())

  active_peak_positions <- reactive({
    if(!isTRUE(input$show_peak_positions)) return(NULL)
    viewed <- active_spectrum_view()
    if(!identical(attr(viewed, "openspecy_selection_status"), "retained")) {
      return(NULL)
    }
    if(isTRUE(input$make_rel_decision)) {
      viewed <- make_rel(viewed, na.rm = TRUE)
    }
    app_peak_positions(viewed, top_n = input$peak_count)
  })

  # SNR ----
  # The selected metric always controls S/N calculation and display. The
  # threshold owner controls only whether its bounds reject/black out pixels.
  effective_signal_selection <- reactive({
      metric <- as.character(input$signal_selection)[1L]
      valid <- c("run_sig_over_noise", "sig_times_noise", "log_tot_sig")
      if(is.na(metric) || !metric %in% valid) "run_sig_over_noise" else metric
  })

  quality_report <- reactive({
      if(is.null(preprocessed$data)) return(NULL)
      if(isTRUE(analysis_needs_reset()) ||
         is.null(canonical_state()$object)) return(NULL)
      selected <- DataR_plot()
      # co2_region/high_tail/spike are assessed here unconditionally, even
      # when their matching correction toggle (co2_decision/range_decision/
      # spike_decision) is off, so turning automatic correction off never
      # hides whether the viewed spectrum actually has the issue -- the
      # user still sees a warning (issue present) or success (none found).
      # assess_spec() takes one shared artifact_ratio for both co2_region
      # and high_tail; the app exposes them as two independent inputs, so
      # this reporting-only call picks the CO2 ratio when set, falling back
      # to the tail ratio, then the package default -- an intentional
      # simplification of the always-on assessment, not of either
      # correction's own (still independently-configured) automatic mode.
      quality_co2_region <- if(is.null(input$MinFlat) || is.null(input$MaxFlat)) {
        c(2200, 2420)
      } else sort(c(input$MinFlat, input$MaxFlat))
      quality_artifact_ratio <- if(!is.null(input$co2_artifact_ratio)) {
        input$co2_artifact_ratio
      } else if(!is.null(input$range_artifact_ratio)) {
        input$range_artifact_ratio
      } else 2
      quality_spike_args <- list(
        method = "residual",
        direction = if(is.null(input$spike_direction)) {
          "both"
        } else input$spike_direction,
        residual_threshold = if(is.null(input$spike_residual_threshold)) {
          8
        } else input$spike_residual_threshold,
        residual_window = if(is.null(input$spike_residual_window)) {
          5L
        } else as.integer(input$spike_residual_window)
      )
      # low_snr is deliberately not in app_quality_checks/requested here --
      # it would be redundant with the app's existing separate "SNR
      # Threshold" finding (app_threshold_quality_report() below, tied to
      # input$MinSNR).
      # saturation reuses app_saturation_value() (the same helper
      # ordinary_process() uses), guarded so an incomplete/invalid manual
      # ceiling degrades to "auto" for this reporting-only call instead of
      # erroring the whole quality report.
      quality_saturation <- tryCatch(
        app_saturation_value(
          if(is.null(input$saturation_mode)) "auto" else input$saturation_mode,
          input$saturation_ceiling
        ),
        error = function(error) "auto"
      )
      assessment <- tryCatch(
        assess_spec(
          selected,
          checks = app_quality_checks,
          report = "all",
          snr_metric = effective_signal_selection(),
          co2_region = quality_co2_region,
          artifact_ratio = quality_artifact_ratio,
          spike_args = quality_spike_args,
          saturation = quality_saturation
        ),
        error = function(error) data.frame(
          status = "warning",
          test_id = paste0(
            "spectrum:", colnames(selected$spectra)[[1L]],
            ":assessment"
          ),
          check = "assessment",
          description = conditionMessage(error),
          likely_cause = "The quality assessment could not complete.",
          potential_fix = paste(
            "Review the processed spectrum and settings, then run the",
            "assessment again."
          ),
          metric = NA_character_, value = NA_real_, threshold = NA_real_,
          region_min = NA_real_, region_max = NA_real_,
          stringsAsFactors = FALSE
        )
      )
      # Restrict Range may deliberately exclude an assessment region. That is
      # a successful no-op, not a warning about bad data. Genuine unavailable
      # checks remain warnings when the selected axis still covers the region.
      assessment <- app_mark_absent_quality_regions(
        assessment, selected$wavenumber,
        list(silent_region = c(2420, 2550),
             co2_region = quality_co2_region)
      )
      selected_index <- selected_unit_index()
      safe_selected_value <- function(values) {
        if(is.na(selected_index) || is.null(values) ||
           selected_index > length(values)) return(NA_real_)
        as.numeric(values[[selected_index]])
      }
      threshold_report <- app_threshold_quality_report(
        spectrum_id = colnames(selected$spectra)[[1L]],
        snr_value = if(isTRUE(input$threshold_decision)) {
          as.numeric(sig_noise(
            selected, step = 10, metric = effective_signal_selection(),
            abs = FALSE
          )[[1L]])
        } else NULL,
        snr_threshold = if(isTRUE(input$threshold_decision)) {
          input$MinSNR
        } else NULL,
        signal_metric = effective_signal_selection(),
        correlation_value = if(isTRUE(input$cor_threshold_decision)) {
          safe_selected_value(max_cor())
        } else NULL,
        correlation_threshold = if(isTRUE(input$cor_threshold_decision)) {
          input$MinCor
        } else NULL
      )
      report <- data.table::rbindlist(
        list(assessment, threshold_report), use.names = TRUE, fill = TRUE
      )
      app_quality_ui_report(report)
  })

  quality_findings <- reactive({
      report <- quality_report()
      stats::setNames(lapply(c("warning", "success"), function(status) {
        app_quality_status_report(report, status)
      }), c("warning", "success"))
  })
  quality_counts <- reactive(stats::setNames(
      vapply(quality_findings(), nrow, integer(1)), c("warning", "success")
  ))
  output$quality_warning_count <- renderText(quality_counts()[["warning"]])
  output$quality_success_count <- renderText(quality_counts()[["success"]])
  outputOptions(output, "quality_warning_count", suspendWhenHidden = FALSE)
  outputOptions(output, "quality_success_count", suspendWhenHidden = FALSE)

  automatic_report_gate <- run_gated_reactive(function() {
      app_automatic_report(
        x = if(is.null(preprocessed$data)) NULL else DataR(),
        diagnostics = correction_diagnostics(),
        enabled = c(
          spike = isTRUE(input$spike_decision),
          saturation = isTRUE(input$saturation_decision),
          flatten = isTRUE(input$co2_decision) && isTRUE(input$co2_automate),
          tails = isTRUE(input$range_decision) && isTRUE(input$range_automate)
        )
      )
  })
  automatic_report <- reactive(automatic_report_gate$read())
  automatic_count <- reactive(sum(automatic_report()$applied, na.rm = TRUE))
  output$quality_automatic_count <- renderText(automatic_count())
  outputOptions(output, "quality_automatic_count", suspendWhenHidden = FALSE)
  observe({
      shinyjs::toggleClass(
        "quality_automatic_details", "openspecy-automatic-applied",
        condition = automatic_count() > 0L
      )
  })

  show_quality_modal <- function(status, title, icon_name) {
      report <- quality_report()
      content <- if(is.null(report)) {
        app_quality_modal_content(NULL, status)
      } else {
        app_quality_modal_content(quality_findings()[[status]], status)
      }
      showModal(modalDialog(
        title = tagList(icon(icon_name), title),
        content,
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
  }
  for(observer_name in c("automatic", "warning", "success")) {
      existing_observer <- quality_modal_observers[[observer_name]]
      if(!is.null(existing_observer)) existing_observer$destroy()
  }
  quality_modal_observers$automatic <- observeEvent(
    input$quality_automatic_details, {
      showModal(modalDialog(
        title = tagList(icon("magic"), "Automatic corrections made"),
        app_automatic_modal_content(automatic_report()),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE
  )
  quality_modal_observers$warning <- observeEvent(
    input$quality_warning_details, {
      show_quality_modal(
        "warning", "Spectral quality warnings", "exclamation-triangle"
      )
    }, ignoreInit = TRUE
  )
  quality_modal_observers$success <- observeEvent(
    input$quality_success_details, {
      show_quality_modal(
        "success", "Successful spectral checks", "check-circle"
      )
    }, ignoreInit = TRUE
  )

  # Every successful Run owns a fresh rank-1/metadata readiness boundary. Key
  # this directly to the action event: two Runs may legitimately produce an
  # identical object, in which case a value-triggered observer would not fire.
  # The lower priority runs after the canonical and quantification gates.
  observeEvent(input$run_analysis, {
      current_run <- suppressWarnings(as.integer(input$run_analysis)[1L])
      state <- canonical_state()
      object <- quantified_data_gate$read()
      success <- !is.null(state$object) && !is.null(object)

      if(success) {
        if(isTRUE(state$settings$file_backed_selection)) {
          mapping <- data.table::as.data.table(state$pixel_to_unit)
          selected_pixel <- app_first_retained_pixel(mapping)
          req(!is.na(selected_pixel))
          data_click$plot <- selected_pixel
          data_click$pixel <- selected_pixel
          data_click$table <- 1L
          uploaded_cache <- app_uploaded_metadata_cache(
            inspection_source_gate(), snr_preview()
          )
          projection <- pixel_projection_gate$read()
          if(!is.null(projection) &&
             length(projection$correlation) == nrow(uploaded_cache)) {
            uploaded_cache$match_val <- projection$correlation
            uploaded_cache$material_class <- projection$material
            uploaded_cache$spectrum_identity <- projection$match_id
          }
          if(!is.null(projection) &&
             nrow(projection$metadata) == nrow(uploaded_cache)) {
            for(name in intersect(c("x", "y", "grid_x", "grid_y"),
                                  names(projection$metadata))) {
              uploaded_cache[[name]] <- projection$metadata[[name]]
            }
          }
          meta_cache(uploaded_cache)
        } else {
          selected <- app_initial_result_selection(object, state$pixel_to_unit)
          data_click$plot <- selected$plot
          data_click$pixel <- selected$pixel
          data_click$table <- selected$table
          uploaded_cache <- app_uploaded_metadata_cache(
            object, canonical_signal_noise()
          )
          projection <- pixel_projection_gate$read()
          if(!isTRUE(state$settings$collapse) && !is.null(projection) &&
             nrow(projection$metadata) == nrow(uploaded_cache)) {
            for(name in intersect(c("x", "y", "grid_x", "grid_y"),
                                  names(projection$metadata))) {
              uploaded_cache[[name]] <- projection$metadata[[name]]
            }
          }
          meta_cache(uploaded_cache)
        }
        selection_ready_run(current_run)
      }

  }, priority = -10L, ignoreInit = TRUE)
  RawR_plot <- reactive({
      req(!is.null(preprocessed$data))
      uploaded <- data()
      selected <- if(isTRUE(canonical_state()$settings$collapse) ||
                    isTRUE(canonical_state()$settings$file_backed_selection)) {
        data_click$pixel
      } else data_click$plot
      selected <- suppressWarnings(as.integer(selected))
      uploaded_count <- source_count(uploaded)
      if(length(selected) != 1L || is.na(selected) ||
         selected < 1L || selected > uploaded_count) {
        axis <- if(inherits(uploaded, "FileSpecs")) {
          OpenSpecy:::.filespec_axis(uploaded)
        } else if(is_Specs(uploaded)) {
          OpenSpecy:::.specs_variables_for_open_specy(uploaded$variables)
        } else uploaded$wavenumber
        return(app_rejected_spectrum(axis))
      }
      if(is_Specs(uploaded)) return(decompress_spec(uploaded, index = selected))
      filter_spec(
        uploaded,
        logic = seq_len(ncol(uploaded$spectra)) == selected
      )
  })
  
  selected_filespec_library_matches <- reactive({
    state <- canonical_state()
    req(isTRUE(state$settings$file_backed_selection))
    req(isTRUE(state$settings$identification_active))
    req(!isTRUE(state$settings$model_library))
    query <- active_spectrum_view()
    req(identical(attr(query, "openspecy_selection_status"), "retained"))
    library <- analysis_library()
    req(!is.null(library), is_OpenSpecy(library))
    reference <- app_reference_for_query(
      library, query,
      preserve_axis = isTRUE(attr(query, "preserve_uploaded_axis", exact = TRUE))
    )
    match_spec(
      query, reference, top_n = state$settings$top_n,
      batch_size = state$settings$identify_batch_size,
      top_n_by = if(isTRUE(state$settings$top_n_per_organization)) {
        "organization"
      } else NULL,
      conform = FALSE, type = "roll"
    )
  })

  identification_matches <- reactive({
    req(!is.null(preprocessed$data))
    state <- canonical_state()
    req(isTRUE(state$settings$identification_active))
    req(!isTRUE(state$settings$model_library))
    if(isTRUE(state$settings$file_backed_selection)) {
      return(selected_filespec_library_matches())
    }
    state$matches
  })

  #The output from the AI classification algorithm.
  ai_output_gate <- run_gated_reactive(function() { #tested working.
      req(!is.null(preprocessed$data))
      settings <- canonical_state()$settings
      req(isTRUE(settings$identification_active))
      req(isTRUE(settings$model_library))
      if(isTRUE(settings$file_backed_selection)) return(NULL)
      model_library <- analysis_library()
      req(!is.null(model_library))
      analysis_phase(
        "Classifying spectra",
        paste0("Running the selected model for ", ncol(DataR()$spectra),
               " uploaded spectrum", if(ncol(DataR()$spectra) == 1L) "." else "s."),
        76
      )

      app_classify_model_library(
        DataR(), model_library, top_n = settings$top_n
      )
  })
  selected_filespec_model_predictions <- reactive({
    state <- canonical_state()
    req(isTRUE(state$settings$file_backed_selection))
    req(isTRUE(state$settings$identification_active))
    req(isTRUE(state$settings$model_library))
    query <- active_spectrum_view()
    req(identical(attr(query, "openspecy_selection_status"), "retained"))
    app_classify_model_library(
      query, analysis_library(), top_n = state$settings$top_n
    )
  })
  ai_output <- reactive({
    state <- canonical_state()
    if(isTRUE(state$settings$file_backed_selection) &&
       isTRUE(state$settings$model_library)) {
      return(selected_filespec_model_predictions())
    }
    ai_output_gate$read()
  })

  # Best values are projected from the compact Top-N table; no full
  # library-by-spectrum matrix is created or retained.
  max_cor <- reactive({
      req(!is.null(preprocessed$data))
      settings <- canonical_state()$settings
      req(isTRUE(settings$identification_active))
      if(isTRUE(settings$model_library)) {
        predictions <- data.table::as.data.table(ai_output())
        winners <- if("rank" %in% names(predictions)) {
          predictions[rank == 1L]
        } else {
          predictions[, .SD[which.max(value)], by = x]
        }
        data.table::setorder(winners, x)
        ai <- as.numeric(winners$value)
        names(ai) <- winners$name
        return(ai)
      }
      matches <- identification_matches()
      if(is.null(matches) || !nrow(matches)) return(NULL)
      best <- best_match_rows(matches)
      index <- match(colnames(DataR()$spectra), best$object_id)
      values <- best$match_val[index]
      names(values) <- best$library_id[index]
      values
  })
  
  #The maximum correlation or AI value. 
  max_cor_identity <- reactive({
      req(!is.null(preprocessed$data))
      values <- max_cor()
      if(is.null(values)) return(NULL)
      identities <- if(!isTRUE(canonical_state()$settings$model_library)) {
        metadata <- data.table::as.data.table(DataR()$metadata)
        if("material_class" %in% names(metadata)) {
          as.character(metadata$material_class)
        } else match_material(names(values))
      } else app_standardize_material_class(names(values))
      data.table::fifelse(
        is.na(values) | values < canonical_state()$settings$min_cor,
        rep.int("unknown", length(values)), identities
      )
  })
  
  output$cor_plot_ui <- renderUI({
      plotlyOutput("cor_plot", height = "16vh")
  })

  output$cor_plot <- renderPlotly({
      state <- canonical_state()
      pixel_matches <- state$pixel_matches
      correlation_active <- isTRUE(state$settings$correlation_active)
      values <- if((correlation_active ||
                    isTRUE(state$settings$file_backed_selection)) &&
                   !is.null(pixel_matches) && nrow(pixel_matches)) {
        best_match_rows(pixel_matches)$match_val
      } else max_cor()
      req(!is.null(values), length(values))
      thresholds <- if(correlation_active) state$settings$min_cor else numeric()
      app_particle_plotly(list(
        type = "histogram", values = as.numeric(values),
        thresholds = thresholds, xlab = "Correlation"
      ), source = "cor_histogram")
  })
  

  
  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
      req(!is.null(preprocessed$data))
      settings <- canonical_state()$settings
      req(isTRUE(settings$identification_active))
      if(isTRUE(settings$model_library)){
          predictions <- data.table::as.data.table(ai_output())
          prediction_object <- if(isTRUE(settings$file_backed_selection)) {
            active_spectrum_view()
          } else DataR()
          data.table::data.table(
            object_id = colnames(prediction_object$spectra)[predictions$x],
            spectrum_index = as.integer(predictions$x),
            prediction_rank = if("rank" %in% names(predictions)) {
              as.integer(predictions$rank)
            } else {
              1L
            },
            .model_class_key = as.character(predictions$name),
            material_class = app_standardize_material_class(predictions$name),
            match_val = signif(as.numeric(predictions$value), 3),
            spectrum_type = if("spectrum_type" %in% names(predictions)) {
              as.character(predictions$spectrum_type)
            } else {
              NA_character_
            }
          )
      }
      else{
          selected <- selected_unit_index()
          if(is.na(selected)) {
            return(data.table::data.table(
              sample_name = character(), match_val = numeric()
            ))
          }
          selected_object_id <- if(isTRUE(settings$file_backed_selection)) {
            colnames(active_spectrum_view()$spectra)[[1L]]
          } else colnames(DataR()$spectra)[selected]
          app_matches_for_object(
            identification_matches(), selected_object_id
          ) %>%
              dplyr::rename(sample_name = library_id) %>%
              left_join(analysis_library()$metadata, by = c("sample_name")) %>%
              mutate(match_val = signif(match_val, 3)) %>%
              {
                settings <- canonical_state()$settings
                if(isTRUE(settings$correlation_active)) {
                  mutate(., name = ifelse(match_val < settings$min_cor, "Unknown",
                                          material_class))
                } else .
              }

      }
  })

  #Spectral data for the selected match. 
  match_selected <- reactive({# Default to first row if not yet clicked
      settings <- canonical_state()$settings
      req(isTRUE(settings$identification_active))
      req(!isTRUE(settings$model_library))

      # Get data from filter_spec
      rows <- matches_to_single()
      req(nrow(rows) > 0L)
      selected_row <- app_selected_rank_index(data_click$table, nrow(rows))
      library <- analysis_library()
      req(!is.null(library), is_OpenSpecy(library))
      filter_spec(
        library,
        logic = colnames(library$spectra) ==
          rows[[selected_row, "sample_name"]]
      )
  })

  selected_match <- reactive({
      settings <- canonical_state()$settings
      if(is.null(preprocessed$data) ||
         !isTRUE(settings$identification_active) ||
         isTRUE(settings$model_library)) return(NULL)
      tryCatch(
        match_selected(),
        shiny.silent.error = function(e) NULL
      )
  })

  selected_model_explanation <- reactive({
      settings <- canonical_state()$settings
      empty <- list(model = NULL, model_class = NULL)
      if(is.null(preprocessed$data) ||
         !isTRUE(settings$identification_active) ||
         !isTRUE(settings$model_library)) return(empty)
      app_selected_model_explanation(
        predictions = matches_to_single(),
        library = analysis_library(),
        selected_index = selected_unit_index(),
        selected_row = data_click$table
      )
  })

  simple_match_label <- reactive({
    app_match_value_label(isTRUE(canonical_state()$settings$model_library))
  })

  simple_signal_label <- reactive({
    app_signal_metric_label(canonical_state()$settings$signal_metric)
  })

  #All matches table for the current selection
  top_matches <- reactive({
      req(!is.null(preprocessed$data))
      settings <- canonical_state()$settings
      req(isTRUE(settings$identification_active))
      req(!is.na(selected_unit_index()))
      app_top_matches_table(
        matches_to_single(), isTRUE(settings$model_library),
        selected_unit_index(), simple = isTRUE(input$simple_metadata),
        match_label = simple_match_label()
      )
  })

#Create the data table that goes below the plot which provides extra metadata.
match_metadata <- reactive({
    req(!is.null(preprocessed$data))
    settings <- canonical_state()$settings
    if(isTRUE(settings$file_backed_selection)) {
      viewed <- active_spectrum_view()
      if(!identical(attr(viewed, "openspecy_selection_status"), "retained")) {
        return(data.table::data.table(
          Selection = "The selected pixel does not pass the signal threshold."
        ))
      }
      pixel <- suppressWarnings(as.integer(data_click$pixel))
      result <- data.table::copy(data.table::as.data.table(viewed$metadata))
      projection <- pixel_projection()
      if(!is.null(projection) && pixel <= nrow(projection$metadata)) {
        result$x <- projection$metadata$x[[pixel]]
        result$y <- projection$metadata$y[[pixel]]
      }
      values <- snr_preview()
      result[, signal_to_noise := as.numeric(values[[pixel]])]
      if(isTRUE(settings$identification_active)) {
        selected_rows <- data.table::as.data.table(matches_to_single())
        if(isTRUE(settings$model_library) &&
           "prediction_rank" %in% names(selected_rows)) {
          selected_rows <- selected_rows[prediction_rank == 1L]
        }
        if(nrow(selected_rows)) {
          winner <- selected_rows[1L]
          for(column in intersect(
              c("material_class", "match_val", "spectrum_identity",
                "organization"), names(winner))) {
            result[[column]] <- winner[[column]][[1L]]
          }
        }
      }
      display_unit <- if(!is.null(projection) && isTruthy(projection$axis_unit)) {
        projection$axis_unit
      } else "pixel"
      return(app_selection_metadata_display(
        result, simple = isTRUE(input$simple_metadata), particle = FALSE,
        pixel_size = 1, pixel_unit = display_unit,
        match_label = simple_match_label(),
        signal_label = simple_signal_label()
      ))
    }
    selected_index <- selected_unit_index()
    if(is.na(selected_index)) {
      return(data.table::data.table(
        Selection = "The selected pixel does not belong to a retained particle."
      ))
    }
    identification_active <- isTRUE(settings$identification_active)
    model_library <- isTRUE(settings$model_library)
    result <- if(!identification_active) {
        selected_object_id <- colnames(quantified_data()$spectra)[selected_index]
        app_selected_metadata(
          quantified_data(),
          data.table::data.table(object_id = selected_object_id),
          canonical_signal_noise()
        )
    } else if (!model_library) {
        rows <- matches_to_single()
        selected_row <- app_selected_rank_index(data_click$table, nrow(rows))
        selected_match <- rows[selected_row, ]
        app_selected_metadata(
          quantified_data(), selected_match, canonical_signal_noise()
        )
    } else {
        selected_object_id <- colnames(quantified_data()$spectra)[selected_index]
        prediction <- matches_to_single()[
          object_id == selected_object_id & prediction_rank == 1L
        ]
        if(nrow(prediction) && prediction$match_val[[1L]] < settings$min_cor) {
          prediction$material_class[[1L]] <- "unknown"
        }
        result <- bind_cols(
          quantified_data()$metadata[selected_index,],
          prediction[1L,]
        )
        result$signal_to_noise <- canonical_signal_noise()[selected_index]
        result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
            select(file_name, col_id, material_class, match_val, signal_to_noise, everything())
        result
    }
    display_calibration <- pixel_calibration()
    projection <- pixel_projection()
    if(!isTRUE(settings$collapse) && !is.null(projection) &&
       isTruthy(projection$axis_unit)) {
      display_calibration <- app_pixel_calibration(1, projection$axis_unit)
      if(selected_index <= nrow(projection$metadata) && nrow(result) == 1L) {
        result$x <- projection$metadata$x[[selected_index]]
        result$y <- projection$metadata$y[[selected_index]]
      }
    }
    app_selection_metadata_display(
      result,
      simple = isTRUE(input$simple_metadata),
      particle = isTRUE(settings$collapse),
      pixel_size = display_calibration$size,
      pixel_unit = display_calibration$unit,
      match_label = simple_match_label(),
      signal_label = simple_signal_label()
    )
})

# Display ----

#Histogram of SNR
output$snr_plot_ui <- renderUI({
    plotlyOutput("snr_plot", height = "16vh")
})

output$snr_plot <- renderPlotly({
    req(!is.null(preprocessed$data))
    values <- snr_preview()
    if(is.null(values)) {
      # req() alone would leave the previous dataset's chart frozen on
      # screen instead of visibly resetting to blank on a fresh upload.
      return(app_particle_plotly(list(
        type = "empty",
        reason = "Click Recalculate Preview (or Run) to compute this histogram."
      ), source = "snr_histogram"))
    }
    thresholds <- if(isTRUE(input$threshold_decision)) {
      c(MinSNR(), MaxSNR())
    } else numeric()
    app_particle_plotly(list(
      type = "histogram", values = as.numeric(values),
      thresholds = thresholds, xlab = preview_signal_label()
    ), source = "snr_histogram")
})

#Table of metadata for the selected spectrum and match
output$eventmetadata <- DT::renderDT({
    req(!is.null(match_metadata()))
    DT::datatable(
        match_metadata(),
        escape = TRUE,
        options = list(
            dom = 't',
            ordering = FALSE,
            paging = FALSE,
            info = FALSE
        ),
        rownames = FALSE,
        style = 'bootstrap',
        caption = "Selection Metadata",
        selection = 'none'
    )
}, server = FALSE)

# Create the data tables for all matches
output$event <- DT::renderDT({
    data <- top_matches()
    DT::datatable(data,
              options = list(scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              rownames = FALSE,
              filter = "top", caption = "Selectable Matches",
              style = "bootstrap",
              selection = list(mode = "single", selected = c(1)))
}, server = FALSE)
outputOptions(output, "event", suspendWhenHidden = FALSE)

#Full metadata table for uploaded spectra
output$sidebar_metadata <- DT::renderDT({
    req(!is.null(meta_cache()))
    selected <- app_uploaded_metadata_row(meta_cache(), data_click$plot)
    settings <- canonical_state()$settings
    calibration <- pixel_calibration()
    projection <- pixel_projection()
    if(!isTRUE(settings$collapse) && !is.null(projection) &&
       isTruthy(projection$axis_unit)) {
      calibration <- app_pixel_calibration(1, projection$axis_unit)
    }
    app_uploaded_metadata_table(
      meta_cache(), selected = selected,
      simple = isTRUE(input$simple_metadata),
      particle = isTRUE(settings$collapse),
      pixel_size = calibration$size, pixel_unit = calibration$unit,
      match_label = simple_match_label(),
      signal_label = simple_signal_label()
    )
}, server = TRUE)
outputOptions(output, "sidebar_metadata", suspendWhenHidden = FALSE)

  pixel_projection_gate <- run_gated_reactive(function() {
    req(!is.null(preprocessed$data))
    spatial <- spatial_data()
    ids <- if(is_Specs(spatial)) {
      specs_coordinates(spatial)$source_id
    } else colnames(spatial$spectra)
    mapping <- canonical_state()$pixel_to_unit
    if(is.null(mapping)) {
      mapping <- app_identity_pixel_mapping(spatial, signal_eligible())
    }
    mapping <- data.table::as.data.table(mapping)
    if(!"pixel_id" %in% names(mapping)) {
      stop("Pixel projection mapping is missing source identifiers.",
           call. = FALSE)
    }
    mapping <- mapping[match(ids, mapping$pixel_id)]

    unit_values <- function(values) {
      if(is.null(values)) return(rep(NA, length(ids)))
      values[match(mapping$unit_id, colnames(DataR()$spectra))]
    }

    state <- canonical_state()
    pixel_matches <- state$pixel_matches
    pixel_best <- if(!is.null(pixel_matches) && nrow(pixel_matches)) {
      best_match_rows(pixel_matches)
    } else NULL
    pixel_best_index <- if(is.null(pixel_best)) rep(NA_integer_, length(ids)) else
      match(ids, pixel_best$object_id)

    if(all(c("threshold_match_val", "threshold_match_id",
             "threshold_material") %in% names(mapping))) {
      correlation <- as.numeric(mapping$threshold_match_val)
      match_id <- as.character(mapping$threshold_match_id)
      material <- as.character(mapping$threshold_material)
    } else if(!is.null(pixel_best) && (
       isTRUE(state$settings$file_backed_selection) ||
       (particle_pipeline_enabled() && isTRUE(input$cor_threshold_decision)))) {
      correlation <- pixel_best$match_val[pixel_best_index]
      match_id <- pixel_best$library_id[pixel_best_index]
      material <- if(isTRUE(state$settings$model_library)) {
        app_standardize_material_class(match_id)
      } else match_material(match_id)
    } else if(is.null(state$object)) {
      correlation <- rep(NA_real_, length(ids))
      match_id <- rep(NA_character_, length(ids))
      material <- rep(NA_character_, length(ids))
    } else if(!isTRUE(state$settings$identification_active)) {
      correlation <- rep(NA_real_, length(ids))
      match_id <- rep(NA_character_, length(ids))
      material <- rep(NA_character_, length(ids))
    } else {
      correlation <- unit_values(max_cor())
      canonical_ids <- if(is.null(max_cor())) NULL else names(max_cor())
      match_id <- unit_values(canonical_ids)
      material <- unit_values(max_cor_identity())
    }

    signal <- as.numeric(signal_to_noise()[match(ids, names(signal_to_noise()))])
    signal_rejected <- app_threshold_rejection_mask(
      signal,
      enabled = isTRUE(input$threshold_decision),
      minimum = MinSNR(), maximum = MaxSNR()
    )
    correlation_rejected <- app_threshold_rejection_mask(
      correlation,
      enabled = isTRUE(input$cor_threshold_decision),
      minimum = MinCor()
    )
    rejected <- signal_rejected | correlation_rejected
    reason <- rep(NA_character_, length(ids))
    reason[signal_rejected & !correlation_rejected] <- "signal/noise"
    reason[!signal_rejected & correlation_rejected] <- "correlation"
    reason[signal_rejected & correlation_rejected] <-
      "signal/noise and correlation"

    calibration <- pixel_calibration()
    coordinate_projection <- app_project_source_coordinates(
      preprocessed$data, source_metadata(spatial),
      pixel_size = calibration$size, pixel_unit = calibration$unit
    )
    list(
      metadata = coordinate_projection$metadata,
      axis_unit = coordinate_projection$unit,
      coordinate_source = coordinate_projection$source, mapping = mapping,
      pixel_id = ids,
      signal_to_noise = signal, correlation = as.numeric(correlation),
      match_id = as.character(match_id), material = as.character(material),
      unit_id = mapping$unit_id, unit_index = mapping$unit_index,
      signal_rejected = signal_rejected,
      correlation_rejected = correlation_rejected,
      rejected = rejected,
      rejection_reason = reason
    )
  })
  pixel_projection <- reactive(pixel_projection_gate$read())

  map_color_choices <- reactive({
    req(source_count(preprocessed$data) > 1)
    state <- canonical_state()
    # Wait for the current dataset's first Run before offering any choice.
    # Rendering earlier (true the instant a map/batch is uploaded, before
    # Run) would default the selectize to whatever's available then --
    # usually just "Signal/Noise" -- and that premature value sticks even
    # once the full Material Class/Match ID/Match Value list exists.
    req(!is.null(state$object))
    projection <- pixel_projection()
    has_text <- function(values) {
      values <- as.character(values)
      any(!is.na(values) & nzchar(trimws(values)))
    }
    has_number <- function(values) any(is.finite(as.numeric(values)))
    preview <- snr_preview()
    availability <- c(
      "Material Class" = has_text(projection$material),
      "Match ID" = has_text(projection$match_id),
      "Match Value" = has_number(projection$correlation),
      "Signal/Noise" = has_number(preview),
      "Particle Unit" = has_number(projection$unit_index)
    )
    app_map_color_choices(
      identification_active = state$settings$identification_active,
      model_library = state$settings$model_library,
      collapse = state$settings$collapse,
      availability = availability,
      signal_label = preview_signal_label()
    )
  })

  resolved_map_color <- reactive({
    choices <- map_color_choices()
    req(length(choices) > 0L)
    selected <- input$map_color
    values <- unname(choices)
    if(!isTruthy(selected) || !selected %in% values) values[[1L]] else selected
  })

# Progress Bars
output$choice_names <- renderUI({
    choice_names <- map_color_choices()
    req(length(choice_names) > 0L)
    selected <- isolate(input$map_color)
    if(!isTruthy(selected) || !selected %in% unname(choice_names)) {
      selected <- unname(choice_names)[[1L]]
    }
        tagList(
            fluidRow(
                column(6, selectInput(inputId = "map_color", 
                                      label = "Map Color", 
                                      choices = choice_names,
                                      selected = selected)
                ),
                column(
                  3,
                  tags$div(
                    style = "padding-top:1.85rem;",
                    actionButton(
                      "heatmap_legend_details", "View Legend",
                      icon = icon("list"), class = "btn btn-outline-info"
                    )
                  )
                ),
                column(3, uiOutput("visual_overlay_controls"))
            )
                )
})

  registered_visual <- reactive({
    req(!is.null(preprocessed$data))
    app_registered_visual(preprocessed$data)
  })

  output$visual_overlay_controls <- renderUI({
    req(!is.null(registered_visual()))
    tagList(
      shinyWidgets::prettySwitch(
        "visual_overlay", "Visual Image Overlay", value = TRUE,
        status = "success", fill = TRUE, inline = TRUE
      ),
      sliderInput(
        "overlay_transparency", "Overlay Transparency",
        min = 0, max = 100, value = 20, step = 5, post = "%"
      )
    )
  })
  outputOptions(output, "visual_overlay_controls", suspendWhenHidden = FALSE)

output$progress_bars <- renderUI({
    req(!is.null(preprocessed$data))
    settings <- canonical_state()$settings
    req(source_count(preprocessed$data) > 1 || isTRUE(settings$collapse))

    # A single rounded percentage (shinyWidgets::progressBar() itself calls
    # round()) reads as "0%"/"none" whenever the true share is small but
    # nonzero -- exactly what a sparse real-world map with many small
    # particles looks like. Pass raw pixel counts as value/total instead, so
    # the bar also shows "142 / 331,180" alongside the rounded percentage.
    pixel_count <- function(x) {
      available <- !is.na(x)
      c(good = sum(x[available]), total = sum(available))
    }

    signal_values <- if(isTRUE(settings$threshold_active)) {
      pixel_projection()$signal_to_noise
    } else {
      NULL
    }
    correlation_values <- if(isTRUE(settings$correlation_active)) {
      pixel_projection()$correlation
    } else {
      NULL
    }

    metric_items <- list()
    if(!is.null(signal_values)) {
      counts <- pixel_count(
        signal_values > settings$min_snr & signal_values < settings$max_snr
      )
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "signal_summary_panel",
        shinyWidgets::progressBar(
          id = "signal_progress",
          value = counts[["good"]], total = counts[["total"]],
          status = "success",
          title = "Good Signal (% Pixels)",
          display_pct = TRUE
        )
      )
    }
    if(!is.null(correlation_values)) {
      counts <- pixel_count(correlation_values >= settings$min_cor)
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "correlation_summary_panel",
        shinyWidgets::progressBar(
          id = "correlation_progress",
          value = counts[["good"]], total = counts[["total"]],
          status = "success",
          title = "Good Match Values (% Pixels)",
          display_pct = TRUE
        )
      )
    }
    if(!is.null(signal_values) && !is.null(correlation_values)) {
      counts <- pixel_count(
        signal_values > settings$min_snr & signal_values < settings$max_snr &
          correlation_values >= settings$min_cor
      )
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "match_summary_panel",
        shinyWidgets::progressBar(
          id = "match_progress",
          value = counts[["good"]], total = counts[["total"]],
          status = "success",
          title = "Good Identifications (% Pixels)",
          display_pct = TRUE
        )
      )
    }

    plot_items <- list()
    if(isTRUE(settings$collapse) && !is.null(canonical_state()$object)) {
      plot_items[[length(plot_items) + 1L]] <- div(
        id = "particle_summary_panel",
        plotOutput("particle_plot", height = "25vh")
      )
    }
    plot_items[[length(plot_items) + 1L]] <- div(
      id = "material_summary_panel",
      plotOutput("material_plot", height = "25vh")
    )

    req(length(metric_items) + length(plot_items) > 0L)
    bs4Dash::box(
      id = "analysis_summary_box",
      title = "Summary",
      maximizable = TRUE,
      width = 12,
      app_summary_row(metric_items),
      app_summary_row(plot_items)
    )
})

  output$MyPlotC <- renderPlotly({
      if(is.null(preprocessed$data)) {
          message <- if(is.null(active_file_info())) {
            "Upload some data to get started."
          } else {
            "A new dataset was uploaded. Click Run to analyze it."
          }
          return(app_empty_spectrum_plot(message) %>%
                   config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape")))
      }

      primary <- DataR_plot()
      raw <- RawR_plot()
      reference <- selected_match()
      explanation <- selected_model_explanation()
      app_spectrum_plot(
        active = primary,
        raw = raw,
        reference = reference,
        model = explanation$model,
        model_class = explanation$model_class,
        peaks = active_peak_positions(),
        make_rel = isTRUE(input$make_rel_decision),
        source = "B",
        plot_width = session$clientData$output_MyPlotC_width
      ) %>%
        app_style_plotly() %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })

 #Heatmap ----
 #Display the map or batch data in a selectable heatmap.
  match_name_palette <- reactive({
      app_category_palette(pixel_projection()$material)
  })

  heatmap_state_for <- function(map_color) {
      projection <- pixel_projection()
      preview <- snr_preview()
      signal <- projection$signal_to_noise
      if(!is.null(preview)) {
        if(!is.null(names(preview)) && !is.null(projection$pixel_id)) {
          matched <- as.numeric(
            preview[match(projection$pixel_id, names(preview))]
          )
          if(any(is.finite(matched)) || length(preview) != length(signal)) {
            signal <- matched
          } else {
            signal <- as.numeric(preview)
          }
        } else if(length(preview) == length(signal)) {
          signal <- as.numeric(preview)
        }
      }
      signal_rejected <- app_threshold_rejection_mask(
        signal, enabled = isTRUE(input$threshold_decision),
        minimum = MinSNR(), maximum = MaxSNR()
      )
      correlation_rejected <- projection$correlation_rejected
      if(is.null(correlation_rejected)) {
        correlation_rejected <- rep(FALSE, length(signal_rejected))
      }
      rejected <- signal_rejected | correlation_rejected
      rejection_reason <- rep(NA_character_, length(rejected))
      rejection_reason[signal_rejected & !correlation_rejected] <- "signal/noise"
      rejection_reason[!signal_rejected & correlation_rejected] <- "correlation"
      rejection_reason[signal_rejected & correlation_rejected] <-
        "signal/noise and correlation"
      categorical <- FALSE
      z <- if(identical(map_color, "Particle Unit")) {
        categorical <- TRUE
        projection$unit_index
      } else if(identical(map_color, "Match ID")) {
        categorical <- TRUE
        projection$match_id
      } else if(identical(map_color, "Match Value")) {
        signif(projection$correlation, 3)
      } else if(identical(map_color, "Signal/Noise")) {
        signif(signal, 3)
      } else if(identical(map_color, "Material Class")) {
        categorical <- TRUE
        projection$material
      } else {
        validate(need(FALSE, "The selected map color is not available."))
      }
      if(categorical) {
        category_levels <- if(identical(map_color, "Particle Unit")) {
          as.character(sort(unique(as.integer(z[!is.na(z)]))))
        } else {
          sort(unique(as.character(z[!is.na(z)])))
        }
        z <- factor(
          as.character(z),
          levels = category_levels
        )
      }
      list(
        metadata = projection$metadata,
        z = z,
        categorical = categorical,
        rejected = rejected,
        rejection_reason = rejection_reason,
        axis_unit = projection$axis_unit
      )
  }

  heatmap_state <- reactive({
      req(!is.null(preprocessed$data))
      req(source_count(preprocessed$data) > 1)
      heatmap_state_for(resolved_map_color())
  })

  nearest_metadata_row <- function(metadata, x, y) {
    if(is.null(metadata) || !nrow(metadata) ||
       !all(c("x", "y") %in% names(metadata))) return(integer())
    dx <- suppressWarnings(as.numeric(metadata$x) - as.numeric(x))
    dy <- suppressWarnings(as.numeric(metadata$y) - as.numeric(y))
    distance <- dx^2 + dy^2
    distance[!is.finite(distance)] <- Inf
    if(all(is.infinite(distance))) return(integer())
    # .particle_map_grid() assigns duplicate x/y cells in row order, so the
    # last pixel at a coordinate is the one the user can actually see. Match
    # that rule here; choosing the first tie could select a hidden retained
    # pixel underneath a visibly rejected black cell.
    candidates <- which(distance == min(distance))
    candidates[[length(candidates)]]
  }

  # Particle and ordinary maps share one Plotly data contract and renderer.
  heatmap_data_for <- function(map_color) {
      state <- heatmap_state_for(map_color)
      legend_title <- if(identical(map_color, "Signal/Noise")) {
        preview_signal_label()
      } else map_color
      app_ordinary_heatmap_data(
        state$metadata, state$z, state$categorical, legend_title,
        rejected = state$rejected,
        rejection_reason = state$rejection_reason,
        axis_unit = state$axis_unit
      )
  }

  current_heatmap_data <- reactive({
      if(is.null(pixel_projection())) {
        return(list(
          type = "empty",
          reason = "A new dataset was uploaded. Click Run to analyze it."
        ))
      }
      data <- heatmap_data_for(resolved_map_color())
      visual <- registered_visual()
      if(!is.null(visual) && isTRUE(input$visual_overlay)) {
        data$visual_image <- visual$image
        transparency <- suppressWarnings(as.numeric(input$overlay_transparency))
        if(length(transparency) != 1L || !is.finite(transparency)) {
          transparency <- 20
        }
        data$overlay_opacity <- 1 - pmin(pmax(transparency, 0), 100) / 100
      }
      data
  })

  # The currently selected point's data coordinates come from the uploaded
  # map metadata used throughout the in-memory analysis.
  current_select_xy <- reactive({
      req(!is.null(preprocessed$data))
      selected <- data_click$pixel
      # data(), not spatial_data(): spatial_smooth() only convolves spectra,
      # it passes metadata (including x/y) through unchanged, and this only
      # needs coordinates. Reading spatial_data() here forced the
      # (potentially expensive) spatial-smoothing computation to run live
      # on every Spatial Smooth/sigma change, via this reactive's own
      # always-on observer below -- before Run, without the map ever
      # visibly changing, since nothing here used the smoothed values.
      metadata <- pixel_projection()$metadata
      if(length(selected) != 1L || is.na(selected) || selected < 1L ||
         selected > nrow(metadata)) {
        mapping <- canonical_state()$pixel_to_unit
        if(!is.null(mapping)) {
          selected <- mapping$pixel_index[match(data_click$plot,
                                                mapping$unit_index)]
        }
      }
      if(length(selected) != 1L || is.na(selected)) return(NULL)
      list(
        x = metadata$x[[selected]],
        y = metadata$y[[selected]]
      )
  })

  observeEvent(data_click$plot, {
    mapping <- canonical_state()$pixel_to_unit
    if(is.null(mapping)) return()
    selected_plot <- suppressWarnings(as.integer(data_click$plot))
    if(length(selected_plot) != 1L || is.na(selected_plot)) return()
    mapping <- data.table::as.data.table(mapping)
    current_pixel <- isolate(data_click$pixel)
    current_unit <- mapping$unit_index[match(current_pixel,
                                             mapping$pixel_index)]
    if(length(current_unit) == 1L && !is.na(current_unit) &&
       identical(as.integer(current_unit), selected_plot)) {
      return()
    }
    representative <- mapping[
      unit_index == selected_plot & kept == TRUE,
      pixel_index
    ]
    if(length(representative)) data_click$pixel <- representative[[1L]]
  }, ignoreNULL = TRUE)

  output$heatmapA <- plotly::renderPlotly({
      heatmap_data <- current_heatmap_data()
      spectrum_count <- if(is.null(preprocessed$data)) 0L else
        source_count(preprocessed$data)
      clickable <- app_has_clickable_heatmap(heatmap_data, spectrum_count)
      plot <- app_particle_plotly(current_heatmap_data(), source = "heat_plot",
        select = current_select_xy()
      )
      if(clickable) {
        # event_register() is attached above. Establish event_data() only in a
        # later flush, after renderPlotly has registered the real heatmap with
        # the session; doing it during startup produced Plotly's unregistered
        # source warning while the hidden map output was still suspended.
        session$onFlushed(function() heatmap_events_ready(TRUE), once = TRUE)
      } else {
        heatmap_events_ready(FALSE)
      }
      plot
  })

  observeEvent(input$heatmap_legend_details, {
      data <- current_heatmap_data()
      model <- app_heatmap_legend_model(data)
      showModal(modalDialog(
        title = paste(model$title, "Legend"),
        app_heatmap_legend_content(model),
        easyClose = TRUE, footer = modalButton("Close")
      ))
  }, ignoreInit = TRUE)

  observe({
      toggle(id = "heatmap_frame",
             condition = isTruthy(
               !is.null(preprocessed$data) &&
                  source_count(preprocessed$data) > 1 &&
                  !isTRUE(analysis_needs_reset()) &&
                  !is.null(pixel_projection())
             ))
  })

  observe({
      req(isTRUE(heatmap_events_ready()))
      click <- plotly::event_data("plotly_click", source = "heat_plot")
      req(!is.null(click))
      curve_number <- if(length(click$curveNumber)) {
        suppressWarnings(as.integer(click$curveNumber[[1L]]))
      } else {
        0L
      }
      image_offset <- as.integer(!is.null(current_heatmap_data()$visual_image))
      if(is.na(curve_number) ||
         !curve_number %in% (c(0L, 1L) + image_offset)) return()
      req(length(click$x), length(click$y))
      click_x <- click$x[[1L]]
      click_y <- click$y[[1L]]

      req(!is.null(preprocessed$data))
      click_metadata <- pixel_projection()$metadata
      selected <- nearest_metadata_row(click_metadata, click_x, click_y)
      if(length(selected) && selected <= source_count(preprocessed$data)) {
        data_click$pixel <- selected
        mapping <- canonical_state()$pixel_to_unit
        if(!is.null(mapping)) {
          unit <- mapping$unit_index[match(selected, mapping$pixel_index)]
          data_click$plot <- if(length(unit) == 1L && !is.na(unit)) {
            unit
          } else NA_integer_
        } else {
          data_click$plot <- selected
        }
      }
  })
  
  #Summary Plots ----
  output$particle_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(canonical_state()$settings$collapse))
      particles <- canonical_final()
      req(particles$metadata$area)
      calibration <- pixel_calibration()
      app_particle_size_plot(
        particles, calibration$size, calibration$unit
      )
  })
  
  output$material_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      settings <- canonical_state()$settings
      if(isTRUE(settings$identification_active)) {
          pixel_matches <- canonical_state()$pixel_matches
          match_names <- if(isTRUE(settings$file_backed_selection) &&
                            !is.null(pixel_matches) && nrow(pixel_matches)) {
            winners <- best_match_rows(pixel_matches)
            if(isTRUE(settings$model_library)) {
              app_standardize_material_class(winners$library_id)
            } else match_material(winners$library_id)
          } else max_cor_identity()
          req(!is.null(match_names), length(match_names))
      } else if(isTRUE(settings$collapse)) {
          particles <- canonical_final()
          req(!is.null(particles),
              "material_class" %in% names(particles$metadata))
          match_names <- particles$metadata$material_class
      } else {
          req(max_cor_identity())
          match_names <- max_cor_identity()
      }

      app_material_summary_plot(match_names, match_name_palette())
  })

  # Data Download options ----
  # Progress Bars
  output$download_ui <- renderUI({
    state <- canonical_state()
    choice_names <- app_download_choices(
      has_upload = !is.null(preprocessed$data),
      identification = !is.null(preprocessed$data) &&
        !is.null(state$object) &&
        isTRUE(state$settings$identification_active),
      collapse = isTRUE(state$settings$collapse) && !is.null(state$object),
      compact = is_Specs(preprocessed$data)
    )
    values <- unname(choice_names)
    current <- isolate(input$download_selection)
    selected <- if(isTruthy(current) && current %in% values) current else
      values[[1L]]
    selectInput(
      inputId = "download_selection",
      label = "Download type",
      choices = choice_names,
      selected = selected
    )
  })
  outputOptions(output, "download_ui", suspendWhenHidden = FALSE)

  # Once a particle-analysis result exists, jump the download type to
  # Thresholded Particles: it is the primary artifact for a large/collapsed
  # run and would otherwise stay stuck on whatever was selected before the
  # particle pipeline had a result (e.g. the initial "Test Data" default).
  observeEvent(canonical_state()$object, {
    state <- canonical_state()
    req(isTRUE(state$settings$collapse), !is.null(state$object))
    updateSelectInput(session, "download_selection",
                      selected = "Thresholded Particles")
  }, ignoreNULL = TRUE)

  # Same "jump to the newly relevant default" treatment for identification
  # results as the canonical particle result gets above -- without it, "User Metadata"
  # (always a valid choice) can never be displaced once selected, even after
  # identification produces Top Matches. particle_pipeline_enabled() is
  # excluded so it doesn't fight the Thresholded Particles default when both
  # are active.
  # max_cor() re-invalidates several times while identification/library
  # loading settle, and each invalidation carries an analysis_phase() busy
  # message; observing it directly kept the busy overlay's idle grace timer
  # from ever elapsing. Debouncing decouples "how often max_cor() recomputes"
  # from "how often we push a client update", so the default jump fires once,
  # after the result actually settles.
  max_cor_settled <- shiny::debounce(reactive(max_cor()), 1000)

  observeEvent(max_cor_settled(), {
    req(!is.null(max_cor_settled()), !isTRUE(canonical_state()$settings$collapse))
    updateSelectInput(session, "download_selection", selected = "Top Matches")
  }, ignoreNULL = TRUE)

  observeEvent(input$download_selection, {
    label <- app_download_label(input$download_selection)
    session$sendCustomMessage(
      "openspecy-download-label",
      list(
        id = "download_data",
        label = label,
        title = paste0(label, ". The file contents follow Download contents.")
      )
    )
  }, ignoreNULL = FALSE)

  output$particle_download_contents <- renderUI({
    req(identical(input$download_selection, "Thresholded Particles"))
    choices <- c(
      "Particle details" = "details",
      "Processed particle object" = "processed",
      "Final particle summary table" = "summary",
      "All analysis figures" = "figures"
    )
    tags$details(
      class = "openspecy-download-details",
      open = NA,
      tags$summary("Thresholded particle contents"),
      checkboxGroupInput(
        "particle_outputs_selected", NULL,
        choices = choices, selected = unname(choices)
      )
    )
  })
  outputOptions(output, "particle_download_contents", suspendWhenHidden = FALSE)

  output$download_data <- downloadHandler(
    filename = function() {
      selection <- input$download_selection
      if(identical(selection, "User Metadata")) {
        return(paste0("os_metadata_", human_ts(), ".csv"))
      }
      extension <- if(selection %in% c("Test Map", "Thresholded Particles")) {
        ".zip"
      } else if(identical(selection, "Compact Map (RDS)")) ".rds" else ".csv"
      paste0(gsub("[^A-Za-z0-9]+", "-", selection), "-", human_ts(), extension)
    },
    content = function(file) {
      selection <- input$download_selection
      req(length(selection) == 1L)
      message("OpenSpecy app: creating '", selection, "' download")

      if(identical(selection, "Test Data")) {
        fwrite(testdata, file)
      } else if(identical(selection, "Test Map")) {
        copied <- file.copy(read_extdata("CA_tiny_map.zip"), file,
                            overwrite = TRUE)
        if(!isTRUE(copied)) stop("Unable to copy the bundled Test Map.")
      } else if(identical(selection, "Processed Spectra")) {
        if(isTRUE(canonical_state()$settings$file_backed_selection)) {
          your_spec <- active_spectrum_view()
          your_spec <- app_attach_quantification(
            your_spec, active_ratio_definitions(),
            active_measurement_definitions()
          )
          pixel <- suppressWarnings(as.integer(data_click$pixel))
          your_spec$metadata$signal_to_noise <- snr_preview()[[pixel]]
        } else {
          your_spec <- quantified_data()
          your_spec$metadata$signal_to_noise <- canonical_signal_noise()
        }
        your_spec$metadata <- app_round_reported_metadata(your_spec$metadata)
        write_spec(your_spec, file)
      } else if(identical(selection, "Compact Map (RDS)")) {
        req(is_Specs(preprocessed$data))
        write_specs(preprocessed$data, file)
      } else if(identical(selection, "Top Matches")) {
        quant_columns <- app_ratio_metadata_columns(
          active_ratio_definitions(),
          active_measurement_definitions()
        )
        run_settings <- canonical_state()$settings
        if(!isTRUE(run_settings$model_library)) {
          top_n <- run_settings$top_n
          if(isTRUE(run_settings$file_backed_selection)) {
            processed <- active_spectrum_view()
            pixel <- suppressWarnings(as.integer(data_click$pixel))
            snr <- stats::setNames(
              as.numeric(snr_preview()[[pixel]]), colnames(processed$spectra)
            )
          } else {
            processed <- quantified_data()
            snr <- canonical_signal_noise()
          }
          all_matches <- app_top_matches_export_compact(
            matches = identification_matches(),
            library_metadata = analysis_library()$metadata,
            spectrum_metadata = processed$metadata,
            signal_to_noise = snr,
            match_threshold = run_settings$min_cor,
            signal_threshold = c(run_settings$min_snr, run_settings$max_snr),
            top_n = top_n,
            top_n_by = if(isTRUE(
              canonical_state()$settings$top_n_per_organization
            )) "organization" else NULL,
            simple = isTRUE(input$simple_metadata),
            quant_columns = quant_columns,
            match_label = simple_match_label(),
            signal_label = simple_signal_label()
          )
          fwrite(all_matches, file)
        } else {
          model_processed <- if(isTRUE(run_settings$file_backed_selection)) {
            active_spectrum_view()
          } else quantified_data()
          spectrum <- data.table::copy(
            data.table::as.data.table(model_processed$metadata)
          )
          if("material_class" %in% names(spectrum)) {
            spectrum[, material_class := NULL]
          }
          spectrum[, `:=`(
            spectrum_index = seq_len(.N),
            object_id = colnames(model_processed$spectra),
            signal_to_noise = if(isTRUE(run_settings$file_backed_selection)) {
              pixel <- suppressWarnings(as.integer(data_click$pixel))
              as.numeric(snr_preview()[[pixel]])
            } else canonical_signal_noise()
          )]
          result <- merge(
            matches_to_single(), spectrum,
            by = c("spectrum_index", "object_id"), all.x = TRUE, sort = FALSE
          )
          keep <- !sapply(result, OpenSpecy::is_empty_vector) |
            names(result) %in% quant_columns
          result <- result[, keep, with = FALSE] %>%
            select(file_name, col_id, material_class, match_val,
                   signal_to_noise, everything()) %>%
            mutate(
              material_class = ifelse(match_val < run_settings$min_cor, "unknown",
                                      material_class)
            )
          if(".model_class_key" %in% names(result)) {
            result[, .model_class_key := NULL]
          }
          result <- app_without_particle_metadata(result)
          result <- app_round_reported_metadata(result)
          if(isTRUE(input$simple_metadata)) {
            result <- app_selection_metadata_display(
              result, simple = TRUE, particle = FALSE, library = TRUE,
              match_label = simple_match_label(),
              signal_label = simple_signal_label()
            )
          }
          fwrite(result, file)
        }
      } else if(identical(selection, "Thresholded Particles")) {
        selected <- input$particle_outputs_selected
        if(is.null(selected)) selected <- c("details", "processed", "summary",
                                            "figures")
        archive_root <- file.path(
          particle_output_root, paste0("download-", human_ts())
        )
        dir.create(archive_root, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(archive_root, recursive = TRUE, force = TRUE), add = TRUE)
        files <- character()
        calibration <- pixel_calibration()
        if("details" %in% selected) {
          path <- file.path(archive_root, "particle_details.csv")
          details <- data.table::copy(
            data.table::as.data.table(canonical_final()$metadata)
          )
          signal_to_noise <- canonical_signal_noise()
          if(length(signal_to_noise) == nrow(details)) {
            details[, signal_to_noise := as.numeric(signal_to_noise)]
          }
          details <- app_selection_metadata_display(
            details, simple = isTRUE(input$simple_metadata), particle = TRUE,
            pixel_size = calibration$size, pixel_unit = calibration$unit,
            match_label = simple_match_label(),
            signal_label = simple_signal_label()
          )
          fwrite(details, path)
          files <- c(files, path)
        }
        if("processed" %in% selected) {
          path <- file.path(archive_root, "particles_processed.rds")
          processed_particles <- canonical_final()
          processed_particles$metadata <- app_particle_metadata_units(
            processed_particles$metadata, calibration$size, calibration$unit
          )
          processed_particles <- app_restore_spatial_coordinates(
            processed_particles
          )
          attr(processed_particles, "openspecy_spatial_unit") <-
            calibration$unit
          processed_particles$metadata <- app_round_reported_metadata(
            processed_particles$metadata
          )
          saveRDS(processed_particles, path)
          files <- c(files, path)
        }
        if("summary" %in% selected) {
          path <- file.path(archive_root, "particle_summary.csv")
          summary_material <- if(isTRUE(
            canonical_state()$settings$identification_active
          )) max_cor_identity() else NULL
          fwrite(app_particle_summary_table(
            canonical_final(), calibration$size, calibration$unit,
            material = summary_material
          ), path)
          files <- c(files, path)
        }
        if("figures" %in% selected) {
          run_settings <- canonical_state()$settings
          sn_thresholds <- if(isTRUE(run_settings$threshold_active)) {
            c(run_settings$min_snr, run_settings$max_snr)
          } else numeric()
          path <- file.path(archive_root, "signal_noise_histogram.png")
          app_write_ggplot_png(app_histogram_ggplot(
            snr_preview(), sn_thresholds, "Signal/Noise"
          ), path)
          files <- c(files, path)

          correlation_values <- if(!is.null(canonical_state()$pixel_matches) &&
                                    nrow(canonical_state()$pixel_matches)) {
            best_match_rows(canonical_state()$pixel_matches)$match_val
          } else max_cor()
          if(!is.null(correlation_values) && length(correlation_values)) {
            cor_thresholds <- if(isTRUE(run_settings$correlation_active)) {
              run_settings$min_cor
            } else numeric()
            path <- file.path(archive_root, "correlation_histogram.png")
            app_write_ggplot_png(app_histogram_ggplot(
              correlation_values, cor_thresholds, "Correlation"
            ), path)
            files <- c(files, path)
          }

          for(map_name in unname(map_color_choices())) {
            slug <- tolower(gsub("[^A-Za-z0-9]+", "_", map_name))
            components <- app_heatmap_export_components(
              heatmap_data_for(map_name)
            )
            path <- file.path(archive_root, paste0(slug, "_heatmap.png"))
            app_write_ggplot_png(
              components$heatmap, path,
              width = 8, height = 7
            )
            files <- c(files, path)
            if(!is.null(components$legend)) {
              path <- file.path(archive_root, paste0(slug, "_legend.png"))
              app_write_grob_png(components$legend, path)
              files <- c(files, path)
            }
          }

          path <- file.path(archive_root, "particle_size_distribution.png")
          app_write_ggplot_png(app_particle_size_plot(
            canonical_final(), calibration$size, calibration$unit
          ), path)
          files <- c(files, path)
          material <- if(isTRUE(run_settings$identification_active)) {
            max_cor_identity()
          } else if("material_class" %in% names(canonical_final()$metadata)) {
            canonical_final()$metadata$material_class
          } else NULL
          if(!is.null(material) && length(material)) {
            path <- file.path(archive_root, "material_summary.png")
            app_write_ggplot_png(app_material_summary_plot(material), path)
            files <- c(files, path)
          }
        }
        if(!length(files)) stop("Choose at least one available particle output.")
        zip_file <- tempfile("openspecy-particles-", fileext = ".zip")
        on.exit(unlink(zip_file, force = TRUE), add = TRUE)
        app_write_particle_archive(
          files, destination = zip_file, root = archive_root
        )
        if(!file.copy(zip_file, file, overwrite = TRUE)) {
          stop("Unable to prepare the thresholded-particle archive.")
        }
      } else if(identical(selection, "User Metadata")) {
        fwrite(data.table::as.data.table(user_metadata()), file)
      } else {
        stop("Unsupported download selection: ", selection)
      }

      if(!file.exists(file) || is.na(file.info(file)$size) ||
         file.info(file)$size <= 0) {
        stop("The app did not create a nonempty download for '", selection, "'.")
      }
      message("OpenSpecy app: completed '", selection, "' download (",
              file.info(file)$size, " bytes)")
    }
  )

  # A DT rerender briefly clears event_rows_selected. Do not treat that transient
  # NULL as a user choice: reset rank only when the viewed spectrum or its
  # prediction set changes, and otherwise retain genuine row clicks.
  observeEvent(input$event_rows_selected, ignoreInit = TRUE, {
      selected <- suppressWarnings(as.integer(input$event_rows_selected)[1L])
      if(!is.na(selected) && selected >= 1L) data_click$table <- selected
  })
  last_rank_unit <- reactiveVal(NA_integer_)
  observeEvent(selected_unit_index(), {
      selected <- selected_unit_index()
      req(!is.na(selected))
      previous <- last_rank_unit()
      last_rank_unit(selected)
      # Fresh-Run rank initialization belongs to the canonical readiness
      # observer. Reset only when a user moves from one valid spectrum/unit to
      # another, keeping the DT row, reference overlay, and metadata aligned.
      if(is.na(previous) || identical(previous, selected)) return()
      data_click$table <- 1L
  }, ignoreInit = TRUE)
  last_filespec_pixel <- reactiveVal(NA_integer_)
  observeEvent(data_click$pixel, {
      if(!isTRUE(canonical_state()$settings$file_backed_selection)) return()
      selected <- suppressWarnings(as.integer(data_click$pixel)[1L])
      previous <- last_filespec_pixel()
      last_filespec_pixel(selected)
      if(!is.na(previous) && !identical(previous, selected)) {
        data_click$table <- 1L
      }
  }, ignoreInit = TRUE)
  # meta_cache()'s .openspecy_index is always a column index into
  # quantified_data()/canonical_final() -- i.e. a *unit* index (one particle
  # per row when collapsed, one pixel per row otherwise; identical when not
  # collapsed). Resolve both data_click$pixel and data_click$plot directly
  # here (mirroring the heatmap-click handler below) instead of only setting
  # $plot and relying on the separate observeEvent(data_click$plot, ...) to
  # pick up the change: that observer -- and an earlier version of this one
  # -- skip the update whenever the clicked unit already equals the current
  # $plot value, which is true by coincidence on the very first row click
  # whenever that row's unit index is 1 (matching data_click$plot's initial
  # default), silently leaving the marker at its stale/default location.
  # Previously this also set data_click$pixel to the *unit* index directly,
  # which is only a raw pixel index by coincidence -- landing on an
  # unrelated/random map location for any other selection.
  observeEvent(input$sidebar_metadata_rows_selected, ignoreInit = TRUE, {
      req(!is.null(meta_cache()))
      sel <- app_uploaded_metadata_spectrum(
        meta_cache(), input$sidebar_metadata_rows_selected
      )
      if(!length(sel)) return()
      # The metadata table rerenders with the selected row whenever
      # data_click$plot changes from ANY source, including a manual heatmap
      # click -- and that client-side selection change can echo straight back
      # through this same input, indistinguishable from a genuine table
      # click. Without this guard, that echo re-derives a "representative"
      # (first, not necessarily clicked) member pixel for the unit and
      # overwrites data_click$pixel, so a manual click anywhere on a
      # multi-pixel collapsed particle silently snaps back to that
      # particle's representative pixel instead of staying where clicked.
      if(identical(sel, suppressWarnings(as.integer(isolate(data_click$plot))))) {
        return()
      }
      mapping <- canonical_state()$pixel_to_unit
      if(!is.null(mapping)) {
        mapping <- data.table::as.data.table(mapping)
        representative <- mapping[unit_index == sel & kept == TRUE, pixel_index]
        if(length(representative)) data_click$pixel <- representative[[1L]]
      }
      data_click$plot <- sel
  })


  move_selection <- function(dx = 0, dy = 0) {
      if(isTRUE(canonical_state()$settings$collapse)) {
        metadata <- source_metadata(spatial_data())
        current <- data_click$pixel
        if(length(current) != 1L || is.na(current) ||
           !all(c("x", "y") %in% names(metadata))) return()
        target <- paste(
          metadata$x[[current]] + dx, metadata$y[[current]] + dy
        )
        target_row <- match(target, paste(metadata$x, metadata$y))
        if(!is.na(target_row)) {
          data_click$pixel <- target_row
          mapping <- canonical_state()$pixel_to_unit
          unit <- mapping$unit_index[match(target_row, mapping$pixel_index)]
          data_click$plot <- if(length(unit) == 1L && !is.na(unit)) {
            unit
          } else NA_integer_
        }
        return()
      }
      req(!is.null(meta_cache()))
      meta <- meta_cache()
      cur <- data_click$plot
      row <- app_uploaded_metadata_row(meta, cur)
      if(!length(row) || !all(c("x", "y") %in% names(meta))) return()
      nav_x <- if("grid_x" %in% names(meta)) meta$grid_x else meta$x
      nav_y <- if("grid_y" %in% names(meta)) meta$grid_y else meta$y
      target <- paste(nav_x[[row]] + dx, nav_y[[row]] + dy)
      target_row <- match(target, meta$.openspecy_coord_key)
      if (!is.na(target_row)) {
        data_click$plot <- meta$.openspecy_index[[target_row]]
      }
  }

  observeEvent(input$left_spec,  { move_selection(dx = -1) })
  observeEvent(input$right_spec, { move_selection(dx =  1) })
  observeEvent(input$up_spec,    { move_selection(dy =  1) })
  observeEvent(input$down_spec,  { move_selection(dy = -1) })

  output$nav_buttons <- renderUI({
      req(!is.null(preprocessed$data))
      if (source_count(preprocessed$data) > 1) {
          tagList(
              div(style = "display:flex;justify-content:center;", actionButton("up_spec", label = NULL, icon = icon("arrow-up"))),
              div(style = "display:flex;justify-content:center;gap:0.5em;", 
                  actionButton("left_spec",  label = NULL, icon = icon("arrow-left")),
                  actionButton("right_spec", label = NULL, icon = icon("arrow-right"))
              ),
              div(style = "display:flex;justify-content:center;", actionButton("down_spec", label = NULL, icon = icon("arrow-down")))
          )
      }
  })
  outputOptions(output, "nav_buttons", suspendWhenHidden = FALSE)

  # Log events ----

  current_file_info <- reactive(active_file_info())
  
  user_metadata <- reactive({
    settings <- stats::setNames(
      lapply(app_user_metadata_input_ids, function(id) input[[id]]),
      app_user_metadata_input_ids
    )
    app_user_metadata_snapshot(
      settings = settings,
      definitions = ratio_definitions(),
      measurements = measurement_definitions(),
      recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      app_version = tryCatch(
        as.character(utils::packageVersion("OpenSpecy")),
        error = function(...) "development"
      ),
      session_id = session_id,
      source = preprocessed$data,
      file_info = current_file_info()
    )
  })

  # observe({
  #   req(!is.null(preprocessed$data))
  #       loggit("INFO", "trigger",
  #              user_metadata())
  # })
  
  #output$event_test <- renderPrint({
  #    list(
  #        conform_spec = input$conform_decision, 
  #        conform_args = list(range = NULL, res = input$conform_res, type = input$conform_selection)
  #    )
  #})
  
}
