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
  data_click <- reactiveValues(plot = NULL, pixel = NULL, table = NULL)
  meta_cache <- reactiveVal(NULL)
  correction_diagnostics <- reactiveVal(data.frame())
  ratio_definitions <- reactiveVal(app_empty_ratio_definitions())
  measurement_definitions <- reactiveVal(app_empty_measurement_definitions())
  quantification_axis <- reactiveVal(NULL)
  quality_modal_observers <- new.env(parent = emptyenv())

  # .match_spec_blockwise() computes and discards one library-by-block
  # correlation matrix at a time so memory stays bounded regardless of query
  # count; the result is identical for any block size. 1000 cuts per-block R
  # loop/allocation overhead substantially versus the old 100 on large maps
  # (tens of thousands of query pixels) while keeping peak memory for one
  # block (library_count * block_size * 8 bytes) small.
  identify_block_size <- 1000L

  # The Run button is the single trigger for the full analysis tranche; it is
  # enabled purely by upload completion (preprocessed$data becoming non-NULL)
  # and is not gated by any other setting.
  observe({
    shinyjs::toggleState("run_analysis", condition = !is.null(preprocessed$data))
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
  settings_signature <- reactive({
    lapply(app_user_metadata_input_ids, function(id) input[[id]])
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

  # One "Turn All On/Off" button per settings tab that has switches. The
  # label names the action the click will take (based on whether every
  # switch in the tab is already on), not the current state.
  app_tab_switch_ids <- list(
    preprocessing = c(
      "make_rel_decision", "smooth_decision", "conform_decision",
      "intensity_decision", "baseline_decision", "range_decision",
      "co2_decision", "spike_decision", "saturation_decision"
    ),
    identification = c("identification_active", "filter_lib"),
    advanced = c(
      "threshold_decision", "cor_threshold_decision", "spatial_decision",
      "xy_grid", "collapse_decision"
    )
  )
  app_render_tab_all_toggle <- function(tab) {
    ids <- app_tab_switch_ids[[tab]]
    values <- vapply(ids, function(id) isTRUE(input[[id]]), logical(1))
    turn_on <- !all(values)
    actionButton(
      paste0(tab, "_all_toggle"),
      if(turn_on) "Turn All On" else "Turn All Off",
      icon = icon(if(turn_on) "toggle-on" else "toggle-off"),
      class = "btn-sm openspecy-tab-all-toggle",
      title = paste0(
        if(turn_on) "Turn on every switch " else "Turn off every switch ",
        "in this tab."
      )
    )
  }
  lapply(names(app_tab_switch_ids), function(tab) {
    output[[paste0(tab, "_all_toggle")]] <- renderUI(
      app_render_tab_all_toggle(tab)
    )
    outputOptions(output, paste0(tab, "_all_toggle"), suspendWhenHidden = FALSE)
    observeEvent(input[[paste0(tab, "_all_toggle")]], {
      ids <- app_tab_switch_ids[[tab]]
      turn_on <- !all(vapply(ids, function(id) isTRUE(input[[id]]), logical(1)))
      for(id in ids) shinyWidgets::updatePrettySwitch(session, id, value = turn_on)
    }, ignoreInit = TRUE)
  })

  observe({
    active <- isTRUE(input$identification_active)
    shinyjs::toggleState("id_spec_type", condition = active)
    shinyjs::toggleState("id_strategy", condition = active)
    shinyjs::toggleState("lib_type", condition = active)
    shinyjs::toggleState("top_n_input", condition = active)
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
  #Sending data to a remote repo. 
observeEvent(input$file, {
  # Read in data when uploaded based on the file type
  data_click$plot <- 1
  data_click$pixel <- 1
  data_click$table <- 1
  preprocessed$data <- NULL
  upload_status_state(NULL)
  meta_cache(NULL)
  correction_diagnostics(data.frame())
  ratio_definitions(app_empty_ratio_definitions())
  measurement_definitions(app_empty_measurement_definitions())
  quantification_axis(NULL)

  upload_size <- app_validate_upload_size(input$file)
  if(!isTRUE(upload_size$ok)) {
    upload_status_state(upload_size$message)
    shinyjs::reset("file")
    return(NULL)
  }

  if (!all(grepl("(\\.tsv$)|(\\.h5$)|(\\.txt$)|(\\.img$)|(\\.dat$)|(\\.hdr$)|(\\.json$)|(\\.rds$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.dx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = T, as.character(input$file$name)))) {
    upload_status_state(paste(
      "Uploaded data type is not supported. Check the upload guidance for",
      "the accepted file extensions."
    ))
    shinyjs::reset("file")
    return(NULL)
  }

  analysis_phase(
    "Reading uploaded spectra",
    paste0("Reading and validating ", nrow(input$file), " uploaded file",
           if(nrow(input$file) == 1L) "." else "s."),
    8
  )
      
      rout <- tryCatch(expr = {
          # RDS maps are already serialized OpenSpecy objects. Reading a lone
          # RDS directly avoids dispatch and, critically for gigabyte maps,
          # avoids hashing/copying the full spectra matrix merely to add a
          # provenance ID. Existing IDs in the serialized object are retained.
          members <- if(nrow(input$file) == 1L &&
                       grepl("\\.rds$", input$file$name[[1L]],
                             ignore.case = TRUE)) {
            as_OpenSpecy(
              readRDS(as.character(input$file$datapath[[1L]])),
              compute_file_id = FALSE
            )
          } else {
            read_any(
              file = as.character(input$file$datapath), c_spec = FALSE
            )
          }
          combined <- if(is_OpenSpecy(members)) {
            members
          } else {
            c_spec(
              members, range = "common",
              res = if(input$conform_decision) input$conform_res else 8
            )
          }
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
      
      if(!inherits(rout, "simpleError") && all(!grepl("(\\.hdr$)|(\\.dat$)|(\\.zip$)", input$file$name))){
          rout$metadata$file_name <- input$file$name
      }
      
      if(!inherits(rout, "simpleError")){
          checkit <- tryCatch(expr = {check_OpenSpecy(rout)},
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
      show_alert(
        title = "Something went wrong with reading the data :-(",
        text =  paste0(if(inherits(rout, "simpleError")){paste0("There was an error during data loading that said ",
                                                                  rout, ".")} else{""},
                       if(inherits(checkit, "simpleError")){paste0(" There was an error during data checking that said ",
                                                                  checkit, ".")} else{""},
                       ". If you uploaded a text/csv file, make sure that the columns are numeric and named 'wavenumber' and 'intensity'."),
        type =  "error"
      )
      reset("file")
      preprocessed$data <- NULL
    }
    else if(inherits(checkit, "simpleWarning")) {
      upload_status_state(paste(
        "The uploaded spectra need attention:", as.character(checkit)
      ))
      reset("file")
      preprocessed$data <- NULL
    }
      
    else {
        analysis_phase(
          "Preparing uploaded spectra",
          "Checking spectral structure and preparing the shared wavenumber axis.",
          15
        )
        preprocessed$data <- rout
        upload_status_state(NULL)
        #print(preprocessed$data)

        # A newly uploaded dataset invalidates every previous Run's results;
        # clear them back to "not yet analyzed" instead of leaving the prior
        # dataset's heatmap/spectra/reports visible until the next Run click.
        analysis_dirty(TRUE)
        analysis_needs_reset(TRUE)
        canonical_state_gate$clear()
        quantified_data_gate$clear()
        quality_report_gate$clear()
        automatic_report_gate$clear()
        ai_output_gate$clear()
        pixel_projection_gate$clear()
    }
})

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
      if(input$id_strategy == "deriv" && input$lib_type == "medoid") {
        load_app_library("medoid_derivative")
      } else if(input$id_strategy == "nobaseline" &&
                input$lib_type == "medoid") {
        load_app_library("medoid_nobaseline")
      } else if(input$id_strategy == "deriv" &&
                input$lib_type == "model") {
        load_app_library("model_derivative")[[input$id_spec_type]]
      } else if(input$id_strategy == "nobaseline" &&
                input$lib_type == "model") {
        load_app_library("model_nobaseline")[[input$id_spec_type]]
      } else if(grepl("nobaseline$", input$id_strategy)) {
        load_app_library("nobaseline")
      } else {
        load_app_library("derivative")
      }
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
      }
      library
  })

  observeEvent(libraryR(), {
      orgs <- sort(unique(libraryR()$metadata$organization))
      current <- isolate(input$lib_org)
      selected <- intersect(current, orgs)
      if(!length(selected)) selected <- orgs
      updatePickerInput(session, "lib_org", choices = orgs,
                        selected = selected)
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
      if(isTruthy(input$xy_grid) &&
         (!all(diff(sort(da$metadata$y)) %in% c(0,1)) ||
          !all(diff(sort(da$metadata$x)) %in% c(0,1)))){
          grid <- gen_grid(nrow(da$metadata))
          da$metadata$x <- grid$x
          da$metadata$y <- grid$y
      }
          da
    })

  # Preprocess ----
  # Ordinary spectral processing is a pure operation over its input. Spatial
  # smoothing is deliberately kept outside this function so S/N and particle
  # partitioning always use the same spatial-only spectra.
  ordinary_process <- function(uploaded) {
    processed <- uploaded

    {
      spike_enabled <- isTRUE(input$spike_decision)
      spike_args <- if(spike_enabled) {
        list(
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
      } else {
        list()
      }
      saturation_enabled <- isTRUE(input$saturation_decision)
      saturation <- if(saturation_enabled) {
        saturation_mode <- if(is.null(input$saturation_mode)) {
          "auto"
        } else input$saturation_mode
        ceiling <- if(identical(saturation_mode, "threshold")) {
          input$saturation_ceiling
        } else {
          NULL
        }
        app_saturation_value(saturation_mode, ceiling)
      } else {
        NULL
      }
      saturation_args <- if(saturation_enabled) {
        list(
          max_saturation_loss = if(is.null(input$saturation_max_loss)) {
            0.7
          } else input$saturation_max_loss
        )
      } else {
        list()
      }
      if(spike_enabled || saturation_enabled) {
        correction_steps <- c(
          if(spike_enabled) "checking isolated spikes",
          if(saturation_enabled) "checking shared saturated ranges"
        )
        analysis_phase(
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
      analysis_phase(
        "Preprocessing spectra",
        paste0(
          "Applying the selected preprocessing steps to ",
          format(ncol(uploaded$spectra), big.mark = ","), " spectrum",
          if(ncol(uploaded$spectra) == 1L) "." else "s."
        ),
        26
      )
      intensity_enabled <- isTRUE(input$intensity_decision)
      intensity_args <- if(intensity_enabled) {
        list(type = input$intensity_corr)
      } else {
        list()
      }

      preserve_uploaded_axis <- app_conform_preserve_axis(
        processed, input$conform_decision, input$conform_selection,
        input$conform_res
      )
      conform_enabled <- isTRUE(input$conform_decision) &&
        !preserve_uploaded_axis
      conform_args <- if(conform_enabled) {
        list(
          range = app_conform_axis(processed, input$conform_res),
          res = NULL,
          # Mean Up only reaches this branch when the target resolution is
          # finer than the upload's native resolution, which calls for
          # interpolation (mean_up itself can only aggregate down).
          type = if(identical(input$conform_selection, "mean_up")) "interp" else
            input$conform_selection
        )
      } else {
        list()
      }

      baseline_enabled <- isTRUE(input$baseline_decision)
      baseline_args <- if(baseline_enabled) {
        if(identical(input$baseline_method, "fill_peaks")) {
          list(
            type = "fill_peaks",
            lambda = input$baseline_lambda,
            hwi = input$baseline_hwi,
            it = input$iterations,
            make_rel = FALSE
          )
        } else {
          list(
            type = "polynomial",
            degree = input$baseline,
            raw = FALSE,
            refit_at_end = input$refit,
            iterations = input$iterations,
            baseline = NULL,
            make_rel = FALSE
          )
        }
      } else {
        list()
      }

      smooth_enabled <- isTRUE(input$smooth_decision)
      smooth_args <- if(smooth_enabled) {
        smoothing_axis <- if(conform_enabled) {
          conform_args$range
        } else {
          processed$wavenumber
        }
        list(
          polynomial = input$smoother,
          window = calc_window_points(smoothing_axis, input$smoother_window),
          derivative = input$derivative_order,
          abs = input$derivative_abs
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
        make_rel = input$make_rel_decision
      )
      processed <- app_copy_correction_history(corrected_source, processed)
    }

    diagnostics <- list()
    if(isTRUE(input$co2_decision)) {
      if(isTRUE(input$co2_automate)) {
        co2_artifact_ratio <- input$co2_artifact_ratio
        if(is.null(co2_artifact_ratio)) co2_artifact_ratio <- 3
        analysis_phase(
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
            min = input$MinFlat,
            max = input$MaxFlat,
            artifact_ratio = co2_artifact_ratio
          )
        )
        processed <- result$data
        diagnostics[[length(diagnostics) + 1L]] <-
          result$diagnostics[result$diagnostics$enabled, , drop = FALSE]
      } else {
        processed <- flatten_range(
          processed,
          min = input$MinFlat,
          max = input$MaxFlat,
          make_rel = FALSE
        )
      }
    }

    if(isTRUE(input$range_decision)) {
      if(isTRUE(input$range_automate)) {
        range_artifact_ratio <- input$range_artifact_ratio
        if(is.null(range_artifact_ratio)) range_artifact_ratio <- 3
        analysis_phase(
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
          updateNumericInput(
            session, "MinRange", value = accepted_bounds[[1L]]
          )
          updateNumericInput(
            session, "MaxRange", value = accepted_bounds[[2L]]
          )
        }
        diagnostics[[length(diagnostics) + 1L]] <-
          result$diagnostics[result$diagnostics$enabled, , drop = FALSE]
      } else {
        processed <- restrict_range(
          processed,
          min = input$MinRange,
          max = input$MaxRange,
          make_rel = FALSE
        )
      }
    }

    diagnostics <- if(length(diagnostics)) {
      do.call(rbind, diagnostics)
    } else {
      data.frame()
    }
    correction_diagnostics(diagnostics)
    if(nrow(diagnostics)) {
      accepted <- sum(diagnostics$accepted)
      skipped <- sum(diagnostics$reason == "no_failures")
      rejected <- nrow(diagnostics) - accepted - skipped
      analysis_phase(
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
    if(is.null(preprocessed$data)) {
      return()
    }
    if(is.null(isolate(canonical_state()$object))) return()

    result <- tryCatch({
      processed <- isolate(DataR())
      type <- isolate(input$quant_ratio_type)
      if(is.null(type)) type <- "area"
      defaults <- app_quantification_defaults(
        processed$wavenumber,
        type = type
      )
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
      if(is.null(numerator) || !length(numerator)) {
        numerator <- defaults$numerator
      }
      if(is.null(denominator) || !length(denominator)) {
        denominator <- defaults$denominator
      }
      app_add_ratio_definition(
        ratio_definitions(),
        name = isolate(input$quant_ratio_name),
        type = type,
        numerator = numerator,
        denominator = denominator,
        axis = processed$wavenumber
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
    if(is.null(preprocessed$data)) {
      return()
    }
    if(is.null(isolate(canonical_state()$object))) return()
    result <- tryCatch({
      processed <- isolate(DataR())
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
      app_add_measurement_definition(
        measurement_definitions(),
        name = isolate(input$quant_measurement_name),
        type = type,
        values = values,
        axis = processed$wavenumber
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
  })

  observeEvent(input$quant_measurement_clear, {
    measurement_definitions(app_empty_measurement_definitions())
  })

  active_ratio_definitions <- reactive({
    ratio_definitions()
  })

  active_measurement_definitions <- reactive({
    measurement_definitions()
  })

  top_n_value <- reactive({
    value <- suppressWarnings(as.integer(input$top_n_input))
    if(length(value) != 1L || is.na(value) || value < 1L) 10L else value
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
  # "Fully Processed" instead runs every other enabled preprocessing step on
  # each pixel first, at real cost on a large map -- deliberately not the
  # default. Either way, this decides collapse eligibility (signal_eligible()
  # below), not what data particles collapse from.
  signal_to_noise_basis <- reactive({
    req(!is.null(preprocessed$data))
    if(identical(input$signal_basis, "fully_processed")) {
      ordinary_process(spatial_data())
    } else {
      spatial_data()
    }
  })

  signal_to_noise <- reactive({
    basis <- signal_to_noise_basis()
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
      threshold_decision = input$threshold_decision,
      MinSNR = input$MinSNR, MaxSNR = input$MaxSNR
    )
  })
  recalculate_snr_preview <- function() {
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
  observeEvent(input$file, {
    snr_preview(NULL)
    snr_preview_signature(NULL)
  }, ignoreInit = TRUE)
  snr_preview_stale <- reactive({
    is.null(snr_preview_signature()) ||
      !identical(snr_preview_signature(), snr_relevant_signature())
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
    isTRUE(input$collapse_decision) && !is.null(preprocessed$data) &&
      ncol(preprocessed$data$spectra) > 1L
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

  identify_blockwise <- function(object) {
    preserve_axis <- isTRUE(attr(object, "preserve_uploaded_axis", exact = TRUE))
    reference <- app_reference_for_query(
      library_filtered(), object, preserve_axis = preserve_axis
    )
    analysis_phase(
      "Identifying spectra",
      paste0(
        "Comparing ", format(ncol(object$spectra), big.mark = ","),
        " spectrum", if(ncol(object$spectra) == 1L) "" else "s",
        " with ", format(ncol(reference$spectra), big.mark = ","),
        " references in blocks of ", format(identify_block_size, big.mark = ","), "."
      ),
      76
    )
    OpenSpecy:::.match_spec_blockwise(
      object, reference, top_n = top_n_value(), block_size = identify_block_size,
      conform = FALSE, type = "roll"
    )
  }

  best_match_rows <- function(matches) {
    matches <- data.table::as.data.table(matches)
    if(!nrow(matches)) return(matches)
    matches[, .SD[1L], by = object_id]
  }

  match_material <- function(library_id) {
    metadata <- data.table::as.data.table(library_filtered()$metadata)
    ids <- if("sample_name" %in% names(metadata)) metadata$sample_name else
      colnames(library_filtered()$spectra)
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
      source_name <- if(!is.null(input$file$name)) input$file$name[[1L]] else
        "uploaded"
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
  identity_pixel_mapping <- function(object, eligible = NULL) {
    metadata <- data.table::as.data.table(object$metadata)
    ids <- colnames(object$spectra)
    eligible <- if(is.null(eligible)) rep(TRUE, length(ids)) else eligible
    data.table::data.table(
      pixel_index = seq_along(ids), pixel_id = ids,
      source_id = OpenSpecy:::.particle_source_vector(metadata, length(ids)),
      x = if("x" %in% names(metadata)) metadata$x else seq_along(ids) - 1,
      y = if("y" %in% names(metadata)) metadata$y else 0,
      eligible = eligible, material = NA_character_, region_id = ids,
      cluster_id = NA_character_, unit_id = ids,
      unit_index = ifelse(eligible, seq_along(ids), NA_integer_),
      area = 1L, kept = eligible,
      rejection_reason = ifelse(eligible, NA_character_, "threshold")
    )
  }

  aggregate_unit_matches <- function(matches, mapping, unit_ids) {
    app_aggregate_unit_matches(
      matches, mapping, unit_ids = unit_ids,
      library_ids = colnames(library_filtered()$spectra),
      top_n = top_n_value()
    )
  }

  expand_pixel_mapping <- function(subset_mapping, full_object,
                                   signal_keep) {
    metadata <- data.table::as.data.table(full_object$metadata)
    ids <- colnames(full_object$spectra)
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

  memory_preflight <- reactive({
    req(!is.null(preprocessed$data))
    library_size <- 0L
    if(!identical(input$lib_type, "model")) {
      library_size <- ncol(library_filtered()$spectra)
    }
    clustered <- particle_pipeline_enabled() &&
      input$particle_id_strategy %in%
        c("partial_collapse", "nonspatial_collapse")
    # Memory only depends on object dimensions/size, not spectral values, so
    # this reads the raw upload directly instead of spatial_data(). Reading
    # spatial_data() here would run the (potentially expensive) spatial
    # smooth as a side effect of this purely advisory, pre-Run estimate.
    tryCatch(
      OpenSpecy:::.app_memory_preflight(
        preprocessed$data, library_size = library_size,
        top_n = top_n_value(), block_size = identify_block_size,
        pca_components = if(clustered) particle_pca_components() else 0L,
        clusters = if(clustered) particle_cluster_k() else 0L
      ),
      error = function(error) structure(list(
        safe = NA, status = "unknown", peak_phase = NA_character_,
        message = paste(
          "Available RAM could not be estimated.",
          "The 10 GiB upload ceiling remains in effect; reduce the file,",
          "library, Top N, PCA components, or clusters if processing fails."
        ),
        error = conditionMessage(error)
      ), class = "OpenSpecy_memory_preflight")
    )
  })

  output$memory_preflight_status <- renderUI({
    if(is.null(preprocessed$data)) return(NULL)
    estimate <- memory_preflight()
    class_name <- switch(
      estimate$status, safe = "text-success", unsafe = "text-danger",
      "text-muted"
    )
    tags$p(class = class_name, estimate$message)
  })
  outputOptions(output, "memory_preflight_status", suspendWhenHidden = FALSE)

  # This state is the only expensive analysis owner. It returns one final
  # OpenSpecy object, its compact Top-N match table, and a complete full-pixel
  # mapping used only to project unit results back onto the map.
  canonical_state_gate <- run_gated_reactive(function() {
    req(!is.null(preprocessed$data))
    # Captured once per Run so every consumer (heatmap, plots, download
    # list, summary panels) can tell what actually produced the current
    # result instead of re-reading these settings live and drifting out of
    # sync with canonical_state() until the next Run.
    run_settings <- list(
      collapse = particle_pipeline_enabled(),
      strategy = input$particle_id_strategy,
      threshold_active = isTRUE(input$threshold_decision),
      correlation_active = particle_pipeline_enabled() &&
        isTRUE(input$cor_threshold_decision),
      min_snr = MinSNR(), max_snr = MaxSNR(), min_cor = MinCor()
    )
    estimate <- memory_preflight()
    if(identical(estimate$status, "unsafe")) {
      return(list(
        object = NULL, matches = NULL, pixel_matches = NULL,
        pixel_to_unit = NULL, partition = NULL, error = NULL,
        diagnostic = estimate$message, settings = run_settings
      ))
    }

    result <- tryCatch({
      spatial <- spatial_data()
      use_library <- isTRUE(input$identification_active) &&
        !identical(input$lib_type, "model")
      collapse <- run_settings$collapse
      strategy <- run_settings$strategy
      clustered <- collapse && strategy %in%
        c("partial_collapse", "nonspatial_collapse")
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

      if((correlation_threshold || (clustered &&
          identical(strategy, "partial_collapse"))) && !use_library) {
        return(unavailable(paste(
          "Correlation thresholds and spatial spectral clusters need",
          "Identification with a medoid or full reference library."
        )))
      }

      if(!collapse) {
        processed <- ordinary_process(spatial)
        matches <- if(use_library) identify_blockwise(processed) else NULL
        processed <- attach_best_matches(processed, matches)
        return(list(
          object = processed, matches = matches, pixel_matches = matches,
          pixel_to_unit = identity_pixel_mapping(processed, signal_eligible()),
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
      signal_subset <- if(all(signal_keep)) spatial else
        filter_spec(spatial, logic = signal_keep)

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
          identify_blockwise(processed_clusters)
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
          library_ids = colnames(library_filtered()$spectra),
          top_n = top_n_value()
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
        matches <- if(use_library) identify_blockwise(processed) else NULL
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
      pixel_matches <- identify_blockwise(processed_pixels)
      subset_mapping <- identity_pixel_mapping(signal_subset)
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
  observeEvent(input$file, canonical_error_key(NULL), ignoreInit = TRUE)

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
    if(identical(strategy, "collapse")) {
      retained <- unique(state$pixel_to_unit$unit_id[
        state$pixel_to_unit$kept & !is.na(state$pixel_to_unit$unit_id)
      ])
      return(tags$p(
        class = "text-muted",
        paste(length(retained), "connected particle regions retained.")
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
    by_unit <- mapping[kept & !is.na(unit_id), .(
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
      count <- ncol(DataR()$spectra)
      if(length(value) != 1L || is.na(value) || value < 1L ||
         value > count) return(NA_integer_)
      value
  })

  DataR_plot <- reactive({
      if(isTruthy(DataR())){
          selected <- selected_unit_index()
          if(is.na(selected)) return(app_rejected_spectrum(DataR()$wavenumber))
          filter_spec(DataR(), logic = seq_len(ncol(DataR()$spectra)) == selected)
       }
      else {
          list(wavenumber = numeric(), spectra = data.table(empty = numeric()))
      }
  })
  
  # SNR ----
  # Keep the metric control inert until thresholding is enabled. This lets a
  # user prepare the setting without invalidating the analysis pipeline.
  effective_signal_selection <- reactive({
      if(!isTRUE(input$threshold_decision)) return("run_sig_over_noise")
      input$signal_selection
  })

  quality_report_gate <- run_gated_reactive(function() {
      if(is.null(preprocessed$data)) return(NULL)
      selected <- DataR_plot()
      assessment <- tryCatch(
        assess_spec(
          selected,
          checks = app_quality_checks,
          report = "all",
          snr_metric = effective_signal_selection()
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
      selected_index <- selected_unit_index()
      safe_selected_value <- function(values) {
        if(is.na(selected_index) || is.null(values) ||
           selected_index > length(values)) return(NA_real_)
        as.numeric(values[[selected_index]])
      }
      threshold_report <- app_threshold_quality_report(
        spectrum_id = colnames(selected$spectra)[[1L]],
        snr_value = if(isTRUE(input$threshold_decision)) {
          safe_selected_value(canonical_signal_noise())
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
  quality_report <- reactive(quality_report_gate$read())

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

  observeEvent(list(quantified_data(), effective_signal_selection()), {
      req(isTruthy(quantified_data()))
      meta_cache(app_uploaded_metadata_cache(
        quantified_data(), canonical_signal_noise()
      ))
  })
  RawR_plot <- reactive({
      req(!is.null(preprocessed$data))
      uploaded <- data()
      selected <- if(isTRUE(canonical_state()$settings$collapse)) {
        data_click$pixel
      } else data_click$plot
      selected <- suppressWarnings(as.integer(selected))
      if(length(selected) != 1L || is.na(selected) ||
         selected < 1L || selected > ncol(uploaded$spectra)) {
        return(app_rejected_spectrum(uploaded$wavenumber))
      }
      filter_spec(
        uploaded,
        logic = seq_len(ncol(uploaded$spectra)) == selected
      )
  })
  
  identification_matches <- reactive({
    req(!is.null(preprocessed$data))
    req(!identical(input$lib_type, "model"))
    canonical_state()$matches
  })

  #The output from the AI classification algorithm.
  ai_output_gate <- run_gated_reactive(function() { #tested working.
      req(!is.null(preprocessed$data))
      req(isTRUE(input$identification_active))
      req(grepl("^model$", input$lib_type))
      analysis_phase(
        "Classifying spectra",
        paste0("Running the selected model for ", ncol(DataR()$spectra),
               " uploaded spectrum", if(ncol(DataR()$spectra) == 1L) "." else "s."),
        76
      )

      #rn <- runif(n = length(unique(libraryR()$all_variables)))
      mean <- rep.int(mean(unlist(DataR()$spectra)), times = length(unique(libraryR()$all_variables)))

      fill <- as_OpenSpecy(as.numeric(unique(libraryR()$all_variables)),
                           spectra = data.frame(mean))

      data <- conform_spec(DataR(), range = fill$wavenumber,
                           res = NULL)

      match_spec(data, library = libraryR(), na.rm = T, fill = fill)
  })
  ai_output <- reactive(ai_output_gate$read())

  # Best values are projected from the compact Top-N table; no full
  # library-by-spectrum matrix is created or retained.
  max_cor <- reactive({
      req(!is.null(preprocessed$data))
      if(identical(input$lib_type, "model")) {
        ai <- as.numeric(ai_output()[["value"]])
        names(ai) <- ai_output()[["name"]]
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
      identities <- if(!identical(input$lib_type, "model")) {
        metadata <- data.table::as.data.table(DataR()$metadata)
        if("material_class" %in% names(metadata)) {
          as.character(metadata$material_class)
        } else match_material(names(values))
      } else names(values)
      data.table::fifelse(
        is.na(values) | values < MinCor(),
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
      values <- if(correlation_active &&
                   !is.null(pixel_matches) && nrow(pixel_matches)) {
        best_match_rows(pixel_matches)$match_val
      } else max_cor()
      req(!is.null(values), length(values))
      thresholds <- if(correlation_active) MinCor() else numeric()
      app_particle_plotly(list(
        type = "histogram", values = as.numeric(values),
        thresholds = thresholds, xlab = "Correlation"
      ), source = "cor_histogram")
  })
  

  
  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
      req(!is.null(preprocessed$data))
      if(grepl("^model$", input$lib_type)){
          data.table(object_id = colnames(DataR()$spectra),
                     material_class = max_cor_identity(),
                     match_val = ai_output()$value)
      }
      else{
          selected <- selected_unit_index()
          if(is.na(selected)) {
            return(data.table::data.table(
              sample_name = character(), match_val = numeric()
            ))
          }
          selected_object_id <- colnames(DataR()$spectra)[selected]
          app_matches_for_object(
            identification_matches(), selected_object_id
          ) %>%
              dplyr::rename(sample_name = library_id) %>%
              left_join(library_filtered()$metadata, by = c("sample_name")) %>%
              mutate(match_val = signif(match_val, 2)) %>%
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
      req(!grepl("^model$", input$lib_type))

      # Get data from filter_spec
      rows <- matches_to_single()
      req(nrow(rows) > 0L)
      selected_row <- min(max(1L, as.integer(data_click$table)), nrow(rows))
      filter_spec(
        library_filtered(),
        logic = colnames(library_filtered()$spectra) ==
          rows[[selected_row, "sample_name"]]
      )
  })

  selected_match <- reactive({
      if(is.null(preprocessed$data) ||
         grepl("^model$", input$lib_type)) return(NULL)
      tryCatch(
        match_selected(),
        shiny.silent.error = function(e) NULL
      )
  })

  #All matches table for the current selection
  top_matches <- reactive({
      req(!is.null(preprocessed$data))
      req(!is.na(selected_unit_index()))
      app_top_matches_table(
        matches_to_single(), grepl("^model$", input$lib_type),
        selected_unit_index()
      )
  })

#Create the data table that goes below the plot which provides extra metadata.
match_metadata <- reactive({
    req(!is.null(preprocessed$data))
    selected_index <- selected_unit_index()
    if(is.na(selected_index)) {
      return(data.table::data.table(
        Selection = "The selected pixel does not belong to a retained particle."
      ))
    }
    model_library <- grepl("^model$", input$lib_type)
    if (!model_library) {
        rows <- matches_to_single()
        selected_row <- min(max(1L, as.integer(data_click$table)), nrow(rows))
        selected_match <- rows[selected_row, ]
        app_selected_metadata(
          quantified_data(), selected_match, canonical_signal_noise()
        )
    } else {
        result <- bind_cols(
          quantified_data()$metadata[selected_index,],
          matches_to_single()[selected_index,]
        )
        result$signal_to_noise <- canonical_signal_noise()[selected_index]
        result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
            mutate(match_val = signif(match_val, 2)) %>%
            select(file_name, col_id, material_class, match_val, signal_to_noise, everything())
        result
    }
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
      thresholds = thresholds, xlab = "Signal/Noise"
    ), source = "snr_histogram")
})

#Table of metadata for the selected spectrum and match
output$eventmetadata <- DT::renderDataTable(server = TRUE, {
    req(!is.null(match_metadata()))
    datatable(
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
})

# Create the data tables for all matches
output$event <- DT::renderDataTable({
    data <- top_matches()
    # AI mode's any_of()-selected row may be missing either column.
    if("organization" %in% names(data)) {
      data <- data %>% mutate(organization = as.factor(organization))
    }
    if("material_class" %in% names(data)) {
      data <- data %>% mutate(material_class = as.factor(material_class))
    }
    datatable(data,
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              rownames = FALSE,
              filter = "top", caption = "Selectable Matches",
              style = "bootstrap",
              selection = list(mode = "single", selected = c(1)))
})

#Full metadata table for uploaded spectra
output$sidebar_metadata <- DT::renderDataTable({
    req(!is.null(meta_cache()))
    app_uploaded_metadata_table(meta_cache())
}, server = FALSE)
outputOptions(output, "sidebar_metadata", suspendWhenHidden = FALSE)

  sidebar_proxy <- DT::dataTableProxy("sidebar_metadata")

  observeEvent(list(meta_cache(), data_click$plot), {
      req(!is.null(meta_cache()))
      row <- app_uploaded_metadata_row(meta_cache(), data_click$plot)
      if (length(row)) {
          DT::selectRows(sidebar_proxy, row)
      }
  }, ignoreInit = FALSE)

  pixel_projection_gate <- run_gated_reactive(function() {
    req(!is.null(preprocessed$data))
    spatial <- spatial_data()
    ids <- colnames(spatial$spectra)
    mapping <- canonical_state()$pixel_to_unit
    if(is.null(mapping)) mapping <- identity_pixel_mapping(spatial)
    mapping <- data.table::as.data.table(mapping)
    mapping <- mapping[match(ids, pixel_id)]

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
    } else if(!is.null(pixel_best) &&
       particle_pipeline_enabled() &&
       isTRUE(input$cor_threshold_decision)) {
      correlation <- pixel_best$match_val[pixel_best_index]
      match_id <- pixel_best$library_id[pixel_best_index]
      material <- match_material(match_id)
    } else if(is.null(state$object)) {
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

    list(
      metadata = data.table::as.data.table(spatial$metadata), mapping = mapping,
      signal_to_noise = signal, correlation = as.numeric(correlation),
      match_id = as.character(match_id), material = as.character(material),
      unit_id = mapping$unit_id, unit_index = mapping$unit_index,
      rejected = rejected,
      rejection_reason = reason
    )
  })
  pixel_projection <- reactive(pixel_projection_gate$read())

  map_color_choices <- reactive({
    req(ncol(preprocessed$data$spectra) > 1)
    state <- canonical_state()
    # Wait for the current dataset's first Run before offering any choice.
    # Rendering earlier (true the instant a map/batch is uploaded, before
    # Run) would default the selectize to whatever's available then --
    # usually just "Signal/Noise" -- and that premature value sticks even
    # once the full Material Class/Match ID/Match Value list exists.
    req(!is.null(state$object))
    identification_enabled <- !is.null(state$object) ||
      (!is.null(state$pixel_matches) && nrow(state$pixel_matches))
    choices <- c(
      if(identification_enabled) "Material Class" else NA_character_,
      if(identification_enabled && !identical(input$lib_type, "model"))
        "Match ID" else NA_character_,
      if(identification_enabled) "Match Value" else NA_character_,
      "Signal/Noise",
      if(isTRUE(state$settings$collapse)) "Particle Unit" else NA_character_
    )
    choices <- choices[!is.na(choices)]
    stats::setNames(choices, choices)
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
                )
            )
                )
})

output$progress_bars <- renderUI({
    req(!is.null(preprocessed$data))
    settings <- canonical_state()$settings
    req(ncol(preprocessed$data$spectra) > 1 || isTRUE(settings$collapse))

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
          return(app_empty_spectrum_plot() %>%
                   config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape")))
      }

      primary <- DataR_plot()
      raw <- RawR_plot()
      reference <- selected_match()
      app_spectrum_plot(
        active = primary,
        raw = raw,
        reference = reference,
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
      categorical <- FALSE
      z <- if(identical(map_color, "Particle Unit")) {
        categorical <- TRUE
        projection$unit_index
      } else if(identical(map_color, "Match ID")) {
        categorical <- TRUE
        projection$match_id
      } else if(identical(map_color, "Match Value")) {
        signif(projection$correlation, 2)
      } else if(identical(map_color, "Signal/Noise")) {
        signif(projection$signal_to_noise, 2)
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
        rejected = projection$rejected,
        rejection_reason = projection$rejection_reason
      )
  }

  heatmap_state <- reactive({
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
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
      app_ordinary_heatmap_data(
        state$metadata, state$z, state$categorical, map_color,
        rejected = state$rejected,
        rejection_reason = state$rejection_reason
      )
  }

  current_heatmap_data <- reactive({
      if(is.null(pixel_projection())) {
        return(list(
          type = "empty",
          reason = "A new dataset was uploaded. Click Run to analyze it."
        ))
      }
      heatmap_data_for(resolved_map_color())
  })

  # The currently selected point's data coordinates come from the uploaded
  # map metadata used throughout the in-memory analysis.
  current_select_xy <- reactive({
      req(!is.null(preprocessed$data))
      selected <- data_click$pixel
      metadata <- spatial_data()$metadata
      if(length(selected) != 1L || is.na(selected) || selected < 1L ||
         selected > nrow(metadata)) {
        mapping <- canonical_state()$pixel_to_unit
        if(!is.null(mapping)) {
          selected <- mapping$pixel_index[match(data_click$plot,
                                                mapping$unit_index)]
        }
      }
      if(length(selected) != 1L || is.na(selected)) return(NULL)
      list(x = metadata$x[[selected]], y = metadata$y[[selected]])
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
      unit_index == selected_plot & kept,
      pixel_index[[1L]]
    ]
    if(length(representative)) data_click$pixel <- representative
  }, ignoreNULL = TRUE)

  output$heatmapA <- plotly::renderPlotly({
      app_particle_plotly(current_heatmap_data(), source = "heat_plot",
                          select = isolate(current_select_xy()))
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
                 ncol(preprocessed$data$spectra) > 1
             ))
  })

  # Cheap selection sync: move only the marker trace instead of a full
  # heatmap redraw. Guarded on
  # a heatmap actually existing client-side yet, so this never races the
  # widget's own first creation (which already places the marker correctly
  # via output$heatmapA's own `select =` argument).
  observeEvent(current_select_xy(), {
      req(!is.null(isolate(current_heatmap_data())))
      select <- current_select_xy()
      x <- if(is.null(select)) NA_real_ else select$x
      y <- if(is.null(select)) NA_real_ else select$y
      plotly::plotlyProxy("heatmapA", session) %>%
        plotly::plotlyProxyInvoke(
          "restyle", list(x = list(list(x)), y = list(list(y))), list(2L)
        )
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(plotly::event_data("plotly_click", source = "heat_plot"), {
      click <- plotly::event_data("plotly_click", source = "heat_plot")
      curve_number <- if(length(click$curveNumber)) {
        suppressWarnings(as.integer(click$curveNumber[[1L]]))
      } else {
        0L
      }
      if(is.na(curve_number) || !curve_number %in% c(0L, 1L)) return()
      req(length(click$x), length(click$y))
      click_x <- click$x[[1L]]
      click_y <- click$y[[1L]]

      req(!is.null(preprocessed$data))
      selected <- nearest_metadata_row(spatial_data()$metadata, click_x,
                                       click_y)
      if(length(selected) && selected <= ncol(preprocessed$data$spectra)) {
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
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  #Summary Plots ----
  output$particle_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(canonical_state()$settings$collapse))
      particles <- canonical_final()
      req(particles$metadata$area)
      app_particle_size_plot(particles)
  })
  
  output$material_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      if(isTRUE(canonical_state()$settings$collapse)) {
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
        !is.null(state$object),
      collapse = isTRUE(state$settings$collapse) && !is.null(state$object)
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

  output$columns_selected_ui <- renderUI({
    req(identical(input$download_selection, "Top Matches"))
    req(!identical(input$lib_type, "model"))
    tags$details(
      class = "openspecy-download-details",
      tags$summary("Top Matches columns"),
      selectInput(
        inputId = "columns_selected", label = "Columns to save",
        choices = c("Simple", "All"), selected = "Simple"
      )
    )
  })
  outputOptions(output, "columns_selected_ui", suspendWhenHidden = FALSE)

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
      } else {
        ".csv"
      }
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
        your_spec <- quantified_data()
        your_spec$metadata$signal_to_noise <- canonical_signal_noise()
        write_spec(your_spec, file)
      } else if(identical(selection, "Top Matches")) {
        quant_columns <- app_ratio_metadata_columns(
          active_ratio_definitions(),
          active_measurement_definitions()
        )
        if(!grepl("^model$", input$lib_type)) {
          top_n <- input$top_n_input
          if(is.null(top_n) || !is.finite(top_n)) top_n <- 10L
          top_n <- min(top_n_value(), max(1L, as.integer(top_n)))
          columns_selected <- input$columns_selected
          if(is.null(columns_selected)) columns_selected <- "Simple"
          processed <- quantified_data()
          snr <- canonical_signal_noise()
          all_matches <- app_top_matches_export_compact(
            matches = identification_matches(),
            library_metadata = library_filtered()$metadata,
            spectrum_metadata = processed$metadata,
            signal_to_noise = snr,
            match_threshold = MinCor(),
            signal_threshold = c(MinSNR(), MaxSNR()),
            top_n = top_n,
            columns_selected = columns_selected,
            quant_columns = quant_columns
          )
          fwrite(all_matches, file)
        } else {
          result <- bind_cols(quantified_data()$metadata, matches_to_single())
          result$signal_to_noise <- canonical_signal_noise()
          keep <- !sapply(result, OpenSpecy::is_empty_vector) |
            names(result) %in% quant_columns
          result <- result[, keep, with = FALSE] %>%
            select(file_name, col_id, material_class, match_val,
                   signal_to_noise, everything()) %>%
            mutate(
              material_class = ifelse(match_val < MinCor(), "unknown",
                                      material_class)
            )
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
        if("details" %in% selected) {
          path <- file.path(archive_root, "particle_details.csv")
          fwrite(data.table::as.data.table(canonical_final()$metadata), path)
          files <- c(files, path)
        }
        if("processed" %in% selected) {
          path <- file.path(archive_root, "particles_processed.rds")
          saveRDS(canonical_final(), path)
          files <- c(files, path)
        }
        if("summary" %in% selected) {
          path <- file.path(archive_root, "particle_summary.csv")
          fwrite(app_particle_summary_table(canonical_final()), path)
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
            path <- file.path(archive_root, paste0(slug, "_heatmap.png"))
            app_write_ggplot_png(
              app_heatmap_ggplot(heatmap_data_for(map_name)), path,
              width = 8, height = 7
            )
            files <- c(files, path)
          }

          path <- file.path(archive_root, "particle_size_distribution.png")
          app_write_ggplot_png(app_particle_size_plot(canonical_final()), path)
          files <- c(files, path)
          if("material_class" %in% names(canonical_final()$metadata)) {
            material <- canonical_final()$metadata$material_class
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

  # Hide functions or objects when they shouldn't exist.

  observe({
      toggle(id = "placeholder1", condition = !isTruthy(preprocessed$data))
  })

  observe({
      if(!isTruthy(input$event_rows_selected)){
          data_click$table <- 1
      }
      else{
          data_click$table <- input$event_rows_selected
      }
  })

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
      mapping <- canonical_state()$pixel_to_unit
      if(!is.null(mapping)) {
        mapping <- data.table::as.data.table(mapping)
        representative <- mapping[unit_index == sel & kept, pixel_index[[1L]]]
        if(length(representative)) data_click$pixel <- representative[[1L]]
      }
      data_click$plot <- sel
  })


  move_selection <- function(dx = 0, dy = 0) {
      if(isTRUE(canonical_state()$settings$collapse)) {
        metadata <- data.table::as.data.table(spatial_data()$metadata)
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
      target <- paste(meta$x[[row]] + dx, meta$y[[row]] + dy)
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
      if (ncol(preprocessed$data$spectra) > 1) {
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

  current_file_info <- reactive(input$file)
  
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
