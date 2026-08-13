function(input, output, session) {
    
  #Setup ----
    options(shiny.maxRequestSize = app_upload_limit_bytes())
    
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
  final_specs <- reactiveVal(NULL)
  final_selection <- reactiveVal(NULL)
  filespec_index_state <- reactiveVal(NULL)
  filespec_selected_position <- reactiveVal(NULL)
  filespec_viewport_state <- reactiveVal(NULL)
  filespec_status_state <- reactiveVal(
    paste(
      "No file-backed source is open. This control is available only in",
      "the local R application."
    )
  )
  data_click <- reactiveValues(plot = NULL, table = NULL)
  heatmap_popover_info <- reactiveVal(NULL)
  meta_cache <- reactiveVal(NULL)
  correction_diagnostics <- reactiveVal(data.frame())
  ratio_definitions <- reactiveVal(app_empty_ratio_definitions())
  measurement_definitions <- reactiveVal(app_empty_measurement_definitions())
  quantification_axis <- reactiveVal(NULL)
  library_axis_cache <- new.env(parent = emptyenv())
  library_axis_cache$key <- NULL
  library_axis_cache$value <- NULL
  quality_modal_observers <- new.env(parent = emptyenv())

  # Advanced's own inputs stay editable while the master switch is off, same
  # as active_identification's children; every reader below already gates on
  # isTRUE(input$active_advanced) so an edit while off cannot invalidate
  # analysis or trigger recomputation.

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

  clear_filespec_state <- function(status = NULL) {
    final_specs(NULL)
    final_selection(NULL)
    filespec_index_state(NULL)
    filespec_selected_position(NULL)
    filespec_viewport_state(NULL)
    meta_cache(NULL)
    if(!is.null(status)) filespec_status_state(status)
    invisible(NULL)
  }

  load_filespec_selection <- function(position) {
    specs <- isolate(final_specs())
    index <- isolate(filespec_index_state())
    if(is.null(specs) || is.null(index)) return(invisible(FALSE))
    position <- suppressWarnings(as.integer(position))
    if(length(position) != 1L || is.na(position) || position < 1L ||
       position > nrow(index)) return(invisible(FALSE))

    row <- index[position, , drop = FALSE]
    analysis_phase(
      "Reading one file-backed spectrum",
      paste0(
        "Materializing pixel ", row$col_id[[1L]], " from ",
        row$region[[1L]], "; the remaining spectra stay on disk."
      ),
      16
    )
    selected <- tryCatch(
      OpenSpecy:::.filespec_read(specs, position),
      error = identity
    )
    if(inherits(selected, "error")) {
      show_alert(
        title = "Unable to read that pixel",
        text = conditionMessage(selected), type = "error"
      )
      return(invisible(FALSE))
    }
    check <- tryCatch(check_OpenSpecy(selected), error = identity,
                      warning = identity)
    if(inherits(check, "condition")) {
      show_alert(
        title = "The selected spectrum is invalid",
        text = conditionMessage(check), type = "error"
      )
      return(invisible(FALSE))
    }

    meta_cache(NULL)
    final_selection(selected)
    preprocessed$data <- selected
    filespec_selected_position(position)
    data_click$plot <- 1L
    data_click$table <- 1L
    correction_diagnostics(data.frame())
    quantification_axis(NULL)
    filespec_status_state(paste0(
      "Open read-only: ", basename(specs$source$members$path[[1L]]), ". ",
      format(nrow(index), big.mark = ","), " indexed spectra; selected ",
      row$col_id[[1L]], " in ", row$region[[1L]], "."
    ))
    invisible(TRUE)
  }

  output$filespec_status <- renderText(filespec_status_state())
  output$filespec_active <- reactive(!is.null(final_specs()))
  outputOptions(output, "filespec_status", suspendWhenHidden = FALSE)
  outputOptions(output, "filespec_active", suspendWhenHidden = FALSE)

  observeEvent(input$filespec_open, {
    req(app_local_file_mode())
    if(!isTRUE(input$active_advanced) || !isTRUE(input$collapse_decision)) {
      show_alert(
        title = "Enable Advanced particle analysis",
        text = paste(
          "Turn on the Advanced master switch and Collapse Particle Spectra",
          "before opening a local H5 or ENVI source."
        ),
        type = "warning"
      )
      return()
    }
    path <- trimws(as.character(input$filespec_path))
    if(length(path) != 1L || is.na(path) || !nzchar(path)) {
      show_alert(
        title = "Enter a source path",
        text = "Choose one existing H5 file or ENVI .hdr/.dat/.img member.",
        type = "warning"
      )
      return()
    }

    preprocessed$data <- NULL
    clear_filespec_state("Validating the requested source path...")
    correction_diagnostics(data.frame())
    ratio_definitions(app_empty_ratio_definitions())
    measurement_definitions(app_empty_measurement_definitions())
    quantification_axis(NULL)
    data_click$plot <- 1L
    data_click$table <- 1L
    shinyjs::reset("file")
    source_bytes <- suppressWarnings(file.info(path)$size)
    size_text <- if(length(source_bytes) && is.finite(source_bytes)) {
      paste0(" (", format(structure(source_bytes, class = "object_size"),
                           units = "auto"), ")")
    } else ""
    analysis_phase(
      "Indexing a large source",
      paste0(
        "Opening ", basename(path), size_text,
        " read-only and fingerprinting its H5/ENVI members."
      ),
      7
    )
    opened <- tryCatch(
      OpenSpecy::open_specs(path, cache_dir = app_filespec_cache_dir()),
      error = identity
    )
    if(inherits(opened, "error")) {
      clear_filespec_state(paste0("Open failed: ", conditionMessage(opened)))
      show_alert(
        title = "Unable to open the large source",
        text = conditionMessage(opened), type = "error"
      )
      return()
    }
    index <- tryCatch(OpenSpecy:::.filespec_index(opened), error = identity)
    if(inherits(index, "error") || !nrow(index)) {
      message <- if(inherits(index, "error")) conditionMessage(index) else
        "The source contains no indexed spectra."
      clear_filespec_state(paste0("Open failed: ", message))
      show_alert(title = "Unable to index the large source", text = message,
                 type = "error")
      return()
    }

    regions <- OpenSpecy:::.filespec_regions(opened)
    final_specs(opened)
    filespec_index_state(index)
    filespec_viewport_state(app_filespec_extent(index, regions[[1L]]))
    updateSelectInput(
      session, "filespec_region", choices = regions, selected = regions[[1L]]
    )
    updatePickerInput(
      session, "collapse_type", choices = "Mean", selected = "Mean"
    )
    updatePickerInput(
      session, "particle_id_strategy",
      choices = c("Connected threshold regions" = "collapse"),
      selected = "collapse"
    )
    analysis_phase(
      "Preparing the bounded map",
      paste0(
        "Indexed ", format(nrow(index), big.mark = ","), " spectra across ",
        length(regions), " region", if(length(regions) == 1L) "." else "s."
      ),
      12
    )
    first <- app_filespec_region_rows(index, regions[[1L]])[[1L]]
    load_filespec_selection(first)
  }, ignoreInit = TRUE)

  observeEvent(input$filespec_region, {
    index <- isolate(filespec_index_state())
    req(!is.null(index), isTruthy(input$filespec_region))
    filespec_viewport_state(
      app_filespec_extent(index, input$filespec_region)
    )
    rows <- app_filespec_region_rows(index, input$filespec_region)
    current <- isolate(filespec_selected_position())
    if(!length(current) || !current %in% rows) load_filespec_selection(rows[[1L]])
  }, ignoreInit = TRUE)

  filespec_preview <- reactive({
    index <- filespec_index_state()
    req(!is.null(index), isTruthy(input$filespec_region))
    viewport <- tryCatch(
      app_filespec_viewport(
        index, input$filespec_region, filespec_viewport_state()
      ),
      error = function(...) app_filespec_extent(index, input$filespec_region)
    )
    app_filespec_preview(
      index, input$filespec_region, roi = viewport
    )
  })

  output$filespec_view_status <- renderText({
    preview <- filespec_preview()
    bounds <- preview$viewport
    paste0(
      format(preview$spectra, big.mark = ","), " of ",
      format(preview$total_spectra, big.mark = ","), " pixels visible; X ",
      signif(bounds[["xmin"]], 6), " to ", signif(bounds[["xmax"]], 6),
      ", Y ", signif(bounds[["ymin"]], 6), " to ",
      signif(bounds[["ymax"]], 6), "."
    )
  })
  outputOptions(output, "filespec_view_status", suspendWhenHidden = FALSE)

  observeEvent(input$filespec_close, {
    preprocessed$data <- NULL
    clear_filespec_state(
      "Closed the file-backed source; source and completed caches were unchanged."
    )
    updatePickerInput(
      session, "collapse_type",
      choices = c("Mean", "Median", "Geometric Mean"), selected = "Mean"
    )
    updatePickerInput(
      session, "particle_id_strategy",
      choices = c(
        "Connected threshold regions" = "collapse",
        "Spectral clusters within regions" = "partial_collapse",
        "Non-spatial spectral clusters" = "nonspatial_collapse",
        "Per-cell identities" = "all_cell_id"
      ),
      selected = "collapse"
    )
  }, ignoreInit = TRUE)

  session$onSessionEnded(function() {
    clear_filespec_state()
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
  data_click$table <- 1
  preprocessed$data <- NULL
  clear_filespec_state(
    "Ordinary upload mode is active; no file-backed source is open."
  )
  correction_diagnostics(data.frame())
  ratio_definitions(app_empty_ratio_definitions())
  measurement_definitions(app_empty_measurement_definitions())
  quantification_axis(NULL)

  upload_size <- app_validate_upload_size(input$file, app_wasm_mode())
  if(!isTRUE(upload_size$ok)) {
    show_alert(
      title = "Upload is too large for ordinary mode",
      text = upload_size$message,
      type = "warning"
    )
    shinyjs::reset("file")
    return(NULL)
  }

  if (!all(grepl("(\\.tsv$)|(\\.h5$)|(\\.txt$)|(\\.img$)|(\\.dat$)|(\\.hdr$)|(\\.json$)|(\\.rds$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.dx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = T, as.character(input$file$datapath)))) {
    show_alert(
      title = "Data type not supported!",
      text = paste0("Uploaded data type is not currently supported; please
                      check the upload tooltip and package website for details."),
      type = "warning")
    return(NULL)
  }

  analysis_phase(
    "Reading uploaded spectra",
    paste0("Reading and validating ", nrow(input$file), " uploaded file",
           if(nrow(input$file) == 1L) "." else "s."),
    8
  )
      
      rout <- tryCatch(expr = {
          read_any(file = as.character(input$file$datapath)) |>
              c_spec(range = "common", res = if(input$conform_decision){input$conform_res} else{8}) |>
              manage_na(ig = c(NA, 0), type = "remove")},
          error = function(e){
              class(e$message) <- "simpleWarning"
              e$message
          }#,
          #warning = function(w){
          #class(w$message) <- "simpleWarning"
          #    w$message
          #}
      )
      #print(rout)
      
      if(!inherits(rout, "simpleWarning") && all(!grepl("(\\.hdr$)|(\\.dat$)|(\\.zip$)", input$file$datapath))){
          rout$metadata$file_name <- input$file$name
      }
      
      if(!inherits(rout, "simpleWarning")){
          checkit <- tryCatch(expr = {check_OpenSpecy(rout)},
                              error = function(e){
                                  class(e$message) <- "simpleWarning"
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
    if (inherits(rout, "simpleWarning") | inherits(checkit, "simpleWarning")) {
      show_alert(
        title = "Something went wrong with reading the data :-(",
        text =  paste0(if(inherits(rout, "simpleWarning")){paste0("There was an error during data loading that said ", 
                                                                  rout, ".")} else{""},
                       if(inherits(checkit, "simpleWarning")){paste0(" There was an error during data checking that said ", 
                                                                  checkit, ".")} else{""},
                       ". If you uploaded a text/csv file, make sure that the columns are numeric and named 'wavenumber' and 'intensity'."),
        type =  "error"
      )
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
        #print(preprocessed$data)
    }
})
  
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
      req(input$active_identification)
      library <- library_source()
      if(identical(input$lib_type, "model")) return(library)

      target_axis <- DataR()$wavenumber
      cache_key <- digest::digest(
        list(
          input$id_strategy, input$lib_type, input$id_spec_type,
          target_axis
        ),
        algo = "md5"
      )
      if(identical(library_axis_cache$key, cache_key) &&
         inherits(library_axis_cache$value, "OpenSpecy")) {
        return(library_axis_cache$value)
      }

      analysis_phase(
        "Preparing the reference library",
        paste0("Filtering ", format(ncol(library$spectra), big.mark = ","),
               " reference spectra to the final processed axis."),
        64
      )
      library <- conform_spec(
        library,
        range = target_axis,
        res = NULL,
        allow_na = TRUE,
        type = "roll"
      )
      if(!identical(library$wavenumber, target_axis)) {
        stop("The reference library did not conform to the displayed shared axis.",
             call. = FALSE)
      }
      keep_spectra <- !apply(library$spectra, 2, function(x) all(is.na(x)))
      library <- filter_spec(library, logic = keep_spectra)

      if(grepl("^ftir", input$id_spec_type)) {
        library <- filter_spec(
          library, logic = library$metadata$spectrum_type == "ftir"
        )
      } else if(grepl("^raman", input$id_spec_type)) {
        library <- filter_spec(
          library, logic = library$metadata$spectrum_type == "raman"
        )
      }
      library_axis_cache$key <- cache_key
      library_axis_cache$value <- library
      library
  })

  observeEvent(libraryR(), {
      req(input$active_identification)
      req(is.null(isolate(input$lib_org)))
      orgs <- sort(unique(libraryR()$metadata$organization))
      updatePickerInput(session, "lib_org", choices = orgs, selected = orgs)
  })
  

  library_filtered <- reactive({
      req(isTRUE(input$active_identification))
      library <- libraryR()
      library_type <- input$lib_type
      filter_enabled <- !identical(library_type, "model") &&
        isTRUE(input$filter_lib)
      if(!filter_enabled) return(library)

      filter_spec(
        library,
        logic = library$metadata$organization %in% input$lib_org
      )
  })
  # Corrects spectral intensity units using the user specified correction

 active_source <- reactive({
    selected <- final_selection()
    if(!is.null(selected)) selected else preprocessed$data
 })

 # Route ordinary uploads or the bounded FileSpecs selection into the same
 # established analysis pipeline.
 data <- reactive({
    req(!is.null(active_source()))
      da <- active_source()
      if(isTRUE(input$active_advanced) && isTruthy(input$xy_grid) &&
         (!all(diff(sort(da$metadata$y)) %in% c(0,1)) ||
          !all(diff(sort(da$metadata$x)) %in% c(0,1)))){
          grid <- gen_grid(nrow(da$metadata))
          da$metadata$x <- grid$x
          da$metadata$y <- grid$y
      }
          da
    })

  # Preprocess ----
  # Acquisition corrections run on the raw upload first, followed by ordinary
  # preprocessing and independent spatial smoothing. High-tail restriction and
  # CO2 flattening stay afterward so their automatic checks assess the final
  # processed signal.
  baseline_data <- reactive({
    req(!is.null(preprocessed$data))
    uploaded <- data()
    processed <- uploaded

    if(isTRUE(input$active_preprocessing)) {
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

      conform_enabled <- isTRUE(input$conform_decision)
      conform_args <- if(conform_enabled) {
        list(
          range = app_conform_axis(processed, input$conform_res),
          res = NULL,
          type = input$conform_selection
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

    if(isTRUE(input$active_advanced) && isTRUE(input$spatial_decision)) {
      analysis_phase(
        "Smoothing the spectral map",
        "Applying the selected spatial smoothing before artifact checks.",
        34
      )
      processed <- spatial_smooth(
        processed,
        sigma = c(input$sigma, input$sigma, input$sigma)
      )
    }

    diagnostics <- list()
    if(isTRUE(input$active_preprocessing) && isTRUE(input$co2_decision)) {
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

    if(isTRUE(input$active_preprocessing) && isTRUE(input$range_decision)) {
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

    app_attach_correction_metadata(processed)
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
      show_alert(
        title = "Process spectra first",
        text = paste(
          "Ratio bounds are set from the shared wavenumber axis of the",
          "displayed processed spectra."
        ),
        type = "warning"
      )
      return()
    }

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
        type = "warning"
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
      show_alert(
        title = "Process spectra first",
        text = paste(
          "Measurements use the shared wavenumber axis of the displayed",
          "processed spectra."
        ),
        type = "warning"
      )
      return()
    }
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
        type = "warning"
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
    if(!isTRUE(input$active_quantification)) {
      return(app_empty_ratio_definitions())
    }
    ratio_definitions()
  })

  active_measurement_definitions <- reactive({
    if(!isTRUE(input$active_quantification)) {
      return(app_empty_measurement_definitions())
    }
    measurement_definitions()
  })

  # Keep analysis spectra independent of ratio-only settings so changing a
  # definition cannot rerun matching or redraw spectral intensities.
  DataR <- reactive({
    req(!is.null(preprocessed$data))
    baseline_data()
  })

  quantified_data <- reactive({
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

  #The data to use in the plot. 
  DataR_plot <- reactive({
      if(isTruthy(DataR())){
          filter_spec(DataR(), logic = 1:ncol(DataR()$spectra) == data_click$plot)
       }
      else {
          list(wavenumber = numeric(), spectra = data.table(empty = numeric()))
      }
  })
  
  # SNR ----
  # Keep the metric control inert until thresholding is enabled. This lets a
  # user prepare the setting without invalidating the analysis pipeline.
  effective_signal_selection <- reactive({
      if(!isTRUE(input$active_advanced) ||
         !isTRUE(input$threshold_decision)) return("run_sig_over_noise")
      input$signal_selection
  })

  quality_report <- reactive({
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
      selected_index <- data_click$plot
      threshold_report <- app_threshold_quality_report(
        spectrum_id = colnames(selected$spectra)[[1L]],
        snr_value = if(isTRUE(input$active_advanced) &&
                       isTRUE(input$threshold_decision)) {
          signal_to_noise()[[selected_index]]
        } else NULL,
        snr_threshold = if(isTRUE(input$active_advanced) &&
                           isTRUE(input$threshold_decision)) {
          input$MinSNR
        } else NULL,
        signal_metric = effective_signal_selection(),
        correlation_value = if(isTRUE(input$active_advanced) &&
                               isTRUE(input$active_identification) &&
                               isTRUE(input$cor_threshold_decision)) {
          max_cor()[[selected_index]]
        } else NULL,
        correlation_threshold = if(isTRUE(input$active_advanced) &&
                                   isTRUE(input$active_identification) &&
                                   isTRUE(input$cor_threshold_decision)) {
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

  automatic_report <- reactive({
      app_automatic_report(
        x = if(is.null(preprocessed$data)) NULL else DataR(),
        diagnostics = correction_diagnostics(),
        enabled = c(
          spike = isTRUE(input$active_preprocessing) &&
            isTRUE(input$spike_decision),
          saturation = isTRUE(input$active_preprocessing) &&
            isTRUE(input$saturation_decision),
          flatten = isTRUE(input$active_preprocessing) &&
            isTRUE(input$co2_decision) && isTRUE(input$co2_automate),
          tails = isTRUE(input$active_preprocessing) &&
            isTRUE(input$range_decision) && isTRUE(input$range_automate)
        )
      )
  })
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

  #The signal to noise ratio
  signal_to_noise <- reactive({
      req(!is.null(preprocessed$data))
      sig_noise(
        x = DataR(), step = 10,
        metric = effective_signal_selection(), abs = FALSE
      )
  })

  observeEvent(list(quantified_data(), effective_signal_selection()), {
      req(isTruthy(quantified_data()))
      meta_cache(app_uploaded_metadata_cache(
        quantified_data(), signal_to_noise()
      ))
  })
  
  
  MinSNR <- reactive({
      req(!is.null(preprocessed$data))
      if(!isTRUE(input$active_advanced) || !input$threshold_decision){
          -Inf
      }
      else{
          input$MinSNR
      }
  })

  MaxSNR <- reactive({
      req(!is.null(preprocessed$data))
      if(!isTRUE(input$active_advanced) || !input$threshold_decision) {
          Inf
      } else {
          value <- suppressWarnings(as.numeric(input$MaxSNR))
          if(length(value) != 1L || is.na(value)) Inf else value
      }
  })

  particle_pipeline_enabled <- reactive({
    isTRUE(input$active_advanced) && isTRUE(input$collapse_decision) &&
      !is.null(preprocessed$data)
  })

  particle_collapse_function <- reactive({
    switch(
      input$collapse_type,
      "Median" = stats::median,
      "Geometric Mean" = function(x) exp(mean(log(x))),
      base::mean
    )
  })

  particle_process_args <- reactive({
    library <- library_filtered()
    if(is.null(final_specs())) {
      return(list(
        conform_spec = TRUE,
        conform_spec_args = list(range = library$wavenumber, res = NULL,
                                 type = "roll"),
        restrict_range = FALSE, flatten_range = FALSE,
        subtr_baseline = FALSE, smooth_intens = FALSE, make_rel = FALSE
      ))
    }

    active <- isTRUE(input$active_preprocessing)
    smooth_active <- active && isTRUE(input$smooth_decision)
    baseline_args <- if(identical(input$baseline_method, "fill_peaks")) {
      list(type = "fill_peaks", lambda = input$baseline_lambda,
           hwi = input$baseline_hwi, it = input$iterations,
           make_rel = FALSE)
    } else {
      list(type = "polynomial", degree = input$baseline, raw = FALSE,
           refit_at_end = input$refit, iterations = input$iterations,
           baseline = NULL, make_rel = FALSE)
    }
    list(
      active = TRUE,
      adj_intens = active && isTRUE(input$intensity_decision),
      adj_intens_args = list(type = input$intensity_corr),
      conform_spec = TRUE,
      conform_spec_args = list(range = library$wavenumber, res = NULL,
                               type = "roll"),
      restrict_range = active && isTRUE(input$range_decision),
      restrict_range_args = list(min = input$MinRange, max = input$MaxRange,
                                 make_rel = FALSE),
      flatten_range = active && isTRUE(input$co2_decision),
      flatten_range_args = list(min = input$MinFlat, max = input$MaxFlat,
                                make_rel = FALSE),
      subtr_baseline = active && isTRUE(input$baseline_decision),
      subtr_baseline_args = baseline_args,
      smooth_intens = smooth_active,
      smooth_intens_args = if(smooth_active) {
        list(
          polynomial = input$smoother,
          window = calc_window_points(library$wavenumber,
                                      input$smoother_window),
          derivative = input$derivative_order,
          abs = input$derivative_abs
        )
      } else {
        list()
      },
      make_rel = active && isTRUE(input$make_rel_decision)
    )
  })

  particle_analysis_state <- reactive({
    if(!particle_pipeline_enabled()) {
      return(list(result = NULL, output_dir = NULL, error = NULL))
    }
    if(!isTRUE(input$active_identification)) {
      return(list(
        result = NULL, output_dir = NULL,
        error = "Turn on Identification to match collapsed particle spectra."
      ))
    }
    if(identical(input$lib_type, "model")) {
      return(list(
        result = NULL, output_dir = NULL,
        error = paste(
          "Particle analysis requires a spectral reference library;",
          "choose a medoid or full library instead of the model."
        )
      ))
    }

    source <- if(!is.null(final_specs())) final_specs() else DataR()
    if(inherits(source, "FileSpecs") &&
       (!identical(input$particle_id_strategy, "collapse") ||
        !identical(input$collapse_type, "Mean"))) {
      return(list(
        result = NULL, output_dir = NULL,
        error = paste(
          "File-backed maps currently require Connected threshold regions",
          "and Mean collapse."
        )
      ))
    }
    settings_key <- digest::digest(list(
      source = if(inherits(source, "FileSpecs")) source$source$id else
        list(input$file$name, dim(source$spectra), source$wavenumber),
      input$particle_id_strategy, input$collapse_type,
      input$particle_area_threshold, effective_signal_selection(),
      MinSNR(), MaxSNR(), MinCor(), input$id_strategy, input$lib_type,
      particle_process_args()
    ))
    output_dir <- file.path(particle_output_root, settings_key)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    progress_for_message <- function(message) {
      if(grepl("complete", message, ignore.case = TRUE)) return(96)
      if(grepl("outputs", message, ignore.case = TRUE)) return(88)
      if(grepl("matching", message, ignore.case = TRUE)) return(76)
      if(grepl("mean|collapse", message, ignore.case = TRUE)) return(58)
      if(grepl("signal/noise", message, ignore.case = TRUE)) return(36)
      if(grepl("index|read|region", message, ignore.case = TRUE)) return(20)
      12
    }
    result <- tryCatch(
      withCallingHandlers(
        automate_particle_analysis(
          source,
          library = library_filtered(),
          output_dir = output_dir,
          material_col = "material_class",
          library_id_col = "sample_name",
          particle_id_strategy = input$particle_id_strategy,
          spectral_smooth = FALSE,
          sn_threshold_min = MinSNR(),
          sn_threshold_max = MaxSNR(),
          cor_threshold = MinCor(),
          area_threshold = input$particle_area_threshold,
          label_unknown = TRUE,
          pixel_length = 1,
          metric = effective_signal_selection(),
          abs = FALSE,
          collapse_function = particle_collapse_function(),
          outputs = unname(app_particle_output_choices()),
          process_args = particle_process_args(),
          origins = list(x = 0, y = 0)
        ),
        message = function(condition) {
          detail <- conditionMessage(condition)
          analysis_phase(
            "Analyzing particles", detail, progress_for_message(detail)
          )
          try(session$flushReact(), silent = TRUE)
          invokeRestart("muffleMessage")
        }
      ),
      error = identity
    )
    if(inherits(result, "error")) {
      return(list(result = NULL, output_dir = output_dir,
                  error = conditionMessage(result)))
    }
    list(result = result, output_dir = output_dir, error = NULL)
  })

  particle_analysis <- reactive(particle_analysis_state()$result)
  particle_output_dir <- reactive(particle_analysis_state()$output_dir)

  observe({
    if(particle_pipeline_enabled()) particle_analysis_state()
  })

  observeEvent(particle_analysis_state()$error, {
    error <- particle_analysis_state()$error
    if(is.null(error)) return()
    message("OpenSpecy app: particle analysis could not run: ", error)
    show_alert(title = "Particle analysis could not run", text = error,
               type = "error")
  }, ignoreNULL = TRUE)

  particle_sample_name <- reactive({
    result <- particle_analysis()
    req(!is.null(result), length(result$samples))
    requested <- if(!is.null(final_specs())) input$filespec_region else NULL
    if(isTruthy(requested) && requested %in% names(result$samples)) {
      requested
    } else {
      names(result$samples)[[1L]]
    }
  })

  particle_sample <- reactive({
    result <- particle_analysis()
    req(!is.null(result))
    result$samples[[particle_sample_name()]]
  })

  collapse_features <- reactive({
      sample <- particle_sample()
      processed <- sample$particles_rds
      if(is.null(processed) || !"feature_id" %in% names(processed$metadata)) {
        return(NULL)
      }
      processed$metadata$feature_id
  })


  #Warnings ----
  observe({
      if(is.null(preprocessed$data)) return()

      identification_enabled <- isTRUE(input$active_identification)
      if(identification_enabled) {
          strategy <- input$id_strategy
          if(identical(strategy, "deriv")) {
              invalid_derivative_setup <-
                !isTRUE(input$active_preprocessing) ||
                !isTRUE(input$smooth_decision) ||
                input$smoother != 3 ||
                input$derivative_order != 1 ||
                input$smoother_window != 90 ||
                !isTRUE(input$derivative_abs)
              if(invalid_derivative_setup) {
                  show_alert(
                    title = "Best practice not followed!",
                    text = paste0(
                      "If you are using the derivative library or model the typical best practice is to preprocess the spectra with ",
                      "Smoothing/Derivative turned on, the Polynomial set to 3, the Derivative Order set to 1, the Wavenumber Window set to 90 ",
                      "and the Absolute Value turned on because that is the way the library was created. You could be doing something special like uploading already processed spectra and if so feel free to ignore this warning."
                    ),
                    type = "warning"
                  )
              }
          }

          if(identical(strategy, "nobaseline")) {
              invalid_no_baseline_setup <-
                !isTRUE(input$active_preprocessing) ||
                !isTRUE(input$baseline_decision) ||
                (isTRUE(input$smooth_decision) &&
                 (input$derivative_order != 0 ||
                  isTRUE(input$derivative_abs)))
              if(invalid_no_baseline_setup) {
                  show_alert(
                    title = "Best practice not followed!",
                    text = paste0(
                      "If you are using the no baseline library or model the typical best practice is to preprocess the spectra with ",
                      "Baseline Correction turned on and setting Derivative Order to 0 and turning off Absolute Value if using Smoothing/Derivative. ",
                      "because that is the way the library was created. You could be doing something special like uploading already processed spectra and if so feel free to ignore this warning."
                    ),
                    type = "warning"
                  )
              }
          }

          if(identical(input$lib_type, "model") &&
             sum(preprocessed$data$wavenumber > 800 &
                 preprocessed$data$wavenumber < 3200) < 100) {
              show_alert(
                title = "Best practice not followed!",
                text = paste0(
                  "If you are using the multinomial model it assumes that your data encompasses the range between 800 and 3200 wavenumbers. It can work if your data partially encompasses that range ",
                  "but won't work at all if you have no data in that range. You won't get this warning if you have more than 100 values in that range but the less data in the range, the worse the model will perform."
                ),
                type = "warning"
              )
          }
      }

  })

  RawR_plot <- reactive({
      req(!is.null(preprocessed$data))
      if(!isTRUE(input$active_preprocessing)) return(NULL)
      uploaded <- data()
      filter_spec(
        uploaded,
        logic = seq_len(ncol(uploaded$spectra)) == data_click$plot
      )
  })
  
  #The correlation matrix between the unknowns and the library. 
  correlation <- reactive({
      req(!is.null(preprocessed$data))
      req(input$active_identification)
      req(!grepl("^model$", input$lib_type))
      reference <- library_filtered()
      analysis_phase(
        "Identifying spectra",
        paste0(
          "Comparing ", format(ncol(DataR()$spectra), big.mark = ","),
          " uploaded spectrum", if(ncol(DataR()$spectra) == 1L) "" else "s",
          " with ", format(ncol(reference$spectra), big.mark = ","),
          " reference spectra."
        ),
        76
      )
      cor_spec(x = DataR(),
               library = reference,
               conform = T,
               type = "roll")
  })

  #The output from the AI classification algorithm. 
  ai_output <- reactive({ #tested working. 
      req(!is.null(preprocessed$data))
      req(input$active_identification)
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
  
  #The maximum correlation or AI value. 
  max_cor <- reactive({
      req(!is.null(preprocessed$data))
      #req(input$active_identification)
      if(isTruthy(input$active_identification)){
          if(!grepl("^model$", input$lib_type)){
          max_cor_named(correlation())
        }
      else {
          ai <- signif(ai_output()[["value"]], 2)
          names(ai) <- ai_output()[["name"]]
          ai
        }
      }
      else{
          NULL
      }
  })
  
  #The maximum correlation or AI value. 
  max_cor_identity <- reactive({
      req(!is.null(preprocessed$data))
      if(isTruthy(input$active_identification)){
          if(!grepl("^model$", input$lib_type)){
              fifelse(max_cor() < MinCor(), rep.int("unknown", length(max_cor())), library_filtered()$metadata$material_class[match(names(max_cor()), library_filtered()$metadata$sample_name)])
          }
          else{
              fifelse(max_cor() < MinCor(), rep.int("unknown", length(max_cor())), names(max_cor()))
          }
      }
      else{
          NULL
      }
  })
  
  MinCor <- reactive({
      req(!is.null(preprocessed$data))
      if(!isTRUE(input$active_advanced) || !input$cor_threshold_decision){
          -Inf
      }
      else{
          input$MinCor
      }
  })
  
  output$cor_plot_ui <- renderUI({
      if(!is.null(particle_analysis())) {
        imageOutput("cor_plot_image", height = "16vh")
      } else {
        plotOutput("cor_plot", height = "16vh")
      }
  })

  output$cor_plot_image <- renderImage({
      req(!is.null(particle_analysis()))
      path <- file.path(
        particle_output_dir(),
        paste0("cor_histogram_", particle_sample_name(), ".png")
      )
      req(file.exists(path))
      list(src = path, contentType = "image/png", alt =
             "Maximum-correlation distribution with the current threshold")
  }, deleteFile = FALSE)

  output$cor_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(input$active_advanced))
      req(isTRUE(input$active_identification))
      req(isTRUE(input$cor_threshold_decision))
      req(is.null(particle_analysis()))
      ggplot() +
          geom_histogram(aes(x = max_cor()), fill = app_plot_palette$primary,
                         color = app_plot_palette$panel) +
          scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
          geom_vline(xintercept = MinCor(), color = app_plot_palette$reference,
                     linewidth = 0.8) +
          theme_black_minimal() +
          labs(x = "Correlation")
  })
  

  
  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
      req(!is.null(preprocessed$data))
      req(input$active_identification)
      if(grepl("^model$", input$lib_type)){
          data.table(object_id = colnames(DataR()$spectra),
                     material_class = max_cor_identity(),
                     match_val = ai_output()$value)
      }
      else{
          data.table(object_id = colnames(DataR()$spectra)[data_click$plot],
                     sample_name = colnames(library_filtered()$spectra),
                     match_val = c(correlation()[,data_click$plot]))[order(-match_val),] %>%
              left_join(library_filtered()$metadata, by = c("sample_name")) %>%
              mutate(match_val = signif(match_val, 2)) %>%
              {if(input$cor_threshold_decision){mutate(., name = ifelse(match_val < input$MinCor, rep.int("Unknown", nrow(.)), material_class))}else{.}}

      }
  })

  #Spectral data for the selected match. 
  match_selected <- reactive({# Default to first row if not yet clicked
      if(!isTRUE(input$active_identification)) {
          return(as_OpenSpecy(
            x = numeric(), spectra = data.table(empty = numeric())
          ))
      }
      req(!grepl("^model$", input$lib_type))

      # Get data from filter_spec
      filter_spec(
        library_filtered(),
        logic = matches_to_single()[[data_click$table, "sample_name"]]
      )
  })

  selected_match <- reactive({
      if(is.null(preprocessed$data) || !isTRUE(input$active_identification) ||
         grepl("^model$", input$lib_type)) return(NULL)
      tryCatch(
        match_selected(),
        shiny.silent.error = function(e) NULL
      )
  })

  #All matches table for the current selection
  top_matches <- reactive({
      req(!is.null(preprocessed$data))
      req(input$active_identification)
      req(!grepl("^model$", input$lib_type))
      matches_to_single() %>%
          dplyr::select("match_val", "material_class", "spectrum_identity",
                        "organization", "sample_name")
  })

#Create the data table that goes below the plot which provides extra metadata.
match_metadata <- reactive({
    req(!is.null(preprocessed$data))
    identification_enabled <- isTRUE(input$active_identification)
    if(!identification_enabled) {
        return(
          quantified_data()$metadata[data_click$plot,] %>%
            .[, !sapply(., OpenSpecy::is_empty_vector), with = FALSE]
        )
    }

    model_library <- grepl("^model$", input$lib_type)
    if (!model_library) {
        selected_match <- matches_to_single()[data_click$table, ]
        app_selected_metadata(
          quantified_data(), selected_match, signal_to_noise()
        )
    } else {
        result <- bind_cols(
          quantified_data()$metadata[data_click$plot,],
          matches_to_single()[data_click$plot,]
        )
        result$signal_to_noise <- signal_to_noise()[data_click$plot]
        result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
            mutate(match_val = signif(match_val, 2)) %>%
            select(file_name, col_id, material_class, match_val, signal_to_noise, everything())
        result
    }
})

# Display ----

#Histogram of SNR
output$snr_plot_ui <- renderUI({
    if(!is.null(particle_analysis())) {
      imageOutput("snr_plot_image", height = "16vh")
    } else {
      plotOutput("snr_plot", height = "16vh")
    }
})

output$snr_plot_image <- renderImage({
    req(!is.null(particle_analysis()))
    path <- file.path(
      particle_output_dir(),
      paste0("sn_histogram_", particle_sample_name(), ".png")
    )
    req(file.exists(path))
    list(src = path, contentType = "image/png", alt =
           "Signal/noise distribution with the current minimum and maximum")
}, deleteFile = FALSE)

output$snr_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    req(isTRUE(input$active_advanced))
    req(isTRUE(input$threshold_decision))
    req(is.null(particle_analysis()))
    ggplot() +
        geom_histogram(aes(x = signal_to_noise()),
                       fill = app_plot_palette$primary,
                       color = app_plot_palette$panel) +
        scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
        geom_vline(xintercept = MinSNR(), color = app_plot_palette$reference,
                   linewidth = 0.8) +
        geom_vline(xintercept = MaxSNR(), color = app_plot_palette$reference,
                   linewidth = 0.8) +
        theme_black_minimal() +
        labs(x = "Signal/Noise")
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
    req(input$active_identification)
    req(!grepl("^model$", input$lib_type))
    datatable(top_matches() %>%
                  mutate(organization = as.factor(organization),
                         material_class = as.factor(material_class)),
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

  map_color_choices <- reactive({
    if(!is.null(particle_analysis())) {
      return(c(
        "Particle Image" = "particle_image",
        "Thresholded Particles" = "particle_heatmap_thresholded",
        "Signal / Noise" = "particle_heatmap",
        "Correlation" = "cor_heatmap"
      ))
    }
    if(!is.null(final_specs())) {
      return(c("Source Index" = "filespec_preview"))
    }
    req(ncol(preprocessed$data$spectra) > 1)
    identification_enabled <- isTRUE(input$active_identification)
    if(identification_enabled) req(!is.null(max_cor()))
    choice_names <- c(
      if(identification_enabled) "Match Name" else NA_character_,
      if(identification_enabled && !identical(input$lib_type, "model"))
        "Match ID" else NA_character_,
      if(identification_enabled && !is.null(max_cor())) "Match Value" else
        NA_character_,
      if(!is.null(signal_to_noise())) "Signal/Noise" else NA_character_
    )
    choice_names <- choice_names[!is.na(choice_names)]
    stats::setNames(choice_names, choice_names)
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
            )
            )
                )
})

output$progress_bars <- renderUI({
    req(!is.null(preprocessed$data))
    req(ncol(preprocessed$data$spectra) > 1 || particle_pipeline_enabled())

    percent_true <- function(x) {
      available <- !is.na(x)
      if(!any(available)) return(0)
      sum(x[available]) / sum(available) * 100
    }

    signal_values <- if(isTRUE(input$active_advanced) &&
                        isTRUE(input$threshold_decision)) {
      signal_to_noise()
    } else {
      NULL
    }
    correlation_values <- if(isTRUE(input$active_advanced) &&
                              isTRUE(input$active_identification) &&
                              isTRUE(input$cor_threshold_decision)) {
      max_cor()
    } else {
      NULL
    }

    metric_items <- list()
    if(!is.null(signal_values)) {
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "signal_summary_panel",
        shinyWidgets::progressBar(
          id = "signal_progress",
          value = percent_true(
            signal_values > MinSNR() & signal_values < MaxSNR()
          ),
          status = "success",
          title = "Good Signal",
          display_pct = TRUE
        )
      )
    }
    if(!is.null(correlation_values)) {
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "correlation_summary_panel",
        shinyWidgets::progressBar(
          id = "correlation_progress",
          value = percent_true(correlation_values > MinCor()),
          status = "success",
          title = "Good Match Values",
          display_pct = TRUE
        )
      )
    }
    if(!is.null(signal_values) && !is.null(correlation_values)) {
      metric_items[[length(metric_items) + 1L]] <- div(
        id = "match_summary_panel",
        shinyWidgets::progressBar(
          id = "match_progress",
          value = percent_true(
            signal_values > MinSNR() & signal_values < MaxSNR() &
              correlation_values > MinCor()
          ),
          status = "success",
          title = "Good Identifications",
          display_pct = TRUE
        )
      )
    }

    plot_items <- list()
    if(particle_pipeline_enabled() && !is.null(particle_analysis())) {
      plot_items[[length(plot_items) + 1L]] <- div(
        id = "particle_summary_panel",
        plotOutput("particle_plot", height = "25vh")
      )
    }
    if(isTRUE(input$active_identification)) {
      plot_items[[length(plot_items) + 1L]] <- div(
        id = "material_summary_panel",
        plotOutput("material_plot", height = "25vh")
      )
    }

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
      reference <- if(isTRUE(input$active_identification)) {
        selected_match()
      } else {
        NULL
      }
      app_spectrum_plot(
        active = primary,
        raw = raw,
        reference = reference,
        make_rel = isTRUE(input$active_preprocessing) &&
          isTRUE(input$make_rel_decision),
        source = "B",
        plot_width = session$clientData$output_MyPlotC_width
      ) %>%
        app_style_plotly() %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })

 #Heatmap ----
 #Display the map or batch data in a selectable heatmap.
  match_name_palette <- reactive({
      if(!isTRUE(input$active_identification)) {
        return(app_category_palette(character()))
      }
      if(particle_pipeline_enabled()) {
        particles <- thresholded_particles()
        if(!is.null(particles) &&
           "material_class" %in% names(particles$metadata)) {
          return(app_category_palette(particles$metadata$material_class))
        }
      }
      app_category_palette(max_cor_identity())
  })

  heatmap_state <- reactive({
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
      test <- DataR()

      map_color <- resolved_map_color()
      categorical <- FALSE
      legend_title <- map_color
      z <- if(!is.null(max_cor()) && identical(map_color, "Match ID")) {
        categorical <- TRUE
        names(max_cor())
      } else if(!is.null(max_cor()) && identical(map_color, "Match Value")) {
        signif(max_cor(), 2)
      } else if(!is.null(signal_to_noise()) &&
                identical(map_color, "Signal/Noise")) {
        signif(signal_to_noise(), 2)
      } else if(!is.null(max_cor()) && identical(map_color, "Match Name")) {
        categorical <- TRUE
        max_cor_identity()
      } else {
        validate(need(FALSE, "The selected map color is not available."))
      }
      if(categorical) {
        z <- factor(
          as.character(z),
          levels = sort(unique(as.character(z[!is.na(z)])))
        )
        keep <- rep(TRUE, length(z))
        sn_values <- signal_to_noise()
        cor_values <- max_cor()
        if(!is.null(sn_values)) keep <- keep & sn_values > MinSNR()
        if(!is.null(cor_values)) keep <- keep & cor_values > MinCor()
        keep[is.na(keep)] <- FALSE
        z[!keep] <- NA
      }
      all_categorical_masked <- categorical && all(is.na(z))
      list(
        data = test,
        z = z,
        categorical = categorical,
        legend_title = if(isTruthy(legend_title)) legend_title else "Value",
        colorscale = if(all_categorical_masked) {
          list(c(0, app_theme$muted), c(1, app_theme$muted))
        } else if(categorical) {
          app_category_colorscale(z)
        } else {
          app_heatmap_colorscale
        }
      )
  })

  output$heatmapB <- plotly::renderPlotly({
      result <- particle_analysis()
      req(!is.null(result))
      sample <- particle_sample()
      app_particle_plotly(sample[[resolved_map_color()]], source = "heat_plot")
  })

  observe({
      show_particle <- !is.null(particle_analysis())
      toggle(id = "heatmapA", condition = !show_particle)
      toggle(id = "heatmapB", condition = show_particle)
  })

  output$heatmapA <- renderPlot({
      if(!is.null(final_specs())) {
        preview <- filespec_preview()
        index <- filespec_index_state()
        position <- filespec_selected_position()
        selected <- NULL
        if(length(position) == 1L && !is.na(position) &&
           position >= 1L && position <= nrow(index)) {
          coordinates <- app_filespec_coordinates(index[position, , drop = FALSE])
          selected <- list(x = coordinates$x[[1L]], y = coordinates$y[[1L]])
        }
        graphics::par(bg = app_theme$canvas, fg = app_theme$text,
                      mar = c(4.5, 5, 3.2, 1))
        app_draw_filespec_preview(preview, selected)
        return(invisible(NULL))
      }
      state <- heatmap_state()
      app_draw_server_heatmap(
        state$data$metadata,
        state$z,
        categorical = state$categorical,
        title = state$legend_title,
        selected = isolate(data_click$plot)
      )
  }, res = 110)

  thresholded_particles <- reactive({
      req(particle_pipeline_enabled())
      particle_sample()$particles_rds
  })
  
  #Summary Plots ----
  output$particle_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(particle_pipeline_enabled())
      req(thresholded_particles()$metadata$area)
      ggplot() +
          geom_histogram(aes(x = sqrt(thresholded_particles()$metadata$area)),
                         fill = app_plot_palette$primary,
                         color = app_plot_palette$panel) +
          theme_black_minimal(base_size = 15) +
          labs(x = "Nominal Particle Size (√area)", y = "Count")
  })
  
  output$material_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(input$active_identification))
      if(particle_pipeline_enabled()) {
          particles <- thresholded_particles()
          req(!is.null(particles),
              "material_class" %in% names(particles$metadata))
          match_names <- particles$metadata$material_class
      } else {
          req(max_cor_identity())
          match_names <- max_cor_identity()
      }

      ggplot() +
          geom_bar(aes(y = match_names, fill = match_names)) +
          scale_fill_manual(
            values = match_name_palette(),
            na.value = app_theme$muted,
            drop = FALSE
          ) +
          theme_black_minimal(base_size = 15) +
          theme(legend.position = "none") +
          labs(x = "Count", y = "Material Class")
  })

  
  # Data Download options ----
  # Progress Bars
  output$download_ui <- renderUI({
    choice_names <- app_download_choices(
      has_upload = !is.null(preprocessed$data),
      identification = !is.null(preprocessed$data) &&
        isTRUE(input$active_identification),
      collapse = particle_pipeline_enabled() && !is.null(particle_analysis())
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
  observeEvent(particle_analysis(), {
    req(particle_pipeline_enabled(), !is.null(particle_analysis()))
    updateSelectInput(session, "download_selection",
                      selected = "Thresholded Particles")
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
  
  output$top_n <- renderUI({
      req(ncol(preprocessed$data$spectra) >= 1)
      req(input$active_identification)
      req(input$download_selection == "Top Matches")
      req(!grepl("^model$", input$lib_type))
      tags$details(
        class = "openspecy-download-details",
        tags$summary("Top Match options"),
        numericInput(
          "top_n_input",
          "Top N",
          value = 1,
          min = 1,
          max = ncol(library_filtered()$spectra),
          step = 1
        ),
        selectInput(
          inputId = "columns_selected",
          label = "Columns to save",
          choices = c("Simple", "All")
        )
      )
  })
  outputOptions(output, "top_n", suspendWhenHidden = FALSE)

  output$particle_download_contents <- renderUI({
    req(identical(input$download_selection, "Thresholded Particles"))
    choices <- app_particle_output_choices()
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
        your_spec$metadata$signal_to_noise <- signal_to_noise()
        write_spec(your_spec, file)
      } else if(identical(selection, "Top Matches")) {
        quant_columns <- app_ratio_metadata_columns(
          active_ratio_definitions(),
          active_measurement_definitions()
        )
        if(!grepl("^model$", input$lib_type)) {
          top_n <- input$top_n_input
          if(is.null(top_n) || !is.finite(top_n)) top_n <- 1L
          top_n <- max(1L, as.integer(top_n))
          columns_selected <- input$columns_selected
          if(is.null(columns_selected)) columns_selected <- "Simple"
          processed <- quantified_data()
          snr <- signal_to_noise()
          all_matches <- app_top_matches_export(
            cor_matrix = correlation(),
            library_metadata = library_filtered()$metadata,
            spectrum_metadata = processed$metadata,
            signal_to_noise = snr,
            match_threshold = MinCor(),
            signal_threshold = MinSNR(),
            top_n = top_n,
            columns_selected = columns_selected,
            quant_columns = quant_columns
          )
          fwrite(all_matches, file)
        } else {
          result <- bind_cols(quantified_data()$metadata, matches_to_single())
          result$signal_to_noise <- signal_to_noise()
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
        if(is.null(selected)) selected <- unname(app_particle_output_choices())
        files <- app_particle_output_files(particle_output_dir(), selected)
        if(!length(files)) {
          stop("No completed particle-analysis files match the selected contents.")
        }
        zip_file <- tempfile("openspecy-particles-", fileext = ".zip")
        on.exit(unlink(zip_file, force = TRUE), add = TRUE)
        app_write_particle_archive(
          files, destination = zip_file, root = particle_output_dir()
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
      toggle(id = "heatmap_frame",
             condition = isTruthy(
               !is.null(final_specs()) || !is.null(particle_analysis()) ||
                 (!is.null(preprocessed$data) &&
                    ncol(preprocessed$data$spectra) > 1)
             ))
      toggle(id = "placeholder1", condition = !isTruthy(preprocessed$data))
  })

  nearest_metadata_row <- function(metadata, x, y) {
    if(is.null(metadata) || !nrow(metadata) ||
       !all(c("x", "y") %in% names(metadata))) return(integer())
    dx <- suppressWarnings(as.numeric(metadata$x) - as.numeric(x))
    dy <- suppressWarnings(as.numeric(metadata$y) - as.numeric(y))
    distance <- dx^2 + dy^2
    distance[!is.finite(distance)] <- Inf
    if(all(is.infinite(distance))) integer() else which.min(distance)
  }

  # Small popover docked in the plot viewer: x, y, and the currently
  # displayed z-value/label only, instead of a full-metadata modal.
  output$heatmap_popover <- renderUI({
      info <- heatmap_popover_info()
      req(!is.null(info))
      rows <- list(
        tags$tr(tags$th("x"), tags$td(format(signif(info$x, 4)))),
        tags$tr(tags$th("y"), tags$td(format(signif(info$y, 4))))
      )
      if(!is.null(info$z)) {
        rows <- c(rows, list(tags$tr(
          tags$th(if(isTruthy(info$label)) info$label else "z"),
          tags$td(as.character(info$z))
        )))
      }
      div(class = "openspecy-heatmap-popover",
          tags$table(tags$tbody(rows)))
  })
  outputOptions(output, "heatmap_popover", suspendWhenHidden = FALSE)

  observeEvent(input$heatmap_click, {
      click <- input$heatmap_click
      req(length(click$x), length(click$y))
      click_x <- click$x[[1L]]
      click_y <- click$y[[1L]]

      if(!is.null(final_specs())) {
        index <- isolate(filespec_index_state())
        region <- isolate(input$filespec_region)
        selected <- app_filespec_nearest_position(
          index, region, click_x, click_y,
          roi = isolate(filespec_viewport_state())
        )
        if(length(selected)) {
          load_filespec_selection(selected)
          row <- index[selected, , drop = FALSE]
          heatmap_popover_info(list(x = row$x[[1L]], y = row$y[[1L]],
                                    z = NULL, label = NULL))
        }
        return()
      }

      state <- isolate(heatmap_state())
      req(!is.null(state$data), nrow(state$data$metadata))
      selected <- nearest_metadata_row(state$data$metadata, click_x, click_y)
      if(length(selected)) {
        if(selected <= ncol(preprocessed$data$spectra)) {
          data_click$plot <- selected
        }
        z_value <- state$z[[selected]]
        heatmap_popover_info(list(
          x = state$data$metadata$x[[selected]],
          y = state$data$metadata$y[[selected]],
          z = if(is.numeric(z_value)) signif(z_value, 3) else z_value,
          label = state$legend_title
        ))
      }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(plotly::event_data("plotly_click", source = "heat_plot"), {
      click <- plotly::event_data("plotly_click", source = "heat_plot")
      req(length(click$x), length(click$y))
      click_x <- click$x[[1L]]
      click_y <- click$y[[1L]]
      click_z <- if(length(click$z)) click$z[[1L]] else NA

      result <- isolate(particle_analysis())
      req(!is.null(result))
      sample <- isolate(particle_sample())
      field <- isolate(resolved_map_color())
      data <- sample[[field]]
      label <- if(isTruthy(data$legend_title)) data$legend_title else field
      z_display <- if(identical(data$type, "heatmap_binary") &&
                      is.finite(click_z)) {
        data$labels[[round(click_z)]]
      } else if(identical(data$type, "heatmap_categorical") &&
               is.finite(click_z)) {
        data$levels[[round(click_z)]]
      } else if(is.numeric(click_z)) {
        signif(click_z, 3)
      } else {
        NULL
      }
      heatmap_popover_info(list(x = click_x, y = click_y, z = z_display,
                                label = label))

      source_map <- if(inherits(sample$particles_raw_rds, "OpenSpecy")) {
        sample$particles_raw_rds
      } else {
        sample$particles_rds
      }
      req(!is.null(source_map), nrow(source_map$metadata))
      selected <- nearest_metadata_row(source_map$metadata, click_x, click_y)
      req(length(selected))
      if(!is.null(isolate(final_specs()))) {
        index <- isolate(filespec_index_state())
        region <- isolate(input$filespec_region)
        rows <- app_filespec_region_rows(index, region)
        local_row <- nearest_metadata_row(index[rows, , drop = FALSE],
                                          click_x, click_y)
        if(length(local_row)) load_filespec_selection(rows[[local_row]])
      } else if(selected <= ncol(isolate(preprocessed$data$spectra))) {
        data_click$plot <- selected
      }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$heatmap_brush, {
    req(!is.null(final_specs()), is.null(isolate(particle_analysis())))
    brush <- input$heatmap_brush
    index <- isolate(filespec_index_state())
    region <- isolate(input$filespec_region)
    candidate <- tryCatch(
      app_filespec_viewport(
        index, region, c(brush$xmin, brush$xmax, brush$ymin, brush$ymax)
      ),
      error = identity
    )
    if(!inherits(candidate, "error")) filespec_viewport_state(candidate)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observe({
      if(!isTruthy(input$event_rows_selected)){
          data_click$table <- 1
      }
      else{
          data_click$table <- input$event_rows_selected
      }
  })

  observeEvent(input$sidebar_metadata_rows_selected, ignoreInit = TRUE, {
      req(!is.null(meta_cache()))
      sel <- app_uploaded_metadata_spectrum(
        meta_cache(), input$sidebar_metadata_rows_selected
      )
      if (length(sel) && !identical(sel, as.integer(data_click$plot))) {
          data_click$plot <- sel
      }
  })


  move_selection <- function(dx = 0, dy = 0) {
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

  current_file_info <- reactive({
    specs <- final_specs()
    if(is.null(specs)) return(input$file)
    members <- specs$source$members
    data.frame(
      name = paste(basename(members$path), collapse = " + "),
      size = sum(as.numeric(members$size)),
      type = paste0("FileSpecs/", toupper(specs$source$backend)),
      lastModified = format(
        as.POSIXct(max(as.numeric(members$mtime)), origin = "1970-01-01"),
        "%Y-%m-%d %H:%M:%S %z"
      ),
      stringsAsFactors = FALSE
    )
  })
  
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
      source = active_source(),
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

