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
  meta_cache <- reactiveVal(NULL)
  correction_diagnostics <- reactiveVal(data.frame())
  ratio_definitions <- reactiveVal(app_empty_ratio_definitions())
  measurement_definitions <- reactiveVal(app_empty_measurement_definitions())
  quantification_axis <- reactiveVal(NULL)
  library_axis_cache <- new.env(parent = emptyenv())
  library_axis_cache$key <- NULL
  library_axis_cache$value <- NULL
  quality_modal_observers <- new.env(parent = emptyenv())

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

  output$filespec_map <- renderPlot({
    preview <- filespec_preview()
    index <- filespec_index_state()
    position <- filespec_selected_position()
    selected <- NULL
    if(length(position) == 1L && !is.na(position) &&
       position >= 1L && position <= nrow(index) &&
       identical(as.character(index$region[[position]]), preview$region)) {
      coordinates <- app_filespec_coordinates(index[position, , drop = FALSE])
      selected <- list(x = coordinates$x[[1L]], y = coordinates$y[[1L]])
    }
    graphics::par(bg = app_theme$canvas, fg = app_theme$text,
                  mar = c(4.5, 5, 3.2, 1))
    app_draw_filespec_preview(preview, selected)
  }, res = 96)

  observeEvent(input$filespec_map_click, {
    click <- input$filespec_map_click
    index <- isolate(filespec_index_state())
    region <- isolate(input$filespec_region)
    req(!is.null(index), isTruthy(region), length(click$x), length(click$y))
    position <- app_filespec_nearest_position(
      index, region, click$x, click$y,
      roi = isolate(filespec_viewport_state())
    )
    if(length(position)) load_filespec_selection(position)
  }, ignoreInit = TRUE)

  observeEvent(input$filespec_map_brush, {
    brush <- input$filespec_map_brush
    index <- isolate(filespec_index_state())
    region <- isolate(input$filespec_region)
    req(!is.null(index), isTruthy(region), !is.null(brush))
    candidate <- tryCatch(
      app_filespec_viewport(
        index, region,
        c(brush$xmin, brush$xmax, brush$ymin, brush$ymax)
      ),
      error = identity
    )
    if(inherits(candidate, "error")) return()
    visible <- tryCatch(
      app_filespec_preview(index, region, roi = candidate,
                           max_width = 2L, max_height = 2L),
      error = identity
    )
    if(!inherits(visible, "error")) filespec_viewport_state(candidate)
  }, ignoreInit = TRUE)

  shift_filespec_view <- function(direction = "reset") {
    index <- isolate(filespec_index_state())
    region <- isolate(input$filespec_region)
    req(!is.null(index), isTruthy(region))
    full <- app_filespec_extent(index, region)
    current <- isolate(filespec_viewport_state())
    if(is.null(current) || identical(direction, "reset")) {
      filespec_viewport_state(full)
      return(invisible(NULL))
    }
    current <- app_filespec_viewport(index, region, current)
    width <- diff(current[c("xmin", "xmax")])
    height <- diff(current[c("ymin", "ymax")])
    center_x <- mean(current[c("xmin", "xmax")])
    center_y <- mean(current[c("ymin", "ymax")])
    if(identical(direction, "left")) center_x <- center_x - 0.4 * width
    if(identical(direction, "right")) center_x <- center_x + 0.4 * width
    if(identical(direction, "down")) center_y <- center_y - 0.4 * height
    if(identical(direction, "up")) center_y <- center_y + 0.4 * height
    if(identical(direction, "out")) {
      width <- min(diff(full[c("xmin", "xmax")]), width * 1.8)
      height <- min(diff(full[c("ymin", "ymax")]), height * 1.8)
    }
    clamp <- function(center, span, lower, upper) {
      if(span >= upper - lower) return(c(lower, upper))
      bounds <- center + c(-0.5, 0.5) * span
      if(bounds[[1L]] < lower) bounds <- bounds + lower - bounds[[1L]]
      if(bounds[[2L]] > upper) bounds <- bounds - (bounds[[2L]] - upper)
      bounds
    }
    x <- clamp(center_x, width, full[["xmin"]], full[["xmax"]])
    y <- clamp(center_y, height, full[["ymin"]], full[["ymax"]])
    filespec_viewport_state(c(
      xmin = x[[1L]], xmax = x[[2L]], ymin = y[[1L]], ymax = y[[2L]]
    ))
    invisible(NULL)
  }

  observeEvent(input$filespec_view_reset, shift_filespec_view("reset"),
               ignoreInit = TRUE)
  observeEvent(input$filespec_view_out, shift_filespec_view("out"),
               ignoreInit = TRUE)
  observeEvent(input$filespec_view_left, shift_filespec_view("left"),
               ignoreInit = TRUE)
  observeEvent(input$filespec_view_right, shift_filespec_view("right"),
               ignoreInit = TRUE)
  observeEvent(input$filespec_view_up, shift_filespec_view("up"),
               ignoreInit = TRUE)
  observeEvent(input$filespec_view_down, shift_filespec_view("down"),
               ignoreInit = TRUE)

  observeEvent(input$filespec_close, {
    preprocessed$data <- NULL
    clear_filespec_state(
      "Closed the file-backed source; source and completed caches were unchanged."
    )
  }, ignoreInit = TRUE)

  session$onSessionEnded(function() {
    clear_filespec_state()
    preprocessed$data <- NULL
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

    if(isTRUE(input$spatial_decision)) {
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
      if(!isTRUE(input$threshold_decision)) return("run_sig_over_noise")
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
        snr_value = if(isTRUE(input$threshold_decision)) {
          signal_to_noise()[[selected_index]]
        } else NULL,
        snr_threshold = if(isTRUE(input$threshold_decision)) {
          input$MinSNR
        } else NULL,
        signal_metric = effective_signal_selection(),
        correlation_value = if(isTRUE(input$active_identification) &&
                               isTRUE(input$cor_threshold_decision)) {
          max_cor()[[selected_index]]
        } else NULL,
        correlation_threshold = if(isTRUE(input$active_identification) &&
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
      if(!input$threshold_decision){
          -Inf
      }
      else{
          input$MinSNR
      }
  })

  particles_logi <- reactive({
      req(isTRUE(input$collapse_decision))
      collapse_logic <- input$collapse_log_type

      if(identical(collapse_logic, "Thresholds")){
          if(isTRUE(input$active_identification) &&
             isTRUE(input$threshold_decision) &&
             isTRUE(input$cor_threshold_decision)){
              return(signal_to_noise() > MinSNR() & max_cor() > MinCor())
          }
          if(isTRUE(input$threshold_decision)){
              return(signal_to_noise() > MinSNR())
          }
          if(isTRUE(input$active_identification) &&
             isTRUE(input$cor_threshold_decision)){
              return(max_cor() > MinCor())
          }          
      }
      if(identical(collapse_logic, "Identities")){
          if(!isTRUE(input$active_identification)) return(NULL)
          return(max_cor_identity())
      }
      if(identical(collapse_logic, "Both")){
          if(!isTRUE(input$active_identification)) return(NULL)
          background_fill <- max_cor_identity()
          if(isTRUE(input$threshold_decision) &&
             isTRUE(input$cor_threshold_decision)){
              background_fill[!(signal_to_noise() > MinSNR() & max_cor() > MinCor())] <- "background"
              return(background_fill)
          }
          if(isTRUE(input$threshold_decision)){
              background_fill[!(signal_to_noise() > MinSNR())] <- "background"
              return(background_fill)
          }
          if(isTRUE(input$cor_threshold_decision)){
              background_fill[!(max_cor() > MinCor())] <- "background"
              return(background_fill)
          }   
      }
      return(NULL)
  })

  collapse_features <- reactive({
      if(!isTRUE(input$collapse_decision) || is.null(preprocessed$data)) {
        return(NULL)
      }
      features <- tryCatch(
        particles_logi(),
        error = function(error) NULL
      )
      if(!(is.logical(features) || is.character(features)) ||
         length(features) != ncol(DataR()$spectra)) {
        return(NULL)
      }
      if(is.logical(features)) {
        features[is.na(features)] <- FALSE
        if(!any(features) || all(features)) return(NULL)
      } else if(!any(!is.na(features) & nzchar(features))) {
        return(NULL)
      }
      features
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

      signal_failed <- isTRUE(input$threshold_decision) &&
        all(signal_to_noise() < MinSNR())
      correlation_failed <- identification_enabled &&
        isTRUE(input$cor_threshold_decision) &&
        all(max_cor() < MinCor())
      if(signal_failed || correlation_failed) {
          show_alert(
            title = "No regions passing threshold",
            text = paste0(
              "The current threshold settings of the Signal-Noise and/or Correlation returned ",
              "no regions passing. This often indicates an issue with the threshold settings ",
              "or data and will return the raw data in the plots."
            ),
            type = "warning"
          )
      }

      if(isTRUE(input$collapse_decision) && !is.null(collapse_features()) &&
         length(unique(as.character(collapse_features()))) == 1) {
          show_alert(
            title = "No or all regions passing threshold",
            text = paste0(
              "The current threshold settings of the Signal-Noise and/or Correlation returned either all ",
              "or no regions passing. This often indicates an issue with the threshold settings or data."
            ),
            type = "warning"
          )
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
      if(!input$cor_threshold_decision){
          -Inf
      }
      else{
          input$MinCor
      }
  })
  
  output$cor_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(input$active_identification))
      req(isTRUE(input$cor_threshold_decision))
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
output$snr_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    req(isTRUE(input$threshold_decision))
    ggplot() +
        geom_histogram(aes(x = signal_to_noise()),
                       fill = app_plot_palette$primary,
                       color = app_plot_palette$panel) +
        scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
        geom_vline(xintercept = MinSNR(), color = app_plot_palette$reference,
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
    req(ncol(preprocessed$data$spectra) > 1)
    identification_enabled <- isTRUE(input$active_identification)
    collapse_enabled <- isTRUE(input$collapse_decision)
    if(identification_enabled) req(!is.null(max_cor()))
    choice_names <- c(
      if(identification_enabled) "Match Name" else NA,
      if(identification_enabled && !identical(input$lib_type, "model")) {
        "Match ID"
      } else NA,
      if(identification_enabled && !is.null(max_cor())) "Match Value" else NA,
      if(!is.null(signal_to_noise())) "Signal/Noise" else NA,
      if(collapse_enabled && !is.null(collapse_features())) "Feature ID" else NA
    )
    choice_names[!is.na(choice_names)]
  })

  resolved_map_color <- reactive({
    choices <- map_color_choices()
    req(length(choices) > 0L)
    selected <- input$map_color
    if(!isTruthy(selected) || !selected %in% choices) choices[[1L]] else selected
  })

# Progress Bars
output$choice_names <- renderUI({
    choice_names <- map_color_choices()
    selected <- isolate(input$map_color)
    if(!isTruthy(selected) || !selected %in% choice_names) {
      selected <- choice_names[[1L]]
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
    req(ncol(preprocessed$data$spectra) > 1)

    percent_true <- function(x) {
      available <- !is.na(x)
      if(!any(available)) return(0)
      sum(x[available]) / sum(available) * 100
    }

    signal_values <- if(isTRUE(input$threshold_decision)) {
      signal_to_noise()
    } else {
      NULL
    }
    correlation_values <- if(isTRUE(input$active_identification) &&
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
          value = percent_true(signal_values > MinSNR()),
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
            signal_values > MinSNR() & correlation_values > MinCor()
          ),
          status = "success",
          title = "Good Identifications",
          display_pct = TRUE
        )
      )
    }

    plot_items <- list()
    if(isTRUE(input$collapse_decision) && !is.null(collapse_features())) {
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
      app_category_palette(max_cor_identity())
  })

  heatmap_state <- reactive({
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
      features <- collapse_features()
      test <- if(!is.null(features)) {
        def_features(DataR(), features = features)
      } else {
        DataR()
      }

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
      } else if(isTRUE(input$collapse_decision) &&
                !is.null(features) &&
                identical(map_color, "Feature ID")) {
        categorical <- TRUE
        test$metadata$feature_id
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

  output$heatmapA <- renderPlotly({
      state <- heatmap_state()
      plot <- heatmap_spec(
        x = state$data,
        z = state$z,
        sn = signif(signal_to_noise(), 2),
        cor = if(is.null(max_cor())) max_cor() else signif(max_cor(), 2),
        min_sn = if(state$categorical) NULL else MinSNR(),
        min_cor = if(state$categorical) NULL else MinCor(),
        select = isolate(data_click$plot),
        colorscale = state$colorscale,
        showlegend = !state$categorical,
        source = "heat_plot"
      ) %>%
        app_style_plotly()
      plot <- plotly::style(
        plot,
        marker = list(
          color = "#F59E0B", size = 14, opacity = 1,
          line = list(color = "#FFF7ED", width = 2)
        ),
        traces = 2L
      )
      if(!state$categorical) {
        legend_layout <- app_heatmap_legend_layout(state$legend_title)
        plot <- plotly::style(
          plot,
          colorbar = legend_layout$colorbar,
          traces = 1L
        ) %>%
          plotly::layout(
            showlegend = FALSE,
            margin = legend_layout$margin
          )
      } else {
        plot <- plotly::style(
          plot,
          zmin = 1,
          zmax = max(1L, length(levels(state$z))),
          traces = 1L
        ) %>%
          plotly::layout(showlegend = FALSE)
      }
      event_register(plot, event = "plotly_click")
  })

  observeEvent(data_click$plot, {
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
      state <- heatmap_state()
      selected <- data_click$plot
      req(length(selected) == 1L, selected >= 1L,
          selected <= nrow(state$data$metadata))
      plotlyProxy("heatmapA", session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(
            # Plotly.restyle consumes the outer list per trace. Keep each
            # selected coordinate as a one-point vector inside that wrapper.
            x = list(list(state$data$metadata$x[[selected]])),
            y = list(list(state$data$metadata$y[[selected]]))
          ),
          list(1L)
        )
  }, ignoreInit = TRUE)

  thresholded_particles <- reactive({
      req(isTRUE(input$collapse_decision))
      collapse_fun <- function(x, type = input$collapse_type) {
          switch(type,
                 "Mean" = mean(x),
                 "Median" = median(x),
                 "Geometric Mean" = exp(mean(log(x))))
      }

      spec <- DataR()
      if (input$active_identification) {
          spec$metadata$material_class <- max_cor_identity()
      }

      features <- collapse_features()
      req(!is.null(features))
      spec_feat <- def_features(spec, features = features)

      collapsed <- collapse_spec(spec_feat, fun = collapse_fun) %>%
          filter_spec(., logic = .$metadata$feature_id != "-88")

      if (input$active_identification) {
          fid <- spec_feat$metadata$feature_id
          classes <- spec_feat$metadata$material_class
          ids <- unique(fid[fid != "-88"])
          majority <- vapply(ids, function(id) {
              vals <- classes[fid == id]
              vals <- vals[!is.na(vals)]
              if (length(vals) == 0) NA_character_ else names(sort(table(vals), decreasing = TRUE))[1]
          }, character(1))
          collapsed$metadata$material_class <- majority[match(collapsed$metadata$feature_id, ids)]
      }

      collapsed
  })
  
  #Summary Plots ----
  output$particle_plot <- renderPlot({
      req(!is.null(preprocessed$data))
      req(isTRUE(input$collapse_decision))
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
      req(max_cor_identity())
      if(input$collapse_decision){
          if (isTruthy(thresholded_particles()) &&
              all(grepl("_[0-9]+", thresholded_particles()$metadata$feature_id))) {
              
              match_names <- gsub("_[0-9]+", "", thresholded_particles()$metadata$feature_id)
              
          } else {
              match_names <- thresholded_particles()$metadata$material_class
              
          }    
      }
        else {
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
      collapse = !is.null(preprocessed$data) &&
        isTRUE(input$collapse_decision) && !is.null(collapse_features())
    )
    selectInput(
      inputId = "download_selection",
      label = "Download contents",
      choices = choice_names,
      selected = choice_names[[1L]]
    )
  })  
  outputOptions(output, "download_ui", suspendWhenHidden = FALSE)

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
  output$download_data <- downloadHandler(
    filename = function() {
      selection <- input$download_selection
      if(identical(selection, "User Metadata")) {
        return(paste0("os_metadata_", human_ts(), ".csv"))
      }
      extension <- if(identical(selection, "Test Map")) ".zip" else ".csv"
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
        write_spec(thresholded_particles(), file = file)
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
             condition = isTruthy(ncol(preprocessed$data$spectra) > 1))
      toggle(id = "placeholder1", condition = !isTruthy(preprocessed$data))
  })

  heatmap_click <- reactive({
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
      suppressWarnings(
          event_data("plotly_click", source = "heat_plot", priority = "event")
      )
  })

  observeEvent(heatmap_click(), {
      click <- heatmap_click()
      curve_number <- if(length(click$curveNumber)) {
        suppressWarnings(as.integer(click$curveNumber[[1L]]))
      } else {
        0L
      }
      if(is.na(curve_number) || curve_number != 0L) return()

      selected <- integer()
      if(length(click$x) && length(click$y)) {
        state <- isolate(heatmap_state())
        metadata <- state$data$metadata
        click_x <- click$x[[1L]]
        click_y <- click$y[[1L]]
        coordinate_match <- function(values, target) {
          if(is.numeric(values) && is.numeric(target)) {
            tolerance <- sqrt(.Machine$double.eps) *
              pmax(1, abs(values), abs(target))
            !is.na(values) & !is.na(target) &
              abs(values - target) <= tolerance
          } else {
            !is.na(values) & !is.na(target) &
              as.character(values) == as.character(target)
          }
        }
        selected <- which(
          coordinate_match(metadata$x, click_x) &
            coordinate_match(metadata$y, click_y)
        )
      }
      if(!length(selected) && length(click$pointNumber)) {
        point_number <- suppressWarnings(as.numeric(
          unlist(click$pointNumber, use.names = FALSE)
        ))
        if(length(point_number) == 1L && is.finite(point_number)) {
          selected <- as.integer(point_number + 1L)
        }
      }
      selected <- selected[
        !is.na(selected) & selected >= 1L &
          selected <= ncol(preprocessed$data$spectra)
      ]
      if(length(selected)) {
          data_click$plot <- selected[[1L]]
      }
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

