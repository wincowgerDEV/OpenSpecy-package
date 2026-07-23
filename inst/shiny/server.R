function(input, output, session) {
    
  #Setup ----
    options(shiny.maxRequestSize=10000*1024^2)
    
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
  data_click <- reactiveValues(plot = NULL, table = NULL)
  meta_cache <- reactiveVal(NULL)
  selected_match_cache <- reactiveVal(NULL)
  correction_diagnostics <- reactiveVal(data.frame())
  ratio_definitions <- reactiveVal(app_empty_ratio_definitions())

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
  selected_match_cache(NULL)
  correction_diagnostics(data.frame())
  ratio_definitions(app_empty_ratio_definitions())

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
  
  #The matching library to use. 
  libraryR <- reactive({
      req(!is.null(preprocessed$data))
      req(input$active_identification)
      analysis_phase(
        "Loading the reference library",
        paste0(
          "Loading the selected ", input$lib_type,
          " library. The first use can take longer if it must be downloaded."
        ),
        52
      )
      if (input$id_strategy == "deriv" && input$lib_type == "medoid") {
          library <- load_app_library("medoid_derivative")
          #return(library)
      }
      else if (input$id_strategy == "nobaseline" &&
               input$lib_type == "medoid") {
          library <- load_app_library("medoid_nobaseline")
      }
      else if (input$id_strategy == "deriv" &&
               input$lib_type == "model") {
          library <- load_app_library("model_derivative")
          library <- library[[input$id_spec_type]]
          return(library)
      }
      else if (input$id_strategy == "nobaseline" &&
               input$lib_type == "model") {
          library <- load_app_library("model_nobaseline")
          library <- library[[input$id_spec_type]]
          return(library)
      }
      else if (grepl("nobaseline$", input$id_strategy)) {
          library <- load_app_library("nobaseline")
      }
      else if (grepl("deriv$", input$id_strategy)) {
          library <- load_app_library("derivative")
      }
      analysis_phase(
        "Preparing the reference library",
        paste0("Filtering ", format(ncol(library$spectra), big.mark = ","),
               " reference spectra to the uploaded range."),
        64
      )
      if(!is.null(preprocessed$data)){
          library <- restrict_range(library, min = min(DataR()$wavenumber), max = max(DataR()$wavenumber), make_rel = F)
          keep_spectra <- !apply(library$spectra, 2, function(x) all(is.na(x)))
          library <- filter_spec(library, logic = keep_spectra)
      }
      
      if(grepl("^both", input$id_spec_type)) {
          library
      }
      else if (grepl("^ftir", input$id_spec_type)){
          filter_spec(library, logic = library$metadata$spectrum_type == "ftir")
      }
      else if (grepl("^raman", input$id_spec_type)){
          filter_spec(library, logic = library$metadata$spectrum_type == "raman")
      }
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

 # Redirecting preprocessed data to be a reactive variable. Not totally sure why this is happening in addition to the other. 
 data <- reactive({
    req(input$file)
      da <- preprocessed$data
      if(isTruthy(input$xy_grid) &&
         (!all(diff(sort(preprocessed$data$metadata$y)) %in% c(0,1)) ||
          !all(diff(sort(preprocessed$data$metadata$x)) %in% c(0,1)))){
          grid <- gen_grid(nrow(preprocessed$data$metadata))
          da$metadata$x <- grid$x
          da$metadata$y <- grid$y
      }
          da
    })

  # Preprocess ----
  # Ordinary preprocessing and independent spatial smoothing run first. Range
  # restriction and flattening are intentionally staged afterward so their
  # automatic checks assess the final processed signal.
  baseline_data <- reactive({
    req(!is.null(preprocessed$data))
    uploaded <- data()
    processed <- uploaded

    if(isTRUE(input$active_preprocessing)) {
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
          range = NULL,
          res = input$conform_res,
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
          seq(100, 4000, by = conform_args$res)
        } else {
          uploaded$wavenumber
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
        x = uploaded,
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

    processed
  })

  automation_status_ui <- function(check, label) {
    diagnostics <- correction_diagnostics()
    row <- diagnostics[diagnostics$check == check, , drop = FALSE]
    if(!nrow(row)) {
      return(tags$p(
        class = "openspecy-automation-status text-muted",
        paste("Waiting for the", label, "check to run on processed spectra.")
      ))
    }
    row <- row[nrow(row), , drop = FALSE]
    total <- row$total_spectra[[1L]]
    before_problems <- total - row$before_passes[[1L]]
    after_problems <- total - row$after_passes[[1L]]
    reason <- row$reason[[1L]]

    comparison <- if(identical(reason, "no_failures")) {
      paste0(
        "Problematic spectra: ", before_problems, " of ", total,
        " before correction; ", after_problems, " of ", total,
        " after correction."
      )
    } else {
      paste0(
        "Problematic spectra: ", before_problems, " of ", total,
        " before correction; ", after_problems, " of ", total,
        " after the candidate correction."
      )
    }
    outcome <- switch(
      reason,
      no_failures = "No correction was necessary.",
      improved = "The correction improved the batch and was retained.",
      not_improved = paste(
        "The candidate did not improve the batch, so the original processed",
        "spectra were retained."
      ),
      correction_error = paste(
        "The correction could not be completed, so the original processed",
        "spectra were retained."
      ),
      invalid_candidate = paste(
        "The candidate did not preserve the batch, so the original processed",
        "spectra were retained."
      ),
      assessment_error = paste(
        "The candidate could not be assessed, so the original processed",
        "spectra were retained."
      ),
      "The automatic check completed."
    )

    tags$div(
      class = paste(
        "openspecy-automation-status",
        paste0("openspecy-automation-status-", reason)
      ),
      role = "status",
      tags$strong(paste0(label, ": ")),
      tags$span(paste(comparison, outcome))
    )
  }

  output$co2_automation_status <- renderUI({
    if(!isTRUE(input$active_preprocessing)) return(NULL)
    if(!isTRUE(input$co2_decision)) return(NULL)
    if(!isTRUE(input$co2_automate)) return(NULL)
    if(is.null(preprocessed$data)) {
      return(tags$p(
        class = "openspecy-automation-status text-muted",
        "Upload spectra to run the automatic CO2 check."
      ))
    }
    automation_status_ui("co2_region", "CO2 check")
  })
  outputOptions(output, "co2_automation_status", suspendWhenHidden = FALSE)

  output$range_automation_status <- renderUI({
    if(!isTRUE(input$active_preprocessing)) return(NULL)
    if(!isTRUE(input$range_decision)) return(NULL)
    if(!isTRUE(input$range_automate)) return(NULL)
    if(is.null(preprocessed$data)) {
      return(tags$p(
        class = "openspecy-automation-status text-muted",
        "Upload spectra to run the automatic high-tail check."
      ))
    }
    automation_status_ui("high_tail", "High-tail check")
  })
  outputOptions(output, "range_automation_status", suspendWhenHidden = FALSE)


  output$quant_ratio_bounds <- renderUI({
    if(is.null(preprocessed$data)) {
      return(tags$p(
        class = "text-muted openspecy-quantification-prompt",
        "Upload and process spectra to set ratio points and ranges."
      ))
    }

    processed <- DataR()
    type <- input$quant_ratio_type
    if(is.null(type)) type <- "area"
    defaults <- app_ratio_slider_defaults(
      processed$wavenumber,
      type = type
    )
    if(identical(type, "area")) {
      tagList(
        sliderInput(
          "quant_numerator_area", "Numerator area (cm^-1)",
          min = defaults$min, max = defaults$max,
          value = defaults$numerator, step = defaults$step
        ),
        sliderInput(
          "quant_denominator_area", "Denominator area (cm^-1)",
          min = defaults$min, max = defaults$max,
          value = defaults$denominator, step = defaults$step
        )
      )
    } else {
      tagList(
        sliderInput(
          "quant_numerator_peak", "Numerator point (cm^-1)",
          min = defaults$min, max = defaults$max,
          value = defaults$numerator, step = defaults$step
        ),
        sliderInput(
          "quant_denominator_peak", "Denominator point (cm^-1)",
          min = defaults$min, max = defaults$max,
          value = defaults$denominator, step = defaults$step
        )
      )
    }
  })
  outputOptions(output, "quant_ratio_bounds", suspendWhenHidden = FALSE)

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
      defaults <- app_ratio_slider_defaults(
        processed$wavenumber,
        type = type
      )
      numerator <- if(identical(type, "peak")) {
        isolate(input$quant_numerator_peak)
      } else {
        isolate(input$quant_numerator_area)
      }
      denominator <- if(identical(type, "peak")) {
        isolate(input$quant_denominator_peak)
      } else {
        isolate(input$quant_denominator_area)
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

  active_ratio_definitions <- reactive({
    if(!isTRUE(input$active_quantification)) {
      return(app_empty_ratio_definitions())
    }
    ratio_definitions()
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
    if(!nrow(definitions)) return(processed)
    analysis_phase(
      "Calculating saved ratios",
      paste0(
        "Calculating ", nrow(definitions), " saved ratio",
        if(nrow(definitions) == 1L) "" else "s",
        " from the displayed processed spectra."
      ),
      49
    )
    app_attach_quantification(processed, definitions)
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
      meta <- quantified_data()$metadata
      meta$signal_to_noise <- signal_to_noise()
      meta <- meta[, !sapply(meta, OpenSpecy::is_empty_vector), with = FALSE]
      meta[, coord_key := paste(x, y)]
      meta <- data.table(Index = seq_len(nrow(meta)), meta)
      meta_cache(meta)
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
  
  
  #Identification ----
  output$correlation_head <- renderUI({
      req(!is.null(preprocessed$data))
      signal_enabled <- isTRUE(input$threshold_decision)
      correlation_enabled <- isTRUE(input$active_identification) &&
        isTRUE(input$cor_threshold_decision)
      req(signal_enabled || correlation_enabled)

      good_sig <- if(signal_enabled) {
        signal_to_noise()[[data_click$plot]] > MinSNR()
      } else {
        TRUE
      }
      good_cor <- if(correlation_enabled) {
        max_cor()[[data_click$plot]] > MinCor()
      } else {
        TRUE
      }
      both_enabled <- signal_enabled && correlation_enabled
      label <- if(both_enabled) {
        "Match"
      } else if(correlation_enabled) {
        "Cor"
      } else {
        "SNR"
      }

      boxLabel(
        text = label,
        status = if(good_sig && good_cor) "success" else "error",
        tooltip = paste(
          "This tells you whether the signal to noise ratio or the match",
          "observed is above or below the thresholds."
        )
      )
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

      if(isTRUE(input$collapse_decision) && isTruthy(particles_logi()) &&
         length(unique(as.character(particles_logi()))) == 1) {
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
      #req(input$file)
      #req(input$active_identification)
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

  observe(priority = -1, {
      if(is.null(preprocessed$data) || !isTRUE(input$active_identification) ||
         grepl("^model$", input$lib_type)) {
          selected_match_cache(NULL)
          return()
      }
      selected_match_cache(NULL)
      selected_match_cache(
        tryCatch(
          match_selected(),
          shiny.silent.error = function(e) NULL
        )
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
        dataR_metadata <- quantified_data()$metadata
        dataR_metadata$signal_to_noise <- signal_to_noise()
        if ("material_class" %in% names(dataR_metadata)) {
            dataR_metadata[, material_class := NULL]
        }
        setkey(dataR_metadata, col_id)
        setkey(selected_match, object_id)

        result <- dataR_metadata[selected_match, on = c(col_id = "object_id")]
        result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
            select(file_name, col_id, material_class, spectrum_identity, match_val, signal_to_noise, everything())
        result
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
        escape = FALSE,
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
output$sidebar_metadata <- DT::renderDataTable(server = TRUE, {
    req(!is.null(meta_cache()))
    datatable(meta_cache(),
              escape = FALSE,
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE,
                             pageLength = 5,
                             columnDefs = list(list(visible = FALSE, targets = 0))),
              rownames = FALSE,
              filter = "top",
              caption = "Uploaded Metadata",
              style = "bootstrap",
              selection = "single")
})

  sidebar_proxy <- DT::dataTableProxy("sidebar_metadata")

  observe({
      req(!is.null(meta_cache()))
      sel <- data_click$plot
      if (!is.null(sel) && sel <= nrow(meta_cache())) {
          DT::selectRows(sidebar_proxy, sel)
      }
  })

# Progress Bars
output$choice_names <- renderUI({
    req(ncol(preprocessed$data$spectra) > 1)
    identification_enabled <- isTRUE(input$active_identification)
    collapse_enabled <- isTRUE(input$collapse_decision)
    choice_names = c(
      if(identification_enabled) "Match Name" else NA,
      if(identification_enabled && !identical(input$lib_type, "model")) {
        "Match ID"
      } else NA,
      if(identification_enabled && !is.null(max_cor())) "Match Value" else NA,
      if(!is.null(signal_to_noise())) "Signal/Noise" else NA,
      if(collapse_enabled && isTruthy(particles_logi())) "Feature ID" else NA
    )
    choice_names = choice_names[!is.na(choice_names)]
        tagList(
            fluidRow(
                column(6, selectInput(inputId = "map_color", 
                                      label = "Map Color", 
                                      choices = choice_names)
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
    if(isTRUE(input$collapse_decision) && isTruthy(particles_logi())) {
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
        selected_match_cache()
      } else {
        NULL
      }
      analysis_phase(
        "Rendering results",
        paste0(
          "Drawing the active spectrum",
          if(is.null(raw)) "" else ", its raw overlay",
          if(is.null(reference)) "." else ", and the selected identification match."
        ),
        94
      )
      app_spectrum_plot(
        active = primary,
        raw = raw,
        reference = reference,
        make_rel = isTRUE(input$active_preprocessing) &&
          isTRUE(input$make_rel_decision),
        source = "B"
      ) %>%
        app_style_plotly() %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })

 #Heatmap ----
 #Display the map or batch data in a selectable heatmap. 
  output$heatmapA <- renderPlotly({
      req(!is.null(preprocessed$data))
      req(ncol(preprocessed$data$spectra) > 1)
      #req(input$map_color)
      if(isTRUE(input$collapse_decision) && isTruthy(particles_logi()) &&
         length(unique(as.character(particles_logi()))) > 1){
          test = def_features(DataR(), features = particles_logi())
      }
      else{
          test = DataR()
      }

      heatmap_spec(x = test, 
                        z = if(!is.null(max_cor()) && !isTruthy(input$map_color)){
                            signif(max_cor(),2)
                        }
                   else if(!is.null(signal_to_noise()) && !isTruthy(input$map_color)){
                       signif(signal_to_noise(),2)
                   }
                   else if(!is.null(max_cor()) && identical(input$map_color, "Match ID")){
                        names(max_cor())
                   }
                   else if(!is.null(max_cor()) && identical(input$map_color, "Match Value")){
                       signif(max_cor(),2)
                   }
                   else if(!is.null(signal_to_noise()) && identical(input$map_color, "Signal/Noise")){
                       signif(signal_to_noise(),2)
                   }
                   else if(!is.null(max_cor()) && identical(input$map_color, "Match Name")){
                       max_cor_identity()
                   }
                   else if(isTRUE(input$collapse_decision) &&
                           isTruthy(particles_logi()) &&
                           identical(input$map_color, "Feature ID")){
                       test$metadata$feature_id
                   }
                   else{NULL},
                        sn = signif(signal_to_noise(), 2), 
                        cor = if(is.null(max_cor())){max_cor()} else{signif(max_cor(), 2)}, 
                        min_sn = MinSNR(),
                        min_cor = MinCor(),
                        select = data_click$plot,
                        source = "heat_plot") %>%
          app_style_plotly() %>%
          event_register(event = "plotly_click")

  })

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

      spec_feat <- def_features(spec, features = particles_logi())

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
        isTRUE(input$collapse_decision)
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
          active_ratio_definitions()
        )
        if(!grepl("^model$", input$lib_type)) {
          top_n <- input$top_n_input
          if(is.null(top_n) || !is.finite(top_n)) top_n <- 1L
          top_n <- max(1L, as.integer(top_n))
          columns_selected <- input$columns_selected
          if(is.null(columns_selected)) columns_selected <- "Simple"
          dataR_metadata <- data.table(
            match_threshold = MinCor(),
            signal_to_noise = signal_to_noise(),
            signal_threshold = MinSNR(),
            good_signal = signal_to_noise() > MinSNR()
          ) %>%
            bind_cols(quantified_data()$metadata)

          all_matches <- reshape2::melt(correlation()) %>%
            as.data.table() %>%
            left_join(
              library_filtered()$metadata %>%
                select(-any_of(c("col_id", "file_name"))),
              by = c("Var1" = "sample_name")
            ) %>%
            left_join(dataR_metadata, by = c("Var2" = "col_id")) %>%
            rename(
              "sample_name" = "Var1",
              "col_id" = "Var2",
              "match_val" = "value"
            ) %>%
            mutate(
              good_match_vals = match_val > match_threshold,
              good_matches = match_val > match_threshold &
                signal_to_noise > signal_threshold
            ) %>%
            {keep <- !sapply(., OpenSpecy::is_empty_vector) |
              names(.) %in% quant_columns
             .[, keep, with = FALSE]} %>%
            select(file_name, col_id, material_class, spectrum_identity,
                   match_val, signal_to_noise, everything()) %>%
            .[order(-match_val), head(.SD, top_n), by = col_id] %>%
            {if(identical(columns_selected, "Simple")) {
              select(., any_of(c(
                "file_name", "col_id", "material_class", "match_val",
                "signal_to_noise", quant_columns
              )))
            } else .} %>%
            mutate(
              material_class = ifelse(match_val < MinCor(), "unknown",
                                      material_class)
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
      if (!is.null(click$pointNumber))
          data_click$plot <- click$pointNumber + 1
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
      sel <- meta_cache()$Index[input$sidebar_metadata_rows_selected]
      if (!is.null(sel) && !identical(sel, data_click$plot)) {
          data_click$plot <- sel
      }
  })


  move_selection <- function(dx = 0, dy = 0) {
      req(!is.null(meta_cache()))
      meta <- meta_cache()
      cur <- data_click$plot
      if (cur > nrow(meta)) return()
      target <- paste(meta$x[cur] + dx, meta$y[cur] + dy)
      idx <- match(target, meta$coord_key)
      if (!is.na(idx)) data_click$plot <- idx
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
  
  user_metadata <- reactive({
    settings <- stats::setNames(
      lapply(app_user_metadata_input_ids, function(id) input[[id]]),
      app_user_metadata_input_ids
    )
    app_user_metadata_snapshot(
      settings = settings,
      definitions = ratio_definitions(),
      recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      app_version = tryCatch(
        as.character(utils::packageVersion("OpenSpecy")),
        error = function(...) "development"
      ),
      session_id = session_id,
      source = preprocessed$data,
      file_info = input$file
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

