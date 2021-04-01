#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
# Libraries ----
library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
# library(viridis)
library(data.table)
library(DT)
library(digest)
library(rdrop2)
library(curl)
library(config)
#devtools::install_github("wincowgerDEV/OpenSpecy")
library(OpenSpecy)
library(ids)
library(shinyEventLogger)

#library(future)
#library(bslib)

if(file.exists(".db_url")){
  set_logging(js_console = F, database = T)
}

# Required Data ----
conf <- config::get()

# Load all data ----
load_data <- function() {
  costs <- fread("data/costs.csv")
  donations <- fread("data/donations.csv")
  testdata <- raman_hdpe

  # Check if spectral library is present and load
  test_lib <- class(tryCatch(check_lib(path = conf$library_path),
                             warning = function(w) {w}))
  if(any(test_lib == "warning")) get_lib(path = conf$library_path)

  spec_lib <- load_lib(path = conf$library_path)

  # Check for Auth Tokens and setup
  droptoken <- file.exists("data/droptoken.rds")

  if(droptoken) {
    drop_auth(rdstoken = "data/droptoken.rds")
  }

  # Name keys for human readable column names
  load("data/namekey.RData")

  # Inject variables into the parent
  invisible(list2env(as.list(environment()), parent.frame()))
}

# This is the actual server functions, all functions before this point are not
# reactive
server <- shinyServer(function(input, output, session) {
  #For theming
  #bs_themer()
    sessionid <- random_id(n = 1)
    
    #User event logging ----
    if(file.exists(".db_url")){
      set_logging_session()
      log_event(sessionid)
      observe(
        log_value(input$intensity_corr)
      )
      observe(
        log_value(digest::digest(preprocessed_data(), algo = "md5"))
      )   
      observe(
        log_value(input$smoother)
      )
      observe(
        log_value(input$baseline)
      )
      observe(
        log_value(input$MinRange)
      )
      observe(
        log_value(input$MaxRange)
      )
      observe(
        log_value(input$event_rows_selected)
      )
      observe(
        log_value(input$smooth_decision)
      )
      observe(
        log_value(input$baseline_decision)
      )
      observe(
        log_value(input$range_decision)
      )
      observe(
        log_value(input$Spectra)
      )
      observe(
        log_value(input$Data)
      )
      observe(
        log_value(input$Library)
      ) 
    }
  
  # Loading overlay
  load_data()
  hide(id = "loading_overlay", anim = TRUE, animType = "fade")
  show("app_content")

  # For desktop version of the app.
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  # Save the metadata and data submitted upon pressing the button
  observeEvent(input$submit, {
    if (input$share_decision & curl::has_internet())
      share <- conf$share else share <- NULL

      sout <- tryCatch(share_spec(
        data(), sapply(names(namekey)[1:24], function(x) input[[x]]),
        share = share),
        warning = function(w) {w}, error = function(e) {e})

      if (inherits(sout, "simpleWarning") | inherits(sout, "simpleError"))
        mess <- sout$message

      if (is.null(sout)) {
        show_alert(
          title = "Thank you for sharing your data!",
          text = "Your data will soon be available at https://osf.io/stmv4/",
          type = "success"
        )
      } else {
        show_alert(
          title = "Something went wrong :-(",
          text = paste0("All mandatory data added? R says: '", mess, "'. ",
                        "Try again."),
          type = "warning"
        )
      }
  })


  # Read in data when uploaded based on the file type
  preprocessed_data <- reactive({
    req(input$file1)
    inFile <- input$file1
    filename <- as.character(inFile$datapath)

    if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
              ignore.case = T, filename)) {
      show_alert(
        title = "Data type not supported!",
        text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
        type = "warning")
      stop()
      }

    if (input$share_decision & curl::has_internet())
      share <- conf$share else share <- NULL

    if(grepl("\\.csv$", ignore.case = T, filename)) {
      rout <- tryCatch(read_text(inFile$datapath, method = "fread",
                                 share = share),
                       error = function(e) {e})
    }
    else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
      rout <- tryCatch(read_0(inFile$datapath, share = share),
                       error = function(e) {e})
    }
    else {
      ex <- strsplit(basename(filename), split="\\.")[[1]]

      rout <- tryCatch(do.call(paste0("read_", tolower(ex[-1])),
                               list(inFile$datapath, share = share)),
                       error = function(e) {e})
    }

    if (inherits(rout, "simpleError")) {
      reset("file1")
      show_alert(
        title = "Something went wrong :-(",
        text = paste0("R says: '", rout$message, "'. ",
                      "If you uploaded a .csv file, make sure that the columns ",
                      "are named 'wavenumber' and 'intensity'."),
        type = "error"
      )
      stop()
    } else {
      rout
    }

  })

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    adj_intens(preprocessed_data(), type = input$intensity_corr)
    })

  #Preprocess Spectra ----
  # All cleaning of the data happens here. Smoothing and Baseline removing
  baseline_data <- reactive({
    testdata <- data() %>% dplyr::filter(wavenumber > input$MinRange &
                                           wavenumber < input$MaxRange)
    test <-  nrow(testdata) < 3
    if (test) {
      data() %>%
        mutate(intensity = if(input$smooth_decision) {
          smooth_intens(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline_decision) {
          subtr_bg(.$wavenumber, .$intensity, degree = input$baseline)$intensity
          } else .$intensity)
    } else {
      data() %>%
        dplyr::filter(
          if(input$range_decision) {wavenumber > input$MinRange &
              wavenumber < input$MaxRange} else {
                wavenumber == wavenumber}) %>%
        mutate(intensity = if(input$smooth_decision) {
          smooth_intens(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline_decision) {
          subtr_bg(.$wavenumber, .$intensity, degree = input$baseline)$intensity
          } else .$intensity)
    }
  })

  # Create file view and preprocess view
  output$MyPlot <- renderPlotly({
    plot_ly(data(), type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~wavenumber, y = ~intensity, name = 'Uploaded Spectrum',
                line = list(color = 'rgba(240,236,19, 0.8)')) %>%
      layout(yaxis = list(title = "absorbance intensity [-]"),
             xaxis = list(title = "wavenumber [cm<sup>-1</sup>]"),
             plot_bgcolor='rgb(17,0,73)',
             paper_bgcolor='black', font = list(color = '#FFFFFF'))
  })
  output$MyPlotB <- renderPlotly({
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(data = baseline_data(), x = ~wavenumber, y = ~intensity,
                name = 'Processed Spectrum',
                line = list(color = 'rgb(240,19,207)')) %>%
      add_trace(data = data(), x = ~wavenumber, y = ~intensity,
                name = 'Uploaded Spectrum',
                line = list(color = 'rgba(240,236,19,0.8)')) %>%
      # Dark blue rgb(63,96,130)
      # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
      layout(yaxis = list(title = "absorbance intensity [-]"),
             xaxis = list(title = "wavenumber [cm<sup>-1</sup>]"),
             plot_bgcolor='rgb(17,0,73)', paper_bgcolor='black',
             font = list(color = '#FFFFFF'))
  })

  # Choose which spectrum to use
  DataR <- reactive({
    if(input$Data == "uploaded") {
      data()
    }
    else if(input$Data == "processed") {
      baseline_data()
    }
  })

  # Identify Spectra function ----
  # Joins their spectrum to the internal database and computes correlation.
  MatchSpectra <- reactive ({
    req(input$tabs == "tab3")
    input
    withProgress(message = 'Analyzing Spectrum', value = 1/3, {

      incProgress(1/3, detail = "Finding Match")

      Lib <- match_spec(DataR(),
                        library = spec_lib, which = input$Spectra,
                        type = input$Library, top_n = 100)

      incProgress(1/3, detail = "Making Plot")

    })
    return(Lib)
  })

  # Create the data tables
  output$event <- DT::renderDataTable({
    datatable(MatchSpectra() %>%
                dplyr::rename("Material" = spectrum_identity) %>%
                dplyr::select(-sample_name) %>%
                dplyr::rename("Pearson's r" = rsq,
                              "Organization" = organization),
              options = list(searchHighlight = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              filter = "top",caption = "Selectable Matches",
              style = "bootstrap",
              selection = list(mode = "single", selected = c(1)))
  })

  output$costs <- DT::renderDataTable({
    datatable(costs, options = list(searchHighlight = TRUE,
                                    sDom  = '<"top">lrt<"bottom">ip',
                                    lengthChange = FALSE, pageLength = 5),
              filter = "top", caption = "Operation Costs", style = "bootstrap",
              selection = list(mode = "single", selected = c(1)))
  })

  output$donations <- DT::renderDataTable({
    datatable(donations, options = list(searchHighlight = TRUE,
                                        sDom  = '<"top">lrt<"bottom">ip',
                                        lengthChange = FALSE, pageLength = 5),
              filter = 'top', caption = "Donations", style = 'bootstrap',
              selection = list(mode = 'single', selected = c(1)))
  })

  output$eventmetadata <- DT::renderDataTable({
    # Default to first row if not yet clicked
    id_select <- ifelse(is.null(input$event_rows_selected),
                        1,
                        MatchSpectra()[[input$event_rows_selected,
                                        "sample_name"]])
    # Get data from find_spec
    current_meta <- find_spec(sample_name == id_select, spec_lib,
                              which = input$Spectra)
    names(current_meta) <- namekey[names(current_meta)]

    datatable(current_meta,
              escape = FALSE, rownames = F,
              options = list(dom = 't', bSort = F, lengthChange = FALSE,
                             rownames = FALSE, info = FALSE),
              style = 'bootstrap', caption = "Selection Metadata",
              selection = list(mode = 'none'))
  })

  # Display matches based on table selection ----
  output$MyPlotC <- renderPlotly({
    if(!length(input$event_rows_selected)) {
      plot_ly(DataR()) %>%
        add_lines(x = ~wavenumber, y = ~intensity,
                  line = list(color = 'rgba(255,255,255,0.8)')) %>%
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]"),
               plot_bgcolor='rgb(17,0, 73)',
               paper_bgcolor='black', font = list(color = '#FFFFFF'))
    }
    else if(length(input$event_rows_selected)) {
      # Default to first row if not yet clicked
      id_select <- ifelse(is.null(input$event_rows_selected),
                          1,
                          MatchSpectra()[[input$event_rows_selected,
                                          "sample_name"]])
      # Get data from find_spec
      current_spectrum <- find_spec(sample_name == id_select,
                                    spec_lib, which = input$Spectra,
                                    type = input$Library)

      TopTens <- current_spectrum %>%
        inner_join(MatchSpectra()[input$event_rows_selected,,drop = FALSE],
                   by = "sample_name") %>%
        select(wavenumber, intensity, spectrum_identity)

      OGData <- DataR() %>%
        select(wavenumber, intensity) %>%
        mutate(spectrum_identity = "Spectrum to Analyze")

      plot_ly(TopTens, x = ~wavenumber, y = ~Intensity) %>%
        add_lines(data = TopTens, x = ~wavenumber, y = ~intensity,
                  color = ~factor(spectrum_identity), colors = "#FF0000") %>%
        # viridisLite::plasma(7, begin = 0.2, end = 0.8)
        add_lines(data = OGData, x = ~wavenumber, y = ~intensity,
                  line = list(color = "rgba(255,255,255,0.8)"),
                  name = "Spectrum to Analyze") %>%
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]"),
               plot_bgcolor = "rgb(17,0, 73)", paper_bgcolor = "black",
               font = list(color = "#FFFFFF"))
    }})

  # Data Download options
  output$downloadData5 <- downloadHandler(
    filename = function() {"ftir_library.csv"},
    content = function(file) {fwrite(spec_lib[["ftir"]][["library"]], file)}
  )

  output$downloadData6 <- downloadHandler(
    filename = function() {"raman_library.csv"},
    content = function(file) {fwrite(spec_lib[["raman"]][["library"]], file)}
  )

  output$downloadData4 <- downloadHandler(
    filename = function() {"raman_metadata.csv"},
    content = function(file) {fwrite(spec_lib[["raman"]][["metadata"]], file)}
  )

  output$downloadData3 <- downloadHandler(
    filename = function() {"raman_metadata.csv"},
    content = function(file) {fwrite(spec_lib[["ftir"]][["metadata"]], file)}
  )

  output$download_testdata <- downloadHandler(
    filename = function() {"testdata.csv"},
    content = function(file) {fwrite(testdata, file)}
  )

  ## Download own data ----
  output$downloadData <- downloadHandler(
    filename = function() {paste('data-', human_ts(), '.csv', sep='')},
    content = function(file) {fwrite(baseline_data(), file)}
  )

  ## Sharing data ----
  # Hide functions which shouldn't exist when there is no internet or
  # when the API token doesn't exist
  observe({
    if((conf$share == "dropbox" & droptoken) | curl::has_internet()) {
      show("share_decision")
      show("share_meta")
    }
    else {
      hide("share_decision")
      hide("share_meta")
    }
  })

  observe({
    if (input$share_decision) {
      show("share_meta")
      } else {
        hide("share_meta")
      }
  })

  observe({
    if (input$smooth_decision) {
      show("smooth_tools")
    } else {
      hide("smooth_tools")
    }
  })

  observe({
    if (input$baseline_decision) {
      show("baseline_tools")
    } else {
      hide("baseline_tools")
    }
  })

  observe({
    if (input$range_decision) {
      show("range_tools")
    } else {
      hide("range_tools")
    }
  })
  #This toggles the hidden metadata input layers.
  observeEvent(input$share_meta, {
    sapply(names(namekey)[1:24], function(x) toggle(x))
    toggle("submit")
  })
  
  # Session Files ----
  observeEvent(input$file1, {
      if(input$share_decision & droptoken){
        output_dir = paste("data/users/", sessionid, sep = "")
        if (!dir.exists(output_dir)) {
          dir.create(output_dir)
          dir.create(paste(output_dir, "/user_data", sep = ""))
          dir.create(paste(output_dir, "/user_log", sep = ""))
        }
        withProgress(message = 'Sharing Spectrum to Community Library', value = 3/3, {
        inFile <- input$file1
        UniqueID <- digest::digest(preprocessed_data(), algo = "md5") #Gets around the problem of people sharing data that is different but with the same name.
        location_data <- paste("data/users/", sessionid, "/user_data/", UniqueID, "_", inFile$name, sep = "")
        file.copy(inFile$datapath, location_data)
        drop_upload(location_data, path = dirname(location_data), mode = "add")
      })
    }
  })
     
  #Log User choices ----
#  toListen <- reactive({
#    list(input$file1,input$smoother, input$baseline, input$MinRange, input$MaxRange, input$event_rows_selected, input$smooth_decision, input$baseline_decision, input$range_decision, input$Spectra, input$Data, input$Library, input$intensity_corr)
#  })
  
#  observeEvent(toListen(), {
#      if(is.list(input$file1) & input$share_decision){
        #Makes sure that a file is uploaded before we start tracking data. not working.
#      log <- c(unlist(input$file1),input$smoother, input$baseline, input$MinRange, input$MaxRange, input$event_rows_selected, input$smooth_decision, input$baseline_decision, input$range_decision, input$Spectra, input$Data, input$Library, input$intensity_corr)
#      location <- paste("data/users/", sessionid, "/user_log/", human_ts(), ".rds", sep = "")
#        saveRDS(log, file = location)
#        drop_upload(location, path = dirname(location), mode = "add")
        
        #if(curl::has_internet() & droptoken){   
        #drop_upload(location, path = paste("data/", input$cookies$name, "/user_log", sep = ""), mode = "add")
        #}
 #     } 
#  })
  
  #Log with shiny event logger ----
 
  
#  output$name_get <- renderUI({
#    if(!is.null(input$cookies$name))
#      h3("Hello,", input$cookies$name)
#    else
#      h3("Who are you?")
#  })

  output$translate <- renderUI({
    if(file.exists("www/googletranslate.html") & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })

  output$analytics <- renderUI({
    if(file.exists("data/google-analytics.js") & curl::has_internet()){
      includeScript("data/google-analytics.js")
    }
  })

})

