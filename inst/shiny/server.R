#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
# Libraries ----
library(shiny)
library(shinyjs)
library(shinyhelper)

library(dplyr)
library(plotly)
# library(viridis)
library(data.table)
library(DT)
library(digest)
library(rdrop2)
library(curl)
library(config)
library(OpenSpecy)

# Required Data ----
conf <- config::get()

costs <- fread("data/costs.csv")
donations <- fread("data/donations.csv")
testdata <- raman_hdpe

# Check if spectral library is present and load ----
test_lib <- class(tryCatch(check_lib(path = ),
                           warning = function(w) {w}))
if(any(test_lib == "warning")) get_lib(path = conf$library_path)

spec_lib <- load_lib(path = conf$library_path)

# Check for Auth Tokens and setup ----
droptoken <- file.exists("data/droptoken.rds")

outputDir <- "Spectra"

if(droptoken) {
  drop_auth(rdstoken = "data/droptoken.rds")
}

# Name keys for human readable column names ----
namekey <- c(
  user_name = "User Name",
  contact_info = "Contact Info",
  organization = "Affiliation/Organization",
  citation = "Data Citation",
  spectrum_type = "Spectrum Type",
  spectrum_identity = "Material/Polymer",
  material_form = "Material Form",
  material_phase = "Material Phase",
  material_producer = "Material Producer",
  material_purity = "Material Purity",
  material_quality = "Material Quality",
  material_color = "Material Color",
  material_other = "Other Material Description",
  cas_number = "CAS number",
  instrument_used = "Instrument Used",
  instrument_accessories = "Instrument Accessories",
  instrument_mode = "Instrument Modes/Settings",
  spectral_resolution = "Spectral Resolution",
  laser_light_used = "Wavelength of Laser/Light",
  number_of_accumulations = "Number of Accumulations",
  total_acquisition_time_s = "Total Acquisition Time (s)",
  data_processing_procedure = "Data Processing Procedure",
  level_of_confidence_in_identification = "Level of Confidence in Identification",
  other_info = "Other information",
  other_information = "Other Material Description",
  color = "Material Color",
  rsq = "Pearson's r",
  sample_name = "Sample ID",
  smoother = "Smoother",
  baseline = "Basline",
  range = "Range"
)

# This is the actual server functions, all functions before this point are not reactive
server <- shinyServer(function(input, output, session) {

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

      sout <- tryCatch(share_spectrum(data(),
                         sapply(names(namekey)[1:24], function(x) input[[x]]),
                         share = share),
                       warning = function(w) {w})

      if (inherits(sout, "simpleWarning")) mess <- sout$message

      if (is.null(sout)) {
        show_alert(
          title = "Success!!",
          text = paste("Thank you for sharing your data! Your data will soon ",
                       "be available at https://osf.io/stmv4/"),
          type = "success"
        )
      } else {
        show_alert(
          title = "Something went wrong :-(",
          text = paste("All mandatory data added? R says:", mess, ".",
                       "Try again."),
          type = "warning"
        )
      }
  })

  # Helper icon requirement
  # Needs to run to add the little helper icons to the data
  observe_helpers(withMathJax = TRUE)


  # Read in data when uploaded based on the file type
  preprocessed_data <- reactive({
    req(input$file1)
    inFile <- input$file1
    filename <- as.character(inFile$datapath)

    shiny::validate(shiny::need(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
                                      ignore.case = T, filename),
                                "Uploaded data type is not currently supported please check help icon (?) and About tab for details on data formatting."))

    if (input$share_decision & curl::has_internet())
      share <- conf$share else share <- NULL

    if(grepl("\\.csv$", ignore.case = T, filename)) {
      read_text(inFile$datapath, method = "fread", share = share)
    }
    else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
      read_0(inFile$datapath, share = share)
    }
    else {
      ex <- strsplit(basename(filename), split="\\.")[[1]]
      do.call(paste0("read_", tolower(ex[-1])), list(inFile$datapath, share = share))
    }

  })

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    adjust_intensity(preprocessed_data(), type = input$IntensityCorr)
  })

  # Compute spectral resolution
  SpectralResolution <- reactive({
    (max(data()$wavenumber) - min(data()$wavenumber)) /
      length(data()$wavenumber)
  })

  # All cleaning of the data happens here. Smoothing and Baseline removing
  baseline_data <- reactive({
    testdata <- data() %>% dplyr::filter(wavenumber > input$MinRange & wavenumber < input$MaxRange)
    test <-  nrow(testdata) < 3
    if(test){
      data() %>%
        mutate(intensity = if(input$smoother != 0) {
          smooth_intensity(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline != 0) {
          subtract_background(.$wavenumber, .$intensity, degree = input$baseline)$intensity }
          else .$intensity)
    }
    else{
      data() %>%
        dplyr::filter(wavenumber > input$MinRange & wavenumber < input$MaxRange) %>%
        mutate(intensity = if(input$smoother != 0) {
          smooth_intensity(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline != 0) {
          subtract_background(.$wavenumber, .$intensity, degree = input$baseline)$intensity }
          else .$intensity)
    }
  })

  # Create file view and preprocess view
  output$MyPlot <- renderPlotly({
    plot_ly(data(), type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~wavenumber, y = ~intensity, name = 'Uploaded Spectrum',
                line = list(color = 'rgba(240,236,19, 0.8)')) %>%
      layout(yaxis = list(title = "Absorbance Intensity"),
             xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0,73)',
             paper_bgcolor='black', font = list(color = '#FFFFFF'))
  })
  output$MyPlotB <- renderPlotly({
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(data = baseline_data(), x = ~wavenumber, y = ~intensity,
                name = 'Processed Spectrum', line = list(color = 'rgb(240,19,207)')) %>%
      add_trace(data = data(), x = ~wavenumber, y = ~intensity,
                name = 'Uploaded Spectrum', line = list(color = 'rgba(240,236,19,0.8)')) %>%
      # Dark blue rgb(63,96,130)
      # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
      layout(yaxis = list(title = "Absorbance Intensity"),
             xaxis = list(title = "Wavenumber (1/cm)"),
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

      Lib <- match_spectrum(DataR(),
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
              filter = "top",caption = "Selectable Matches", style = "bootstrap",
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
                        MatchSpectra()[[input$event_rows_selected, "sample_name"]])
    # Get data from find_spectrum
    current_meta <- find_spectrum(sample_name == id_select,
                                  spec_lib, which = input$Spectra)
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
                  line = list(color = 'rgba(255,255,255,0.8)'))%>%
        layout(yaxis = list(title = "Absorbance Intensity"),
               xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0, 73)',
               paper_bgcolor='black', font = list(color = '#FFFFFF'))
    }
    else if(length(input$event_rows_selected)) {
      # Default to first row if not yet clicked
      id_select <- ifelse(is.null(input$event_rows_selected),
                          1,
                          MatchSpectra()[[input$event_rows_selected, "sample_name"]])
      # Get data from find_spectrum
      current_spectrum <- find_spectrum(sample_name == id_select,
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
        layout(yaxis = list(title = "Absorbance Intensity"),
               xaxis = list(title = "Wavenumber (1/cm)"),
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
    filename = function() {paste('data-', human_timestamp(), '.csv', sep='')},
    content = function(file) {fwrite(baseline_data(), file)}
  )

  ## Sharing data ----
  # Hide functions which shouldn't exist when there is no internet or
  # when the API token doesn't exist
  observe({
    if((conf$share == "dropbox" & droptoken) | curl::has_internet()) {
      show("share_decision")
      show("share_meta")
      show("helper1")
    }
    else {
      hide("share_decision")
      hide("share_meta")
      hide("helper1")
    }
  })

  observe({
    if (input$share_decision) {
      show("share_meta")
      } else {
        hide("share_meta")
      }
  })

  observeEvent(input$share_meta, {
    toggle("user_name")
    toggle("contact_info")
    toggle("organization")
    toggle("citation")
    toggle("spectrum_type")
    toggle("spectrum_identity")
    toggle("material_form")
    toggle("material_phase")
    toggle("material_producer")
    toggle("material_purity")
    toggle("material_quality")
    toggle("material_color")
    toggle("material_other")
    toggle("cas_number")
    toggle("instrument_used")
    toggle("instrument_accessories")
    toggle("instrument_mode")
    toggle("spectral_resolution")
    toggle("laser_light_used")
    toggle("number_of_accumulations")
    toggle("total_acquisition_time_s")
    toggle("data_processing_procedure")
    toggle("level_of_confidence_in_identification")
    toggle("other_info")
    toggle("submit")
  })

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
