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

if(droptoken){
  drop_auth(rdstoken = "data/droptoken.rds")
}

# Name keys for human readable column names ----
namekey <- c(spectrum_identity = "Material",
             material_form = "Material Form",
             material_producer = "Material Producer",
             material_purity = "Material Purity",
             material_quality = "Material Quality",
             material_color = "Material Color",
             material_other = "Other Material Form Description",
             cas_number = "CAS",
             organization = "Organization",
             contact_info = "Contact Info",
             spectrum_type = "Spectrum Type",
             sample_name = "Sample Name",
             rsq = "Pearson's r",
             instrument_used = "Instrument Used",
             instrument_accessories = "Instrument Accessories",
             instrument_mode = "Instrument Mode",
             spectral_resolution = "Spectral Resolution",
             laser_light_used = "Laser or Light Used",
             total_acquisition_time_s = "Total Acquisition Time",
             number_of_accumulations = "Number of Accumulations",
             data_processing_procedure = "Data Processing Procedure",
             level_of_confidence_in_identification = "Level of Confidence in Identification",
             other_information = "Other Information",
             "User Name", "Affiliation", "Data Citation", "smoother", "baseline",
             "range" # small bug, range is getting replaced with the datetime somehow
)
# Keep here for now
fieldsAll <- unname(namekey)

# This is the actual server functions, all functions before this point are not reactive
server <- shinyServer(function(input, output, session) {

  # For desktop version of the app.
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  # File sharing functions ----
  # Share if share is selected on upload
  observeEvent(input$file1, {
    if(input$ShareDecision == "Share" & curl::has_internet() & droptoken){
      withProgress(message = 'Sharing Spectrum to Community Library', value = 3/3, {
        inFile <- input$file1
        UniqueID <- digest::digest(preprocesseddata(), algo = "md5")
        drop_upload(inFile$datapath, path = paste0(outputDir, "/", UniqueID), mode = "add")
      })
    }
  })

  #Save data to cloud
  saveData <- function(data, UniqueID) {
    # Create a unique file name
    fileName <- paste0(paste(human_timestamp(), UniqueID, sep = "_"), ".csv")#sprintf("%s_%s.csv", as.integer(Sys.time()), digest(data))
    # Write the data to a temporary file locally
    filepath <- file.path(tempdir(), fileName)
    write.csv(data, filepath, row.names = FALSE, quote = TRUE)
    # Upload the file to dropbox
    UniqueID <- digest::digest(preprocesseddata(), algo = "md5")
    drop_upload(filepath, path = outputDir)
  }

  # How to save the metadata
  saveDataForm <- function(data, UniqueID) {
    data <- data.frame(input = data, variable = c(fieldsAll))
    # Create a unique file name
    fileName <- paste0(paste(human_timestamp(), UniqueID, "form", sep = "_"), ".csv")#sprintf("%s_%s.csv", as.integer(Sys.time()), digest(data))
    # Write the data to a temporary file locally
    filepath <- file.path(tempdir(), fileName)
    write.csv(data, filepath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filepath, path = outputDir)
  }

  # Compile metadata input
  formData <- reactive({
    data <- sapply(fieldsAll[1:(length(fieldsAll)-1)], function(x) input[[x]])
    data <- c(data, paste(input[[fieldsAll[length(fieldsAll)]]][1],
                          input[[fieldsAll[length(fieldsAll)]]][2], sep = "_"),
              timestamp = human_timestamp())
    data
  })

  # Save the metadata and data submitted upon pressing the button
  observeEvent(input$submit,{
    if(curl::has_internet() & droptoken){
      UniqueID <- digest::digest(data(), algo = "md5")
      saveDataForm(formData(), UniqueID)
      saveData(data(), UniqueID)
      shinyjs::alert(paste("Thank you for sharing your data! Your data will soon be available @ https://osf.io/stmv4/")) #Your Fortune For Today is:  ", sample(Fortunes$Fortunes, 1), sep = ""))
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

    if(grepl("\\.csv$", ignore.case = T, filename)) {
      read_text(inFile$datapath, method = "fread")
    }
    else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
      read_0(inFile$datapath)
    }
    else {
      ex <- strsplit(basename(filename), split="\\.")[[1]]
      do.call(paste0("read_", tolower(ex[-1])), list(inFile$datapath))
    }

  })

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    adjust_intensity(preprocessed_data(), type = tolower(input$IntensityCorr))
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
                            library = spec_lib, which = tolower(input$Spectra),
                            type = tolower(input$Library), top_n = 100)

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
                                  spec_lib, which = tolower(input$Spectra))
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
                                        spec_lib, which = tolower(input$Spectra),
                                        type = tolower(input$Library))

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

  output$downloadData7 <- downloadHandler(
    filename = function() {"testdata.csv"},
    content = function(file) {fwrite(testdata, file)}
  )

  ## Download their own data ----
  output$downloadData <- downloadHandler(
    filename = function() {paste('data-', human_timestamp(), '.csv', sep='')},
    content = function(file) {fwrite(baseline_data(), file)}
  )

  # Hide functions which shouldn't exist when there is no internet or when the API token doesnt exist ----
  observe({
    if(droptoken & curl::has_internet()) {
      show("ShareDecision")
      show("btn")
      show("helper1")
    }
    else{
      hide("ShareDecision")
      hide("btn")
      hide("helper1")
    }
  })

  observeEvent(input$btn, {
    toggle("User Name")
    toggle("Contact Info")
    toggle("Affiliation")
    toggle("Data Citation")
    toggle("Spectrum Identity")
    toggle("Spectrum Type")
    toggle("Color")
    toggle("CAS Number")
    toggle("Material Producer")
    toggle("Material Phase")
    toggle("Material Form")
    toggle("Other Material Form Description")
    toggle("Material Purity")
    toggle("Material Quality")
    toggle("Instrument Used")
    toggle("Instrument Accessories")
    toggle("Instrument Mode")
    toggle("Spectral Resolution")
    toggle("LaserLight Used")
    toggle("Number of Accumulations")
    toggle("Total Acquisition Time")
    toggle("Data Processing Procedure")
    toggle("Level of Confidence in Identification")
    toggle("Description of Identification")
    toggle("Other Information")
    toggle("submit")
  })

  output$translate <- renderUI({
    if(file.exists("www/googletranslate.html") & curl::has_internet()){
      includeHTML("www/googletranslate.html")
    }
    else{
      NULL
    }
  })

  output$analytics <- renderUI({
    if(file.exists("data/google-analytics.js") & curl::has_internet()){
      includeScript("data/google-analytics.js")
    }
    else{
      NULL
    }
  })

})
