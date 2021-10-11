#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#'
# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("data/droptoken.rds")
db <- file.exists(".db_url")
translate <- file.exists("www/googletranslate.html")

# Libraries ----
library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
# library(viridis)
library(data.table)
library(DT)
library(digest)
library(curl)
library(config)
library(mongolite)
library(loggit)

if(droptoken) library(rdrop2)

#devtools::install_github("wincowgerDEV/OpenSpecy")
library(OpenSpecy)

#library(future)
#library(bslib)

# Global config ----
conf <- config::get()

# Logging ----
if(conf$log) {
  if(db) {
    database <- mongo(url = readLines(".db_url"))
  } else {
    set_logfile(file.path(tempdir(), "OpenSpecy.log"))
  }
}

# Load all data ----
load_data <- function() {
  costs <- fread("data/costs.csv")
  donations <- fread("data/donations.csv")
  testdata <- raman_hdpe

  goals <- tibble(
    Status =      c("Revolutionizing", 
                    "Thriving", 
                    "Maintaining", 
                    "Supporting", 
                    "Saving"),
    Description = c("A paid team that is pushing Open Specy closer to the ultimate goal of 100% accurate spectral identification and deep spectral diagnostics with a single click",
                    "A single paid staff person working to update and build the community and the tool",
                    "Maintenance costs and minor ad-hoc updates and bug fixes",
                    "Keeping the app online and essential maintenance",
                    "Long term storage only"),
    'Annual Need'  = c(">100,000$",
                       "10,000–100,000$",
                       "1,000–10,000$",
                       "100–1,000$",
                       "<100$")
  )
  # Check if spectral library is present and load
  test_lib <- class(tryCatch(check_lib(path = conf$library_path),
                             warning = function(w) {w}))

  if(any(test_lib == "warning")) get_lib(path = conf$library_path)

  spec_lib <- load_lib(path = conf$library_path)

  if(droptoken) {
    drop_auth(rdstoken = "data/droptoken.rds")
  }

  # Name keys for human readable column names
  load("data/namekey.RData")

  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}

# This is the actual server functions, all functions before this point are not
# reactive
server <- shinyServer(function(input, output, session) {
  #For theming
  #bs_themer()

  session_id <- digest(runif(10))

  # Loading overlay
  load_data()
  hide(id = "loading_overlay", anim = TRUE, animType = "fade")
  show("app_content")

# For desktop version of the app.
#  if (!interactive()) {
#    session$onSessionEnded(function() {
#      stopApp()
#      q("no")
#    })
#  }
  
  #brks <- seq(5, 320000, 1000)
  clrs <- colorRampPalette(c("white", "#6baed6"))(5 + 1)
  
  output$event_goals <- DT::renderDataTable({
    datatable(goals,
              options = list(
                             dom = "t",
                             ordering = FALSE,
                             paging = FALSE,
                             searching = FALSE
                             #sDom  = '<"top">lrt<"bottom">ip',
                             
                             ),
              caption = "Progress (current staus selected)",
              style = "bootstrap",
              class = 'row-border',
              escape = FALSE,
              rownames = FALSE,
              #formatStyle(c("Annual Need"), backgroundColor = styleColorBar(color = clrs)),
              selection = list(mode = "single", selected = c(2)))
  })
  
  #Reading Data and Startup ----
  # Sharing ID
  id <- reactive({
    if (!is.null(input$fingerprint)) {
      paste(input$fingerprint, session_id, sep = "/")
    } else {
      paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")
    }
  })

  # Save the metadata and data submitted upon pressing the button
  observeEvent(input$submit, {
    if (input$share_decision & !is.null(data()) & curl::has_internet()) {
      withProgress(message = "Sharing Metadata",
                   value = 3/3, {
        sout <- tryCatch(share_spec(
          data = preprocessed_data(),
          metadata = sapply(names(namekey)[c(1:24,32)], function(x) input[[x]]),
          share = conf$share,
          id = id()),
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
    }
  })

  # Read in data when uploaded based on the file type
  preprocessed_data <- reactive({
    req(input$file1)
    file <- input$file1
    filename <- as.character(file$datapath)

    if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
              ignore.case = T, filename)) {
      show_alert(
        title = "Data type not supported!",
        text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
        type = "warning")
      return(NULL)
      }

    if (input$share_decision & curl::has_internet()) {
      share <- conf$share
      progm <- "Sharing Spectrum to Community Library"
      } else {
        share <- NULL
        progm <- "Reading Spectrum"
      }

    withProgress(message = progm, value = 3/3, {
      if(grepl("\\.csv$", ignore.case = T, filename)) {
        rout <- tryCatch(read_text(filename, method = "fread",
                                   share = share,
                                   id = id()),
                         error = function(e) {e})
      }
      else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
        rout <- tryCatch(read_0(filename, share = share, id = id()),
                         error = function(e) {e})
      }
      else {
        ex <- strsplit(basename(filename), split="\\.")[[1]]

        rout <- tryCatch(do.call(paste0("read_", tolower(ex[-1])),
                                 list(filename, share = share, id = id())),
                         error = function(e) {e})
      }

      if (inherits(rout, "simpleError")) {
        reset("file1")
        show_alert(
          title = "Something went wrong :-(",
          text = paste0("R says: '", rout$message, "'. ",
                        "If you uploaded a text/csv file, make sure that the ",
                        "columns are numeric and named 'wavenumber' and ",
                        "'intensity'."),
          type = "error"
        )
        return(NULL)
      } else {
        rout
      }
    })
  })

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    req(preprocessed_data())
    adj_intens(preprocessed_data(), type = input$intensity_corr)
    })

  #Preprocess Spectra ----
  # All cleaning of the data happens here. Smoothing and Baseline removing
  baseline_data <- reactive({
    req(data())

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
        mutate(intensity = if(input$baseline_decision & input$baseline_selection == "Polynomial") {
          subtr_bg(.$wavenumber, .$intensity, degree = input$baseline)$intensity
          }
          else if(input$baseline_decision & input$baseline_selection == "Manual" & !is.null(trace$data)){
            make_rel(.$intensity - approx(trace$data$wavenumber, trace$data$intensity, xout = .$wavenumber, rule = 2, method = "linear", ties = mean)$y)
          } else .$intensity)
    }
  })

  # Create file view and preprocess view
  output$MyPlot <- renderPlotly({
    plot_ly(data(), type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~wavenumber, y = ~intensity, name = 'Uploaded Spectrum',
                line = list(color = 'rgba(240,236,19, 0.8)')) %>%
      layout(yaxis = list(title = "absorbance intensity [-]"),
             xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                          autorange = "reversed"),
             plot_bgcolor = 'rgb(17,0,73)',
             paper_bgcolor = 'rgba(0,0,0,0.5)',
             font = list(color = '#FFFFFF'))
  })

  output$MyPlotB <- renderPlotly({
    plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
      add_trace(data = baseline_data(), x = ~wavenumber, y = ~intensity,
                name = 'Processed Spectrum',
                line = list(color = 'rgb(240,19,207)')) %>%
      add_trace(data = data(), x = ~wavenumber, y = ~intensity,
                name = 'Uploaded Spectrum',
                line = list(color = 'rgba(240,236,19,0.8)')) %>%
      # Dark blue rgb(63,96,130)
      # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
      layout(yaxis = list(title = "absorbance intensity [-]"),
             xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                          autorange = "reversed"),
             plot_bgcolor = 'rgb(17,0,73)',
             paper_bgcolor = 'rgba(0,0,0,0.5)',
             font = list(color = '#FFFFFF')) %>%
      config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape" ))
  })

trace <- reactiveValues(data = NULL)

observeEvent(input$go, {
  pathinfo <- event_data(event = "plotly_relayout", source = "B")$shapes$path
  if (is.null(pathinfo)) trace$data <- NULL
  else {
   nodes <- unlist(strsplit(
             gsub("(L)|(M)", "_",
                  paste(unlist(pathinfo), collapse = "")),
             "(,)|(_)"))
   nodes = nodes[-1]
   df <- as.data.frame(matrix(nodes, ncol = 2, byrow = T))
   names(df) <- c("wavenumber", "intensity")
   trace$data <- df
  }
})

observeEvent(input$reset, {
  #js$resetClick()
  #runjs("Shiny.setInputValue('plotly_selected-B', null);")
  trace$data <- NULL
})

#  output$text <- renderPrint({
#   trace$data#
#    })

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
              filter = "top", caption = "Selectable Matches",
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
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor='rgb(17,0, 73)',
               paper_bgcolor= 'rgba(0,0,0,0.5)',
               font = list(color = '#FFFFFF'))
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
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = "rgb(17,0, 73)",
               paper_bgcolor = 'rgba(0,0,0,0.5)',
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
    filename = function() {"ftir_metadata.csv"},
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
        sapply(names(namekey)[c(1:24,32)], function(x) hide(x))
        hide("submit")
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
    if (input$baseline_selection == "Polynomial") {
      show("baseline")
      hide("go")
      hide("reset")
    } else {
      hide("baseline")
      show("go")
      show("reset")
    }
  })

  observe({
    if (input$range_decision) {
      show("range_tools")
    } else {
      hide("range_tools")
    }
  })

  observe({
    if (is.null(preprocessed_data())) {
      show("placeholder1")
      show("placeholder2")
      show("placeholder3")
    } else {
      hide("placeholder1")
      hide("placeholder2")
      hide("placeholder3")
    }
  })

  #This toggles the hidden metadata input layers.
  observeEvent(input$share_meta, {
    sapply(names(namekey)[c(1:24,32)], function(x) toggle(x))
    toggle("submit")
  })

  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })

  # Log events ----

observeEvent(input$go, {
  if(conf$log) {
    if(db) {
      database$insert(data.frame(user_name = input$fingerprint,
                                 session_name = session_id,
                                 wavenumber = trace$data$wavenumber,
                                 intensity = trace$data$intensity,
                                 data_id = digest::digest(preprocessed_data(),
                                                          algo = "md5"),
                                 ipid = input$ipid,
                                 time = human_ts()))
      }
  }
})

  observe({
    req(input$file1)
    req(input$share_decision)
    if(conf$log) {
      if(db) {
        database$insert(data.frame(user_name = input$fingerprint,
                                   session_name = session_id,
                                   intensity_adj = input$intensity_corr,
                                   smoother = input$smoother,
                                   smooth_decision = input$smooth_decision,
                                   baseline = input$baseline,
                                   baseline_decision = input$baseline_decision,
                                   max_range = input$MinRange,
                                   min_range = input$MaxRange,
                                   range_decision = input$range_decision,
                                   data_id = digest::digest(preprocessed_data(),
                                                            algo = "md5"),
                                   spectra_type = input$Spectra,
                                   analyze_type = input$Data,
                                   region_type = input$Library,
                                   ipid = input$ipid,
                                   time = human_ts()))
      } else {
        loggit("INFO", "trigger",
               user_name = input$fingerprint,
               session_name = session_id,
               intensity_adj = input$intensity_corr,
               smoother = input$smoother,
               smooth_decision = input$smooth_decision,
               baseline = input$baseline,
               baseline_decision = input$baseline_decision,
               max_range = input$MinRange,
               min_range = input$MaxRange,
               range_decision = input$range_decision,
               data_id = digest::digest(preprocessed_data(), algo = "md5"),
               spectra_type = input$Spectra,
               analyze_type = input$Data,
               region_type = input$Library,
               ipid = input$ipid,
               time = human_ts())
      }
    }

  })

})

