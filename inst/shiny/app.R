#' @param input provided by shiny
#' @param output provided by shiny
#'
#'
# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.

#Ideas
# see https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel/36426258

# Global config ----
conf <- config::get()
droptoken <- file.exists("data/droptoken.rds")
db <- FALSE#file.exists(".db_url") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")
# Name keys for human readable column names
load("data/namekey.RData")

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
library(shiny.i18n)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(purrr)

if(droptoken) library(rdrop2)

#devtools::install_github("wincowgerDEV/OpenSpecy")
library(OpenSpecy)

# Logging ----
if(conf$log) {
    if(db) {
        database <- mongo(url = readLines(".db_url"))
    } else {
        set_logfile(file.path(tempdir(), "OpenSpecy.log"))
    }
}

# Commands for translator
i18n <- Translator$new(translation_json_path = "languages/json_translation.json")
i18n$set_translation_language("en")

#source functions
source("scripts/all_functions.R")

# UI ----
ui <- tagList(
    #Script for all pages ----
    shiny.i18n::usei18n(i18n),
    shinyjs::useShinyjs(), # Required for any of the shinyjs functions.
    #extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }", functions = "resetClick"),
    inputIp("ipid"),
    inputUserid("fingerprint"),
    # tags$head(uiOutput("name_get")),
    tags$head(tags$style(css),
              tags$script(async = NA, src = "https://platform.twitter.com/widgets.js"),
              tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")), 
              tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
    #theme = bs_theme(fg = "#F9FBFA", bootswatch = "cyborg", bg = "#060606"),
    theme = shinytheme("cyborg"), # Change this for other themes
    
    setBackgroundImage("jumbotron.png"),
    
    shinyjs::inlineCSS(appCSS),
    
    #Startup ----
    div(
        id = "loading_overlay",
        h2("Loading Open Specy"),
        br(),
        tags$div(
            style="margin-bottom:20%;",
        ),
        h4(icon("spinner", class = "btn-loading-indicator fa-spin")),
        br(),
        h6("If you start Open Specy for the first time, this may take a while ...")),
    
    hidden(div(id = "app_content",
               uiOutput("ui")
               )))

# Server ----
server <- shinyServer(function(input, output, session){
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
                  caption = "Progress (current status selected)",
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

    output$tweets <- renderUI({
        map(tweets, ~ render_tweet(.x))
    })
    
    # Translate page ----
    observeEvent(input$selected_language, {
        update_lang(session, input$selected_language)
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
    
    #Render UI ----
    output$ui <- renderUI({
        #Title Panel ----
        titlePanel(
            fluidRow(
                column(10, align = "left", img(src = "logo.png", width = 300, height = 75)),
                column(2, align = "right", 
                       div(style = "width: 90%;
                            padding: 15px;
                            font-size: 14pt;
                            border-radius: 0;
                            outline: none;
                            border: none;
                            text-align:left !important;",
                           selectInput(
                               inputId="selected_language",
                               label=i18n$t("Change language"),
                               choices = i18n$get_languages(),
                               selected = i18n$get_key_translation(),
                               width = "200px"
                           )
                       )
                )#uiOutput("translate")) # Google Translate
            ), windowTitle = "Open Specy"
        )
        # About Tab ----
        tabsetPanel(id = "tabs",
                    tabPanel(i18n$t("About"), value = "tab0",
                             containerfunction(
                                 h2(i18n$t("Welcome")),
                                 p(class = "lead", i18n$t("Join the hundreds of researchers from around 
                               the world who are part of the Open Specy community by 
                               analyzing, sharing, processing, and identifying 
                               their Raman and IR spectra. These services are
                               free and open source thanks to our partners:")),
                               fluidRow(
                                   column(6, img(src = "dancing.jpg", width = "100%")
                                   ),
                                   column(6,                            
                                          h3(i18n$t("Monetary Partners")),
                                          panel(style = "overflow-y:scroll; max-height: 300px;  align: centre",
                                                
                                                div(class = "jumbotron",
                                                    style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                              background-color:rgb(255, 215, 0, 0.5)",
                              h3(i18n$t("Revolutionizing (>100,000$)"))
                                                ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                               h3(i18n$t("Thriving (10,000–100,000$)")),
                               img(src = "https://mooreplasticresearch.org/wp-content/uploads/2021/06/HorizontalLogo-FullName-1.png", style = "padding:1rem", height = 100),
                               h4("Mcpike Zima Charitable Foundation")
                              ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                               h3(i18n$t("Maintaining (1,000–10,000$)")),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/UC_Riverside_logo.svg/1024px-UC_Riverside_logo.svg.png", style = "padding:1rem", height = 50),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7e/NSF_logo.png", style = "padding:1rem", height = 50),
                               img(src = "https://www.awi.de/typo3conf/ext/sms_boilerplate/Resources/Public/Images/AWI/awi_logo.svg", style = "padding:1rem",  height = 50),
                               img(src = "https://www.hpu.edu/_global/images/header-logo.png", style = "padding:1rem",  height = 50),
                               img(src = "https://www.nist.gov/libraries/nist-component-library/dist/img/logo/nist_logo_sidestack_rev.svg", style = "padding:1rem",  height = 50),
                               img(src = "https://www.utoronto.ca/sites/all/themes/uoft_stark/img/U-of-T-logo.svg", style = "padding:1rem",  height = 50),
                               img(src = "https://www.uni-koblenz-landau.de/logo.png", style = "padding:1rem",  height = 50),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Thermo_Fisher_Scientific_logo.svg/2560px-Thermo_Fisher_Scientific_logo.svg.png", style = "padding:1rem", height = 50)
                              ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                               h3(i18n$t("Supporting (100–1,000$)")),
                               h5( "Jennifer Gadd")
                              ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(128, 0, 128, 0.5)",
                               h3(i18n$t("Saving (<100$)")),
                               h6("Susanne Brander (Oregon State University), Jeremy Conkle (TEXAS  A&M  UNIVERSITY  CORPUS  CHRISTI)")
                              )
                                          ),
                              h3(i18n$t("In-Kind Partners")),
                              panel(style = "overflow-y:scroll; max-height: 300px;  align: centre",
                                    div(class = "jumbotron",
                                        style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                              background-color:rgb(255, 215, 0, 0.5)",
                              h3(i18n$t("Revolutionizing (>100,000$)"))
                                    ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                               h3(i18n$t("Thriving (10,000–100,000$)")),
                               h4("Win Cowger, Zacharias Steinmetz")
                              ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                               h3(i18n$t("Maintaining (1,000–10,000$)")),
                               h5("Sebastian Primpke, Andrew Gray, Chelsea Rochman, Orestis Herodotu, Hannah De Frond, Keenan Munno, Hannah Hapich, Jennifer Lynch")
                              ),
                              div(class = "jumbotron",
                                  style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                               h3(i18n$t("Supporting (100–1,000$)")),
                               h6( "Shreyas Patankar, Andrea Faltynkova, Alexandre Dehaut, Gabriel Erni Cassola, Aline Carvalho")
                              )
                              )
                                   )
                              
                               )
                             ),
                             containerfunction(
                                 h2(i18n$t("Testimonials")),#
                                 panel(style = "overflow-y:scroll; max-height: 400px;  align: centre",
                                       uiOutput("tweets")#,
                                       
                                 )
                                 
                             ),
                             containerfunction(
                                 h2(i18n$t("Quick Video Tutorial")),
                                 HTML("<iframe width='560' height='315' src='https://www.youtube-nocookie.com/embed/w55WGtV2Dz4' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                             ),
                             containerfunction(
                                 h2(i18n$t("Instructions")),
                                 p(class = "lead", i18n$t("In Brief: To use the tool upload a csv, asp, jdx, spc, or spa file to the upload file tab.
                                  If csv, one column should be named 'wavenumber' (in units of 1/cm) and another named 'intensity'.
                                  You can smooth your data using an SG filter, baseline correct your data using the polynomial order of iModPolyFit, and restrict the wavelength range for the match.
                                  The result will be compared to an internal Raman or FTIR spectra library. The strongest 1000 matches along with your
                                  uploaded or processed data will be presented in an interactive plot and table. For more details click the button below
                                  or watch the detailed instructional video.")),
                                 a(i18n$t("Detailed Standard Operating Procedure"),
                                   onclick = "window.open('https://cran.r-project.org/web/packages/OpenSpecy/vignettes/sop.html', '_blank')",
                                   class="btn btn-primary btn-lg"),
                                 br(),
                                 br(),
                                 HTML("<iframe width='560' height='315' src='https://www.youtube-nocookie.com/embed/JjhCdhjdcRY' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                                 
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Download Open Data")),
                                 p(class = "lead", i18n$t("Reference spectra was sourced from open access resources 
                                online, peer reviewed publications, and corporate donations. In the future, 
                                spectra that is uploaded to the tool will be incorporated to the reference 
                                library to make it even better.")),
                                div(
                                    downloadButton("downloadData6", i18n$t("Raman Reference Library"), style = "background-color: #2a9fd6;"),
                                    downloadButton("downloadData5", i18n$t("FTIR Reference Library"), style = "background-color: #2a9fd6;"),
                                    downloadButton("downloadData4", i18n$t("Raman Reference Library Metadata"), style = "background-color: #2a9fd6;"),
                                    downloadButton("downloadData3", i18n$t("FTIR Reference Library Metadata"), style = "background-color: #2a9fd6;")
                                )
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Contribute Spectra")),
                                 p(class = "lead", i18n$t("To share spectra upload a file to the upload file tab. 
                             If you selected Share a copy of your spectra will be sent to the Community 
                             Data Warehouse on Open Science Framework. To add additional metadata, 
                             fill in the avaliable metadata fields and click -Share Data-. The 
                             spectra file that you uploaded along with your responses will be copied 
                             to the a -With Metadata- subfolder at the link below. All shared data holds 
                             a Creative Commons Attribution License 4.0.")),
                             div(
                                 a(i18n$t("Community Data Warehouse"),
                                   onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                   class="btn btn-primary btn-lg")
                             )
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Tool Validation")),
                                 p(class = "lead", i18n$t("All parameters in this tool are tested to validate that 
                                the tool is functioning as best as possible and determine the best default 
                                parameters to use. Our current validation proceedure includes correcting 
                                duplicated entries in the reference libraries, checking for spectra in 
                                metadata that isn't in the spectral library, and ensuring the the default 
                                parameters provide over 80% accuracy in the first match.")
                                 ),
                                div(
                                    a(i18n$t("Detailed Validation Procedure"),
                                      onclick = "window.open('https://docs.google.com/document/d/1Zd2GY4bWIwegGeE4JpX8O0S5l_IYju0sLDl1ddTTMxU/edit?usp=sharing', '_blank')",
                                      class="btn btn-primary btn-lg")
                                )
                             ),
                             
                             
                             containerfunction(
                                 h2(i18n$t("Updates, Feature Requests, and Bug Reports")),
                                 p(class = "lead", i18n$t("We keep track of all updates using version control on our code. Features can be requested and bug reported on GitHub.")),
                                 div(
                                     a(i18n$t("Updates, Feature Requests, Bug Reports"),
                                       onclick = "window.open('https://github.com/wincowgerDEV/OpenSpecy', '_blank')",
                                       class="btn btn-primary btn-lg")
                                 )
                                 
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Stay up to date!")),
                                 p(class = "lead", i18n$t("Follow us on Twitter @OpenSpecy. E-mail wincowger@gmail.com to be added to the mailing list."))
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Citation")),
                                 p(class = "lead", citation)
                             ),
                             
                             containerfunction(
                                 h2("Useful Links"),
                                 a(href = "https://simple-plastics.eu/", i18n$t("Free FTIR Software: siMPle microplastic IR spectral identification software"), class = "lead"),
                                 p(),
                                 a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", i18n$t("Free Spectroscopy Learning Academy from ThermoFisher"), class = "lead"),
                                 p(),
                                 a(href = "https://micro.magnet.fsu.edu/primer/", i18n$t("Free Optical Microscopy Learning Resource from Florida State University"), class = "lead"),
                                 p(),
                                 a(href = "https://www.effemm2.de/spectragryph/index.html", i18n$t("Free desktop application for spectral analysis and links to reference databases."), class = "lead")
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Terms And Conditions")),
                                 pre(includeText("www/TOS.txt"))
                             ),
                             
                             containerfunction(
                                 h2(i18n$t("Privacy Policy")),
                                 pre(includeText("www/privacy_policy.txt"))
                             ),
                    ),
                    
                    #Upload File Tab ----
                    tabPanel(i18n$t("Upload File"), value = "tab1",
                             titlePanel(h4(i18n$t("Upload, View, and Share Spectra"))),
                             br(),
                             fluidRow(
                                 column(3, style = columnformat(),
                                        tags$label(i18n$t("Choose .csv (preferred), .asp, .jdx, .spc, .spa, or .0 File")),
                                        
                                        prettySwitch("share_decision",
                                                     label = i18n$t("Share Your Data?"),
                                                     inline = T,
                                                     value = T,
                                                     status = "success",
                                                     fill = T),
                                        bsPopover(
                                            id = "share_decision",
                                            title = "Share Help",
                                            content = "If you like, we share your uploaded spectra and settings with the spectroscopy community.
                                              By default, all data will be licensed under Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0).
                                              Uploaded spectra will appear here: https://osf.io/rjg3c",
                                            placement = "bottom",
                                            trigger = "hover"
                                        ),
                                        
                                        fileInput("file1", NULL,
                                                  placeholder = ".csv, .asp, .jdx, .spc, .spa, .0",
                                                  accept=c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv", ".asp", ".spc", ".jdx", ".spa", ".0")),
                                        bsPopover(
                                            id = "file1",
                                            title = "Upload Help",
                                            content = "Upload Raman or FTIR spectrum files as a csv, jdx, spc, or spa. A csv file is preferred. If a csv, the file must contain one column labeled 'wavenumber' in units of (1/cm) and another column labeled 'intensity' in absorbance units.
                                            If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). These files will not always work perfectly because they are tricky to read so double check them in another software.
                                            Hit the 'Test Data' button to download a sample Raman spectrum.",
                                            placement = "bottom",
                                            trigger = "click"
                                        ),
                                        
                                        radioButtons("intensity_corr", "Intensity Adjustment",
                                                     c("None" = "none",
                                                       "Transmittance" = "transmittance", "Reflectance" = "reflectance")),
                                        bsPopover(
                                            id = "intensity_corr",
                                            title = "Intensity Correction Help",
                                            content = "If the uploaded spectrum is not in absorbance units, 
                                              use this input to specify the units to convert from.The transmittance adjustment 
                                              uses the log10(1/T) calculation which does not correct for system 
                                              and particle characteristics. The reflectance adjustment uses the 
                                              Kubelka-Munk equation (1-R)2/(2*R). We assume that the reflectance 
                                              is formatted as a percent from 1-100 and first correct the intensity by dividing by 100
                                              so that it fits the form expected by the equation.
                                              If none is selected, Open Specy assumes that the uploaded data is 
                                              an absorbance spectrum.",
                                            placement = "bottom",
                                            trigger = "hover"
                                        ),
                                        
                                        tags$br(),
                                        
                                        tags$div(downloadButton("download_testdata", i18n$t("Sample File"), style = "background-color: #2a9fd6;")),
                                        bsPopover(
                                            id = "download_testdata",
                                            title = "Sample Data Help",
                                            content = "This is a sample spectrum that can be uploaded to the tool for testing it out and understanding how the csv files should be formatted.",
                                            placement = "bottom",
                                            trigger = "hover"
                                        ),
                                        
                                        tags$br(),
                                        
                                        actionButton("share_meta", i18n$t("Metadata Input"), style = "background-color: #2a9fd6;"),
                                        bsPopover(
                                            id = "share_meta",
                                            title = "Metadata Help",
                                            content = "We share any uploaded spectra and metadata with the spectroscopy community if you fill out the metadata here and select share.
                                              Uploaded spectra and metadata will appear here: https://osf.io/rjg3c",
                                            placement = "bottom",
                                            trigger = "hover"
                                        ),
                                        
                                        hidden(
                                            textInput(names(namekey)[1],
                                                      labelMandatory(namekey[1]),
                                                      placeholder = "e.g. Win Cowger"),
                                            textInput(names(namekey)[2],
                                                      label = namekey[2],
                                                      placeholder = "e.g. 1-513-673-8956, wincowger@gmail.com"),
                                            textInput(names(namekey)[3],
                                                      label = namekey[3],
                                                      placeholder = "e.g. University of California, Riverside"),
                                            textInput(names(namekey)[4],
                                                      label = namekey[4],
                                                      placeholder = "e.g. Primpke, S., Wirth, M., Lorenz, C., & Gerdts, G. (2018). Reference database design for the automated analysis of microplastic samples based on Fourier transform infrared (FTIR) spectroscopy. Analytical and Bioanalytical Chemistry. doi: 10.1007/s00216-018-1156-x"),
                                            textInput(names(namekey)[5],
                                                      labelMandatory(namekey[5]),
                                                      placeholder = "Raman or FTIR"),
                                            textInput(names(namekey)[6],
                                                      labelMandatory(namekey[6]),
                                                      placeholder = "e.g. polystyrene"),
                                            textInput(names(namekey)[7],
                                                      label = namekey[7],
                                                      placeholder = "e.g. textile fiber, rubber band, sphere, granule"),
                                            textInput(names(namekey)[8],
                                                      label = namekey[8],
                                                      placeholder = "liquid, gas, solid"),
                                            textInput(names(namekey)[9],
                                                      label = namekey[9],
                                                      placeholder = "e.g. Dow" ),
                                            textInput(names(namekey)[10],
                                                      label = namekey[10],
                                                      placeholder = "e.g. 99.98%"),
                                            textInput(names(namekey)[11],
                                                      label = namekey[11],
                                                      placeholder = "consumer product, manufacturer material, analytical standard, environmental sample"),
                                            textInput(names(namekey)[12],
                                                      label = namekey[12],
                                                      placeholder = "e.g. blue, #0000ff, (0, 0, 255)"),
                                            textInput(names(namekey)[13],
                                                      label = namekey[13],
                                                      placeholder = "e.g. 5 µm diameter fibers, 1 mm spherical particles"),
                                            textInput(names(namekey)[14],
                                                      label = namekey[14],
                                                      placeholder = "9003-53-6"),
                                            textInput(names(namekey)[15],
                                                      label = namekey[15],
                                                      placeholder = "Horiba LabRam"),
                                            textInput(names(namekey)[16],
                                                      label = namekey[16],
                                                      placeholder = "Focal Plane Array, CCD"),
                                            textInput(names(namekey)[17],
                                                      label = namekey[17],
                                                      placeholder = "transmission, reflectance"),
                                            textInput(names(namekey)[18],
                                                      label = namekey[18],
                                                      placeholder = "e.g. 4/cm"),
                                            textInput(names(namekey)[19],
                                                      label = namekey[19],
                                                      placeholder = "e.g. 785 nm" ),
                                            textInput(names(namekey)[20],
                                                      label = namekey[20],
                                                      placeholder = "e.g. 5"),
                                            textInput(names(namekey)[21],
                                                      label = namekey[21],
                                                      placeholder = "10 s"),
                                            textInput(names(namekey)[22],
                                                      label = namekey[22],
                                                      placeholder = "spikefilter, baseline correction, none"),
                                            textInput(names(namekey)[23],
                                                      label = namekey[23],
                                                      placeholder = "e.g. 99%"),
                                            textInput(names(namekey)[24], label = "Other information"),
                                            selectInput(names(namekey)[32],
                                                        label = namekey[32],
                                                        selected = "CC BY-NC",
                                                        choices = c("CC0", "CC BY",
                                                                    "CC BY-SA",
                                                                    "CC BY-NC",
                                                                    "CC BY-ND",
                                                                    "CC BY-NC-SA",
                                                                    "CC BY-NC-ND")),
                                            
                                            tags$br(),
                                            actionButton("submit", "Share Metadata", class = "btn-primary")
                                        )
                                 ),
                                 
                                 
                                 column(9,
                                        plotcontainerfunction(h4(id = "placeholder1", i18n$t("Upload some data to get started...")), plotlyOutput("MyPlot")),
                                        style = bodyformat()
                                        
                                 ),
                             ),
                             hr(),
                             fluidRow(
                                 column(3),
                                 column(6, align = "center",
                                        tags$p(citation),
                                        tags$p(version)
                                 ),
                                 column(3)
                                 
                             )),
                    
                    
                    #Preprocess Spectrum Tab ----
                    tabPanel(i18n$t("Preprocess Spectrum"), value = "tab2",
                             titlePanel(h4(i18n$t("Smooth, Baseline Correct, and Download Processed Spectra"))),
                             br(),
                             fluidRow(
                                 column(3, style = columnformat(),
                                        fluidRow(
                                            column(12,
                                                   downloadButton("downloadData", i18n$t("Download (recommended)"), style = "background-color: #2a9fd6;"),
                                                   bsPopover(id = "downloadData",
                                                             title = "Download Help",
                                                             content = "Some users may wish to save a copy of their processed spectrum. This button downloads the processed spectrum as a csv file.",
                                                             placement = "bottom",
                                                             trigger = "hover")
                                            )
                                        ),
                                        tags$br(),
                                        fluidRow(
                                            column(10,
                                                   prettySwitch(inputId = "smooth_decision",
                                                                label = i18n$t("Smoothing"),
                                                                inline = T,
                                                                value = T,
                                                                status = "success",
                                                                fill = T),
                                                   bsPopover(
                                                       id = "smooth_decision",
                                                       title = "Smoother Help",
                                                       content = "This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified.",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   )
                                            ),
                                            column(2,
                                                   prettyCheckbox("smooth_tools", label = "adv",  icon = icon("gear"), status = "warning", shape = "square"),
                                            )
                                        ),
                                        
                                        fluidRow(
                                            column(10,
                                                   prettySwitch("baseline_decision",
                                                                label = i18n$t("Baseline Correction"),
                                                                inline = T,
                                                                value = T,
                                                                status = "success",
                                                                fill = T),
                                                   bsPopover(
                                                       id = "baseline_decision",
                                                       title = "Baseline Correction Help",
                                                       content = "This baseline correction routine has two options for baseline correction, 1) the polynomial imodpolyfit procedure to itteratively find the baseline of the spectrum using a polynomial fit to the entire region of the spectra. 2) manual lines can be drawn using the line tool on the plot and the correct button will use the lines to subtract the baseline.",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   )
                                                   
                                            ),
                                            column(2,
                                                   prettyCheckbox("baseline_tools", label = "adv", icon = icon("gear"), status = "warning", shape = "square"),
                                            )
                                        ),
                                        fluidRow(
                                            column(10,
                                                   prettySwitch("range_decision",
                                                                label = i18n$t("Range Selection"),
                                                                inline = T,
                                                                value = T,
                                                                status = "success",
                                                                fill = T),
                                                   bsPopover(
                                                       id = "range_decision",
                                                       title = "Spectral Range Help",
                                                       content = "Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   )
                                            ),
                                            column(2,
                                                   prettyCheckbox("range_tools", label = "adv", icon = icon("gear"), status = "warning", shape = "square")
                                            )
                                        ),
                                        fluidRow(column(12,
                                                        conditionalPanel("input.smooth_tools == true & input.smooth_decision == true",
                                                                         plotcontainerfunction(sliderInput("smoother", i18n$t("Smoothing Polynomial"), min = 0, max = 7, value = 3)
                                                                         )),
                                                        conditionalPanel("input.baseline_tools == true & input.baseline_decision == true",
                                                                         plotcontainerfunction(
                                                                             selectInput(inputId = "baseline_selection", label = i18n$t("Technique"), choices = c("Polynomial", "Manual")),
                                                                             sliderInput("baseline", i18n$t("Baseline Correction Polynomial"), min = 1, max = 20, value = 8),
                                                                             fluidRow(
                                                                                 column(6,
                                                                                        actionButton("go", i18n$t("Correct With Trace")),
                                                                                 ),
                                                                                 column(6,
                                                                                        actionButton("reset", i18n$t("Reset")),
                                                                                 )
                                                                             )
                                                                         )
                                                                         
                                                        ), 
                                                        conditionalPanel("input.range_tools == true & input.range_decision == true",
                                                                         plotcontainerfunction(
                                                                             numericInput(
                                                                                 "MaxRange",
                                                                                 i18n$t("Maximum Spectral Range"),
                                                                                 value = 6000,
                                                                                 min = NA,
                                                                                 max = NA,
                                                                                 step = NA,
                                                                                 width = NULL
                                                                             ),
                                                                             numericInput(
                                                                                 "MinRange",
                                                                                 i18n$t("Minimum Spectral Range"),
                                                                                 value = 0,
                                                                                 min = NA,
                                                                                 max = NA,
                                                                                 step = NA,
                                                                                 width = NULL
                                                                             )
                                                                         )
                                                        )
                                        )
                                        )
                                 ),
                                 
                                 
                                 column(9,
                                        plotcontainerfunction(h4(id = "placeholder2", i18n$t("Upload some data to get started...")), plotlyOutput("MyPlotB")),
                                        #verbatimTextOutput(outputId = "text"),
                                        style = bodyformat()
                                        
                                 )),
                             hr(),
                             fluidRow(
                                 column(3),
                                 column(6, align = "center",
                                        tags$p(citation),
                                        tags$p(version)
                                 ),
                                 column(3)
                                 
                             )),
                    
                    #Match Spectrum Tab ----
                    tabPanel(i18n$t("Identify Spectrum"),value = "tab3",
                             titlePanel(h4(i18n$t("Identify Spectrum Using the Reference Library"))),
                             br(),
                             fluidRow(
                                 column(3, style = columnformat(),
                                        fluidRow(
                                            column(4,
                                                   radioButtons("Spectra", "Type",
                                                                c("Raman" = "raman",
                                                                  "FTIR" = "ftir")),
                                                   bsPopover(
                                                       id = "Spectra",
                                                       title = "Spectrum Type Help",
                                                       content = "This selection will determine whether the FTIR or Raman matching library is used. Choose the spectrum type that was uploaded.",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   )),
                                            column(4,
                                                   radioButtons("Data", "Analysis",
                                                                c("Processed" = "processed",
                                                                  "Uploaded" = "uploaded"
                                                                )),
                                                   bsPopover(
                                                       id = "Data",
                                                       title = "Spectrum to Analyze Help",
                                                       content = "This selection will determine whether the uploaded (not processed) spectrum or the spectrum processed using the processing tab is used in the spectrum match.",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   ),
                                            ),
                                            column(4,
                                                   radioButtons("Library", "Region",
                                                                c("Full" = "full",
                                                                  "Peaks" = "peaks")),
                                                   bsPopover(
                                                       id = "Library",
                                                       title = "Region to Match Help",
                                                       content = "This selection will determine whether the library you are matching to consists of the full spectrum or only spectrum peaks.",
                                                       placement = "bottom",
                                                       trigger = "hover"
                                                   )
                                                   
                                            )
                                        ),
                                        fluidRow(style = "padding:1rem",
                                                 DT::dataTableOutput("event")
                                        )
                                 ),
                                 
                                 column(9,
                                        plotcontainerfunction(h4(id = "placeholder3", i18n$t("Upload some data to get started...")), plotlyOutput("MyPlotC"),
                                                              DT::dataTableOutput("eventmetadata")),
                                        style = bodyformat()
                                        
                                 )
                             ),
                             
                             
                             hr(),
                             fluidRow(
                                 column(3),
                                 column(6, align = "center",
                                        tags$p(citation),
                                        tags$p(version)
                                 ),
                                 column(3)
                                 
                             )),
                    
                    
                    #Partner With Us tab ----
                    tabPanel(i18n$t("Partner With Us"),
                             titlePanel(h4(i18n$t("Help us reach our goal of revolutionizing spectroscopy."))),
                             fluidRow(
                                 column(1),
                                 column(9,
                                        div(style = "font-size:150%",
                                            DT::dataTableOutput("event_goals"),
                                            br()
                                        ),
                                 ),
                                 column(2)
                             ),
                             fluidRow(
                                 column(1),
                                 column(3,
                                        plotcontainerfunction(
                                            tags$h3(i18n$t("Donate Cash")),
                                            icon = icon("shopping-cart"),
                                            img(src = "https://p.turbosquid.com/ts-thumb/rX/Wm1eqB/t5/currencysymbolsgoldensetc4dmodel000/jpg/1613802168/300x300/sharp_fit_q85/a31625492ce9c8009ab3e4281ad752006e1163ec/currencysymbolsgoldensetc4dmodel000.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                            actionButton(inputId = "ab1", label = i18n$t("Donate"), style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                         icon = icon("donate"),
                                                         onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')")
                                        )),
                                 column(3,
                                        plotcontainerfunction(tags$h3(i18n$t("Buy From Swag Store")),
                                                              img(src = "https://image.spreadshirtmedia.com/image-server/v1/products/T813A823PA3132PT17X42Y46D1038541132FS4033/views/1,width=650,height=650,appearanceId=823/updated-logo-for-open-specy-designed-by-alex-mcgoran.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                                              actionButton(inputId = "ab2", label = i18n$t("Shop"), style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                                           icon = icon("shopping-cart"),
                                                                           onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")
                                        )),
                                 column(3,
                                        plotcontainerfunction(
                                            h2(i18n$t("Contribute time")),
                                            #p(class = "lead", "We are looking for coders, moderators, spectroscopy experts, microplastic researchers, industry, government, and others to join the Open Specy team. Please contact Win at wincowger@gmail.com"),
                                            img(src = "https://health.sunnybrook.ca/wp-content/uploads/2020/02/healthy-hands-810x424.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                            actionButton(inputId = "ab3", label = i18n$t("Guidelines"), style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                         icon = icon("clock"),
                                                         onclick ="window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')")
                                        )
                                 ),
                                 column(2)
                             ),
                             hr(),
                             fluidRow(
                                 column(3),
                                 column(6, align = "center",
                                        tags$p(citation),
                                        tags$p(version)
                                 ),
                                 column(3)
                                 
                             )
                        )
                )   
         })
})

shinyApp(ui = ui, server = server)



