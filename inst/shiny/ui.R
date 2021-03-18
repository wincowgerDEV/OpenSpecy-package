#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#'
# Libraries ----
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(plotly)
library(DT)

# Name keys for human readable column names ----
load("data/namekey.RData")

# Functions ----
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# CSS for star
appCSS <-
  ".mandatory_star { color: red; }
    #loading_overlay {
    position: absolute;
    margin-top: 10%;
    background: #000000;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    color: #FFFFFF;
    }"

# UI ----
ui <- fluidPage(
  shinyjs::useShinyjs(), # Required for any of the shinyjs functions.
  tags$head(uiOutput("analytics")), # Google analytics.
  theme = shinytheme("cyborg"), # Change this for other themes
  tags$head( #This is for the error messages.
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
    # This will allow us to reference tabs in other tabs and create links
    # see https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel/36426258
    tags$head(
      tags$script(HTML('var fakeClick = function(tabName) {
                        var dropdownList = document.getElementsByTagName("a");
                        for (var i = 0; i < dropdownList.length; i++) {
                        var link = dropdownList[i];
                        if(link.getAttribute("data-value") == tabName) {
                        link.click();
                        };
                        }
                        };
                        ')))
  ),
  shinyjs::inlineCSS(appCSS),

  div(
    id = "loading_overlay",
    h2("Loading Open Specy"),
    br(),
    tags$div(
      style="margin-bottom:20%;",
    ),
    h4(icon("spinner", class = "btn-loading-indicator fa-spin")),
    br(),
    h6("If you start Open Specy for the first time, this may take a while ...")
  ),

  hidden(div(id = "app_content",

  # About Tab ----
  titlePanel(
    fluidRow(
      column(9, "Open Specy"),
      column(3, align = "right", uiOutput("translate")) # Google Translate
    ), windowTitle = "Open Specy"
  ),
  tabsetPanel(id = "tabs",
              tabPanel("About", value = "about",
                       fluidRow(
                         column(2),
                         column(8, align = "center",
                                img(src = "graphical_abstract.png", width = "100%")),
                         column(2)
                       ),
                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>OVERVIEW</h1> ",
                                            "</center><br>"),
                                shiny::HTML("<h5>More than 800 people from around ",
                                            "the world have used Open Specy to ",
                                            "analyze, share, process, and identify ",
                                            "their Raman and IR spectra.</h5>")
                         ),
                         column(3)
                       ),

                       fluidRow(
                         style = "height:50px;"),
                       # PAGE BREAK
                       tags$hr(),
                       # HOW
                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Instructions</h1> </center><br>"),
                                shiny::HTML("<h5>In Brief: To use the tool upload a csv, asp, jdx, spc, or spa file to the upload file tab.
                                  If csv, one column should be named 'wavenumber' (in units of 1/cm) and another named 'intensity'.
                                  You can smooth your data using an SG filter, baseline correct your data using the polynomial order of iModPolyFit, and restrict the wavelength range for the match.
                                  The result will be compared to an internal Raman or FTIR spectra library. The strongest 1000 matches along with your
                                  uploaded or processed data will be presented in an interactive plot and table.</h5>"),
                                tags$div(align = "center",
                                         tags$a("Detailed Standard Operating Procedure",
                                                onclick = "window.open('https://docs.google.com/document/d/1DU9zsqKJqN5eW5yhDZ123BGUmYAdfXJ2KAykjjXIhOg/edit?usp=sharing', '_blank')",
                                                class="btn btn-primary btn-lg")


                                )
                         ),
                         column(3)
                       ),
                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Download Open Data</h1> </center><br>"),
                                shiny::HTML("<h5>Reference spectra was sourced from open access resources ",
                                "online, peer reviewed publications, and corporate donations. In the future, ",
                                "spectra that is uploaded to the tool will be incorporated to the reference ",
                                "library to make it even better.</h5>")
                         ),
                         column(3)
                       ),

                       fluidRow(
                         column(1),
                         column(10,
                                tags$div(align = "center",
                                         downloadButton('downloadData6', 'Raman Reference Library'),
                                         downloadButton('downloadData5', 'FTIR Reference Library'),
                                         downloadButton('downloadData4', 'Raman Reference Library Metadata'),
                                         downloadButton('downloadData3', 'FTIR Reference Library Metadata')
                                )
                         ),
                         column(1)
                       ),


                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Tool Validation</h1> </center><br>"),
                                shiny::HTML("<h5>All parameters in this tool are tested to validate that ",
                                "the tool is functioning as best as possible and determine the best default ",
                                "parameters to use. Our current validation proceedure includes correcting ",
                                "duplicated entries in the reference libraries, checking for spectra in ",
                                "metadata that isn't in the spectral library, and ensuring the the default ",
                                "parameters provide over 80% accuracy in the first match.</h5>"),
                                tags$div(align = "center",
                                         tags$a("Detailed Validation Procedure",
                                                onclick = "window.open('https://docs.google.com/document/d/1Zd2GY4bWIwegGeE4JpX8O0S5l_IYju0sLDl1ddTTMxU/edit?usp=sharing', '_blank')",
                                                class="btn btn-primary btn-lg")
                                )),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Updates, Feature Requests, and Bug Reports</h1> </center><br>"),
                                shiny::HTML("<h5>We keep track of all updates using version control on our code. Features can be requested and bug reported on GitHub.</h5>"),
                                tags$div(align = "center",
                                         tags$a("Updates, Feature Requests, Bug Reports",
                                                onclick = "window.open('https://github.com/wincowgerDEV/OpenSpecy', '_blank')",
                                                class="btn btn-primary btn-lg")
                                )),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Contribute spectra</h1> </center><br>"),
                                shiny::HTML("<h5>To share spectra upload a file to the upload file tab. ",
                                "If you selected Share a copy of your spectra will be sent to the Community ",
                                "Data Warehouse on Open Science Framework. To add additional metadata, ",
                                "fill in the avaliable metadata fields and click -Share Data-. The ",
                                "spectra file that you uploaded along with your responses will be copied ",
                                "to the a -With Metadata- subfolder at the link below. All shared data holds ",
                                "a Creative Commons Attribution License 4.0.</h5>")
                         ),
                         column(3)
                       ),
                       fluidRow(
                         column(3),
                         column(6,

                                tags$div(align = "center",
                                         tags$a("Community Data Warehouse",
                                                onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                                class="btn btn-primary btn-lg")


                                )
                         ),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Contribute time</h1> </center>
                                  <br>"),
                                shiny::HTML("<h5>We are looking for coders, moderators, spectroscopy experts, microplastic researchers, industry, government, and others to join the Open Specy team. Please contact Win at wincowger@gmail.com</h5>"),
                                tags$div(align = "center",
                                         tags$a("Community Contribution Guidelines",
                                                onclick = "window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')",
                                                class="btn btn-primary btn-lg")
                                )),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),


                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Stay up to date!</h1> </center>
                                  <br>"),
                                shiny::HTML("<h5>Follow us on Twitter @OpenSpecy. Email wincowger@gmail.com to be added to the mailing list.</h5>")
                         ),
                         column(3)
                       ),


                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                                shiny::HTML("<h5>W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org</h5>")
                         ),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Useful Links</h1> </center>
                                  <br>"),
                                tags$a(align = "center", href = "https://simple-plastics.eu/", "Free FTIR Software: siMPle microplastic IR spectral identification software"),
                                tags$p(),
                                tags$a(align = "center", href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher"),
                                tags$p(),
                                tags$a(align = "center", href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University"),
                                tags$p(),
                                tags$a(align = "center", href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.")),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       # PAGE BREAK
                       tags$hr(),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Terms And Conditions</h1> </center>
                                  <br>"),
                                pre(includeText("www/TOS.txt"))
                         ),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),

                       fluidRow(
                         column(3),
                         column(6,
                                shiny::HTML("<br><br><center> <h1>Privacy Policy</h1> </center>
                                  <br>"),
                                pre(includeText("www/privacy_policy.txt"))
                         ),
                         column(3)
                       ),

                       fluidRow(

                         style = "height:50px;"),
                       tags$hr()
              ),

              #Upload File Tab ----
              tabPanel("Upload File", value = "tab1",
                       titlePanel(tags$h4("Upload, View and Share Spectra")),
                       fluidRow(
                         column(2,
                                tags$label("Choose .csv (preferred), .asp, .jdx, .spc, .spa, or .0 File"),

                                prettySwitch("share_decision",
                                             label = "Share File?",
                                             inline = T, 
                                             value = T,
                                             status = "success",
                                             fill = T),
                                bsPopover(
                                  id = "share_decision",
                                  title = "Share Help",
                                  content = c("We share any uploaded spectra with the spectroscopy community if you select share.",
                                              "Uploaded spectra will appear here: https://osf.io/rjg3c"),
                                  placement = "bottom", 
                                  trigger = "hover"
                                ),

                                fileInput("file1", NULL,
                                          accept=c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv', ".asp", ".spc", ".jdx", ".spa", ".0")),
                                bsPopover(
                                  id = "file1",
                                  title = "Upload Help",
                                  content = c("Upload Raman or FTIR spectrum files as a csv, jdx, spc, or spa. A csv file is preferred. If a csv, the file must contain one column labeled 'wavenumber' in units of (1/cm) and another column labeled 'intensity' in absorbance units.
                                            If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). These files will not always work perfectly because they are tricky to read so double check them in another software.",
                                            "",
                                            "Hit the 'Test Data' button to download a sample Raman spectrum."),
                                  placement = "bottom", 
                                  trigger = "hover"
                                ),

                                actionButton("share_meta", "Share Metadata"),
                                bsPopover(
                                  id = "share_meta", 
                                  title = "Metadata Help",
                                  content = c("We share any uploaded spectra and metadata with the spectroscopy community if you fill out the metadata here and select share.",
                                              "Uploaded spectra and metadata will appear here: https://osf.io/rjg3c"),
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

                                  tags$br(),
                                  actionButton("submit", "Share Metadata", class = "btn-primary")
                                ),

                                tags$br(),
                                tags$br(),

                                tags$div(downloadButton('download_testdata', 'Sample File'))
                                ),


                         column(8,
                                plotlyOutput('MyPlot')

                         ),
                         column(2,

                                selectInput("IntensityCorr", "Intensity Adjustment",
                                            c("None" = "none",
                                              "Transmittance" = "transmittance", "Reflectance" = "reflectance")),
                                bsPopover(
                                  id = "IntensityCorr", 
                                  title = "Intensity Correction Help",
                                  content = c("If the uploaded spectrum is not in absorbance units, ",
                                              "use this input to specify the units to convert from.Open Specy can ",
                                              "adjust reflectance or transmittance spectra to Absorbance units using ",
                                              "this drop down in the upload file tab. All of the preceding tabs ",
                                              "assume that the data is in absorbance units so you should make the ",
                                              "correction before continuing if needed. The transmittance adjustment ",
                                              "uses the log10(1/T) calculation which does not correct for system ",
                                              "and particle characteristics. The reflectance adjustment uses the ",
                                              "Kubelka-Munk equation (1-R)2/(2*R). ",
                                              "If none is selected, Open Specy assumes that the uploaded data is ",
                                              "an absorbance spectrum."),
                                  placement = "bottom", 
                                  trigger = "hover"
                                )
                         )),
                       fluidRow(
                         align = "center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, ",
                         "K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       )),


              #Preprocess Spectrum Tab ----
              tabPanel("Preprocess Spectrum", value = "tab2",
                       titlePanel(tags$h4("Smooth, Baseline Correct, and Download Processed Spectra")),
                       fluidRow(
                         column(2,
                                fluidRow(
                                  column(12, 
                                  downloadButton('downloadData', 'Download (recommended)'),
                                    bsPopover(id = 'downloadData', 
                                              title = "Download Help",
                                              content = c("Some users may wish to save a copy of their processed spectrum. This button downloads the processed spectrum as a csv file."),
                                              placement = "bottom", 
                                              trigger = "hover")
                                    )
                                ),
                                tags$br(),
                                fluidRow(
                                  column(2, 
                                         dropdownButton(inputId = "smooth_tools",
                                                        sliderInput("smoother", "Smoothing Polynomial", min = 0, max = 7, value = 3),
                                                        icon = icon("gear"),
                                                        size = "xs",
                                                        status = "success", 
                                                        width = "300px", 
                                                        circle = TRUE)
                                  ),
                                  column(10, 
                                         prettySwitch("smooth_decision",
                                             label = "Smoothing",
                                             inline = T, 
                                             value = T,
                                             bigger = T,
                                             status = "success",
                                             fill = T),
                                         bsPopover(
                                           id = "smooth_decision",
                                           title = "Smoother Help",
                                           content = c("This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified."),
                                           placement = "bottom", 
                                           trigger = "hover"
                                           )
                                  )
                                  
                                ),
                                
                                fluidRow(
                                  column(2,
                                   dropdownButton(inputId = "baseline_tools",
                                    sliderInput("baseline", "Baseline Correction Polynomial", min = 0, max = 20, value = 8),
                                    icon = icon("gear"),
                                    size = "xs",
                                    status = "success", 
                                    width = "300px", 
                                    circle = TRUE
                                                )
                                         ),
                                  column(10, 
                                  prettySwitch("baseline_decision",
                                             label = "Baseline Correction",
                                             inline = T, 
                                             value = T,
                                             bigger = T,
                                             status = "success",
                                             fill = T),
                                  bsPopover(
                                    id = "baseline_decision",
                                    title = "Baseline Correction Help",
                                    content = c("This baseline correction routine utilizes the imodpolyfit procedure to itteratively find the baseline of the spectrum using a polynomial fit to the entire region of the spectra."),
                                    placement = "bottom", 
                                    trigger = "hover"
                                  )

                                         )
                                ),
                                fluidRow(
                                  column(2, 
                                         dropdownButton(inputId = "range_tools",
                                                        numericInput(
                                                          "MinRange",
                                                          "Minimum Spectral Range",
                                                          value = 0,
                                                          min = NA,
                                                          max = NA,
                                                          step = NA,
                                                          width = NULL
                                                        ),
                                                        numericInput(
                                                          "MaxRange",
                                                          "Maximum Spectral Range",
                                                          value = 6000,
                                                          min = NA,
                                                          max = NA,
                                                          step = NA,
                                                          width = NULL
                                                        ),
                                                        icon = icon("gear"),
                                                        size = "xs",
                                                        status = "success", 
                                                        width = "300px", 
                                                        circle = TRUE
                                         )
                                  ),
                                  column(10, 
                                         prettySwitch("range_decision",
                                             label = "Range Selection",
                                             inline = T, 
                                             value = T,
                                             bigger = T,
                                             status = "success",
                                             fill = T),
                                         bsPopover(
                                           id = "range_decision",
                                           title = "Spectral Range Help",
                                           content = c("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching"),
                                           placement = "bottom", 
                                           trigger = "hover"
                                         )
                                  )
                                )
                            ),

                         column(10,
                                plotlyOutput('MyPlotB')

                         )),
                       fluidRow(
                         align="center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       )),

              #Match Spectrum Tab ----
              tabPanel("Match Spectrum",value = "tab3",
                       titlePanel(tags$h4("Identify Spectrum Using the Reference Library")),
                       fluidRow(
                         column(2,
                                radioButtons("Spectra", "Spectrum Type",
                                             c("Raman" = "raman",
                                               "FTIR" = "ftir")),
                                bsPopover(
                                  id = "Spectra",
                                  title = "Spectrum Type Help",
                                  content = c("This selection will determine whether the FTIR or Raman matching library is used. Choose the spectrum type that was uploaded."),
                                  placement = "bottom",
                                  trigger = "hover"
                                ),
                                selectInput("Data", "Spectrum to Analyze",
                                            c("Processed" = "processed",
                                              "Uploaded" = "uploaded"
                                            )),
                                bsPopover(
                                  id = "Data",
                                  title = "Spectrum to Analyze Help",
                                  content = c("This selection will determine whether the uploaded (not processed) spectrum or the spectrum processed using the processing tab is used in the spectrum match."),
                                  placement = "bottom",
                                  trigger = "hover"
                                ),
                                selectInput("Library", "Region to Match",
                                            c("Full Spectrum" = "full",
                                              "Peaks Only" = "peaks")),
                                bsPopover(
                                  id = "Library",
                                  title = "Region To Match Help",
                                  content = c("This selection will determine whether the the library you are matching to consists of the full spectrum or only spectrum peaks."),
                                  placement = "bottom",
                                  trigger = "hover"
                                )

                         ),

                         column(7,

                                plotlyOutput('MyPlotC'),
                                DT::dataTableOutput('eventmetadata')

                         ),
                         column(3, DT::dataTableOutput('event'))),



                       fluidRow(
                         align = "center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       )),


              #Support us tab ----
              tabPanel("Support Us",
                       fluidRow(
                         column(1),
                         column(10,
                                tags$h2("We are trying to make this project financially sustainable. It currently costs the developers about a 1000$ each year and no one is paid for their time. See expenses and donations below."),
                                tags$h3("You can help with a direct donation. Donate here."),
                                actionButton(inputId = 'ab1', label = "Donate", width = "100%",
                                             icon = icon("donate"),
                                             onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')"),
                                tags$h3("Or by purchasing some Open Specy merchandise here."),
                                actionButton(inputId = 'ab2', label = "Swag Shop", width = "100%",
                                             icon = icon("shopping-cart"),
                                             onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")#,
                         ),
                         column(1)

                       ),
                       fluidRow(
                         column(1),
                         column(4,
                                DT::dataTableOutput('donations')),
                         column(5,
                                DT::dataTableOutput('costs')),
                         column(1)
                       ),

                       fluidRow(
                         align = "center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       ))
  )
)
))
