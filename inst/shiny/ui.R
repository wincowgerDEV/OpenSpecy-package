#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#'
# Libraries ----
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyhelper)

library(dplyr)
library(plotly)
library(DT)

# Functions ----
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# CSS for star
appCSS <-
  ".mandatory_star { color: red; }"

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

  # About Tab ----
  titlePanel(
    fluidRow(
      column(9, "Open Specy"),
      column(3, align = "right", uiOutput("translate")) # Google Translate
    )
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
                                shiny::HTML("<h5>In Brief: To use the tool upload a csv, jdx, spc, or spa file to the upload file tab.
                                  If csv, one column should be named -Wavelength- (in units of 1/cm) and another named -Absorbance-.
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
                       titlePanel(tags$h4("Upload. View and Share Spectra Files")),
                       fluidRow(
                         column(2,
                                fileInput('file1', 'Choose .csv (preferred), .jdx, .spc, .spa, or .0 File',
                                          accept=c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv', ".spc", ".jdx", ".spa", ".0"))%>%
                                  helper(type = "inline",
                                         title = "Upload Help",
                                         content = c("Upload Raman or FTIR spectrum files as a csv, jdx, spc, or spa. A csv file is preferred. If a csv, the file must contain one column labeled Wavelength in units of (1/cm) and another column labeled Absorbance in absorbance units.
                                            If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavelength in (1/cm). These files will not always work perfectly because they are tricky to read so double check them in another software.",
                                                     "<a href=https://drive.google.com/file/d/1T7Zm3MmMaQ9bd_6f1cV_NGRduaCYLhz5/view?usp=sharing> Example Dataset (HDPE,Raman)"),
                                         size = "m"),
                                tags$div(downloadButton('downloadData7', 'Test Data')),
                                tags$br(),


                                helper(selectInput("ShareDecision", "Share uploaded spectra?",
                                                   c("Share" = "Share",
                                                     "Not Now" = "Not now")),
                                       type = "inline",
                                       title = "Share Help",
                                       content = c("We share any uploaded spectra with the spectroscopy community if you select share.",
                                                   "<a href=https://osf.io/rjg3c> Uploaded spectra will appear here."),
                                       size = "m"),

                                actionButton("btn", "Share metadata")%>%
                                  helper(type = "inline",
                                         title = "Metadata Help",
                                         content = c("We share any uploaded spectra and metadata with the spectroscopy community if you fill out the metadata here and select share.",
                                                     "<a href=https://osf.io/rjg3c> Uploaded spectra and metadata will appear here."),
                                         size = "m"),

                                hidden(textInput("User Name", labelMandatory("User Name"), placeholder = "Win Cowger"),
                                       textInput("Contact Info", label = "Contact Info", placeholder = "1-513-673-8956, wincowger@gmail.com"),
                                       textInput("Affiliation", label = "Affiliation", placeholder = "University of California, Riverside"),
                                       textInput("Data Citation", label = "Data Citation", placeholder = "Primpke, S., Wirth, M., Lorenz, C., & Gerdts, G. (2018). Reference database design for the automated analysis of microplastic samples based on Fourier transform infrared (FTIR) spectroscopy. Analytical and Bioanalytical Chemistry. https://doi.org/10.1007/s00216-018-1156-x"),
                                       textInput("Spectrum Identity", labelMandatory("Spectrum Identity"), placeholder = "Polystyrene"),
                                       textInput("Spectrum Type", labelMandatory("Spectrum Type"), placeholder = "Raman or FTIR"),
                                       textInput("Color", label = "Material Color", placeholder = "E.g. Blue, #0000ff, (0,0,255)"),
                                       textInput("CAS Number", label = "Material CAS Number(s)", placeholder = "9003-53-6"),
                                       textInput("Material Producer", label = "Material Producer", placeholder = "Dow"),
                                       textInput("Material Phase", label = "Material Phase", placeholder = "liquid, gas, solid"),
                                       textInput("Material Form", label = "Material Form", placeholder = "E.g. rubber band, granular, cup"),
                                       textInput("Other Material Form Description", label = "Other Material Form Description", placeholder = "E.g. 5 micron diameter fibers, 1 mm spherical particles."),
                                       textInput("Material Purity", label = "Material Purity", placeholder = "99.98%"),
                                       textInput("Material Quality", label = "Material Quality", placeholder = "Consumer Material, Manufacturer Material, Scientific Standard, Environmental Material"),
                                       textInput("Instrument Used", label = "Instrument Used", placeholder = "Horiba LabRam"),
                                       textInput("Instrument Accessories", label = "Instrument Accesories", placeholder = "Focal Plane Array, CCD"),
                                       textInput("Instrument Mode", label = "Instrument Mode", placeholder = "transmission, reflectance"),
                                       textInput("Spectral Resolution", label = "Spectral Resolution", placeholder =  "4/cm-1"),
                                       textInput("LaserLight Used", label = "Laser/Light Used", placeholder = "785 nm"),
                                       textInput("Number of Accumulations", label = "Number of Accumulations", placeholder = "5"),
                                       textInput("Total Acquisition Time", label = "Total Acquisition Time (s)", placeholder = "10 s"),
                                       textInput("Data Processing Procedure", label = "Data Processing Procedure", placeholder =  "spikefilter, baseline correction, none"),
                                       textInput("Level of Confidence in Identification", label = "Level of Confidence in Identification", placeholder = "99%"),
                                       textInput("Description of Identification", label = "Description of Identification", placeholder = "E.g. Spectra were matched with 99% HQI in Know-it-all"),
                                       textInput("Other Information", label = "Other Information"),

                                       tags$br(),
                                       actionButton("submit", "Share Data", class = "btn-primary"))


                         ),


                         column(8,
                                plotlyOutput('MyPlot')

                         ),
                         column(2,

                                selectInput("IntensityCorr", "Intensity Adjustment",
                                            c("None" = "None",
                                              "Transmittance" = "Transmittance", "Reflectance" = "Reflectance")) %>%
                                  helper(type = "inline",
                                         title = "Share Help",
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
                                         size = "m")

                         )),
                       fluidRow(
                         align="center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, ",
                         "K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       )),


              #Preprocess Spectrum Tab ----
              tabPanel("Preprocess Spectrum", value = "tab2",
                       titlePanel(tags$h4("Smooth, Baseline Correct, and Download Processed Spectra")),
                       fluidRow(
                         column(2,
                                sliderInput("smoother", "Smoothing Polynomial", min = 0, max = 7, value = 3) %>%
                                  helper(type = "inline",
                                         title = "Smoother Help",
                                         content = c("This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified."),
                                         size = "m"),
                                sliderInput("baseline", "Baseline Correction Polynomial", min = 0, max = 20, value = 8)%>%
                                  helper(type = "inline",
                                         title = "Baseline Correction Help",
                                         content = c("This baseline correction routine utilizes the imodpolyfit procedure to itteratively find the baseline of the spectrum using a polynomial fit to the entire region of the spectra."),
                                         size = "m"),

                                numericInput(
                                  "MinRange",
                                  "Minimum Spectral Range",
                                  value = 0,
                                  min = NA,
                                  max = NA,
                                  step = NA,
                                  width = NULL
                                ) %>%
                                  helper(type = "inline",
                                         title = "Spectral Range Help",
                                         content = c("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching"),
                                         size = "m"),
                                numericInput(
                                  "MaxRange",
                                  "Maximum Spectral Range",
                                  value = 6000,
                                  min = NA,
                                  max = NA,
                                  step = NA,
                                  width = NULL
                                ) %>%
                                  helper(type = "inline",
                                         title = "Spectral Range Help",
                                         content = c("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching"),
                                         size = "m"),

                                downloadButton('downloadData', 'Download (recommended)') %>%
                                  helper(type = "inline",
                                         title = "Download Help",
                                         content = c("Some users may wish to save a copy of their processed spectrum. This button downloads the processed spectrum as a csv file."),
                                         size = "m")


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
                                selectInput("Spectra", "Spectrum Type:",
                                            c("Raman" = "Raman",
                                              "FTIR" = "FTIR")) %>%
                                  helper(type = "inline",
                                         title = "Spectrum Type Help",
                                         content = c("This selection will determine whether the FTIR or Raman matching library is used. Choose the spectrum type that was uploaded."),
                                         size = "m"),
                                selectInput("Data", "Spectrum To Analyze:",
                                            c("Processed" = "processed",
                                              "Uploaded" = "uploaded"
                                            ))%>%
                                  helper(type = "inline",
                                         title = "Spectrum To Analyze Help",
                                         content = c("This selection will determine whether the uploaded (not processed) spectrum or the spectrum processed using the processing tab is used in the spectrum match."),
                                         size = "m"),
                                selectInput("Library", "Region To Match:",
                                            c("Full Spectrum" = "Full",
                                              "Peaks Only" = "Peaks"))%>%
                                  helper(type = "inline",
                                         title = "Region To Match Help",
                                         content = c("This selection will determine whether the the library you are matching to consists of the full spectrum or only spectrum peaks."),
                                         size = "m")
                         ),

                         column(7,

                                plotlyOutput('MyPlotC'),
                                DT::dataTableOutput('eventmetadata')

                         ),
                         column(3, DT::dataTableOutput('event'))),



                       fluidRow(
                         align="center",
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
                                actionButton(inputId='ab1', label="Donate", width = "100%",
                                             icon = icon("donate"),
                                             onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')"),
                                tags$h3("Or by purchasing some Open Specy merchandise here."),
                                actionButton(inputId='ab2', label="Swag Shop", width = "100%",
                                             icon = icon("shopping-cart"),
                                             onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")#,
                         ),
                         column(1)

                       ),
                       fluidRow(
                         column(1),
                         column(5,
                                DT::dataTableOutput('donations')),
                         column(5,
                                DT::dataTableOutput('costs')),
                         column(1)
                       ),

                       fluidRow(
                         align="center",
                         hr(),
                         tags$p("Citation: W. Cowger, A. Gray, H. Hapich, C. Rochman, J. Lynch, S. Primpke, K. Munno, H. De Frond, O. Herodotou. 2020. Open Specy. www.openspecy.org")
                       ))
  )
)
