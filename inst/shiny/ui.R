
# UI ----
dashboardPage(dark = T, 
              help = T, 
              fullscreen = T,
        #Header ----
        dashboardHeader( 
            title = tags$a(href="https://www.openanalysis.org", 
                           target="_blank",
                        tags$img(src = "logo.png", 
                                 style = 'width: 15vw; padding:1rem;'),
                        tags$head(
                            HTML(
                                '<div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" id = "openspecweba"></div>'
                            )
                        )
                        ), 
            tags$li(
                class = "dropdown",
                style = "list-style-type: none;",
                tags$a(
                    app_version_display$text,
                    href = app_version_display$href,
                    target = "_blank",
                    title = app_version_display$title,
                    style = "font-size: 19px;text-decoration: none;"
                     )
                )
            ),
        #Sidebar ----
        dashboardSidebar(
            skin = "dark",
            sidebarUserPanel(
                name = "Welcome!"
            ),
            sidebarMenu(
                id = "sidebarmenu",
                menuItem(
                    "Analyze Spectra",
                    tabName = "analyze",
                    icon = icon("bar-chart")
                ),
                menuItem(
                    "About",
                    tabName = "about",
                    icon = icon("sliders-h")
                ),
                menuItem(
                    "Partner With Us",
                    tabName = "partner",
                    icon = icon("hands-helping")
                ),
                menuItem(
                    "Contract Us",
                    tabName = "contract",
                    icon = icon("file-contract")
                )
            )
        ),
        #Body ----
        dashboardBody(
            #Script for all pages ----
                # Required for any of the shinyjs functions.
            shinyjs::useShinyjs(),
            
            # Donation prompts live in the Partner With Us tab so app startup
            # remains immediately usable.
            NULL,

            tags$head(
                      tags$script(async = T, src = "https://buttons.github.io/buttons.js"),
                      tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
                     #HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>'),
                      tags$link(rel = "icon", type = "image/png", href = "favicon.png")
                      #This is for the error messages.
            ),
            tabItems(
                # About Tab ----
                tabItem(
                   tabName = "about",
                   accordion(
                       id = "accordion_welcome",
                       accordionItem(
                           title = "Welcome",
                           status = "info",
                           collapsed = F,
                         fluidRow(
                           column(6,
                                  p(class = "lead", "Join the hundreds of
                               researchers from around the world who are part of
                               the Open Specy community by
                               analyzing, sharing, processing, and identifying
                               their Raman and IR spectra."),
                                  p(class = "lead",
                                    tags$span(style = "position:relative; top:.8ex;",
                                              tags$a(
                                                  href   = "https://www.linkedin.com/in/win-cowger/",
                                                  target = "_blank",                   # open in new tab
                                                  class  = "linkedin-button",          # custom CSS (below)
                                                  icon("linkedin", class = "fa-2x")    # larger icon
                                              )
                                    ),
                                    "Follow Win on LinkedIn for Latest Updates"
                                  ),
                               p(class = "lead",
                                 HTML("<span style='position: relative; top:.8ex;'><a
                                    class='github-button' href='https://github.com/wincowgerDEV/OpenSpecy/subscription'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-size='large' aria-label='Watch wincowgerDEV/OpenSpecy'>Watch</a></span>
                                    us develop Open Specy on GitHub, file an
                                    <span style='position: relative; top:.8ex;'><a
                                    class='github-button'
                                    href='https://github.com/wincowgerDEV/OpenSpecy/issues'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-icon='octicon-issue-opened' data-size='large'
                                    aria-label='Issue wincowgerDEV/OpenSpecy on GitHub'>Issue</a></span>,
                                    or request a feature")
                               ),
                               p(class = "lead",
                                 HTML("Or just e-mail <a href='mailto:wincowger@gmail.com?subject=Open Specy mailing list'>
                                          wincowger@gmail.com</a>
                                          to be added to the Open Specy mailing list")
                               ),
                               br(),
                               p(class = "lead", "Open Specy is free and open
                               source thanks to our partners."),
                               br(),
                               p(class = "lead", "Looking for the classic version of OpenSpecy? Go to wincowger.shinyapps.io/openspecy-classic")),
                           column(6, HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/3RKufDxzriE' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                                )
                            )
                         )
                       ),
                   accordion(
                       id = "accordion_instructions",
                       accordionItem(
                           title = "Detailed Instructions",
                           status = "info",
                           collapsed = TRUE,
                         fluidRow(
                           column(6,
                                  HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?si=HmRLfamgtrCYg5Gm&amp;list=PLqdH8O1nalYa4a8JXQ6GbNsH3YQV_aY7g" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'),
                           ),
                           column(6,
                                  tags$ol(
                                  tags$li("Upload a .zip, .csv, .0, .asp, .jdx, .spc, or .spa file to the upload file tab."),
                                  tags$li("Process your data using smoothing, derivative, baseline correction, flattening, range selection, and intensity adjustment."),
                                  tags$li("Identify your spectra using onboard reference libraries and/or AI"),
                                  tags$li("Download your results"),
                                  tags$li("For more details click the button below for the SOP or watch the detailed instructional videos.")
                                  ),
                                  a("SOP",
                                    onclick = "window.open('https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html', '_blank')",
                                    class="btn btn-primary btn-lg", 
                                    style = "width: 100%;")
                           )
                         )
                        )
                       ),
                       accordion(
                           id = "accordion_links",
                           accordionItem(
                               title = "Useful Links",
                               status = "info",
                               collapsed = TRUE,
                               a(href = "https://simple-plastics.eu/", "Free FTIR Software: siMPle microplastic IR spectral identification software", class = "lead"),
                               br(),
                               a(href = "https://gitlab.ipfdd.de/GEPARD/gepard", "Free Raman and FTIR Software: GEPARD (Gepard-Enabled PARticle Detection for Raman microscopes) Designed for particle-based microplastic analysis", class = "lead"),
                               br(),
                               a(href = "https://molview.org/", "Free chemical modeling tool with built in spectral query, MolView.", class = "lead"),
                               br(),
                               a(href = "https://webbook.nist.gov/", "Free spectroscopy and chemical database NIST Chemistry WebBook", class = "lead"),
                               br(),
                               a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher", class = "lead"),
                               br(),
                               a(href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University", class = "lead"),
                               br(),
                               a(href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.", class = "lead")   
                           )
                       )
              ),
              #Analyze Spectra Tab ----
              tabItem("analyze", 
                      br(),
                       fluidRow(
                           column(2,
                                  ##Upload/download ----
                                  #tags$label("Upload File"),
                                  fluidRow(style = "display: flex; align-items: flex-end;",
                                      column(12, 
                                             fileInput("file", NULL, multiple = T,
                                                       placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, ...",
                                                       accept=c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv", ".asp", ".tsv", ".spc", ".jdx", ".dx", ".RData",
                                                                ".spa", ".0", ".zip", ".img",  ".h5", ".txt",
                                                                ".json", ".rds", ".hdr", ".dat")) %>%
                                                 bs4Dash::popover(
                                                     title = "Upload Raman or FTIR spectrum files as a csv, tsv, h5, txt, img, dx, hdr, dat, rds, json, zip, asp, jdx, spc, 0, or spa. A csv file is preferred. If a csv, the file must contain one column labeled wavenumber in units of (1/cm) and another column labeled intensity in absorbance units. If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). If zip, batch upload using a zip file with multiple spectral files that all have the same wavenumbers or a map file formatted as .hdr and .dat. Hit the Download button to download a sample Raman spectrum.",
                                                     content = "File Upload", placement = "right"
                                                 )
                                  )
                            )
                           ),
                           column(8,
                                                 fluidRow( 
                                                     column(6, 
                                                            ## Preprocessing ----
                                                            fluidRow(
                                                                box(width = 12,
                                                                    collapsed = T,
                                                                    style = "height: 50vh; overflow-y: auto;",
                                                                    footer = footnote("Options for processing the spectra."),
                                                                    title = prettySwitch(inputId = "active_preprocessing",
                                                                                       label = "Preprocessing",
                                                                                       inline = T,
                                                                                       value = T,
                                                                                       status = "success",
                                                                                       fill = T),
                                                                    fluidRow(
                                                                        box(width = 12,
                                                                            footer = footnote("Signal thresholding technique, value, and histogram threshold plot.",
                                                                                              "If turned on, the threshold will be applied to any batch analysis results and values lower than the minimum will appear blacked out.",
                                                                                              "'Signal Over Noise' divides the highest peak by a low region.",
                                                                                              "'Signal Times Noise' multiplies the mean intensity by the standard deviation.",
                                                                                              "'Total Signal' is the sum of signal intensities.",
                                                                                              "The line on the histogram plot is the threshold being used."),
                                                                            title = prettySwitch("threshold_decision",
                                                                                                 label = "Threshold Signal-Noise",
                                                                                                 inline = T,
                                                                                                 value = F,
                                                                                                 status = "success",
                                                                                                 fill = T),
                                                                            collapsed = T,
                                                                            numericInput(
                                                                                "MinSNR",
                                                                                "Minimum Value",
                                                                                value = 4,
                                                                                min = -10000,
                                                                                max = 10000,
                                                                                step = 1
                                                                            ),
                                                                            br(),
                                                                            selectInput(inputId = "signal_selection", 
                                                                                        label = "Signal Thresholding Technique", 
                                                                                        choices = c("Signal Over Noise" = "run_sig_over_noise", 
                                                                                                    "Signal Times Noise" = "sig_times_noise", 
                                                                                                    "Total Signal" = "log_tot_sig")), 
                                                                            br(), 
                                                                            plotOutput("snr_plot", height = "10vh")
                                                                        )
                                                                    ),
                                                                    fluidRow(
                                                                        box(width = 12,
                                                                            footer = footnote("Min-Max normalization improves comparability between spectra.",
                                                                                              "Except in cases where raw intensity values are necessary for interpretation.",
                                                                                              "For example raw values can be useful for thresholding.",
                                                                                              "Min-Max normalization rescales spectral intensity values between 0-1"),
                                                                            title = prettySwitch("make_rel_decision",
                                                                                                 label = "Min-Max Normalize",
                                                                                                 inline = T,
                                                                                                 value = T,
                                                                                                 status = "success",
                                                                                                 fill = T),
                                                                            collapsed = T
                                                                        )
                                                                    ),
                                                                    fluidRow(
                                                                        box(width = 12,
                                                                            collapsed = T,
                                                                            footer = footnote("Smoothing can enhance signal to noise using the SG filter.",
                                                                                              "Derivative transformation uses the order specified.",
                                                                                              "If doing identification with a derivative library, 1 is required.",
                                                                                              "0 should be used if no derivative transformation is desired.",
                                                                                              "Smoothing uses the SG filter on a window of data points with the wavenumber window determining how many points to use.",
                                                                                              "Specifying the wavenumber window larger will make the spectra more smooth.",
                                                                                              "The absolute value does something similar to intensity correction to make the spectra more absorbance-like and is required for comparison with the derivative library."),
                                                                            title =  prettySwitch(inputId = "smooth_decision",
                                                                                                  label = "Smoothing/Derivative",
                                                                                                  inline = T,
                                                                                                  value = T,
                                                                                                  status = "success",
                                                                                                  fill = T),
                                                                            sliderInput("smoother", "Polynomial", min = 0, max = 5, value = 3),
                                                                            sliderInput("derivative_order", "Derivative Order", min = 0, max = 3, value = 1),
                                                                            sliderInput("smoother_window", "Wavenumber Window", min = 50, max = 200, value = 90, step = 5),
                                                                            prettySwitch("derivative_abs", 
                                                                                         label = "Absolute Value",  
                                                                                         inline = T,
                                                                                         value = T,
                                                                                         status = "success",
                                                                                         fill = T))),
                                                                    fluidRow(
                                                                        box(width = 12,
                                                                            footer = footnote("Options for conforming spectra to a new wavenumber resolution.",
                                                                                              "Conformation technique specifies the strategy for performing the conformation.",
                                                                                              "Nearest will use the nearest value to the wavenumber resolution specified, this is faster but less accurate.",
                                                                                              "Linear Interpolation will perform a linear regression between the nearest points to identify the intensity values at the new wavenumbers.",
                                                                                              "Wavenumber Resolution will set the step size in wavenumbers for the new wavenumber values."),
                                                                            title = prettySwitch("conform_decision",
                                                                                                 label = "Conform Wavenumbers",
                                                                                                 inline = T,
                                                                                                 value = T,
                                                                                                 status = "success",
                                                                                                 fill = T),
                                                                            collapsed = T,
                                                                            selectInput(inputId = "conform_selection", 
                                                                                        label = "Conformation Technique", 
                                                                                        choices = c("Linear Interpolation" = "interp",
                                                                                                    "Nearest" = "roll")), 
                                                                            br(),
                                                                            sliderInput("conform_res", "Wavenumber Resolution", min = 4, max = 16, value = 6)
                                                                            
                                                                        )
                                                                    ),
                                                                fluidRow(
                                                                    box(
                                                                    width = 12,
                                                                    collapsed = T,
                                                                    footer = footnote("Open Specy assumes spectra are in Absorbance units.",
                                                                                      "If the uploaded spectrum is not in absorbance units, use this input to specify the units to convert from.",
                                                                                      "The transmittance adjustment uses the log10(1/T) calculation which does not correct for system and particle characteristics.",
                                                                                      "The reflectance adjustment uses the Kubelka-Munk equation (1-R)^2/(2*R).",
                                                                                      "We assume that the reflectance is formatted as a percent from 1-100 and first correct the intensity by dividing by 100 so that it fits the form expected by the equation.",
                                                                                      "If none is selected, Open Specy assumes that the uploaded data is an absorbance spectrum."),
                                                                    title =  prettySwitch(inputId = "intensity_decision",
                                                                                        label = "Intensity Adjustment",
                                                                                        value = F,
                                                                                        inline = T,
                                                                                        status = "success",
                                                                                        fill = T),
                                                                                                radioButtons("intensity_corr", "Intensity Units",
                                                                                                             c("Absorbance" = "none", "Transmittance" = "transmittance", "Reflectance" = "reflectance"))
                                                                                        )),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     footer = footnote("This algorithm automatically fits to the baseline by fitting polynomials of the provided order to the whole spectrum using the iModPolyFit+ algorithm.",
                                                                                                       "New options for the maximum number of iterations while finding the baseline (previously defaulted to 10).",
                                                                                                       "This option tends to impact the outcome of high noise spectra more than low noise spectra.",
                                                                                                       "Additionally an option for whether to refit a polynomial to the baseline found is provided.",
                                                                                                       "Turning this off can produce better baseline subtraction for low wavenumber resolution spectra and better maintain the features of the noise and subtle baseline shapes if that is of interest."),
                                                                                     title = prettySwitch("baseline_decision",
                                                                                                     label = "Baseline Correction",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                     sliderInput("baseline", "Baseline Correction Polynomial", min = 1, max = 20, value = 8),
                                                                                     sliderInput("iterations", "Iterations", min = 1, max = 100, value = 10),
                                                                                     prettySwitch("refit",
                                                                                                  label = "Refit Polynomial",
                                                                                                  inline = T,
                                                                                                  value = F,
                                                                                                  status = "success",
                                                                                                  fill = T)
                                                                             )),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     footer = footnote("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching.",
                                                                                                       "These options control the maximum and minimum wavenumbers in the range to crop the spectra."),
                                                                                     title =  prettySwitch("range_decision",
                                                                                                     label = "Range Selection",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                         numericInput(
                                                                                             "MinRange",
                                                                                             "Minimum Wavenumber",
                                                                                             value = 300,
                                                                                             min = NA,
                                                                                             max = NA,
                                                                                             step = NA,
                                                                                             width = NULL
                                                                                         ),
                                                                                     numericInput(
                                                                                         "MaxRange",
                                                                                         "Maximum Wavenumber",
                                                                                         value = 2000,
                                                                                         min = NA,
                                                                                         max = NA,
                                                                                         step = NA,
                                                                                         width = NULL
                                                                                     ))),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     footer = footnote("Sometimes peaks are undesirable.",
                                                                                                       "These options will replace peak regions with the mean of their edges.",
                                                                                                       "Specify the edge locations of the peaks minimum and maximum wavenumbers to use for flattening.",
                                                                                                       "Defaults are set to flatten the CO2 region in infrared spectra."),
                                                                                     title = prettySwitch("co2_decision",
                                                                                                     label = "Flatten Region",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                     numericInput(
                                                                                         "MinFlat",
                                                                                         "Minimum Wavenumber",
                                                                                         value = 2200,
                                                                                         min = 1,
                                                                                         max = 6000,
                                                                                         step = 1
                                                                                     ),
                                                                                     numericInput(
                                                                                         "MaxFlat",
                                                                                         "Maximum Wavenumber",
                                                                                         value = 2400,
                                                                                         min = 1,
                                                                                         max = 6000,
                                                                                         step = 1
                                                                                     ))),
                                                                fluidRow(
                                                                    box(width = 12,
                                                                        collapsed = T,
                                                                        footer = footnote("Options for showing collapsed versions of identification.",
                                                                                          "This is only useful for hyperspectral image analysis as it will interpret the data as an image of particles and characterize the particles in the summary data by size.",
                                                                                          "Collapse Function will determine how the multiple spectra for each particle are averaged.",
                                                                                          "Logic will determine how particle regions are determined.",
                                                                                          "If Thresholds is set then any Threshold Signal-Noise or Threshold Correlation settings currently chosen will be used.",
                                                                                          "If Identities is chosen then the names of the material class matches determined by the identification routine will be used.",
                                                                                          "If both is specified then both are used together."),
                                                                        title = prettySwitch("collapse_decision",
                                                                                             label = "Collapse Particle Spectra",
                                                                                             inline = T,
                                                                                             value = F,
                                                                                             status = "success",
                                                                                             fill = T),
                                                                        pickerInput(inputId = "collapse_type", label =  "Collapse Function",
                                                                                    choices =  c("Median", "Mean", "Geometric Mean")),
                                                                        pickerInput(inputId = "collapse_log_type", label =  "Particle Region Logic",
                                                                                    choices =  c("Thresholds","Identities", "Both"))
                                                                        
                                                                        )), 
                                                                fluidRow(
                                                                    box(width = 12,
                                                                        collapsed = T,
                                                                        footer = footnote("Spatial Gaussian smoothing of hyperspectral images can reduce background noise and improve image analysis.",
                                                                                          "Increasing this parameter will increase how smooth the image is while decreasing it does the opposite."),
                                                                        title = prettySwitch("spatial_decision",
                                                                                             label = "Spatial Smooth",
                                                                                             inline = T,
                                                                                             value = F,
                                                                                             status = "success",
                                                                                             fill = T),
                                                                        
                                                                        numericInput(
                                                                            "sigma",
                                                                            "Spatial Standard Deviation",
                                                                            value = 1,
                                                                            min = 0.01,
                                                                            max = 3,
                                                                            step = 0.01
                                                                        )
                                                                    )),
                                                                fluidRow(
                                                                    box(width = 12,
                                                                        collapsed = T,
                                                                        footer = footnote("Check that xy grid is continuous",
                                                                                          "This parameter will force uploadeded data and batch uploads to have a continuous grid no matter what the xy coordinates of the data are."),
                                                                        title = prettySwitch("xy_grid",
                                                                                             label = "XY Grid Conform",
                                                                                             inline = T,
                                                                                             value = F,
                                                                                             status = "success",
                                                                                             fill = T)
                                                                    ))
                                                                             
                                                            )
                                                        )
                                                     ),
                                                     ## Identification ----
                                                     column(6, 
                                                                            fluidRow(
                                                                                box(width = 12,
                                                                                    collapsed = T,
                                                                                    footer = footnote("These options define the strategy for identification.",
                                                                                     "The Spectrum Type will inform which library is used. Both (default) will search both FTIR and Raman libraries while specifying FTIR or Raman will only search the specified library.",
                                                                                     "Library Transformation determines how the library is transformed and should be in line with how you choose to process your spectra. Derivative will search against a derivative transformed library.",
                                                                                     "No Baseline will search against a baseline corrected library. Library Type will determine the approach used to identify the spectra. Full (only available through R) will search all available libraries and will be slower but more accurate.",
                                                                                     "Medoid will only search library spectra that have been identified as critical to search. Multinomial will use a multinomial regression to identify the spectra.",
                                                                                     "Correlation thresholding will set the minimum value from matching to use as a 'confident identification' and is typically set to 0.7 but may be different depending on your needs."),
                                                                                    title = prettySwitch(inputId = "active_identification",
                                                                                                    label = "Identification",
                                                                                                    inline = T,
                                                                                                    value = F,
                                                                                                    status = "success",
                                                                                                    fill = T),
                                                                                    pickerInput(inputId = "id_spec_type", label =  "Spectrum Type",
                                                                                                choices =  c("Both" = "both",
                                                                                                             "FTIR" = "ftir",
                                                                                                             "Raman" = "raman")),
                                                                                     pickerInput(inputId = "id_strategy", label =  "Library Transformation",
                                                                                                        choices =  c("Derivative" = "deriv",
                                                                                                                     "No Baseline" = "nobaseline")),
                                                                                     pickerInput(inputId = "lib_type", label =  "Library Type",
                                                                                                choices =  c("Full" = "full",
                                                                                                             "Medoid" = "medoid",
                                                                                                             "Multinomial" = "model")),
                                                                                     conditionalPanel(
                                                                                         condition = "input.lib_type != 'model'",
                                                                                         fluidRow(
                                                                                            box(width = 12,
                                                                                             collapsed = T,
                                                                                             title = prettySwitch(inputId = "filter_lib",
                                                                                                                  label = "Filter Library",
                                                                                                                  inline = T,
                                                                                                                  value = F,
                                                                                                                  status = "success",
                                                                                                                  fill = T),
                                                                                             pickerInput(inputId = "lib_org",
                                                                                                    label = "Library Organization",
                                                                                                    choices = NULL,
                                                                                                    multiple = TRUE,
                                                                                                    options = list(`actions-box` = TRUE))))
                                                                                     ),
                                                                                    fluidRow(
                                                                                        box(width = 12,
                                                                                            collapsed = T,
                                                                                            title = prettySwitch("cor_threshold_decision",
                                                                                                                 label = "Threshold Correlation",
                                                                                                                 inline = T,
                                                                                                                 value = T,
                                                                                                                 status = "success",
                                                                                                                 fill = T), 
                                                                                            numericInput(
                                                                                                "MinCor",
                                                                                                "Minimum Value",
                                                                                                value = 0.7,
                                                                                                min = 0,
                                                                                                max = 1,
                                                                                                step = 0.1#,
                                                                                                #width = '25%'
                                                                                            ),
                                                                                            plotOutput("cor_plot", height = "10vh")
                                                                                            
                                                                                        )
                                                                                    )
                                                                             )  
                                                            )
                                                     ))),
                           column(2,
                                  uiOutput("download_ui"),
                                  uiOutput("top_n")
                           )
                           ),
                          ## Plot ----
                          fluidRow(
                              #verbatimTextOutput("event_test"),
                              box(title = HTML(paste0("Spectra")), 
                                  maximizable = T,
                                  width = 12,
                                  #background = "black",
                                  label = uiOutput("correlation_head"),
                                  h4(id = "placeholder1", "Upload some data to get started..."),
                                  uiOutput("choice_names"),
                                  fluidRow(
                                      column(11, plotlyOutput("heatmapA", inline = TRUE)),
                                      column(1, uiOutput("nav_buttons"))
                                  ),
                                  plotlyOutput("MyPlotC", inline = TRUE),
                                  div(style = "overflow-x: scroll",
                                      DT::dataTableOutput("eventmetadata")
                                  ),
                                  sidebar = boxSidebar(
                                      id = "mycardsidebar",
                                      tabsetPanel(
                                          id = "sidebar_tables",
                                          tabPanel("Library Matches",
                                                   fluidRow(style = "padding:1rem; overflow-x: scroll",
                                                            DT::dataTableOutput("event"))),
                                          tabPanel("Uploaded Metadata",
                                                   fluidRow(style = "padding:1rem; overflow-x: scroll",
                                                            DT::dataTableOutput("sidebar_metadata")))
                                      )
                                  )
                              )
                          ),
                      uiOutput("progress_bars")
                      ),
              tabItem("partner", 
                      #Partner With Us tab ----
                               titlePanel(h4("Help us reach our goal to revolutionize spectroscopy.")),
                               br(),
                      accordion(
                          id = "accordion_partners",
                          accordionItem(
                              title = "Partners",
                              status = "info",
                              collapsed = T,
                              fluidRow(
                                  column(6,
                                         h3("Monetary Partners"),
                                         panel(style = "align: centre",
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                                                   h3("Thriving (10,000–100,000$)"),
                                                   h4("Moore Institute for Plastic Pollution Research"),
                                                   h4("Helmholtz Information & Data Science Academy"),
                                                   h4("National Renewable Energy Laboratory"),
                                                   h4("McPZ Foundation")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                                                   h3("Maintaining (1,000–10,000$)"),
                                                   h5("University of California, Riverside"),
                                                   h5("National Science Foundation"),
                                                   h5("Alfred Wegener Institute"),
                                                   h5("Hawai'i Pacific University"),
                                                   h5("National Institute of Standards and Technology"),
                                                   h5("University of Toronto"),
                                                   h5("University of Koblenz-Landau"),
                                                   h5("Thermo Fisher Scientific")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                                                   h3("Supporting (100–1,000$)"),
                                                   h5( "Jennifer Gadd")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(128, 0, 128, 0.5)",
                                                   h3("Saving (<100$)"),
                                                   h6("Anne Jefferson, Heather Szafranski, Gwendolyn Lattin, Collin Weber, Gregory Gearhart, Anika Ballent, Shelly Moore, Susanne Brander (Oregon State University), Jeremy Conkle (TEXAS  A&M  UNIVERSITY  CORPUS  CHRISTI)")
                                               )
                                         )
                                  ),
                                  column(6,
                                         h3("In-Kind Partners"),
                                         panel(style = "align: centre",
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(205, 127, 50, 0.5)",
                                                   h3("Thriving (10,000–100,000$)"),
                                                   h4("Win Cowger, Zacharias Steinmetz")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(3, 252, 15, 0.5)",
                                                   h3("Maintaining (1,000–10,000$)"),
                                                   h5("Garth Covernton, Jamie Leonard, Shelly Moore, Rachel Kozloski, Katherine Lasdin, Aleksandra Karapetrova, Laura Markley, Walter Yu, Walter Waldman, Vesna Teofilovic, Monica Arienzo, Mary Fey Long Norris, Cristiane Vidal, Scott Coffin, Charles Moore, Aline Carvalho, Shreyas Patankar, Andrea Faltynkova, Sebastian Primpke, Andrew Gray, Chelsea Rochman, Orestis Herodotu, Hannah De Frond, Keenan Munno, Hannah Hapich, Jennifer Lynch")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(0, 0, 255, 0.5)",
                                                   h3("Supporting (100–1,000$)"),
                                                   h6("Alexandre Dehaut, Gabriel Erni Cassola")
                                               )
                                         )
                                  )
                              )
                          ),
                          accordionItem(
                              title = "Donate Cash",
                              status = "info",
                              collapsed = TRUE,
                              img(src = "donation.png", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                              actionButton(inputId = "ab1", label = "Donate", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                           icon = icon("donate"),
                                           onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')")
                              ),
                          accordionItem(
                              title = "Buy Merch",
                              status = "info",
                              collapsed = TRUE,
                              p(class = "lead", "Open Specy merchandise is available through the online shop."),
                              actionButton(inputId = "ab2", label = "Shop", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                           icon = icon("shopping-cart"),
                                           onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")
                          ),
                          accordionItem(
                              title = "Contribute Time",
                              status = "info",
                              collapsed = T,
                                  p(class = "lead", "Volunteer time and expertise help keep the app useful for the community."),
                                        actionButton(inputId = "ab3", label = "Guidelines", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                     icon = icon("clock"),
                                                     onclick ="window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')")
                          ),
                          
                              accordionItem(
                                  title = "Contribute Spectra",
                                  status = "info",
                                  collapsed = TRUE,
                                  p(class = "lead", "To share spectra upload a file to the upload file tab.
                             If you selected Share a copy of your spectra will be sent to the Community
                             Data Warehouse on Open Science Framework. To add additional metadata,
                             fill in the avaliable metadata fields and click -Share Data-. The
                             spectra file that you uploaded along with your responses will be copied
                             to the a -With Metadata- subfolder at the link below. All shared data holds
                             a Creative Commons Attribution License 4.0."),
                                  div(
                                      a("Community Data Warehouse",
                                        onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                        class="btn btn-primary btn-lg",
                                        style = "width: 100%;")
                                  )
                              )
                      )
                ),
              tabItem("contract",
                      div(
                          h2("We are a group of experienced spectroscopists and can provide a variety of services for hire, please contact wincowger@gmail.com to inquire about any of the services below.", style = "color: lightblue;"),
                          h3(tags$ul(
                              tags$li("Adding new features to OpenSpecy"),
                              tags$li("Creating spectroscopy software"),
                              tags$li("Microplastic sample analysis"),
                              tags$li("Spectral identification"),
                              tags$li("Study design"),
                              tags$li("So much more...")
                          ), style = "color: lightyellow;"), 
                          style = "padding: 50px"
                      )
                      
              )
              )
            ),
    
    #Footer ----
    footer = dashboardFooter(
        left = p(citation),
        right = HTML(paste0(uiOutput("translate"), 
                       a(href = "TOS.txt", "Terms And Conditions", class = "lead"),
                       br(),
                       a(href = "privacy_policy.txt", "Privacy Policy", class = "lead")
                       )
        )
    )
)
