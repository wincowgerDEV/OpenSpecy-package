# UI helpers ----
app_section_switch <- function(input_id, label, value = TRUE) {
  div(
    class = "openspecy-section-switch",
    prettySwitch(
      inputId = input_id,
      label = label,
      inline = TRUE,
      value = value,
      status = "success",
      fill = TRUE
    )
  )
}

app_control_box <- function(input_id, label, value = FALSE, ...,
                            note = character()) {
  bs4Dash::box(
    width = 12,
    collapsed = TRUE,
    title = prettySwitch(
      inputId = input_id,
      label = label,
      inline = TRUE,
      value = value,
      status = "success",
      fill = TRUE
    ),
    footer = if(length(note)) {
      do.call(footnote, c(list("What this changes"), as.list(note)))
    },
    ...
  )
}

preprocessing_controls <- tagList(
  app_section_switch("active_preprocessing", "Preprocessing", TRUE),
  app_control_box(
    "make_rel_decision", "Min-Max Normalize", TRUE,
    note = c(
      "Rescales each spectrum between zero and one for comparison.",
      "Turn this off when raw intensity values are needed."
    )
  ),
  app_control_box(
    "smooth_decision", "Smoothing / Derivative", TRUE,
    sliderInput("smoother", "Polynomial", min = 0, max = 5, value = 3),
    sliderInput("derivative_order", "Derivative Order",
                min = 0, max = 3, value = 1),
    sliderInput("smoother_window", "Wavenumber Window",
                min = 50, max = 200, value = 90, step = 5),
    prettySwitch("derivative_abs", "Absolute Value", inline = TRUE,
                 value = TRUE, status = "success", fill = TRUE),
    note = c(
      "Savitzky-Golay smoothing can improve signal-to-noise.",
      "Use derivative order 1 with the derivative identification library."
    )
  ),
  app_control_box(
    "conform_decision", "Conform Wavenumbers", TRUE,
    selectInput(
      "conform_selection", "Conformation Technique",
      choices = c("Linear Interpolation" = "interp", "Nearest" = "roll")
    ),
    sliderInput("conform_res", "Wavenumber Resolution",
                min = 4, max = 16, value = 6),
    note = "Creates a regular shared wavenumber axis at the selected resolution."
  ),
  app_control_box(
    "intensity_decision", "Intensity Adjustment", FALSE,
    radioButtons(
      "intensity_corr", "Intensity Units",
      c("Absorbance" = "none", "Transmittance" = "transmittance",
        "Reflectance" = "reflectance")
    ),
    note = "Convert transmittance or reflectance input to absorbance-like values."
  ),
  app_control_box(
    "baseline_decision", "Baseline Correction", FALSE,
    sliderInput("baseline", "Baseline Correction Polynomial",
                min = 1, max = 20, value = 8),
    sliderInput("iterations", "Iterations", min = 1, max = 100, value = 10),
    prettySwitch("refit", "Refit Polynomial", inline = TRUE,
                 value = FALSE, status = "success", fill = TRUE),
    note = "Fits and removes the spectral baseline with iModPolyFit+."
  ),
  app_control_box(
    "range_decision", "Range Selection", TRUE,
    prettySwitch("range_automate", "Automatic High-Tail Correction",
                 inline = TRUE, value = TRUE, status = "success", fill = TRUE),
    numericInput("MinRange", "Minimum Wavenumber", value = 300),
    numericInput("MaxRange", "Maximum Wavenumber", value = 2000),
    note = c(
      "Automatic mode checks the fully processed spectra and crops a shared high tail only when the batch improves.",
      "Manual bounds are used when automatic mode is off."
    )
  ),
  app_control_box(
    "co2_decision", "Flatten Region", TRUE,
    prettySwitch("co2_automate", "Automatic CO2 Correction",
                 inline = TRUE, value = TRUE, status = "success", fill = TRUE),
    numericInput("MinFlat", "Minimum Wavenumber", value = 2200,
                 min = 1, max = 6000, step = 1),
    numericInput("MaxFlat", "Maximum Wavenumber", value = 2400,
                 min = 1, max = 6000, step = 1),
    note = c(
      "Automatic mode checks the fully processed spectra and flattens CO2 only when the batch improves.",
      "Manual bounds are used when automatic mode is off."
    )
  )
)

identification_controls <- tagList(
  app_section_switch("active_identification", "Identification", TRUE),
  bs4Dash::box(
    width = 12,
    title = "Identification Strategy",
    footer = footnote(
      "Identification options",
      "Choose a spectrum type and a library transformation that matches preprocessing.",
      "The local app supports full, medoid, and multinomial libraries; the browser app uses compact libraries."
    ),
    pickerInput(
      "id_spec_type", "Spectrum Type",
      choices = c("Both" = "both", "FTIR" = "ftir", "Raman" = "raman")
    ),
    pickerInput(
      "id_strategy", "Library Transformation",
      choices = c("Derivative" = "deriv", "No Baseline" = "nobaseline")
    ),
    pickerInput("lib_type", "Library Type", choices = app_library_type_choices())
  ),
  conditionalPanel(
    condition = "input.lib_type != 'model'",
    app_control_box(
      "filter_lib", "Filter Library", FALSE,
      pickerInput(
        "lib_org", "Library Organization", choices = NULL,
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      note = "Limit matching to one or more reference-library organizations."
    )
  )
)

advanced_controls <- tagList(
  div(
    class = "openspecy-section-switch openspecy-section-note",
    tags$strong("Advanced"),
    tags$span("These settings operate independently of the two main switches.")
  ),
  app_control_box(
    "threshold_decision", "Threshold Signal / Noise", FALSE,
    numericInput("MinSNR", "Minimum Value", value = 4,
                 min = -10000, max = 10000, step = 1),
    selectInput(
      "signal_selection", "Signal Thresholding Technique",
      choices = c(
        "Signal Over Noise" = "run_sig_over_noise",
        "Signal Times Noise" = "sig_times_noise",
        "Total Signal" = "log_tot_sig"
      )
    ),
    div(class = "openspecy-mini-plot", plotOutput("snr_plot", height = "16vh")),
    note = "Threshold batch/map spectra and preview the selected cutoff."
  ),
  app_control_box(
    "cor_threshold_decision", "Threshold Correlation", TRUE,
    numericInput("MinCor", "Minimum Value", value = 0.7,
                 min = 0, max = 1, step = 0.1),
    div(class = "openspecy-mini-plot", plotOutput("cor_plot", height = "16vh")),
    note = "Set the minimum match score used for a confident identification."
  ),
  app_control_box(
    "spatial_decision", "Spatial Smooth", FALSE,
    numericInput("sigma", "Spatial Standard Deviation", value = 1,
                 min = 0.01, max = 3, step = 0.01),
    note = "Apply Gaussian smoothing across a hyperspectral image."
  ),
  app_control_box(
    "xy_grid", "XY Grid Conform", FALSE,
    note = "Replace discontinuous uploaded map coordinates with a continuous XY grid."
  ),
  app_control_box(
    "collapse_decision", "Collapse Particle Spectra", FALSE,
    pickerInput(
      "collapse_type", "Collapse Function",
      choices = c("Median", "Mean", "Geometric Mean")
    ),
    pickerInput(
      "collapse_log_type", "Particle Region Logic",
      choices = c("Thresholds", "Identities", "Both")
    ),
    note = "Combine spectra within inferred particle regions for hyperspectral maps."
  )
)

# UI ----
dashboardPage(
  dark = TRUE,
  help = TRUE,
  fullscreen = TRUE,
  header = dashboardHeader(
    title = tags$a(
      href = "https://www.openanalysis.org",
      target = "_blank",
      tags$img(
        src = "logo.png",
        alt = "Open Analysis",
        style = paste(
          "display:block;width:100%;height:50px;",
          "object-fit:contain;padding:4px 8px;"
        )
      )
    ),
    tags$li(
      class = "dropdown",
      style = "list-style-type:none;",
      tags$a(
        app_version_display$text,
        href = app_version_display$href,
        target = "_blank",
        title = app_version_display$title,
        style = "font-size:19px;text-decoration:none;"
      )
    )
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(src = "parent-frame.js"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$style(HTML("
        :root {
          --openspecy-panel: #0b1220;
          --openspecy-panel-2: #111c2f;
          --openspecy-border: #31506f;
          --openspecy-accent: #38bdf8;
          --openspecy-accent-2: #fb7185;
          --openspecy-text: #e6edf7;
          --openspecy-muted: #a9b8cb;
        }
        .content-wrapper { background: #07101d !important; }
        .content { padding-top: 18px; }
        .openspecy-app-main { max-width: 1800px; margin: 0 auto; }
        #analysis_settings_box {
          border: 1px solid var(--openspecy-border);
          background: var(--openspecy-panel);
          box-shadow: 0 12px 28px rgba(0, 0, 0, .32);
        }
        #analysis_settings_box .nav-tabs {
          border-bottom: 1px solid var(--openspecy-border);
        }
        #analysis_settings_box .nav-link { color: var(--openspecy-muted); }
        #analysis_settings_box .nav-link.active {
          color: #fff;
          background: #17324d;
          border-color: var(--openspecy-accent);
        }
        .openspecy-tab-scroll {
          max-height: 50vh;
          overflow-y: auto;
          padding: 10px 6px 0;
        }
        .openspecy-section-switch {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
          padding: 10px 12px;
          margin-bottom: 10px;
          border: 1px solid var(--openspecy-border);
          border-radius: 8px;
          background: var(--openspecy-panel-2);
        }
        .openspecy-section-note { align-items: baseline; color: var(--openspecy-text); }
        .openspecy-section-note span { color: var(--openspecy-muted); }
        .openspecy-download-panel,
        .openspecy-download-details {
          display: block;
          padding: 12px;
          margin-bottom: 12px;
          border: 1px solid var(--openspecy-border);
          border-radius: 8px;
          background: var(--openspecy-panel);
        }
        .openspecy-download-button {
          display: block;
          width: 100%;
          margin-bottom: 10px;
          color: #06111d !important;
          background: var(--openspecy-accent) !important;
          border-color: #7dd3fc !important;
          font-weight: 700;
          text-align: center;
        }
        .openspecy-download-details summary {
          cursor: pointer;
          color: var(--openspecy-text);
          font-weight: 600;
        }
        .openspecy-plot-frame,
        .openspecy-mini-plot {
          overflow: hidden;
          border: 1px solid var(--openspecy-border);
          border-radius: 10px;
          background: var(--openspecy-panel);
          box-shadow: inset 0 0 0 1px rgba(56, 189, 248, .06),
                      0 8px 24px rgba(0, 0, 0, .28);
        }
        .openspecy-plot-frame { padding: 8px; margin: 8px 0 16px; }
        .openspecy-mini-plot { margin-top: 8px; }
        .shiny-output-error-validation { color: #7dd3fc; font-size: 130%; }
        #openspecy_busy_overlay {
          display: none;
          position: fixed;
          inset: 60px 0 0 0;
          background: rgba(7, 16, 29, .78);
          z-index: 1030;
          pointer-events: all;
          align-items: center;
          justify-content: center;
        }
        html.openspecy-busy-visible #openspecy_busy_overlay { display: flex; }
        .openspecy-busy-card {
          width: min(620px, calc(100vw - 40px));
          padding: 24px 28px;
          border: 1px solid var(--openspecy-border);
          border-radius: 10px;
          background: rgba(11, 18, 32, .98);
          color: var(--openspecy-text);
          text-align: center;
          box-shadow: 0 18px 50px rgba(0, 0, 0, .5);
        }
        .openspecy-busy-spinner {
          width: 42px;
          height: 42px;
          margin: 0 auto 16px;
          border: 4px solid rgba(125, 211, 252, .24);
          border-top-color: var(--openspecy-accent);
          border-radius: 50%;
          animation: openspecy-spin 1s linear infinite;
        }
        #openspecy_busy_message { margin: 0 0 8px; }
        #openspecy_busy_detail { margin: 0 0 12px; color: var(--openspecy-muted); }
        #openspecy_busy_elapsed { margin: 4px 0 10px; color: #d7e4f3; }
        .openspecy-progress-track {
          height: 10px;
          overflow: hidden;
          border: 1px solid var(--openspecy-border);
          border-radius: 999px;
          background: #050b14;
        }
        #openspecy_busy_progress_fill {
          width: 4%;
          height: 100%;
          border-radius: inherit;
          background: linear-gradient(90deg, #0ea5e9, #67e8f9);
          transition: width .35s ease;
        }
        @keyframes openspecy-spin { to { transform: rotate(360deg); } }
        @media (prefers-reduced-motion: reduce) {
          .openspecy-busy-spinner { animation: none; }
          #openspecy_busy_progress_fill { transition: none; }
        }
        @media (max-width: 991px) {
          .openspecy-tab-scroll { max-height: none; }
          .openspecy-section-note { align-items: flex-start; flex-direction: column; }
        }
      "))
    ),
    tags$div(
      id = "openspecy_busy_overlay",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      `aria-hidden` = "true",
      tags$div(
        class = "openspecy-busy-card",
        tags$div(class = "openspecy-busy-spinner", `aria-hidden` = "true"),
        tags$h3(id = "openspecy_busy_message", "Preparing analysis..."),
        tags$p(id = "openspecy_busy_detail",
               "Open Specy is preparing the next result."),
        tags$p(id = "openspecy_busy_elapsed", "Elapsed: 0 seconds"),
        tags$div(
          id = "openspecy_busy_progress",
          class = "openspecy-progress-track",
          role = "progressbar",
          `aria-label` = "Expected analysis progress",
          `aria-valuemin` = "0",
          `aria-valuemax` = "100",
          `aria-valuenow` = "4",
          tags$div(id = "openspecy_busy_progress_fill", `aria-hidden` = "true")
        )
      )
    ),
    div(
      class = "openspecy-app-main",
      fluidRow(
        column(
          2,
          fileInput(
            "file", NULL, multiple = TRUE,
            placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, ...",
            accept = c(
              "text/csv", "text/comma-separated-values,text/plain", ".csv",
              ".asp", ".tsv", ".spc", ".jdx", ".dx", ".RData", ".spa",
              ".0", ".zip", ".img", ".h5", ".txt", ".json", ".rds",
              ".hdr", ".dat"
            )
          ) %>%
            bs4Dash::popover(
              title = "Upload Raman or FTIR spectra. CSV files should contain wavenumber and intensity columns; ZIP files may contain batches or ENVI maps.",
              content = "File Upload", placement = "right"
            )
        ),
        column(
          8,
          bs4Dash::tabBox(
            id = "analysis_settings",
            selected = "preprocessing",
            width = 12,
            title = NULL,
            tabPanel(
              "Preprocessing",
              value = "preprocessing",
              div(class = "openspecy-tab-scroll", preprocessing_controls)
            ),
            tabPanel(
              "Identification",
              value = "identification",
              div(class = "openspecy-tab-scroll", identification_controls)
            ),
            tabPanel(
              "Advanced",
              value = "advanced",
              div(class = "openspecy-tab-scroll", advanced_controls)
            )
          )
        ),
        column(
          2,
          div(
            class = "openspecy-download-panel",
            shiny::downloadButton(
              "download_data",
              "Download selected",
              class = "openspecy-download-button",
              title = "Download the selected test data or current analysis result"
            ),
            uiOutput("download_ui")
          ),
          uiOutput("top_n")
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Spectra",
          maximizable = TRUE,
          width = 12,
          label = uiOutput("correlation_head"),
          h4(id = "placeholder1", "Upload some data to get started..."),
          uiOutput("choice_names"),
          fluidRow(
            column(
              11,
              div(
                id = "heatmap_frame",
                class = "openspecy-plot-frame",
                plotlyOutput("heatmapA")
              )
            ),
            column(1, uiOutput("nav_buttons"))
          ),
          div(
            class = "openspecy-plot-frame openspecy-spectrum-frame",
            plotlyOutput("MyPlotC", height = "45vh")
          ),
          div(
            style = "overflow-x:auto",
            DT::dataTableOutput("eventmetadata")
          ),
          sidebar = boxSidebar(
            id = "mycardsidebar",
            tabsetPanel(
              id = "sidebar_tables",
              tabPanel(
                "Library Matches",
                conditionalPanel(
                  condition = "input.active_identification",
                  fluidRow(
                    style = "padding:1rem;overflow-x:auto",
                    DT::dataTableOutput("event")
                  )
                )
              ),
              tabPanel(
                "Uploaded Metadata",
                fluidRow(
                  style = "padding:1rem;overflow-x:auto",
                  DT::dataTableOutput("sidebar_metadata")
                )
              )
            )
          )
        )
      ),
      uiOutput("progress_bars")
    )
  ),
  footer = dashboardFooter(
    left = p(citation),
    right = tagList(
      a(href = "TOS.txt", "Terms and Conditions", class = "lead"),
      br(),
      a(href = "privacy_policy.txt", "Privacy Policy", class = "lead")
    )
  )
)
