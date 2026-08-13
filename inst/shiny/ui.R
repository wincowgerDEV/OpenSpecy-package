# UI helpers ----
app_section_switch <- function(input_id, label, value = TRUE,
                               note = character()) {
  note <- trimws(as.character(note))
  note <- paste(note[!is.na(note) & nzchar(note)], collapse = " ")
  has_note <- nzchar(note)

  div(
    class = if(has_note) {
      "openspecy-section-switch openspecy-section-switch-with-note"
    } else {
      "openspecy-section-switch"
    },
    prettySwitch(
      inputId = input_id,
      label = label,
      inline = TRUE,
      value = value,
      status = "success",
      fill = TRUE
    ),
    if(has_note) tags$span(class = "openspecy-section-description", note)
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
  app_section_switch(
    "active_preprocessing", "Preprocessing", TRUE,
    "Transforms uploaded spectra before artifact checks and identification."
  ),
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
    selectInput(
      "baseline_method", "Baseline Method",
      choices = c(
        "Modified Polynomial (iModPolyFit+)" = "polynomial",
        "Fill Peaks (4S)" = "fill_peaks"
      )
    ),
    conditionalPanel(
      condition = "input.baseline_method == 'polynomial'",
      sliderInput("baseline", "Baseline Correction Polynomial",
                  min = 1, max = 20, value = 8),
      prettySwitch("refit", "Refit Polynomial", inline = TRUE,
                   value = FALSE, status = "success", fill = TRUE)
    ),
    conditionalPanel(
      condition = "input.baseline_method == 'fill_peaks'",
      sliderInput("baseline_lambda", "Primary Smoothing Penalty",
                  min = 0, max = 12, value = 4, step = 1),
      numericInput("baseline_hwi", "Local Half-Window (buckets)",
                   value = 50, min = 1, step = 1)
    ),
    sliderInput("iterations", "Iterations", min = 1, max = 100, value = 10),
    note = c(
      "Modified polynomial fitting estimates a whole-spectrum baseline.",
      "Fill Peaks iteratively suppresses peaks in local windows and is useful for nonlinear or locally varying baselines."
    )
  ),
  app_control_box(
    "range_decision", "Range Selection", TRUE,
    prettySwitch("range_automate", "Automatic High-Tail Correction",
                 inline = TRUE, value = TRUE, status = "success", fill = TRUE),
    conditionalPanel(
      condition = "input.range_automate",
      numericInput(
        "range_artifact_ratio", "Artifact Ratio Threshold",
        value = 3, min = 1.1, step = 0.1
      )
    ),
    div(
      id = "manual_range_bounds",
      class = "openspecy-manual-range openspecy-inputs-disabled",
      shinyjs::disabled(
        numericInput("MinRange", "Manual Minimum Wavenumber", value = 300)
      ),
      shinyjs::disabled(
        numericInput("MaxRange", "Manual Maximum Wavenumber", value = 2000)
      )
    ),
    note = c(
      "Automatic mode scans the full processed wavenumber axis and crops a shared high tail only when its artifact ratio exceeds the threshold and the batch improves.",
      "Manual bounds are ignored and locked while automatic mode is on; turn it off to set the range yourself."
    )
  ),
  app_control_box(
    "co2_decision", "Flatten Region", TRUE,
    prettySwitch("co2_automate", "Automatic CO2 Correction",
                 inline = TRUE, value = TRUE, status = "success", fill = TRUE),
    conditionalPanel(
      condition = "input.co2_automate",
      numericInput(
        "co2_artifact_ratio", "Artifact Ratio Threshold",
        value = 3, min = 1.1, step = 0.1
      )
    ),
    numericInput("MinFlat", "Minimum Wavenumber", value = 2200,
                 min = 1, max = 6000, step = 1),
    numericInput("MaxFlat", "Maximum Wavenumber", value = 2400,
                 min = 1, max = 6000, step = 1),
    note = c(
      "Minimum and Maximum Wavenumber define the CO2 or artifact region tested by automatic mode and flattened when correction is retained.",
      "Automatic mode corrects that region only when its artifact ratio exceeds the threshold and the batch improves; manual mode uses the same bounds directly."
    )
  ),
  app_control_box(
    "spike_decision", "Remove Isolated Spikes", TRUE,
    selectInput(
      "spike_direction", "Spike Direction",
      choices = c("Positive and negative" = "both",
                  "Positive only" = "positive",
                  "Negative only" = "negative"),
      selected = "both"
    ),
    numericInput(
      "spike_residual_threshold", "Robust Residual Threshold",
      value = 8, min = 3, step = 0.5
    ),
    numericInput(
      "spike_residual_window", "Neighbor Points per Side",
      value = 5, min = 2, step = 1
    ),
    note = c(
      "Spike Direction tests upward impulses, downward impulses, or both relative to a wavenumber-aware prediction from neighboring samples.",
      "Robust Residual Threshold is the prediction error scaled by local median absolute deviation: higher values are more conservative, while lower values admit more candidates.",
      "Neighbor Points per Side is the number of finite samples used on each side of a candidate; larger windows add context but can span narrow real bands.",
      "Only one-point candidates that pass boundary, interpolation, and band-protection safeguards are replaced. Safeguarded candidates remain unchanged and are explained under Automatic Corrections Made."
    )
  ),
  app_control_box(
    "saturation_decision", "Remove Saturated Ranges", FALSE,
    selectInput(
      "saturation_mode", "Saturation Detection",
      choices = c("Automatic hard plateaus" = "auto",
                  "Detector ceiling" = "threshold"),
      selected = "auto"
    ),
    conditionalPanel(
      condition = "input.saturation_mode == 'threshold'",
      numericInput(
        "saturation_ceiling", "Detector Ceiling", value = 65535,
        min = 0, step = 1
      )
    ),
    sliderInput(
      "saturation_max_loss", "Maximum Shared-Axis Loss",
      min = 0, max = 0.7, value = 0.7, step = 0.05
    ),
    note = c(
      "Saturation Detection chooses either automatic hard-plateau detection (at least two adjacent, effectively constant values at a spectrum maximum) or a known detector ceiling.",
      "Detector Ceiling is expressed in the uploaded intensity units; threshold mode flags values at or above it, such as 65535 for appropriate 16-bit detector data.",
      "Maximum Shared-Axis Loss is the permitted fraction of wavenumber coverage removed from every spectrum (0.10 means 10%); lower values reject more proposals and the maximum is 0.70.",
      "Accepted intervals receive a one-sample guard and are removed identically across the batch. A proposal is a no-op when it exceeds the loss limit or leaves too few shared points, because reconstructing clipped peaks would be scientifically unsafe."
    )
  )
)

identification_controls <- tagList(
  app_section_switch(
    "active_identification", "Identification", TRUE,
    "Matches processed spectra to references and displays the best results."
  ),
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
    pickerInput(
      "lib_type", "Library Type",
      choices = app_library_type_choices(), selected = "medoid"
    )
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
  app_section_switch(
    "active_advanced", "Advanced", FALSE,
    "Enables the map thresholds, spatial controls, and particle pipeline below. Turning this off negates every Advanced setting."
  ),
  if(app_local_file_mode()) bs4Dash::box(
    id = "filespec_source_box",
    title = "Local H5 / ENVI source",
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    tags$p(
      class = "openspecy-filespec-intro",
      paste(
        "Open one H5 file or ENVI .hdr/.dat/.img member by local path.",
        "The source stays read-only and spectra stream from disk. Configure",
        "the particle settings below before opening the source."
      )
    ),
    fluidRow(
      column(
        8,
        textInput(
          "filespec_path", "Source path",
          placeholder = "C:/data/hyperspectral-map.h5"
        )
      ),
      column(
        4,
        actionButton(
          "filespec_open", "Open read-only",
          icon = icon("folder-open"), class = "btn-primary"
        )
      )
    ),
    tags$p(
      class = "openspecy-filespec-status",
      textOutput("filespec_status", container = tags$span)
    ),
    conditionalPanel(
      condition = "output.filespec_active === true",
      selectInput("filespec_region", "Map region", choices = character()),
      div(
        class = "openspecy-filespec-toolbar",
        actionButton("filespec_close", "Close source",
                     icon = icon("times"), class = "btn-outline-danger")
      ),
      tags$p(
        class = "openspecy-filespec-status",
        textOutput("filespec_view_status", container = tags$span)
      )
    )
  ),
  app_control_box(
    "threshold_decision", "Threshold Signal / Noise", FALSE,
    fluidRow(
      column(
        6,
        numericInput("MinSNR", "Minimum Value", value = 4, step = 1)
      ),
      column(
        6,
        numericInput("MaxSNR", "Maximum Value", value = 1e12, step = 1)
      )
    ),
    selectInput(
      "signal_selection", "Signal Thresholding Technique",
      choices = c(
        "Signal Over Noise" = "run_sig_over_noise",
        "Signal Times Noise" = "sig_times_noise",
        "Total Signal" = "log_tot_sig"
      )
    ),
    div(class = "openspecy-mini-plot", uiOutput("snr_plot_ui")),
    note = c(
      "Minimum and Maximum Value define the accepted interval on the selected metric scale; the histogram draws both current thresholds.",
      "Signal Over Noise is a local peak-to-noise ratio, Signal Times Noise emphasizes absolute response, and Total Signal sums intensity. Larger values are not interchangeable between metrics.",
      "Pixels outside either bound are background. The default maximum is deliberately high; lower it when saturated or unusually intense pixels must be excluded."
    )
  ),
  app_control_box(
    "cor_threshold_decision", "Threshold Correlation", TRUE,
    numericInput("MinCor", "Minimum Value", value = 0.7,
                 min = 0, max = 1, step = 0.1),
    div(class = "openspecy-mini-plot", uiOutput("cor_plot_ui")),
    note = "Set the minimum match score used for a confident identification."
  ),
  app_control_box(
    "spatial_decision", "Spatial Smooth", FALSE,
    numericInput("sigma", "Spatial Standard Deviation", value = 1,
                 min = 0.01, max = 3, step = 0.01),
    note = "Apply Gaussian smoothing to ordinary in-memory maps. This does not alter the bounded FileSpecs particle pipeline."
  ),
  app_control_box(
    "xy_grid", "XY Grid Conform", FALSE,
    note = "Replace discontinuous uploaded map coordinates with a continuous XY grid."
  ),
  app_control_box(
    "collapse_decision", "Collapse Particle Spectra", FALSE,
    pickerInput(
      "collapse_type", "Collapse Function",
      choices = c("Mean", "Median", "Geometric Mean"), selected = "Mean"
    ),
    pickerInput(
      "particle_id_strategy", "Particle ID Strategy",
      choices = c(
        "Connected threshold regions" = "collapse",
        "Spectral clusters within regions" = "partial_collapse",
        "Non-spatial spectral clusters" = "nonspatial_collapse",
        "Per-cell identities" = "all_cell_id"
      ),
      selected = "collapse"
    ),
    numericInput(
      "particle_area_threshold", "Minimum Particle Area (pixels)",
      value = 1, min = 0, step = 1
    ),
    note = c(
      "Runs automate_particle_analysis() only while both Advanced and this switch are on. Turning collapse off leaves raw spectra in the ordinary app workflow.",
      "Connected threshold regions is the bounded FileSpecs strategy. Other strategies are available for ordinary in-memory maps; raw is intentionally represented by turning this switch off.",
      "Minimum Particle Area rejects connected regions at or below the selected pixel area."
    )
  )
)

quantification_controls <- tagList(
  app_section_switch("active_quantification", "Quantification", FALSE),
  bs4Dash::box(
    width = 12,
    title = "Custom Ratios",
    textInput(
      "quant_ratio_name", "Ratio Name",
      placeholder = "For example: Carbonyl index"
    ),
    radioButtons(
      "quant_ratio_type", "Ratio Type",
      choices = c("Area ratio" = "area", "Peak ratio" = "peak"),
      selected = "area",
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.quant_ratio_type == 'area'",
      fluidRow(
        column(
          6,
          numericInput(
            "quant_numerator_area_min", "Numerator minimum (cm^-1)",
            value = 1650, step = 1
          )
        ),
        column(
          6,
          numericInput(
            "quant_numerator_area_max", "Numerator maximum (cm^-1)",
            value = 1850, step = 1
          )
        )
      ),
      fluidRow(
        column(
          6,
          numericInput(
            "quant_denominator_area_min", "Denominator minimum (cm^-1)",
            value = 1420, step = 1
          )
        ),
        column(
          6,
          numericInput(
            "quant_denominator_area_max", "Denominator maximum (cm^-1)",
            value = 1500, step = 1
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.quant_ratio_type == 'peak'",
      fluidRow(
        column(
          6,
          numericInput(
            "quant_numerator_peak", "Numerator point (cm^-1)",
            value = 1715, step = 1
          )
        ),
        column(
          6,
          numericInput(
            "quant_denominator_peak", "Denominator point (cm^-1)",
            value = 1460, step = 1
          )
        )
      )
    ),
    div(
      class = "openspecy-quant-builder-actions",
      actionButton(
        "quant_ratio_add", "Add Ratio",
        icon = icon("plus"),
        class = "openspecy-add-ratio-button"
      )
    ),
    div(
      class = "openspecy-saved-ratios",
      tags$h5("Saved Ratios"),
      uiOutput("quant_saved_ratios")
    ),
    footer = footnote(
      "How ratios are calculated",
      "Every ratio uses exactly the final processed uploaded spectrum visible as the primary trace in the Spectra plot; reference-match overlays are not used and no separate quantification treatment is applied.",
      "For one polyethylene carbonyl-area scenario, choose Area ratio, name it Carbonyl area, use 1650-1850 cm^-1 as the numerator and 1420-1500 cm^-1 as the denominator, then click Add Ratio.",
      "Choose Peak ratio when a method compares two individual wavenumbers. Confirm suitable bands and preprocessing for the material, instrument, and method you are following."
    )
  ),
  bs4Dash::box(
    width = 12,
    title = "Single Measurements",
    textInput(
        "quant_measurement_name", "Measurement Name",
        placeholder = "For example: Carbonyl area"
      ),
      radioButtons(
        "quant_measurement_type", "Measurement Type",
        choices = c(
          "Area under a region" = "area",
          "Single-wavenumber intensity" = "intensity"
        ),
        selected = "area",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.quant_measurement_type == 'area'",
        fluidRow(
          column(
            6,
            numericInput(
              "quant_measurement_area_min", "Region minimum (cm^-1)",
              value = 1650, step = 1
            )
          ),
          column(
            6,
            numericInput(
              "quant_measurement_area_max", "Region maximum (cm^-1)",
              value = 1850, step = 1
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.quant_measurement_type == 'intensity'",
        numericInput(
          "quant_measurement_wavenumber", "Wavenumber (cm^-1)",
          value = 1715, step = 1
        )
      ),
      div(
        class = "openspecy-quant-builder-actions",
        actionButton(
          "quant_measurement_add", "Add Measurement",
          icon = icon("plus"),
          class = "openspecy-add-measurement-button"
        ),
        actionButton(
          "quant_measurement_clear", "Clear All",
          icon = icon("eraser"),
          class = "btn-outline-warning"
        )
      ),
      div(
        class = "openspecy-saved-measurements",
        tags$h5("Saved Measurements"),
        uiOutput("quant_measurement_definitions"),
        selectInput(
          "quant_measurement_remove_id", "Saved measurements",
          choices = character()
        ),
        actionButton(
          "quant_measurement_remove", "Remove Selected",
          icon = icon("trash"),
          class = "btn-outline-danger"
        )
      ),
    footer = footnote(
      "How single measurements are calculated",
      "Area measurements integrate one selected wavenumber region; intensity measurements use the nearest available value to one requested wavenumber.",
      "Saved ratios and saved single measurements can be calculated together from the same final processed spectra."
    )
  )
)

# UI ----
dashboardPage(
  dark = NULL,
  help = NULL,
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
    rightUi = tagList(
      tags$li(
        class = "dropdown nav-item openspecy-version-item",
        tags$a(
          app_version_display$text,
          class = "nav-link openspecy-version-link",
          href = app_version_display$href,
          target = "_blank",
          title = app_version_display$title
        )
      ),
      tags$li(
        class = "dropdown nav-item openspecy-support-item",
        actionButton(
          "support_openspecy",
          "Support Open Source Software",
          icon = icon("donate"),
          class = "openspecy-support-button",
          title = "Open donation options for Open Specy"
        )
      )
    )
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$meta(
        name = "openspecy-wasm-mode",
        content = if(app_wasm_mode()) "true" else "false"
      ),
      tags$script(src = "parent-frame.js"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$style(HTML(paste0(
        app_theme_css(),
        "
        html,
        body,
        .wrapper,
        .content-wrapper { background: var(--openspecy-canvas) !important; }
        body,
        .content-wrapper,
        .content-wrapper a:not(.btn) { color: var(--openspecy-text); }
        .main-header.navbar,
        .main-header .navbar,
        .main-footer {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-border) !important;
          box-shadow: 0 3px 18px rgba(0, 0, 0, .34);
        }
        .main-header.navbar { border-bottom: 1px solid var(--openspecy-border) !important; }
        .main-footer { border-top: 1px solid var(--openspecy-border) !important; }
        .main-header a,
        .main-footer a { color: var(--openspecy-accent) !important; }
        .content { padding-top: 18px; }
        .openspecy-app-main { max-width: 1800px; margin: 0 auto; }
        .card {
          border: 1px solid var(--openspecy-border);
          background: var(--openspecy-panel) !important;
          color: var(--openspecy-text);
          box-shadow: 0 12px 28px rgba(0, 0, 0, .32);
        }
        .card-header,
        .card-footer {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .card-body { background: var(--openspecy-panel) !important; }
        .card-title,
        .card-title a { color: var(--openspecy-text) !important; }
        .card .btn-tool { color: var(--openspecy-accent) !important; }
        .card .btn-tool:hover,
        .card .btn-tool:focus {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
        }
        #analysis_settings_box,
        #download_panel_box {
          width: 100%;
          margin-bottom: 18px;
        }
        #analysis_settings_box > .card-header,
        #download_panel_box > .card-header {
          min-height: 52px;
        }
        #analysis_settings_box .nav-tabs {
          border-bottom: 1px solid var(--openspecy-grid);
          flex-wrap: nowrap;
          overflow-x: auto;
          overflow-y: hidden;
        }
        #analysis_settings_box .nav-link {
          color: var(--openspecy-muted);
          border: 1px solid transparent;
          border-radius: 6px;
          white-space: nowrap;
        }
        #analysis_settings_box .nav-link:hover,
        #analysis_settings_box .nav-link:focus {
          color: var(--openspecy-text);
          border-color: var(--openspecy-grid);
          background: var(--openspecy-panel);
        }
        #analysis_settings_box .nav-link.active {
          color: var(--openspecy-text);
          background: var(--openspecy-panel-2);
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
        .openspecy-section-description {
          flex: 1 1 auto;
          max-width: 72%;
          color: var(--openspecy-muted);
          font-size: .92rem;
          line-height: 1.35;
          text-align: right;
        }
        .openspecy-automation-status {
          display: block;
          padding: 9px 11px;
          margin: 10px 0 4px;
          color: var(--openspecy-muted);
          background: var(--openspecy-canvas);
          border: 1px solid var(--openspecy-grid);
          border-left: 3px solid var(--openspecy-accent);
          border-radius: 6px;
        }
        .openspecy-automation-status:empty { display: none; }
        .openspecy-quality-controls {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 10px;
          margin: 10px 0 8px;
        }
        .btn.openspecy-quality-button {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          gap: .45rem;
          min-height: 44px;
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border: 1px solid var(--openspecy-grid) !important;
          font-weight: 700;
        }
        .btn.openspecy-quality-button:hover,
        .btn.openspecy-quality-button:focus {
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-accent) !important;
          box-shadow: 0 0 0 .16rem rgba(56, 189, 248, .2);
        }
        .btn.openspecy-quality-automatic {
          border-color: var(--openspecy-grid) !important;
        }
        .btn.openspecy-quality-warning { border-color: #FACC15 !important; }
        .btn.openspecy-quality-success {
          border-color: var(--openspecy-success) !important;
        }
        .openspecy-quality-icon-warning { color: #FACC15 !important; }
        .openspecy-quality-icon-success {
          color: var(--openspecy-success) !important;
        }
        .openspecy-quality-automatic.openspecy-automatic-applied {
          border: 2px solid transparent !important;
          background:
            linear-gradient(var(--openspecy-panel-2), var(--openspecy-panel-2)) padding-box,
            linear-gradient(90deg, #FB7185, #E69F00, #F0E442, #009E73, #56B4E9, #CC79A7) border-box !important;
        }
        .openspecy-quality-automatic.openspecy-automatic-applied
          .openspecy-quality-icon-automatic {
          color: transparent !important;
          background: linear-gradient(
            90deg, #FB7185, #E69F00, #F0E442, #009E73, #56B4E9, #CC79A7
          );
          background-clip: text;
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
        }
        .openspecy-quality-count { min-width: 1ch; }
        .openspecy-quality-finding {
          padding: 12px 14px;
          margin-bottom: 12px;
          border: 1px solid var(--openspecy-grid);
          border-radius: 8px;
          background: var(--openspecy-panel-2);
        }
        .openspecy-quality-finding-warning { border-color: #FACC15; }
        .openspecy-quality-finding-success,
        .openspecy-quality-finding-pass {
          border-color: var(--openspecy-success);
        }
        .openspecy-quality-finding-automatic.openspecy-automatic-applied {
          border: 2px solid transparent;
          background:
            linear-gradient(var(--openspecy-panel-2), var(--openspecy-panel-2)) padding-box,
            linear-gradient(90deg, #FB7185, #E69F00, #F0E442, #009E73, #56B4E9, #CC79A7) border-box;
        }
        .openspecy-quality-finding h4 {
          color: var(--openspecy-text);
          text-transform: capitalize;
        }
        .openspecy-quality-finding p:last-child { margin-bottom: 0; }
        .openspecy-download-details {
          display: block;
          padding: 12px;
          margin-bottom: 12px;
          border: 1px solid var(--openspecy-border);
          border-radius: 8px;
          background: var(--openspecy-panel);
        }
        .btn.openspecy-download-button {
          display: inline-flex;
          align-items: center;
          gap: .65rem;
          width: 100% !important;
          max-width: 100% !important;
          margin: 0;
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
          border-color: var(--openspecy-accent) !important;
          font-weight: 700;
          text-align: center;
          justify-content: center;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          box-sizing: border-box;
        }
        #download_panel_box .card-title {
          width: calc(100% - 44px);
          max-width: calc(100% - 44px);
          flex: 0 0 calc(100% - 44px);
          margin: 0;
        }
        #download_panel_box .card-body { padding-top: 14px; }
        .openspecy-download-details summary {
          cursor: pointer;
          color: var(--openspecy-text);
          font-weight: 600;
        }
        .openspecy-info-details {
          margin-top: 4px;
          color: var(--openspecy-muted);
        }
        .openspecy-info-details summary {
          cursor: pointer;
          color: var(--openspecy-text);
          font-weight: 600;
        }
        .openspecy-info-details-body { padding-top: 8px; }
        .openspecy-info-details-body p { margin: 0 0 7px; }
        .openspecy-info-details-body p:last-child { margin-bottom: 0; }
        .openspecy-support-item {
          display: flex;
          align-items: center;
          margin-right: 10px;
        }
        .openspecy-version-item { display: flex; align-items: center; }
        .openspecy-version-link {
          font-size: 19px;
          text-decoration: none;
          white-space: nowrap;
        }
        .btn.openspecy-support-button {
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
          border-color: var(--openspecy-accent) !important;
          font-weight: 700;
          white-space: nowrap;
        }
        .openspecy-quant-builder-actions {
          display: flex;
          justify-content: flex-end;
          gap: .5rem;
          margin: 4px 0 14px;
        }
        .btn.openspecy-add-ratio-button,
        .btn.openspecy-add-measurement-button {
          display: inline-flex;
          align-items: center;
          gap: .5rem;
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
          border-color: var(--openspecy-accent) !important;
          font-weight: 700;
        }
        .openspecy-saved-ratios,
        .openspecy-saved-measurements {
          padding-top: 12px;
          border-top: 1px solid var(--openspecy-grid);
        }
        .openspecy-saved-ratios h5,
        .openspecy-saved-measurements h5 { color: var(--openspecy-text); }
        .modal-content {
          color: var(--openspecy-text);
          background: var(--openspecy-panel);
          border: 1px solid var(--openspecy-border);
        }
        .modal-header,
        .modal-footer { border-color: var(--openspecy-grid); }
        .modal-header .close { color: var(--openspecy-text); text-shadow: none; }
        .openspecy-donation-options {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          margin-top: 12px;
        }
        .btn.openspecy-donation-link {
          min-width: 96px;
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
          border-color: var(--openspecy-accent) !important;
          font-weight: 700;
        }
        label,
        .control-label,
        .radio label,
        .checkbox label { color: var(--openspecy-text) !important; }
        .form-control,
        .custom-select,
        .custom-file-label,
        .input-group-text,
        .selectize-input,
        .selectize-dropdown,
        .bootstrap-select > .dropdown-toggle,
        .bootstrap-select .dropdown-menu,
        .dropdown-menu {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .selectize-control.dropdown-active { z-index: 1100; }
        .selectize-dropdown { z-index: 1101 !important; }
        #choice_names { position: relative; z-index: 20; }
        .custom-file-label::after,
        .input-group-text {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .btn-default,
        .btn-secondary,
        .btn-file {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .btn-default:hover,
        .btn-default:focus,
        .btn-secondary:hover,
        .btn-secondary:focus,
        .btn-file:hover,
        .btn-file:focus {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-accent) !important;
        }
        .selectize-input input,
        .selectize-dropdown .option,
        .dropdown-item,
        .bootstrap-select .dropdown-item { color: var(--openspecy-text) !important; }
        .selectize-dropdown .active,
        .selectize-dropdown .selected,
        .dropdown-item:hover,
        .dropdown-item:focus,
        .bootstrap-select .dropdown-item.active {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
        }
        .form-control:focus,
        .custom-select:focus,
        .selectize-input.focus,
        .bootstrap-select > .dropdown-toggle:focus {
          border-color: var(--openspecy-accent) !important;
          box-shadow: 0 0 0 .16rem rgba(56, 189, 248, .2) !important;
        }
        .openspecy-manual-range.openspecy-inputs-disabled {
          opacity: .54;
        }
        .openspecy-manual-range.openspecy-inputs-disabled label {
          color: var(--openspecy-muted) !important;
        }
        .openspecy-manual-range.openspecy-inputs-disabled .form-control:disabled {
          color: var(--openspecy-muted) !important;
          background: var(--openspecy-panel) !important;
          cursor: not-allowed;
        }
        .irs--shiny .irs-bar,
        .irs--shiny .irs-single,
        .irs--shiny .irs-from,
        .irs--shiny .irs-to { background: var(--openspecy-accent) !important; }
        .irs--shiny .irs-line {
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .irs--shiny .irs-min,
        .irs--shiny .irs-max {
          color: var(--openspecy-muted) !important;
          background: var(--openspecy-panel-2) !important;
        }
        .irs--shiny .irs-grid-text { color: var(--openspecy-muted) !important; }
        .irs--shiny .irs-handle { border-color: var(--openspecy-accent) !important; }
        .pretty.p-switch .state:before { background: var(--openspecy-panel-2) !important; }
        .pretty.p-switch input:checked ~ .state:before {
          background: var(--openspecy-success) !important;
          border-color: var(--openspecy-success) !important;
        }
        .pretty input:checked ~ .state.p-success label::after,
        .pretty.p-switch input:checked ~ .state.p-success label::after {
          background: #FFFFFF !important;
        }
        #spectra_box .direct-chat-contacts {
          z-index: 40;
          overflow-y: auto;
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-left: 1px solid var(--openspecy-border);
          box-shadow: -12px 0 28px rgba(0, 0, 0, .38);
        }
        #spectra_box .direct-chat-contacts .contacts-list,
        #spectra_box .direct-chat-contacts .contacts-list > li,
        #spectra_box .direct-chat-contacts .tabbable,
        #spectra_box .direct-chat-contacts .tab-content,
        #spectra_box .direct-chat-contacts .tab-pane {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
        }
        #spectra_box .direct-chat-contacts .contacts-list {
          padding: 0;
          margin: 0;
        }
        #spectra_box .direct-chat-contacts .contacts-list > li {
          padding: 0 !important;
          border-bottom: 0 !important;
        }
        #sidebar_tables {
          padding: 10px 10px 0;
          margin: 0;
          background: var(--openspecy-panel-2) !important;
          border-bottom: 1px solid var(--openspecy-grid) !important;
        }
        #sidebar_tables > li > a {
          display: block;
          padding: .6rem .85rem;
          color: var(--openspecy-muted) !important;
          background: var(--openspecy-panel) !important;
          border: 1px solid transparent !important;
          border-radius: 6px 6px 0 0;
        }
        #sidebar_tables > li > a:hover,
        #sidebar_tables > li > a:focus {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        #sidebar_tables > li.active > a,
        #sidebar_tables > li > a.active {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-accent) !important;
          border-bottom-color: var(--openspecy-panel-2) !important;
        }
        #spectra_box .direct-chat-contacts .tab-content {
          min-height: 100%;
          padding: 10px;
        }
        #spectra_box #mycardsidebar {
          color: var(--openspecy-accent) !important;
          background: var(--openspecy-panel) !important;
          border: 1px solid var(--openspecy-grid) !important;
          border-radius: 6px;
        }
        #spectra_box #mycardsidebar:hover,
        #spectra_box #mycardsidebar:focus,
        #spectra_box.direct-chat-contacts-open #mycardsidebar {
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
          border-color: var(--openspecy-accent) !important;
          box-shadow: 0 0 0 .16rem rgba(56, 189, 248, .2);
        }
        #spectra_box.direct-chat-contacts-open > .card-header {
          border-bottom-color: var(--openspecy-accent) !important;
        }
        #spectra_box.direct-chat-contacts-open #choice_names {
          z-index: 0;
          pointer-events: none;
        }
        #spectra_box .direct-chat-contacts .close,
        #spectra_box .direct-chat-contacts [data-dismiss] {
          color: var(--openspecy-accent) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .dataTables_wrapper,
        .dataTables_wrapper label,
        .dataTables_wrapper .dataTables_info,
        table.dataTable,
        table.dataTable caption,
        .table { color: var(--openspecy-text) !important; }
        table.dataTable,
        .table {
          width: 100% !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-grid) !important;
        }
        table.dataTable thead th,
        table.dataTable thead td,
        .table thead th {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-border) !important;
        }
        table.dataTable tbody tr,
        table.dataTable tbody td,
        .table tbody tr,
        .table tbody td {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-grid) !important;
        }
        table.dataTable tbody tr:hover td,
        table.dataTable tbody tr.selected td,
        .table-hover tbody tr:hover td { background: var(--openspecy-panel-2) !important; }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel-2) !important;
          border: 1px solid var(--openspecy-grid) !important;
        }
        .dataTables_wrapper .paginate_button,
        .pagination .page-link {
          color: var(--openspecy-accent) !important;
          background: var(--openspecy-panel-2) !important;
          border-color: var(--openspecy-grid) !important;
        }
        .dataTables_wrapper .paginate_button.current,
        .dataTables_wrapper .paginate_button:hover,
        .pagination .page-item.active .page-link {
          color: var(--openspecy-text) !important;
          background: var(--openspecy-panel) !important;
          border-color: var(--openspecy-accent) !important;
        }
        #spectra_box,
        #analysis_summary_box { width: 100%; }
        #progress_bars > .col-sm-12 {
          padding-right: 0;
          padding-left: 0;
        }
        #analysis_summary_box .progress {
          background: var(--openspecy-canvas) !important;
          border: 1px solid var(--openspecy-grid);
        }
        #analysis_summary_box .progress-bar {
          color: var(--openspecy-canvas) !important;
          background: var(--openspecy-accent) !important;
        }
        .openspecy-summary-grid {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
          gap: 14px;
          margin: 0;
        }
        .openspecy-summary-grid > .openspecy-summary-panel {
          flex: 1 1 240px;
          width: auto;
          max-width: none;
          min-width: 0;
          padding: 0;
        }
        .openspecy-summary-panel {
          padding: 12px !important;
          border: 1px solid var(--openspecy-grid);
          border-radius: 8px;
          background: var(--openspecy-panel-2);
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
        .openspecy-heatmap-popover {
          position: absolute;
          top: 16px;
          right: 16px;
          z-index: 20;
          max-width: 220px;
          padding: 8px 10px;
          border: 1px solid var(--openspecy-border);
          border-radius: 8px;
          background: var(--openspecy-panel-2);
          box-shadow: 0 8px 20px rgba(0, 0, 0, .35);
          font-size: 12px;
          line-height: 1.5;
          pointer-events: none;
        }
        .openspecy-heatmap-popover table { width: 100%; }
        .openspecy-heatmap-popover th {
          text-align: left;
          padding-right: 8px;
          font-weight: 600;
          opacity: .75;
        }
        .openspecy-heatmap-popover td { text-align: right; }
        #filespec_source_box { margin-bottom: 18px; }
        .openspecy-filespec-toolbar {
          display: flex;
          flex-wrap: wrap;
          gap: 8px;
          align-items: center;
          margin: 8px 0;
        }
        .openspecy-filespec-toolbar .btn { min-width: 42px; }
        .openspecy-filespec-intro,
        .openspecy-filespec-status {
          color: var(--openspecy-muted);
          line-height: 1.45;
        }
        .openspecy-filespec-status {
          padding: 8px 10px;
          border-left: 3px solid var(--openspecy-accent);
          background: var(--openspecy-canvas);
        }
        .openspecy-filespec-map {
          min-height: 320px;
          padding: 8px;
          border: 1px solid var(--openspecy-grid);
          border-radius: 7px;
          background: var(--openspecy-canvas);
        }
        .shiny-output-error-validation {
          color: var(--openspecy-accent);
          font-size: 130%;
        }
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
        #openspecy_busy_elapsed {
          margin: 4px 0 10px;
          color: var(--openspecy-text);
        }
        .openspecy-progress-track {
          height: 10px;
          overflow: hidden;
          border: 1px solid var(--openspecy-border);
          border-radius: 999px;
          background: var(--openspecy-canvas);
        }
        #openspecy_busy_progress_fill {
          width: 4%;
          height: 100%;
          border-radius: inherit;
          background: linear-gradient(
            90deg,
            var(--openspecy-accent),
            var(--openspecy-axis)
          );
          transition: width .35s ease;
        }
        @keyframes openspecy-spin { to { transform: rotate(360deg); } }
        @media (prefers-reduced-motion: reduce) {
          .openspecy-busy-spinner { animation: none; }
          #openspecy_busy_progress_fill { transition: none; }
        }
        @media (max-width: 991px) {
          .openspecy-tab-scroll { max-height: none; }
          .openspecy-section-note,
          .openspecy-section-switch-with-note {
            align-items: flex-start;
            flex-direction: column;
          }
          .openspecy-section-description {
            max-width: none;
            text-align: left;
          }
          .openspecy-upload-column { margin-bottom: 8px; }
          .openspecy-support-button { max-width: 260px; overflow: hidden; text-overflow: ellipsis; }
        }
        @media (max-width: 575px) {
          .openspecy-summary-grid > .openspecy-summary-panel { flex-basis: 100%; }
          .openspecy-quality-controls { grid-template-columns: 1fr; }
          .main-footer { text-align: left; }
          .openspecy-support-button { max-width: 52px; }
        }
      ")))
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
          class = "openspecy-upload-column",
          fileInput(
            "file", NULL, multiple = TRUE,
            placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, ...",
            accept = c(
              "text/csv", "text/comma-separated-values,text/plain", ".csv",
              ".asp", ".tsv", ".spc", ".jdx", ".dx", ".RData", ".spa",
              ".0", ".zip", ".img", ".h5", ".txt", ".json", ".rds",
              ".hdr", ".dat"
            )
          )
        ),
        column(
          5,
          bs4Dash::tabBox(
            id = "analysis_settings",
            selected = "preprocessing",
            width = 12,
            title = NULL,
            collapsible = TRUE,
            collapsed = TRUE,
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
            ),
            tabPanel(
              "Quantification",
              value = "quantification",
              div(class = "openspecy-tab-scroll", quantification_controls)
            )
          )
        ),
        column(
          5,
          bs4Dash::box(
            id = "download_panel_box",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = shiny::downloadButton(
              "download_data",
              tags$span(
                class = "openspecy-download-label",
                "Download Test Data"
              ),
              class = "openspecy-download-button",
              title = "Download the selected test data or current analysis result"
            ),
            div(
              class = "openspecy-download-body",
              uiOutput("download_ui"),
              uiOutput("top_n"),
              uiOutput("particle_download_contents")
            )
          )
        )
      ),
      fluidRow(
        bs4Dash::box(
          id = "spectra_box",
          title = "Spectra",
          maximizable = TRUE,
          width = 12,
          h4(id = "placeholder1", "Upload some data to get started..."),
          uiOutput("choice_names"),
          fluidRow(
            column(
              11,
              div(
                id = "heatmap_frame",
                class = "openspecy-plot-frame",
                style = "display:none; position: relative;",
                plotOutput(
                  "heatmapA", height = "48vh",
                  click = clickOpts(id = "heatmap_click", clip = TRUE),
                  brush = brushOpts(
                    id = "heatmap_brush", direction = "xy",
                    resetOnNew = TRUE
                  )
                ),
                plotly::plotlyOutput("heatmapB", height = "48vh"),
                uiOutput("heatmap_popover")
              )
            ),
            column(1, uiOutput("nav_buttons"))
          ),
          div(
            class = "openspecy-quality-controls",
            role = "group",
            `aria-label` = "Automatic corrections and spectral quality checks",
            actionButton(
              "quality_automatic_details",
              tagList(
                icon(
                  "magic",
                  class = paste(
                    "openspecy-quality-icon",
                    "openspecy-quality-icon-automatic"
                  ),
                  `aria-hidden` = "true"
                ),
                textOutput("quality_automatic_count", inline = TRUE),
                tags$span("Automatic Corrections Made")
              ),
              class = paste(
                "openspecy-quality-button openspecy-quality-automatic"
              ),
              title = "Open automatic correction details"
            ),
            actionButton(
              "quality_warning_details",
              tagList(
                icon(
                  "exclamation-triangle",
                  class = paste(
                    "openspecy-quality-icon",
                    "openspecy-quality-icon-warning"
                  ),
                  `aria-hidden` = "true"
                ),
                textOutput("quality_warning_count", inline = TRUE),
                tags$span("Warnings")
              ),
              class = paste(
                "openspecy-quality-button openspecy-quality-warning"
              ),
              title = "Open warning findings for the active spectrum"
            ),
            actionButton(
              "quality_success_details",
              tagList(
                icon(
                  "check-circle",
                  class = paste(
                    "openspecy-quality-icon",
                    "openspecy-quality-icon-success"
                  ),
                  `aria-hidden` = "true"
                ),
                textOutput("quality_success_count", inline = TRUE),
                tags$span("Successes")
              ),
              class = "openspecy-quality-button openspecy-quality-success",
              title = "Open successful checks for the active spectrum"
            )
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
      fluidRow(
        column(
          12,
          class = "openspecy-summary-column",
          uiOutput("progress_bars")
        )
      )
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
