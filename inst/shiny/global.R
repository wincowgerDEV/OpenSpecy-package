if (file.exists("wasm-config.R")) {
  source("wasm-config.R", local = TRUE)
}

app_wasm_mode <- function() {
  env <- tolower(Sys.getenv("OPENSPECY_SHINY_WASM", ""))
  isTRUE(getOption("openspecy.shiny.wasm", FALSE)) ||
    env %in% c("1", "true", "yes", "on")
}

validate_wasm_package_version <- function() {
  if (!app_wasm_mode()) return(invisible(TRUE))

  expected <- getOption("openspecy.shiny.wasm.package_version", "")
  actual <- as.character(utils::packageVersion("OpenSpecy"))
  if (!nzchar(expected) || !identical(actual, expected)) {
    commit <- getOption("openspecy.shiny.wasm.package_sha", "unknown")
    stop(
      "The WebAssembly app loaded OpenSpecy ", actual,
      " but its pinned build requires ", expected,
      " from commit ", commit, ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#remotes::install_github("wincowgerDEV/OpenSpecy-package@vignettes")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(digest)
#library(curl)
#library(loggit)
library(bs4Dash)
library(ggplot2)
library(reshape2)

library(OpenSpecy)
validate_wasm_package_version()
#library(glmnet)

app_download_choices <- function(has_upload, identification,
                                 collapse = FALSE) {
  tests <- c("Test Data", "Test Map")
  metadata <- "User Metadata"
  if (!isTRUE(has_upload)) return(c(tests, metadata))

  choices <- if (isTRUE(identification)) {
    c("Top Matches", "Processed Spectra")
  } else {
    "Processed Spectra"
  }
  if (isTRUE(collapse)) choices <- c(choices, "Thresholded Particles")
  c(choices, tests, metadata)
}

app_download_label <- function(selection) {
  labels <- c(
    "Test Data" = "Download Test Data",
    "Test Map" = "Download Test Map",
    "Processed Spectra" = "Download Processed Spectra",
    "Top Matches" = "Download Top Matches",
    "Thresholded Particles" = "Download Thresholded Particles",
    "User Metadata" = "Download User Metadata"
  )
  if(length(selection) != 1L || is.na(selection) ||
     !selection %in% names(labels)) {
    return("Download selected")
  }
  unname(labels[[selection]])
}

app_empty_ratio_definitions <- function() {
  data.frame(
    id = integer(),
    name = character(),
    column = character(),
    type = character(),
    numerator_min = numeric(),
    numerator_max = numeric(),
    denominator_min = numeric(),
    denominator_max = numeric(),
    stringsAsFactors = FALSE
  )
}

# Input IDs are kept as the exported column names so the settings snapshot is
# readable beside the app source without promising a future import contract.
app_user_metadata_input_ids <- c(
  # Preprocessing
  "active_preprocessing", "make_rel_decision", "smooth_decision",
  "smoother", "derivative_order", "smoother_window", "derivative_abs",
  "conform_decision", "conform_selection", "conform_res",
  "intensity_decision", "intensity_corr", "baseline_decision",
  "baseline_method", "baseline", "refit", "baseline_lambda",
  "baseline_hwi", "iterations", "range_decision", "range_automate",
  "range_artifact_ratio", "MinRange", "MaxRange", "co2_decision",
  "co2_automate", "co2_artifact_ratio", "MinFlat", "MaxFlat",
  # Identification
  "active_identification", "id_spec_type", "id_strategy", "lib_type",
  "filter_lib", "lib_org",
  # Advanced
  "threshold_decision", "MinSNR", "signal_selection",
  "cor_threshold_decision", "MinCor", "spatial_decision", "sigma",
  "xy_grid", "collapse_decision", "collapse_type", "collapse_log_type",
  # Quantification builder
  "active_quantification", "quant_ratio_name", "quant_ratio_type",
  "quant_numerator_area", "quant_denominator_area",
  "quant_numerator_peak", "quant_denominator_peak"
)

app_metadata_scalar <- function(value, separator = " | ") {
  if(is.null(value) || !length(value)) return(NA_character_)
  if(inherits(value, "POSIXt")) {
    value <- format(value, "%Y-%m-%d %H:%M:%S %z")
  }
  if(is.factor(value)) value <- as.character(value)
  if(is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if(!length(value)) return(NA_character_)
  if(length(value) == 1L && is.atomic(value)) return(value[[1L]])
  paste(as.character(value), collapse = separator)
}

app_saved_ratio_definitions <- function(definitions) {
  if(is.null(definitions) || !is.data.frame(definitions) ||
     !nrow(definitions)) {
    return(NA_character_)
  }
  required <- names(app_empty_ratio_definitions())
  if(!all(required %in% names(definitions))) {
    stop("Saved ratio definitions have an unexpected structure.",
         call. = FALSE)
  }
  paste(vapply(seq_len(nrow(definitions)), function(i) {
    definition <- definitions[i, required, drop = FALSE]
    paste(
      paste0("id=", definition$id[[1L]]),
      paste0("name=", definition$name[[1L]]),
      paste0("column=", definition$column[[1L]]),
      paste0("type=", definition$type[[1L]]),
      paste0("numerator_min=", definition$numerator_min[[1L]]),
      paste0("numerator_max=", definition$numerator_max[[1L]]),
      paste0("denominator_min=", definition$denominator_min[[1L]]),
      paste0("denominator_max=", definition$denominator_max[[1L]]),
      sep = "; "
    )
  }, character(1)), collapse = " || ")
}

app_user_metadata_snapshot <- function(settings, definitions, recorded_at,
                                       app_version, session_id,
                                       source = NULL, file_info = NULL) {
  if(!is.list(settings)) {
    stop("App settings must be supplied as a named list.", call. = FALSE)
  }

  uploaded <- !is.null(source)
  spectra_count <- if(uploaded) ncol(source$spectra) else NA_integer_
  wavenumber_count <- if(uploaded) length(source$wavenumber) else NA_integer_
  wavenumber_min <- if(uploaded && wavenumber_count) {
    min(source$wavenumber, na.rm = TRUE)
  } else NA_real_
  wavenumber_max <- if(uploaded && wavenumber_count) {
    max(source$wavenumber, na.rm = TRUE)
  } else NA_real_
  data_digest <- if(uploaded) {
    digest::digest(source, algo = "md5")
  } else NA_character_

  file_value <- function(name) {
    if(is.null(file_info) || !is.data.frame(file_info) ||
       !name %in% names(file_info)) return(NA_character_)
    app_metadata_scalar(file_info[[name]])
  }
  settings <- stats::setNames(lapply(app_user_metadata_input_ids, function(id) {
    app_metadata_scalar(settings[[id]])
  }), app_user_metadata_input_ids)

  snapshot <- c(
    list(
      recorded_at = app_metadata_scalar(recorded_at),
      app_version = app_metadata_scalar(app_version),
      session_id = app_metadata_scalar(session_id),
      data_uploaded = uploaded,
      data_file_name = file_value("name"),
      data_file_size_bytes = file_value("size"),
      data_file_type = file_value("type"),
      data_file_last_modified = file_value("lastModified"),
      data_digest_md5 = data_digest,
      data_spectrum_count = spectra_count,
      data_wavenumber_count = wavenumber_count,
      data_wavenumber_min = wavenumber_min,
      data_wavenumber_max = wavenumber_max
    ),
    settings,
    list(
      quant_saved_ratio_count = if(is.data.frame(definitions)) {
        nrow(definitions)
      } else 0L,
      quant_saved_ratio_definitions = app_saved_ratio_definitions(definitions)
    )
  )

  snapshot <- lapply(snapshot, app_metadata_scalar)
  if(any(lengths(snapshot) != 1L)) {
    stop("Every user metadata field must contain exactly one value.",
         call. = FALSE)
  }
  snapshot
}

app_quantification_source_value <- "displayed_processed_spectra"

app_ratio_column_name <- function(name, type) {
  if(!is.character(name) || length(name) != 1L || is.na(name) ||
     !nzchar(trimws(name))) {
    stop("Enter a nonempty ratio name before adding it.", call. = FALSE)
  }
  type <- match.arg(type, c("area", "peak"))
  plain <- iconv(trimws(name), to = "ASCII//TRANSLIT", sub = "")
  slug <- tolower(gsub("[^A-Za-z0-9]+", "_", plain))
  slug <- gsub("^_+|_+$", "", slug)
  if(is.na(slug) || !nzchar(slug)) {
    stop("The ratio name must contain at least one letter or number.",
         call. = FALSE)
  }
  paste0(type, "_ratio_", slug)
}

app_add_ratio_definition <- function(definitions, name, type, numerator,
                                     denominator, axis) {
  expected <- names(app_empty_ratio_definitions())
  if(!is.data.frame(definitions) || !identical(names(definitions), expected)) {
    stop("Ratio definitions have an unexpected structure.", call. = FALSE)
  }
  type <- match.arg(type, c("area", "peak"))
  name <- trimws(name)
  column <- app_ratio_column_name(name, type)
  if(column %in% definitions$column) {
    stop("A ratio with the same metadata name has already been added.",
         call. = FALSE)
  }

  axis <- sort(unique(as.numeric(axis)))
  axis <- axis[is.finite(axis)]
  if(!length(axis)) {
    stop("Upload and process a valid spectrum before adding ratios.",
         call. = FALSE)
  }
  normalize_selection <- function(value, expected_length, label) {
    if(!is.numeric(value) || length(value) != expected_length ||
       any(!is.finite(value))) {
      stop(label, " must contain ", expected_length,
           " finite wavenumber value", if(expected_length == 1L) "." else "s.",
           call. = FALSE)
    }
    sort(as.numeric(value))
  }

  if(identical(type, "area")) {
    numerator <- normalize_selection(numerator, 2L, "Numerator range")
    denominator <- normalize_selection(denominator, 2L, "Denominator range")
  } else {
    numerator <- rep(normalize_selection(numerator, 1L, "Numerator point"), 2L)
    denominator <- rep(normalize_selection(
      denominator, 1L, "Denominator point"
    ), 2L)
  }

  values <- c(numerator, denominator)
  if(any(values < axis[[1L]] | values > axis[[length(axis)]])) {
    stop(
      "Ratio selections must stay within the displayed processed wavenumber range.",
      call. = FALSE
    )
  }
  if(identical(type, "area") &&
     (!any(axis >= numerator[[1L]] & axis <= numerator[[2L]]) ||
      !any(axis >= denominator[[1L]] & axis <= denominator[[2L]]))) {
    stop(
      "Each area range must contain at least one displayed processed wavenumber.",
      call. = FALSE
    )
  }

  next_id <- if(nrow(definitions)) max(definitions$id) + 1L else 1L
  rbind(
    definitions,
    data.frame(
      id = next_id,
      name = name,
      column = column,
      type = type,
      numerator_min = numerator[[1L]],
      numerator_max = numerator[[2L]],
      denominator_min = denominator[[1L]],
      denominator_max = denominator[[2L]],
      stringsAsFactors = FALSE
    )
  )
}

app_ratio_definition_label <- function(definition) {
  if(identical(definition$type[[1L]], "area")) {
    paste0(
      definition$name[[1L]], " (area: ",
      format(definition$numerator_min[[1L]]), "-",
      format(definition$numerator_max[[1L]]), " / ",
      format(definition$denominator_min[[1L]]), "-",
      format(definition$denominator_max[[1L]]), " cm^-1)"
    )
  } else {
    paste0(
      definition$name[[1L]], " (peak: ",
      format(definition$numerator_min[[1L]]), " / ",
      format(definition$denominator_min[[1L]]), " cm^-1)"
    )
  }
}

app_ratio_slider_defaults <- function(axis, type = c("area", "peak")) {
  type <- match.arg(type)
  axis <- sort(unique(as.numeric(axis)))
  axis <- axis[is.finite(axis)]
  if(length(axis) < 2L) {
    stop("Process a spectrum with at least two distinct wavenumbers.",
         call. = FALSE)
  }

  axis_min <- as.integer(ceiling(min(axis)))
  axis_max <- as.integer(floor(max(axis)))
  if(axis_min >= axis_max) {
    stop("The processed wavenumber range must contain at least two integers.",
         call. = FALSE)
  }
  clamp_integer <- function(value) {
    as.integer(pmax(axis_min, pmin(axis_max, round(value))))
  }
  closest_integer <- function(value) {
    clamp_integer(axis[[which.min(abs(axis - value))]])
  }
  # Ratios may use any whole-number boundary or point within the processed
  # axis; peak lookup resolves a selected point to measured data later.
  step <- 1L

  if(identical(type, "area")) {
    scenario <- c(1650, 1850, 1420, 1500)
    if(all(scenario >= axis_min & scenario <= axis_max)) {
      numerator <- sort(vapply(scenario[1:2], closest_integer, integer(1)))
      denominator <- sort(vapply(
        scenario[3:4], closest_integer, integer(1)
      ))
    } else {
      selections <- axis[pmax(
        1L,
        pmin(length(axis), round(c(.60, .78, .24, .42) * length(axis)))
      )]
      numerator <- sort(clamp_integer(selections[1:2]))
      denominator <- sort(clamp_integer(selections[3:4]))
    }
  } else {
    scenario <- c(1715, 1460)
    if(all(scenario >= axis_min & scenario <= axis_max)) {
      numerator <- closest_integer(scenario[[1L]])
      denominator <- closest_integer(scenario[[2L]])
    } else {
      numerator <- clamp_integer(
        axis[[max(1L, round(.67 * length(axis)))]]
      )
      denominator <- clamp_integer(
        axis[[max(1L, round(.33 * length(axis)))]]
      )
    }
  }

  list(
    min = axis_min,
    max = axis_max,
    step = step,
    numerator = numerator,
    denominator = denominator
  )
}

app_ratio_definitions_text <- function(definitions) {
  if(!nrow(definitions)) return(character())
  paste(
    vapply(seq_len(nrow(definitions)), function(i) {
      app_ratio_definition_label(definitions[i, , drop = FALSE])
    }, character(1)),
    collapse = "; "
  )
}

app_ratio_metadata_columns <- function(definitions) {
  if(!nrow(definitions)) return(character())
  c("quantification_source", "quantification_definitions",
    definitions$column)
}

app_area_ratio <- function(source, numerator, denominator) {
  source <- as_OpenSpecy(source)
  axis <- source$wavenumber
  named_na <- stats::setNames(
    rep(NA_real_, ncol(source$spectra)), colnames(source$spectra)
  )
  complete <- all(c(numerator, denominator) >= min(axis) &
                    c(numerator, denominator) <= max(axis)) &&
    any(axis >= numerator[[1L]] & axis <= numerator[[2L]]) &&
    any(axis >= denominator[[1L]] & axis <= denominator[[2L]])
  if(!complete) {
    warning("The source spectrum does not fully cover this area ratio; returning NA.",
            call. = FALSE)
    return(named_na)
  }
  numerator_values <- area_under_band(
    source, min = numerator[[1L]], max = numerator[[2L]]
  )
  denominator_values <- area_under_band(
    source, min = denominator[[1L]], max = denominator[[2L]]
  )
  values <- numerator_values / denominator_values
  invalid <- !is.finite(numerator_values) | !is.finite(denominator_values) |
    denominator_values == 0 | !is.finite(values)
  if(any(invalid)) {
    warning("One or more area ratios had a zero or non-finite value; returning NA for those spectra.",
            call. = FALSE)
    values[invalid] <- NA_real_
  }
  values
}

app_attach_quantification <- function(x, definitions) {
  x <- as_OpenSpecy(x)
  if(!nrow(definitions)) return(x)

  x$metadata <- data.table::copy(x$metadata)
  x$metadata$quantification_source <- app_quantification_source_value
  x$metadata$quantification_definitions <-
    app_ratio_definitions_text(definitions)
  for(i in seq_len(nrow(definitions))) {
    definition <- definitions[i, , drop = FALSE]
    values <- if(identical(definition$type[[1L]], "area")) {
      app_area_ratio(
        x,
        c(definition$numerator_min[[1L]], definition$numerator_max[[1L]]),
        c(definition$denominator_min[[1L]], definition$denominator_max[[1L]])
      )
    } else {
      peak_ratio(
        x,
        numerator = definition$numerator_min[[1L]],
        denominator = definition$denominator_min[[1L]],
        method = "nearest"
      )
    }
    if(length(values) != nrow(x$metadata)) {
      stop("Quantification returned an unexpected number of values for '",
           definition$name[[1L]], "'.", call. = FALSE)
    }
    x$metadata[[definition$column[[1L]]]] <- as.numeric(values)
  }
  x
}

.app_range_assessment <- function(x, check, correction_args = list()) {
  check <- match.arg(check, c("co2_region", "high_tail"))
  value_or <- function(name, default) {
    value <- correction_args[[name]]
    if(is.null(value)) default else value
  }

  artifact_ratio <- value_or("artifact_ratio", 3)
  tail_n <- value_or("tail_n", 5L)
  co2_region <- if(identical(check, "co2_region")) {
    min <- value_or("min", 2200)
    max <- value_or("max", 2400)
    if(length(min) != 1L || length(max) != 1L) {
      stop("automatic CO2 correction requires one flattening range",
           call. = FALSE)
    }
    c(min, max)
  } else {
    value_or("co2_region", c(2200, 2420))
  }

  issues <- assess_spec(
    x,
    checks = check,
    artifact_ratio = artifact_ratio,
    tail_n = tail_n,
    co2_region = co2_region
  )
  failures <- length(unique(issues$spectrum_index))
  total <- ncol(x$spectra)

  list(
    issues = issues,
    failures = failures,
    passes = total - failures,
    total = total
  )
}

.app_range_candidate_preserves_batch <- function(before, candidate) {
  if(!inherits(candidate, "OpenSpecy") ||
     ncol(candidate$spectra) != ncol(before$spectra) ||
     !identical(colnames(candidate$spectra), colnames(before$spectra)) ||
     !identical(candidate$metadata, before$metadata)) {
    return(FALSE)
  }

  original_attributes <- attributes(before)
  protected <- setdiff(names(original_attributes), c("names", "class"))
  all(vapply(protected, function(name) {
    identical(attr(candidate, name), original_attributes[[name]])
  }, logical(1)))
}

.app_range_diagnostic <- function(step, check, enabled, attempted, accepted,
                                  total, before, after, reason,
                                  message = "") {
  data.frame(
    step = step,
    check = check,
    enabled = enabled,
    attempted = attempted,
    accepted = accepted,
    total_spectra = as.integer(total),
    before_passes = as.integer(before),
    after_passes = as.integer(after),
    reason = reason,
    message = message,
    stringsAsFactors = FALSE
  )
}

.app_attempt_range_automation <- function(x, step, correction_args = list()) {
  check <- if(identical(step, "flatten_range")) "co2_region" else "high_tail"
  before <- .app_range_assessment(x, check, correction_args)
  if(before$failures == 0L) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, FALSE, FALSE, before$total,
        before$passes, before$passes, "no_failures"
      )
    ))
  }

  correction <- if(identical(step, "flatten_range")) {
    flatten_range
  } else {
    restrict_range
  }
  correction_args$x <- NULL
  correction_args$automate <- TRUE
  if(is.null(correction_args$make_rel)) correction_args$make_rel <- FALSE
  candidate <- tryCatch(
    do.call(correction, c(list(x = x), correction_args)),
    error = function(e) e
  )
  if(inherits(candidate, "error")) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "correction_error",
        conditionMessage(candidate)
      )
    ))
  }
  if(!.app_range_candidate_preserves_batch(x, candidate)) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "invalid_candidate",
        "candidate changed spectrum identifiers, metadata, or input attributes"
      )
    ))
  }

  after <- tryCatch(
    .app_range_assessment(candidate, check, correction_args),
    error = function(e) e
  )
  if(inherits(after, "error")) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "assessment_error",
        conditionMessage(after)
      )
    ))
  }

  accepted <- after$passes > before$passes
  list(
    data = if(accepted) candidate else x,
    diagnostic = .app_range_diagnostic(
      step, check, TRUE, TRUE, accepted, before$total,
      before$passes, after$passes,
      if(accepted) "improved" else "not_improved"
    )
  )
}

app_apply_range_automation <- function(x, flatten = TRUE, restrict = TRUE,
                                       flatten_args = list(),
                                       restrict_args = list()) {
  if(!inherits(x, "OpenSpecy")) {
    stop("'x' must be an OpenSpecy object", call. = FALSE)
  }

  current <- x
  diagnostics <- list()
  steps <- list(
    list(name = "flatten_range", check = "co2_region", enabled = flatten,
         args = flatten_args),
    list(name = "restrict_range", check = "high_tail", enabled = restrict,
         args = restrict_args)
  )
  for(step in steps) {
    if(!isTRUE(step$enabled)) {
      total <- ncol(current$spectra)
      diagnostics[[length(diagnostics) + 1L]] <- .app_range_diagnostic(
        step$name, step$check, FALSE, FALSE, FALSE, total,
        NA_integer_, NA_integer_, "disabled"
      )
      next
    }

    result <- .app_attempt_range_automation(
      current, step$name, correction_args = step$args
    )
    current <- result$data
    diagnostics[[length(diagnostics) + 1L]] <- result$diagnostic
  }

  list(data = current, diagnostics = do.call(rbind, diagnostics))
}

app_theme <- list(
  canvas = "#050B14",
  panel = "#0B1929",
  panel_2 = "#10243A",
  border = "#168FC2",
  accent = "#38BDF8",
  success = "#22C55E",
  text = "#E6EDF7",
  muted = "#A9B8CB",
  grid = "#28536F",
  axis = "#6F86A3",
  raw = "#CBD5E1",
  reference = "#FB7185",
  spectrum = "#FFFFFF"
)

app_theme_css <- function(theme = app_theme) {
  required <- c(
    "canvas", "panel", "panel_2", "border", "accent", "success",
    "text", "muted", "grid", "axis", "raw", "spectrum"
  )
  if(!is.list(theme) || !all(required %in% names(theme))) {
    stop("The app theme is missing one or more required color tokens.",
         call. = FALSE)
  }

  values <- unlist(theme[required], use.names = TRUE)
  css_names <- gsub("_", "-", names(values), fixed = TRUE)
  paste0(
    ":root {\n",
    paste0("  --openspecy-", css_names, ": ", values, ";",
           collapse = "\n"),
    "\n}\n"
  )
}

app_plot_palette <- list(
  panel = app_theme$panel,
  grid = app_theme$grid,
  axis = app_theme$axis,
  text = app_theme$text,
  primary = app_theme$accent,
  raw = app_theme$raw,
  reference = app_theme$reference,
  spectrum = app_theme$spectrum
)

app_summary_row <- function(items) {
  if(!is.list(items)) {
    stop("Summary items must be supplied as a list.", call. = FALSE)
  }
  items <- Filter(function(item) !is.null(item), items)
  count <- length(items)
  if(count == 0L) return(NULL)

  widths <- rep.int(12L %/% count, count)
  remainder <- 12L %% count
  if(remainder > 0L) {
    widths[seq_len(remainder)] <- widths[seq_len(remainder)] + 1L
  }
  columns <- Map(
    function(item, width) {
      shiny::column(width, class = "openspecy-summary-panel", item)
    },
    items,
    widths
  )
  do.call(
    shiny::fluidRow,
    c(list(class = "openspecy-summary-grid"), unname(columns))
  )
}

app_style_plotly <- function(plot) {
  plotly::layout(
    plot,
    plot_bgcolor = app_plot_palette$panel,
    paper_bgcolor = app_plot_palette$panel,
    font = list(color = app_plot_palette$text),
    xaxis = list(
      gridcolor = app_plot_palette$grid,
      zerolinecolor = app_plot_palette$grid,
      linecolor = app_plot_palette$axis,
      tickcolor = app_plot_palette$axis
    ),
    yaxis = list(
      gridcolor = app_plot_palette$grid,
      zerolinecolor = app_plot_palette$grid,
      linecolor = app_plot_palette$axis,
      tickcolor = app_plot_palette$axis
    ),
    hoverlabel = list(
      bgcolor = app_theme$panel_2,
      bordercolor = app_plot_palette$axis,
      font = list(color = app_plot_palette$text)
    )
  )
}

app_spectrum_plot <- function(active, raw = NULL, reference = NULL,
                              make_rel = FALSE, source = "B") {
  prepare_trace <- function(x, normalize = FALSE) {
    if(is.null(x)) return(NULL)
    x <- as_OpenSpecy(x)
    if(isTRUE(normalize)) x <- OpenSpecy::make_rel(x, na.rm = TRUE)
    if(ncol(x$spectra) < 1L) return(NULL)
    data.frame(
      wavenumber = x$wavenumber,
      intensity = as.numeric(as.matrix(x$spectra)[, 1L])
    )
  }
  add_spectrum <- function(plot, values, name, color, width, dash = "solid") {
    if(is.null(values)) return(plot)
    plotly::add_trace(
      plot,
      data = values,
      x = ~wavenumber,
      y = ~intensity,
      type = "scatter",
      mode = "lines",
      name = name,
      legendgroup = name,
      showlegend = TRUE,
      line = list(color = color, width = width, dash = dash),
      hovertemplate = paste0(
        name, "<br>%{x:.1f} cm<sup>-1</sup><br>",
        "%{y:.4g}<extra></extra>"
      ),
      inherit = FALSE
    )
  }

  plot <- plotly::plot_ly(source = source)
  plot <- add_spectrum(
    plot, prepare_trace(raw, normalize = make_rel), "Raw spectrum",
    "rgba(203, 213, 225, 0.24)", 1.2
  )
  plot <- add_spectrum(
    # Keep the active trace byte-for-byte on the final DataR() scale so that
    # displayed values and quantification use exactly the same processed data.
    plot, prepare_trace(active), "Active spectrum",
    app_plot_palette$spectrum, 2.4
  )
  plot <- add_spectrum(
    plot, prepare_trace(reference, normalize = make_rel),
    "Identification match",
    app_plot_palette$reference, 2.2, "dot"
  )
  plotly::layout(
    plot,
    xaxis = list(
      title = "wavenumber [cm<sup>-1</sup>]",
      autorange = "reversed"
    ),
    yaxis = list(title = "intensity [-]"),
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.03,
      bgcolor = "rgba(11, 25, 41, 0.82)",
      bordercolor = app_plot_palette$grid,
      borderwidth = 1,
      font = list(color = app_plot_palette$text)
    ),
    margin = list(t = 58)
  )
}

app_empty_spectrum_plot <- function() {
  plotly::plot_ly(type = "scatter", mode = "lines") |>
    plotly::layout(
      xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                   range = c(4000, 400)),
      yaxis = list(title = "intensity [-]", range = c(0, 1))
    ) |>
    app_style_plotly()
}

# App metadata ----
metadata_file <- ".openspecy-shiny-metadata.rds"

read_app_metadata <- function(path = metadata_file) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(...) NULL)
}

build_version_display <- function(metadata) {
  default_href <- "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/openspecy?tab=readme-ov-file#version-history"
  default_text <- paste0("Last Updated: ", format(Sys.Date()))
  default_title <- "Click here to view older versions of this app"

  if (is.null(metadata)) {
    return(list(text = default_text, href = default_href, title = default_title))
  }

  commit <- metadata$commit
  ref <- metadata$ref
  owner <- metadata$owner
  repo <- metadata$repo

  downloaded_time <- metadata$downloaded_at

  text <- paste0("App metadata date: ", downloaded_time)
  commit_display <- NULL
  if (!is.null(commit)) {
    commit_display <- substr(commit, 1, min(nchar(commit), 7))
    text <- paste0(text, " • Commit ", commit_display)
  }

  href <- default_href
  if (!is.null(owner) && !is.null(repo)) {
    href <- sprintf("https://github.com/%s/%s/commits", owner, repo)
    if (!is.null(ref)) {
      href <- sprintf("%s/%s", href, utils::URLencode(ref, reserved = TRUE))
    }
  }

  title <- default_title
  if (!is.null(downloaded_time) || !is.null(commit)) {
    parts <- c()
    if (!is.null(downloaded_time)) {
      parts <- c(parts, paste0("App metadata date ", downloaded_time))
    }
    if (!is.null(commit)) {
      parts <- c(parts, paste0("Commit ", commit))
    }
    if (length(parts)) {
      title <- paste(parts, collapse = " — ")
    }
  }

  list(text = text, href = href, title = title)
}

app_metadata <- read_app_metadata()
app_version_display <- build_version_display(app_metadata)

# The app now ships inside the OpenSpecy package. Override the historical
# remote-download metadata with package release metadata.
build_version_display <- function() {
  package_version <- tryCatch(
    as.character(utils::packageVersion("OpenSpecy")),
    error = function(...) "development"
  )

  list(
    text = paste0("OpenSpecy ", package_version),
    href = "https://github.com/wincowgerDEV/OpenSpecy-package/releases",
    title = "Click here to view OpenSpecy package releases"
  )
}

app_version_display <- build_version_display()

app_library_dir <- function() {
  configured <- Sys.getenv("OPENSPECY_SHINY_LIBRARY_PATH", "")
  if (!nzchar(configured)) {
    configured <- shiny::getShinyOption("library_path", default = "")
  }

  dir <- if (nzchar(configured) && !identical(configured, "system")) {
    configured
  } else {
    file.path(tools::R_user_dir("OpenSpecy", "cache"),
              "reference_libraries")
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

app_library_revisions <- c(
  medoid_derivative = "iThmNyMeUKhkWMvbBxQqpf1sESdQBFTs",
  medoid_nobaseline = "CLJCDpeFCMZw4hFUW4Y1QFT2cj23W1Yz",
  model_derivative = "Wk7H.Zjj4coxiMGlqQlXjV5smmZou.IH",
  model_nobaseline = "rtJY7zQTDzRISfGpvYrU0bcj8nnRYs26",
  nobaseline = "XHh26IfFkVgU6O011uKpGeXGoPNsB0_t",
  derivative = "k9DA01hqGk0dNudCu3ddhwQX.whPGrsp"
)

app_wasm_library_types <- function() {
  configured <- getOption("openspecy.shiny.wasm.libraries", character())
  if (length(configured)) return(configured)
  c("medoid_derivative", "medoid_nobaseline",
    "model_derivative", "model_nobaseline")
}

app_library_type_choices <- function() {
  if (app_wasm_mode()) {
    return(c("Medoid" = "medoid", "Multinomial" = "model"))
  }

  c("Full" = "full", "Medoid" = "medoid", "Multinomial" = "model")
}

app_validate_library_type <- function(type) {
  if (app_wasm_mode() && !type %in% app_wasm_library_types()) {
    stop(
      "The WebAssembly app only includes medoid and model libraries. ",
      "Requested unsupported library: ", type,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_app_library <- function(type) {
  app_validate_library_type(type)

  installed_library <- tryCatch(
    load_lib(type),
    error = function(e) e
  )

  if (!inherits(installed_library, "error")) {
    return(installed_library)
  }

  library_path <- app_library_dir()
  cached_library <- tryCatch(
    load_lib(type, path = library_path),
    error = function(e) e
  )

  if (!inherits(cached_library, "error")) {
    return(cached_library)
  }

  download_result <- tryCatch(
    get_lib(
      type,
      path = library_path,
      revision = unname(app_library_revisions[[type]]),
      aws = TRUE
    ),
    error = function(e) e,
    warning = function(w) w
  )

  if (inherits(download_result, c("error", "warning"))) {
    stop(
      "Unable to load the Open Specy reference library '", type,
      "' from the installed package or app cache, and downloading it failed: ",
      conditionMessage(download_result),
      ". Run get_lib(\"", type, "\") before run_app(), or check your ",
      "network connection.",
      call. = FALSE
    )
  }

  load_lib(type, path = library_path)
}

# Helper to create collapsible footnotes ----
footnote <- function(summary, ...) {
  content <- list(...)
  has_content <- length(content) && any(vapply(content, function(item) {
    text <- trimws(gsub("<[^>]+>", "", paste(as.character(item),
                                               collapse = " ")))
    nzchar(text)
  }, logical(1)))
  if(!has_content) {
    stop("Information disclosures require substantive details.",
         call. = FALSE)
  }

  tags$details(
    class = "openspecy-info-details",
    tags$summary(summary),
    tags$div(
      class = "openspecy-info-details-body",
      lapply(content, tags$p)
    )
  )
}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")

  intensity <- if(is.data.frame(raman_hdpe$spectra)) {
    raman_hdpe$spectra$intensity
  } else {
    as.numeric(raman_hdpe$spectra[, 1])
  }

  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                 intensity = intensity)

  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}

# Name keys for human readable column names ----

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))
citation <- HTML(
  'Cowger, W., Karapetrova, A., Lincoln, C., Chamas, A., Sherrod, H., Leong, N., Lasdin, K. S., 
  Knauss, C., Teofilović, V., Arienzo, M. M., Steinmetz, Z., Primpke, S., 
  Darjany, L., Murphy-Hagan, C., Moore, S., Moore, C., Lattin, G., 
  Gray, A., Kozloski, R., Bryksa, J., Maurer, B. (2025). 
  "Open Specy 1.0: Automated (Hyper)spectroscopy for Microplastics." 
  <i>Analytical Chemistry.</i> doi:
  <a href="https://doi.org/10.1021/acs.analchem.5c00962">10.1021/acs.analchem.5c00962</a>.'
)


# Define the custom theme
theme_black_minimal <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = app_plot_palette$panel,
                                     color = app_plot_palette$axis,
                                     linewidth = 0.6),
      panel.background = element_rect(fill = app_plot_palette$panel,
                                      color = NA),
      panel.border = element_rect(fill = NA, color = app_plot_palette$axis,
                                  linewidth = 0.5),
      panel.grid.major = element_line(color = app_plot_palette$grid,
                                      linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = app_plot_palette$axis),
      axis.ticks = element_line(color = app_plot_palette$axis),
      axis.text = element_text(color = app_plot_palette$text),
      axis.title = element_text(color = app_plot_palette$text),
      plot.title = element_text(color = app_plot_palette$text, hjust = 0.5),
      plot.subtitle = element_text(color = app_plot_palette$text, hjust = 0.5),
      plot.caption = element_text(color = app_plot_palette$text),
      legend.text = element_text(color = app_plot_palette$text),
      legend.title = element_text(color = app_plot_palette$text),
      legend.background = element_rect(fill = app_plot_palette$panel,
                                       color = NA),
      legend.key = element_rect(fill = app_plot_palette$panel, color = NA),
      strip.background = element_rect(fill = app_theme$panel_2,
                                      color = app_plot_palette$axis),
      strip.text = element_text(color = app_plot_palette$text)
    )
}
