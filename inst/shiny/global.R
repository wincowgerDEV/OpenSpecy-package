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
  if (!isTRUE(has_upload)) return(tests)

  choices <- if (isTRUE(identification)) {
    c("Top Matches", "Processed Spectra")
  } else {
    "Processed Spectra"
  }
  if (isTRUE(collapse)) choices <- c(choices, "Thresholded Particles")
  c(choices, tests)
}

app_download_label <- function(selection) {
  labels <- c(
    "Test Data" = "Download Test Data",
    "Test Map" = "Download Test Map",
    "Processed Spectra" = "Download Processed Spectra",
    "Top Matches" = "Download Top Matches",
    "Thresholded Particles" = "Download Thresholded Particles"
  )
  if(length(selection) != 1L || is.na(selection) ||
     !selection %in% names(labels)) {
    return("Download selected")
  }
  unname(labels[[selection]])
}

app_quantification_treatments <- c(
  "Fill Peaks baseline corrected (recommended)" = "fill_peaks",
  "Modified polynomial baseline corrected" = "modpolyfit",
  "Min-max normalized" = "min_max",
  "Uploaded spectrum (no quantification preprocessing)" = "raw"
)

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
  if(!length(axis)) stop("Upload a valid spectrum before adding ratios.",
                         call. = FALSE)
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
    stop("Ratio selections must stay within the uploaded wavenumber range.",
         call. = FALSE)
  }
  if(identical(type, "area") &&
     (!any(axis >= numerator[[1L]] & axis <= numerator[[2L]]) ||
      !any(axis >= denominator[[1L]] & axis <= denominator[[2L]]))) {
    stop("Each area range must contain at least one uploaded wavenumber.",
         call. = FALSE)
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
    stop("Upload a spectrum with at least two distinct wavenumbers.",
         call. = FALSE)
  }

  closest <- function(value) axis[[which.min(abs(axis - value))]]
  axis_range <- range(axis)
  positive_steps <- diff(axis)
  positive_steps <- positive_steps[positive_steps > 0]
  step <- if(length(positive_steps)) stats::median(positive_steps) else 1

  if(identical(type, "area")) {
    scenario <- c(1650, 1850, 1420, 1500)
    if(all(scenario >= axis_range[[1L]] & scenario <= axis_range[[2L]])) {
      numerator <- sort(vapply(scenario[1:2], closest, numeric(1)))
      denominator <- sort(vapply(scenario[3:4], closest, numeric(1)))
    } else {
      selections <- axis[pmax(
        1L,
        pmin(length(axis), round(c(.60, .78, .24, .42) * length(axis)))
      )]
      numerator <- sort(selections[1:2])
      denominator <- sort(selections[3:4])
    }
  } else {
    scenario <- c(1715, 1460)
    if(all(scenario >= axis_range[[1L]] & scenario <= axis_range[[2L]])) {
      numerator <- closest(scenario[[1L]])
      denominator <- closest(scenario[[2L]])
    } else {
      numerator <- axis[[max(1L, round(.67 * length(axis)))]]
      denominator <- axis[[max(1L, round(.33 * length(axis)))]]
    }
  }

  list(
    min = axis_range[[1L]],
    max = axis_range[[2L]],
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
  c("quantification_treatment", "quantification_definitions",
    definitions$column)
}

app_prepare_quantification_source <- function(
    x, treatment = "fill_peaks", intensity_type = "none", lambda = 4,
    hwi = 50, iterations = 10, degree = 8) {
  treatment <- match.arg(
    treatment, unname(app_quantification_treatments)
  )
  intensity_type <- match.arg(
    intensity_type, c("none", "transmittance", "reflectance")
  )
  x <- as_OpenSpecy(x)

  if(!identical(intensity_type, "none")) {
    x <- adj_intens(x, type = intensity_type, make_rel = FALSE)
  }

  switch(
    treatment,
    raw = x,
    min_max = make_rel(x),
    fill_peaks = subtr_baseline(
      x, type = "fill_peaks", lambda = lambda, hwi = hwi,
      it = iterations, make_rel = FALSE
    ),
    modpolyfit = subtr_baseline(
      x, type = "polynomial", degree = degree, iterations = iterations,
      make_rel = FALSE
    )
  )
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

app_attach_quantification <- function(target, source, definitions, treatment) {
  target <- as_OpenSpecy(target)
  source <- as_OpenSpecy(source)
  if(!nrow(definitions)) return(target)
  if(ncol(target$spectra) != ncol(source$spectra) ||
     !identical(colnames(target$spectra), colnames(source$spectra)) ||
     nrow(target$metadata) != ncol(target$spectra)) {
    stop("Quantification source and processed spectra are not aligned.",
         call. = FALSE)
  }

  target$metadata <- data.table::copy(target$metadata)
  target$metadata$quantification_treatment <- treatment
  target$metadata$quantification_definitions <-
    app_ratio_definitions_text(definitions)
  for(i in seq_len(nrow(definitions))) {
    definition <- definitions[i, , drop = FALSE]
    values <- if(identical(definition$type[[1L]], "area")) {
      app_area_ratio(
        source,
        c(definition$numerator_min[[1L]], definition$numerator_max[[1L]]),
        c(definition$denominator_min[[1L]], definition$denominator_max[[1L]])
      )
    } else {
      peak_ratio(
        source,
        numerator = definition$numerator_min[[1L]],
        denominator = definition$denominator_min[[1L]],
        method = "nearest"
      )
    }
    if(length(values) != nrow(target$metadata)) {
      stop("Quantification returned an unexpected number of values for '",
           definition$name[[1L]], "'.", call. = FALSE)
    }
    target$metadata[[definition$column[[1L]]]] <- as.numeric(values)
  }
  target
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
  reference = "#FB7185",
  spectrum = "#FFFFFF"
)

app_theme_css <- function(theme = app_theme) {
  required <- c(
    "canvas", "panel", "panel_2", "border", "accent", "success",
    "text", "muted", "grid", "axis", "spectrum"
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
