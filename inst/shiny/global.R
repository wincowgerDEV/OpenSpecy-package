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

app_plot_palette <- list(
  panel = "#0B1220",
  grid = "#26364D",
  axis = "#6F86A3",
  text = "#E6EDF7",
  primary = "#67E8F9",
  reference = "#FB7185"
)

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
      bgcolor = "#16243A",
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
  tags$details(
    tags$summary(summary),
    tags$small(...)
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
      strip.background = element_rect(fill = "#16243A",
                                      color = app_plot_palette$axis),
      strip.text = element_text(color = app_plot_palette$text)
    )
}
