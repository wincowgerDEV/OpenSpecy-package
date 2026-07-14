# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
translate <- file.exists("www/googletranslate.html")

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

# Define the custom theme
  theme_black_minimal <- function(base_size = 11, base_family = "") {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
            plot.background = element_rect(fill = "black", color = NA),
            panel.background = element_rect(fill = "black", color = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white"),
            axis.line = element_line(color = "white"),
            axis.ticks = element_line(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            plot.title = element_text(color = "white", hjust = 0.5),
            plot.subtitle = element_text(color = "white", hjust = 0.5),
            plot.caption = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.background = element_rect(fill = "black"),
            legend.key = element_rect(fill = "black"),
            strip.background = element_rect(fill = "black", color = NA),
      strip.text = element_text(color = "white")
    )
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


# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define the custom theme
theme_black_minimal <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      axis.line = element_line(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5),
      plot.subtitle = element_text(color = "white", hjust = 0.5),
      plot.caption = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      strip.background = element_rect(fill = "black", color = NA),
      strip.text = element_text(color = "white")
    )
}
