#' @title Run Open Specy app
#'
#' @description
#' This wrapper function starts the graphical user interface of Open Specy.
#'
#' @details
#' By default, `run_app()` launches the Shiny app bundled with the installed
#' OpenSpecy package at `system.file("shiny", package = "OpenSpecy")`.
#' Historical GitHub download support has been removed so package installs use
#' the same app files offline. Set `path` to an explicit app directory when
#' testing a local Shiny app during development.
#'
#' @param path Shiny app directory, or `"system"` to launch the bundled app.
#' @param log logical; enables/disables Shiny logging to `tempdir()`.
#' @param ref retained for compatibility with older releases; ignored because
#'   the app is bundled with the package.
#' @param check_local retained for compatibility with older releases; ignored
#'   because `path = "system"` always uses the bundled app.
#' @param test_mode logical; for internal testing only.
#' @param launch.browser option for `shiny::runApp()`.
#' @param \dots arguments passed to `shiny::runApp()`.
#'
#' @return
#' This function normally does not return any value, see `shiny::runApp()`.
#' In `test_mode`, it invisibly returns the resolved app path.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @author
#' Win Cowger, Zacharias Steinmetz, Garth Covernton
#'
#' @seealso
#' `shiny::runApp()`
#'
#' @importFrom shiny shinyOptions runApp
#' @export
run_app <- function(path = "system", log = TRUE, ref = NULL,
                    check_local = TRUE, test_mode = FALSE,
                    launch.browser = getOption("shiny.launch.browser",
                                               interactive()), ...) {
  if(!is.null(ref)) {
    warning(
      "`ref` is ignored because the Open Specy Shiny app is bundled with ",
      "the package.",
      call. = FALSE
    )
  }

  if(!isTRUE(check_local)) {
    warning(
      "`check_local` is ignored because remote Shiny app downloads have ",
      "been removed.",
      call. = FALSE
    )
  }

  app_path <- .openspecy_shiny_app_path(path)

  if(identical(path, "system")) {
    .openspecy_validate_bundled_shiny_app(app_path)
  }

  Sys.setenv(R_CONFIG_ACTIVE = "run_app")
  shinyOptions(log = log)

  if(test_mode) {
    return(invisible(app_path))
  }

  .openspecy_require_shiny_packages()

  message("Launching bundled OpenSpecy Shiny app from: ", app_path)
  runApp(app_path, launch.browser = launch.browser, ...)
}

.openspecy_app_packages <- function() {
  c("shinyjs", "shinyWidgets", "bs4Dash", "dplyr", "ggplot2", "DT",
    "reshape2", "curl", "scales")
}

.openspecy_require_shiny_packages <- function() {
  miss <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]

  if(length(miss)) {
    install_cmd <- paste0(
      "install.packages(c(",
      paste(paste0("\"", miss, "\""), collapse = ", "),
      "))"
    )
    message(
      "run_app() requires the following Shiny app packages: ",
      paste(miss, collapse = ", ")
    )
    message("Install the missing packages by running:\n  ", install_cmd)
    stop("Missing required Shiny app packages.", call. = FALSE)
  }

  invisible(TRUE)
}

.openspecy_is_shiny_app_dir <- function(path) {
  file.exists(file.path(path, "app.R")) ||
    (file.exists(file.path(path, "server.R")) &&
       file.exists(file.path(path, "ui.R")))
}

.openspecy_find_shiny_app_path <- function(path) {
  if(!dir.exists(path)) {
    return(NULL)
  }

  candidates <- unique(c(path, list.dirs(path, recursive = TRUE,
                                         full.names = TRUE)))
  matches <- candidates[vapply(candidates, .openspecy_is_shiny_app_dir,
                               logical(1))]

  if(!length(matches)) {
    return(NULL)
  }

  matches[[1]]
}

.openspecy_shiny_app_path <- function(path = "system") {
  if(identical(path, "system")) {
    app_path <- system.file("shiny", package = "OpenSpecy")
    if(!nzchar(app_path)) {
      stop(
        "Unable to locate the bundled OpenSpecy Shiny app. Reinstall ",
        "OpenSpecy or provide a local app directory with `path`.",
        call. = FALSE
      )
    }
    return(normalizePath(app_path, winslash = "/", mustWork = TRUE))
  }

  base_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  app_path <- .openspecy_find_shiny_app_path(base_path)
  if(is.null(app_path)) {
    stop(
      "Unable to locate a Shiny app at `path`. Expected an app.R file or ",
      "a server.R/ui.R pair.",
      call. = FALSE
    )
  }

  normalizePath(app_path, winslash = "/", mustWork = TRUE)
}

.openspecy_bundled_shiny_files <- function() {
  c(
    "global.R",
    "server.R",
    "ui.R",
    "config.yml",
    "OPEN_SPECY_SHINY_SOURCE",
    "www/logo.png",
    "www/donation.png",
    "www/favicon.png",
    "www/favicon.ico",
    "www/TOS.txt",
    "www/privacy_policy.txt"
  )
}

.openspecy_validate_bundled_shiny_app <- function(path) {
  required <- .openspecy_bundled_shiny_files()
  missing <- required[!file.exists(file.path(path, required))]

  if(length(missing)) {
    stop(
      "The bundled OpenSpecy Shiny app is incomplete. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
