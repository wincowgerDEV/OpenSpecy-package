#' @title Run Open Specy app
#'
#' @description
#' This wrapper function starts the graphical user interface of Open Specy.
#'
#' @details
#' After running this function the Open Specy GUI should open in a separate
#' window or in your computer browser.
#'
#' @param path to store the downloaded app files; defaults to \code{"system"}
#' pointing to \code{system.file(package = "OpenSpecy")}.
#' @param log logical; enables/disables logging to \code{\link[base]{tempdir}()}
#' @param ref git reference; could be a commit, tag, or branch name. Defaults to
#' "main". Only change this in case of errors.
#' @param check_local logical; when \code{TRUE} a previously downloaded copy of
#' the Shiny app located at \code{path} is used instead of downloading a fresh
#' copy from GitHub.
#' @param test_mode logical; for internal testing only.
#' @param \dots arguments passed to \code{\link[shiny]{runApp}()}.
#'
#' @return
#' This function normally does not return any value, see
#' \code{\link[shiny]{runApp}()}.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @author
#' Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[shiny]{runApp}()}
#'
#' @importFrom shiny shinyOptions runApp
#' @importFrom utils installed.packages download.file untar
#' @importFrom jsonlite fromJSON
#' @export
run_app <- function(path = "system", log = TRUE, ref = "main",
                    check_local = FALSE, test_mode = FALSE, ...) {
  pkg <- c("shinyjs", "shinyWidgets", "bs4Dash",
           "dplyr", "ggplot2", "DT")

  miss <- pkg[!(pkg %in% installed.packages()[, "Package"])]

  if(length(miss)) {
    install_cmd <- paste0(
      "install.packages(c(",
      paste(paste0("\"", miss, "\""), collapse = ", "),
      "))"
    )
    message(
      "run_app() requires the following packages: ",
      paste(miss, collapse = ", ")
    )
    message("Install the missing packages by running:\n  ", install_cmd)
    stop("Missing required packages.", call. = FALSE)
  }

  resolve_path <- function(x) {
    if(identical(x, "system")) {
      system.file(package = "OpenSpecy")
    } else {
      x
    }
  }

  ensure_directory <- function(dir_path) {
    if(!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    if(!dir.exists(dir_path)) {
      stop("Unable to create directory '", dir_path, "'.", call. = FALSE)
    }
  }

  find_app_path <- function(dir_path) {
    if(!dir.exists(dir_path)) return(NULL)
    candidates <- c(dir_path, list.dirs(dir_path, recursive = FALSE, full.names = TRUE))
    for(candidate in candidates) {
      if(file.exists(file.path(candidate, "app.R"))) {
        return(candidate)
      }
    }
    NULL
  }

  dd <- resolve_path(path)
  if(!nzchar(dd)) {
    stop("Unable to resolve the destination directory. Please provide a valid 'path'.", call. = FALSE)
  }
  dd <- normalizePath(dd, winslash = "/", mustWork = FALSE)

  if(!identical(path, "system")) {
    ensure_directory(dd)
  }

  Sys.setenv(R_CONFIG_ACTIVE = "run_app")
  shinyOptions(log = log)

  if(check_local) {
    local_app <- find_app_path(dd)
    if(!is.null(local_app)) {
      message("Running local OpenSpecy Shiny app from: ", local_app)
      if(test_mode) {
        return(invisible(local_app))
      }
      return(runApp(local_app, ...))
    }
  }

  if(test_mode) {
    return(invisible(dd))
  }

  owner <- "wincowgerDEV"
  repo <- "OpenSpecy-shiny"

  if(missing(ref)) {
    commits_page <- sprintf("https://github.com/%s/%s/commits/main", owner, repo)
    message("Downloading the OpenSpecy Shiny app from the 'main' branch.")
    message("You can supply the 'ref' argument to download a different branch, tag, or commit.")
    message("Browse commits at: ", commits_page)

    commits_url <- sprintf("https://api.github.com/repos/%s/%s/commits?per_page=10", owner, repo)
    commit_info <- try(fromJSON(commits_url), silent = TRUE)
    if(!inherits(commit_info, "try-error") && length(commit_info)) {
      hashes <- substr(commit_info$sha, 1, 10)
      commit_dates <- format(as.POSIXct(commit_info$commit$author$date, tz = "UTC"),
                             "%Y-%m-%d %H:%M:%S %Z")
      commit_table <- data.frame(hash = hashes, date = commit_dates, stringsAsFactors = FALSE)
      message("10 most recent commits:")
      print(commit_table, row.names = FALSE)
    } else {
      message("Unable to retrieve recent commit information from GitHub.")
    }
  }

  ensure_directory(dd)

  tar_name <- sprintf("%s_%s.tar.gz", repo, ref)
  tar_path <- file.path(dd, tar_name)
  download_url <- sprintf("https://api.github.com/repos/%s/%s/tarball/%s", owner, repo, ref)

  download.file(download_url, destfile = tar_path, mode = "wb", quiet = TRUE)
  message("Saved tarball to: ", tar_path)

  extracted_files <- untar(tar_path, list = TRUE)
  top_dirs <- unique(sub("/.*$", "", extracted_files))
  untar(tar_path, exdir = dd)

  extracted_dir <- NULL
  if(length(top_dirs)) {
    candidate <- file.path(dd, top_dirs[1])
    if(dir.exists(candidate)) {
      target_dir <- file.path(dd, sprintf("%s_%s", repo, ref))
      if(dir.exists(target_dir)) {
        unlink(target_dir, recursive = TRUE, force = TRUE)
      }
      if(suppressWarnings(file.rename(candidate, target_dir))) {
        extracted_dir <- target_dir
      } else {
        extracted_dir <- candidate
      }
    }
  }

  if(is.null(extracted_dir)) {
    extracted_dir <- dd
  }

  app_path <- find_app_path(extracted_dir)

  if(is.null(app_path)) {
    stop("Unable to locate the Shiny app entry point after downloading.", call. = FALSE)
  }

  runApp(app_path, ...)
}
