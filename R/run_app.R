#' @title Run Open Specy app
#'
#' @description
#' This wrapper function starts the graphical user interface of Open Specy.
#'
#' @details
#' After running this function the Open Specy GUI should open in a separate
#' window or in your computer browser. When downloads are required, the function
#' reports the GitHub commit used and preserves that information for subsequent
#' reuse when \code{check_local = TRUE}.
#'
#' @param path to store the downloaded app files; defaults to \code{"system"}
#' pointing to \code{system.file(package = "OpenSpecy")}.
#' @param log logical; enables/disables logging to \code{\link[base]{tempdir}()}
#' @param ref git reference; could be a commit, tag, or branch name. Defaults to
#' "main". Only change this in case of errors.
#' @param check_local logical; when \code{TRUE} a previously downloaded copy of
#' the Shiny app located at \code{path} is used instead of downloading a fresh
#' copy from GitHub. The directory may contain either a single-file
#' \code{app.R} application or a \code{server.R}/\code{ui.R} pair. Metadata
#' about downloaded copies, including the originating commit hash, is stored
#' alongside the app and surfaced when the local copy is reused. When a
#' specific \code{ref} is requested, matching local copies are preferred and
#' unmatched directories are ignored.
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
                    check_local = TRUE, test_mode = FALSE, ...) {
  pkg <- c("shinyjs", "shinyWidgets", "bs4Dash",
           "dplyr", "ggplot2", "DT")

  owner <- "wincowgerDEV"
  repo <- "OpenSpecy-shiny"
  metadata_filename <- ".openspecy-shiny-metadata.rds"
  ref_missing <- missing(ref)

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

  metadata_path <- function(dir_path) {
    file.path(dir_path, metadata_filename)
  }

  read_metadata <- function(dir_path) {
    file <- metadata_path(dir_path)
    if(!file.exists(file)) return(NULL)
    tryCatch(readRDS(file), error = function(...) NULL)
  }

  write_metadata <- function(dir_path, data) {
    file <- metadata_path(dir_path)
    tryCatch(saveRDS(data, file), error = function(e) {
      warning(
        "Unable to save metadata for the downloaded app: ",
        conditionMessage(e), call. = FALSE
      )
      NULL
    })
    invisible(file)
  }

  commit_url <- function(owner_val, repo_val, commit_sha) {
    if(is.null(commit_sha) || !nzchar(commit_sha)) return(NULL)
    sprintf("https://github.com/%s/%s/commit/%s", owner_val, repo_val, commit_sha)
  }

  fallback <- function(value, default) {
    if(is.null(value)) return(default)
    if(is.character(value)) {
      if(!length(value) || !nzchar(value[1])) return(default)
      return(value[1])
    }
    value
  }

  find_app_paths <- function(dir_path) {
    if(!dir.exists(dir_path)) return(character())
    candidates <- unique(c(dir_path, list.dirs(dir_path, recursive = TRUE,
                                               full.names = TRUE)))

    keep <- vapply(candidates, function(candidate) {
      if(!dir.exists(candidate)) return(FALSE)

      has_app <- file.exists(file.path(candidate, "app.R"))
      has_server_ui <- file.exists(file.path(candidate, "server.R")) &&
        file.exists(file.path(candidate, "ui.R"))

      has_app || has_server_ui
    }, logical(1), USE.NAMES = FALSE)

    candidates[keep]
  }

  find_app_path <- function(dir_path) {
    candidates <- find_app_paths(dir_path)
    if(!length(candidates)) return(NULL)
    candidates[[1]]
  }
  


  parse_download_time <- function(value) {
    if(is.null(value) || !length(value)) return(-Inf)

    if(inherits(value, "POSIXt")) {
      parsed <- suppressWarnings(as.numeric(as.POSIXct(value, tz = "UTC")))
      if(!is.na(parsed)) return(parsed)
    }

    if(inherits(value, "Date")) {
      parsed <- suppressWarnings(as.numeric(as.POSIXct(value, tz = "UTC")))
      if(!is.na(parsed)) return(parsed)
    }

    if(is.numeric(value)) {
      numeric_val <- suppressWarnings(as.numeric(value[1]))
      if(!is.na(numeric_val)) return(numeric_val)
    }

    if(is.character(value)) {
      value <- trimws(value[1])
      if(!nzchar(value)) return(-Inf)

      collapse_ws <- function(txt) {
        gsub("  +", " ", txt)
      }

      strip_trailing_tz <- function(txt) {
        collapse_ws(sub("([[:space:]])([A-Z]{2,5})$", "", txt, perl = TRUE))
      }

      strip_mid_tz <- function(txt) {
        collapse_ws(sub("([[:space:]])([A-Z]{2,5})([[:space:]]+[0-9]{4})$", "\\1\\3", txt,
                              perl = TRUE))
      }

      candidate_values <- unique(trimws(c(value, strip_trailing_tz(value), strip_mid_tz(value))))
      candidate_values <- candidate_values[nzchar(candidate_values)]

      try_formats <- c(
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%dT%H:%M:%OS",
        "%a %b %d %H:%M:%S %Y",
        "%m/%d/%Y %H:%M:%OS",
        "%Y-%m-%d"
      )

      for(candidate in candidate_values) {
        parsed <- suppressWarnings(
          as.POSIXct(candidate, tz = "UTC", tryFormats = try_formats)
        )
        if(!is.na(parsed)) {
          parsed_num <- suppressWarnings(as.numeric(parsed))
          if(!is.na(parsed_num)) return(parsed_num)
        }
      }

      offset_candidate <- trimws(gsub("\\b(UTC|GMT)\\b", "+0000", value, ignore.case = TRUE))
      if(nzchar(offset_candidate) && !identical(offset_candidate, value)) {
        offset_formats <- c(
          "%Y-%m-%d %H:%M:%OS %z",
          "%a %b %d %H:%M:%S %z %Y"
        )
        parsed <- suppressWarnings(
          as.POSIXct(offset_candidate, tz = "UTC", tryFormats = offset_formats)
        )
        if(!is.na(parsed)) {
          parsed_num <- suppressWarnings(as.numeric(parsed))
          if(!is.na(parsed_num)) return(parsed_num)
        }
      }

      parsed_default <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
      if(!is.na(parsed_default)) {
        parsed_num <- suppressWarnings(as.numeric(parsed_default))
        if(!is.na(parsed_num)) return(parsed_num)
      }
    }

    -Inf
  }


  select_local_app <- function(dir_path, requested_ref) {
    candidates <- find_app_paths(dir_path)
    if(!length(candidates)) return(NULL)

    requested_ref_val <- fallback(requested_ref, "")
    requested_ref_lower <- tolower(requested_ref_val)

    best_path <- NULL
    best_score <- -Inf
    best_time <- -Inf
    best_metadata <- NULL
    best_match_type <- "none"

    for(candidate in candidates) {
      metadata <- read_metadata(candidate)
      commit_val <- fallback(metadata$commit, "")
      ref_val <- fallback(metadata$ref, "")
      downloaded_at <- fallback(metadata$downloaded_at, "")

      commit_lower <- tolower(commit_val)
      ref_lower <- tolower(ref_val)

      score <- 0
      match_type <- "none"

      if(nzchar(requested_ref_lower) && nzchar(commit_lower) &&
         (startsWith(requested_ref_lower, commit_lower)||startsWith(commit_lower, requested_ref_lower))) {
        score <- 3
        match_type <- "commit"
      } else if(nzchar(requested_ref_lower) && nzchar(ref_lower) &&
                identical(ref_lower, requested_ref_lower)) {
        score <- 2
        match_type <- "ref"
      } else if(length(metadata)) {
        score <- 1
        match_type <- "metadata"
      }

      downloaded_time <- parse_download_time(downloaded_at)

      if(is.null(best_path) || score > best_score ||
         (score == best_score && downloaded_time > best_time)) {
        best_path <- candidate
        best_score <- score
        best_time <- downloaded_time
        best_metadata <- metadata
        best_match_type <- match_type
      }
    }

    if(is.null(best_path)) return(NULL)

    list(
      path = best_path,
      metadata = best_metadata,
      match_type = best_match_type
    )
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
    selection <- select_local_app(dir_path = dd,requested_ref =  ref)

    is_commit_ref <- function(x) {
        if(!is.character(x) || !length(x)) return(FALSE)
        grepl("^[0-9a-f]{7,40}$", x[1], ignore.case = TRUE)
    }
    
    require_exact_match <- !ref_missing && !identical(ref, "main")
    require_commit_match <- require_exact_match && is_commit_ref(ref)

    if(!is.null(selection) &&
       (!require_exact_match || selection$match_type != "none") &&
       (!require_commit_match || selection$match_type == "commit")) {
      local_app <- selection$path
      metadata <- selection$metadata
      if(is.null(metadata)) metadata <- list()
      effective_owner <- fallback(metadata$owner, owner)
      effective_repo <- fallback(metadata$repo, repo)
      message("Running local OpenSpecy Shiny app from: ", local_app)
      if(!is.null(metadata$commit) && nzchar(metadata$commit)) {
        url <- commit_url(effective_owner, effective_repo, metadata$commit)
        if(!is.null(url)) {
          message(
            "Local app was downloaded from commit ",
            metadata$commit, ". View commit: ", url
          )
        } else {
          message(
            "Local app was downloaded from commit ",
            metadata$commit, "."
          )
        }
      } else {
        message("Commit information is not available for the local app.")
      }
      if(test_mode) {
        return(invisible(local_app))
      }
      return(runApp(local_app, ...))
    }
  }

  if(test_mode) {
    return(invisible(dd))
  }

  if(ref_missing || identical(ref, "main")) {
    commits_page <- sprintf("https://github.com/%s/%s/commits/main", owner, repo)
    message("Downloading the OpenSpecy Shiny app from the 'main' branch.")
    message("You can supply the 'ref' argument to download a different branch, tag, or commit.")
    message("Browse commits at: ", commits_page)

    commits_url <- sprintf("https://api.github.com/repos/%s/%s/commits?per_page=10", owner, repo)
    commit_info <- try(fromJSON(commits_url), silent = TRUE)
    if(!inherits(commit_info, "try-error") && length(commit_info)) {
      hashes <- commit_info$sha
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
  commit_sha <- NULL
  if(length(top_dirs)) {
    pattern <- sprintf("^%s-%s-([0-9a-f]{7,40})$", owner, repo)
    sha_candidate <- sub(pattern, "\\1", top_dirs[1], perl = TRUE)
    if(!identical(sha_candidate, top_dirs[1])) {
      commit_sha <- sha_candidate
    }
  }
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

  metadata <- list(
    commit = commit_sha,
    ref = ref,
    owner = owner,
    repo = repo,
    downloaded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  write_metadata(app_path, metadata)

  message("Launching OpenSpecy Shiny app from: ", app_path)
  if(!is.null(commit_sha) && nzchar(commit_sha)) {
    remote_url <- commit_url(owner, repo, commit_sha)
    if(!is.null(remote_url)) {
      message("App commit: ", commit_sha, ". View commit: ", remote_url)
    } else {
      message("App commit: ", commit_sha, ".")
    }
  }

  runApp(app_path, ...)
}
