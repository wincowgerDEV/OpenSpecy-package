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
#' @param \dots arguments passed to \code{\link[shiny]{runApp}()}.
#'
#' @return
#' This function normally does not return any value, see
#' \code{\link[shiny]{runGitHub}()}.
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
#' \code{\link[shiny]{runGitHub}()}
#'
#' @importFrom shiny runGitHub shinyOptions
#' @importFrom utils installed.packages
#' @export
run_app <- function(path = "system", log = TRUE, ref = "main", ...) {
  pkg <- c("config", "shinyjs", "shinythemes", "shinyBS", "shinyWidgets",
           "plotly", "data.table", "DT", "curl", "rdrop2", "mongolite",
           "loggit")
  mpkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]

  if(length(mpkg)) stop("run_app() requires the following packages: ",
                        paste(paste0("'", mpkg, "'"), collapse = ", "),
                        call. = F)

  dd <- ifelse(path == "system",
               system.file(package = "OpenSpecy"),
               path)

  Sys.setenv(R_CONFIG_ACTIVE = "run_app")

  shinyOptions(log = log)
  runGitHub("OpenSpecy-shiny", "wincowgerDEV", destdir = dd, ref = ref, ...)
}
