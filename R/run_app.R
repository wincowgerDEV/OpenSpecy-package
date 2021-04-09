#' @title Run Open Specy app
#'
#' @description
#' This wrapper function starts the graphical user interface of Open Specy.
#'
#' @details
#' After running this function the Open Specy GUI should open in a separate
#' window or in your computer browser.
#'
#' @param app_dir the app to run; defaults to \code{"system"} pointing to
#' \code{system.file("shiny", package = "OpenSpecy")}.
#' @param path where to look for the local library files; defaults to
#' \code{"system"} pointing to
#' \code{system.file("extdata", package = "OpenSpecy")}.
#' @param log logical; enables/disables logging to \code{\link[base]{tempdir}()}
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
#' @importFrom shiny runApp shinyOptions
#' @importFrom utils installed.packages
#' @export
run_app <- function(app_dir = "system", path = "system", log = TRUE, ...) {
  if (is.null(app_dir) || app_dir == "")
    stop("Could not find app directory. Try reinstalling OpenSpecy.",
         call. = FALSE)

  pkg <- c("config", "shinyjs", "shinythemes", "shinyBS", "shinyWidgets",
           "plotly", "data.table", "DT", "curl", "rdrop2", "mongolite",
           "loggit")
  mpkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]

  if(length(mpkg)) stop("run_app() requires the following packages: ",
                        paste(paste0("'", mpkg, "'"), collapse = ", "),
                        call. = F)

  ad <- ifelse(app_dir == "system",
               system.file("shiny", package = "OpenSpecy"),
               app_dir)
  wd <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  Sys.setenv(R_CONFIG_ACTIVE = "run_app")

  shinyOptions(library_path = wd, log = log)
  runApp(ad, ...)
}
