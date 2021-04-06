#' @title Run Open Specy app
#'
#' @description
#' This wrapper function starts the graphical user interface of Open Specy.
#'
#' @details
#' After running this function the Open Specy GUI should open in a separate
#' window or in your computer browser.
#'
#' @param app_dir the app to run; defaults to
#' \code{system.file("shiny", package = "OpenSpecy")}.
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
#' @importFrom shiny runApp
#' @importFrom utils installed.packages
#' @export
run_app <- function(app_dir = system.file("shiny", package = "OpenSpecy"),
                    ...) {
  if (app_dir == "") stop("Could not find app directory. ",
                          "Try reinstalling OpenSpecy.", call. = FALSE)

  pkg <- c("config", "shinyjs", "shinythemes", "shinyBS", "shinyEventLogger",
           "shinyWidgets", "ids", "plotly", "data.table", "DT", "curl",
           "rdrop2")
  mpkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]

  if(length(mpkg)) stop("run_app() requires the following packages: ",
                        paste(paste0("'", mpkg, "'"), collapse = ", "),
                        call. = F)

  runApp(app_dir, ...)
}
