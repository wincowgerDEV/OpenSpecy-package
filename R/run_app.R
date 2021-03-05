#' @title Run Open Specy app
#'
#' @description
#' This function starts the graphical user interface of Open Specy.
#'
#' @param \dots arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @examples
#' \dontrun{
#' run_app(launch.browser = TRUE)
#' }
#'
#' @importFrom shiny runApp
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "OpenSpecy")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling OpenSpecy.",
         call. = FALSE)
  }

  runApp(app_dir, ...)
}