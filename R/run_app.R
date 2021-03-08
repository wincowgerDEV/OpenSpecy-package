#' @title Run Open Specy app
#'
#' @description
#' This function starts the graphical user interface of Open Specy.
#'
#' @details
#' After running this function the Open Specy GUI should open in a separate
#' window or in your computer browser.
#'
#' @param app_dir the app to run.
#' @param \dots arguments passed to \code{\link[shiny]{runApp}()}.
#'
#' @seealso
#' \code{\link[shiny]{runApp}()}
#'
#' @examples
#' \dontrun{
#' run_app(launch.browser = TRUE)
#' }
#'
#' @importFrom shiny runApp
#' @export
run_app <- function(app_dir = system.file("shiny", package = "OpenSpecy"),
                    ...) {
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling OpenSpecy.",
         call. = FALSE)
  }

  runApp(app_dir, ...)
}
