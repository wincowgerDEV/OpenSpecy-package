#' @title Create human readable timestamps
#'
#' @description
#' This helper function creates human readable timestamps in the form of
#' \code{\%Y\%m\%d-\%H\%M\%OS} at the current time.
#'
#' @details
#' Human readable timestamps are appended to file names and fields when metadata
#' are shared to the Open Specy data portal at
#' \url{https://www.dropbox.com/sh/vibzoiq1i4o5usp/AAAe50RJhnvt6NoMDAJZ1CY8a?dl=0}
#'
#' @seealso
#' \code{\link[base]{format.Date}} for date conversion functions
#'
#' @examples
#' human_timestamp()
#'
#' @importFrom magrittr %>%
#' @export
human_timestamp <- function() {
  Sys.time() %>% format("%Y%m%d-%H%M%OS")
}
