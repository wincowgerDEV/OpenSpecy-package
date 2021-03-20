#' @title Create human readable timestamps
#'
#' @description
#' This helper function creates human readable timestamps in the form of
#' \code{\%Y\%m\%d-\%H\%M\%OS} at the current time.
#'
#' @details
#' Human readable timestamps are appended to file names and fields when metadata
#' are shared with the Open Specy community.
#'
#' @return
#' \code{human_timestamp()} returns a character value with the respective
#' timestamp.
#'
#' @examples
#' human_timestamp()
#'
#' @author
#' Win Cowger
#'
#' @seealso
#' \code{\link[base]{format.Date}} for date conversion functions
#'
#' @importFrom magrittr %>%
#' @export
human_timestamp <- function() {
  Sys.time() %>% format("%Y%m%d-%H%M%OS")
}
