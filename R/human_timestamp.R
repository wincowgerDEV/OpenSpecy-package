#' @title Create human readable timestamps
#'
#' @description
#' This helper function creates human readable timestamps in the form of
#' \code{\%Y\%m\%d-\%H\%M\%OS}.
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
