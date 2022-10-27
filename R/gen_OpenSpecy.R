#' @rdname gen_OpenSpecy
#'
#' @title Generic functions for \code{OpenSpecy} objects
#'
#' @description
#' Functions to
#'
#' @details
#' \code{as_OpenSpecy()}
#'
#' @param x x.
#' @param \ldots args.
#'
#' @return
#' return
#'
#' @examples
#' c()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' seealso.
#'
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export
head.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) %>% head(...)
}

#' @rdname gen_OpenSpecy
#'
#' @export
print.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) %>% print(...)
  cat("\n$metadata\n")
  print(x$metadata)
}
