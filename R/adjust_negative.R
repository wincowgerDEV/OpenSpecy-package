#' @title Adjust negative values
#'
#' @description
#' This helper function converts a numeric values \code{x < 1} into values >= 1
#' by keeping absolute differences between values. If \code{na.rm} is
#' \code{TRUE}, missing values are removed before the computation proceeds.
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso
#' \code{\link[base]{min}} for the calculation of minima
#'
#' @examples
#' adjust_negative(-5:5)
#' adjust_negative(c(0.2, 0.7, 1))
#'
#' @importFrom magrittr %>%
#' @export
adjust_negative <- function(x, na.rm = FALSE) {
  if (min(x, na.rm = na.rm) < 1) {
    x + min(x, na.rm = na.rm) %>% abs() + 1
  } else {
    x
  }
}
