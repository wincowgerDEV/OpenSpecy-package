#' @rdname data_normalization
#'
#' @title Normalization of spectral data
#'
#' @description
#' \code{adjust_negative()} converts numeric values \code{x < 1} into values
#' >= 1 by keeping absolute differences between values.
#' \code{make_relative()} converts values \code{x} into relative values between
#' 0 and 1.
#' If \code{na.rm} is \code{TRUE}, missing values are removed before the
#' computation proceeds.
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}.
#' @param na.rm logical. Should missing values be removed?
#'
#' @seealso
#' \code{\link[base]{min}} for the calculation of minima
#'
#' @examples
#' adjust_negative(c(-1000, -1, 0, 1, 10))
#' make_relative(-2:10)
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

#' @rdname data_normalization
#'
#' @export
make_relative <- function(x, na.rm = FALSE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}
