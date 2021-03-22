#' @rdname data_normalization
#'
#' @title Normalization of spectral data
#'
#' @description
#' \code{adjust_negative()} converts numeric values \code{x} < 1 into values
#' >= 1, keeping absolute differences between values by shifting intensity
#' values with the value of the smallest number.
#' \code{make_relative()} converts values \code{x} into relative values between
#' 0 and 1 using the standard normalization equation.
#' If \code{na.rm} is \code{TRUE}, missing values are removed before the
#' computation proceeds.
#'
#' @details
#' \code{adjust_negative()} is used in Open Specy to avoid errors that could
#' arise from log transforming spectra when using
#' \code{\link{adjust_intensity}()} and other functions.
#' \code{make_relative()} is used in Open Specy to retain the relative
#' height proportions between spectra while avoiding the large numbers that can
#' result from some spectral instruments.
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}.
#' @param na.rm logical. Should missing values be removed?
#'
#' @return
#' \code{adjust_negative()} and \code{make_relative()} return numeric vectors
#' with the normalized data.
#'
#' @examples
#' adjust_negative(c(-1000, -1, 0, 1, 10))
#' make_relative(c(-1000, -1, 0, 1, 10))
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} for the calculation of minima;
#' \code{\link{adjust_intensity}()} for log transformation functions
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
