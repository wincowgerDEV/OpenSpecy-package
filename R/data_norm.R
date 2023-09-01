#' @rdname data_norm
#' @title Normalization and conversion of spectral data
#'
#' @description
#' \code{adj_res()} and \code{conform_res()} are helper functions to align
#' wavenumbers in terms of their spectral resolution.
#' \code{adj_neg()} converts numeric intensities \code{y} < 1 into values >= 1,
#' keeping absolute differences between intensity values by shifting each value
#' by the minimum intensity.
#' \code{make_rel()} converts intensities \code{y} into relative values between
#' 0 and 1 using the standard normalization equation.
#' If \code{na.rm} is \code{TRUE}, missing values are removed before the
#' computation proceeds.
#'
#' @details
#' \code{adj_res()} and \code{conform_res()} are used in Open Specy to
#' facilitate comparisons of spectra with different resolutions.
#' \code{adj_neg()} is used to avoid errors that could arise from log
#' transforming spectra when using \code{\link{adj_intens}()} and other
#' functions.
#' \code{make_rel()} is used to retain the relative height proportions between
#' spectra while avoiding the large numbers that can result from some spectral
#' instruments.
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}; \code{x} should contain the spectral
#' wavenumbers.
#' @param y a numeric vector containing the spectral intensities.
#' @param res spectral resolution supplied to \code{fun}.
#' @param fun the function to be applied to each element of \code{x}; defaults
#' to \code{\link[base]{round}()} to round to a specific resolution \code{res}.
#' @param na.rm logical. Should missing values be removed?
#'
#' @return
#' \code{adj_res()} and \code{conform_res()} return a numeric vector with
#' resolution-conformed wavenumbers.
#' \code{adj_neg()} and \code{make_rel()} return numeric vectors
#' with the normalized intensity data.
#'
#' @examples
#' adj_res(seq(500, 4000, 4), 5)
#' conform_res(seq(500, 4000, 4))
#' adj_neg(c(-1000, -1, 0, 1, 10))
#' make_rel(c(-1000, -1, 0, 1, 10))
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} and \code{\link[base]{round}()};
#' \code{\link{adj_intens}()} for log transformation functions;
#' \code{\link{conform_spec}()} for conforming wavenumbers of an
#' \code{OpenSpecy} object to be matched with a reference library
#'
#' @export
adj_res <- function(x, res = 1, fun = round) {
  fun(x / res) * res
}

#' @rdname data_norm
#'
#' @export
conform_res <- function(x, res = 5) {
  seq(adj_res(min(x), res, ceiling), adj_res(max(x), res, floor), by = res)
}

#' @rdname data_norm
#'
#' @export
adj_neg <- function(y, na.rm = FALSE) {
  if (min(y, na.rm = na.rm) < 1) {
    y + min(y, na.rm = na.rm) |> abs() + 1
  } else {
    y
  }
}

#' @rdname data_norm
#' @importFrom data.table fifelse
#'
#' @export
mean_replace <- function(y, na.rm = TRUE) {
  m <- mean(y, na.rm = na.rm)

  fifelse(is.na(y), m, y)
}

#' @rdname data_norm
#'
#' @export
is_empty_vector <- function(x) {
  # Check if the vector is NULL or has zero length
  if (is.null(x) || length(x) == 0) {
    return(TRUE)
  }

  # Check if all values are NA or NaN (for numeric vectors)
  if (is.numeric(x)) {
    return(all(is.na(x) | is.nan(x)))
  }

  # Check if all values are NA or empty strings (for character vectors)
  if (is.character(x)) {
    return(all(is.na(x) | x == ""))
  }

  # Check if all values are NA (for other types of vectors)
  return(all(is.na(x)))
}
