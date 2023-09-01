#' @rdname make_rel
#' @title Make spectral intensities relative
#'
#' @description
#' \code{make_rel()} converts intensities \code{x} into relative values between
#' 0 and 1 using the standard normalization equation.
#' If \code{na.rm} is \code{TRUE}, missing values are removed before the
#' computation proceeds.
#'
#' @details
#' \code{make_rel()} is used to retain the relative height proportions between
#' spectra while avoiding the large numbers that can result from some spectral
#' instruments.
#'
#' @param x a numeric vector or an \R OpenSpecy object
#' @param na.rm logical. Should missing values be removed?
#' @param \ldots further arguments passed to \code{make_rel()}.
#'
#' @return
#' \code{make_rel()} return numeric vectors (if vector provided) or an
#' \code{OpenSpecy} object with the normalized intensity data.
#'
#' @examples
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
#'
#' @export
make_rel <- function(x, ...) {
  UseMethod("make_rel")
}

#' @rdname make_rel
#'
#' @export
make_rel.default <- function(x, na.rm = FALSE, ...) {
  r <- range(x, na.rm = na.rm)

  return((x - r[1]) / (r[2] - r[1]))
}

#' @rdname make_rel
#'
#' @export
make_rel.OpenSpecy <- function(x, na.rm = FALSE, ...) {
  x$spectra <- x$spectra[, lapply(.SD, make_rel, na.rm = na.rm, ...)]

  return(x)
}
