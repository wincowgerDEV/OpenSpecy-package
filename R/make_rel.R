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
#' instruments. A finite constant spectrum has no relative contrast and is
#' therefore returned as zero rather than producing non-finite values.
#'
#' @param x a numeric vector or an \R OpenSpecy object
#' @param na.rm logical. Should missing values be removed?
#' @param \ldots further arguments passed to \code{make_rel()}.
#'
#' @return
#' \code{make_rel()} returns numeric vectors, numeric matrices with each
#' spectrum normalized by column, or an \code{OpenSpecy} object with the
#' normalized intensity data.
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

  span <- r[2L] - r[1L]
  if (is.finite(span) && span == 0) return(x - r[1L])

  return((x - r[1L]) / span)
}

#' @rdname make_rel
#'
#' @export
make_rel.matrix <- function(x, na.rm = FALSE, ...) {
  if (ncol(x) == 0L) return(x)

  # Column ranges normalize all spectra in one pass and avoid per-spectrum
  # apply() calls for hyperspectral matrices.
  mins <- matrixStats::colMins(x, na.rm = na.rm)
  maxs <- matrixStats::colMaxs(x, na.rm = na.rm)
  spans <- maxs - mins
  # A zero span is a valid flat spectrum. Dividing by one keeps its finite
  # values at zero while preserving any NA positions.
  flat <- is.finite(spans) & spans == 0
  spans[flat] <- 1
  out <- x - rep(mins, each = nrow(x))
  out <- out / rep(spans, each = nrow(x))
  colnames(out) <- colnames(x)
  rownames(out) <- rownames(x)
  out
}

#' @rdname make_rel
#'
#' @export
make_rel.OpenSpecy <- function(x, na.rm = FALSE, ...) {
  x <- as_OpenSpecy(x)
  x$spectra <- make_rel(x$spectra, na.rm = na.rm)
  .append_specs_transformation(x, list(
    method = "make_rel", na_rm = isTRUE(na.rm), lossy = TRUE
  ))
}
