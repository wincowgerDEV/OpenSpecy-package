#' @rdname peak_ratio
#' @title Calculate ratios between spectral intensities at two wavenumbers
#'
#' @description
#' Calculates the ratio between intensities at numerator and denominator
#' wavenumbers for every spectrum in an \code{OpenSpecy} object.
#'
#' @param x an \code{OpenSpecy} object.
#' @param numerator a finite numeric scalar giving the numerator wavenumber.
#' @param denominator a finite numeric scalar giving the denominator
#' wavenumber.
#' @param method character; use \code{"nearest"} to select measured points or
#' \code{"linear"} to interpolate between adjacent measured points.
#' @param \ldots additional arguments passed to methods.
#'
#' @return
#' A named numeric vector with one ratio per spectrum. Names and order match
#' the columns of \code{x$spectra}. Ratios with a zero denominator or a
#' non-finite numerator, denominator, or result are returned as \code{NA}.
#'
#' @details
#' This function evaluates intensities at two user-supplied points; it does not
#' search for local maxima. With \code{method = "nearest"}, a point exactly
#' halfway between two measured wavenumbers uses the lower wavenumber. Linear
#' interpolation is confined to the two adjacent measured points and never
#' extrapolates beyond the shared wavenumber axis.
#'
#' For an area-over-area ratio, calculate the numerator and denominator from
#' explicit ranges with \code{\link{area_under_band}()} and divide the two
#' returned vectors. Named area-under-band indices remain available through
#' that function's \code{index} argument.
#'
#' @examples
#' data("raman_hdpe")
#' peak_ratio(raman_hdpe, numerator = 2880, denominator = 2840)
#' peak_ratio(raman_hdpe, numerator = 2880, denominator = 2840,
#'            method = "linear")
#'
#' @seealso
#' \code{\link{area_under_band}()} for individual band areas and named
#' area-over-area indices.
#'
#' @export
peak_ratio <- function(x, ...) {
  UseMethod("peak_ratio")
}

#' @rdname peak_ratio
#'
#' @export
peak_ratio.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = FALSE)
}

#' @rdname peak_ratio
#'
#' @export
peak_ratio.OpenSpecy <- function(x, numerator, denominator,
                                 method = c("nearest", "linear"), ...) {
  x <- as_OpenSpecy(x)
  method <- match.arg(method)

  validate_point <- function(value, name) {
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L ||
        !is.finite(value)) {
      stop("'", name, "' must be a finite numeric scalar", call. = FALSE)
    }
    as.numeric(value)
  }

  numerator <- validate_point(numerator, "numerator")
  denominator <- validate_point(denominator, "denominator")
  wavenumber <- x$wavenumber

  if (!length(wavenumber) || any(!is.finite(wavenumber))) {
    stop("'x$wavenumber' must contain finite values", call. = FALSE)
  }
  if (anyDuplicated(wavenumber)) {
    stop("'x$wavenumber' must contain unique values", call. = FALSE)
  }

  spectrum_names <- colnames(x$spectra)
  named_na <- function() {
    stats::setNames(rep(NA_real_, ncol(x$spectra)), spectrum_names)
  }

  axis_range <- range(wavenumber)
  if (numerator < axis_range[1L] || numerator > axis_range[2L] ||
      denominator < axis_range[1L] || denominator > axis_range[2L]) {
    warning("The wavenumber axis does not cover both requested peak-ratio ",
            "points; returning NA", call. = FALSE)
    return(named_na())
  }

  ord <- order(wavenumber)
  wavenumber <- wavenumber[ord]
  spectra <- x$spectra[ord, , drop = FALSE]

  point_values <- function(point) {
    exact <- match(point, wavenumber)
    if (!is.na(exact)) {
      return(stats::setNames(as.numeric(spectra[exact, ]), spectrum_names))
    }

    left <- findInterval(point, wavenumber)
    right <- left + 1L
    if (identical(method, "nearest")) {
      # Strict inequality deliberately resolves midpoint ties to the lower
      # wavenumber.
      row <- if (abs(wavenumber[right] - point) <
                 abs(point - wavenumber[left])) right else left
      return(stats::setNames(as.numeric(spectra[row, ]), spectrum_names))
    }

    weight <- (point - wavenumber[left]) /
      (wavenumber[right] - wavenumber[left])
    values <- spectra[left, ] +
      (spectra[right, ] - spectra[left, ]) * weight
    stats::setNames(as.numeric(values), spectrum_names)
  }

  numerator_values <- point_values(numerator)
  denominator_values <- point_values(denominator)
  values <- numerator_values / denominator_values
  invalid <- !is.finite(numerator_values) |
    !is.finite(denominator_values) |
    denominator_values == 0 |
    !is.finite(values)

  if (any(invalid)) {
    warning("One or more peak ratios had a non-finite numerator, a zero or ",
            "non-finite denominator, or a non-finite result; returning NA ",
            "for those spectra", call. = FALSE)
    values[invalid] <- NA_real_
  }

  stats::setNames(as.numeric(values), spectrum_names)
}
