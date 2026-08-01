#' @rdname point_intensity
#' @title Measure spectral intensity at one wavenumber
#'
#' @description
#' Measures the intensity at one user-supplied wavenumber for every spectrum
#' in an \code{OpenSpecy} object.
#'
#' @param x an \code{OpenSpecy} object.
#' @param wavenumber a finite numeric scalar giving the wavenumber to measure.
#' @param method character; use \code{"nearest"} to select the nearest
#' measured point or \code{"linear"} to interpolate between adjacent measured
#' points.
#' @param \ldots additional arguments passed to methods.
#'
#' @return
#' A named numeric vector with one intensity per spectrum. Names and order
#' match the columns of \code{x$spectra}. A non-finite selected or interpolated
#' intensity is returned as \code{NA} for that spectrum. If the requested
#' wavenumber is outside the shared axis, all values are returned as \code{NA}.
#'
#' @details
#' This function measures a specified spectral point; it does not search for a
#' local maximum. With \code{method = "nearest"}, a point exactly halfway
#' between two measured wavenumbers uses the lower wavenumber. Linear
#' interpolation is confined to the two adjacent measured points and never
#' extrapolates beyond the shared wavenumber axis.
#'
#' @examples
#' data("raman_hdpe")
#' point_intensity(raman_hdpe, wavenumber = 2880)
#' point_intensity(raman_hdpe, wavenumber = 2880, method = "linear")
#'
#' @seealso
#' \code{\link{peak_ratio}()} for ratios between two point intensities and
#' \code{\link{area_under_band}()} for measurements over a spectral region.
#'
#' @export
point_intensity <- function(x, ...) {
  UseMethod("point_intensity")
}

#' @rdname point_intensity
#'
#' @export
point_intensity.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = FALSE)
}

.validate_spectral_point <- function(value, name) {
  if (!is.numeric(value) || is.complex(value) || length(value) != 1L ||
      !is.finite(value)) {
    stop("'", name, "' must be a finite numeric scalar", call. = FALSE)
  }
  as.numeric(value)
}

.spectral_point_values <- function(wavenumber, spectra, point, method,
                                   spectrum_names) {
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

#' @rdname point_intensity
#'
#' @export
point_intensity.OpenSpecy <- function(x, wavenumber,
                                      method = c("nearest", "linear"), ...) {
  x <- as_OpenSpecy(x)
  method <- match.arg(method)
  point <- .validate_spectral_point(wavenumber, "wavenumber")
  axis <- x$wavenumber

  if (!length(axis) || any(!is.finite(axis))) {
    stop("'x$wavenumber' must contain finite values", call. = FALSE)
  }
  if (anyDuplicated(axis)) {
    stop("'x$wavenumber' must contain unique values", call. = FALSE)
  }

  spectrum_names <- colnames(x$spectra)
  named_na <- function() {
    stats::setNames(rep(NA_real_, ncol(x$spectra)), spectrum_names)
  }
  axis_range <- range(axis)
  if (point < axis_range[1L] || point > axis_range[2L]) {
    warning("The wavenumber axis does not cover the requested point; ",
            "returning NA", call. = FALSE)
    return(named_na())
  }

  ord <- order(axis)
  axis <- axis[ord]
  spectra <- x$spectra[ord, , drop = FALSE]
  values <- .spectral_point_values(
    axis, spectra, point, method, spectrum_names
  )
  invalid <- !is.finite(values)
  if (any(invalid)) {
    warning("One or more point intensities were non-finite; returning NA ",
            "for those spectra", call. = FALSE)
    values[invalid] <- NA_real_
  }

  stats::setNames(as.numeric(values), spectrum_names)
}
