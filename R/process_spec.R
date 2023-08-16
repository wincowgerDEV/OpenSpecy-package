#' @title Preprocess Spectra
#' @rdname process_spec
#'
#' @description
#' Process spectra data by applying various preprocessing steps. This is a monolithic function for all common preprocessing steps in one place.
#'
#' @details
#' \code{sample_spec()} samples spectra from an \code{OpenSpecy} object.
#'
#' @param x An \code{OpenSpecy} object containing metadata and spectral data.
#' @param active_processing Logical value indicating whether to perform preprocessing. If \code{TRUE}, the preprocessing steps will be applied. If \code{FALSE}, the original data will be returned.
#' @param adj_intensity_decision A Logical describing whether to adjust the intensity units.
#' @param type Type of intensity adjustment to use. Can be one of "none", "transmittance", or "reflectance".
#' @param conform_decision Whether to conform the spectra to a new wavenumber range and resolution.
#' @param new_wavenumbers A new range to conform to, will use the min and max of all values.
#' @param res The resoltion for the conforming the spectra.
#' @param range_decision Logical value indicating whether to restrict the wavenumber range of the spectra.
#' @param min_range Numeric value specifying the minimum wavenumber for range restriction.
#' @param max_range Numeric value specifying the maximum wavenumber for range restriction.
#' @param flatten_decision Logical value indicating whether to flatten the range around the carbon dioxide region.
#' @param flatten_min Numeric value specifying the minimum wavenumber for the carbon dioxide region.
#' @param flatten_max Numeric value specifying the maximum wavenumber for the carbon dioxide region.
#' @param smooth_decision Logical value indicating whether to apply a smoothing filter to the spectra.
#' @param smooth_polynomial Integer value specifying the polynomial order for smoothing.
#' @param smooth_window Integer value specifying the window size for smoothing.
#' @param baseline_decision Logical value indicating whether to subtract the baseline from the spectra.
#' @param baseline_selection Character value specifying the type of baseline subtraction method. Options are "polynomial" or "manual".
#' @param raw_baseline Logical value indicating whether to use the raw baseline values for subtraction.
#' @param baseline_polynomial Integer value specifying the polynomial order for baseline subtraction.
#' @param baseline_wavenumber Numeric vector of wavenumbers used for baseline fitting.
#' @param baseline_intensity Numeric vector of intensities used for baseline fitting.
#' @param derivative_decision Logical value indicating whether to apply derivative to the spectra.
#' @param derivative_order Integer value specifying the order of the derivative.
#' @param derivative_polynomial Integer value specifying the polynomial order for derivative calculation.
#' @param abs Logical value indicating whether to calculate the absolute values of the derivative.
#' @param derivative_window Integer value specifying the window size for derivative calculation.
#' @param \ldots further arguments passed to subfunctions.
#'
#' @return
#' \code{process_spec()} returns an \code{OpenSpecy} object with preprocessed
#' spectra based on the specified parameters.
#' \code{sample_spec()} returns an \code{OpenSpecy} object with a subset of the spectra.
#'
#' @examples
#' data("raman_hdpe")
#' plot(raman_hdpe)
#'
#' # Process spectra with range restriction and baseline subtraction
#' process_spec(raman_hdpe,
#'              active_processing = TRUE,
#'              range_decision = TRUE,
#'              min_range = 500,
#'              max_range = 3000,
#'              baseline_decision = TRUE,
#'              baseline_selection = "polynomial",
#'              baseline_polynomial = 8,
#'              derivative_decision = FALSE) |>
#'   lines(col = "darkred")
#'
#' # Process spectra with smoothing and derivative
#' process_spec(raman_hdpe,
#'              active_processing = TRUE,
#'              smooth_decision = TRUE,
#'              smooth_polynomial = 3,
#'              smooth_window = 11,
#'              derivative_decision = TRUE,
#'              derivative_order = 1,
#'              derivative_polynomial = 3,
#'              derivative_window = 11) |>
#'   lines(col = "darkgreen")
#'
#' # Sampling a spectrum
#' tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
#' sampled <- sample_spec(tiny_map, size = 3)
#' print(sampled)
#' plot(sampled)
#'
#' @export
process_spec <- function(x, ...) {
  UseMethod("process_spec")
}

#' @rdname process_spec
#'
#' @export
process_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname process_spec
#'
#' @export
process_spec.OpenSpecy <- function(x,
                                   active_processing = T,
                                   adj_intensity_decision = F,
                                   type = "none",
                                   conform_decision = T,
                                   new_wavenumbers = NULL,
                                   res = 5,
                                   range_decision = F,
                                   min_range = 0,
                                   max_range = 6000,
                                   flatten_decision = F,
                                   flatten_min = 2200,
                                   flatten_max = 2420,
                                   smooth_decision = F,
                                   smooth_polynomial = 3,
                                   smooth_window = 11,
                                   baseline_decision = F,
                                   baseline_selection = "polynomial",
                                   raw_baseline = F,
                                   baseline_polynomial = 8,
                                   baseline_wavenumber = NULL,
                                   baseline_intensity = NULL,
                                   derivative_decision = T,
                                   derivative_order = 1,
                                   derivative_polynomial = 3,
                                   abs = T,
                                   derivative_window = 11, ...) {
  if(active_processing) {
    if(adj_intensity_decision)
      x <- adj_intens(x, type = type, make_rel = F)
    if(conform_decision)
      x <- conform_spec(x, range = new_wavenumbers, res = res)
    if(range_decision)
      x <- restrict_range(x, min_range = min_range, max_range = max_range,
                          make_rel = F)
    if(baseline_decision)
      x <- subtr_baseline(x, degree = baseline_polynomial,
                          baseline_wavenumber = baseline_wavenumber,
                          baseline_intensity = baseline_intensity, raw = raw_baseline,
                          make_rel = F, type = baseline_selection)
    if(flatten_decision)
      x <- flatten_range(x, min_range = flatten_min, max_range = flatten_max,
                         make_rel = F)
    if(smooth_decision)
      x <- smooth_intens(x, p = smooth_polynomial, n = smooth_window, m = 0,
                         make_rel = F)
    if(derivative_decision)
      x <- smooth_intens(x, p = derivative_polynomial, n = derivative_window,
                         m = derivative_order, abs = abs, make_rel = F)
  }

  return(x)
}

#' @rdname process_spec
#'
#' @export
sample_spec <- function(x, ...) {
  UseMethod("sample_spec")
}

#' @rdname process_spec
#'
#' @export
sample_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname process_spec
#'
#' @export
sample_spec.OpenSpecy <- function(x, ...) {
  # replace = false is mandatory currently because we don't have a way to
  # rename and recoordinate duplicates.
  cols <- sample(1:ncol(x$spectra), ...)

  as_OpenSpecy(
    x = x$wavenumber,
    spectra = x$spectra[, cols, with = F],
    metadata = x$metadata[cols, ]
  )
}
