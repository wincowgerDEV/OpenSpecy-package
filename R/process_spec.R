#' @title Preprocess Spectra
#' @rdname process_spec
#'
#' @description
#' \code{process_spec()} is a monolithic wrapper function for all spectral
#' preprocessing steps.
#'
#' @param x an \code{OpenSpecy} object.
#' @param active logical; indicating whether to perform preprocessing.
#' If \code{TRUE}, the preprocessing steps will be applied.
#' If \code{FALSE}, the original data will be returned.
#' @param adj_intens logical; describing whether to adjust the intensity units.
#' @param adj_intens_args named list of arguments passed to
#' \code{\link{smooth_intens}()}.
#' @param conform_spec Whether to conform the spectra to a new wavenumber range and resolution.
#' @param conform_spec_args named list of arguments passed to
#' \code{\link{conform_spec}()}.
#' @param restrict_range Logical value indicating whether to restrict the wavenumber range of the spectra.
#' @param restrict_range_args named list of arguments passed to
#' \code{\link{restrict_range}()}.
#' @param flatten_range Logical value indicating whether to flatten the range around the carbon dioxide region.
#' @param flatten_range_args named list of arguments passed to
#' \code{\link{flatten_range}()}.
#' @param smooth_intens Logical value indicating whether to apply a smoothing filter to the spectra.
#' @param smooth_intens_args named list of arguments passed to
#' \code{\link{smooth_intens}()}.
#' @param subtr_baseline Logical value indicating whether to subtract the baseline from the spectra.
#' @param subtr_baseline_args named list of arguments passed to
#' \code{\link{subtr_baseline}()}.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to subfunctions.
#'
#' @return
#' \code{process_spec()} returns an \code{OpenSpecy} object with preprocessed
#' spectra based on the specified parameters.
#'
#' @examples
#' data("raman_hdpe")
#' plot(raman_hdpe)
#'
#' # Process spectra with range restriction and baseline subtraction
#' process_spec(raman_hdpe,
#'              restrict_range = TRUE,
#'              restrict_range_args = list(min = 500, max = 3000),
#'              subtr_baseline = TRUE,
#'              subtr_baseline_args = list(type = "polynomial",
#'                                         polynomial = 8)) |>
#'   lines(col = "darkred")
#'
#' # Process spectra with smoothing and derivative
#' process_spec(raman_hdpe,
#'              smooth_intens = TRUE,
#'              smooth_intens_args = list(
#'                polynomial = 3,
#'                window = 11,
#'                derivative = 1
#'                )
#'              ) |>
#'   lines(col = "darkgreen")
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
process_spec.OpenSpecy <- function(x, active = TRUE,
                                   adj_intens = FALSE,
                                   adj_intens_args = list(
                                     type = "none"
                                     ),
                                   conform_spec = TRUE,
                                   conform_spec_args = list(
                                     range = NULL, res = 5
                                     ),
                                   restrict_range = FALSE,
                                   restrict_range_args = list(
                                     min = 0, max = 6000
                                     ),
                                   flatten_range = FALSE,
                                   flatten_range_args = list(
                                     min = 2200, max = 2420
                                     ),
                                   subtr_baseline = FALSE,
                                   subtr_baseline_args = list(
                                     type = "polynomial",
                                     degree = 8,
                                     raw = FALSE,
                                     baseline_wavenumber = NULL,
                                     baseline_intensity = NULL),
                                   smooth_intens = FALSE,
                                   smooth_intens_args = list(
                                     polynomial = 3,
                                     window = 11,
                                     derivative = 0,
                                     abs = T),
                                   make_rel = F,
                                   ...) {
  if(active) {
    if(adj_intens)
      x <- do.call("adj_intens", c(list(x, make_rel = F), adj_intens_args))
    if(conform_spec)
      x <- do.call("conform_spec", c(list(x), conform_spec_args))
    if(restrict_range)
      x <- do.call("restrict_range", c(list(x, make_rel = F),
                                       restrict_range_args))
    if(subtr_baseline)
      x <- do.call("subtr_baseline", c(list(x, make_rel = F),
                                       subtr_baseline_args))
    if(flatten_range)
      x <- do.call("flatten_range", c(list(x, make_rel = F),
                                      flatten_range_args))
    if(smooth_intens)
      x <- do.call("smooth_intens", c(list(x, make_rel = F),
                                      smooth_intens_args))
    if(make_rel)
      x$spectra <- x$spectra[, lapply(.SD, make_rel)]
  }

  return(x)
}
