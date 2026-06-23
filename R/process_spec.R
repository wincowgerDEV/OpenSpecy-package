#' @rdname process_spec
#' @title Process Spectra
#'
#' @description
#' \code{process_spec()} is a monolithic wrapper function for all spectral
#' processing steps. Intensity operations automatically use
#' \code{\link{manage_na}()} when spectra contain missing values and run the
#' underlying functions directly otherwise. Processing attributes are updated
#' automatically.
#'
#' @param x an \code{OpenSpecy} object.
#' @param active logical; indicating whether to perform processing.
#' If \code{TRUE}, the processing steps will be applied.
#' If \code{FALSE}, the original data will be returned.
#' @param adj_intens logical; describing whether to adjust the intensity units.
#' @param adj_intens_args named list of arguments passed to
#' \code{\link{adj_intens}()}.
#' @param conform_spec logical; whether to conform the spectra to a new
#' wavenumber range and resolution.
#' @param conform_spec_args named list of arguments passed to
#' \code{\link{conform_spec}()}.
#' @param restrict_range logical; indicating whether to restrict the wavenumber
#' range of the spectra.
#' @param restrict_range_args named list of arguments passed to
#' \code{\link{restrict_range}()}.
#' @param flatten_range logical; indicating whether to flatten the range around
#' the carbon dioxide region.
#' @param flatten_range_args named list of arguments passed to
#' \code{\link{flatten_range}()}.
#' @param smooth_intens logical; indicating whether to apply a smoothing filter
#' to the spectra.
#' @param smooth_intens_args named list of arguments passed to
#' \code{\link{smooth_intens}()}.
#' @param subtr_baseline logical; indicating whether to subtract the baseline
#' from the spectra.
#' @param subtr_baseline_args named list of arguments passed to
#' \code{\link{subtr_baseline}()}.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param make_rel_args named list of arguments passed to
#' \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to subfunctions.
#'
#' @return
#' \code{process_spec()} returns an \code{OpenSpecy} object with processed
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
#'   plot()
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
#'   plot()
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
                                     range = NULL, res = 5, type = "interp"
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
                                     type = "polynomial", degree = 8,
                                     raw = FALSE, baseline = NULL),
                                   smooth_intens = TRUE,
                                   smooth_intens_args = list(
                                     polynomial = 3, window = 11,
                                     derivative = 1, abs = TRUE),
                                   make_rel = TRUE,
                                   make_rel_args = list(
                                       na.rm = TRUE),
                                   ...) {
  x <- as_OpenSpecy(x)

  apply_intensity_step <- function(x, fun, args = list()) {
    .process_intensity_step(x, fun, args)
  }

  if(active) {
    if(adj_intens) {
      args <- utils::modifyList(list(make_rel = FALSE), adj_intens_args)
      x <- apply_intensity_step(x, "adj_intens", args)
      intensity_type <- if (is.null(args$type)) "none" else args$type
      if (!identical(intensity_type, "none")) {
        attr(x, "intensity_unit") <- "absorbance"
      }
    }
    if(conform_spec)
      x <- do.call("conform_spec", c(list(x), conform_spec_args))
    if(restrict_range)
      x <- do.call("restrict_range",
                   c(list(x), utils::modifyList(list(make_rel = FALSE),
                                                restrict_range_args)))
    if(flatten_range) {
      args <- utils::modifyList(list(make_rel = FALSE), flatten_range_args)
      x <- apply_intensity_step(x, "flatten_range", args)
    }
    if(subtr_baseline) {
      args <- utils::modifyList(list(make_rel = FALSE), subtr_baseline_args)
      x <- apply_intensity_step(x, "subtr_baseline", args)
      attr(x, "baseline") <- "nobaseline"
    }
    if(smooth_intens) {
      args <- utils::modifyList(list(make_rel = FALSE), smooth_intens_args)
      x <- apply_intensity_step(x, "smooth_intens", args)
      derivative <- if (is.null(args$derivative)) 1 else args$derivative
      attr(x, "derivative_order") <- as.character(derivative)
    }
    if(make_rel) {
      x <- apply_intensity_step(x, "make_rel", make_rel_args)
    }
  }

  return(x)
}

.process_intensity_step <- function(x, fun, args = list()) {
  na_cols <- colSums(is.na(x$spectra)) > 0L
  if (!any(na_cols)) {
    return(do.call(fun, c(list(x), args)))
  }

  step <- function(value) {
    do.call(fun, c(list(value), args))
  }
  if (all(na_cols)) {
    return(manage_na(x, fun = step))
  }

  out <- x
  complete_cols <- which(!na_cols)
  missing_cols <- which(na_cols)

  complete <- .subset_open_specy_columns(x, complete_cols)
  complete <- do.call(fun, c(list(complete), args))
  .check_intensity_step_shape(complete, x, complete_cols)
  out$spectra[, complete_cols] <- complete$spectra
  out <- .copy_open_specy_attributes(out, complete)

  missing <- .subset_open_specy_columns(x, missing_cols)
  missing <- manage_na(missing, fun = step)
  .check_intensity_step_shape(missing, x, missing_cols)
  out$spectra[, missing_cols] <- missing$spectra
  out <- .copy_open_specy_attributes(out, missing)

  out
}

.subset_open_specy_columns <- function(x, cols) {
  out <- x
  out$spectra <- x$spectra[, cols, drop = FALSE]
  out$metadata <- data.table::copy(x$metadata[cols, ])
  out
}

.check_intensity_step_shape <- function(processed, original, cols) {
  if (!identical(processed$wavenumber, original$wavenumber) ||
      !identical(dim(processed$spectra),
                 c(length(original$wavenumber), length(cols)))) {
    stop("Intensity processing steps must preserve the wavenumber axis and ",
         "spectra dimensions", call. = FALSE)
  }
  invisible(NULL)
}
