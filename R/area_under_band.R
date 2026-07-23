#' @rdname area_under_band
#' @title Measure the area under band of spectra
#'
#' @description
#' Area under the band calculations are useful for quantifying spectral features. 
#' Specialized fields in spectroscopy have different area under the band regions of interest
#' and their ratios that help with understanding differences in materials. Additional processing 
#' is typically required prior to calculating these values for accuracy and reproducibility.
#'
#' @param x an \code{OpenSpecy} object.
#' @param min a numeric value of the smallest wavenumber to begin calculation.
#' @param max a numeric value of the largest wavenumber to end calculation.
#' @param na.rm a logical value for whether to ignore NA values.
#' @param index an optional character scalar naming a predefined area-under-band
#' ratio. When supplied, `min` and `max` must both be omitted. Supported values
#' are `"carbonyl_index_saub"`, `"carbonyl_index_pe"`,
#' `"hydroxyl_index_pe"`, `"hydroxyl_index_pp"`,
#' `"carbon_oxygen_index_pe"`, and `"carbon_oxygen_index_pp"`.
#' @param \ldots additional arguments passed to methods.
#'
#' @return
#' A named numeric vector with one value per spectrum. Custom `min`/`max`
#' calculations return the inclusive sum of intensities in that band. Named
#' indices return the numerator-band sum divided by the denominator-band sum.
#' An index is returned as `NA` when the shared wavenumber axis does not fully
#' cover its required bands or its ratio is not finite.
#'
#' @details
#' Named indices define band ratios only; they do not apply baseline correction,
#' normalization, or other preprocessing. Compose those steps explicitly before
#' calling `area_under_band()`. The preset definitions and intended materials
#' are:
#'
#' - `carbonyl_index_saub`: 1650--1850 / 1420--1500 cm^-1; specified
#'   area-under-band carbonyl index used for polyethylene (PE) and
#'   polypropylene (PP).
#' - `carbonyl_index_pe`: 1700--1770 / 1423--1495 cm^-1; PE.
#' - `hydroxyl_index_pe`: 3021--3353 / 1467--1504 cm^-1; PE.
#' - `hydroxyl_index_pp`: 3300--3400 / 952--986 cm^-1; PP.
#' - `carbon_oxygen_index_pe`: 924--1197 / 2866--2987 cm^-1; PE.
#' - `carbon_oxygen_index_pp`: 1000--1200 / 2885--2940 cm^-1; PP.
#'
#' All bounds are inclusive. The function preserves signed intensities and does
#' not clip negative values.
#'
#' @examples
#' data("raman_hdpe")
#' #Single area calculation
#' area_under_band(raman_hdpe, min = 1000,max = 2000)
#' #Ratio of two areas. 
#' area_under_band(raman_hdpe, min = 1000,max = 2000)/area_under_band(raman_hdpe, min = 500,max = 700)
#' #A predefined ratio (requires spectra covering both bands)
#' area_under_band(raman_hdpe, index = "carbonyl_index_saub")
#' @author
#' Win Cowger
#'
#' @references
#' Almond J, Sugumaar P, Wenzel MN, Hill G, Wallis C (2020). Determination of
#' the carbonyl index of polyethylene and polypropylene using specified area
#' under band methodology with ATR-FTIR spectroscopy. *e-Polymers*, 20,
#' 369--381. \doi{10.1515/epoly-2020-0041}.
#'
#' Campanale C, Savino I, Massarelli C, Uricchio VF (2023). Fourier transform
#' infrared spectroscopy to assess the degree of alteration of artificially
#' aged and environmentally weathered microplastics. *Polymers*, 15, 911.
#' \doi{10.3390/polym15040911}.
#'
#' @importFrom data.table .SD
#' @export
area_under_band <- function(x, ...) {
  UseMethod("area_under_band")
}

#' @rdname area_under_band
#'
#' @export
area_under_band.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

.area_under_band_indices <- list(
  carbonyl_index_saub = list(
    numerator = c(1650, 1850),
    denominator = c(1420, 1500)
  ),
  carbonyl_index_pe = list(
    numerator = c(1700, 1770),
    denominator = c(1423, 1495)
  ),
  hydroxyl_index_pe = list(
    numerator = c(3021, 3353),
    denominator = c(1467, 1504)
  ),
  hydroxyl_index_pp = list(
    numerator = c(3300, 3400),
    denominator = c(952, 986)
  ),
  carbon_oxygen_index_pe = list(
    numerator = c(924, 1197),
    denominator = c(2866, 2987)
  ),
  carbon_oxygen_index_pp = list(
    numerator = c(1000, 1200),
    denominator = c(2885, 2940)
  )
)

#' @rdname area_under_band
#'
#' @export
area_under_band.OpenSpecy <- function(x, min = NULL, max = NULL,
                                      na.rm = FALSE, index = NULL, ...) {
  x <- as_OpenSpecy(x)

  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("'na.rm' must be TRUE or FALSE", call. = FALSE)
  }

  named_na <- function() {
    stats::setNames(rep(NA_real_, ncol(x$spectra)), colnames(x$spectra))
  }

  if (!is.null(index)) {
    if (!is.null(min) || !is.null(max)) {
      stop("'min' and 'max' must be omitted when 'index' is supplied",
           call. = FALSE)
    }
    if (!is.character(index) || length(index) != 1L || is.na(index)) {
      stop("'index' must be one supported character value", call. = FALSE)
    }
    if (!index %in% names(.area_under_band_indices)) {
      stop("Unsupported area-under-band index: ", index, call. = FALSE)
    }

    definition <- .area_under_band_indices[[index]]
    required <- range(c(definition$numerator, definition$denominator))
    axis_range <- range(x$wavenumber)
    numerator_rows <- x$wavenumber >= definition$numerator[1L] &
      x$wavenumber <= definition$numerator[2L]
    denominator_rows <- x$wavenumber >= definition$denominator[1L] &
      x$wavenumber <= definition$denominator[2L]

    if (axis_range[1L] > required[1L] || axis_range[2L] < required[2L] ||
        !any(numerator_rows) || !any(denominator_rows)) {
      warning("The wavenumber axis does not fully cover the bands required ",
              "for '", index, "'; returning NA", call. = FALSE)
      return(named_na())
    }

    numerator <- colSums(x$spectra[numerator_rows, , drop = FALSE],
                         na.rm = na.rm)
    denominator <- colSums(x$spectra[denominator_rows, , drop = FALSE],
                           na.rm = na.rm)
    values <- numerator / denominator
    invalid <- !is.finite(denominator) | denominator == 0 |
      !is.finite(values)
    if (any(invalid)) {
      warning("One or more '", index, "' ratios had a zero or non-finite ",
              "denominator or result; returning NA for those spectra",
              call. = FALSE)
      values[invalid] <- NA_real_
    }
    return(values)
  }

  if (is.null(min) || is.null(max)) {
    stop("'min' and 'max' are both required when 'index' is not supplied",
         call. = FALSE)
  }
  if (!is.numeric(min) || length(min) != 1L || !is.finite(min) ||
      !is.numeric(max) || length(max) != 1L || !is.finite(max) || min > max) {
    stop("'min' and 'max' must be finite numeric scalars with min <= max",
         call. = FALSE)
  }

  logic <- x$wavenumber >= min & x$wavenumber <= max
  colSums(x$spectra[logic, , drop = FALSE], na.rm = na.rm)
}
