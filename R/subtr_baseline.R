#' @rdname subtr_baseline
#' @title Automated background subtraction for spectral data
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using a polynomial fitting or accepts a manual baseline.
#'
#' @details
#' This is a translation of Michael Stephen Chen's MATLAB code written for the
#' \code{imodpolyfit} routine from Zhao et al. 2007.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param type one of \code{"polynomial"} or \code{"manual"} depending on
#' whether you want spectra to be corrected with a manual baseline or with
#' polynomial baseline fitting.
#' @param degree the degree of the polynomial. Must be less than the number of
#' unique points when raw is \code{FALSE}. Typically a good fit can be
#' found with a 8th order polynomial.
#' @param raw if \code{TRUE}, use raw and not orthogonal polynomials.
#' @param baseline an \code{OpenSpecy} object containing the baseline data to be
#' subtracted.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to \code{\link[stats]{poly}()}.
#'
#' @return
#' \code{subtr_baseline()} returns a data frame containing two columns named
#' \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' subtr_baseline(raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[stats]{poly}()};
#' \code{\link{smooth_intens}()}
#'
#' @references
#' Chen MS (2020). Michaelstchen/ModPolyFit. \emph{MATLAB}.
#' Retrieved from \url{https://github.com/michaelstchen/modPolyFit}
#' (Original work published July 28, 2015)
#'
#' Zhao J, Lui H, McLean DI, Zeng H (2007). “Automated Autofluorescence
#' Background Subtraction Algorithm for Biomedical Raman Spectroscopy.”
#' \emph{Applied Spectroscopy}, \strong{61}(11), 1225–1232.
#' \doi{10.1366/000370207782597003}.
#'
#' @importFrom stats terms model.frame sd lm poly approx
#' @importFrom data.table .SD
#' @export
subtr_baseline <- function(x, ...) {
  UseMethod("subtr_baseline")
}

#' @rdname subtr_baseline
#'
#' @export
subtr_baseline.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname subtr_baseline
#'
#' @export
subtr_baseline.OpenSpecy <- function(x, type = "polynomial",
                                     degree = 8, raw = FALSE,
                                     baseline, make_rel = TRUE, ...) {
  if(type == "polynomial") {
    sbg <- x$spectra[, lapply(.SD, .subtr_bl_poly, x = x$wavenumber,
                              degree = degree, raw = raw, ...)]
  } else if(type == "manual") {
    if(!is_OpenSpecy(baseline))
      stop("'baseline' needs to be of class 'OpenSpecy'", call. = F)
    sbg <- x$spectra[, lapply(.SD, .subtr_bl_manual, x = x$wavenumber,
                              bl_y = baseline$spectra[[1]],
                              bl_x = baseline$wavenumber)]
  } else stop("'type' must be either 'polynomial' or 'manual'", call. = F)

  if (make_rel) x$spectra <- sbg[, lapply(.SD, make_rel)] else x$spectra <- sbg

  return(x)
}

.subtr_bl_poly <- function(y, x, degree, raw, ...) {
  xout <- x
  yin <- y
  dev_prev <- 0 # standard deviation residuals for the last iteration of polyfit;
  # set initially to 0
  first_iter <- TRUE
  criteria_met <- FALSE

  while (!criteria_met) {
    # Predict the intensity using the polynomial of specified length
    paramVector <- lm(y ~ stats::poly(x, degree = degree, raw = raw, ...))

    residual <- paramVector$residuals

    mod_poly <- paramVector$fitted.values

    dev_curr <- sd(residual)

    # Remove peaks
    if (first_iter) {
      peaks <- c()
      for (i in 1:length(y)) {
        if (y[i] > mod_poly[i] + dev_curr) {
          peaks <- c(peaks,i)
        }
      }
      y <- y[-peaks]
      mod_poly <- mod_poly[-peaks]
      x <- x[-peaks]
      first_iter <- FALSE
    }

    # Replace data with lower value if polynomial is lower
    for (j in 1:length(y)) {
      if (mod_poly[j] + dev_curr > y[j]) {
        y[j] <- y[j]
      } else {
        y[j] <- mod_poly[j]
      }
    }

    # Test criteria
    criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= 0.05

    # Approximate the intensity back to the original wavelengths, allows below
    # the peak to be interpolated
    if(criteria_met) {
      ysbg <- approx(x, y, xout = xout, rule = 2, method = "linear",
                     ties = mean)[2] |>
        unlist() |>
        unname()

      return(yin - ysbg)
    }

    # Update previous residual metric
    dev_prev <- dev_curr
  }
}

.subtr_bl_manual <- function(y, x, bl_y, bl_x, ...) {
  y - approx(bl_x, bl_y, xout = x, rule = 2, method = "linear", ties = mean)$y
}
