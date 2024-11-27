#' @rdname subtr_baseline
#' @title Automated background subtraction for spectral data
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using polynomial fitting methods or accepts a manual baseline.
#'
#' @details
#' This function supports three types of baseline correction:
#' \code{"imodpoly"} for iterative polynomial fitting based on Zhao et al. (2007),
#' \code{"smodpoly"} for segmented iterative polynomial fitting with enhanced peak detection
#' from the S-Modpoly algorithm (\url{https://github.com/jackma123-rgb/S-Modpoly}),
#' and \code{"manual"} for applying a user-provided baseline.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param type one of \code{"imodpoly"}, \code{"smodpoly"}, or \code{"manual"} depending on
#' the desired baseline correction method.
#' @param degree the degree of the full spectrum polynomial for \code{"imodpoly"} and \code{"smodpoly"}. Must be less than the number of
#' unique points when \code{raw} is \code{FALSE}. Typically, a good fit can be
#' found with an 8th order polynomial.
#' @param degree_part the degree of the polynomial for \code{"smodpoly"}. Must be less than the number of
#' unique points.
#' @param iteration the number of iterations for \code{"smodpoly"} baseline correction.
#' @param peak_width_mult scaling factor for the width of peak detection regions in \code{"smodpoly"}.
#' @param peak_height_mult scaling factor for the height of peak detection regions in \code{"smodpoly"}.
#' @param raw if \code{TRUE}, use raw and not orthogonal polynomials for \code{"imodpoly"}.
#' @param baseline an \code{OpenSpecy} object containing the baseline data to be
#' subtracted (only for \code{"manual"}).
#' @param make_rel logical; if \code{TRUE}, spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to \code{\link[stats]{poly}()} or \code{"smodpoly"} parameters.
#'
#' @return
#' \code{subtr_baseline()} returns a data frame containing two columns named
#' \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' # Use imodpoly
#' subtr_baseline(raman_hdpe, type = "imodpoly", degree = 8)
#'
#' # Use smodpoly
#' subtr_baseline(raman_hdpe, type = "smodpoly", iteration = 5)
#'
#' # Use manual
#' bl <- raman_hdpe
#' bl$spectra$intensity <- bl$spectra$intensity / 2
#' subtr_baseline(raman_hdpe, type = "manual", baseline = bl)
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
#' Jackma123 (2023). S-Modpoly: Segmented modified polynomial fitting for spectral baseline correction.
#' \emph{GitHub Repository}. Retrieved from \url{https://github.com/jackma123-rgb/S-Modpoly}.
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
subtr_baseline.OpenSpecy <- function(x, type = "imodpoly",
                                     degree = 8, raw = FALSE,
                                     iteration = 10,  
                                     peak_width_mult = 3,
                                     peak_height_mult = 0.05,
                                     degree_part = 2, 
                                     baseline, make_rel = TRUE, ...) {
    if (type == "imodpoly") {
        sbg <- x$spectra[, lapply(.SD, .subtr_bl_poly, x = x$wavenumber,
                                  degree = degree, iteration = iteration, raw = raw, ...)]
    } else if (type == "manual") {
        if (!is_OpenSpecy(baseline))
            stop("'baseline' needs to be of class 'OpenSpecy'", call. = F)
        sbg <- x$spectra[, lapply(.SD, .subtr_bl_manual, x = x$wavenumber,
                                  bl_y = baseline$spectra[[1]],
                                  bl_x = baseline$wavenumber, ...)]
    } else if (type == "smodpoly") {
        sbg <- x$spectra[, lapply(.SD, .subtr_bl_smod, x = x$wavenumber,
                                  iteration = iteration, peak_width_mult = peak_width_mult, 
                                  peak_height_mult = peak_height_mult, 
                                  degree_full = degree, degree_part = degree_part, ...)]
    } else {
        stop("'type' must be either 'polynomial', 'manual', or 'smodpoly'", call. = F)
    }
    
    if (make_rel) x$spectra <- sbg[, lapply(.SD, make_rel)] else x$spectra <- sbg
    
    return(x)
}


.subtr_bl_poly <- function(y, x, degree, iteration, raw, ...) {
  xout <- x
  yin <- y
  dev_prev <- 0 # standard deviation residuals for the last iteration of polyfit;
  # set initially to 0
  criteria_met <- FALSE
  it = 1
  while (!criteria_met) {
    # Predict the intensity using the polynomial of specified length
    paramVector <- lm(y ~ stats::poly(x, degree = degree, raw = raw))
    residual <- paramVector$residuals
    mod_poly <- paramVector$fitted.values
    dev_curr <- sd(residual)

    # Remove peaks
    if (it == 1) {
      peaks <- y > mod_poly + dev_curr
      y <- y[!peaks]
      mod_poly <- mod_poly[!peaks]
      x <- x[!peaks]
    }

    y <- pmin(mod_poly + dev_curr, y)

    # Test criteria
    it = it + 1
    if(!is.null(iteration)){
        criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= 0.05 | it == iteration
    }
    else{
        criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= 0.05
    }

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

.subtr_bl_smod <- function(y, x, iteration, peak_width_mult, peak_height_mult, degree_full, degree_part) {
    # Step 1: Initial polynomial fit
    or_y = y
    or_x = x
    pixels_number <- length(y)
    p <- lm(y ~ poly(x, degree_full))
    setarray <- predict(p, newdata = data.frame(x = x))

    # Step 2: Detect intersections
    left_intersection <- which(diff(setarray >= y) == -1)
    right_intersection <- which(diff(setarray <= y) == -1)
    
    # Ensure boundaries are handled correctly
    if (length(right_intersection) == 0 || length(left_intersection) == 0) {
        left_intersection <- c(1, left_intersection)
        right_intersection <- c(right_intersection, pixels_number)
    }
    
    # Ensure boundaries are handled correctly
    if (min(right_intersection) < min(left_intersection)) {
        left_intersection <- c(1, left_intersection)
    }
    
    # Ensure boundaries are handled correctly
    if (max(right_intersection) < max(left_intersection)) {
        right_intersection <- c(right_intersection, pixels_number)
    }
    
    # Step 3: Compute original width and peaks
    #max peak width
    or_width <- max(right_intersection - left_intersection)
    #peak location
    opeaks <- round((right_intersection + left_intersection) / 2)
    #height difference at peak locations
    ipeaks <- y[opeaks] - setarray[opeaks]
    #max height difference
    mpeaks <- max(ipeaks)
    #actual peak locations
    peaks <- opeaks[ipeaks > peak_height_mult * mpeaks]
    
    dev_curr <- sd(p$residuals)
    
    # Remove peaks
    peak_points <- y > setarray + dev_curr
    y <- pmin(setarray + dev_curr, y)[!peak_points]
    x <- x[!peak_points]
    pixels_number <- length(x)
    # Step 4: Initialize boundaries for iterative fitting
    if(peak_width_mult * or_width <= degree_part) {
        step_size <- degree_part + 1 
    }
    else{
        step_size <- peak_width_mult * or_width
    }
    left_boundary <- 1
    right_boundary <- step_size
    adjusted_spectrum <- numeric(length(x))
    
    # Helper function for iterative fitting
    fit_function <- function(start, end, iter, spectrum, order) {
        seg_x <- seq(start, end)
        seg_y <- spectrum[start:end]
        
        if(length(seg_y) <= order) return(seg_y)
        
        for (i in seq_len(iter)) {
            p <- lm(seg_y ~ poly(seg_x, order))
            py_fit <- predict(p, newdata = data.frame(seg_x = seg_x))
            dev_curr <- sd(p$residuals)
            seg_y <- pmin(seg_y, py_fit + dev_curr)
        }
        
        seg_y
    }
    
    # Step 5: Iterative fitting
    while (left_boundary < pixels_number) {
        if (right_boundary >= pixels_number) {
            right_boundary <- pixels_number
        }

        # Fit the segment
        fit_array <- fit_function(start = left_boundary, end = right_boundary, iter = iteration, spectrum = y, order = degree_part)
        adjusted_spectrum[left_boundary:right_boundary] <- fit_array
        
        left_boundary <- right_boundary + 1
        right_boundary <- min(left_boundary + step_size, pixels_number)
    }
    
    # Step 6: Final adjustment and fitting a new polynomial to the baseline to smooth
    final_fit <- lm(adjusted_spectrum ~ poly(x, degree_full))
    final_baseline <- predict(final_fit, newdata = data.frame(x = or_x))
    
    # pixels_number <- length(final_baseline)
    
    # # Step 7: Correct boundaries peaks
    # if (pixels_number - max(peaks) >= or_width) {
    #     final_baseline[(max(peaks) + floor(0.5 * or_width)):pixels_number] <- or_y[(max(peaks) + floor(0.5 * or_width)):pixels_number]
    # }
    # if (min(peaks) >= or_width) {
    #     final_baseline[1:floor(0.5 * or_width)] <- or_y[1:floor(0.5 * or_width)]
    # }
    
    return(or_y - final_baseline)
}

