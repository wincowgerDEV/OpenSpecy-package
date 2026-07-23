#' @rdname subtr_baseline
#' @title Automated background subtraction for spectral data
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using polynomial or Fill Peaks methods, or accepts a manual baseline.
#'
#' @details
#' This function supports \code{"polynomial"} automated baseline correction,
#' \code{"fill_peaks"} iterative local-window baseline suppression, and
#' \code{"manual"} subtraction of a user-provided baseline. Polynomial default
#' settings are closest to \code{"imodpoly"} for iterative polynomial
#' fitting based on Zhao et al. (2007). Additionally options recommended by
#' \code{"smodpoly"} for segmented iterative polynomial fitting with enhanced peak detection
#' from the S-Modpoly algorithm (\url{https://github.com/jackma123-rgb/S-Modpoly}).
#' Fill Peaks uses the implementation from the \pkg{baseline} package.
#'
#' @param x a list object of class \code{OpenSpecy} or a vector of wavenumbers.
#' @param y a vector of spectral intensities.
#' @param type one of \code{"polynomial"}, \code{"fill_peaks"}, or
#' \code{"manual"}, depending on the desired baseline correction method.
#' @param degree the degree of the full spectrum polynomial. Must be less than the number of
#' unique points when \code{raw} is \code{FALSE}. Typically, a good fit can be
#' found with an 8th order polynomial.
#' @param degree_part the degree of the polynomial for \code{"smodpoly"}. Must be less than the number of
#' unique points.
#' @param iterations the number of iterations for polynomial baseline
#' correction. For \code{type = "fill_peaks"}, this value is used only when
#' `it` is omitted.
#' @param peak_width_mult scaling factor for the width of peak detection regions.
#' @param termination_diff scaling factor for the ratio of difference in residual standard deviation to terminate iterative fitting with.
#' @param raw if \code{TRUE}, use raw and not orthogonal polynomials.
#' @param full logical, whether to use the full spectrum as in \code{"imodpoly"} or to partition as in \code{"smodpoly"}. 
#' @param remove_peaks logical, whether to remove peak regions during first iteration. 
#' @param refit_at_end logical, whether to refit a polynomial to the end result (TRUE) or to use linear approximation.
#' @param crop_boundaries logical, whether to smartly crop the boundaries to match the spectra based on peak proximity.
#' @param baseline an \code{OpenSpecy} object containing the baseline data to be
#' subtracted (only for \code{"manual"}).
#' @param bl_x a vector of wavenumbers for the baseline. 
#' @param bl_y a vector of spectral intensities for the baseline. 
#' @param make_rel logical; if \code{TRUE}, spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param lambda non-negative numeric scalar controlling the second-derivative
#' penalty for initial Fill Peaks smoothing.
#' @param hwi positive integer half-width, in subsampled buckets, of the initial
#' Fill Peaks suppression window.
#' @param it positive integer number of Fill Peaks suppression iterations. When
#' omitted, `iterations` is used.
#' @param int optional integer number of Fill Peaks subsampling buckets. If
#' `NULL`, it is derived as approximately one tenth of the spectrum length,
#' bounded between three and the number of spectral points.
#' @param \ldots further arguments passed to methods.
#'
#' @return
#' The `OpenSpecy` method returns an `OpenSpecy` object with corrected spectra,
#' the original shared wavenumber axis, aligned metadata, and existing
#' attributes. The default vector method returns a numeric vector of corrected
#' intensities.
#'
#' @examples
#' data("raman_hdpe")
#'
#' # Use polynomial
#' subtr_baseline(raman_hdpe, type = "polynomial", degree = 8)
#'
#' subtr_baseline(raman_hdpe, type = "polynomial", iterations = 5)
#'
#' # Use Fill Peaks with explicit tuning
#' subtr_baseline(raman_hdpe, type = "fill_peaks", lambda = 4,
#'                hwi = 50, it = 10, make_rel = FALSE)
#'
#' # Use manual
#' bl <- raman_hdpe
#' bl$spectra[, "intensity"] <- bl$spectra[, "intensity"] / 2
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
#' Liland KH (2015). 4S Peak Filling -- baseline estimation by iterative mean
#' suppression. \emph{MethodsX}, \strong{2}, 135--140.
#' \doi{10.1016/j.mex.2015.02.009}.
#'
#' @importFrom stats terms model.frame sd lm poly approx
#' @importFrom data.table .SD
#' @export
subtr_baseline <- function(x, ...) {
  UseMethod("subtr_baseline")
}

.fill_peaks_parameters <- function(n_points, lambda, hwi, it, int) {
    if (!is.numeric(n_points) || length(n_points) != 1L ||
        !is.finite(n_points) || n_points < 4) {
        stop("Fill Peaks requires at least four spectral points",
             call. = FALSE)
    }
    n_points <- as.integer(n_points)

    if (!is.numeric(lambda) || length(lambda) != 1L ||
        !is.finite(lambda) || lambda < 0) {
        stop("'lambda' must be a non-negative numeric scalar",
             call. = FALSE)
    }
    if (!is.numeric(hwi) || length(hwi) != 1L || !is.finite(hwi) ||
        hwi < 1 || hwi != as.integer(hwi)) {
        stop("'hwi' must be a positive integer", call. = FALSE)
    }
    if (!is.numeric(it) || length(it) != 1L || !is.finite(it) ||
        it < 1 || it != as.integer(it)) {
        stop("'it' must be a positive integer", call. = FALSE)
    }

    if (is.null(int)) {
        int <- min(n_points, max(3L, as.integer(round(n_points / 10))))
    } else if (!is.numeric(int) || length(int) != 1L || !is.finite(int) ||
               int < 3 || int != as.integer(int) || int > n_points) {
        stop("'int' must be an integer between three and the number of ",
             "spectral points", call. = FALSE)
    }

    list(
        lambda = as.numeric(lambda),
        hwi = as.integer(hwi),
        it = as.integer(it),
        int = as.integer(int)
    )
}

.fill_peaks_matrix <- function(spectra, lambda, hwi, it, int = NULL) {
    if (!is.matrix(spectra) || !is.numeric(spectra) || is.complex(spectra)) {
        stop("Fill Peaks requires a real numeric spectra matrix",
             call. = FALSE)
    }
    if (any(!is.finite(spectra))) {
        stop("Fill Peaks requires finite spectral intensities",
             call. = FALSE)
    }

    parameters <- .fill_peaks_parameters(
        ncol(spectra), lambda = lambda, hwi = hwi, it = it, int = int
    )
    corrected <- baseline::baseline.fillPeaks(
        spectra = spectra,
        lambda = parameters$lambda,
        hwi = parameters$hwi,
        it = parameters$it,
        int = parameters$int
    )$corrected
    dimnames(corrected) <- dimnames(spectra)
    corrected
}

#' @rdname subtr_baseline
#'
#' @export
subtr_baseline.default <- function(x,y,type = "polynomial",
                                   degree = 8, raw = FALSE,
                                   full = T,
                                   remove_peaks = T,
                                   refit_at_end = F,
                                   crop_boundaries = F,
                                   iterations = 10,  
                                   peak_width_mult = 3,
                                   termination_diff = 0.05,
                                   degree_part = 2, 
                                   bl_x = NULL, bl_y = NULL, make_rel = TRUE,
                                   lambda = 4, hwi = 50, it = 10,
                                   int = NULL, ...) {
    fill_it <- if (missing(it)) iterations else it
    type <- match.arg(type, c("polynomial", "fill_peaks", "manual"))

    if(type == "fill_peaks") {
        if (missing(y) || !is.numeric(y) || !is.null(dim(y)) ||
            !is.numeric(x) || !is.null(dim(x)) || length(x) != length(y)) {
            stop("Fill Peaks default method requires equal-length numeric ",
                 "vectors 'x' and 'y'", call. = FALSE)
        }
        spectra <- matrix(as.numeric(y), nrow = 1L,
                          dimnames = list(NULL, names(y)))
        corrected <- .fill_peaks_matrix(
            spectra, lambda = lambda, hwi = hwi, it = fill_it, int = int
        )[1L, ]
        names(corrected) <- names(y)
        return(if (make_rel) make_rel(corrected) else corrected)
    }

    if(type == "manual"){
        corrected = y - approx(bl_x, bl_y, xout = x, rule = 2, method = "linear", ties = mean)$y
    }
    
    if(type == "polynomial"){
        xout <- x
        yin <- y
        it = 1
        dev_prev <- 0 # standard deviation residuals for the last iteration of polyfit;
        # set initially to 0
        criteria_met <- FALSE
        # Step 1: Initial polynomial fit
        pixels_number <- length(y)
        paramVector <- lm(y ~ stats::poly(x, degree = degree, raw = raw))
        mod_poly <- paramVector$fitted.values
        dev_curr <- sd(paramVector$residuals)
        # Step 2: Detect intersections
        left_intersection <- which(diff(mod_poly + dev_curr >= y) == -1)
        right_intersection <- which(diff(mod_poly + dev_curr <= y) == -1)
            
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
            
            # Step 3: Compute original width and peak locations
            #max peak width
            or_width <- max(right_intersection - left_intersection)
            #keeping because could be useful one day
            #peak location
            opeaks <- round((right_intersection + left_intersection) / 2)
            #height difference at peak locations
            #ipeaks <- y[opeaks] - setarray[opeaks]
            #max height difference
            #mpeaks <- max(ipeaks)
            
        if(remove_peaks){
            peaks <- y > mod_poly + dev_curr
            y <- y[!peaks]
            mod_poly <- mod_poly[!peaks]
            x <- x[!peaks]            
        }

        if(!full){
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
                    p <- lm(seg_y ~ poly(seg_x, order, raw = raw))
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
                fit_array <- fit_function(start = left_boundary, end = right_boundary, iter = iterations, spectrum = y, order = degree_part)
                adjusted_spectrum[left_boundary:right_boundary] <- fit_array
                
                left_boundary <- right_boundary + 1
                right_boundary <- min(left_boundary + step_size, pixels_number)
            }
        }
       
        else{
            while (!criteria_met) {
                # Predict the intensity using the polynomial of specified length
                paramVector <- lm(y ~ stats::poly(x, degree = degree, raw = raw))
                mod_poly <- paramVector$fitted.values
                dev_curr <- sd(paramVector$residuals)
                
                y <- pmin(mod_poly + dev_curr, y)
                
                # Test criteria
                if(!is.null(iterations)){
                    criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= termination_diff | it == iterations
                }
                else{
                    criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= termination_diff
                }
                
                # Update previous residual metric
                it = it + 1
                dev_prev <- dev_curr
            }
        }
        
        # Step 6: Final adjustment and fitting a new polynomial to the baseline to smooth
        if(refit_at_end){
            final_fit <- lm(y ~ poly(x, degree, raw = raw))
            final_baseline <- predict(final_fit, newdata = data.frame(x = xout))            
        }
        else{
            final_baseline <- approx(x, y, xout = xout, rule = 2, method = "linear",
                   ties = mean)[2] |>
                unlist() |>
                unname()
        }

        if(crop_boundaries){
            pixels_number <- length(final_baseline)
            # # Step 7: Correct boundaries peaks
            if (pixels_number - max(opeaks) >= or_width) {
                final_baseline[(max(opeaks) + floor(0.7 * or_width)):pixels_number] <- yin[(max(opeaks) + floor(0.7 * or_width)):pixels_number]
            }
            if (min(opeaks) >= or_width) {
                final_baseline[1:floor(0.3 * or_width)] <- yin[1:floor(0.3 * or_width)]
            }            
        }
        corrected = yin - final_baseline
    }

    
    if (make_rel) make_rel(corrected) else corrected
    
}

#' @rdname subtr_baseline
#'
#' @export
subtr_baseline.OpenSpecy <- function(x, type = "polynomial",
                                     degree = 8, raw = FALSE,
                                     full = T,
                                     remove_peaks = T,
                                     refit_at_end = F,
                                     crop_boundaries = F,
                                     iterations = 10,  
                                     peak_width_mult = 3,
                                     termination_diff = 0.05,
                                     degree_part = 2, 
                                     baseline = list(wavenumber = NULL, spectra = NULL),
                                     make_rel = TRUE,
                                     lambda = 4, hwi = 50, it = 10,
                                     int = NULL, ...) {
    fill_it <- if (missing(it)) iterations else it
    type <- match.arg(type, c("polynomial", "fill_peaks", "manual"))
    x <- as_OpenSpecy(x)
    if (type == "manual" & !is_OpenSpecy(baseline)) stop("'baseline' needs to be of class 'OpenSpecy'", call. = F)
    if (type == "manual") baseline <- as_OpenSpecy(baseline)
    bl_x <- if (type == "manual") baseline$wavenumber else NULL
    bl_y <- if (type == "manual") baseline$spectra[, 1L] else NULL

    if (type == "fill_peaks") {
        spectra_names <- colnames(x$spectra)
        corrected <- .fill_peaks_matrix(
            t(x$spectra), lambda = lambda, hwi = hwi, it = fill_it,
            int = int
        )
        x$spectra <- t(corrected)
        colnames(x$spectra) <- spectra_names
        if (make_rel) x$spectra <- make_rel(x$spectra)
        return(x)
    }

    if (type == "manual") {
        baseline_y <- approx(bl_x, bl_y, xout = x$wavenumber, rule = 2,
                             method = "linear", ties = mean)$y
        corrected <- x$spectra - baseline_y
        x$spectra <- if (make_rel) make_rel(corrected) else corrected
        return(x)
    }

    x$spectra <- .apply_spectra(x$spectra, function(y) {
        subtr_baseline(x = x$wavenumber,
                       y = y,
                       type = type,
                       degree = degree, 
                       raw = raw,
                       full = full,
                       remove_peaks = remove_peaks,
                       refit_at_end = refit_at_end,
                       crop_boundaries = crop_boundaries,
                       iterations = iterations,  
                       peak_width_mult = peak_width_mult,
                       termination_diff = termination_diff,
                       degree_part = degree_part, 
                       bl_x = bl_x, 
                       bl_y = bl_y, 
                       make_rel = make_rel)
    })
    
    x
}




