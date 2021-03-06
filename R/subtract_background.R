#' @title Automated background subtraction for spectral data
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using a polynomial fitting.
#'
#' @details
#' This is an translation of a matlab code written for the imodpolyfit routine from Zhao et a. 2007.
#'
#' @param x wavenumber column
#' @param y intensity column
#' @param make_relative Data Normalization
#' @param formula formula
#' @param data Dataset to subtract background from.
#' @param degree the degree of the polynomial. Must be less than the number of
#' unique points when raw is false, as by default. Typically a good fit can be found with a 8th order polynomial.
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' data("raman_hdpe")
#' subtract_background(intensity ~ wavenumber, data = raman_hdpe)
#'
#' @importFrom magrittr %>%
#' @importFrom stats terms model.frame sd lm poly approx
#' @export
subtract_background <- function(x, ...) {
  UseMethod("subtract_background")
}

#' @rdname subtract_background
#'
#' @export
subtract_background.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("subtract_background", c(lst, list(...)))
}

#' @rdname subtract_background
#'
#' @export
subtract_background.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'")

  do.call("subtract_background", list(x$wavenumber, x$intensity, ...))
}

#' @rdname subtract_background
#'
#' @export
subtract_background.default <- function(x, y, degree = 8, make_relative = TRUE,
                                        ...) {
  xin <- x
  yin <- y
  dev_prev <- 0 # standard deviation residuals for the last iteration of polyfit;
                # set initially to 0
  first_iter <- TRUE
  criteria_met <- FALSE

  while (!criteria_met) {
    # Predict the intensity using the polynomial of specified length
    paramVector <- lm(y ~ stats::poly(x, degree = degree, raw = TRUE))

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
      ysbg <- approx(x, y, xout = xin, rule = 2, method = "linear",
                         ties = mean)[2] %>%
        unlist() %>%
        unname()

      if (make_relative) yout <- make_relative(yin - ysbg) else yout <- yin - ysbg

      return(data.frame(wavenumber = xin, intensity = yout))
    }

    # Update previous residual metric
    dev_prev <- dev_curr
  }
}
