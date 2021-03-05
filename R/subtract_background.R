#' @title Automated background subtraction for spectral data
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using a polynomial fitting.
#'
#' @param x wavenumber
#' @param y absorbance
#' @param formula formula
#' @param data data
#' @param degree the degree of the polynomial. Must be less than the number of
#' unique points when raw is false, as by default.
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' data("raman_hdpe")
#' subtract_background(absorbance ~ wavenumber, data = raman_hdpe)
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
  names(lst) <- c("x", "y")

  do.call("subtract_background", c(lst, list(...)))
}

#' @rdname subtract_background
#'
#' @export
subtract_background.default <- function(x, y, degree = 8, ...) {
  xin <- x
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
        y[j] = y[j]
      } else {
        y[j] = mod_poly[j]
      }
    }

    # Test criteria
    criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= 0.05

    # Approximate the intensity back to the original wavelengths, allows below
    # the peak to be interpolated
    if(criteria_met) {
      yout <- approx(x, y, xout = xin, rule = 2, method = "linear",
                         ties = mean)[2] %>%
        unlist() %>%
        unname()
      return(data.frame(wavenumber = xin, absorbance = yout))
    }

    # Update previous residual metric
    dev_prev <- dev_curr
  }
}
