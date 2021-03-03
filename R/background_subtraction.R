#' @title Automated background subtraction for Raman spectroscopy
#'
#' @description
#' This baseline correction routine iteratively finds the baseline of a spectrum
#' using a polynomial fitting.
#'
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
#' data("raman_pe")
#' background_subtraction(absorbance ~ wavenumber, data = raman_pe)
#'
#' @importFrom magrittr %>%
#' @importFrom stats terms model.frame sd lm poly approx
#' @export
background_subtraction <- function(formula, data = NULL, degree = 8, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  # Collate data
  mf <- model.frame(formula, data)
  ab <- mf[, 1L]
  wn <- mf[, 2L]

  dev_prev <- 0 # standard deviation residuals for the last iteration of polyfit;
                # set initially to 0
  first_iter <- TRUE
  criteria_met <- FALSE

  while (!criteria_met) {

    # Predict the intensity using the polynomial of specified length
    paramVector <- lm(ab ~ stats::poly(wn, degree = degree, raw = TRUE))

    residual <- paramVector$residuals

    mod_poly <- paramVector$fitted.values

    dev_curr <- sd(residual)

    # Remove peaks
    if (first_iter) {
      peaks <- c()
      for (i in 1:length(ab)) {
        if (ab[i] > mod_poly[i] + dev_curr) {
          peaks <- c(peaks,i)
        }
      }
      ab <- ab[-peaks]
      mod_poly <- mod_poly[-peaks]
      wn <- wn[-peaks]
      first_iter <- FALSE
    }

    # Replace data with lower value if polynomial is lower
    for (j in 1:length(ab)) {
      if (mod_poly[j] + dev_curr > ab[j]) {
        ab[j] = ab[j]
      } else {
        ab[j] = mod_poly[j]
      }
    }

    # Test criteria
    criteria_met <- abs((dev_curr - dev_prev) / dev_curr) <= 0.05

    # Approximate the intensity back to the original wavelengths, allows below
    # the peak to be interpolated
    if(criteria_met) {
      mf[, 1L] <- approx(wn, ab, xout = mf[, 2L], rule = 2, method = "linear",
                         ties = mean)[2] %>%
        unlist() %>%
        unname()
      return(mf)
    }

    # Update previous residual metric
    dev_prev <- dev_curr
  }
}
