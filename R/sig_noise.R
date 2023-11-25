#' @rdname sig_noise
#' @title Calculate signal and noise metrics for OpenSpecy objects
#'
#' @description
#' This function calculates common signal and noise metrics for \code{OpenSpecy}
#' objects.
#'
#' @param x an \code{OpenSpecy} object.
#' @param metric character; specifying the desired metric to calculate.
#' @param step numeric; the step size of the region to look for the run_sig_over_noise option.
#' @param sig_min numeric; the minimum wavenumber value for the signal region.
#' @param sig_max numeric; the maximum wavenumber value for the signal region.
#' @param noise_min numeric; the minimum wavenumber value for the noise region.
#' @param noise_max numeric; the maximum wavenumber value for the noise region.
#' @param abs logical; whether to return the absolute value of the result
#' Options include \code{"sig"} (mean intensity), \code{"noise"} (standard
#' deviation of intensity), \code{"sig_times_noise"} (absolute value of
#' signal times noise), \code{"sig_over_noise"} (absolute value of signal /
#' noise), \code{"run_sig_over_noise"} (absolute value of signal /
#' noise where signal is estimated as the max intensity and noise is
#' estimated as the height of a low intensity region.),
#' \code{"log_tot_sig"} (sum of the inverse log intensities, useful for spectra  in log units),
#' or \code{"tot_sig"} (sum of intensities).
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating signal and noise. Default is \code{TRUE}.
#' @param \ldots further arguments passed to subfunctions; currently not used.
#'
#' @return
#' A numeric vector containing the calculated metric for each spectrum in the
#' \code{OpenSpecy} object.
#'
#' @seealso [restrict_range()]
#' @examples
#' data("raman_hdpe")
#'
#' sig_noise(raman_hdpe, metric = "sig")
#' sig_noise(raman_hdpe, metric = "noise")
#' sig_noise(raman_hdpe, metric = "sig_times_noise")
#'
#' @importFrom stats median
#' @importFrom data.table frollapply
#'
#' @export
sig_noise <- function(x, ...) {
  UseMethod("sig_noise")
}

#' @rdname sig_noise
#'
#' @export
sig_noise.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname sig_noise
#'
#' @export
sig_noise.OpenSpecy <- function(x, metric = "run_sig_over_noise",
                                na.rm = TRUE, step = 20,
                                sig_min = NULL, sig_max = NULL,
                                noise_min = NULL, noise_max = NULL, abs = T, ...) {

  values <- vapply(x$spectra, function(y) {
    if(metric == "run_sig_over_noise") {
      if(length(y[!is.na(y)]) < step) {
        warning(paste0("Need at least ", step, " intensity values to calculate ",
                       "the signal or noise values accurately with ",
                       "run_sig_over_noise; returning NA"), call. = F)
        return(NA)
      }
      max <- frollapply(y[!is.na(y)], step, max)
      max[(length(max) - (step-1)):length(max)] <- NA
      signal <- max(max, na.rm = T)
      noise <- median(max[max != 0], na.rm = T)
    } else {
      if(!is.null(sig_min) & !is.null(sig_max)){
        sig_intens <- y[x$wavenumber >= sig_min & x$wavenumber <= sig_max]
      } else {
        sig_intens <- y
      }
      if(!is.null(noise_min) & !is.null(noise_max)){
        noise_intens <- y[x$wavenumber >= noise_min & x$wavenumber <= noise_max]
      } else {
        noise_intens <- y
      }
      signal <- mean(sig_intens, na.rm = na.rm)
      noise <- sd(noise_intens, na.rm = na.rm)
    }

    if(metric == "sig") return(signal)
    if(metric == "noise") return(noise)
    if(metric == "sig_times_noise") return(signal * noise)

    if(metric %in% c("sig_over_noise", "run_sig_over_noise"))
      return(signal/noise)
    if(metric == "tot_sig") return(sum(y))
    if(metric == "log_tot_sig") return(sum(exp(y)))
  }, FUN.VALUE = numeric(1))

  if(abs) {
    return(abs(values))
  } else {
    return(values)
  }
}
