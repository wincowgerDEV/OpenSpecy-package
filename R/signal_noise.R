#' Calculate signal and noise metrics for Open Specy objects
#'
#' This function calculates common signal and noise metrics for Open Specy objects.
#'
#' @param x an Open Specy object.
#' @param return A character specifying the desired metric to calculate. Options include "signal" (mean intensity), "noise" (standard deviation of intensity), "signal_times_noise" (absolute value of signal times noise), "signal_over_noise" (absolute value of signal / noise), or "total_signal" (total signal = signal * number of data points).
#' @param na.rm Logical value indicating whether missing values should be removed when calculating signal and noise. Default is \code{TRUE}.
#'
#' @return A numeric vector containing the calculated metric for each spectrum in the Open Specy object.
#'
#' @examples
#' data("raman_hdpe")
#' signal_noise(raman_hdpe, return = "signal")
#' signal_noise(raman_hdpe, return = "noise")
#' signal_noise(raman_hdpe, return = "signal_times_noise")
#'
#' @importFrom stats median
#' @importFrom data.table frollapply
#'
#' @export
signal_noise <- function(x, return = "signal_over_noise", na.rm = TRUE) {

  vapply(x$spectra, function(intensity){
    if(length(intensity[!is.na(intensity)]) < 20){
      warning("Need at least 20 intensity values to calculate the signal or noise values accurately. Returning NA.")
        return(NA)
    }
    if(return == "run_signal_over_noise"){
      max = frollapply(intensity[!is.na(intensity)], 20, max)
      max[(length(max) - 19):length(max)] <- NA
      signal = max(max, na.rm = T)#/mean(x, na.rm = T)
      noise = median(max[max != 0], na.rm = T)
    }
    else{
      signal = mean(intensity, na.rm = na.rm)
      noise = sd(intensity, na.rm = na.rm)
    }
    if(return == "signal"){
      return(signal)
    }
    if(return == "noise"){
      return(noise)
    }
    if(return == "signal_times_noise"){
      return(abs(signal*noise))
    }
    if(return %in% c("signal_over_noise", "run_signal_over_noise")){
      return(abs(signal/noise))
    }
    if(return == "total_signal"){
      return(sum(intensity))
    }
    if(return == "log_total_signal"){
      return(sum(exp(intensity)))
    }
  }, FUN.VALUE = numeric(1))}
