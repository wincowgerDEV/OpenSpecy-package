#' Calculate signal and noise metrics for Open Specy objects
#'
#' This function calculates common signal and noise metrics for Open Specy objects.
#'
#' @param object An Open Specy object.
#' @param return A character specifying the desired metric to calculate. Options include "signal" (mean intensity), "noise" (standard deviation of intensity), "signal_times_noise" (absolute value of signal times noise), "signal_over_noise" (absolute value of signal / noise), or "total_signal" (total signal = signal * number of data points).
#' @param na.rm Logical value indicating whether missing values should be removed when calculating signal and noise. Default is \code{TRUE}.
#'
#' @return A numeric vector containing the calculated metric for each spectrum in the Open Specy object.
#'
#' @examples
#' data("raman_hdpe")
#' signal_noise(os, return = "signal")
#' signal_noise(os, return = "noise")
#' signal_noise(os, return = "signal_times_noise")
#'
#' @importFrom stats mean sd
#'
#' @export
 signal_noise <- function(object, return = "signal_over_noise", na.rm = TRUE){
     
     vapply(object, function(intensity){
         signal = mean(intensity, na.rm = na.rm)
         noise = sd(intensity, na.rm = na.rm)
         if(return == "signal"){
             return(signal)
         }
         if(return == "noise"){
             return(noise)
         }
         if(return == "signal_times_noise"){
             return(abs(signal*noise))
         }
         if(return == "signal_over_noise"){
             return(abs(signal/noise))
         }
         if(return == "total_signal"){
             signal*length(intensity)
         }
    }, FUN.VALUE = numeric(1))}

 