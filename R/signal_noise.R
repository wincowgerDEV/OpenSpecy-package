#' @rdname signal_noise
#' @title Calculate signal and noise metrics for OpenSpecy objects
#'
#' @description
#' This function calculates common signal and noise metrics for \code{OpenSpecy}
#' objects.
#'
#' @param x an \code{OpenSpecy} object.
#' @param metric character; specifying the desired metric to calculate.
#' Options include \code{"signal"} (mean intensity), \code{"noise"} (standard
#' deviation of intensity), \code{"signal_times_noise"} (absolute value of
#' signal times noise), \code{"signal_over_noise"} (absolute value of signal /
#' noise), or \code{"total_signal"} (total signal = signal * number of data
#' points).
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating signal and noise. Default is \code{TRUE}.
#' @param \ldots further arguments passed to subfunctions; currently not used.
#'
#' @return
#' A numeric vector containing the calculated metric for each spectrum in the
#' \code{OpenSpecy} object.
#'
#' @examples
#' data("raman_hdpe")
#'
#' signal_noise(raman_hdpe, metric = "signal")
#' signal_noise(raman_hdpe, metric = "noise")
#' signal_noise(raman_hdpe, metric = "signal_times_noise")
#'
#' @importFrom stats median
#' @importFrom data.table frollapply
#'
#' @export
signal_noise <- function(x, ...) {
  UseMethod("signal_noise")
}

#' @rdname signal_noise
#'
#' @export
signal_noise.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname signal_noise
#'
#' @export
signal_noise.OpenSpecy <- function(x, metric = "signal_over_noise",
                                   na.rm = TRUE, ...) {
  vapply(x$spectra, function(y) {
    if(length(y[!is.na(y)]) < 20) {
      warning("Need at least 20 intensity values to calculate the signal or ",
              "noise values accurately; returning NA", call. = F)
      return(NA)
    }

    if(metric == "run_signal_over_noise") {
      max <- frollapply(y[!is.na(y)], 20, max)
      max[(length(max) - 19):length(max)] <- NA
      signal <- max(max, na.rm = T)#/mean(x, na.rm = T)
      noise <- median(max[max != 0], na.rm = T)
    }
    else{
      signal = mean(y, na.rm = na.rm)
      noise = sd(y, na.rm = na.rm)
    }
    if(metric == "signal"){
      return(signal)
    }
    if(metric == "noise"){
      return(noise)
    }
    if(metric == "signal_times_noise"){
      return(abs(signal*noise))
    }
    if(metric %in% c("signal_over_noise", "run_signal_over_noise")){
      return(abs(signal/noise))
    }
    if(metric == "total_signal"){
      return(sum(y))
    }
    if(metric == "log_total_signal"){
      return(sum(exp(y)))
    }
  }, FUN.VALUE = numeric(1))
}
