#' @rdname smooth_intens
#' @title Smooth spectral intensities
#'
#' @description
#' This smoother can enhance the signal to noise ratio of the data using a
#' Savitzky-Golay or Whittaker-Henderson filter.
#'
#' @details
#' For Savitzky-Golay this uses OpenSpecy's internal matrix filter to improve
#' integration with other OpenSpecy functions.
#' A typical good smooth can be achieved with 11 data point window and a 3rd or
#' 4th order polynomial.
#' For Whittaker-Henderson, the code is largely based off of the whittaker() function in the pracma package.
#' In general Whittaker-Henderson is expected to be slower but more robust than Savitzky-Golay.
#'
#' @param x an object of class \code{OpenSpecy} or vector for \code{calc_window_points()}.
#' @param polynomial polynomial order for the filter
#' @param window number of data points in the window, filter length (must be
#' odd).
#' @param wavenum_width the width of the window you want in wavenumbers.
#' @param derivative the derivative order if you want to calculate the
#' derivative. Zero (default) is no derivative.
#' @param abs logical; whether you want to calculate the absolute value of the
#' resulting output.
#' @param lambda smoothing parameter for Whittaker-Henderson smoothing, 50 results in rough smoothing and 10^4 results in a high level of smoothing.
#' @param d order of differences to use for Whittaker-Henderson smoothing, typically set to 2.
#' @param lag the lag to use for the numeric derivative calculation if using Whittaker-Henderson. Greater values lead to smoother derivatives, 1 or 2 is common.
#' @param type the type of smoothing to use "wh" for Whittaker-Henerson or "sg" for Savitzky-Golay.
#' @param make_rel logical; if \code{TRUE} spectra are automatically normalized
#' with \code{\link{make_rel}()}.
#' @param \ldots further arguments passed to the Savitzky-Golay coefficient
#' generator, currently including \code{ts}.
#'
#' @return
#' \code{smooth_intens()} returns an \code{OpenSpecy} object.
#'
#' \code{calc_window_points()} returns a single numberic vector object of the
#' number of points needed to fill the window and can be passed to \code{smooth_intens()}.
#' For many applications, this is more reusable than specifying a static number of points.
#'
#' @examples
#' data("raman_hdpe")
#'
#' smooth_intens(raman_hdpe)
#'
#' smooth_intens(raman_hdpe, window = calc_window_points(x = raman_hdpe, wavenum_width = 70))
#'
#' smooth_intens(raman_hdpe, lambda = 1600, d = 2, lag = 2, type = "wh")
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{smooth_intens}()}
#'
#' @references
#' Savitzky A, Golay MJ (1964). “Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures.” \emph{Analytical Chemistry},
#' \strong{36}(8), 1627--1639.
#'
#' @importFrom data.table .SD
#' @export
smooth_intens <- function(x, ...) {
  UseMethod("smooth_intens")
}

#' @rdname smooth_intens
#'
#' @export
smooth_intens.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname smooth_intens
#'
#' @export
smooth_intens.OpenSpecy <- function(x, polynomial = 3,
                                    window = 11,
                                    derivative = 1,
                                    abs = TRUE,
                                    lambda = 1600,
                                    d = 2,
                                    type = "sg",
                                    lag = 2,
                                    make_rel = TRUE, ...) {
  x <- as_OpenSpecy(x)

  if(type == "sg") {
    filt <- .sgfilt_matrix(x$spectra, p = polynomial, n = window,
                           m = derivative, ...)
  } else if(type == "wh") {
    filt <- .whittaker_matrix(
      .derivative_matrix(x$spectra,
                         res = spec_res(x),
                         derivative = derivative,
                         lag = lag),
      d = d,
      lambda = lambda
    )
  } else {
    stop("type must be one of 'wh' or 'sg'")
  }

  if(abs) filt <- abs(filt)
  if(make_rel) x$spectra <- make_rel(filt) else x$spectra <- filt

  return(x)
}

#' @rdname smooth_intens
#'
#' @export
calc_window_points <- function(x, ...) {
  UseMethod("calc_window_points")
}

#' @rdname smooth_intens
#'
#' @export
calc_window_points.default <- function(x, wavenum_width = 70, ...) {
  raw_points <- floor(wavenum_width/spec_res(x))

  if(raw_points %% 2 == 0){
    raw_points <- raw_points - 1
  }
  if(raw_points > length(x)){
    stop("The wavenum_width must be shorter than the full spectrum.")
  }
  if(raw_points <= 3){
    stop("The wavenum_width must be longer than 3X the spectral resolution.")
  }
  return(raw_points)
}

#' @rdname smooth_intens
#'
#' @export
calc_window_points.OpenSpecy <- function(x, wavenum_width = 70, ...){
  do.call("calc_window_points", list(x = x$wavenumber, wavenum_width = wavenum_width))
}

.sgfilt <- function(y, p, n, m, ...) {
  as.numeric(.sgfilt_matrix(matrix(y, ncol = 1L), p = p, n = n, m = m, ...))
}

.sgfilt_matrix <- function(y, p, n, m, ...) {
  filt <- .sgolay_filter(p = p, n = n, m = m, ...)
  len <- nrow(y)
  n <- nrow(filt)
  k <- floor(n / 2)

  if (len < n) {
    stop("'window' must be less than or equal to the number of wavenumbers",
         call. = FALSE)
  }

  out <- matrix(NA_real_, nrow = len, ncol = ncol(y),
                dimnames = list(NULL, colnames(y)))
  out[seq_len(k), ] <- filt[seq_len(k), , drop = FALSE] %*%
    y[seq_len(n), , drop = FALSE]

  mid_n <- len - n + 1L
  mid <- matrix(0, nrow = mid_n, ncol = ncol(y))
  center <- filt[k + 1L, ]

  # The centered window is the common case; loop over the small filter width
  # once and update every spectrum column at the same time.
  for (i in seq_len(n)) {
    rows <- i:(i + mid_n - 1L)
    mid <- mid + center[i] * y[rows, , drop = FALSE]
  }
  out[(k + 1L):(len - k), ] <- mid

  out[(len - k + 1L):len, ] <- filt[(k + 2L):n, , drop = FALSE] %*%
    y[(len - n + 1L):len, , drop = FALSE]
  out
}

.sgolay_filter <- function(p, n, m = 0, ts = 1) {
  if (n %% 2 != 1)
    stop("sgolay needs an odd filter length n", call. = FALSE)
  if (p >= n)
    stop("sgolay needs filter length n larger than polynomial order p",
         call. = FALSE)

  filter_matrix <- matrix(0, n, n)
  k <- floor(n / 2)
  powers <- matrix(rep(0:p, each = n), nrow = n)

  for (row in seq_len(k + 1L)) {
    centered <- matrix((seq_len(n) - row), nrow = n, ncol = p + 1L)
    coef_matrix <- centered^powers
    filter_matrix[row, ] <- .matrix_ginv(coef_matrix,
                                          tol = .Machine$double.eps)[1L + m, ]
  }

  filter_matrix[(k + 2L):n, ] <- (-1)^m * filter_matrix[k:1L, n:1L]
  if (m > 0) filter_matrix <- filter_matrix * prod(seq_len(m)) / (ts^m)
  class(filter_matrix) <- "sgolayFilter"
  filter_matrix
}

.matrix_ginv <- function(x, tol = sqrt(.Machine$double.eps)) {
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("'x' must be a numeric or complex matrix", call. = FALSE)
  if (!is.matrix(x)) x <- as.matrix(x)

  x_svd <- svd(x)
  if (is.complex(x)) x_svd$u <- Conj(x_svd$u)
  positive <- x_svd$d > max(tol * x_svd$d[1L], 0)

  if (all(positive)) {
    x_svd$v %*% (1 / x_svd$d * t(x_svd$u))
  } else if (!any(positive)) {
    array(0, dim(x)[2L:1L])
  } else {
    x_svd$v[, positive, drop = FALSE] %*%
      ((1 / x_svd$d[positive]) * t(x_svd$u[, positive, drop = FALSE]))
  }
}

.derivative <- function(y, res = NULL, derivative = 1, lag = 1) {
  if(derivative == 0) {
    return(y)
  }
  else if(derivative > 0) {
    if(is.null(res)) stop("res must be specified for the derivative to work")
    size = length(y)
    y = diff(y, lag = lag, differences = derivative)/(res*lag)
    y = c(y, rep(y[length(y)], size - length(y)))
    return(y)
  } else {
    stop("derivative must be greater than or equal to zero")
  }
}

.derivative_matrix <- function(y, res = NULL, derivative = 1, lag = 1) {
  if(derivative == 0) {
    return(y)
  }
  else if(derivative > 0) {
    if(is.null(res)) stop("res must be specified for the derivative to work")
    size <- nrow(y)
    diff_y <- diff(y, lag = lag, differences = derivative)/(res*lag)
    out <- matrix(NA_real_, nrow = size, ncol = ncol(y),
                  dimnames = list(NULL, colnames(y)))
    out[seq_len(nrow(diff_y)), ] <- diff_y
    out[(nrow(diff_y) + 1L):size, ] <-
      matrix(rep(diff_y[nrow(diff_y), ], each = size - nrow(diff_y)),
             nrow = size - nrow(diff_y),
             ncol = ncol(diff_y))
    colnames(out) <- colnames(y)
    return(out)
  } else {
    stop("derivative must be greater than or equal to zero")
  }
}

.whittaker <- function(y, lambda = 1600, d = 2) {
  m <- length(y)
  E <- .eye(m)
  D <- diff(E, lag = 1, differences = d)
  B <- E + (lambda * t(D) %*% D)
  z <- solve(B, y)

  return(z)
}

.whittaker_matrix <- function(y, lambda = 1600, d = 2) {
  m <- nrow(y)
  E <- .eye(m)
  D <- diff(E, lag = 1, differences = d)
  B <- E + (lambda * t(D) %*% D)
  z <- solve(B, y)
  colnames(z) <- colnames(y)
  z
}

.eye <- function(n, m = n) {
  stopifnot(is.numeric(n), length(n) == 1,
            is.numeric(m), length(m) == 1)
  n <- floor(n)
  m <- floor(m)
  if (n <= 0 || m <= 0) return(matrix(NA, 0, 0))
  else                  return(base::diag(1, n, m))
}
