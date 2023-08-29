#' @rdname adj_range
#' @title Range restriction and flattening for spectra
#'
#' @description
#' \code{restrict_range()} restricts wavenumber ranges to user specified values.
#' Multiple ranges can be specified by inputting a series of max and min
#' values in order.
#' \code{flatten_range()} will flatten ranges of the spectra that should have no
#' peaks.
#' Multiple ranges can be specified by inputting the series of max and min
#' values in order.
#'
#' @param x an \code{OpenSpecy} object.
#' @param min a vector of minimum values for the range to be flattened.
#' @param max a vector of maximum values for the range to be flattened.
#' @param make_rel logical; should the output intensities be normalized to the
#' range \[0, 1\] using `make_rel()` function?
#' @param \ldots additional arguments passed to subfunctions; currently not
#' in use.
#'
#' @return
#' An \code{OpenSpecy} object with the spectral intensities within specified
#' ranges restricted or flattened.
#'
#' @examples
#' test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
#'                            spectra = data.frame(intensity = rnorm(361)))
#' plot(test_noise)
#'
#' restrict_range(test_noise, min = 1000, max = 2000)
#'
#' flattened_intensities <- flatten_range(test_noise, min = c(1000, 2000),
#'                                        max = c(1500, 2500))
#' plot(flattened_intensities)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{conform_spec}()} for conforming wavenumbers to be matched with
#' a reference library;
#' \code{\link{adj_intens}()} for log transformation functions;
#' \code{\link[base]{min}()} and \code{\link[base]{round}()}
#'
#' @importFrom data.table as.data.table .SD
#' @export
restrict_range <- function(x, ...) {
  UseMethod("restrict_range")
}

#' @rdname adj_range
#'
#' @export
restrict_range.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
restrict_range.OpenSpecy <- function(x, min, max, make_rel = TRUE,
                                     ...) {
  test <- as.data.table(lapply(1:length(min), function(y){
    x$wavenumber >= min[y] & x$wavenumber <= max[y]})
  )

  vals <- rowSums(test) > 0
  filt <- x$spectra[vals,]
  x$wavenumber <- x$wavenumber[vals]

  if (make_rel) x$spectra <- filt[, lapply(.SD, make_rel)] else x$spectra <- filt

  return(x)
}

#' @rdname adj_range
#'
#' @export
flatten_range <- function(x, ...) {
  UseMethod("flatten_range")
}

#' @rdname adj_range
#'
#' @export
flatten_range.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
flatten_range.OpenSpecy <- function(x, min = 2200, max = 2400, make_rel = TRUE,
                                    ...) {
  if(length(min) != length(max)) {
    stop("min and max need to be the same length", call. = F)
  }
  if(any(vapply(1:length(min), function(y) {
    min[y] > max[y]
  }, FUN.VALUE = logical(1)))) {
    stop("all min values must be lower than corresponding max", call. = F)
  }
  flat <- x$spectra[, lapply(.SD, .flatten_range, x = x$wavenumber,
                             min = min, max = max)]

  if (make_rel) x$spectra <- flat[, lapply(.SD, make_rel)] else x$spectra <- flat

  return(x)
}

.flatten_range <- function(y, x, min, max) {
  if(all(min > max(x)) ||  all(max < min(x)))
    stop("'min' or 'max' out of range")

  for(i in 1:length(min)) {
    y[x >= min[i] & x <= max[i]] <-
      mean(c(y[min(which(x >= min[i]))],
             y[max(which(x <= max[i]))]))
  }
  return(y)
}
