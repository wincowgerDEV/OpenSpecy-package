#' @rdname adj_range
#' @title Range restriction and flattening for spectra
#'
#' @description
#' \code{restrict_range()} restricts wavenumber ranges to user specified values.
#' Multiple ranges can be specified by inputting the series of max and min values in order.
#' \code{flatten_range()} will flatten ranges of the spectra that should have no peaks.
#' Multiple ranges can be specified by inputting the series of max and min values in order.
#'
#' @param x an OpenSpecy object containing spectral wavenumbers and intensities.
#' @param min_range a vector of minimum values for the range to be flattened.
#' @param max_range a vector of maximum values for the range to be flattened.
#' @param make_rel logical; should the output intensities be normalized to the range [0, 1] using make_rel() function?
#' @param \ldots additional arguments passed to subfunctions; currently not in use.
#'
#' @return
#' An OpenSpecy object with the spectral intensities within specified ranges
#' restricted or flattened.
#'
#' @examples
#' test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
#'                            spectra = data.frame(intensity = rnorm(361)))
#' plot(test_noise)
#'
#' restrict_range(test_noise, min_range = 1000, max_range = 2000)
#'
#' flattened_intensities <- flatten_range(test_noise, min_range = c(1000, 2000),
#'                                        max_range = c(1500, 2500))
#' plot(flattened_intensities)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link[base]{min}()} and \code{\link[base]{round}()};
#' \code{\link{adj_intens}()} for log transformation functions
#'
#'
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table .SD
#' @export
restrict_range <- function(x, ...) {
    UseMethod("restrict_range")
}

#' @rdname adj_range
#'
#' @export
restrict_range.default <- function(x, ...) {
    stop("x needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
restrict_range.OpenSpecy <- function(x,
                                     min_range = 0,
                                     max_range = 6000,
                                     make_rel = TRUE,
                                        ...) {
    test <- as.data.table(lapply(1:length(min_range), function(y){
        x$wavenumber >= min_range[y] & x$wavenumber <= max_range[y]})
    )

    vals = rowSums(test) > 0

    filt <- x$spectra[vals,]

    x$wavenumber <- x$wavenumber[vals]

    if (make_rel) x$spectra <- make_rel(filt) else x$spectra <- filt

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
  stop("x needs to be of class 'OpenSpecy'")
}

#' @rdname adj_range
#'
#' @export
flatten_range.OpenSpecy <- function(x,
                                    min_range = NULL,
                                    max_range = NULL,
                                    make_rel = TRUE,
                                    ...) {
  if(is.null(min_range)|is.null(max_range)){
    stop("You need to specify a min and max range to flatten.")
  }
  if(length(min_range) != length(max_range)){
    stop("min_range and max_range need to be the same length.")
  }
  if(any(vapply(1:length(min_range), function(y){
    min_range[y] > max_range[y]
  }, FUN.VALUE = logical(1)))){
    stop("all min_range values must be lower than corresponding max_range")
  }
  filt <- x$spectra[,lapply(.SD, function(y){
    .flatten_range(wavenumber = x$wavenumber,
                   spectra = y,
                   min_range = min_range,
                   max_range = max_range)
  })]

  if (make_rel) x$spectra <- filt[, lapply(.SD, make_rel)] else x$spectra <- filt

  return(x)
}

.flatten_range <- function(wavenumber, spectra, min_range, max_range) {
  for(i in 1:length(min_range)){
    spectra[wavenumber >= min_range[i] & wavenumber <= max_range[i]] <- mean(c(spectra[min(which(wavenumber >= min_range[i]))],
                                                                               spectra[max(which(wavenumber <= max_range[i]))]))
  }

  return(spectra)
}
