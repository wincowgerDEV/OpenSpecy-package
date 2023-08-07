#' @title Generic Open Specy Functions
#' @rdname gen_OpenSpecy
#'
#' @description
#' Functions to manipulate and visualize OpenSpecy objects.
#'
#' @details
#' \code{print.OpenSpecy()} prints the contents of an OpenSpecy object.
#' \code{head.OpenSpecy()} shows the first few lines of an OpenSpecy object.
#' \code{sample.OpenSpecy()} samples spectra from an OpenSpecy object.
#'
#' @param x an OpenSpecy object.
#' @param size a positive integer specifying the number of items to select from the spectra. (only for \code{sample()})
#' @param replace should sampling be done with replacement? (only for \code{sample()})
#' @param prob a vector of probability weights for obtaining the elements of the spectra being sampled. (only for \code{sample()})
#' @param \ldots args.
#'
#' @return
#' \code{print.OpenSpecy()} returns a textual representation of an OpenSpecy object.
#' \code{head.OpenSpecy()} returns the first few lines of an OpenSpecy object.
#' \code{sample.OpenSpecy()} returns an OpenSpecy object with a subset of the spectra.
#'
#' @examples
#' data("raman_hdpe")
#'
#' # Printing the OpenSpecy object
#' print(raman_hdpe)
#'
#' # Displaying the first few lines of the OpenSpecy object
#' head(raman_hdpe)
#'
#' # Plotting the spectra
#' plot(raman_hdpe)
#'
#' # Sampling the OpenSpecy object
#' tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
#' sampled <- sample(tiny_map, size = 3)
#' print(sampled)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' Other related functions.
#'
#' @importFrom utils head
#' @importFrom graphics matplot
#' @export
head.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) |> head(...)
}

#' @rdname gen_OpenSpecy
#'
#' @method print OpenSpecy
#' @export
print.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) |> print(...)
  cat("\n$metadata\n")
  print(x$metadata)
}

#' @rdname gen_OpenSpecy
#'
#' @method sample OpenSpecy
#' @export
sample.OpenSpecy <- function(x, size, ...) {
  # replace = false is mandatory currently because we don't have a way to
  # rename and recoordinate duplicates.
  cols <- sample(1:ncol(x$spectra), size = size, ...)

  as_OpenSpecy(
    x = x$wavenumber,
    spectra = x$spectra[, ..cols],
    metadata = x$metadata[cols, ]
  )
}

#' @rdname gen_OpenSpecy
#'
#' @method plot OpenSpecy
#' @export
plot.OpenSpecy <- function(x, ...) {
  matplot(x$wavenumber, x$spectra, type = "l",
  xlab = "wavenumber",
  ylab = "absorbance intensity", ...)
}

#Could be nice to add at some point.
#summary.OpenSpecy <- function(x, ...) {

#}
