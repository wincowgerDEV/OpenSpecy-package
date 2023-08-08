#' @title Generic Open Specy Functions
#' @rdname gen_OpenSpecy
#'
#' @description
#' Functions to manipulate and visualize OpenSpecy objects.
#'
#' @details
#' \code{print()} prints the contents of an OpenSpecy object.
#' \code{head()} shows the first few lines of an OpenSpecy object.
#'
#' @param x an OpenSpecy object.
#' @param \ldots args.
#'
#' @return
#' \code{print()} returns a textual representation of an OpenSpecy object.
#' \code{head()} returns the first few lines of an OpenSpecy object.
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
#' @method plot OpenSpecy
#' @export
plot.OpenSpecy <- function(x, ...) {
  matplot(x$wavenumber, x$spectra, type = "l",
  xlab = "wavenumber",
  ylab = "absorbance intensity",
  xlim = rev(range(x$wavenumber)),
  ...)
}

#Could be nice to add at some point.
#summary.OpenSpecy <- function(x, ...) {

#}
