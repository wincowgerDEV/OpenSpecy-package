#' @title Generic Open Specy Methods
#' @rdname gen_OpenSpecy
#'
#' @description
#' Methods to visualize OpenSpecy objects.
#'
#' @details
#' \code{head()} shows the first few lines of an OpenSpecy object.
#' \code{print()} prints the contents of an OpenSpecy object.
#' \code{summary()} produces a result summary of an OpenSpecy object.
#' \code{plot()} produces a \code{\link[graphics]{matplot}()} of an OpenSpecy
#' object; \code{lines()} adds new spectra to it.
#'
#' @param x an OpenSpecy object.
#' @param object an OpenSpecy object.
#' @param \ldots further arguments passed to the respective default method.
#'
#' @return
#' \code{head()}, \code{print()}, and \code{summary()} return a textual
#' representation of an OpenSpecy object.
#' \code{plot()} and \code{lines()} return a plot.
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
#' \code{\link[utils]{head}()}, \code{\link[base]{print}()},
#' \code{\link[base]{summary}()}, \code{\link[graphics]{matplot}()}, and
#' \code{\link[graphics]{matlines}()}
#'
#' @importFrom utils head
#' @importFrom graphics matplot matlines
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

#' @rdname gen_OpenSpecy
#'
#' @method lines OpenSpecy
#' @export
lines.OpenSpecy <- function(x, ...) {
  matlines(x$wavenumber, x$spectra, type = "l",
          ...)
}

#' @rdname gen_OpenSpecy
#'
#' @method summary OpenSpecy
#' @export
summary.OpenSpecy <- function(object, ...) {
  cat("$wavenumber\n")
  wl <- length(object$wavenumber)
  wr <- range(object$wavenumber)
  res <- spec_res(object)
  array(c(wl, wr, res), c(1,4), list("", c("Length", "Min.", "Max.",
                                           "Res."))) |>
    print()

  cat("\n$spectra\n")
  sl <- length(object$spectra)
  sr <- range(object$spectra)
  array(c(sl, sr), c(1,3), list("", c("Number", "Min. Intensity",
                                      "Max. Intensity"))) |>
    print()

  cat("\n$metadata\n")
  xr <- range(object$metadata$x)
  yr <- range(object$metadata$y)
  t(array(c(xr, yr), c(2,2), list(c("Min.", "Max."), c("x", "y")))) |> print()
  names(object$metadata) |> print()
}
