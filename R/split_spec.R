#' @rdname split_spec
#'
#' @title Split Open Specy objects
#'
#' @description
#' Convert a list of Open Specy objects with any number of spectra into
#' a list of Open Specy objects with one spectrum each.
#'
#' @param x a list of OpenSpecy objects
#'
#' @details
#' Function will accept a list of Open Specy objects of any length and will split
#' them to their individual components. For example a list of two objects,
#' an Open Specy with only one spectrum and an Open Specy with 50 spectra
#' will return a list of length 51 each with Open Specy objects that only have
#' one spectrum.
#'
#' @return
#' A list of Open Specy objects each with 1 spectrum.
#'
#' @examples
#' data("test_lib")
#' data("raman_hdpe")
#' listed <- list(test_lib, raman_hdpe)
#' test <- split_spec(listed)
#' test2 <- split_spec(list(test_lib))
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{c_spec}()} for combining \code{OpenSpecy} objects.
#' \code{\link{collapse_spec}()} for summarizing \code{OpenSpecy} objects.
#'
#'
#' @export
split_spec <- function(x){
  if(is_OpenSpecy(x)) stop("x must be a list of Open Specy objects")

  lapply(x, function(x) {
    if(ncol(x$spectra) == 1){
      return(list(x))
    } else {
      lapply(1:ncol(x$spectra), function(y){
        as_OpenSpecy(x$wavenumber,
                     x$spectra[,y, with = F],
                     x$metadata[y,])
      })
    }
  }) |>
    unlist(recursive = F)
}
