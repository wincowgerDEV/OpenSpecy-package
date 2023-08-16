#' @rdname manage_spec
#' @title Manage spectral objects
#'
#' @description
#' Functions for concatenating \code{OpenSpecy} objects.
#'
#' @param x a list of \code{OpenSpecy} objects.
#' @param range a numeric providing your own wavenumber ranges or character
#' argument called \code{"common"} to let \code{c_spec()} find the common
#' wavenumber range of the supplied spectra. \code{NULL} will interpret the
#' spectra having all the same wavenumber range.
#' @param res defaults to \code{NULL}, the resolution you want the output
#' wavenumbers to be.
#' @param \ldots further arguments passed to submethods.
#'
#' @return
#' A single \code{OpenSpecy} object.
#'
#' @examples
#' spectra <- lapply(c(read_extdata("raman_hdpe.csv"),
#'                     read_extdata("ftir_ldpe_soil.asp")), read_any)
#' common <- c_spec(spectra, range = "common", res = 5)
#' range <- c_spec(spectra, range = c(1000, 2000), res = 5)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[OpenSpecy]{conform_spec}()} for conforming wavenumbers
#'
#' @importFrom data.table data.table as.data.table fread rbindlist
#'
#' @export
c_spec <- function(x, ...) {
  UseMethod("c_spec")
}

#' @rdname manage_spec
#'
#' @export
c_spec.default <- function(x, ...) {
  stop("object 'x' needs to be a list of 'OpenSpecy' objects")
}

#' @rdname manage_spec
#'
#' @export
c_spec.OpenSpecy <- function(x, ...) {
  warning("object 'x' needs to be a list of 'OpenSpecy' objects; ",
          "nothing to concatenate, returning 'x'")

  return(x)
}

#' @rdname manage_spec
#'
#' @export
c_spec.list <- function(x, range = NULL, res = 5, ...) {
  if(!all(vapply(x, function(y) {inherits(y, "OpenSpecy")}, FUN.VALUE = T)))
    stop("object 'x' needs to be a list of 'OpenSpecy' objects", call. = F)

  if(!is.null(range)) {
    if(is.numeric(range)) {
      wn <- range
    }
    else if(!is.null(range) && range == "common") {
      pmin <- vapply(x, function(y) min(y$wavenumber), FUN.VALUE = numeric(1))
      pmax <- vapply(x, function(y) max(y$wavenumber), FUN.VALUE = numeric(1))

      if(any(max(pmin) > pmax) | any(min(pmax) < pmin))
        stop("data points need to overlap in their ranges", call. = F)

      wn <- c(max(pmin), min(pmax))
    }

    x <- lapply(x, conform_spec, range = wn, res = res)
  }

  unlisted <- unlist(x, recursive = F)
  list <- tapply(unlisted, names(unlisted), unname)

  if(length(unique(vapply(list$wavenumber, length, FUN.VALUE = numeric(1)))) > 1
     & is.null(range)) {
    stop("wavenumbers need to be identical between spectra; specify how; use ",
         "'range' to specify how wavenumbers should be merged", call. = F)
  }

  as_OpenSpecy(x = list$wavenumber[[1]],
               spectra = as.data.table(list$spectra),
               metadata = rbindlist(list$metadata, fill = T)[,-c("x","y")]
  )
}
