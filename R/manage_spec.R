#' @rdname manage_spec
#'
#' @title Manage spectral objects
#'
#' @description
#' Functions for
#'
#' @details
#' details
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param file file to be read from or written to.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param method submethod to be used for reading text files; defaults to
#' \code{\link[data.table]{fread}()} but \code{\link[utils]{read.csv}()} works
#' as well.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return data frames containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' c()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[hyperSpec]{read.jdx}()};
#'
#' @importFrom magrittr %>%
#' @importFrom data.table data.table as.data.table fread
#' @export


#' @rdname data_norm
#'
#' @export
c_spec <- function(..., wavenumbers = NULL, res = NULL, coords = NULL){

  if(!is.list(files)) {
    lof <- lapply(files, read_spec, coords = NULL)
  }
  else{
    lof <- files
  }

  if(!is.null(wavenumbers)){
    if(wavenumbers == "first"){
      lof <- lapply(lof, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(lof[[1]]$wavenumber, res = res) else lof[[1]]$wavenumber},
                        coords = NULL)
      })
    }
    if(wavenumbers == "max_range"){
      all = unique(unlist(lapply(lof, function(x) x$wavenumber)))
      lof <- lapply(lof, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(all, res = res) else all},
                        coords = NULL)})
    }
    if(wavenumbers == "min_range"){
      smallest_range = which.min(vapply(lof, function(x) length(x$wavenumber), FUN.VALUE = numeric(1)))
      lof <- lapply(lof, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(lof[[smallest_range]]$wavenumber, res = res) else lof[[smallest_range]]$wavenumber},
                        coords = NULL)})
    }
    if(wavenumbers == "most_common_range"){
      wavenumbers = table(unlist(lapply(lof, function(x) x$wavenumber)))
      common_range = as.numeric(names(wavenumbers)[wavenumbers == max(wavenumbers)])
      lof <- lapply(lof, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(lof[[common_range]]$wavenumber, res = res) else lof[[common_range]]$wavenumber},
                        coords = NULL)})
    }
  }

  unlisted <- unlist(lof, recursive = F)

  list <- tapply(unlisted, names(unlisted), FUN = function(x) unname((x)))

  if(length(unique(vapply(list$wavenumber, length, FUN.VALUE = numeric(1)))) > 1 & is.null(wavenumbers)){
    stop("Wavenumbers are not the same between spectra, you need to specify how the wavenumbers should be merged.", call. = F)
  }

  as_OpenSpecy(
    x = list$wavenumber[[1]],
    spectra = as.data.table(list$spectra),
    metadata = rbindlist(list$metadata, fill = T)
  )
}


#' @rdname manage_spec
#'
#' @importFrom data.table rbindlist
#' @export
# c_spec <- function(...) {
#     cin <- c(...)
#
#     lst <- tapply(cin, names(cin), FUN = function(x) unname((x)))
#
#     as_OpenSpecy(
#         x = lst$wavenumber[[1]],
#         # TODO: Probably should add a check to make sure all the wavenumbers are
#         # aligned before doing this.
#         spectra = as.data.table(lst$spectra),
#         metadata = rbindlist(lst$metadata, fill = T)
#     )
# }

c_spec <- function(...) {
  UseMethod("conform_spec")
}

#' @rdname conform_spec
#'
#' @export
conform_spec.default <- function(...) {
  stop("'...' items need to be of class 'OpenSpecy'", call. = F)
}

#' @rdname conform_spec
#'
#' @export
conform_spec.list <- function(...) {

}

#' @rdname conform_spec
#'
#' @export
conform_spec.OpenSpecy <- function(...) {
  lst <- list(...)

  do.call
}
