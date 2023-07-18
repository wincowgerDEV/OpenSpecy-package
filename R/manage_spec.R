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
#' @importFrom data.table data.table as.data.table fread rbindlist
#' @export
#' @rdname c_spec
#'
#' @export
c_spec <- function(objects, wavenumbers = NULL, res = NULL, coords = NULL){

    if(!is.list(objects) | !all(lapply(objects, function(x){inherits(x, "OpenSpecy")}))){
        stop("Objects you are trying to concatenate must be a list of Open Specy objects")
    }  
    
    if(!is.null(wavenumbers)){
    if(wavenumbers == "first"){
      objects <- lapply(objects, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(objects[[1]]$wavenumber, res = res) else objects[[1]]$wavenumber},
                        coords = NULL)
      })
    }
    if(wavenumbers == "max_range"){
      all = unique(unlist(lapply(objects, function(x) x$wavenumber)))
      objects <- lapply(objects, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(all, res = res) else all},
                        coords = NULL)})
    }
    if(wavenumbers == "min_range"){
      smallest_range = which.min(vapply(objects, function(x) length(x$wavenumber), FUN.VALUE = numeric(1)))
      objects <- lapply(objects, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(objects[[smallest_range]]$wavenumber, res = res) else objects[[smallest_range]]$wavenumber},
                        coords = NULL)})
    }
    if(wavenumbers == "most_common_range"){
      wavenumbers = table(unlist(lapply(objects, function(x) x$wavenumber)))
      common_range = as.numeric(names(wavenumbers)[wavenumbers == max(wavenumbers)])
      objects <- lapply(objects, function(x) {
        conform_spectra(data = x,
                        xout = {if(!is.null(res)) conform_res(objects[[common_range]]$wavenumber, res = res) else objects[[common_range]]$wavenumber},
                        coords = NULL)})
    }
  }

  unlisted <- unlist(objects, recursive = F)

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
