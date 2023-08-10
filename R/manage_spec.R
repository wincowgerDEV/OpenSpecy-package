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
#' @param objects a list object of class \code{OpenSpecy}.
#' @param wavenumber_transform a numeric if providing your own wavenumbers or character argument of "common_range" depending on how you want to merge the wavenumbers between the spectra, NULL will be interpreted as the spectra are all the same wavenumber range, see details for more information.
#' @param res defaults to \code{NULL}, the resolution you want the output wavenumbers to be.
#'
#' @return
#' A single OpenSpecy object. 
#'
#' @examples
#' spectra <- lapply(c(read_extdata("raman_hdpe.csv"),  read_extdata("ftir_ldpe_soil.asp")), read_any)
#' spectra2 <- c_spec(objects = spectra, wavenumber_transform = "common_range", res = 5)
#' spectra3 <- c_spec(objects = spectra, wavenumber_transform = c(1000, 2000), res = 5)
#' 
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[OpenSpecy]{conform_spec}()};
#' \code{\link[OpenSpecy]{conform_res}()};
#' \code{\link[OpenSpecy]{as_OpenSpecy}()};
#'
#' @importFrom data.table data.table as.data.table fread rbindlist
#'
#' @export
c_spec <- function(objects, wavenumber_transform = NULL, res = 5){

    if(!is.list(objects) | !all(vapply(objects, function(x){inherits(x, "OpenSpecy")}, FUN.VALUE = TRUE))){
        stop("Objects you are trying to concatenate must be a list of Open Specy objects")
    }  
    
    if(!is.null(wavenumber_transform)){
    if(is.numeric(wavenumber_transform)){
        new_wavenumbers = wavenumber_transform
    }
    else if(wavenumber_transform == "common_range"){
        possible_min = vapply(objects, function(x) min(x$wavenumber), FUN.VALUE = numeric(1))
        possible_max = vapply(objects, function(x) max(x$wavenumber), FUN.VALUE = numeric(1))
        smallest = max(possible_min)
        largest = min(possible_max)
        if(any(smallest > possible_max) | any(largest < possible_min)){
            stop("All data points need to have some overlap in their ranges for the merge to work.")
        }
        new_wavenumbers = c(smallest, largest)
    }
        objects <- lapply(objects, function(x) {
            conform_spec(x,
                         new_wavenumbers = new_wavenumbers, 
                         res = res)})
    }

  unlisted <- unlist(objects, recursive = F)

  list <- tapply(unlisted, names(unlisted), FUN = function(x) unname((x)))

  if(length(unique(vapply(list$wavenumber, length, FUN.VALUE = numeric(1)))) > 1 & is.null(wavenumber_transform)){
    stop("Wavenumbers are not the same between spectra, you need to specify how the wavenumbers should be merged.", call. = F)
  }

  as_OpenSpecy(
    x = list$wavenumber[[1]],
    spectra = as.data.table(list$spectra),
    metadata = rbindlist(list$metadata, fill = T)[,-c("x","y")]
  )
}
