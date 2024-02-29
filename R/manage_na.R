#' @rdname manage_na
#' @title Ignore or Remove NA intensities
#'
#' @description
#' Sometimes you want to keep or remove NA values in intensities to allow for spectra with 
#' varying shapes to be analyzed together or maintained in a single Open Specy object. 
#' 
#' @param x a numeric vector or an \R OpenSpecy object
#' @param lead_tail_only logical whether to only look at leading adn tailing values. 
#' @param ig_zero logical, whether to ignore both zeros and NAs
#' @param fun the name of the function you want run. 
#' @param type character of either "ignore" or "remove".
#' @param \ldots further arguments passed to \code{fun}.
#'
#' @return
#' \code{manage_na()} return logical vectors of NA locations (if vector provided) or an
#' \code{OpenSpecy} object with ignored or removed NA values.
#'
#' @examples
#' manage_na(c(NA, -1, NA, 1, 10))
#' manage_na(c(NA, -1, NA, 1, 10), lead_tail_only = F)
#' manage_na(c(NA, 0, NA, 1, 10), lead_tail_only = F, ig_zero = T)
#' data(raman_hdpe)
#' raman_hdpe$spectra[[1]][1:10] <- NA
#' manage_na(raman_hdpe, fun = make_rel) #would normally return all NA without na.rm = T but doesn't here. 
#' manage_na(raman_hdpe, type = "remove") #will remove the first 10 values we set to NA
#' 
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{OpenSpecy} object to be matched with a reference library
#' \code{fill_spec()} can be used to fill NA values in Open Specy objects.
#' \code{restrict_range()} can be used to restrict spectral ranges in other ways than removing NAs.
#'
#' @export
manage_na <- function(x, ...) {
    UseMethod("manage_na")
}

#' @rdname manage_na
#' @export
manage_na.default <- function(x, lead_tail_only = T, ig_zero = F, ...) {

    if(lead_tail_only){
        na_positions <- logical(length(x))
        if(is.na(x[1])){
            criteria = T
            y = 1
            while(criteria){
                if(is.na(x[y])|(ig_zero & x[y] == 0)) na_positions[y] <- T 
                y = y + 1
                criteria = is.na(x[y])|(ig_zero & x[y] == 0) 
            }
        }
        if(is.na(x[length(x)])){
            criteria = T
            y = length(x)
            while(criteria){
                if(is.na(x[y])|(ig_zero & x[y] == 0)) na_positions[y] <- T 
                y = y - 1
                criteria = is.na(x[y])|(ig_zero & x[y] == 0) 
            }
        }
    }
    else{
        na_positions <- is.na(x)|(ig_zero & x == 0)
    }
    
    return(na_positions)
}

#' @rdname manage_na
#' @export
manage_na.OpenSpecy <- function(x, lead_tail_only = T, ig_zero = F, fun, type = "ignore", ...) {
    
    consistent <- x$spectra[, lapply(.SD, manage_na, 
                               lead_tail_only = lead_tail_only, 
                               ig_zero = ig_zero)] |>
        rowSums() == 0
    
    if(sum(consistent) < 3 & type == "ignore") stop("Not enough non NA values, need atleast 3")
    
    if(type == "ignore"){
        reduced <- as_OpenSpecy(x$wavenumber[consistent], x$spectra[consistent,], x$metadata) |>
            fun(...)
        
        x$spectra <- x$spectra[, lapply(.SD, as.numeric)]
        
        x$spectra[consistent,] <- reduced$spectra        
    }
    
    if(sum(!consistent) < 3 & type == "remove") stop("Not enough non NA values left, need atleast 3")
    
    if(type == "remove"){
        x <- as_OpenSpecy(x$wavenumber[consistent], x$spectra[consistent,], x$metadata)
    }
    
    return(x)

}

