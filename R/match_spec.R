#' @rdname match_spec
#' 
#' @title
#' Identify and filter spectra
#'
#' @description
#' This function correlates two OpenSpecy objects, typically one with knowns and one with unknowns.
#'
#' @param object An OpenSpecy object, typically with unknowns.
#' @param library An OpenSpecy object representing the library of spectra to correlate with.
#' @param na.rm Logical value indicating whether missing values should be removed when calculating correlations. Default is \code{TRUE}.
#' @param top_n Integer value specifying the number of top matches to return. If NULL (default), all matches will be returned.
#' @param add_library_metadata Name of the column in the library metadata containing the column names or NULL if you don't want to join. 
#' @param add_object_metadata Name of the column in the object metadata containing the column names or NULL if you don't want to join.
#' @param logic a logical vector describing which spectra to keep (TRUE).
#' @param ... Additional arguments passed to the \code{cor()} function for correlation calculation.
#'
#' @return
#' A data table containing correlations between spectra and the library.
#' The table has three columns: \code{object_id}, \code{library_id}, and \code{match_val}.
#' Each row represents a unique pairwise correlation between a spectrum in the object and a spectrum in the library.
#' If \code{top_n} is specified, only the top \code{top_n} matches for each object spectrum will be returned.
#' If \code{add_library_metadata} is \code{is.character}, the library metadata will be added to the output.
#' If \code{add_object_metadata} is \code{is.character}, the object metadata will be added to the output.
#'
#' @examples
#' data("test_lib")
#' unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) %>%
#'                       conform_spec(., new_wavenumbers = test_lib$wavenumber, res = spec_res(test_lib)) %>%
#'                       process_spectra(.)
#' matches <- OpenSpecy::correlate_spectra(unknown, test_lib, top_n = 10, add_library_metadata = "sample_name")
#' 
#' test_lib_extract <- filter_spec(test_lib, logic = test_lib$metadata$polymer_class == "polycarbonates")
#' 
#' matches2 <- OpenSpecy::correlate_spectra(object = unknown, library = test_lib_extract, top_n = 20, add_library_metadata = "sample_name")
#' 
#' @importFrom magrittr %>%
#' @importFrom data.table data.table fifelse .SD
#' @importFrom dplyr left_join
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{adj_intens}()} converts spectra;
#' \code{\link{get_lib}()} retrieves the Open Specy reference library;
#' \code{\link{load_lib}()} loads the Open Specy reference library into an \R
#' object of choice
#' @export
correlate_spectra <- function(object, ...) {
    UseMethod("correlate_spectra")
}

#' @export
correlate_spectra.default <- function(object, ...) {
    stop("object needs to be of class 'OpenSpecy'")
}

#' @export
correlate_spectra.OpenSpecy <- function(object, library, na.rm = T, top_n = NULL, add_library_metadata = NULL, add_object_metadata = NULL, ...){
    prep_data = object$spectra[object$wavenumber %in% library$wavenumber,][,lapply(.SD, make_rel, na.rm = na.rm)][,lapply(.SD, mean_replace)]
    prep_library = library$spectra[library$wavenumber %in% object$wavenumber,][,lapply(.SD, make_rel, na.rm = na.rm)][,lapply(.SD, mean_replace)]
    
    if(is.numeric(top_n) & top_n > ncol(library$spectra)){
        top_n = NULL
        message("top_n was larger than the number of spectra in the library, returning all matches")
    }
    
    data.table(object_id = colnames(object$spectra),
                                   library_id = rep(colnames(library$spectra),
                                               each = ncol(object$spectra)),
                                   match_val = c(cor(prep_data, prep_library))) %>%
        { if (is.numeric(top_n)) .[order(-match_val), head(.SD, top_n), by = object_id] else .} %>%
        { if (is.character(add_library_metadata)) left_join(., library$metadata, by = c("library_id" = add_library_metadata)) else . } %>%
        { if (is.character(add_object_metadata)) left_join(., object$metadata, by = c("object_id" = add_object_metadata)) else . } 
    
    
}


mean_replace <- function(intensity, na.rm = T){
    fifelse(is.na(intensity), mean(intensity, na.rm = na.rm), intensity)
}


#' @rdname match_spec
#'
#' @export
filter_spec <- function(object, logic) {
    object$spectra <- object$spectra[,..logic]
    object$metadata <- object$metadata[logic,]
    object
}




