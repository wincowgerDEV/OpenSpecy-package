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
#' @param cor_matrix A correlation matrix for object and library, can be returned by \code{correlate_spectra()}
#' @param add_library_metadata Name of the column in the library metadata containing the column names or NULL if you don't want to join.
#' @param add_object_metadata Name of the column in the object metadata containing the column names or NULL if you don't want to join.
#' @param remove_empty Whether to remove empty columns in the metadata where there are no values.
#' @param logic a logical or numeric vector describing which spectra to keep (TRUE).
#' @param \ldots Additional arguments passed to the \code{cor()} function for correlation calculation.
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
#' unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) |>
#'   conform_spec(new_wavenumbers = test_lib$wavenumber, res = spec_res(test_lib)) |>
#'   process_spectra()
#' matches <- correlate_spectra(unknown, test_lib)
#'
#' test_lib_extract <- filter_spec(
#'   test_lib,
#'   logic = test_lib$metadata$polymer_class == "polycarbonates"
#' )
#'
#' matches2 <- correlate_spectra(object = unknown, library = test_lib_extract)
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
#'
#' @export
correlate_spectra <- function(object, ...) {
    UseMethod("correlate_spectra")
}

#' @export
correlate_spectra.default <- function(object, ...) {
    stop("object needs to be of class 'OpenSpecy'")
}

#' @export
correlate_spectra.OpenSpecy <- function(object, library, na.rm = T, ...){
    if(sum(object$wavenumber %in% library$wavenumber) < 3){
        stop("There are less than 3 matching wavenumbers in the objects you are trying to correlate, this won't work for correlation analysis. Consider first conforming the spectra to the same wavenumbers.")
    }
    cor(library$spectra[library$wavenumber %in% object$wavenumber,][,lapply(.SD, make_rel, na.rm = na.rm)][,lapply(.SD, mean_replace)],
        object$spectra[object$wavenumber %in% library$wavenumber,][,lapply(.SD, make_rel, na.rm = na.rm)][,lapply(.SD, mean_replace)],
        ...)
}

#' @export
identify_spectra <- function(cor_matrix, object, library, top_n = NULL,
                             add_library_metadata = NULL,
                             add_object_metadata = NULL, ...){
    if(is.numeric(top_n) && top_n > ncol(library$spectra)){
        top_n = NULL
        message("top_n was larger than the number of spectra in the library, returning all matches")
    }

    data.table(object_id = colnames(object$spectra),
                                   library_id = rep(colnames(library$spectra),
                                               each = ncol(object$spectra)),
                                   match_val = c(cor_matrix)) %>%
        { if (is.numeric(top_n)) .[order(-match_val), head(.SD, top_n), by = object_id] else .} %>%
        { if (is.character(add_library_metadata)) left_join(., library$metadata, by = c("library_id" = add_library_metadata)) else . } %>%
        { if (is.character(add_object_metadata)) left_join(., object$metadata, by = c("object_id" = add_object_metadata)) else . }
}

#' @export
mean_replace <- function(intensity, na.rm = T){
    fifelse(is.na(intensity), mean(intensity, na.rm = na.rm), intensity)
}

#' @export
get_metadata <- function(object, logic, remove_empty = T){
    if(is.character(logic)){
        logic = which(names(object$spectra) %in% logic)
    }
    object$metadata[logic,] %>%
        {if(remove_empty){.[, !sapply(., is_empty_vector), with = F]} else{.}}
}

#' @export
is_empty_vector <- function(v) {
    # Check if the vector is NULL or has zero length
    if (is.null(v) || length(v) == 0) {
        return(TRUE)
    }

    # Check if all values are NA or NaN (for numeric vectors)
    if (is.numeric(v)) {
        return(all(is.na(v) | is.nan(v)))
    }

    # Check if all values are NA or empty strings (for character vectors)
    if (is.character(v)) {
        return(all(is.na(v) | v == ""))
    }

    # Check if all values are NA (for other types of vectors)
    return(all(is.na(v)))
}

#' @export
max_cor_named <- function(cor_matrix, na.rm = T) {
    # Find the indices of maximum correlations
    max_cor_indices <- apply(cor_matrix, 2, function(x) which.max(x))

    # Use indices to get max correlation values
    max_cor_values <- vapply(1:length(max_cor_indices), function(idx) cor_matrix[max_cor_indices[idx],idx], FUN.VALUE = numeric(1))

    # Use indices to get the corresponding names
    names(max_cor_values) <- rownames(cor_matrix)[max_cor_indices]

    return(max_cor_values)
}

#' @export
filter_spec <- function(object, logic) {
    if(is.character(logic)){
        logic = which(names(object$spectra) %in% logic)
    }
    object$spectra <- object$spectra[,..logic]
    object$metadata <- object$metadata[logic,]
    object
}
