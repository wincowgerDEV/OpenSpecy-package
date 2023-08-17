#' @rdname match_spec
#' @title Identify and filter spectra
#'
#' @description
#' \code{cor_spec()} correlate two \code{OpenSpecy} objects, typically one with
#' knowns and one with unknowns.
#'
#' @param x an \code{OpenSpecy} object, typically with unknowns.
#' @param library sn \code{OpenSpecy} object representing the reference library
#' of spectra to correlate with.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating correlations. Default is \code{TRUE}.
#' @param top_n integer; specifying the number of top matches to return.
#' If \code{NULL} (default), all matches will be returned.
#' @param cor_matrix a correlation matrix for object and library,
#' can be returned by \code{cor_spec()}
#' @param add_library_metadata name of a column in the library metadata to be
#' joined; \code{NULL} if you don't want to join.
#' @param add_object_metadata name of a column in the object metadata to be
#' joined; \code{NULL} if you don't want to join.
#' @param rm_empty logical; whether to remove empty columns in the metadata.
#' @param logic a logical or numeric vector describing which spectra to keep.
#' @param \ldots additional arguments passed \code{\link[stats]{cor}()}.
#'
#' @return
#' A \code{\link[data.table]{data.table-class}()} containing correlations
#' between spectra and the library.
#' The table has three columns: \code{object_id}, \code{library_id}, and
#' \code{match_val}.
#' Each row represents a unique pairwise correlation between a spectrum in the
#' object and a spectrum in the library.
#' If \code{top_n} is specified, only the top \code{top_n} matches for each
#' object spectrum will be returned.
#' If \code{add_library_metadata} is \code{is.character}, the library metadata
#' will be added to the output.
#' If \code{add_object_metadata} is \code{is.character}, the object metadata
#' will be added to the output.
#'
#' @examples
#' data("test_lib")
#' unknown <- read_any(read_extdata("ftir_ldpe_soil.asp")) |>
#'   conform_spec(range = test_lib$wavenumber,
#'                res = spec_res(test_lib)) |>
#'   process_spec()
#' matches <- cor_spec(unknown, test_lib)
#'
#' test_lib_extract <- filter_spec(test_lib,
#'   logic = grepl("polycarbonate", test_lib$metadata$polymer_class,
#'                 ignore.case = TRUE)
#' )
#'
#' matches2 <- cor_spec(unknown, library = test_lib_extract)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{adj_intens}()} converts spectra;
#' \code{\link{get_lib}()} retrieves the Open Specy reference library;
#' \code{\link{load_lib}()} loads the Open Specy reference library into an \R
#' object of choice
#'
#' @importFrom stats cor
#' @importFrom data.table data.table setorder fifelse .SD
#' @export
cor_spec <- function(x, ...) {
  UseMethod("cor_spec")
}

#' @rdname match_spec
#'
#' @export
cor_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
cor_spec.OpenSpecy <- function(x, library, na.rm = T, ...) {
  if(sum(x$wavenumber %in% library$wavenumber) < 3)
    stop("there are less than 3 matching wavenumbers in the objects you are ",
         "trying to correlate; this won't work for correlation analysis. ",
         "Consider first conforming the spectra to the same wavenumbers.",
         call. = F)

  lib <- library$spectra[library$wavenumber %in% x$wavenumber, ]
  lib <- lib[, lapply(.SD, make_rel, na.rm = na.rm)]
  lib <- lib[, lapply(.SD, mean_replace)]

  spec <- x$spectra[x$wavenumber %in% library$wavenumber,]
  spec <- spec[,lapply(.SD, make_rel, na.rm = na.rm)]
  spec <- spec[,lapply(.SD, mean_replace)]

  cor(lib, spec, ...)
}

#' @rdname match_spec
#'
#' @export
ident_spec <- function(cor_matrix, x, library, top_n = NULL,
                       add_library_metadata = NULL,
                       add_object_metadata = NULL, ...){
  if(is.numeric(top_n) && top_n > ncol(library$spectra)){
    top_n = NULL
    message("'top_n' was larger than the number of spectra in the library; ",
            "returning all matches")
  }

  out <- data.table(object_id = colnames(x$spectra),
                    library_id = rep(colnames(library$spectra),
                                     each = ncol(x$spectra)),
                    match_val = c(cor_matrix))

  if (is.character(add_library_metadata))
    out <- merge(out, library$metadata,
                 by.x = "library_id", by.y = add_library_metadata, all.x = T)
  if (is.character(add_object_metadata))
    out <- merge(out, x$metadata,
                 by.x = "object_id", by.y = add_object_metadata, all.x = T)
  if (is.numeric(top_n)) {
    setorder(out, -"match_val")
    out <- out[, head(.SD, top_n), by = "object_id"]
  }

  return(out)
}

#' @rdname match_spec
#'
#' @export
get_metadata <- function(x, ...) {
  UseMethod("get_metadata")
}

#' @rdname match_spec
#'
#' @export
get_metadata.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
get_metadata.OpenSpecy <- function(x, logic, rm_empty = TRUE, ...) {
  if(is.character(logic))
    logic <- which(names(x$spectra) %in% logic)

  res <- x$metadata[logic, ]

  if(rm_empty)
    res <- res[, !sapply(res, is_empty_vector), with = F]

  return(res)
}

#' @rdname match_spec
#'
#' @export
max_cor_named <- function(cor_matrix, na.rm = T) {
  # Find the indices of maximum correlations
  max_cor_indices <- apply(cor_matrix, 2, function(x) which.max(x))

  # Use indices to get max correlation values
  max_cor_values <- vapply(1:length(max_cor_indices), function(idx) {
    cor_matrix[max_cor_indices[idx],idx]}, FUN.VALUE = numeric(1))

  # Use indices to get the corresponding names
  names(max_cor_values) <- rownames(cor_matrix)[max_cor_indices]

  return(max_cor_values)
}

#' @rdname match_spec
#'
#' @export
filter_spec <- function(x, ...) {
  UseMethod("filter_spec")
}

#' @rdname match_spec
#'
#' @export
filter_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
filter_spec.OpenSpecy <- function(x, logic, ...) {
  if(is.character(logic)){
    logic = which(names(x$spectra) %in% logic)
  }
  x$spectra <- x$spectra[, logic, with = F]
  x$metadata <- x$metadata[logic,]

  return(x)
}
