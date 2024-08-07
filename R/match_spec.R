#' @rdname match_spec
#' @title Identify and filter spectra
#'
#' @description
#' \code{match_spec()} joins two \code{OpenSpecy} objects and their metadata
#' based on similarity.
#' \code{cor_spec()} correlates two \code{OpenSpecy} objects, typically one with
#' knowns and one with unknowns.
#' \code{ident_spec()} retrieves the top match values from a correlation matrix
#' and formats them with metadata.
#' \code{get_metadata()} retrieves metadata from OpenSpecy objects.
#' \code{max_cor_named()} formats the top correlation values from a correlation
#' matrix as a named vector.
#' \code{filter_spec()} filters an Open Specy object.
#' \code{fill_spec()} adds filler values to an \code{OpenSpecy} object where it doesn't have intensities.
#' \code{os_similarity()} EXPERIMENTAL, returns a single similarity metric between two OpenSpecy objects based on the method used.
#' @param x an \code{OpenSpecy} object, typically with unknowns.
#' @param y an \code{OpenSpecy} object to perform similarity search against x.
#' @param conform Whether to conform the spectra to the library wavenumbers or not.
#' @param type the type of conformation to make returned by \code{conform_spec()}
#' @param library an \code{OpenSpecy} or \code{glmnet} object representing the
#' reference library of spectra or model to use in identification.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating correlations. Default is \code{TRUE}.
#' @param top_n integer; specifying the number of top matches to return.
#' If \code{NULL} (default), all matches will be returned.
#' @param cor_matrix a correlation matrix for object and library,
#' can be returned by \code{cor_spec()}
#' @param order an \code{OpenSpecy} used for sorting, ideally the unprocessed
#' one; \code{NULL} skips sorting.
#' @param add_library_metadata name of a column in the library metadata to be
#' joined; \code{NULL} if you don't want to join.
#' @param add_object_metadata name of a column in the object metadata to be
#' joined; \code{NULL} if you don't want to join.
#' @param rm_empty logical; whether to remove empty columns in the metadata.
#' @param logic a logical or numeric vector describing which spectra to keep.
#' @param fill an \code{OpenSpecy} object with a single spectrum to be used to
#' fill missing values for alignment with the AI classification.
#' @param method the type of similarity metric to return.
#' @param \ldots additional arguments passed \code{\link[stats]{cor}()}.
#'
#' @return
#' \code{match_spec()} and \code{ident_spec()} will return
#' a \code{\link[data.table]{data.table-class}()} containing correlations
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
#' \code{filter_spec()} returns an \code{OpenSpecy} object.
#' \code{fill_spec()} returns an \code{OpenSpecy} object.
#' \code{cor_spec()} returns a correlation matrix.
#' \code{get_metadata()} returns a \code{\link[data.table]{data.table-class}()}
#' with the metadata for columns which have information.
#' \code{os_similarity()} returns a single numeric value representing the type
#' of similarity metric requested. 'wavenumber' similarity is based on the
#' proportion of wavenumber values that overlap between the two objects,
#' 'metadata' is the proportion of metadata column names,
#' 'hamming' is something similar to the hamming distance where we discretize
#' all spectra in the OpenSpecy object by wavenumber intensity values and then
#' relate the wavenumber intensity value distributions by mean difference in
#' min-max normalized space. 'pca' tests the distance between the OpenSpecy
#' objects in PCA space using the first 4 component values and calculating the
#' max-range normalized distance between the mean components. The first two
#' metrics are pretty straightforward and definitely ready to go, the 'hamming'
#' and 'pca' metrics are pretty experimental but appear to be working under our
#' current test cases.
#'
#' @examples
#' data("test_lib")
#'
#' unknown <- read_extdata("ftir_ldpe_soil.asp") |>
#'   read_any() |>
#'   conform_spec(range = test_lib$wavenumber,
#'                res = spec_res(test_lib)) |>
#'   process_spec()
#' cor_spec(unknown, test_lib)
#'
#' match_spec(unknown, test_lib, add_library_metadata = "sample_name",
#'            top_n = 1)
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
#' @importFrom stats cor predict prcomp
#' @importFrom glmnet predict.glmnet
#' @importFrom data.table data.table setorder fifelse .SD as.data.table rbindlist transpose
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

cor_spec.OpenSpecy <- function(x, library, na.rm = T, conform = F,
                               type = "roll", ...) {
  if(conform) x <- conform_spec(x, library$wavenumber, res = NULL, allow_na = F, type)

  if(!is.null(attr(x, "intensity_unit")) &&
     attr(x, "intensity_unit") != attr(library,  "intensity_unit"))
    warning("Intensity units between the library and unknown are not the same")

  if(!is.null(attr(x, "derivative_order")) &&
     attr(x, "derivative_order") != attr(library,  "derivative_order"))
    warning("Derivative orders between the library and unknown are not the same")

  if(!is.null(attr(x, "baseline")) &&
     attr(x, "baseline") != attr(library,  "baseline"))
    warning("Baselines between the library and unknown are not the same")

  if(!is.null(attr(x, "spectra_type")) &&
     attr(x, "spectra_type") != attr(library,  "spectra_type"))
    warning("Spectra types between the library and unknown are not the same")

  if(sum(x$wavenumber %in% library$wavenumber) < 3)
    stop("there are less than 3 matching wavenumbers in the objects you are ",
         "trying to correlate; this won't work for correlation analysis; ",
         "consider first conforming the spectra to the same wavenumbers",
         call. = F)

  if(!all(x$wavenumber %in% library$wavenumber))
    warning(paste0("some wavenumbers in 'x' are not in the library and the ",
                   "function is not using these in the identification routine: ",
                   paste(x$wavenumber[!x$wavenumber %in% library$wavenumber],
                         collapse = " ")),
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
#' @export
match_spec <- function(x, ...) {
  UseMethod("match_spec")
}

#' @rdname match_spec
#'
#' @export
match_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export

match_spec.OpenSpecy <- function(x, library, na.rm = T, conform = F,
                                 type = "roll", top_n = NULL, order = NULL,
                                 add_library_metadata = NULL,
                                 add_object_metadata = NULL, fill = NULL, ...) {
  if(is_OpenSpecy(library)) {
    res <- cor_spec(x, library = library, conform = conform, type = type) |>
      ident_spec(x, library = library, top_n = top_n,
                 add_library_metadata = add_library_metadata,
                 add_object_metadata = add_object_metadata)
  } else {
    res <- ai_classify(x, library, fill)
  }

  if(!is.null(order)) {
    .reorder <- NULL
    match <- match(colnames(order$spectra), res$object_id)
    setorder(res[, .reorder := order(match)], .reorder)[, .reorder := NULL]
  }

  return(res)
}

#' @rdname match_spec
#'
#' @export
ident_spec <- function(cor_matrix, x, library, top_n = NULL,
                       add_library_metadata = NULL,
                       add_object_metadata = NULL, ...){
  if(is.numeric(top_n) && top_n > ncol(library$spectra)){
    top_n = NULL
    message("'top_n' larger than the number of spectra in the library; ",
            "returning all matches")
  }

  out <-  as.data.table(cor_matrix, keep.rownames = T) |> melt(id.vars = "rn")

  names(out) <- c("library_id", "object_id", "match_val")

  if(is.numeric(top_n)) {
    match_val <- NULL # workaround for data.table non-standard evaluation
    setorder(out, -match_val)
    out <- out[!is.na(match_val), head(.SD, top_n), by = "object_id"]
  }

  if(is.character(add_library_metadata))
    out <- merge(out, library$metadata,
                 by.x = "library_id", by.y = add_library_metadata, all.x = T)

  if(is.character(add_object_metadata))

    out <- merge(out, x$metadata,
                 by.x = "object_id", by.y = add_object_metadata, all.x = T)

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

  if(ncol(x$spectra) == 0 | ncol(x$metadata) == 0)

    stop("the OpenSpecy object created contains zero spectra, this is not well ",
         "supported, if you have specific scenarios where this is required ",
         "please share it with the developers and we can make a workaround")

  return(x)
}

#' @rdname match_spec
#'
#' @export
ai_classify <- function(x, ...) {
  UseMethod("ai_classify")
}

#' @rdname match_spec
#'
#' @export
ai_classify.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
ai_classify.OpenSpecy <- function(x, library, fill = NULL, ...) {
  if(!is.null(fill)) {
    filled <- fill_spec(x, fill)
  } else {
    filled <- x
  }
  filled$spectra$wavenumber <- filled$wavenumber
  proc <- transpose(filled$spectra, make.names = "wavenumber") |>
    as.matrix()

  pred <- predict(library$model,
                  newx = proc,
                  min(library$model$lambda),
                  type = "response") |>
    as.data.table()
  
  names(pred)[1:3] <- c("x", "y", "z")
  pred$x <- as.integer(pred$x)
  pred$y <- as.integer(pred$y)
  pred <- merge(pred, data.table(x = 1:dim(proc)[1]), all.y = T)

  value <- NULL # workaround for data.table non-standard evaluation
  filt <- pred[, .SD[value == max(value, na.rm = T) | is.na(value)], by = "x"]

  res <- merge(filt, library$dimension_conversion, all.x = T,
               by.x = "y", by.y = "factor_num")
  setorder(res, "x")

  return(res)
}

#' @rdname match_spec
#'
#' @export
fill_spec <- function(x, ...) {
    UseMethod("fill_spec")
}

#' @rdname match_spec
#'
#' @export
fill_spec.default <- function(x, ...) {
    stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
fill_spec.OpenSpecy <- function(x, fill, ...) {
  blank_dt <- x$spectra[1,]

  blank_dt[1,] <- NA

  test <- rbindlist(
    lapply(1:length(fill$wavenumber),
           function(x) {
             blank_dt
           }
    )
  )[, lapply(.SD,
             function(x) {
               unlist(fill$spectra)
             })]
  
  test[match(x$wavenumber, fill$wavenumber),] <- x$spectra

  x$spectra <- test
  x$wavenumber <- fill$wavenumber

  return(x)
}

# OS Similarity
#' @rdname match_spec
#'
#' @export
os_similarity <- function(x, ...) {
    UseMethod("os_similarity")
}

#' @rdname match_spec
#'
#' @export
os_similarity.default <- function(x, ...) {
    stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname match_spec
#'
#' @export
os_similarity.OpenSpecy <- function(x, y, method = "hamming", na.rm = T, ...) {
  if(method == "wavenumber"){
    series = c(x$wavenumber, y$wavenumber)
    return(sum(duplicated(series)) / length(unique(series)))
  }
  if(method %in% c("pca", "hamming")){
    if(sum(x$wavenumber %in% y$wavenumber) < 3)
      stop("there are less than 3 matching wavenumbers in the objects you are ",
           "trying to correlate; this won't work for correlation analysis. ",
           "Consider first conforming the spectra to the same wavenumbers.",
           call. = F)

    series = c(x$wavenumber, y$wavenumber)

    if(sum(duplicated(series))/length(unique(series)) != 1)
      warning(paste0("some wavenumbers in 'x' are not in the 'y' and the ",
                     "function is not using these in the identification routine: ",
                     paste(unique(c(x$wavenumber[!x$wavenumber %in% y$wavenumber], y$wavenumber[!y$wavenumber %in% x$wavenumber])),
                           collapse = " ")),
              call. = F)

    if(ncol(x$spectra) + ncol(y$spectra) < 8 & method == "pca")
      stop("There must be at least 8 spectra total combined from the two Open Specy objects",
           "to conduct the pca analysis. Consider using the hamming distance if you want a multispectra-metric",
           "with fewer spectra.",
           call. = F)

    spec_y <- y$spectra[y$wavenumber %in% x$wavenumber, ]
    spec_y <- spec_y[, lapply(.SD, make_rel, na.rm = na.rm)]
    spec_y <- spec_y[, lapply(.SD, mean_replace)]
    spec_x <- x$spectra[x$wavenumber %in% y$wavenumber,]
    spec_x <- spec_x[,lapply(.SD, make_rel, na.rm = na.rm)]
    spec_x <- spec_x[, lapply(.SD, mean_replace)]

  }
  if(method == "pca"){

    perform_combined_pca <- function(spec_obj1, spec_obj2) {
      # Extract intensities and transpose
      intensities1 <- t(spec_obj1)
      intensities2 <- t(spec_obj2)

      # Combine the datasets
      combined_intensities <- rbind(intensities1, intensities2)

      # Perform PCA
      pca_result <- prcomp(combined_intensities, scale. = TRUE)

      # Determine the index range for each dataset
      index_spec_obj1 <- 1:nrow(intensities1)
      index_spec_obj2 <- (nrow(intensities1) + 1):(nrow(intensities1) + nrow(intensities2))

      # Extract PCA results for each dataset
      pca_spec_obj1 <- pca_result$x[index_spec_obj1, 1:4]
      pca_spec_obj2 <- pca_result$x[index_spec_obj2, 1:4]

      # Calculate central locations

      pca_range <- apply(pca_result$x[,1:4], 2, function(column) abs(max(column) - min(column)))

      if(is.null(dim(pca_spec_obj1))){
        central_loc1 <- pca_spec_obj1
      }
      else{
        central_loc1 <- colMeans(pca_spec_obj1)
      }
      if(is.null(dim(pca_spec_obj2))){
        central_loc2 <- pca_spec_obj2
      }
      else{
        central_loc2 <- colMeans(pca_spec_obj2)
      }

      return(list(central_loc1, central_loc2, pca_range))
    }

    central_locs <- perform_combined_pca(spec_obj1 = spec_x, spec_obj2 = spec_y)
    
    return(
      1-mean(abs(central_locs[[1]] - central_locs[[2]])/central_locs[[3]])
    )
  }
  if(method == "hamming"){
    spec_y <- transpose(spec_y)
    spec_y <- spec_y[,lapply(.SD, function(x){
      values <- make_rel(table(round(x,1)))
      sequence <- seq(0, 1, by = 0.1)
      empty <- numeric(length = length(sequence))
      empty[match(names(values), seq(0, 1, by = 0.1))] <- values
      ifelse(is.nan(empty), 1, empty)
    })]

    spec_x <- transpose(spec_x)
    spec_x <- spec_x[,lapply(.SD, function(x){
      values <- make_rel(table(round(x,1)))
      sequence <- seq(0, 1, by = 0.1)
      empty <- numeric(length = length(sequence))
      empty[match(names(values), seq(0, 1, by = 0.1))] <- values
      ifelse(is.nan(empty), 1, empty)
    })]

    return(1 - unlist(abs(spec_x - spec_y)) |> mean(na.rm = T))
  }
  if(method == "metadata"){
    series = c(names(x$metadata), names(y$metadata))
    return(sum(duplicated(series))/length(unique(series)))
  }
}
