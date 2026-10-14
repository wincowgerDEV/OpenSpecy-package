#' @rdname match_spec
#' @title Identify and filter spectra
#'
#' @description
#' \code{match_spec()} joins two \code{OpenSpecy} objects and their metadata
#' based on similarity. Trained flat models return class probabilities. Saved
#' pooled hierarchical logistic models retain their normalized joint leaf
#' probabilities. Schema-versioned gated models instead choose the top class at
#' each ordered gate, score only that selected child, and return terminal-node
#' conditional leaf probabilities without multiplying or averaging confidence
#' across levels.
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
#' @param compute the compute strategy used for correlation, "optimized" by default
#' will use the current most optimized strategy for Pearson correlation, "base"
#' will use base R's \code{cor()}
#' @param library an \code{OpenSpecy} or trained model object representing the
#' reference library of spectra or model to use in identification.
#' Legacy hierarchical artifacts must retain their root, branch, hierarchy,
#' class-conversion, predictor-axis, and training-filler runtime state. Gated
#' artifacts must retain their schema, ordered hierarchy columns, topology,
#' edges, node models, leaf conversion, predictor axis, and filler.
#' @param na.rm logical; indicating whether missing values should be removed
#' when calculating correlations. Default is \code{TRUE}.
#' @param top_n integer; specifying the number of top matches to return.
#' For spectral libraries, \code{NULL} returns all matches. Model libraries
#' preserve their historical single winning class when \code{NULL}; a positive
#' value returns that many ranked class probabilities per spectrum. For a gated
#' model, ranking is restricted to leaves in the routed terminal node, and
#' \code{value} is \eqn{P(leaf | selected path)}, not an end-to-end path
#' probability.
#' @param top_n_by optional single library metadata column name. When supplied
#' for a spectral library, \code{top_n} matches are retained independently for
#' each non-missing group (for example, \code{"organization"}). It is not
#' supported for trained model libraries.
#' @param batch_size optional positive integer number of query spectra to match
#' per correlation block. For spectral libraries this bounds peak memory and
#' requires a finite \code{top_n}; \code{NULL} preserves the ordinary dense
#' correlation path unless \code{top_n_by} requires grouped matching. It is not
#' supported for trained model libraries.
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
#' fill missing values for alignment with AI classification. When omitted and
#' the model library contains a \code{fill} object, that stored training filler
#' is used. Finite query values replace the filler; query \code{NA}s do not.
#' @param method the type of similarity metric to return.
#' @param \ldots additional arguments passed \code{\link[stats]{cor}()}.
#'
#' @return
#' With a spectral reference library, \code{match_spec()} and
#' \code{ident_spec()} return a
#' \code{\link[data.table]{data.table-class}()} containing correlations between
#' spectra and the library. The table has \code{object_id}, \code{library_id},
#' and \code{match_val}; each row is one pairwise match retained by
#' \code{top_n}. With a trained model, \code{match_spec()} returns model row
#' index \code{x}, class index \code{y}, selected tuning value \code{z}, score
#' \code{value}, \code{rank}, and class \code{name}.
#' If \code{add_library_metadata} is \code{is.character}, the library metadata
#' will be added to the output.
#' If \code{add_object_metadata} is \code{is.character}, the object metadata
#' will be added to the output.
#' Gated model rows also contain \code{terminal_node}, one
#' \code{routed_<hierarchy column>} field per gate, and its corresponding
#' \code{<hierarchy column>_probability}. These gate probabilities are audit
#' fields and are not combined with the returned conditional leaf \code{value}.
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
#' test_lib$metadata[["organization"]] <- rep(
#'   c("collection_a", "collection_b"), length.out = nrow(test_lib$metadata)
#' )
#' match_spec(unknown, test_lib, top_n = 1, top_n_by = "organization")
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
#' @importFrom data.table data.table setorder .SD as.data.table rbindlist
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
                               type = "roll", compute = "optimized",
                               ...) {
  inputs <- .cor_spec_inputs(x, library, conform = conform, type = type)

  lib <- inputs$library$spectra[inputs$library_rows, , drop = FALSE]
  lib <- make_rel(lib, na.rm = na.rm)
  lib <- .matrix_mean_replace(lib)

  spec <- inputs$x$spectra[inputs$x_rows, , drop = FALSE]
  spec <- make_rel(spec, na.rm = na.rm)
  spec <- .matrix_mean_replace(spec)

  if(compute == "optimized"){
      # Self-correlation is common in medoid selection. The one-matrix
      # tcrossprod() path uses the symmetric BLAS kernel and avoids scaling and
      # multiplying the same matrix twice while returning the same full matrix.
      if(identical(lib, spec)) return(.fast_correlation(lib))
      return(.fast_correlation(lib, spec, ...))
  }
  if(compute == "base"){
      return(cor(lib, spec, ...))
  }
}

.cor_spec_inputs <- function(x, library, conform = FALSE, type = "roll") {
  x <- as_OpenSpecy(x)
  if (is_OpenSpecy(library)) library <- as_OpenSpecy(library)

  if(conform) x <- conform_spec(x, library$wavenumber, res = NULL, allow_na = F, type)

  if(!is.null(attr(x, "intensity_unit")) &&
     !is.null(attr(library, "intensity_unit")) &&
     !identical(attr(x, "intensity_unit"), attr(library, "intensity_unit")))
    warning("Intensity units between the library and unknown are not the same")

  if(!is.null(attr(x, "derivative_order")) &&
     !is.null(attr(library, "derivative_order")) &&
     !identical(attr(x, "derivative_order"), attr(library, "derivative_order")))
    warning("Derivative orders between the library and unknown are not the same")

  if(!is.null(attr(x, "baseline")) &&
     !is.null(attr(library, "baseline")) &&
     !identical(attr(x, "baseline"), attr(library, "baseline")))
    warning("Baselines between the library and unknown are not the same")

  if(!is.null(attr(x, "spectra_type")) &&
     !is.null(attr(library, "spectra_type")) &&
     !identical(attr(x, "spectra_type"), attr(library, "spectra_type")))
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

  list(
    x = x,
    library = library,
    x_rows = x$wavenumber %in% library$wavenumber,
    library_rows = library$wavenumber %in% x$wavenumber
  )
}

.scale_correlation_spectra <- function(x) {
  x <- t(x)
  x <- x - rowMeans(x)
  x / sqrt(rowSums(x^2))
}

.fast_correlation <- function(x, y = NULL) {
  mat_1 <- .scale_correlation_spectra(x)
  if(!is.null(y)){
    mat_2 <- .scale_correlation_spectra(y)
    # tcrossprod() keeps the heavy work in BLAS; the transpose is retained here
    # because benchmarks show it is faster for the spectra-by-wavenumber layout.
    mat_3 <- tcrossprod(mat_1, mat_2)
    colnames(mat_3) <- colnames(y)
    rownames(mat_3) <- colnames(x)
    return(mat_3)
  }
  mat_3 <- tcrossprod(mat_1)
  colnames(mat_3) <- colnames(x)
  rownames(mat_3) <- colnames(x)
  return(mat_3)
}

# Internal bounded matcher used by in-memory app workflows. Correlation is
# calculated for at most `block_size` query spectra, ranked immediately, and
# discarded so callers never retain the full library-by-query matrix.
.match_spec_blockwise <- function(x, library, top_n = 10L, top_n_by = NULL,
                                  block_size = 100L, na.rm = TRUE,
                                  conform = FALSE, type = "roll",
                                  progress = NULL, ...) {
  if(length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) ||
     !is.finite(top_n) || top_n < 1 || top_n > .Machine$integer.max ||
     top_n != floor(top_n)) {
    stop("'top_n' must be a positive integer", call. = FALSE)
  }
  top_n <- as.integer(top_n)
  if(!is.null(progress) && !is.function(progress)) {
    stop("'progress' must be NULL or a function", call. = FALSE)
  }

  candidate_block_size <- tryCatch(
    suppressWarnings(as.integer(block_size)),
    error = function(error) NA_integer_
  )
  if(length(candidate_block_size) != 1L || is.na(candidate_block_size) ||
     candidate_block_size < 1L) {
    candidate_block_size <- 100L
  }

  inputs <- .cor_spec_inputs(x, library, conform = conform, type = type)
  library_spectra <- inputs$library$spectra[
    inputs$library_rows, , drop = FALSE
  ]
  library_count <- ncol(library_spectra)
  query_count <- ncol(inputs$x$spectra)
  groups <- .library_match_groups(inputs$library, top_n_by)
  group_rows <- if (is.null(groups)) {
    list(all = seq_len(library_count))
  } else {
    split(seq_len(library_count), groups, drop = TRUE)
  }
  retained_per_query <- sum(vapply(
    group_rows, function(rows) min(top_n, length(rows)), integer(1L)
  ))
  if(query_count == 0L || library_count == 0L) {
    return(data.table(
      object_id = character(), library_id = character(), match_val = numeric()
    ))
  }
  capacity <- .blockwise_retained_capacity(query_count, retained_per_query)
  library_ids <- colnames(library_spectra)
  query_ids <- colnames(inputs$x$spectra)

  starts <- seq.int(1L, query_count, by = candidate_block_size)
  result_rows <- as.integer(capacity$rows)
  object_id <- character(result_rows)
  library_id <- character(result_rows)
  match_val <- numeric(result_rows)
  cursor <- 1
  total_blocks <- length(starts) * length(group_rows)
  completed_blocks <- 0L
  for(group_index in seq_along(group_rows)) {
    library_columns <- group_rows[[group_index]]
    group_library <- library_spectra[, library_columns, drop = FALSE]
    group_library <- make_rel(group_library, na.rm = na.rm)
    group_library <- .matrix_mean_replace(group_library)
    scaled_library <- .scale_correlation_spectra(group_library)
    rm(group_library)
    for(i in seq_along(starts)) {
      columns <- seq.int(
        starts[[i]], min(query_count, starts[[i]] + candidate_block_size - 1L)
      )
      query_block <- inputs$x$spectra[
        inputs$x_rows, columns, drop = FALSE
      ]
      query_block <- make_rel(query_block, na.rm = na.rm)
      query_block <- .matrix_mean_replace(query_block)
      scaled_query_block <- .scale_correlation_spectra(query_block)
      rm(query_block)
      scores <- tcrossprod(scaled_library, scaled_query_block)
      rownames(scores) <- library_ids[library_columns]
      colnames(scores) <- query_ids[columns]
      block <- .top_match_rows(scores, min(top_n, length(library_columns)))
      rows <- seq.int(cursor, length.out = nrow(block))
      object_id[rows] <- block$object_id
      library_id[rows] <- block$library_id
      match_val[rows] <- block$match_val
      cursor <- cursor + nrow(block)
      completed_blocks <- completed_blocks + 1L
      if(!is.null(progress)) {
        .report_match_progress(
          progress, completed_blocks, total_blocks,
          group = names(group_rows)[[group_index]],
          completed_groups = group_index, total_groups = length(group_rows),
          completed_query_blocks = i, total_query_blocks = length(starts)
        )
      }
    }
    rm(scaled_library)
  }
  out <- data.table(
    object_id = object_id, library_id = library_id, match_val = match_val
  )
  object_order <- match(out$object_id, query_ids)
  library_order <- match(out$library_id, library_ids)
  out[order(object_order, -match_val, library_order, na.last = TRUE)]
}

.report_match_progress <- function(progress, completed_blocks, total_blocks,
                                   ...) {
  details <- list(...)
  accepted <- names(formals(progress))
  args <- c(list(completed_blocks = completed_blocks,
                 total_blocks = total_blocks), details)
  if (!is.null(accepted) && !"..." %in% accepted) {
    args <- args[intersect(names(args), accepted)]
  }
  do.call(progress, args)
}

.blockwise_retained_capacity <- function(query_count, top_n,
                                         bytes_per_row = 32) {
  rows <- as.double(query_count) * as.double(top_n)
  bytes <- rows * as.double(bytes_per_row)
  max_bytes <- suppressWarnings(as.numeric(getOption(
    "OpenSpecy.blockwise_max_retained_bytes", 10 * 1024^3
  )))
  if(length(max_bytes) != 1L || is.na(max_bytes) || !is.finite(max_bytes) ||
     max_bytes <= 0) {
    max_bytes <- 10 * 1024^3
  }
  if(!is.finite(rows) || rows > .Machine$integer.max ||
     !is.finite(bytes) || bytes > max_bytes) {
    stop(
      "Retaining Top N would require too many result rows (approximately ",
      format(rows, scientific = FALSE, big.mark = ",", trim = TRUE),
      "). Lower Top N or split the dataset before identification.",
      call. = FALSE
    )
  }
  list(rows = rows, bytes = bytes, max_bytes = max_bytes)
}

.top_match_rows <- function(cor_matrix, top_n) {
  orders <- vapply(seq_len(ncol(cor_matrix)), function(column) {
    head(order(cor_matrix[, column], decreasing = TRUE, na.last = TRUE,
               method = "radix"), top_n)
  }, FUN.VALUE = integer(top_n))
  orders <- matrix(orders, nrow = top_n, ncol = ncol(cor_matrix))
  order_vector <- as.vector(orders)
  column_vector <- rep(seq_len(ncol(cor_matrix)), each = top_n)

  data.table(
    object_id = rep(colnames(cor_matrix), each = top_n),
    library_id = rownames(cor_matrix)[order_vector],
    match_val = cor_matrix[cbind(order_vector, column_vector)]
  )
}

.library_match_groups <- function(library, top_n_by = NULL) {
  if (is.null(top_n_by)) return(NULL)
  if (!is.character(top_n_by) || length(top_n_by) != 1L ||
      is.na(top_n_by) || !nzchar(top_n_by)) {
    stop("'top_n_by' must be NULL or one library metadata column name",
         call. = FALSE)
  }
  if (is.null(library$metadata) || !top_n_by %in% names(library$metadata)) {
    stop("'top_n_by' column '", top_n_by,
         "' is not present in the library metadata", call. = FALSE)
  }
  groups <- trimws(as.character(library$metadata[[top_n_by]]))
  library_count <- if (!is.null(library$spectra)) {
    ncol(library$spectra)
  } else {
    ncol(library$values)
  }
  if (length(groups) != library_count ||
      anyNA(groups) || any(!nzchar(groups))) {
    stop("'top_n_by' column '", top_n_by,
         "' must contain one nonblank value per library spectrum",
         call. = FALSE)
  }
  factor(groups, levels = unique(groups))
}

.validate_grouped_top_n <- function(top_n, top_n_by) {
  if (is.null(top_n_by)) return(invisible(NULL))
  if (length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) ||
      !is.finite(top_n) || top_n < 1 || top_n != floor(top_n)) {
    stop("a positive integer 'top_n' is required when 'top_n_by' is used",
         call. = FALSE)
  }
  invisible(NULL)
}

.validate_match_batch_size <- function(batch_size) {
  if (is.null(batch_size)) return(NULL)
  if (length(batch_size) != 1L || !is.numeric(batch_size) ||
      is.na(batch_size) || !is.finite(batch_size) || batch_size < 1 ||
      batch_size > .Machine$integer.max || batch_size != floor(batch_size)) {
    stop("'batch_size' must be a positive integer or NULL", call. = FALSE)
  }
  as.integer(batch_size)
}

.append_match_metadata <- function(res, x, library,
                                   add_library_metadata = NULL,
                                   add_object_metadata = NULL) {
  if (!is.character(add_library_metadata) &&
      !is.character(add_object_metadata)) return(res)
  res <- data.table::copy(data.table::as.data.table(res))
  res[, ".match_result_order" := list(seq_len(nrow(res)))]
  if (is.character(add_library_metadata)) {
    res <- merge(
      res, library$metadata, by.x = "library_id",
      by.y = add_library_metadata, all.x = TRUE, sort = FALSE
    )
  }
  if (is.character(add_object_metadata)) {
    res <- merge(
      res, x$metadata, by.x = "object_id",
      by.y = add_object_metadata, all.x = TRUE, sort = FALSE
    )
  }
  data.table::setorderv(res, ".match_result_order")
  res[, ".match_result_order" := NULL]
  res
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
                                 top_n_by = NULL, batch_size = NULL,
                                 add_library_metadata = NULL,
                                 add_object_metadata = NULL, 
                                 compute = "optimized",
                                 fill = NULL, ...) {
  .validate_grouped_top_n(top_n, top_n_by)
  batch_size <- .validate_match_batch_size(batch_size)
  if(is_OpenSpecy(library)) {
    bounded <- !is.null(batch_size) || !is.null(top_n_by)
    if (isTRUE(bounded) && (is.null(top_n) || length(top_n) != 1L ||
        !is.numeric(top_n) || is.na(top_n) || !is.finite(top_n) ||
        top_n < 1 || top_n != floor(top_n))) {
      stop("a positive integer 'top_n' is required when 'batch_size' or ",
           "'top_n_by' is used", call. = FALSE)
    }
    if (!isTRUE(bounded)) {
      res <- cor_spec(x, library = library, conform = conform, type = type,
                      compute = compute) |>
        ident_spec(x, library = library, top_n = top_n,
                   add_library_metadata = add_library_metadata,
                   add_object_metadata = add_object_metadata)
    } else {
      res <- .match_spec_blockwise(
        x, library = library, top_n = top_n, top_n_by = top_n_by,
        block_size = if (is.null(batch_size)) 1000L else batch_size,
        na.rm = na.rm, conform = conform, type = type, ...
      )
      res <- .append_match_metadata(
        res, x, library, add_library_metadata, add_object_metadata
      )
    }
  } else {
    if (!is.null(top_n_by)) {
      stop("'top_n_by' is not supported for trained model libraries",
           call. = FALSE)
    }
    if (!is.null(batch_size)) {
      stop("'batch_size' is not supported for trained model libraries",
           call. = FALSE)
    }
    if (is.null(fill) && is.list(library) && is_OpenSpecy(library$fill)) {
      fill <- library$fill
    }
    res <- ai_classify(x, library = library, fill = fill, top_n = top_n)
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
    match_val <- NULL

    if(is.null(top_n) || top_n > nrow(cor_matrix)){
        top_n = nrow(cor_matrix)
        message("'top_n' larger than the number of spectra in the library; ",
                "returning all matches")
    }
    
    lib_names <- rownames(cor_matrix)
    unk_ids   <- colnames(cor_matrix)
    top_n <- as.integer(top_n)
    
    # Preallocate the result vectors once; this avoids creating and rbinding one
    # data.table per spectrum for hyperspectral maps.
    ord <- matrix(NA_integer_, nrow = top_n, ncol = ncol(cor_matrix))
    for (j in seq_len(ncol(cor_matrix))) {
        ord[, j] <- head(order(cor_matrix[, j], decreasing = TRUE), top_n)
    }
    ord_vec <- as.vector(ord)
    col_vec <- rep(seq_len(ncol(cor_matrix)), each = top_n)
    out <- data.table(
        object_id  = rep(unk_ids, each = top_n),
        library_id = lib_names[ord_vec],
        match_val  = cor_matrix[cbind(ord_vec, col_vec)]
    )
    
    setorder(out, -match_val)
    
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
  x <- as_OpenSpecy(x)

  if(is.character(logic))
    logic <- which(colnames(x$spectra) %in% logic)

  res <- x$metadata[logic, ]

  if(rm_empty)
    res <- res[, !sapply(res, is_empty_vector), with = F]

  return(res)
}

#' @rdname match_spec
#'
#' @export
max_cor_named <- function(cor_matrix, na.rm = T) {
  cor_no_na <- cor_matrix
  cor_no_na[is.na(cor_no_na)] <- -Inf
  max_cor_indices <- max.col(t(cor_no_na), ties.method = "first")
  all_na <- colSums(!is.na(cor_matrix)) == 0L
  max_cor_indices[all_na] <- NA_integer_

  max_cor_values <- rep(NA_real_, length(max_cor_indices))
  ok <- !is.na(max_cor_indices)
  max_cor_values[ok] <- cor_matrix[cbind(max_cor_indices[ok], which(ok))]
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
  x <- as_OpenSpecy(x)

  if(is.character(logic)){
    logic = which(colnames(x$spectra) %in% logic)
  }
  if (is.logical(logic)) {
    if (length(logic) != ncol(x$spectra)) {
      stop("logical filters must have the same length as the number of spectra",
           call. = FALSE)
    }
    logic[is.na(logic)] <- FALSE
  }
  if (anyNA(logic)) {
    stop("filters must not contain NA values", call. = FALSE)
  }
  x$spectra <- x$spectra[, logic, drop = FALSE]
  x$metadata <- x$metadata[logic,]

  if(ncol(x$spectra) == 0 | nrow(x$metadata) == 0)

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

.ai_logistic_probability_matrix <- function(model, newx, n) {
  if (isTRUE(model$deterministic)) {
    return(matrix(
      1, nrow = n, ncol = 1L,
      dimnames = list(rownames(newx), "1")
    ))
  }
  if (is.null(model$model) || !inherits(model$model, "glmnet")) {
    stop("A hierarchical logistic node has no deployable glmnet model",
         call. = FALSE)
  }
  probabilities <- .lib_probability_matrix(
    predict(
      model$model, newx = newx, s = model$lambda_selected,
      type = "response"
    ),
    n = n
  )
  conversion <- data.table::as.data.table(model$dimension_conversion)
  if (ncol(probabilities) != nrow(conversion)) {
    stop("Hierarchical node probabilities do not match its class conversion",
         call. = FALSE)
  }
  colnames(probabilities) <- as.character(conversion$factor_num)
  probabilities
}

.ai_hierarchical_probabilities <- function(newx, library) {
  required <- c(
    "broad_dimension_conversion", "branch_models", "dimension_conversion"
  )
  missing <- required[!vapply(required, function(name) {
    !is.null(library[[name]])
  }, logical(1L))]
  if (length(missing)) {
    stop(
      "Hierarchical model is missing runtime state: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  n <- nrow(newx)
  broad_model <- list(
    model = library$model,
    deterministic = isTRUE(library$broad_deterministic),
    lambda_selected = library$lambda_selected,
    dimension_conversion = library$broad_dimension_conversion
  )
  broad_probability <- .ai_logistic_probability_matrix(
    broad_model, newx, n
  )
  broad_conversion <- data.table::as.data.table(
    library$broad_dimension_conversion
  )
  leaf_conversion <- data.table::as.data.table(library$dimension_conversion)
  probabilities <- matrix(
    0, nrow = n, ncol = nrow(leaf_conversion),
    dimnames = list(rownames(newx), as.character(leaf_conversion$factor_num))
  )
  assigned <- rep.int(0L, nrow(leaf_conversion))
  for (branch in as.character(broad_conversion$name)) {
    node <- library$branch_models[[branch]]
    if (is.null(node)) {
      stop("Hierarchical model has no branch for '", branch, "'",
           call. = FALSE)
    }
    conditional <- .ai_logistic_probability_matrix(node, newx, n)
    branch_factor <- broad_conversion$factor_num[match(
      branch, broad_conversion$name
    )]
    branch_probability <- broad_probability[, as.character(branch_factor)]
    node_conversion <- data.table::as.data.table(node$dimension_conversion)
    for (leaf in as.character(node_conversion$name)) {
      leaf_factor <- leaf_conversion$factor_num[match(
        leaf, leaf_conversion$name
      )]
      if (is.na(leaf_factor)) {
        stop("Hierarchical branch contains an unknown leaf class '", leaf,
             "'", call. = FALSE)
      }
      child_factor <- node_conversion$factor_num[match(
        leaf, node_conversion$name
      )]
      probabilities[, as.character(leaf_factor)] <-
        branch_probability * conditional[, as.character(child_factor)]
      assigned[[leaf_factor]] <- assigned[[leaf_factor]] + 1L
    }
  }
  if (any(assigned != 1L)) {
    stop("Every hierarchical leaf class must occur in exactly one branch",
         call. = FALSE)
  }
  probabilities <- .lib_normalize_probability_rows(probabilities)
  array(
    probabilities,
    dim = c(nrow(probabilities), ncol(probabilities), 1L),
    dimnames = list(
      rownames(newx), colnames(probabilities), "hierarchical_joint"
    )
  )
}

.ai_validate_gated_runtime <- function(library) {
  required <- c(
    "model_schema", "hierarchy_cols", "root_node_id", "gated_nodes",
    "gated_topology", "gated_edges", "dimension_conversion"
  )
  missing <- required[!vapply(required, function(name) {
    !is.null(library[[name]])
  }, logical(1L))]
  if (length(missing)) {
    stop(
      "Gated hierarchical model is missing runtime state: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (!identical(library$model_schema, "OpenSpecy_gated_hierarchy_v1")) {
    stop(
      "Unsupported gated hierarchical model schema: ",
      as.character(library$model_schema), call. = FALSE
    )
  }
  hierarchy_cols <- as.character(library$hierarchy_cols)
  if (!length(hierarchy_cols) || anyNA(hierarchy_cols) ||
      any(!nzchar(trimws(hierarchy_cols))) || anyDuplicated(hierarchy_cols)) {
    stop("Gated hierarchy columns are invalid", call. = FALSE)
  }
  topology <- data.table::copy(data.table::as.data.table(
    library$gated_topology
  ))
  edges <- data.table::copy(data.table::as.data.table(library$gated_edges))
  leaf_conversion <- data.table::copy(data.table::as.data.table(
    library$dimension_conversion
  ))
  topology_required <- c(
    "node_id", "parent_id", "incoming_label", "depth", "target_col",
    "terminal"
  )
  edge_required <- c("node_id", "label", "child_node_id")
  conversion_required <- c("factor_num", "name")
  if (!all(topology_required %in% names(topology)) ||
      !all(edge_required %in% names(edges)) ||
      !all(conversion_required %in% names(leaf_conversion))) {
    stop("Gated hierarchy runtime tables are malformed", call. = FALSE)
  }
  topology[, `:=`(
    node_id = as.character(node_id), parent_id = as.character(parent_id),
    incoming_label = as.character(incoming_label),
    target_col = as.character(target_col)
  )]
  edges[, `:=`(
    node_id = as.character(node_id), label = as.character(label),
    child_node_id = as.character(child_node_id)
  )]
  leaf_conversion[, name := as.character(name)]
  if (!nrow(topology) || anyNA(topology$node_id) ||
      any(!nzchar(topology$node_id)) || anyDuplicated(topology$node_id) ||
      anyNA(topology$depth) || any(!is.finite(topology$depth)) ||
      any(topology$depth < 0 | topology$depth != floor(topology$depth)) ||
      !is.logical(topology$terminal) || anyNA(topology$terminal)) {
    stop("Gated hierarchy topology has invalid or duplicate nodes",
         call. = FALSE)
  }
  root_id <- as.character(library$root_node_id)
  root_rows <- which(is.na(topology$parent_id))
  if (length(root_id) != 1L || is.na(root_id) || !nzchar(root_id) ||
      length(root_rows) != 1L || topology$node_id[root_rows] != root_id ||
      topology$depth[root_rows] != 0L ||
      !is.na(topology$incoming_label[root_rows])) {
    stop("Gated hierarchy has no unique root node", call. = FALSE)
  }
  nodes <- library$gated_nodes
  if (!is.list(nodes) || is.null(names(nodes)) || anyDuplicated(names(nodes)) ||
      !setequal(topology$node_id, names(nodes))) {
    stop("Gated hierarchy runtime nodes do not match its topology",
         call. = FALSE)
  }
  if (!nrow(leaf_conversion) || anyNA(leaf_conversion$name) ||
      any(!nzchar(leaf_conversion$name)) ||
      !is.numeric(leaf_conversion$factor_num) ||
      anyNA(leaf_conversion$factor_num) ||
      any(!is.finite(leaf_conversion$factor_num)) ||
      any(leaf_conversion$factor_num < 1 |
            leaf_conversion$factor_num != floor(leaf_conversion$factor_num)) ||
      anyDuplicated(leaf_conversion$name) ||
      anyDuplicated(leaf_conversion$factor_num)) {
    stop("Gated hierarchy leaf conversion is not unique", call. = FALSE)
  }

  child_rows <- setdiff(seq_len(nrow(topology)), root_rows)
  if (anyNA(topology$parent_id[child_rows]) ||
      any(!nzchar(topology$parent_id[child_rows])) ||
      anyNA(topology$incoming_label[child_rows]) ||
      any(!nzchar(topology$incoming_label[child_rows]))) {
    stop("Gated hierarchy child nodes require a parent and incoming label",
         call. = FALSE)
  }
  parent_position <- match(topology$parent_id[child_rows], topology$node_id)
  if (anyNA(parent_position) ||
      any(topology$terminal[parent_position]) ||
      any(topology$depth[child_rows] != topology$depth[parent_position] + 1L)) {
    stop("Gated hierarchy parent/depth relationships are invalid",
         call. = FALSE)
  }
  terminal_depth <- length(hierarchy_cols)
  terminal_target <- unique(topology$target_col[topology$terminal])
  terminal_target <- terminal_target[
    !is.na(terminal_target) & nzchar(trimws(terminal_target))
  ]
  if (length(terminal_target) != 1L) {
    stop("Gated hierarchy terminal nodes must share one target column",
         call. = FALSE)
  }
  expected_target <- ifelse(
    topology$terminal, terminal_target,
    hierarchy_cols[pmin(topology$depth + 1L, terminal_depth)]
  )
  if (any(topology$terminal & topology$depth != terminal_depth) ||
      any(!topology$terminal & topology$depth >= terminal_depth) ||
      any(topology$target_col != expected_target)) {
    stop("Gated hierarchy depths and target columns are inconsistent",
         call. = FALSE)
  }

  expected_edges <- topology[child_rows, .(
    node_id = parent_id, label = incoming_label, child_node_id = node_id
  )]
  actual_edges <- edges[, .(node_id, label, child_node_id)]
  if (anyNA(actual_edges$node_id) || anyNA(actual_edges$label) ||
      anyNA(actual_edges$child_node_id) ||
      any(!nzchar(actual_edges$node_id)) || any(!nzchar(actual_edges$label)) ||
      any(!nzchar(actual_edges$child_node_id))) {
    stop("Gated hierarchy edges contain missing values", call. = FALSE)
  }
  data.table::setorderv(expected_edges, names(expected_edges))
  data.table::setorderv(actual_edges, names(actual_edges))
  if (!identical(expected_edges, actual_edges)) {
    stop("Gated hierarchy edges do not match its parent topology",
         call. = FALSE)
  }
  reached <- root_id
  repeat {
    next_nodes <- unique(actual_edges$child_node_id[
      actual_edges$node_id %in% reached
    ])
    expanded <- union(reached, next_nodes)
    if (length(expanded) == length(reached)) break
    reached <- expanded
  }
  if (!setequal(reached, topology$node_id)) {
    stop("Gated hierarchy contains unreachable nodes", call. = FALSE)
  }

  terminal_leaves <- character()
  for (node_id in topology$node_id) {
    node <- nodes[[node_id]]
    conversion <- data.table::as.data.table(node$dimension_conversion)
    if (!all(conversion_required %in% names(conversion)) || !nrow(conversion)) {
      stop("Gated hierarchy node conversion is malformed at '", node_id,
           "'", call. = FALSE)
    }
    conversion_names <- as.character(conversion$name)
    if (anyNA(conversion_names) || any(!nzchar(conversion_names)) ||
        !is.numeric(conversion$factor_num) || anyNA(conversion$factor_num) ||
        any(!is.finite(conversion$factor_num)) ||
        any(conversion$factor_num < 1 |
              conversion$factor_num != floor(conversion$factor_num)) ||
        anyDuplicated(conversion_names) ||
        anyDuplicated(conversion$factor_num)) {
      stop("Gated hierarchy node conversion is malformed at '", node_id,
           "'", call. = FALSE)
    }
    if (isTRUE(node$deterministic)) {
      if (nrow(conversion) != 1L) {
        stop("A deterministic gated node must have exactly one class at '",
             node_id, "'", call. = FALSE)
      }
    } else if (is.null(node$model) || !inherits(node$model, "glmnet") ||
               length(node$lambda_selected) != 1L ||
               !is.finite(node$lambda_selected)) {
      stop("Gated hierarchy node has no deployable glmnet model at '",
           node_id, "'", call. = FALSE)
    }
    terminal <- topology$terminal[topology$node_id == node_id]
    outgoing <- actual_edges$label[actual_edges$node_id == node_id]
    if (isTRUE(terminal)) {
      if (length(outgoing)) {
        stop("Gated hierarchy terminal nodes cannot have child edges",
             call. = FALSE)
      }
      terminal_leaves <- c(terminal_leaves, conversion_names)
    } else if (!setequal(outgoing, conversion_names) ||
               anyDuplicated(outgoing)) {
      stop("Gated hierarchy edge labels do not match node classes at '",
           node_id, "'", call. = FALSE)
    }
  }
  if (anyDuplicated(terminal_leaves) ||
      !setequal(terminal_leaves, leaf_conversion$name)) {
    stop("Every gated terminal leaf must occur exactly once globally",
         call. = FALSE)
  }
  list(
    hierarchy_cols = hierarchy_cols, topology = topology, edges = edges,
    leaf_conversion = leaf_conversion, nodes = nodes, root_id = root_id
  )
}

.ai_validate_gated_probabilities <- function(probabilities, node_id) {
  if (any(!is.finite(probabilities))) {
    stop("Gated node '", node_id, "' returned non-finite probabilities",
         call. = FALSE)
  }
  if (any(probabilities < 0 | probabilities > 1) ||
      any(rowSums(probabilities) <= 0)) {
    stop("Gated node '", node_id,
         "' returned invalid or non-positive probabilities",
         call. = FALSE)
  }
  invisible(probabilities)
}

.ai_gated_prediction_table <- function(newx, library, top_n = 1L) {
  if (length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) ||
      !is.finite(top_n) || top_n < 1 || top_n != floor(top_n)) {
    stop("'top_n' must be a positive integer", call. = FALSE)
  }
  top_n <- as.integer(top_n)
  runtime <- .ai_validate_gated_runtime(library)
  hierarchy_cols <- runtime$hierarchy_cols
  topology <- runtime$topology
  edges <- runtime$edges
  leaf_conversion <- runtime$leaf_conversion
  nodes <- runtime$nodes
  root_id <- runtime$root_id

  n <- nrow(newx)
  active <- stats::setNames(vector("list", nrow(topology)), topology$node_id)
  active[[root_id]] <- seq_len(n)
  routed_names <- matrix(
    NA_character_, nrow = n, ncol = length(hierarchy_cols),
    dimnames = list(rownames(newx), hierarchy_cols)
  )
  routed_probabilities <- matrix(
    NA_real_, nrow = n, ncol = length(hierarchy_cols),
    dimnames = list(rownames(newx), hierarchy_cols)
  )
  reached_terminal <- rep.int(FALSE, n)
  terminal_results <- list()
  ordered_nodes <- topology[order(depth, node_id), node_id]

  for (node_id in ordered_nodes) {
    current_node_id <- node_id
    rows <- active[[current_node_id]]
    if (!length(rows)) next
    node_row <- topology[topology$node_id == current_node_id]
    node <- nodes[[current_node_id]]
    probabilities <- .ai_logistic_probability_matrix(
      node, newx[rows, , drop = FALSE], length(rows)
    )
    .ai_validate_gated_probabilities(probabilities, current_node_id)
    conversion <- data.table::as.data.table(node$dimension_conversion)

    if (isTRUE(node_row$terminal[[1L]])) {
      keep_n <- min(top_n, ncol(probabilities))
      ordered <- t(vapply(seq_len(length(rows)), function(row) {
        head(order(probabilities[row, ], decreasing = TRUE, method = "radix"),
             keep_n)
      }, integer(keep_n)))
      row_position <- rep(seq_along(rows), each = keep_n)
      class_column <- as.vector(t(ordered))
      values <- probabilities[cbind(row_position, class_column)]
      leaf_names <- as.character(conversion$name[class_column])
      leaf_factor <- leaf_conversion$factor_num[
        match(leaf_names, leaf_conversion$name)
      ]
      lambda <- if (is.null(node$lambda_selected) ||
                    !length(node$lambda_selected)) {
        NA_real_
      } else {
        node$lambda_selected[[1L]]
      }
      block <- data.table::data.table(
        x = rep(rows, each = keep_n),
        y = suppressWarnings(as.integer(leaf_factor)),
        z = rep(lambda, length(row_position)),
        value = as.numeric(values),
        rank = rep(seq_len(keep_n), times = length(rows)),
        terminal_node = rep(current_node_id, length(row_position))
      )
      for (level in seq_along(hierarchy_cols)) {
        block[[paste0("routed_", hierarchy_cols[[level]])]] <-
          rep(routed_names[rows, level], each = keep_n)
        block[[paste0(hierarchy_cols[[level]], "_probability")]] <-
          rep(routed_probabilities[rows, level], each = keep_n)
      }
      terminal_results[[length(terminal_results) + 1L]] <- block
      reached_terminal[rows] <- TRUE
      next
    }

    gate <- as.integer(node_row$depth[[1L]]) + 1L
    if (gate > length(hierarchy_cols) ||
        !identical(as.character(node_row$target_col[[1L]]),
                   hierarchy_cols[[gate]])) {
      stop("Gated hierarchy topology has an invalid gate at '",
           current_node_id,
           "'", call. = FALSE)
    }
    selected <- max.col(probabilities, ties.method = "first")
    selected_name <- rep.int(NA_character_, length(rows))
    selected_probability <- rep.int(NA_real_, length(rows))
    valid <- !is.na(selected)
    selected_name[valid] <- as.character(conversion$name[selected[valid]])
    selected_probability[valid] <- probabilities[cbind(
      which(valid), selected[valid]
    )]
    routed_names[rows, gate] <- selected_name
    routed_probabilities[rows, gate] <- selected_probability

    node_edges <- edges[edges$node_id == current_node_id]
    for (label in unique(selected_name[valid])) {
      child <- node_edges$child_node_id[node_edges$label == label]
      if (length(child) != 1L || !child %in% topology$node_id) {
        stop("Gated hierarchy has no unique child for '", label,
             "' at ", current_node_id, call. = FALSE)
      }
      selected_rows <- rows[!is.na(selected_name) & selected_name == label]
      active[[child]] <- c(active[[child]], selected_rows)
    }
  }

  if (any(!reached_terminal)) {
    stop("Gated hierarchy did not route every spectrum to a terminal node",
         call. = FALSE)
  }
  result <- data.table::rbindlist(terminal_results, use.names = TRUE,
                                  fill = TRUE)
  data.table::setorder(result, x, rank)
  result
}

#' @rdname match_spec
#'
#' @export
ai_classify.OpenSpecy <- function(x, library, fill = NULL, top_n = 1L, ...) {
  x <- as_OpenSpecy(x)

  if (is.null(fill) && is.list(library) && is_OpenSpecy(library$fill)) {
    fill <- library$fill
  }
  if(!is.null(fill)) {
    filled <- fill_spec(x, fill)
  } else {
    filled <- x
  }
  proc <- t(filled$spectra)
  colnames(proc) <- filled$wavenumber

  model_type <- if (!is.null(library$model_type)) {
    library$model_type
  } else if (inherits(library$model, "ranger")) {
    "random_forest"
  } else {
    "logistic_regression"
  }
  gated <- identical(model_type, "gated_hierarchical_logistic_regression")
  if (gated) {
    if (is.null(top_n)) top_n <- 1L
    filt <- .ai_gated_prediction_table(proc, library, top_n = top_n)
  } else if (identical(model_type, "hierarchical_logistic_regression")) {
    pred <- .ai_hierarchical_probabilities(proc, library)
  } else if (identical(model_type, "random_forest")) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop(
        "Using a random-forest model requires the suggested 'ranger' package",
        call. = FALSE
      )
    }
    pred <- predict(library$model, data = proc, num.threads = 0L)$predictions
  } else {
    selected_lambda <- if (!is.null(library$lambda_selected)) {
      library$lambda_selected
    } else {
      min(library$model$lambda)
    }
    pred <- predict(library$model,
                    newx = proc,
                    selected_lambda,
                    type = "response")
  }
  if (!gated) {
    if (is.null(top_n)) top_n <- 1L
    filt <- .ai_prediction_table(pred, n = nrow(proc), top_n = top_n)
  }
  
  res <- merge(filt, library$dimension_conversion, all.x = T,
               by.x = "y", by.y = "factor_num")
  setorder(res, "x", "rank")

  return(res)
}

.ai_prediction_table <- function(pred, n, top_n = 1L) {
  if (length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) ||
      top_n < 1 || top_n != floor(top_n)) {
    stop("'top_n' must be a positive integer", call. = FALSE)
  }
  top_n <- as.integer(top_n)
  pred_dim <- dim(pred)

  if (is.null(pred_dim)) {
    pred <- matrix(pred, nrow = n)
    pred_dim <- dim(pred)
  }

  if (length(pred_dim) == 2L) {
    pred_names <- dimnames(pred)
    dim(pred) <- c(pred_dim, 1L)
    dimnames(pred) <- list(pred_names[[1L]], pred_names[[2L]], NULL)
    pred_dim <- dim(pred)
  }

  if (length(pred_dim) != 3L) {
    stop("model predictions must be a vector, matrix, or 3D array",
         call. = FALSE)
  }

  if (pred_dim[1L] != n) {
    stop("number of model predictions does not match number of spectra",
         call. = FALSE)
  }

  pred_names <- dimnames(pred)
  class_id <- pred_names[[2L]]
  lambda_id <- pred_names[[3L]]
  if (is.null(class_id)) class_id <- seq_len(pred_dim[2L])
  if (is.null(lambda_id)) lambda_id <- seq_len(pred_dim[3L])

  if (pred_dim[3L] != 1L && top_n > 1L) {
    stop("ranked model predictions require exactly one selected tuning value",
         call. = FALSE)
  }
  vals <- matrix(as.vector(pred), nrow = pred_dim[1L])
  if (top_n > 1L) {
    scores <- matrix(pred[, , 1L], nrow = pred_dim[1L], ncol = pred_dim[2L])
    scores_order <- scores
    scores_order[is.na(scores_order)] <- -Inf
    keep_n <- min(top_n, pred_dim[2L])
    ordered <- t(vapply(seq_len(pred_dim[1L]), function(row) {
      head(order(scores_order[row, ], decreasing = TRUE, method = "radix"),
           keep_n)
    }, integer(keep_n)))
    row_id <- rep(seq_len(pred_dim[1L]), each = keep_n)
    class_column <- as.vector(t(ordered))
    value <- scores[cbind(row_id, class_column)]
    class_value <- suppressWarnings(as.integer(class_id[class_column]))
    class_value[!is.finite(value)] <- NA_integer_
    return(data.table(
      x = row_id,
      y = class_value,
      z = rep(lambda_id[[1L]], length(row_id)),
      value = value,
      rank = rep(seq_len(keep_n), times = pred_dim[1L])
    ))
  }
  vals_no_na <- vals
  vals_no_na[is.na(vals_no_na)] <- -Inf
  best <- max.col(vals_no_na, ties.method = "first")
  all_na <- rowSums(!is.na(vals)) == 0L
  best[all_na] <- NA_integer_

  combo <- expand.grid(y = class_id, z = lambda_id,
                       KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE)
  value <- rep(NA_real_, pred_dim[1L])
  ok <- !is.na(best)
  value[ok] <- vals[cbind(which(ok), best[ok])]

  data.table(
    x = seq_len(pred_dim[1L]),
    y = as.integer(combo$y[best]),
    z = combo$z[best],
    value = value,
    rank = 1L
  )
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
  x <- as_OpenSpecy(x)
  fill <- as_OpenSpecy(fill)

  test <- matrix(rep(fill$spectra[, 1L], times = ncol(x$spectra)),
                 nrow = length(fill$wavenumber),
                 ncol = ncol(x$spectra),
                 dimnames = list(NULL, colnames(x$spectra)))
  
  aligned_rows <- match(x$wavenumber, fill$wavenumber)
  available <- which(!is.na(aligned_rows))
  if (length(available)) {
    target <- test[aligned_rows[available], , drop = FALSE]
    source <- x$spectra[available, , drop = FALSE]
    finite <- is.finite(source)
    target[finite] <- source[finite]
    test[aligned_rows[available], ] <- target
  }

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
  x <- as_OpenSpecy(x)
  y <- as_OpenSpecy(y)

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

    spec_y <- y$spectra[y$wavenumber %in% x$wavenumber, , drop = FALSE]
    spec_y <- make_rel(spec_y, na.rm = na.rm)
    spec_y <- .matrix_mean_replace(spec_y)
    spec_x <- x$spectra[x$wavenumber %in% y$wavenumber, , drop = FALSE]
    spec_x <- make_rel(spec_x, na.rm = na.rm)
    spec_x <- .matrix_mean_replace(spec_x)

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
    hamming_profile <- function(spec) {
      spec <- t(spec)
      vapply(seq_len(ncol(spec)), function(i) {
        x <- spec[, i]
      values <- table(round(x, 1))
      if (length(values) && diff(range(values)) == 0) {
        # Equal occupied-bin counts represent equal support. This was
        # historically produced by replacing make_rel() NaNs with one; keep
        # the similarity contract explicit now that flat normalization is
        # finite.
        values[] <- 1
      } else {
        values <- make_rel(values)
      }
      sequence <- seq(0, 1, by = 0.1)
      empty <- numeric(length = length(sequence))
      empty[match(names(values), seq(0, 1, by = 0.1))] <- values
      ifelse(is.nan(empty), 1, empty)
      }, FUN.VALUE = numeric(11L))
    }

    spec_y <- hamming_profile(spec_y)
    spec_x <- hamming_profile(spec_x)

    return(1 - unlist(abs(spec_x - spec_y)) |> mean(na.rm = T))
  }
  if(method == "metadata"){
    series = c(names(x$metadata), names(y$metadata))
    return(sum(duplicated(series))/length(unique(series)))
  }
}
