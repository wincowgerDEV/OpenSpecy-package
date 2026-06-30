#' @rdname Specs
#' @export
cor_spec.Specs <- function(x, library, na.rm = TRUE, compute = "optimized",
                           ...) {
  x <- as_Specs(x)
  library <- as_Specs(library)
  if (.is_hilbert_specs(x) || .is_hilbert_specs(library)) {
    stop("Hilbert-encoded Specs objects use distance matching; call ",
         "match_spec() with compatible Hilbert Specs objects instead",
         call. = FALSE)
  }
  .compatible_specs(x, library)

  lib <- .specs_values_for_cor(library$values, na.rm = na.rm)
  spec <- .specs_values_for_cor(x$values, na.rm = na.rm)

  if (compute == "optimized") {
    return(.fast_correlation(lib, spec))
  }
  if (compute == "base") {
    return(stats::cor(lib, spec, ...))
  }
  stop("'compute' must be 'optimized' or 'base'", call. = FALSE)
}

#' @rdname Specs
#' @export
match_spec.Specs <- function(x, library, top_n = NULL, expand = FALSE,
                             add_library_metadata = NULL,
                             add_object_metadata = NULL,
                             compute = "optimized", na.rm = TRUE, ...) {
  x <- as_Specs(x)
  library <- as_Specs(library)

  if (.is_hilbert_specs(x) || .is_hilbert_specs(library)) {
    res <- .match_specs_hilbert(
      x, library = library, top_n = top_n,
      add_library_metadata = add_library_metadata,
      add_object_metadata = add_object_metadata
    )
  } else {
    res <- cor_spec(x, library = library, compute = compute, na.rm = na.rm, ...) |>
      ident_spec(x, library = library, top_n = top_n,
                 add_library_metadata = add_library_metadata,
                 add_object_metadata = add_object_metadata)
  }

  if (isTRUE(expand))
    res <- .expand_specs_matches(res, x)

  res
}

.match_specs_hilbert <- function(x, library, top_n = NULL,
                                 add_library_metadata = NULL,
                                 add_object_metadata = NULL) {
  match_distance <- NULL

  .compatible_hilbert_specs(x, library)
  lib_codes <- .hilbert_code_numeric(library)
  obj_codes <- .hilbert_code_numeric(x)

  if (is.null(top_n) || top_n > length(lib_codes)) {
    top_n <- length(lib_codes)
    message("'top_n' larger than the number of spectra in the library; ",
            "returning all matches")
  }
  top_n <- as.integer(top_n)

  if (top_n == 1L) {
    best <- vapply(obj_codes, function(code) {
      which.min(abs(lib_codes - code))
    }, FUN.VALUE = integer(1L))
    out <- data.table(
      object_id = names(obj_codes),
      library_id = names(lib_codes)[best],
      match_val = -abs(lib_codes[best] - obj_codes),
      match_distance = abs(lib_codes[best] - obj_codes)
    )
  } else {
    out <- data.table::rbindlist(lapply(seq_along(obj_codes), function(j) {
      dist <- abs(lib_codes - obj_codes[j])
      idx <- head(order(dist), top_n)
      data.table(
        object_id = names(obj_codes)[j],
        library_id = names(lib_codes)[idx],
        match_val = -dist[idx],
        match_distance = dist[idx]
      )
    }))
  }

  data.table::setorder(out, match_distance)

  if (is.character(add_library_metadata))
    out <- merge(out, library$metadata,
                 by.x = "library_id", by.y = add_library_metadata,
                 all.x = TRUE)

  if (is.character(add_object_metadata))
    out <- merge(out, x$metadata,
                 by.x = "object_id", by.y = add_object_metadata,
                 all.x = TRUE)

  out
}

#' @rdname Specs
#' @export
def_features.Specs <- function(x, features,
                               shape_kernel = c(3, 3),
                               shape_type = "box",
                               close = FALSE,
                               close_kernel = c(4, 4),
                               close_type = "box",
                               img = NULL,
                               bottom_left = NULL,
                               top_right = NULL,
                               ...) {
  x <- as_Specs(x)

  if (length(features) != nrow(x$coords))
    stop("'features' must have one value per row in x$coords",
         call. = FALSE)

  if (is.logical(features) || is.character(features)) {
    if (length(unique(features)) == 1L)
      stop("features cannot be all one class, e.g. all TRUE, FALSE, or one category",
           call. = FALSE)
    if (is.character(features) && close)
      stop("closing is not supported when features is a character vector; ",
           "convert to logical or use close = FALSE",
           call. = FALSE)

    tmp <- list(metadata = data.table::as.data.table(x$coords))
    features_df <- .def_features(tmp, binary = features,
                                 shape_kernel = shape_kernel,
                                 shape_type = shape_type,
                                 close = close,
                                 close_kernel = close_kernel,
                                 close_type = close_type,
                                 img = img,
                                 bottom_left = bottom_left,
                                 top_right = top_right)
  } else {
    stop("features needs to be a character or logical vector", call. = FALSE)
  }

  obj <- x
  x <- y <- feature_id <- r <- g <- b <- NULL
  md <- features_df[data.table::setDT(obj$coords), on = c("x", "y")]
  md[, feature_id := ifelse(is.na(feature_id), "-88", feature_id)]
  if (all(c("r", "g", "b") %in% names(md))) {
    md[, `:=`(mean_r = as.integer(sqrt(mean(r^2))),
              mean_g = as.integer(sqrt(mean(g^2))),
              mean_b = as.integer(sqrt(mean(b^2)))), by = "feature_id"]
  }
  md[, "centroid_x" := mean(x), by = "feature_id"]
  md[, "centroid_y" := mean(y), by = "feature_id"]
  md[, "first_x" := x[1], by = "feature_id"]
  md[, "first_y" := y[1], by = "feature_id"]
  md[, "rand_x" := sample(x, 1), by = "feature_id"]
  md[, "rand_y" := sample(y, 1), by = "feature_id"]

  obj$coords <- md
  .append_specs_transformation(obj, list(method = "def_features"))
}

#' @rdname Specs
#' @export
collapse_spec.Specs <- function(x, fun = mean, column = "feature_id", ...) {
  collapse_size <- value_id <- NULL

  x <- as_Specs(x)
  if (!column %in% names(x$coords))
    stop("column '", column, "' was not found in x$coords", call. = FALSE)

  ids <- as.character(x$coords[[column]])
  value_idx <- match(x$coords$value_id, colnames(x$values))
  if (any(is.na(value_idx)))
    stop("Some coords$value_id values are not present in values",
         call. = FALSE)

  uids <- unique(ids)
  out <- matrix(NA_real_, nrow = nrow(x$values), ncol = length(uids),
                dimnames = list(x$variables, uids))
  FUN <- match.fun(fun)

  for (i in seq_along(uids)) {
    sel <- ids == uids[i]
    idx <- value_idx[sel]

    if (identical(FUN, base::mean)) {
      weights <- tabulate(idx, nbins = ncol(x$values))
      out[, i] <- as.numeric(x$values %*% weights) / sum(weights)
    } else if (identical(FUN, base::sum)) {
      weights <- tabulate(idx, nbins = ncol(x$values))
      out[, i] <- as.numeric(x$values %*% weights)
    } else if (identical(FUN, stats::median)) {
      out[, i] <- matrixStats::rowMedians(x$values[, idx, drop = FALSE], ...)
    } else {
      out[, i] <- apply(x$values[, idx, drop = FALSE], 1L, FUN, ...)
    }
  }

  coords <- data.table::as.data.table(x$coords)
  coords[, value_id := ids]

  metadata <- coords[match(uids, ids)]
  metadata[, value_id := uids]
  metadata[, collapse_size := as.integer(tabulate(match(ids, uids),
                                                  nbins = length(uids)))]
  data.table::setcolorder(metadata,
                          c("value_id", setdiff(names(metadata), "value_id")))

  compression <- attr(x, "spectrum_compression")
  if (is.null(compression)) compression <- list()
  compression$collapse <- list(
    method = "collapse",
    column = column,
    values = length(uids)
  )

  out_obj <- Specs(
    variables = x$variables,
    values = out,
    coords = coords,
    metadata = metadata,
    attributes = list(
      specs_version = attr(x, "specs_version"),
      variable_model = attr(x, "variable_model"),
      hilbert_model = attr(x, "hilbert_model"),
      spectrum_compression = compression,
      transformations = attr(x, "transformations")
    )
  )
  .append_specs_transformation(out_obj, list(
    method = "collapse",
    column = column,
    values = length(uids)
  ))
}
