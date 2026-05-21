#' @rdname Specs
#' @title Create compressed Specs objects
#'
#' @description
#' \code{Specs} objects store compressed spectral data for large hyperspectral
#' datasets. They use a structure similar to \code{OpenSpecy}, but store latent
#' \code{variables}, compressed \code{values}, coordinate data, and metadata
#' separately.
#'
#' @param variables vector of latent variable names.
#' @param values numeric matrix with one row per variable and one column per
#' active spectrum or cluster.
#' @param coords coordinate \code{data.frame} or \code{data.table}; should
#' include \code{x}, \code{y}, \code{source_id}, and \code{value_id}.
#' @param metadata metadata \code{data.frame} or \code{data.table} with one row
#' per column in \code{values}.
#' @param attributes list of Specs attributes to attach.
#' @param x an object to test, convert, decompress, or write.
#' @param model a \code{SpecsPCA} model returned by \code{fit_specs_pca()}.
#' @param kmeans logical; should spectra be compressed with K-means after PCA?
#' @param centers passed to \code{\link[stats]{kmeans}()} when \code{kmeans} is
#' \code{TRUE}.
#' @param n_components number of PCA components to keep.
#' @param center,scale. arguments passed to \code{\link[stats]{prcomp}()}.
#' @param expand logical; if \code{TRUE}, decompress or match one row per
#' original coordinate; if \code{FALSE}, keep active spectra or clusters.
#' @param file file path for reading or writing a Specs object.
#' @param compress compression argument passed to \code{\link[base]{saveRDS}()}.
#' @param library a \code{Specs} object to match against.
#' @param top_n integer; number of top latent matches to return.
#' @param add_library_metadata name of a library metadata column to join.
#' @param add_object_metadata name of an object metadata column to join.
#' @param compute correlation compute strategy, \code{"optimized"} or
#' \code{"base"}.
#' @param na.rm logical; should missing values be removed for latent matching?
#' @param features logical or character vector with one value per row in
#' \code{x$coords}.
#' @param shape_kernel,shape_type,close,close_kernel,close_type,img,bottom_left,top_right
#' arguments passed to the feature-definition routine.
#' @param fun function used to collapse latent values.
#' @param column coordinate column used to group spectra for collapse.
#' @param \ldots additional arguments passed to submethods.
#'
#' @return
#' \code{Specs()} and \code{as_Specs()} return a \code{Specs} object.
#' \code{fit_specs_pca()} returns a \code{SpecsPCA} model.
#' \code{decompress_spec()} returns an approximate \code{OpenSpecy} object.
#' \code{read_specs()} returns a \code{Specs} object.
#'
#' @examples
#' data("raman_hdpe")
#' model <- fit_specs_pca(raman_hdpe, n_components = 1)
#' specs <- as_Specs(raman_hdpe, model)
#' decompress_spec(specs)
#'
#' @author
#' Win Cowger
#'
#' @importFrom data.table data.table as.data.table setDT rbindlist
#' @export
Specs <- function(variables, values, coords = NULL, metadata = NULL,
                  attributes = list()) {
  .N <- source_id <- value_id <- NULL

  if (!is.vector(variables))
    stop("'variables' must be a vector", call. = FALSE)

  values <- .as_specs_values(values)
  variables <- as.character(variables)
  if (length(variables) != nrow(values))
    stop("'variables' must have the same length as nrow(values)",
         call. = FALSE)

  value_ids <- colnames(values)
  if (is.null(value_ids))
    value_ids <- paste0("V", seq_len(ncol(values)))
  value_ids <- as.character(value_ids)
  if (length(unique(value_ids)) != length(value_ids))
    stop("column names in 'values' must be unique", call. = FALSE)
  colnames(values) <- value_ids
  rownames(values) <- variables

  if (is.null(coords)) {
    coords <- data.table(
      x = seq_len(ncol(values)) - 1L,
      y = 0L,
      source_id = value_ids,
      value_id = value_ids
    )
  } else {
    coords <- as.data.table(coords)
    if (!all(c("x", "y") %in% names(coords)))
      stop("'coords' must include columns named 'x' and 'y'", call. = FALSE)
    if (!"source_id" %in% names(coords))
      coords[, source_id := paste0("S", seq_len(.N))]
    if (!"value_id" %in% names(coords))
      coords[, value_id := source_id]
    coords[, source_id := as.character(source_id)]
    coords[, value_id := as.character(value_id)]
  }

  if (!all(coords$value_id %in% value_ids))
    stop("all 'coords$value_id' values must be column names in 'values'",
         call. = FALSE)

  if (is.null(metadata)) {
    metadata <- data.table(value_id = value_ids)
  } else {
    metadata <- as.data.table(metadata)
    if (nrow(metadata) != ncol(values))
      stop("'metadata' must have one row per column in 'values'",
           call. = FALSE)
    if (!"value_id" %in% names(metadata))
      metadata[, value_id := value_ids]
    metadata[, value_id := as.character(value_id)]
  }

  if (!identical(metadata$value_id, value_ids)) {
    idx <- match(value_ids, metadata$value_id)
    if (any(is.na(idx)))
      stop("'metadata$value_id' must contain all column names in 'values'",
           call. = FALSE)
    metadata <- metadata[idx]
  }

  obj <- structure(
    list(
      variables = variables,
      values = values,
      coords = coords,
      metadata = metadata
    ),
    class = c("Specs", "list")
  )

  attr(obj, "specs_version") <- .specs_attr(attributes, "specs_version", "0.1.0")
  attr(obj, "variable_model") <- .specs_attr(attributes, "variable_model", NULL)
  attr(obj, "spectrum_compression") <- .specs_attr(attributes, "spectrum_compression", NULL)
  attr(obj, "transformations") <- .specs_attr(attributes, "transformations", list())

  obj
}

#' @rdname Specs
#' @export
is_Specs <- function(x) {
  inherits(x, "Specs")
}

#' @rdname Specs
#' @export
check_Specs <- function(x) {
  if (!(cos <- is_Specs(x)))
    warning("Object 'x' is not of class 'Specs'", call. = FALSE)
  if (!(cln <- identical(names(x), c("variables", "values", "coords", "metadata"))))
    warning("Names of the object components are incorrect", call. = FALSE)
  if (!(cv <- is.vector(x$variables) && !any(is.na(x$variables))))
    warning("'variables' must be a vector without NA values", call. = FALSE)
  if (!(cval <- is.matrix(x$values) && is.numeric(x$values)))
    warning("'values' must be a numeric matrix", call. = FALSE)
  if (!(cl <- cval && length(x$variables) == nrow(x$values)))
    warning("Length of 'variables' is not equal to nrow(values)", call. = FALSE)
  if (!(cu <- cval && !is.null(colnames(x$values)) &&
        length(unique(colnames(x$values))) == ncol(x$values)))
    warning("Column names in 'values' are not unique", call. = FALSE)
  if (!(cc <- data.table::is.data.table(x$coords)))
    warning("'coords' must be a data.table", call. = FALSE)
  if (!(cm <- data.table::is.data.table(x$metadata)))
    warning("'metadata' must be a data.table", call. = FALSE)
  if (!(ccn <- cc && all(c("x", "y", "source_id", "value_id") %in% names(x$coords))))
    warning("'coords' must include x, y, source_id, and value_id", call. = FALSE)
  if (!(cmn <- cm && "value_id" %in% names(x$metadata)))
    warning("'metadata' must include value_id", call. = FALSE)
  if (!(cr <- cval && cm && nrow(x$metadata) == ncol(x$values)))
    warning("Number of metadata rows is not equal to ncol(values)", call. = FALSE)
  if (!(cvid <- cval && ccn && all(x$coords$value_id %in% colnames(x$values))))
    warning("Some coords$value_id values are not present in values", call. = FALSE)
  if (!(mvid <- cval && cmn && identical(as.character(x$metadata$value_id),
                                         colnames(x$values))))
    warning("metadata$value_id must match colnames(values)", call. = FALSE)

  all(cos, cln, cv, cval, cl, cu, cc, cm, ccn, cmn, cr, cvid, mvid)
}

#' @rdname Specs
#' @export
as_Specs <- function(x, ...) {
  UseMethod("as_Specs")
}

#' @rdname Specs
#' @export
as_Specs.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy' or 'Specs'",
       call. = FALSE)
}

#' @rdname Specs
#' @export
as_Specs.Specs <- function(x, ...) {
  x
}

#' @rdname Specs
#' @export
as_Specs.OpenSpecy <- function(x, model, kmeans = FALSE, centers = NULL,
                               ...) {
  y <- source_id <- value_id <- NULL

  x <- as_OpenSpecy(x)
  .check_specs_pca(model)

  if (!isTRUE(all.equal(as.numeric(x$wavenumber),
                        as.numeric(model$original_variables),
                        check.attributes = FALSE))) {
    stop("OpenSpecy wavenumbers must match the PCA model variables exactly; ",
         "conform or process the spectra before calling as_Specs()",
         call. = FALSE)
  }

  value_ids <- colnames(x$spectra)
  if (is.null(value_ids))
    value_ids <- paste0("V", seq_len(ncol(x$spectra)))
  value_ids <- as.character(value_ids)

  scores <- .predict_specs_pca(model, t(x$spectra))
  scores <- t(scores)
  colnames(scores) <- value_ids
  rownames(scores) <- model$variables

  md <- as.data.table(x$metadata)
  coords <- md[, intersect(c("x", "y"), names(md)), with = FALSE]
  if (!"x" %in% names(coords))
    coords[, x := seq_len(ncol(x$spectra)) - 1L]
  if (!"y" %in% names(coords))
    coords[, y := 0L]
  coords[, source_id := value_ids]
  coords[, value_id := value_ids]

  metadata_cols <- setdiff(names(md), c("x", "y"))
  metadata <- md[, metadata_cols, with = FALSE]
  if ("value_id" %in% names(metadata))
    metadata[, value_id := NULL]
  metadata[, value_id := value_ids]
  data.table::setcolorder(metadata, c("value_id", setdiff(names(metadata), "value_id")))

  specs <- Specs(
    variables = model$variables,
    values = scores,
    coords = coords,
    metadata = metadata,
    attributes = list(
      variable_model = .specs_model_metadata(model),
      transformations = list(list(
        method = "pca",
        model_id = model$model_id,
        n_components = length(model$variables)
      ))
    )
  )

  if (isTRUE(kmeans)) {
    if (is.null(centers))
      stop("'centers' must be supplied when kmeans = TRUE", call. = FALSE)
    specs <- .kmeans_specs(specs, centers = centers, ...)
  }

  specs
}

#' @rdname Specs
#' @export
fit_specs_pca <- function(x, n_components, center = TRUE, scale. = FALSE,
                          ...) {
  if (!is.numeric(n_components) || length(n_components) != 1L ||
      is.na(n_components) || n_components < 1) {
    stop("'n_components' must be a positive integer", call. = FALSE)
  }
  n_components <- as.integer(n_components)

  if (is_OpenSpecy(x)) {
    x <- as_OpenSpecy(x)
    data <- t(x$spectra)
    original_variables <- x$wavenumber
  } else if (inherits(x, c("matrix", "data.frame"))) {
    data <- as.matrix(x)
    storage.mode(data) <- "double"
    original_variables <- colnames(data)
    if (is.null(original_variables)) {
      original_variables <- seq_len(ncol(data))
    } else {
      original_variables_num <- suppressWarnings(as.numeric(original_variables))
      if (any(is.na(original_variables_num))) {
        original_variables <- seq_len(ncol(data))
      } else {
        original_variables <- original_variables_num
      }
    }
  } else {
    stop("'x' must be an OpenSpecy object, matrix, or data.frame",
         call. = FALSE)
  }

  if (n_components > min(dim(data)))
    stop("'n_components' must be no larger than min(nrow(data), ncol(data))",
         call. = FALSE)

  pca <- stats::prcomp(data, center = center, scale. = scale.,
                       rank. = n_components, ...)
  rotation <- pca$rotation[, seq_len(n_components), drop = FALSE]
  variables <- paste0("PC", seq_len(n_components))
  colnames(rotation) <- variables

  variances <- pca$sdev^2
  variance_explained <- variances[seq_len(n_components)] / sum(variances)

  model <- list(
    model_type = "pca",
    original_variables = original_variables,
    variables = variables,
    rotation = rotation,
    center = pca$center,
    scale = pca$scale,
    sdev = pca$sdev[seq_len(n_components)],
    variance_explained = variance_explained
  )
  model$model_id <- digest::digest(model)
  class(model) <- c("SpecsPCA", "list")
  model
}

#' @rdname Specs
#' @export
decompress_spec <- function(x, ...) {
  UseMethod("decompress_spec")
}

#' @rdname Specs
#' @export
decompress_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'Specs'", call. = FALSE)
}

#' @rdname Specs
#' @export
decompress_spec.Specs <- function(x, expand = TRUE, ...) {
  .N <- y <- NULL

  x <- as_Specs(x)
  model <- attr(x, "variable_model")
  if (is.null(model) || !identical(model$model_type, "pca"))
    stop("Only Specs objects with PCA model metadata can be decompressed",
         call. = FALSE)

  active_values <- if (isTRUE(expand)) {
    x$values[, match(x$coords$value_id, colnames(x$values)), drop = FALSE]
  } else {
    x$values
  }

  spectra <- .inverse_specs_pca(model, t(active_values))
  spectra <- t(spectra)

  if (isTRUE(expand)) {
    colnames(spectra) <- x$coords$source_id
    md <- data.table::as.data.table(x$coords)
  } else {
    colnames(spectra) <- colnames(x$values)
    md <- data.table::as.data.table(x$metadata)
    if (!"x" %in% names(md))
      md[, x := seq_len(.N) - 1L]
    if (!"y" %in% names(md))
      md[, y := 0L]
  }

  as_OpenSpecy(
    x = as.numeric(model$original_variables),
    spectra = spectra,
    metadata = md,
    coords = NULL
  )
}

#' @rdname Specs
#' @export
write_specs <- function(x, file, compress = "xz", ...) {
  if (!is_Specs(x))
    stop("object 'x' needs to be of class 'Specs'", call. = FALSE)
  saveRDS(x, file = file, compress = compress, ...)
}

#' @rdname Specs
#' @export
read_specs <- function(file, ...) {
  x <- readRDS(file, ...)
  if (!is_Specs(x))
    stop("file does not contain a Specs object", call. = FALSE)
  x
}

.as_specs_values <- function(values) {
  if (!inherits(values, c("data.frame", "matrix")))
    stop("'values' must inherit from data.frame or matrix", call. = FALSE)
  if (is.null(dim(values)) || length(dim(values)) != 2L)
    stop("'values' must be a two-dimensional object", call. = FALSE)
  if (inherits(values, "data.frame")) {
    numeric_cols <- vapply(values, function(x) {
      is.numeric(x) || is.logical(x)
    }, FUN.VALUE = logical(1))
    if (!all(numeric_cols))
      stop("all columns of 'values' must be numeric or logical",
           call. = FALSE)
    values <- as.matrix(values)
  } else if (!is.numeric(values) && !is.logical(values)) {
    stop("'values' matrix must be numeric or logical", call. = FALSE)
  }
  storage.mode(values) <- "double"
  values
}

.specs_attr <- function(attributes, name, default = NULL) {
  if (!is.null(attributes[[name]])) attributes[[name]] else default
}

.append_specs_transformation <- function(x, record) {
  transforms <- attr(x, "transformations")
  if (is.null(transforms)) transforms <- list()
  attr(x, "transformations") <- c(transforms, list(record))
  x
}

.check_specs_pca <- function(model) {
  if (!inherits(model, "SpecsPCA") ||
      !identical(model$model_type, "pca") ||
      is.null(model$rotation) ||
      is.null(model$original_variables)) {
    stop("'model' must be a SpecsPCA object returned by fit_specs_pca()",
         call. = FALSE)
  }
  invisible(TRUE)
}

.specs_model_metadata <- function(model) {
  list(
    model_type = model$model_type,
    model_id = model$model_id,
    original_variables = model$original_variables,
    variables = model$variables,
    rotation = model$rotation,
    center = model$center,
    scale = model$scale,
    sdev = model$sdev,
    variance_explained = model$variance_explained
  )
}

.predict_specs_pca <- function(model, data) {
  data <- as.matrix(data)
  if (ncol(data) != length(model$original_variables))
    stop("'data' has the wrong number of variables for the PCA model",
         call. = FALSE)

  if (!isFALSE(model$center))
    data <- sweep(data, 2L, model$center, "-")
  if (!isFALSE(model$scale))
    data <- sweep(data, 2L, model$scale, "/")

  data %*% model$rotation
}

.inverse_specs_pca <- function(model, scores) {
  scores <- as.matrix(scores)
  data <- scores %*% t(model$rotation)

  if (!isFALSE(model$scale))
    data <- sweep(data, 2L, model$scale, "*")
  if (!isFALSE(model$center))
    data <- sweep(data, 2L, model$center, "+")

  colnames(data) <- as.character(model$original_variables)
  data
}

.kmeans_specs <- function(x, centers, ...) {
  value_id <- NULL

  active_ids <- colnames(x$values)
  km <- stats::kmeans(t(x$values), centers = centers, ...)

  cluster_ids <- paste0("KM", seq_len(nrow(km$centers)))
  values <- t(km$centers)
  colnames(values) <- cluster_ids
  rownames(values) <- x$variables

  lookup <- cluster_ids[km$cluster]
  names(lookup) <- active_ids
  coords <- data.table::as.data.table(x$coords)
  coords[, value_id := lookup[value_id]]

  metadata <- data.table(
    value_id = cluster_ids,
    cluster_size = as.integer(km$size),
    withinss = as.numeric(km$withinss)
  )

  attrs <- list(
    specs_version = attr(x, "specs_version"),
    variable_model = attr(x, "variable_model"),
    spectrum_compression = list(
      method = "kmeans",
      centers = length(cluster_ids),
      tot.withinss = km$tot.withinss,
      iter = km$iter,
      ifault = km$ifault
    ),
    transformations = attr(x, "transformations")
  )

  out <- Specs(x$variables, values, coords = coords, metadata = metadata,
               attributes = attrs)
  .append_specs_transformation(out, list(
    method = "kmeans",
    centers = length(cluster_ids),
    original_values = length(active_ids)
  ))
}

.compatible_specs <- function(x, y) {
  x_model <- attr(x, "variable_model")
  y_model <- attr(y, "variable_model")
  if (is.null(x_model) || is.null(y_model) ||
      !identical(x_model$model_id, y_model$model_id)) {
    stop("Specs objects must use the same variable model for latent matching",
         call. = FALSE)
  }
  if (!identical(x$variables, y$variables))
    stop("Specs objects must have identical variables", call. = FALSE)
  invisible(TRUE)
}

.specs_values_for_cor <- function(values, na.rm = TRUE) {
  if (isTRUE(na.rm)) {
    values <- values
    values[!is.finite(values)] <- NA_real_
    values <- .matrix_mean_replace(values, na.rm = TRUE)
  }
  values
}

.expand_specs_matches <- function(res, x) {
  active_value_id <- object_id <- source_id <- NULL

  coords <- data.table::as.data.table(x$coords)
  expanded <- merge(res, coords, by.x = "object_id", by.y = "value_id",
                    allow.cartesian = TRUE, all.x = TRUE)
  expanded[, active_value_id := object_id]
  if ("source_id" %in% names(expanded))
    expanded[, object_id := source_id]
  data.table::setcolorder(
    expanded,
    c("object_id", "active_value_id",
      setdiff(names(expanded), c("object_id", "active_value_id")))
  )
  expanded
}
