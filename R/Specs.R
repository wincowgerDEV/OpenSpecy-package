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
#' @param model optional \code{SpecsPCA} model returned by
#' \code{fit_specs_pca()}; if omitted and \code{"pca"} is in \code{steps}, a
#' model is fit from \code{x}.
#' @param steps character vector of compression steps. Supported values are
#' \code{"pca"}, \code{"kmeans"}, and \code{"hilbert"}. K-means can be placed
#' before, between, or after the other steps; PCA cannot be placed after
#' Hilbert encoding.
#' @param centers passed to \code{\link[stats]{kmeans}()} when K-means is used.
#' @param n_components number of PCA components to keep.
#' @param center,scale. arguments passed to \code{\link[stats]{prcomp}()}.
#' @param bits_per_variable positive whole number of bits used for each
#' Hilbert-encoded variable. If \code{NULL}, the value is inferred from the
#' number of variables to pack the available 64-bit code space.
#' @param limits optional two-column matrix, data frame, or Hilbert model with
#' per-variable minimum and maximum values used for quantization.
#' @param expand logical; if \code{TRUE}, decompress or match one row per
#' original coordinate; if \code{FALSE}, keep active spectra or clusters.
#' @param index optional positive integer vector selecting spectra to
#' decompress. With \code{expand = TRUE}, indexes refer to rows in
#' \code{x$coords}; with \code{expand = FALSE}, indexes refer to columns in
#' \code{x$values}.
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
#' \code{Specs()}, \code{as_Specs()}, \code{encode_specs_hilbert()}, and
#' \code{decode_specs_hilbert()} return a \code{Specs} object.
#' \code{fit_specs_pca()} returns a \code{SpecsPCA} model.
#' \code{decompress_spec()} returns an approximate \code{OpenSpecy} object.
#' \code{read_specs()} returns a \code{Specs} object.
#'
#' @examples
#' data("raman_hdpe")
#' specs <- as_Specs(raman_hdpe, n_components = 1)
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
  attr(obj, "hilbert_model") <- .specs_attr(attributes, "hilbert_model", NULL)
  attr(obj, "spectrum_compression") <- .specs_attr(attributes, "spectrum_compression", NULL)
  attr(obj, "transformations") <- .specs_attr(attributes, "transformations", list())
  attr(obj, "visual_image") <- .specs_attr(attributes, "visual_image", NULL)

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
  if (!(ch <- .check_specs_hilbert_invariants(x)))
    warning("Hilbert Specs metadata or code rows are invalid", call. = FALSE)

  all(cos, cln, cv, cval, cl, cu, cc, cm, ccn, cmn, cr, cvid, mvid, ch)
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
as_Specs.OpenSpecy <- function(x, model = NULL, steps = c("pca", "hilbert"),
                               n_components = NULL, centers = NULL,
                               bits_per_variable = NULL, limits = NULL,
                               ...) {
  x <- as_OpenSpecy(x)
  steps <- .normalize_specs_steps(steps, model = model)
  .validate_specs_steps(steps)

  specs <- .open_specy_to_specs(x)
  for (step in steps) {
    if (identical(step, "kmeans")) {
      if (is.null(centers))
        stop("'centers' must be supplied when K-means is used",
             call. = FALSE)
      specs <- .kmeans_specs(specs, centers = centers, ...)
    } else if (identical(step, "pca")) {
      if (is.null(model)) {
        n_fit <- .default_specs_n_components(
          specs, n_components = n_components, steps = steps,
          bits_per_variable = bits_per_variable
        )
        model <- fit_specs_pca(specs, n_components = n_fit)
      }
      specs <- .pca_specs(specs, model)
    } else if (identical(step, "hilbert")) {
      specs <- encode_specs_hilbert(
        specs, bits_per_variable = bits_per_variable, limits = limits
      )
    }
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
  } else if (is_Specs(x)) {
    x <- as_Specs(x)
    if (.is_hilbert_specs(x))
      stop("PCA cannot be fit after Hilbert encoding; decode first or place ",
           "'pca' before 'hilbert' in steps", call. = FALSE)
    data <- t(x$values)
    original_variables <- .specs_variables_for_open_specy(x$variables)
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
    stop("'x' must be an OpenSpecy object, Specs object, matrix, or data.frame",
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
decompress_spec.Specs <- function(x, expand = TRUE, index = NULL, ...) {
  .N <- y <- NULL

  x <- as_Specs(x)
  x <- .subset_specs_for_decompression(x, expand = expand, index = index)
  if (.is_hilbert_specs(x))
    x <- decode_specs_hilbert(x)

  model <- attr(x, "variable_model")
  if (!is.null(model) && !identical(model$model_type, "pca"))
    stop("Only Specs objects with PCA or Hilbert metadata can be decompressed",
         call. = FALSE)

  active_values <- if (isTRUE(expand)) {
    x$values[, match(x$coords$value_id, colnames(x$values)), drop = FALSE]
  } else {
    x$values
  }

  if (!is.null(model)) {
    spectra <- .inverse_specs_pca(model, t(active_values))
    spectra <- t(spectra)
    variables <- as.numeric(model$original_variables)
  } else {
    spectra <- active_values
    variables <- .specs_variables_for_open_specy(x$variables)
  }

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

  out <- as_OpenSpecy(
    x = variables,
    spectra = spectra,
    metadata = md,
    coords = NULL
  )
  if (!is.null(attr(x, "visual_image")))
    attr(out, "visual_image") <- attr(x, "visual_image")
  out
}

#' @rdname Specs
#' @export
encode_specs_hilbert <- function(x, bits_per_variable = NULL, limits = NULL,
                                 ...) {
  x <- as_Specs(x)
  if (.is_hilbert_specs(x))
    stop("'x' is already Hilbert-encoded", call. = FALSE)

  hilbert <- .fit_specs_hilbert(
    x$values, variables = x$variables,
    bits_per_variable = bits_per_variable, limits = limits
  )
  codes <- .hilbert_encode_coords(hilbert$coords,
                                  bits_per_variable = hilbert$bits_per_variable)
  colnames(codes) <- colnames(x$values)

  compression <- attr(x, "spectrum_compression")
  if (is.null(compression)) compression <- list()
  compression$method <- "hilbert"
  compression$hilbert <- list(
    bits_per_variable = hilbert$bits_per_variable,
    variables = length(hilbert$original_variables)
  )

  out <- Specs(
    variables = c("hilbert_hi", "hilbert_lo"),
    values = codes,
    coords = x$coords,
    metadata = x$metadata,
    attributes = list(
      specs_version = attr(x, "specs_version"),
      variable_model = attr(x, "variable_model"),
      hilbert_model = hilbert$model,
      spectrum_compression = compression,
      transformations = attr(x, "transformations"),
      visual_image = attr(x, "visual_image")
    )
  )
  .append_specs_transformation(out, list(
    method = "hilbert",
    bits_per_variable = hilbert$bits_per_variable,
    variables = length(hilbert$original_variables)
  ))
}

#' @rdname Specs
#' @export
decode_specs_hilbert <- function(x, ...) {
  x <- as_Specs(x)
  if (!.is_hilbert_specs(x))
    stop("'x' must be a Hilbert-encoded Specs object", call. = FALSE)

  model <- attr(x, "hilbert_model")
  coords <- .hilbert_decode_codes(
    x$values, n_dims = model$n_variables,
    bits_per_variable = model$bits_per_variable
  )
  values <- .dequantize_specs_hilbert(coords, model)
  colnames(values) <- colnames(x$values)

  compression <- attr(x, "spectrum_compression")
  if (!is.null(compression)) {
    compression$hilbert <- NULL
    if (identical(compression$method, "hilbert"))
      compression$method <- NULL
  }

  Specs(
    variables = model$original_variables,
    values = values,
    coords = x$coords,
    metadata = x$metadata,
    attributes = list(
      specs_version = attr(x, "specs_version"),
      variable_model = attr(x, "variable_model"),
      spectrum_compression = compression,
      transformations = attr(x, "transformations"),
      visual_image = attr(x, "visual_image")
    )
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

.subset_specs_for_decompression <- function(x, expand = TRUE, index = NULL) {
  value_id <- NULL

  if (is.null(index))
    return(x)

  if (!is.numeric(index) || anyNA(index) || any(index != floor(index)) ||
      any(index < 1)) {
    stop("'index' must be a positive whole-number vector", call. = FALSE)
  }
  index <- as.integer(index)
  if (!length(index))
    stop("'index' must select at least one spectrum", call. = FALSE)
  if (anyDuplicated(index))
    stop("'index' must not contain duplicate values", call. = FALSE)

  if (isTRUE(expand)) {
    if (any(index > nrow(x$coords))) {
      stop("'index' contains a coordinate row outside x$coords",
           call. = FALSE)
    }
    coords <- data.table::as.data.table(x$coords)[index]
    value_ids <- coords$value_id[!duplicated(coords$value_id)]
  } else {
    if (any(index > ncol(x$values))) {
      stop("'index' contains an active spectrum outside x$values",
           call. = FALSE)
    }
    value_ids <- colnames(x$values)[index]
    coords <- data.table::as.data.table(x$coords)[value_id %in% value_ids]
  }

  value_idx <- match(value_ids, colnames(x$values))
  if (anyNA(value_idx))
    stop("Some selected coords$value_id values are not present in values",
         call. = FALSE)

  values <- x$values[, value_idx, drop = FALSE]
  metadata <- data.table::as.data.table(x$metadata)[
    match(colnames(values), x$metadata$value_id)
  ]

  Specs(
    variables = x$variables,
    values = values,
    coords = coords,
    metadata = metadata,
    attributes = list(
      specs_version = attr(x, "specs_version"),
      variable_model = attr(x, "variable_model"),
      hilbert_model = attr(x, "hilbert_model"),
      spectrum_compression = attr(x, "spectrum_compression"),
      transformations = attr(x, "transformations"),
      visual_image = attr(x, "visual_image")
    )
  )
}

.normalize_specs_steps <- function(steps, model = NULL) {
  if (is.null(steps)) steps <- character()
  steps <- tolower(as.character(steps))
  allowed <- c("pca", "kmeans", "hilbert")
  bad <- setdiff(steps, allowed)
  if (length(bad))
    stop("unsupported Specs compression step(s): ", paste(bad, collapse = ", "),
         call. = FALSE)
  if (anyDuplicated(steps))
    stop("'steps' must not repeat compression steps", call. = FALSE)

  if (!is.null(model) && !"pca" %in% steps)
    steps <- c("pca", steps)

  steps
}

.validate_specs_steps <- function(steps) {
  pca <- match("pca", steps)
  hilbert <- match("hilbert", steps)
  if (!is.na(pca) && !is.na(hilbert) && pca > hilbert) {
    stop("PCA cannot run after Hilbert encoding; use steps with 'pca' ",
         "before 'hilbert' or omit one of those steps", call. = FALSE)
  }
  invisible(TRUE)
}

.open_specy_to_specs <- function(x) {
  .N <- source_id <- value_id <- NULL

  value_ids <- colnames(x$spectra)
  if (is.null(value_ids))
    value_ids <- paste0("V", seq_len(ncol(x$spectra)))
  value_ids <- as.character(value_ids)

  values <- x$spectra
  colnames(values) <- value_ids
  rownames(values) <- as.character(x$wavenumber)

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
  data.table::setcolorder(metadata,
                          c("value_id", setdiff(names(metadata), "value_id")))

  Specs(x$wavenumber, values, coords = coords, metadata = metadata,
        attributes = list(visual_image = attr(x, "visual_image")))
}

.default_specs_n_components <- function(x, n_components = NULL, steps,
                                        bits_per_variable = NULL) {
  if (!is.null(n_components))
    return(n_components)

  max_components <- min(dim(t(x$values)))
  if ("hilbert" %in% steps) {
    bits <- if (is.null(bits_per_variable)) 4L else bits_per_variable
    .validate_hilbert_bits(bits, n_variables = 1L)
    max_components <- min(max_components, floor(64L / as.integer(bits)))
  }
  max(1L, as.integer(max_components))
}

.pca_specs <- function(x, model) {
  x <- as_Specs(x)
  .check_specs_pca(model)
  if (.is_hilbert_specs(x))
    stop("PCA cannot run after Hilbert encoding; decode first or place ",
         "'pca' before 'hilbert' in steps", call. = FALSE)

  if (!.same_specs_variables(x$variables, model$original_variables)) {
    stop("Specs variables must match the PCA model variables exactly; ",
         "conform or process the spectra before applying PCA",
         call. = FALSE)
  }

  value_ids <- colnames(x$values)
  scores <- .predict_specs_pca(model, t(x$values))
  scores <- t(scores)
  colnames(scores) <- value_ids
  rownames(scores) <- model$variables

  out <- Specs(
    variables = model$variables,
    values = scores,
    coords = x$coords,
    metadata = x$metadata,
    attributes = list(
      specs_version = attr(x, "specs_version"),
      variable_model = .specs_model_metadata(model),
      spectrum_compression = attr(x, "spectrum_compression"),
      transformations = attr(x, "transformations"),
      visual_image = attr(x, "visual_image")
    )
  )
  .append_specs_transformation(out, list(
    method = "pca",
    model_id = model$model_id,
    n_components = length(model$variables)
  ))
}

.same_specs_variables <- function(x, y) {
  x_num <- suppressWarnings(as.numeric(x))
  y_num <- suppressWarnings(as.numeric(y))
  if (!anyNA(x_num) && !anyNA(y_num)) {
    return(isTRUE(all.equal(x_num, y_num, check.attributes = FALSE)))
  }
  identical(as.character(x), as.character(y))
}

.specs_variables_for_open_specy <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (anyNA(out)) as.character(x) else out
}

.is_hilbert_specs <- function(x) {
  is_Specs(x) &&
    identical(as.character(x$variables), c("hilbert_hi", "hilbert_lo")) &&
    !is.null(attr(x, "hilbert_model"))
}

.check_specs_hilbert_invariants <- function(x) {
  model <- attr(x, "hilbert_model")
  has_code_rows <- is.list(x) &&
    identical(as.character(x$variables), c("hilbert_hi", "hilbert_lo"))
  if (is.null(model))
    return(!has_code_rows)
  if (!has_code_rows || !is.matrix(x$values) || nrow(x$values) != 2L)
    return(FALSE)
  if (!all(is.finite(x$values)) || any(x$values < 0) ||
      any(x$values > .uint32_max()))
    return(FALSE)
  if (!identical(model$model_type, "hilbert") ||
      is.null(model$bits_per_variable) ||
      is.null(model$n_variables) ||
      is.null(model$limits))
    return(FALSE)
  if (!identical(model$n_variables, length(model$original_variables)))
    return(FALSE)
  is.matrix(model$limits) && nrow(model$limits) == model$n_variables &&
    ncol(model$limits) == 2L
}

.fit_specs_hilbert <- function(values, variables, bits_per_variable = NULL,
                               limits = NULL) {
  if (is.list(limits) && !is.null(limits$limits)) {
    if (is.null(bits_per_variable))
      bits_per_variable <- limits$bits_per_variable
    limits <- limits$limits
  }

  values <- as.matrix(values)
  if (!all(is.finite(values)))
    stop("Hilbert encoding requires finite Specs values", call. = FALSE)

  n_variables <- nrow(values)
  bits_per_variable <- .resolve_hilbert_bits(bits_per_variable, n_variables)
  limits <- .normalize_hilbert_limits(limits, values, variables)
  coords <- .quantize_specs_hilbert(values, limits, bits_per_variable)

  model <- list(
    model_type = "hilbert",
    original_variables = as.character(variables),
    n_variables = n_variables,
    bits_per_variable = bits_per_variable,
    total_bits = n_variables * bits_per_variable,
    limits = limits,
    reconstruction = "bin_center"
  )
  model$model_id <- digest::digest(model)

  list(
    model = model,
    coords = coords,
    bits_per_variable = bits_per_variable,
    original_variables = variables
  )
}

.resolve_hilbert_bits <- function(bits_per_variable, n_variables) {
  if (is.null(bits_per_variable)) {
    bits_per_variable <- floor(64L / n_variables)
    if (bits_per_variable < 1L) {
      stop("Cannot Hilbert-encode ", n_variables,
           " variables in a 64-bit code. Reduce the number of variables, ",
           "for example by lowering PCA 'n_components'.", call. = FALSE)
    }
  }
  .validate_hilbert_bits(bits_per_variable, n_variables)
}

.validate_hilbert_bits <- function(bits_per_variable, n_variables) {
  if (!is.numeric(bits_per_variable) || length(bits_per_variable) != 1L ||
      is.na(bits_per_variable) || bits_per_variable < 1 ||
      bits_per_variable != floor(bits_per_variable)) {
    stop("'bits_per_variable' must be a positive whole number",
         call. = FALSE)
  }
  bits_per_variable <- as.integer(bits_per_variable)
  total <- n_variables * bits_per_variable
  if (total > 64L) {
    max_variables <- floor(64L / bits_per_variable)
    max_bits <- floor(64L / n_variables)
    stop("Cannot Hilbert-encode ", n_variables, " variables with ",
         bits_per_variable, " bits each; that needs ", total,
         " bits but Specs Hilbert codes store 64 bits. Use no more than ",
         max_variables, " variables at ", bits_per_variable,
         " bits each, or no more than ", max_bits, " bits for ",
         n_variables, " variables. Reduce PCA 'n_components' or ",
         "'bits_per_variable'.", call. = FALSE)
  }
  bits_per_variable
}

.normalize_hilbert_limits <- function(limits, values, variables) {
  if (is.null(limits)) {
    limits <- cbind(
      min = apply(values, 1L, min),
      max = apply(values, 1L, max)
    )
  } else {
    limits <- as.matrix(limits)
    storage.mode(limits) <- "double"
    if (ncol(limits) != 2L && nrow(limits) == 2L)
      limits <- t(limits)
    if (nrow(limits) != nrow(values) || ncol(limits) != 2L) {
      stop("'limits' must have one row per Specs variable and two columns",
           call. = FALSE)
    }
    colnames(limits) <- c("min", "max")
  }
  rownames(limits) <- as.character(variables)
  if (!all(is.finite(limits)) || any(limits[, 1L] > limits[, 2L]))
    stop("'limits' must contain finite min/max pairs", call. = FALSE)
  limits
}

.quantize_specs_hilbert <- function(values, limits, bits_per_variable) {
  bins <- 2^bits_per_variable
  coords <- matrix(0, nrow = nrow(values), ncol = ncol(values),
                   dimnames = dimnames(values))
  for (i in seq_len(nrow(values))) {
    lo <- limits[i, 1L]
    hi <- limits[i, 2L]
    span <- hi - lo
    if (span == 0) {
      coords[i, ] <- 0
    } else {
      clipped <- pmin(pmax(values[i, ], lo), hi)
      coord <- floor((clipped - lo) / span * bins)
      coords[i, ] <- pmin(coord, bins - 1)
    }
  }
  coords
}

.dequantize_specs_hilbert <- function(coords, model) {
  limits <- model$limits
  bins <- 2^model$bits_per_variable
  values <- matrix(0, nrow = model$n_variables, ncol = ncol(coords),
                   dimnames = list(model$original_variables, colnames(coords)))
  for (i in seq_len(model$n_variables)) {
    lo <- limits[i, 1L]
    hi <- limits[i, 2L]
    span <- hi - lo
    if (span == 0) {
      values[i, ] <- lo
    } else {
      values[i, ] <- lo + ((coords[i, ] + 0.5) / bins) * span
    }
  }
  values
}

.hilbert_encode_coords <- function(coords, bits_per_variable) {
  coords <- as.matrix(coords)
  out <- matrix(NA_real_, nrow = 2L, ncol = ncol(coords),
                dimnames = list(c("hilbert_hi", "hilbert_lo"),
                                colnames(coords)))
  for (j in seq_len(ncol(coords))) {
    x_bits <- .coords_to_bitmat(coords[, j], bits_per_variable)
    h_transpose <- .axes_to_hilbert_transpose(x_bits)
    h_bits <- as.vector(h_transpose)
    out[, j] <- .bits_to_uint64_parts(h_bits)
  }
  out
}

.hilbert_decode_codes <- function(codes, n_dims, bits_per_variable) {
  codes <- .normalize_hilbert_code_values(codes)
  out <- matrix(NA_real_, nrow = n_dims, ncol = ncol(codes),
                dimnames = list(NULL, colnames(codes)))
  nbits <- n_dims * bits_per_variable
  for (j in seq_len(ncol(codes))) {
    h_bits <- .uint64_parts_to_bits(codes[, j], nbits)
    h_transpose <- matrix(h_bits, nrow = n_dims, ncol = bits_per_variable)
    x_bits <- .hilbert_transpose_to_axes(h_transpose)
    out[, j] <- .bitmat_to_coords(x_bits)
  }
  out
}

.coords_to_bitmat <- function(coord, bits_per_variable) {
  powers <- 2^((bits_per_variable - 1L):0L)
  mat <- t(vapply(coord, function(x) {
    as.integer(floor(x / powers) %% 2L)
  }, FUN.VALUE = integer(bits_per_variable)))
  storage.mode(mat) <- "integer"
  mat
}

.bitmat_to_coords <- function(bits) {
  powers <- 2^((ncol(bits) - 1L):0L)
  as.numeric(bits %*% powers)
}

.axes_to_hilbert_transpose <- function(bits) {
  bits <- bits == 1L
  n_dims <- nrow(bits)
  p <- ncol(bits)

  if (p >= 2L) {
    for (q in seq.int(p, 2L)) {
      idx <- p - q + 1L
      lower <- if (idx < p) (idx + 1L):p else integer()
      for (i in seq_len(n_dims)) {
        if (bits[i, idx]) {
          bits[1L, lower] <- !bits[1L, lower]
        } else if (length(lower)) {
          t <- xor(bits[1L, lower], bits[i, lower])
          bits[1L, lower] <- xor(bits[1L, lower], t)
          bits[i, lower] <- xor(bits[i, lower], t)
        }
      }
    }
  }

  if (n_dims > 1L) {
    for (i in 2L:n_dims)
      bits[i, ] <- xor(bits[i, ], bits[i - 1L, ])
  }
  t <- rep(FALSE, p)
  if (p >= 2L) {
    for (q in seq.int(p, 2L)) {
      idx <- p - q + 1L
      lower <- if (idx < p) (idx + 1L):p else integer()
      if (bits[n_dims, idx])
        t[lower] <- !t[lower]
    }
  }
  for (i in seq_len(n_dims))
    bits[i, ] <- xor(bits[i, ], t)
  matrix(as.integer(bits), nrow = n_dims, dimnames = dimnames(bits))
}

.hilbert_transpose_to_axes <- function(bits) {
  bits <- bits == 1L
  n_dims <- nrow(bits)
  p <- ncol(bits)

  t <- c(FALSE, bits[n_dims, seq_len(max(0L, p - 1L))])
  if (n_dims > 1L) {
    for (i in n_dims:2L)
      bits[i, ] <- xor(bits[i, ], bits[i - 1L, ])
  }
  bits[1L, ] <- xor(bits[1L, ], t)

  if (p >= 2L) {
    for (q in seq.int(2L, p)) {
      idx <- p - q + 1L
      lower <- if (idx < p) (idx + 1L):p else integer()
      for (i in rev(seq_len(n_dims))) {
        if (bits[i, idx]) {
          bits[1L, lower] <- !bits[1L, lower]
        } else if (length(lower)) {
          t <- xor(bits[1L, lower], bits[i, lower])
          bits[1L, lower] <- xor(bits[1L, lower], t)
          bits[i, lower] <- xor(bits[i, lower], t)
        }
      }
    }
  }
  matrix(as.integer(bits), nrow = n_dims, dimnames = dimnames(bits))
}

.bits_to_uint64_parts <- function(bits) {
  bits <- as.integer(bits)
  if (length(bits) > 64L)
    stop("Hilbert codes cannot exceed 64 bits", call. = FALSE)
  bits <- c(rep.int(0L, 64L - length(bits)), bits)
  c(
    hilbert_hi = .bits_to_uint32(bits[1L:32L]),
    hilbert_lo = .bits_to_uint32(bits[33L:64L])
  )
}

.bits_to_uint32 <- function(bits) {
  sum(as.numeric(bits) * 2^(31L:0L))
}

.uint64_parts_to_bits <- function(parts, nbits) {
  parts <- .normalize_hilbert_code_values(matrix(parts, nrow = 2L))
  bits <- c(.uint32_to_bits(parts[1L, 1L]), .uint32_to_bits(parts[2L, 1L]))
  tail(bits, nbits)
}

.uint32_to_bits <- function(x) {
  powers <- 2^(31L:0L)
  as.integer(floor(x / powers) %% 2L)
}

.uint32_max <- function() {
  2^32 - 1
}

.normalize_hilbert_code_values <- function(values) {
  values <- as.matrix(values)
  values <- round(values)
  values[values < 0] <- 0
  values[values > .uint32_max()] <- .uint32_max()
  rownames(values) <- c("hilbert_hi", "hilbert_lo")
  values
}

.hilbert_code_numeric <- function(x) {
  codes <- .normalize_hilbert_code_values(x$values)
  codes[1L, ] * 2^32 + codes[2L, ]
}

.compatible_hilbert_specs <- function(x, y) {
  if (!.is_hilbert_specs(x) || !.is_hilbert_specs(y))
    stop("Both Specs objects must be Hilbert-encoded for Hilbert matching",
         call. = FALSE)
  xh <- attr(x, "hilbert_model")
  yh <- attr(y, "hilbert_model")
  if (!identical(xh$bits_per_variable, yh$bits_per_variable) ||
      !identical(xh$n_variables, yh$n_variables) ||
      !identical(xh$original_variables, yh$original_variables) ||
      !isTRUE(all.equal(xh$limits, yh$limits, check.attributes = FALSE))) {
    stop("Hilbert Specs objects must use the same variables, limits, and ",
         "bits_per_variable for distance matching", call. = FALSE)
  }
  xm <- attr(x, "variable_model")
  ym <- attr(y, "variable_model")
  if (!is.null(xm) || !is.null(ym)) {
    if (is.null(xm) || is.null(ym) ||
        !identical(xm$model_id, ym$model_id)) {
      stop("Hilbert Specs objects must use the same PCA model for distance ",
           "matching", call. = FALSE)
    }
  }
  invisible(TRUE)
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
  if (.is_hilbert_specs(x))
    values <- .normalize_hilbert_code_values(values)
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

  compression <- attr(x, "spectrum_compression")
  if (is.null(compression)) compression <- list()
  compression$method <- "kmeans"
  compression$kmeans <- list(
    centers = length(cluster_ids),
    tot.withinss = km$tot.withinss,
    iter = km$iter,
    ifault = km$ifault
  )

  attrs <- list(
    specs_version = attr(x, "specs_version"),
    variable_model = attr(x, "variable_model"),
    hilbert_model = attr(x, "hilbert_model"),
    spectrum_compression = compression,
    transformations = attr(x, "transformations"),
    visual_image = attr(x, "visual_image")
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
