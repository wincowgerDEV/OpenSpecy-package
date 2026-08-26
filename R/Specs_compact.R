# Compact source mappings and metadata for Specs objects.

#' @rdname Specs
#' @param metric signal/noise metric passed to [sig_noise()].
#' @param minimum,maximum strict accepted signal/noise bounds.
#' @param sigma optional three-dimensional Gaussian smoothing sigma. `NULL`
#'   classifies the unsmoothed spectra.
#' @param step run-length step passed to [sig_noise()].
#' @export
specs_background_filter <- function(metric = "run_sig_over_noise", minimum,
                                    maximum = Inf, sigma = NULL, step = 10) {
  metric <- as.character(metric)
  if (length(metric) != 1L || is.na(metric) || !nzchar(metric)) {
    stop("'metric' must be one nonempty string", call. = FALSE)
  }
  minimum <- suppressWarnings(as.numeric(minimum))
  maximum <- suppressWarnings(as.numeric(maximum))
  if (length(minimum) != 1L || is.na(minimum) ||
      length(maximum) != 1L || is.na(maximum) || minimum >= maximum) {
    stop("'minimum' must be one number below 'maximum'", call. = FALSE)
  }
  if (!is.null(sigma)) {
    sigma <- suppressWarnings(as.numeric(sigma))
    if (length(sigma) != 3L || anyNA(sigma) || any(!is.finite(sigma)) ||
        any(sigma < 0)) {
      stop("'sigma' must be NULL or three nonnegative finite numbers",
           call. = FALSE)
    }
  }
  step <- suppressWarnings(as.numeric(step))
  if (length(step) != 1L || is.na(step) || !is.finite(step) || step <= 0) {
    stop("'step' must be one positive finite number", call. = FALSE)
  }
  structure(
    list(metric = metric, minimum = minimum, maximum = maximum,
         sigma = sigma, step = step),
    class = c("SpecsBackgroundFilter", "list")
  )
}

.validate_specs_background_filter <- function(x) {
  if (is.null(x)) return(NULL)
  if (!inherits(x, "SpecsBackgroundFilter")) {
    stop("'background_filter' must be returned by specs_background_filter()",
         call. = FALSE)
  }
  specs_background_filter(x$metric, x$minimum, x$maximum, x$sigma, x$step)
}

.specs_value_index <- function(x) {
  if (inherits(x$coords, "SpecsCoords")) return(x$coords$value_index)
  coords <- data.table::as.data.table(x$coords)
  if ("value_index" %in% names(coords)) return(as.integer(coords$value_index))
  out <- match(as.character(coords$value_id), colnames(x$values))
  out[as.character(coords$value_id) == "0"] <- 0L
  as.integer(out)
}

#' @rdname Specs
#' @param columns optional coordinate or metadata columns to return.
#' @export
specs_source_count <- function(x) {
  if (!is_Specs(x)) stop("'x' must be a Specs object", call. = FALSE)
  if (inherits(x$coords, "SpecsCoords")) return(as.integer(x$coords$n_source))
  nrow(x$coords)
}

#' @rdname Specs
#' @export
specs_background_mask <- function(x, index = NULL) {
  mapping <- .specs_value_index(x) == 0L
  if (!is.null(index)) mapping <- mapping[.specs_source_index(x, index)]
  unname(mapping)
}

#' @rdname Specs
#' @export
specs_source_values <- function(x, index = NULL) {
  if (!is_Specs(x)) stop("'x' must be a Specs object", call. = FALSE)
  index <- .specs_source_index(x, index)
  mapping <- .specs_value_index(x)[index]
  out <- matrix(0, nrow = nrow(x$values), ncol = length(index),
                dimnames = list(x$variables, NULL))
  foreground <- mapping > 0L
  if (any(foreground)) {
    out[, foreground] <- x$values[, mapping[foreground], drop = FALSE]
  }
  coords <- specs_coordinates(x, index)
  colnames(out) <- coords$source_id
  out
}

.specs_source_index <- function(x, index = NULL) {
  n <- specs_source_count(x)
  if (is.null(index)) return(seq_len(n))
  if (!is.numeric(index) || anyNA(index) || any(index != floor(index)) ||
      any(index < 1L)) {
    stop("'index' must be a positive whole-number vector", call. = FALSE)
  }
  if (any(index > n))
    stop("'index' contains a coordinate row outside x$coords", call. = FALSE)
  if (anyDuplicated(index))
    stop("'index' must not contain duplicate values", call. = FALSE)
  as.integer(index)
}

#' @rdname Specs
#' @export
specs_coordinates <- function(x, index = NULL, columns = NULL) {
  if (!is_Specs(x)) stop("'x' must be a Specs object", call. = FALSE)
  index <- .specs_source_index(x, index)
  if (!inherits(x$coords, "SpecsCoords")) {
    out <- data.table::copy(data.table::as.data.table(x$coords)[index])
    if (!"value_index" %in% names(out)) {
      out[, value_index := .specs_value_index(x)[index]]
    }
  } else {
    model <- x$coords
    region_end <- cumsum(model$regions$n)
    region_start <- region_end - model$regions$n + 1L
    region_index <- findInterval(index - 1L, c(0L, region_end))
    local <- index - region_start[region_index]
    region <- model$regions[region_index]
    x_coord <- region$x_origin + (local %% region$nx) * region$x_step
    y_coord <- region$y_origin + (local %/% region$nx) * region$y_step
    source_id <- if (!is.null(model$source_id)) {
      model$source_id[index]
    } else {
      prefix <- ifelse(is.na(region$id_prefix), "", region$id_prefix)
      paste0(prefix, y_coord, "_", x_coord)
    }
    value_index <- model$value_index[index]
    value_id <- rep("0", length(value_index))
    foreground <- value_index > 0L
    value_id[foreground] <- colnames(x$values)[value_index[foreground]]
    out <- data.table::data.table(
      x = x_coord, y = y_coord, source_id = source_id,
      value_id = value_id, value_index = value_index,
      region = region$name
    )
  }
  if (!is.null(columns)) {
    missing <- setdiff(columns, names(out))
    if (length(missing)) {
      stop("unknown coordinate column(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    out <- out[, columns, with = FALSE]
  }
  out
}

#' @rdname Specs
#' @export
specs_metadata <- function(x, index = NULL, columns = NULL) {
  if (!is_Specs(x)) stop("'x' must be a Specs object", call. = FALSE)
  index <- .specs_source_index(x, index)
  model <- attr(x, "source_metadata")
  if (inherits(model, "SpecsMetadata")) {
    out <- .decode_specs_metadata(model, index)
  } else if (data.table::is.data.table(model) || is.data.frame(model)) {
    out <- data.table::copy(data.table::as.data.table(model)[index])
  } else {
    out <- specs_coordinates(x, index)
    value_index <- out$value_index
    foreground <- value_index > 0L
    value_md <- data.table::as.data.table(x$metadata)
    for (name in setdiff(names(value_md), "value_id")) {
      values <- rep(NA, length(index))
      values[foreground] <- value_md[[name]][value_index[foreground]]
      out[[name]] <- values
    }
  }
  coords <- specs_coordinates(x, index)
  for (name in names(coords)) {
    if (!name %in% names(out)) out[[name]] <- coords[[name]]
  }
  if (!is.null(columns)) {
    missing <- setdiff(columns, names(out))
    if (length(missing)) {
      stop("unknown metadata column(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    out <- out[, columns, with = FALSE]
  }
  out
}

.compact_specs_coords <- function(regions, value_index, source_id = NULL) {
  regions <- data.table::as.data.table(regions)
  required <- c("name", "n", "nx", "ny", "x_origin", "y_origin",
                "x_step", "y_step", "id_prefix")
  if (!all(required %in% names(regions))) {
    stop("compact coordinate regions are incomplete", call. = FALSE)
  }
  n_source <- sum(regions$n)
  if (length(value_index) != n_source || anyNA(value_index) ||
      any(value_index < 0L)) {
    stop("compact value mapping must contain one nonnegative index per source",
         call. = FALSE)
  }
  if (!is.null(source_id) && length(source_id) != n_source) {
    stop("compact source IDs must align with the source mapping",
         call. = FALSE)
  }
  structure(
    list(n_source = as.integer(n_source), regions = regions,
         value_index = as.integer(value_index), source_id = source_id),
    class = c("SpecsCoords", "list")
  )
}

.validate_specs_coords_model <- function(model, n_values) {
  if (!inherits(model, "SpecsCoords") || !is.list(model) ||
      length(model$n_source) != 1L || is.na(model$n_source) ||
      model$n_source < 1L || model$n_source != as.integer(model$n_source)) {
    stop("compact coordinates require one positive whole-number source count",
         call. = FALSE)
  }
  regions <- model$regions
  required <- c("name", "n", "nx", "ny", "x_origin", "y_origin",
                "x_step", "y_step", "id_prefix")
  if (!data.table::is.data.table(regions) || !nrow(regions) ||
      !all(required %in% names(regions))) {
    stop("compact coordinate regions are incomplete", call. = FALSE)
  }
  whole_positive <- function(value) {
    is.numeric(value) && !anyNA(value) && all(is.finite(value)) &&
      all(value >= 1L) && all(value == floor(value))
  }
  if (!whole_positive(regions$n) || !whole_positive(regions$nx) ||
      !whole_positive(regions$ny) ||
      any(regions$n != regions$nx * regions$ny) ||
      sum(regions$n) != model$n_source) {
    stop("compact region dimensions do not match the source count",
         call. = FALSE)
  }
  if (anyNA(regions$name) || any(!nzchar(as.character(regions$name))) ||
      anyDuplicated(as.character(regions$name))) {
    stop("compact region names must be nonempty and unique", call. = FALSE)
  }
  geometry <- unlist(regions[, c("x_origin", "y_origin", "x_step", "y_step"),
                             with = FALSE], use.names = FALSE)
  if (!is.numeric(geometry) || anyNA(geometry) || any(!is.finite(geometry))) {
    stop("compact region geometry must be finite", call. = FALSE)
  }
  mapping <- model$value_index
  if (!is.numeric(mapping) || length(mapping) != model$n_source ||
      anyNA(mapping) || any(mapping != floor(mapping)) || any(mapping < 0L) ||
      any(mapping > n_values)) {
    stop("compact value mapping must select 0 or an available value column",
         call. = FALSE)
  }
  if (!is.null(model$source_id) &&
      (length(model$source_id) != model$n_source || anyNA(model$source_id) ||
       any(!nzchar(as.character(model$source_id))) ||
       anyDuplicated(as.character(model$source_id)))) {
    stop("compact source IDs must be nonempty, unique, and source-aligned",
         call. = FALSE)
  }
  invisible(TRUE)
}

.encode_specs_metadata <- function(metadata, exclude = character()) {
  metadata <- data.table::as.data.table(metadata)
  keep <- setdiff(names(metadata), exclude)
  fields <- lapply(keep, function(name) {
    values <- metadata[[name]]
    unique_values <- unique(values)
    if (length(unique_values) == 1L) {
      list(type = "constant", value = unique_values[[1L]], class = class(values))
    } else if (length(unique_values) <= max(256L, nrow(metadata) %/% 4L)) {
      list(type = "dictionary", values = unique_values,
           index = match(values, unique_values), class = class(values))
    } else {
      list(type = "explicit", values = values, class = class(values))
    }
  })
  names(fields) <- keep
  structure(list(n = nrow(metadata), fields = fields),
            class = c("SpecsMetadata", "list"))
}

.validate_specs_metadata_model <- function(model, n_source) {
  if (is.null(model)) return(invisible(TRUE))
  if (data.table::is.data.table(model) || is.data.frame(model)) {
    if (nrow(model) != n_source) {
      stop("source metadata rows must align with compact sources",
           call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(model, "SpecsMetadata") || !is.list(model) ||
      length(model$n) != 1L || is.na(model$n) || model$n != n_source ||
      !is.list(model$fields)) {
    stop("compact source metadata descriptor is invalid", call. = FALSE)
  }
  for (name in names(model$fields)) {
    field <- model$fields[[name]]
    if (!is.list(field) || length(field$type) != 1L ||
        !field$type %in% c("constant", "dictionary", "explicit")) {
      stop("compact source metadata field '", name, "' is invalid",
           call. = FALSE)
    }
    if (identical(field$type, "constant") && length(field$value) != 1L) {
      stop("compact constant metadata field '", name, "' is invalid",
           call. = FALSE)
    }
    if (identical(field$type, "explicit") &&
        length(field$values) != n_source) {
      stop("compact explicit metadata field '", name, "' is misaligned",
           call. = FALSE)
    }
    if (identical(field$type, "dictionary")) {
      index <- field$index
      if (!length(field$values) || length(index) != n_source || anyNA(index) ||
          any(index != floor(index)) || any(index < 1L) ||
          any(index > length(field$values))) {
        stop("compact dictionary metadata field '", name, "' is invalid",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

.validate_specs_background <- function(x) {
  mapping <- .specs_value_index(x)
  background <- attr(x, "background")
  if (is.null(background)) {
    if (any(mapping == 0L)) {
      stop("source mapping 0 requires background provenance", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  required <- c("mask", "signal_to_noise", "reason", "reason_levels",
                "policy")
  if (!is.list(background) || !all(required %in% names(background)) ||
      !is.logical(background$mask) || length(background$mask) != length(mapping) ||
      !identical(unname(background$mask), unname(mapping == 0L)) ||
      length(background$signal_to_noise) != length(mapping) ||
      length(background$reason) != length(mapping) ||
      anyNA(background$reason) ||
      !identical(background$policy,
                 .validate_specs_background_filter(background$policy))) {
    stop("background provenance is incomplete or source-misaligned",
         call. = FALSE)
  }
  invisible(TRUE)
}

.decode_specs_metadata <- function(model, index = seq_len(model$n)) {
  out <- lapply(model$fields, function(field) {
    if (identical(field$type, "constant")) {
      rep(field$value, length(index))
    } else if (identical(field$type, "dictionary")) {
      field$values[field$index[index]]
    } else {
      field$values[index]
    }
  })
  data.table::as.data.table(out)
}

.specs_attrs <- function(x, overrides = list()) {
  attrs <- list(
    specs_version = attr(x, "specs_version"),
    variable_model = attr(x, "variable_model"),
    hilbert_model = attr(x, "hilbert_model"),
    spectrum_compression = attr(x, "spectrum_compression"),
    transformations = attr(x, "transformations"),
    visual_image = attr(x, "visual_image"),
    background = attr(x, "background"),
    source_metadata = attr(x, "source_metadata"),
    source_attributes = attr(x, "source_attributes")
  )
  utils::modifyList(attrs, overrides, keep.null = TRUE)
}

.specs_value_weights <- function(x) {
  mapping <- .specs_value_index(x)
  tabulate(mapping[mapping > 0L], nbins = ncol(x$values))
}

.fit_specs_pca_weighted <- function(x, n_components, center, scale.) {
  data <- t(x$values)
  weights <- .specs_value_weights(x)
  if (!nrow(data) || !sum(weights)) {
    stop("PCA requires at least one foreground spectrum", call. = FALSE)
  }
  if (n_components > min(nrow(data), ncol(data))) {
    stop("'n_components' must be no larger than the foreground data rank",
         call. = FALSE)
  }
  total <- sum(weights)
  center_value <- if (isTRUE(center)) {
    colSums(data * weights) / total
  } else if (isFALSE(center)) {
    FALSE
  } else {
    center <- as.numeric(center)
    if (length(center) != ncol(data) || anyNA(center)) {
      stop("numeric 'center' must have one value per Specs variable",
           call. = FALSE)
    }
    center
  }
  work <- if (isFALSE(center_value)) data else
    sweep(data, 2L, center_value, "-")
  scale_value <- if (isTRUE(scale.)) {
    if (total <= 1L) {
      stop("scaled PCA requires at least two foreground source spectra",
           call. = FALSE)
    }
    sqrt(colSums((work^2) * weights) / (total - 1))
  } else if (isFALSE(scale.)) {
    FALSE
  } else {
    scale. <- as.numeric(scale.)
    if (length(scale.) != ncol(data) || anyNA(scale.) || any(scale. == 0)) {
      stop("numeric 'scale.' must contain one nonzero value per variable",
           call. = FALSE)
    }
    scale.
  }
  if (!isFALSE(scale_value)) {
    if (any(!is.finite(scale_value)) || any(scale_value == 0)) {
      stop("cannot rescale a constant Specs variable to unit variance",
           call. = FALSE)
    }
    work <- sweep(work, 2L, scale_value, "/")
  }
  weighted <- work * sqrt(weights)
  fit <- base::svd(weighted, nu = 0L, nv = n_components)
  rotation <- fit$v[, seq_len(n_components), drop = FALSE]
  variables <- paste0("PC", seq_len(n_components))
  rownames(rotation) <- as.character(x$variables)
  colnames(rotation) <- variables
  denom <- max(1, total - 1)
  all_variance <- fit$d^2 / denom
  model <- list(
    model_type = "pca",
    original_variables = .specs_variables_for_open_specy(x$variables),
    variables = variables,
    rotation = rotation,
    center = center_value,
    scale = scale_value,
    sdev = fit$d[seq_len(n_components)] / sqrt(denom),
    variance_explained = all_variance[seq_len(n_components)] /
      sum(all_variance),
    source_weight = total,
    weighted = TRUE
  )
  model$model_id <- digest::digest(model)
  class(model) <- c("SpecsPCA", "list")
  model
}

.weighted_specs_kmeans <- function(data, weights, centers, iter.max = 10L,
                                   nstart = 1L) {
  if (!nrow(data)) stop("K-means requires foreground spectra", call. = FALSE)
  if (length(weights) != nrow(data) || any(weights <= 0L)) {
    stop("K-means source weights are invalid", call. = FALSE)
  }
  numeric_centers <- length(centers) == 1L
  if (numeric_centers) {
    k <- as.integer(centers)
    if (is.na(k) || k < 1L || k > nrow(data)) {
      stop("'centers' must be between 1 and the foreground value count",
           call. = FALSE)
    }
  } else {
    centers <- as.matrix(centers)
    if (ncol(centers) != ncol(data)) {
      stop("initial K-means centers must match the Specs variables",
           call. = FALSE)
    }
    k <- nrow(centers)
    nstart <- 1L
  }
  iter.max <- as.integer(iter.max)
  nstart <- as.integer(nstart)
  best <- NULL
  for (start in seq_len(max(1L, nstart))) {
    current <- if (numeric_centers) {
      data[sample.int(nrow(data), k, replace = FALSE, prob = weights), ,
           drop = FALSE]
    } else centers
    cluster <- integer(nrow(data))
    for (iter in seq_len(max(1L, iter.max))) {
      distance <- vapply(seq_len(k), function(j) {
        rowSums((data - matrix(current[j, ], nrow(data), ncol(data),
                               byrow = TRUE))^2)
      }, numeric(nrow(data)))
      assigned <- max.col(-distance, ties.method = "first")
      if (identical(assigned, cluster)) break
      cluster <- assigned
      for (j in seq_len(k)) {
        rows <- which(cluster == j)
        if (!length(rows)) {
          current[j, ] <- data[sample.int(nrow(data), 1L, prob = weights), ]
        } else {
          current[j, ] <- colSums(data[rows, , drop = FALSE] * weights[rows]) /
            sum(weights[rows])
        }
      }
    }
    withinss <- vapply(seq_len(k), function(j) {
      rows <- which(cluster == j)
      if (!length(rows)) return(0)
      sum(rowSums((data[rows, , drop = FALSE] -
                     matrix(current[j, ], length(rows), ncol(data),
                            byrow = TRUE))^2) * weights[rows])
    }, numeric(1))
    candidate <- list(cluster = cluster, centers = current,
                      size = as.integer(vapply(seq_len(k), function(j) {
                        sum(weights[cluster == j])
                      }, numeric(1))), withinss = withinss,
                      tot.withinss = sum(withinss), iter = iter, ifault = 0L)
    if (is.null(best) || candidate$tot.withinss < best$tot.withinss) {
      best <- candidate
    }
  }
  best
}

.background_specs <- function(x, background_filter) {
  policy <- .validate_specs_background_filter(background_filter)
  if (is.null(policy)) return(x)
  if (!is.null(attr(x, "variable_model")) || .is_hilbert_specs(x)) {
    stop("background suppression must run before PCA or Hilbert encoding",
         call. = FALSE)
  }
  dense <- decompress_spec(x)
  basis <- if (is.null(policy$sigma)) dense else
    spatial_smooth(dense, sigma = policy$sigma)
  snr <- sig_noise(basis, metric = policy$metric, step = policy$step,
                   spatial_smooth = FALSE, abs = FALSE)
  .apply_specs_background_result(x, policy, snr, basis = if (
    is.null(policy$sigma)
  ) "uploaded" else "spatially_smoothed")
}

.apply_specs_background_result <- function(x, background_filter,
                                           signal_to_noise,
                                           basis = "uploaded") {
  x <- as_Specs(x)
  policy <- .validate_specs_background_filter(background_filter)
  if (is.null(policy)) return(x)
  if (!is.null(attr(x, "background")) || any(.specs_value_index(x) == 0L)) {
    stop("background suppression has already been applied", call. = FALSE)
  }
  snr <- suppressWarnings(as.numeric(signal_to_noise))
  if (length(snr) != specs_source_count(x)) {
    stop("background classification did not align with source spectra",
         call. = FALSE)
  }
  keep <- is.finite(snr) & snr > policy$minimum & snr < policy$maximum
  reason <- integer(length(snr))
  reason[!is.finite(snr)] <- 3L
  reason[is.finite(snr) & snr <= policy$minimum] <- 1L
  reason[is.finite(snr) & snr >= policy$maximum] <- 2L

  old_index <- .specs_value_index(x)
  source_mapping <- old_index
  source_mapping[!keep] <- 0L
  used <- sort(unique(source_mapping[source_mapping > 0L]))
  remap <- integer(ncol(x$values))
  remap[used] <- seq_along(used)
  source_mapping[keep] <- remap[source_mapping[keep]]
  values <- x$values[, used, drop = FALSE]
  value_ids <- paste0("V", seq_len(ncol(values)))
  colnames(values) <- value_ids
  coords <- x$coords
  if (inherits(coords, "SpecsCoords")) {
    coords$value_index <- source_mapping
  } else {
    coords <- data.table::copy(data.table::as.data.table(coords))
    coords[, value_index := source_mapping]
    source_value_ids <- rep("0", length(source_mapping))
    source_foreground <- source_mapping > 0L
    source_value_ids[source_foreground] <-
      value_ids[source_mapping[source_foreground]]
    coords[, value_id := source_value_ids]
  }
  metadata <- data.table::as.data.table(x$metadata)[used]
  metadata$value_id <- value_ids
  data.table::setcolorder(metadata,
                          c("value_id", setdiff(names(metadata), "value_id")))
  background <- list(
    mask = !keep, signal_to_noise = as.numeric(snr), reason = reason,
    reason_levels = c("foreground", "below_minimum", "above_maximum",
                      "nonfinite"), policy = policy, basis = as.character(basis)
  )
  out <- Specs(x$variables, values, coords, metadata,
               attributes = .specs_attrs(x, list(background = background)))
  .append_specs_transformation(out, list(
    method = "background", retained = sum(keep), suppressed = sum(!keep),
    metric = policy$metric, minimum = policy$minimum,
    maximum = policy$maximum, sigma = policy$sigma,
    basis = as.character(basis), lossy = TRUE
  ))
}
