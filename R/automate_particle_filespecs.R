#' @rdname automate_particle_analysis
#' @export
automate_particle_analysis.FileSpecs <- function(
    x, library, output_dir = NULL, images = NULL, bottom_left = NULL,
    top_right = NULL, origins = NULL, material_col = "material_class",
    library_id_col = "sample_name",
    particle_id_strategy = c("collapse", "partial_collapse",
                             "nonspatial_collapse", "all_cell_id", "raw"),
    spectral_smooth = FALSE, sigma1 = c(1, 1, 1),
    sigma2 = c(3, 3), close = FALSE,
    close_kernel = c(4, 4), sn_threshold_min = 0.04,
    sn_threshold_max = Inf, cor_threshold = 0.7, area_threshold = 1,
    label_unknown = FALSE, remove_materials = NULL, remove_unknown = FALSE,
    pixel_length = 25, metric = "sig_times_noise", abs = FALSE,
    collapse_function = stats::median,
    outputs = c("details", "summary"),
    process_args = list(), specs_steps = c("pca", "kmeans"),
    specs_centers = NULL, ...) {
  .reject_removed_particle_args(list(...))
  .validate_particle_sn_thresholds(sn_threshold_min, sn_threshold_max)
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = FALSE)
  strategy <- .normalize_particle_strategy(particle_id_strategy)
  if (!identical(strategy, "collapse")) {
    stop("FileSpecs particle analysis currently supports only ",
         "'particle_id_strategy = \"collapse\"'", call. = FALSE)
  }
  if (!identical(match.fun(collapse_function), base::mean)) {
    stop("FileSpecs particle analysis currently requires ",
         "'collapse_function = mean'", call. = FALSE)
  }
  if (isTRUE(spectral_smooth) &&
      (!is.numeric(sigma1) || length(sigma1) != 3L || anyNA(sigma1) ||
       any(sigma1 < 0))) {
    stop("'sigma1' must be a nonnegative numeric vector of length 3 when ",
         "spectral_smooth = TRUE", call. = FALSE)
  }
  if (identical(metric, "entropy")) {
    stop("FileSpecs entropy S/N requires explicit global breaks and is not ",
         "part of the initial particle pipeline", call. = FALSE)
  }

  outputs <- .normalize_particle_outputs(outputs)
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  views <- split_spec(x, by = "region")
  if (!length(views)) views <- list(source = x)
  if (is.null(names(views)) || any(!nzchar(names(views)))) {
    names(views) <- paste0("region_", seq_along(views))
  }

  sample_results <- lapply(seq_along(views), function(i) {
    .particle_progress(names(views)[[i]], "region", sprintf("%d of %d", i,
                                                            length(views)))
    .automate_particle_filespec_region(
      x = views[[i]], library = library, sample_name = names(views)[[i]],
      output_dir = output_dir, image = .indexed_argument(images, i),
      bottom_left = .indexed_argument(bottom_left, i),
      top_right = .indexed_argument(top_right, i),
      origin = .particle_origin(origins, i), material_col = material_col,
      library_id_col = library_id_col,
      spectral_smooth = spectral_smooth, sigma1 = sigma1,
      sigma2 = sigma2, close = close,
      close_kernel = close_kernel,
      sn_threshold_min = sn_threshold_min,
      sn_threshold_max = sn_threshold_max, cor_threshold = cor_threshold,
      area_threshold = area_threshold, label_unknown = label_unknown,
      remove_materials = remove_materials, remove_unknown = remove_unknown,
      pixel_length = pixel_length, metric = metric, abs = abs,
      outputs = outputs, process_args = process_args
    )
  })
  names(sample_results) <- names(views)

  details_all <- .bind_particle_tables(
    lapply(sample_results, .sample_particle_item, "particle_details_csv")
  )
  summary_all <- .bind_particle_tables(
    lapply(sample_results, .sample_particle_item, "particle_summary_csv")
  )
  if (!is.null(output_dir)) {
    .write_particle_all_outputs(output_dir, details_all, summary_all, outputs)
  }
  structure(
    list(samples = sample_results, particle_details_all_csv = details_all,
         particle_summary_all_csv = summary_all),
    class = c("OpenSpecyParticleAnalysis", "list")
  )
}

.automate_particle_filespec_region <- function(
    x, library, sample_name, output_dir, image, bottom_left, top_right,
    origin, material_col, library_id_col, spectral_smooth, sigma1, sigma2,
    close, close_kernel, sn_threshold_min, sn_threshold_max, cor_threshold,
    area_threshold, label_unknown, remove_materials, remove_unknown,
    pixel_length, metric, abs, outputs, process_args,
    chunk_size = getOption("OpenSpecy.filespec.chunk_size", 8192L)) {
  time_start <- Sys.time()
  .particle_progress(sample_name, "index")
  index <- data.table::copy(.filespec_index(x))
  if (!nrow(index)) {
    stop("the FileSpecs region view contains no spectra", call. = FALSE)
  }
  if (is.null(image) && "particle_image" %in% outputs) {
    visual <- .filespec_materialize_visual(x)
    if (!is.null(visual$image)) {
      image <- visual$image
      bottom_left <- visual$bottom_left
      top_right <- visual$top_right
    }
  }
  source_axis <- .filespec_axis(x)
  bands <- which(
    (source_axis >= 750 & source_axis <= 2200) |
      (source_axis >= 2420 & source_axis <= 4000)
  )
  if (!length(bands)) {
    stop("the FileSpecs axis does not overlap the particle S/N ranges",
         call. = FALSE)
  }
  axis <- .filespec_read(x, index = 1L)$wavenumber

  cache_key <- digest::digest(list(
    schema = "filespec-particle-collapse-3", source = x$source$id,
    view = x$view, metric = metric, abs = abs,
    spectral_smooth = isTRUE(spectral_smooth), sigma1 = sigma1,
    sigma2 = sigma2, close = close,
    close_kernel = close_kernel, sn_min = sn_threshold_min,
    sn_max = sn_threshold_max, area = area_threshold,
    image = .filespec_image_identity(image, bottom_left, top_right)
  ))
  cache_file <- .filespec_cache_path(x, "particle-collapse", cache_key)
  cached <- if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else {
    NULL
  }

  if (is.null(cached)) {
    .particle_progress(sample_name, "streaming signal/noise")
    snr <- .filespec_particle_snr(
      x, index = index, bands = bands, metric = metric, abs = abs,
      spectral_smooth = spectral_smooth, sigma1 = sigma1,
      chunk_size = chunk_size
    )
    threshold <- snr > sn_threshold_min & snr < sn_threshold_max
    threshold[is.na(threshold)] <- FALSE
    display <- .filespec_particle_display(index, snr, threshold)
    display <- .attach_particle_image(display, list(image), list(bottom_left),
                                      list(top_right), 1L)

    if (!any(threshold)) {
      cached <- list(snr = snr, threshold = threshold,
                     feature_metadata = NULL, collapsed = NULL)
    } else {
      if (all(threshold)) {
        stop("the FileSpecs thresholds select every pixel; choose thresholds ",
             "that separate particles from background", call. = FALSE)
      }
      id_map <- def_features(
        display, threshold, shape_kernel = sigma2, close = close,
        close_kernel = close_kernel
      )
      region_name <- as.character(index$region[[1L]])
      feature <- as.character(id_map$metadata$feature_id)
      foreground <- !is.na(feature) & feature != "-88"
      feature[foreground] <- paste(region_name, feature[foreground], sep = ":")
      id_map$metadata$feature_id <- feature
      feature_ids <- unique(feature[
        foreground & id_map$metadata$area > area_threshold
      ])
      feature_ids <- feature_ids[!is.na(feature_ids)]
      collapsed <- if (length(feature_ids)) {
        .particle_progress(sample_name, "streaming particle means")
        .filespec_mean_features(
          x, index = index, feature_metadata = id_map$metadata,
          feature_ids = feature_ids, axis = axis,
          spectral_smooth = spectral_smooth, sigma1 = sigma1,
          chunk_size = chunk_size
        )
      } else {
        NULL
      }
      cached <- list(
        snr = snr, threshold = threshold,
        feature_metadata = id_map$metadata, collapsed = collapsed
      )
    }
    .filespec_atomic_save_rds(cached, cache_file)
  } else {
    .particle_progress(sample_name, "reuse cached particle means")
  }

  display <- .filespec_particle_display(index, cached$snr, cached$threshold)
  if (!is.null(cached$feature_metadata)) {
    display$metadata <- data.table::copy(cached$feature_metadata)
  }
  display <- .attach_particle_image(display, list(image), list(bottom_left),
                                    list(top_right), 1L)
  plot_outputs <- .particle_pre_match_plots(
    display, sample_name, output_dir, outputs, pixel_length, origin,
    sn_threshold_min, sn_threshold_max
  )
  if (is.null(cached$collapsed)) {
    out <- .empty_particle_result(sample_name, x, time_start, outputs,
                                  plot_outputs, output_dir)
    return(out)
  }

  .particle_progress(sample_name, "library matching")
  proc_map <- .process_for_particle_match(cached$collapsed, library,
                                          process_args)
  proc_map <- .append_particle_matches(
    proc_map, library = library, material_col = material_col,
    library_id_col = library_id_col
  )
  proc_map <- .filter_particle_matches(
    proc_map, material_col = material_col, cor_threshold = cor_threshold,
    label_unknown = label_unknown, remove_materials = remove_materials,
    remove_unknown = remove_unknown
  )
  display <- .join_particle_display_matches(display, proc_map, material_col)
  details <- if ("details" %in% outputs) {
    .particle_details_table(proc_map, sample_name, material_col,
                            cor_threshold, pixel_length, origin)
  } else NULL
  summary <- if ("summary" %in% outputs) {
    .particle_summary_table(proc_map, sample_name, material_col)
  } else NULL
  plot_outputs <- utils::modifyList(
    plot_outputs,
    .particle_post_match_plots(
      display, proc_map, sample_name, output_dir, outputs, material_col,
      pixel_length, origin, cor_threshold
    )
  )
  .particle_progress(sample_name, "outputs")
  elapsed <- Sys.time() - time_start
  if (!is.null(output_dir)) {
    .write_particle_outputs(
      output_dir, sample_name, x, proc_map, details, summary, outputs,
      material_col, pixel_length, origin, elapsed
    )
  }
  result <- list(
    sample_id = sample_name,
    particle_details_csv = details,
    particle_summary_csv = summary,
    particles_raw_rds = if ("raw" %in% outputs) x else NULL,
    particles_rds = if ("processed" %in% outputs) proc_map else NULL,
    particle_image = plot_outputs$particle_image,
    particle_heatmap = plot_outputs$particle_heatmap,
    particle_heatmap_thresholded = plot_outputs$particle_heatmap_thresholded,
    cor_heatmap = plot_outputs$cor_heatmap,
    sn_histogram = plot_outputs$sn_histogram,
    cor_histogram = plot_outputs$cor_histogram,
    time_rds = if ("time" %in% outputs) elapsed else NULL
  )
  .particle_progress(sample_name, "complete")
  result
}

.filespec_particle_snr <- function(x, index, bands, metric, abs,
                                   spectral_smooth, sigma1, chunk_size) {
  chunks <- .filespec_particle_chunks(x, index, chunk_size)
  out <- rep(NA_real_, nrow(index))
  for (rows in chunks) {
    values <- if (isTRUE(spectral_smooth)) {
      .filespec_smoothed_values(x, index, rows, bands = bands,
                                sigma1 = sigma1)
    } else {
      .filespec_read_values(x, index = rows, bands = bands)
    }
    block <- as_OpenSpecy(
      values$wavenumber, spectra = values$spectra,
      metadata = data.frame(col_id = colnames(values$spectra)),
      coords = "gen_grid", session_id = FALSE, compute_file_id = FALSE
    )
    out[rows] <- sig_noise(block, metric = metric, spatial_smooth = FALSE,
                          abs = abs)
  }
  out
}

# Purely geometric column grouping from row/col grid coordinates, independent
# of backend. `.filespec_particle_chunks()` only applies it for the h5
# backend (matching its original chunking heuristic); halo-based
# spectral_smooth uses it directly for any backend, since correctness of the
# padding math depends only on a complete rectangular grid, not on how the
# backend physically reads pixels.
.filespec_column_chunk_id <- function(index, chunk_size) {
  if (!all(c("row", "col") %in% names(index))) return(NULL)
  rows <- sort(unique(index$row))
  columns <- sort(unique(index$col))
  complete <- nrow(index) == length(rows) * length(columns) &&
    !anyDuplicated(index[, c("row", "col"), with = FALSE])
  if (!isTRUE(complete)) return(NULL)
  columns_per_chunk <- max(1L, floor(as.integer(chunk_size) / length(rows)))
  ceiling(match(index$col, columns) / columns_per_chunk)
}

.filespec_particle_chunks <- function(x, index, chunk_size) {
  positions <- seq_len(nrow(index))
  if (!identical(x$source$backend, "h5")) {
    return(split(positions, ceiling(positions / as.integer(chunk_size))))
  }
  col_chunk <- .filespec_column_chunk_id(index, chunk_size)
  if (is.null(col_chunk)) {
    return(split(positions, ceiling(positions / as.integer(chunk_size))))
  }
  split(positions, col_chunk)
}

# Read a halo-padded column block around `rows` (positions into `index`, a
# single region's complete row x col grid), 3-D Gaussian-smooth it with the
# same mmand::gaussianSmooth() call the eager reader uses, then trim back to
# exactly the requested pixels. The halo equals mmand's own kernel radius for
# `sigma1`, so trimmed values are numerically identical to smoothing the full
# region at once, without ever reading more than one padded column slab.
.filespec_smoothed_values <- function(x, index, rows, bands, sigma1) {
  target <- index[rows]
  if (!all(c("row", "col") %in% names(index))) {
    stop("spectral_smooth requires row/col grid coordinates for this ",
         "FileSpecs region", call. = FALSE)
  }
  rows_all <- sort(unique(index$row))
  cols_all <- sort(unique(index$col))
  complete <- nrow(index) == length(rows_all) * length(cols_all) &&
    !anyDuplicated(index[, c("row", "col"), with = FALSE])
  if (!isTRUE(complete)) {
    stop("spectral_smooth requires a complete rectangular row/col grid for ",
         "this FileSpecs region; irregular regions are not supported",
         call. = FALSE)
  }

  target_cols <- sort(unique(target$col))
  halo <- .gaussian_kernel_half_width(sigma1[[3L]])
  col_lo_i <- max(1L, match(min(target_cols), cols_all) - halo)
  col_hi_i <- min(length(cols_all), match(max(target_cols), cols_all) + halo)
  padded_cols <- cols_all[col_lo_i:col_hi_i]

  block_rows <- which(index$col %in% padded_cols)
  block <- .filespec_read_values(x, index = block_rows, bands = NULL)
  sel <- block$index
  nband <- length(block$wavenumber)

  row_map <- match(sel$row, rows_all)
  col_map <- match(sel$col, padded_cols)
  lin <- (col_map - 1L) * length(rows_all) + row_map
  arr <- matrix(NA_real_, nrow = nband,
                ncol = length(rows_all) * length(padded_cols))
  arr[, lin] <- block$spectra
  dim(arr) <- c(nband, length(rows_all), length(padded_cols))

  smoothed <- mmand::gaussianSmooth(arr, sigma = sigma1)

  band_keep <- if (is.null(bands)) {
    seq_len(nband)
  } else {
    match(x$source$axis[bands], block$wavenumber)
  }
  out_row <- match(target$row, rows_all)
  out_col <- match(target$col, padded_cols)
  out_lin <- (out_col - 1L) * length(rows_all) + out_row
  spectra <- matrix(smoothed, nrow = nband)[band_keep, out_lin, drop = FALSE]
  colnames(spectra) <- target$col_id

  list(wavenumber = block$wavenumber[band_keep], spectra = spectra,
       index = target)
}

.gaussian_kernel_half_width <- function(sigma) {
  if (!is.finite(sigma) || sigma <= 0) return(0L)
  size <- ceiling(6 * sigma)
  if (size %% 2L == 0L) size <- size + 1L
  as.integer((size - 1L) / 2L)
}

.filespec_particle_display <- function(index, snr, threshold) {
  md <- data.table::copy(index)
  md$snr <- as.numeric(snr)
  md$threshold <- as.logical(threshold)
  ids <- if ("col_id" %in% names(md)) as.character(md$col_id) else
    as.character(md$source_id)
  spectra <- matrix(0, nrow = 1L, ncol = nrow(md),
                    dimnames = list("preview", ids))
  as_OpenSpecy(0, spectra = spectra, metadata = md, coords = NULL,
               compute_file_id = FALSE)
}

.filespec_mean_features <- function(x, index, feature_metadata, feature_ids,
                                    axis, spectral_smooth, sigma1,
                                    chunk_size) {
  ids <- as.character(feature_metadata$feature_id)
  keep <- ids %in% feature_ids
  selected <- which(keep)
  sums <- matrix(0, nrow = length(axis), ncol = length(feature_ids),
                 dimnames = list(as.character(axis), feature_ids))
  counts <- integer(length(feature_ids))
  if (isTRUE(spectral_smooth)) {
    col_chunk <- .filespec_column_chunk_id(index, chunk_size)
    if (is.null(col_chunk)) {
      stop("spectral_smooth requires a complete rectangular row/col grid ",
           "for this FileSpecs region", call. = FALSE)
    }
    chunks <- split(selected, col_chunk[selected])
  } else {
    chunks <- split(selected, ceiling(seq_along(selected) /
                                        as.integer(chunk_size)))
  }
  for (rows in chunks) {
    block <- if (isTRUE(spectral_smooth)) {
      .filespec_smoothed_values(x, index, rows, bands = NULL,
                                sigma1 = sigma1)
    } else {
      .filespec_read_values(x, index = rows)
    }
    groups <- match(ids[rows], feature_ids)
    for (group in unique(groups)) {
      cols <- which(groups == group)
      sums[, group] <- sums[, group] + rowSums(block$spectra[, cols,
                                                             drop = FALSE])
      counts[[group]] <- counts[[group]] + length(cols)
    }
  }
  spectra <- sweep(sums, 2L, counts, "/")
  md <- data.table::copy(feature_metadata[match(feature_ids, ids)])
  md$col_id <- feature_ids
  as_OpenSpecy(axis, spectra = spectra, metadata = md, coords = NULL,
               compute_file_id = FALSE)
}

.filespec_image_identity <- function(image, bottom_left, top_right) {
  if (is.character(image) && length(image) == 1L && file.exists(image)) {
    info <- file.info(image)
    image <- list(path = normalizePath(image, winslash = "/"),
                  size = info$size, mtime = info$mtime,
                  sha256 = digest::digest(image, algo = "sha256", file = TRUE))
  } else if (!is.null(image)) {
    image <- list(class = class(image), dim = dim(image),
                  sha256 = digest::digest(image, algo = "sha256",
                                          serialize = TRUE))
  }
  list(image = image, bottom_left = bottom_left, top_right = top_right)
}
