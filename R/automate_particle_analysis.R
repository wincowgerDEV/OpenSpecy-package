#' @rdname automate_particle_analysis
#' @title Automate particle analysis for spectral maps
#'
#' @description
#' `automate_particle_analysis()` generalizes the batch map workflow used for
#' particle detection, spectral matching, particle details, summaries, and
#' optional base-graphics particle images. Visual images attached to map objects
#' or read from supported H5 mosaics are used for particle color extraction when
#' feature definition is requested. It keeps file output optional and returns all
#' results as R objects.
#'
#' @param x character vector of files, an `OpenSpecy`/`Specs` object, or a list
#' of objects/files.
#' @param library reference `OpenSpecy` object or trained model library passed
#' to \code{\link{match_spec}()}.
#' @param output_dir optional directory for CSV/RDS/PNG outputs.
#' @param images optional image path(s) or image objects aligned with `x`.
#' @param bottom_left,top_right optional lists of image corners; if missing and
#' an image is supplied, \code{\link{detect_image_origin}()} is attempted.
#' @param origins optional list with `x` and `y` origin offsets for map-unit
#' outputs.
#' @param material_col material/class column in matched library metadata.
#' @param library_id_col library metadata column used to join match metadata.
#' @param particle_id_strategy one of `"collapse"`, `"partial_collapse"`,
#' `"nonspatial_collapse"`, `"all_cell_id"`, or `"raw"`.
#' @param spectral_smooth,sigma1 apply 3D Gaussian smoothing to spectral maps;
#' file readers apply this while reading and in-memory maps are smoothed after
#' coercion.
#' @param sigma2 shape kernel passed to \code{\link{def_features}()}.
#' @param close,close_kernel passed to \code{\link{def_features}()}.
#' @param sn_threshold_min,sn_threshold_max signal/noise thresholds.
#' @param cor_threshold minimum match value for confident particle labels.
#' @param area_threshold minimum feature area in pixels (inclusive).
#' @param label_unknown logical; label low-correlation matches as `"unknown"`.
#' @param remove_materials optional material labels to remove after matching.
#' @param remove_unknown logical; remove `"unknown"` after matching.
#' @param pixel_length map pixel length used for output dimensions.
#' @param metric,abs signal/noise arguments passed to \code{\link{sig_noise}()}.
#' @param collapse_function function used by \code{\link{collapse_spec}()}.
#' @param outputs character vector containing any of `"details"`, `"summary"`,
#' `"particle_image"`, `"particle_heatmap"`,
#' `"particle_heatmap_thresholded"`, `"cor_heatmap"`, `"sn_histogram"`,
#' `"cor_histogram"`, `"raw"`, `"processed"`, or `"time"`. Short aliases
#' `"heatmap"`, `"thresholded"`, and `"correlation"` are also accepted.
#' @param process_args optional named list overriding \code{\link{process_spec}()}
#' arguments for spectra before matching.
#' @param specs_steps retained for signature compatibility; clustering
#' strategies require the concrete `c("pca", "kmeans")` workflow.
#' @param specs_centers requested K-means cluster count for clustering
#' strategies; the effective count is clamped to the eligible data.
#' @param \ldots catches removed legacy arguments and otherwise is reserved.
#'
#' @return
#' A list with `samples`, `particle_details_all_csv`, and
#' `particle_summary_all_csv`. Each per-sample entry has `particle_details_csv`,
#' `particle_summary_csv`, `particles_raw_rds`, `particles_rds`, and `time_rds`,
#' plus one plot-data list for each requested plot output: `particle_image`,
#' `particle_heatmap`, `particle_heatmap_thresholded`, `cor_heatmap`,
#' `sn_histogram`, and `cor_histogram`. Each plot-data list carries the grid or
#' histogram values needed to build a custom `plot()`/`plotly`/`ggplot2` view
#' (a `type` field plus `x`/`y`/`z`, `values`, `thresholds`, or `levels` as
#' appropriate), or `type = "empty"` with a `reason` string when nothing
#' passed filtering. `output_dir` still writes the matching static PNG/JPG for
#' each requested plot. The result has class `OpenSpecyParticleAnalysis`; use
#' its `plot()` method to draw one of these plots with base graphics.
#'
#' @examples
#' tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' data("test_lib")
#' res <- automate_particle_analysis(tiny_map, test_lib,
#'                                   outputs = c("details", "summary"),
#'                                   sn_threshold_min = 0.1)
#' names(res)
#'
#' @importFrom data.table as.data.table copy fwrite rbindlist setorder
#' @export
automate_particle_analysis <- function(
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
  UseMethod("automate_particle_analysis")
}

#' @rdname automate_particle_analysis
#' @export
automate_particle_analysis.default <- function(
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
  particle_id_strategy <- .normalize_particle_strategy(particle_id_strategy)
  outputs <- .normalize_particle_outputs(outputs)
  samples <- .normalize_particle_samples(x)
  if (!is.null(output_dir)) dir.create(output_dir, recursive = TRUE,
                                       showWarnings = FALSE)

  sample_results <- vector("list", length(samples))
  names(sample_results) <- names(samples)

  for (i in seq_along(samples)) {
    time_start <- Sys.time()
    sample_name <- names(samples)[i]
    .particle_progress(sample_name, "read", sprintf("sample %d of %d", i,
                                                     length(samples)))
    map <- .read_particle_sample(samples[[i]], spectral_smooth = spectral_smooth,
                                 sigma = sigma1)
    map <- .attach_particle_image(map, images, bottom_left, top_right, i)

    origin <- .particle_origin(origins, i)
    .particle_progress(sample_name, "signal/noise")
    snr <- sig_noise(
      restrict_range(map, min = c(750, 2420), max = c(2200, 4000),
                     make_rel = FALSE),
      metric = metric,
      spatial_smooth = FALSE,
      abs = abs
    )
    map$metadata$snr <- snr
    threshold <- snr > sn_threshold_min & snr < sn_threshold_max
    threshold[is.na(threshold)] <- FALSE
    map$metadata$threshold <- threshold
    plot_outputs <- .particle_pre_match_plots(
      map, sample_name, output_dir, outputs, pixel_length, origin,
      sn_threshold_min, sn_threshold_max
    )

    if (!any(threshold) && !particle_id_strategy %in% c("raw",
                                                        "nonspatial_collapse")) {
      sample_results[[i]] <- .empty_particle_result(sample_name, map,
                                                    time_start, outputs,
                                                    plot_outputs, output_dir)
      next
    }

    if (all(threshold) && particle_id_strategy %in%
        c("collapse", "partial_collapse")) {
      stop("S/N thresholds retained every map pixel; choose a higher minimum ",
           "or lower maximum before defining spatial particles", call. = FALSE)
    }

    if (identical(particle_id_strategy, "collapse")) {
      .particle_progress(sample_name, "particle detection and collapse")
    }

    strategy_result <- .particle_strategy_map(
      map = map,
      threshold = threshold,
      library = library,
      particle_id_strategy = particle_id_strategy,
      close = close,
      close_kernel = close_kernel,
      sigma2 = sigma2,
      area_threshold = area_threshold,
      collapse_function = collapse_function,
      process_args = process_args,
      specs_steps = specs_steps,
      specs_centers = specs_centers,
      material_col = material_col,
      library_id_col = library_id_col
    )
    if (is.null(strategy_result)) {
      sample_results[[i]] <- .empty_particle_result(sample_name, map,
                                                    time_start, outputs,
                                                    plot_outputs, output_dir)
      next
    }
    proc_map <- strategy_result$processed
    display_map <- strategy_result$display

    if (is.null(proc_map) || ncol(proc_map$spectra) == 0L) {
      sample_results[[i]] <- .empty_particle_result(sample_name, map,
                                                    time_start, outputs,
                                                    plot_outputs, output_dir)
      next
    }

    .particle_progress(sample_name, "library matching")
    proc_map <- .append_particle_matches(
      proc_map, library = library, material_col = material_col,
      library_id_col = library_id_col
    )
    proc_map <- .filter_particle_matches(
      proc_map,
      material_col = material_col,
      cor_threshold = cor_threshold,
      label_unknown = label_unknown,
      remove_materials = remove_materials,
      remove_unknown = remove_unknown
    )
    display_map <- .join_particle_display_matches(display_map, proc_map,
                                                  material_col)
    map <- display_map

    details <- if ("details" %in% outputs) {
      .particle_details_table(proc_map, sample_name, material_col,
                              cor_threshold, pixel_length, origin)
    } else {
      NULL
    }
    summary <- if ("summary" %in% outputs) {
      .particle_summary_table(proc_map, sample_name, material_col)
    } else {
      NULL
    }
    plot_outputs <- utils::modifyList(
      plot_outputs,
      .particle_post_match_plots(
        map, proc_map, sample_name, output_dir, outputs, material_col,
        pixel_length, origin, cor_threshold
      )
    )

    .particle_progress(sample_name, "outputs")
    elapsed <- Sys.time() - time_start
    if (!is.null(output_dir)) {
      .write_particle_outputs(output_dir, sample_name, map, proc_map, details,
                              summary, outputs, material_col, pixel_length,
                              origin, elapsed)
    }

    sample_results[[i]] <- list(
      sample_id = sample_name,
      particle_details_csv = details,
      particle_summary_csv = summary,
      particles_raw_rds = if ("raw" %in% outputs) map else NULL,
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
  }

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

#' Plot a recorded particle-analysis diagnostic
#'
#' @param x an `OpenSpecyParticleAnalysis` result.
#' @param sample sample name or numeric position.
#' @param which one of `"particle_image"`, `"particle_heatmap"`,
#' `"particle_heatmap_thresholded"`, `"cor_heatmap"`, `"sn_histogram"`, or
#' `"cor_histogram"`. If `NULL`, the first plot with data is used.
#' @param ... reserved for future plotting options.
#'
#' @return `x` invisibly.
#' @export
plot.OpenSpecyParticleAnalysis <- function(x, sample = 1L, which = NULL, ...) {
  samples <- x$samples
  if (is.character(sample)) {
    if (length(sample) != 1L || !sample %in% names(samples)) {
      stop("unknown particle-analysis sample: ", paste(sample, collapse = ", "),
           call. = FALSE)
    }
    item <- samples[[sample]]
  } else {
    sample <- as.integer(sample)[1L]
    if (is.na(sample) || sample < 1L || sample > length(samples)) {
      stop("'sample' is outside the available result range", call. = FALSE)
    }
    item <- samples[[sample]]
  }

  fields <- c("particle_image", "particle_heatmap",
             "particle_heatmap_thresholded", "cor_heatmap", "sn_histogram",
             "cor_histogram")
  present <- fields[vapply(fields, function(nm) !is.null(item[[nm]]),
                          logical(1))]
  if (!length(present)) {
    stop("the selected sample has no plot data; rerun with a broader ",
         "'outputs' argument to request one", call. = FALSE)
  }
  if (is.null(which)) {
    non_empty <- present[vapply(present, function(nm) {
      !identical(item[[nm]]$type, "empty")
    }, logical(1))]
    field <- if (length(non_empty)) non_empty[[1L]] else present[[1L]]
  } else {
    field <- as.character(which)[1L]
    if (!field %in% fields) {
      stop("unknown plot '", which, "'; choose one of: ",
           paste(fields, collapse = ", "), call. = FALSE)
    }
    if (!field %in% present) {
      stop("plot '", which, "' was not requested for this analysis; add ",
           "it to 'outputs' and rerun", call. = FALSE)
    }
  }
  .draw_particle_plot_data(item[[field]])
  invisible(x)
}

.reject_removed_particle_args <- function(args) {
  removed <- intersect(names(args),
                       c("adj_map_baseline", "k", "k_weighting",
                         "vote_count", "spatial_smooth", "top_n"))
  if (length(removed)) {
    stop("Removed automate_particle_analysis argument(s): ",
         paste(removed, collapse = ", "),
         ". Use composable preprocessing, exact best-match output, or explicit ",
         "post-processing outside this workflow.", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_particle_sn_thresholds <- function(minimum, maximum) {
  valid_scalar <- function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value)
  }
  if(!valid_scalar(minimum) || !valid_scalar(maximum) || minimum >= maximum) {
    stop("sn_threshold_min must be a numeric scalar below sn_threshold_max",
         call. = FALSE)
  }
  invisible(TRUE)
}

.normalize_particle_strategy <- function(strategy) {
  strategy <- gsub(" ", "_", as.character(strategy), fixed = TRUE)
  if (length(strategy) > 1L) strategy <- strategy[1L]
  removed <- c("particle_cell_vote", "median_spec_plot", "median_spec")
  if (strategy %in% removed) {
    stop("particle strategy '", strategy, "' has been removed",
         call. = FALSE)
  }
  allowed <- c("collapse", "partial_collapse", "nonspatial_collapse",
               "all_cell_id", "raw")
  if (!strategy %in% allowed) {
    stop("'particle_id_strategy' must be one of ",
         paste(allowed, collapse = ", "), call. = FALSE)
  }
  strategy
}

.validate_particle_specs_steps <- function(steps) {
  expected <- c("pca", "kmeans")
  if (!is.character(steps) || anyNA(steps) || !identical(steps, expected)) {
    stop(
      "'specs_steps' is retained for compatibility, but partial and ",
      "nonspatial collapse now require c(\"pca\", \"kmeans\")",
      call. = FALSE
    )
  }
  invisible(expected)
}

.normalize_particle_outputs <- function(outputs) {
  aliases <- c(heatmap = "particle_heatmap",
               thresholded = "particle_heatmap_thresholded",
               correlation = "cor_heatmap")
  outputs <- as.character(outputs)
  replace <- outputs %in% names(aliases)
  outputs[replace] <- aliases[outputs[replace]]
  unique(outputs)
}

.normalize_particle_samples <- function(x) {
  if (is.character(x) && length(x) > 1L) {
    x <- as.list(x)
  } else if (is.character(x) || is_OpenSpecy(x) || is_Specs(x)) {
    x <- list(x)
  }
  if (!is.list(x)) {
    stop("'x' must be files, OpenSpecy/Specs objects, or a list",
         call. = FALSE)
  }
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  empty <- !nzchar(nms)
  nms[empty] <- vapply(seq_along(x)[empty], function(i) {
    if (is.character(x[[i]]) && length(x[[i]]) == 1L) {
      tools::file_path_sans_ext(basename(x[[i]]))
    } else {
      paste0("sample_", i)
    }
  }, FUN.VALUE = character(1))
  names(x) <- make.unique(nms)
  x
}

.read_particle_sample <- function(x, spectral_smooth, sigma) {
  if (is_OpenSpecy(x)) {
    return(.smooth_particle_sample(as_OpenSpecy(x), spectral_smooth, sigma))
  }
  if (is_Specs(x)) {
    return(.smooth_particle_sample(decompress_spec(x), spectral_smooth, sigma))
  }
  if (!is.character(x) || length(x) != 1L)
    stop("sample entries must be file paths or spectral objects",
         call. = FALSE)
  if (grepl("\\.h5$", x, ignore.case = TRUE)) {
    return(read_h5(x, collapse = FALSE, spectral_smooth = spectral_smooth,
                   sigma = sigma))
  }
  if (grepl("\\.(dat|img)$", x, ignore.case = TRUE)) {
    return(read_envi(x, spectral_smooth = spectral_smooth, sigma = sigma))
  }
  .smooth_particle_sample(read_any(x), spectral_smooth, sigma)
}

.smooth_particle_sample <- function(x, spectral_smooth, sigma) {
  x <- as_OpenSpecy(x)
  if (!isTRUE(spectral_smooth)) return(x)
  md <- data.table::as.data.table(x$metadata)
  can_smooth <- all(c("x", "y", "col_id") %in% names(md)) &&
    !is.null(colnames(x$spectra)) &&
    all(md$col_id %in% colnames(x$spectra))
  if (!can_smooth) return(x)
  spatial_smooth(x, sigma = sigma)
}

.attach_particle_image <- function(map, images, bottom_left, top_right, i) {
  img <- .indexed_argument(images, i)
  if (is.null(img)) return(map)
  bl <- .indexed_argument(bottom_left, i)
  tr <- .indexed_argument(top_right, i)
  detection <- NULL
  if (is.null(bl) || is.null(tr)) {
    detection <- tryCatch(detect_image_origin(img), error = function(e) NULL)
    if (!is.null(detection)) {
      bl <- detection$bottom_left
      tr <- detection$top_right
    }
  }
  if (!is.null(bl) && !is.null(tr)) {
    map <- add_visual_image(
      map, img, bottom_left = bl, top_right = tr,
      detection_method = if (!is.null(detection)) detection$detection_method else NULL,
      diagnostics = if (!is.null(detection)) detection$diagnostics else NULL
    )
  }
  map
}

.indexed_argument <- function(x, i) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) return(x[[min(i, length(x))]])
  if (length(x) >= i) return(x[[i]])
  NULL
}

.particle_origin <- function(origins, i) {
  if (is.null(origins)) return(c(0, 0))
  if (is.list(origins) && all(c("x", "y") %in% names(origins))) {
    return(c(origins$x[min(i, length(origins$x))],
             origins$y[min(i, length(origins$y))]))
  }
  if (is.list(origins)) return(origins[[min(i, length(origins))]])
  c(0, 0)
}

.particle_strategy_map <- function(map, threshold, library,
                                   particle_id_strategy, close,
                                   close_kernel, sigma2, area_threshold,
                                   collapse_function, process_args,
                                   specs_steps, specs_centers, material_col,
                                   library_id_col) {
  if (particle_id_strategy %in%
      c("collapse", "partial_collapse", "nonspatial_collapse")) {
    clustering <- particle_id_strategy %in%
      c("partial_collapse", "nonspatial_collapse")
    if (clustering) {
      .validate_particle_specs_steps(specs_steps)
      centers <- specs_centers
      if (is.null(centers)) {
        centers <- max(1L, min(50L, sum(threshold, na.rm = TRUE)))
      }
    } else {
      # Connected collapse has no PCA/K-means stage, so public compression
      # controls must not change or invalidate it.
      centers <- 1L
    }
    partition <- .partition_particle_map(
      map,
      eligible = threshold,
      strategy = particle_id_strategy,
      pca_components = 10L,
      centers = centers,
      collapse_function = collapse_function,
      area_threshold = area_threshold,
      shape_kernel = sigma2,
      close = close,
      close_kernel = close_kernel,
      seed = 1L
    )
    if (is.null(partition$analysis_units)) return(NULL)
    return(.particle_strategy_result(
      .process_for_particle_match(partition$analysis_units, library,
                                  process_args),
      partition$display,
      pixel_to_unit = partition$pixel_to_unit,
      settings = partition$settings
    ))
  }

  if (identical(particle_id_strategy, "raw")) {
    if (!any(threshold)) return(NULL)
    return(.particle_strategy_result(
      .process_for_particle_match(filter_spec(map, threshold), library,
                                  process_args),
      map
    ))
  }

  if (identical(particle_id_strategy, "all_cell_id")) {
    if (!any(threshold)) return(NULL)
    cell_map <- .process_for_particle_match(filter_spec(map, threshold),
                                            library, process_args)
    cell_map <- .append_particle_matches(cell_map, library, material_col,
                                         library_id_col)
    map <- .join_particle_cell_matches(map, cell_map, material_col)
    material <- map$metadata[[material_col]]
    material[is.na(material) | !threshold] <- "background"
    partition <- .partition_particle_map(
      map,
      eligible = threshold,
      strategy = "collapse",
      material = material,
      collapse_function = collapse_function,
      area_threshold = area_threshold,
      shape_kernel = sigma2,
      close = close,
      close_kernel = close_kernel,
      seed = 1L
    )
    if (is.null(partition$analysis_units)) return(NULL)
    return(.particle_strategy_result(
      .process_for_particle_match(partition$analysis_units, library,
                                  process_args),
      partition$display,
      pixel_to_unit = partition$pixel_to_unit,
      settings = partition$settings
    ))
  }

  stop("unknown particle strategy: ", particle_id_strategy, call. = FALSE)
}

.particle_strategy_result <- function(processed, display,
                                      pixel_to_unit = NULL,
                                      settings = NULL) {
  list(processed = processed, display = display,
       pixel_to_unit = pixel_to_unit, settings = settings)
}

.specs_particle_strategy <- function(map, threshold, library, strategy,
                                     specs_steps, specs_centers, close,
                                     close_kernel, sigma2, process_args) {
  .validate_particle_specs_steps(specs_steps)
  centers <- specs_centers
  if (is.null(centers)) centers <- max(1L, min(50L, sum(threshold)))
  partition <- .partition_particle_map(
    map, eligible = threshold, strategy = strategy,
    pca_components = 10L, centers = centers,
    collapse_function = mean, area_threshold = 1,
    shape_kernel = sigma2, close = close, close_kernel = close_kernel,
    seed = 1L
  )
  if (is.null(partition$analysis_units)) return(NULL)
  .process_for_particle_match(partition$analysis_units, library, process_args)
}

# Partition a spatial-only spectral map into stable analysis units. This helper
# deliberately accepts material identities rather than a library or correlation
# matrix so callers can reuse a single identification pass.
.partition_particle_map <- function(
    x, eligible = rep(TRUE, ncol(x$spectra)),
    strategy = c("collapse", "partial_collapse", "nonspatial_collapse"),
    material = NULL, pca_components = 10L, centers = 10L,
    collapse_function = stats::median, area_threshold = 1,
    shape_kernel = c(3, 3), close = FALSE, close_kernel = c(4, 4),
    seed = 1L) {
  x <- as_OpenSpecy(x)
  strategy <- match.arg(strategy)
  n_pixels <- ncol(x$spectra)
  eligible_input <- .particle_logical_mask(eligible, n_pixels, "eligible")
  material <- .particle_material_vector(material, n_pixels)
  eligible <- eligible_input
  missing_material <- rep(FALSE, n_pixels)
  if (!is.null(material)) {
    missing_material <- is.na(material) | !nzchar(trimws(material))
    eligible <- eligible & !missing_material
  }
  area_threshold <- .particle_scalar_number(
    area_threshold, "area_threshold", minimum = 0
  )
  pca_components <- .particle_positive_integer(pca_components,
                                               "pca_components")
  centers <- .particle_positive_integer(centers, "centers")
  seed <- .particle_integer(seed, "seed")
  collapse <- .particle_collapse_function(collapse_function)

  md <- data.table::as.data.table(x$metadata)
  pixel_id <- colnames(x$spectra)
  if (is.null(pixel_id)) pixel_id <- paste0("pixel_", seq_len(n_pixels))
  if (length(pixel_id) != n_pixels || anyNA(pixel_id) ||
      anyDuplicated(pixel_id)) {
    stop("particle partitioning requires unique spectrum column names",
         call. = FALSE)
  }
  x_coord <- if ("x" %in% names(md)) as.numeric(md$x) else
    seq_len(n_pixels) - 1
  y_coord <- if ("y" %in% names(md)) as.numeric(md$y) else
    rep(0, n_pixels)
  source_id <- .particle_source_vector(md, n_pixels)
  source_levels <- unique(source_id)
  source_index <- match(source_id, source_levels)
  source_token <- sprintf("source_%06d", source_index)

  region_id <- rep(NA_character_, n_pixels)
  cluster_id <- rep(NA_character_, n_pixels)
  effective_components <- 0L
  effective_centers <- integer()

  if (any(eligible)) {
    if (identical(strategy, "collapse")) {
      region_id <- .particle_connected_regions(
        x_coord, y_coord, eligible, material, source_id,
        shape_kernel = shape_kernel, close = close,
        close_kernel = close_kernel
      )
    }

    if (strategy %in% c("partial_collapse", "nonspatial_collapse")) {
      pca <- .particle_shared_pca(x$spectra, eligible, pca_components)
      scores <- pca$scores
      effective_components <- pca$n_components
      grouping <- if (is.null(material)) {
        if (length(source_levels) == 1L) {
          ifelse(eligible, "global", NA_character_)
        } else {
          ifelse(eligible, source_token, NA_character_)
        }
      } else {
        material_group <- paste0("material:", material)
        if (length(source_levels) > 1L) {
          material_group <- paste(source_token, material_group, sep = ":")
        }
        ifelse(eligible, material_group, NA_character_)
      }
      clustered <- .particle_kmeans_groups(
        scores, pixel_index = which(eligible), grouping = grouping[eligible],
        centers = centers, seed = seed
      )
      cluster_id[eligible] <- clustered$cluster_id
      effective_centers <- clustered$effective_centers
    }
  }

  candidate <- rep(NA_character_, n_pixels)
  if (identical(strategy, "collapse")) {
    candidate[eligible] <- paste(source_token[eligible], region_id[eligible],
                                 sep = ":")
  } else {
    group_key <- if (is.null(material)) rep("global", n_pixels) else material
    candidate[eligible] <- paste(
      source_token[eligible], group_key[eligible], cluster_id[eligible],
      sep = ":"
    )
  }
  candidate[!eligible] <- NA_character_
  candidate_levels <- unique(candidate[!is.na(candidate)])
  candidate_area <- tabulate(match(candidate, candidate_levels),
                             nbins = length(candidate_levels))
  names(candidate_area) <- candidate_levels
  area <- unname(candidate_area[candidate])
  keep_candidate <- names(candidate_area)[candidate_area >= area_threshold]
  kept <- !is.na(candidate) & candidate %in% keep_candidate
  kept_levels <- unique(candidate[kept])
  unit_lookup <- stats::setNames(
    sprintf("unit_%06d", seq_along(kept_levels)), kept_levels
  )
  unit_id <- unname(unit_lookup[candidate])
  unit_id[!kept] <- NA_character_
  unit_levels <- unname(unit_lookup[kept_levels])
  unit_index <- match(unit_id, unit_levels)

  rejection_reason <- rep(NA_character_, n_pixels)
  rejection_reason[!eligible_input] <- "threshold"
  rejection_reason[eligible_input & missing_material] <-
    "missing material identity"
  rejection_reason[eligible & !kept] <- "area"
  pixel_to_unit <- data.table::data.table(
    pixel_index = seq_len(n_pixels), pixel_id = as.character(pixel_id),
    source_id = source_id, x = x_coord, y = y_coord, eligible = eligible,
    material = if (is.null(material)) rep(NA_character_, n_pixels) else material,
    region_id = region_id, cluster_id = cluster_id, unit_id = unit_id,
    unit_index = as.integer(unit_index), area = as.integer(area), kept = kept,
    rejection_reason = rejection_reason
  )

  display <- x
  display_md <- data.table::copy(md)
  display_md[, `:=`(
    partition_source = pixel_to_unit$source_id,
    partition_eligible = pixel_to_unit$eligible,
    partition_material = pixel_to_unit$material,
    region_id = pixel_to_unit$region_id,
    cluster_id = pixel_to_unit$cluster_id,
    unit_id = pixel_to_unit$unit_id,
    feature_id = pixel_to_unit$unit_id,
    area = pixel_to_unit$area,
    partition_rejection = pixel_to_unit$rejection_reason
  )]
  display$metadata <- display_md
  display <- .particle_partition_colors(display, pixel_to_unit)

  analysis_units <- .collapse_particle_units(
    display, pixel_to_unit, collapse$fun, geometric = collapse$geometric
  )
  settings <- list(
    strategy = strategy,
    requested_pca_components = pca_components,
    pca_components = effective_components,
    requested_centers = centers,
    centers = effective_centers,
    area_threshold = area_threshold,
    collapse = collapse$name,
    seed = seed
  )
  list(analysis_units = analysis_units, pixel_to_unit = pixel_to_unit,
       display = display, settings = settings)
}

.particle_partition_colors <- function(display, mapping) {
  md <- data.table::copy(data.table::as.data.table(display$metadata))
  # A single visual-image registration cannot disambiguate repeated local x/y
  # coordinates from several source maps. Preserve existing RGB metadata, but
  # do not perform an unsafe cross-source image lookup.
  if (!all(c("r", "g", "b") %in% names(md)) &&
      !is.null(visual_image(display)) &&
      data.table::uniqueN(mapping$source_id) == 1L) {
    vi <- .resolve_visual_image(display)
    if (!is.null(vi$image) && !is.null(vi$bottom_left) &&
        !is.null(vi$top_right) && all(c("x", "y") %in% names(md))) {
      image_raster <- .visual_image_raster(vi$image)
      map_dim <- .visual_map_dim(vi, md)
      xy <- .map_to_image_coords(md$x, md$y, map_dim, vi$bottom_left,
                                 vi$top_right)
      clipped <- .clip_image_coords(
        cbind(xy$y, xy$x), dim(image_raster),
        tolerance = .image_edge_tolerance(
          map_dim, vi$bottom_left, vi$top_right
        )
      )
      colors <- rep(NA_character_, nrow(md))
      if (any(clipped$valid)) {
        colors[clipped$valid] <- image_raster[
          clipped$coords[clipped$valid, , drop = FALSE]
        ]
      }
      rgb <- matrix(NA_integer_, nrow = 3L, ncol = nrow(md))
      if (any(clipped$valid)) {
        rgb[, clipped$valid] <- grDevices::col2rgb(colors[clipped$valid])
      }
      md[, `:=`(r = rgb[1L, ], g = rgb[2L, ], b = rgb[3L, ])]
    }
  }
  display$metadata <- md
  display
}

.particle_logical_mask <- function(x, n, name) {
  if (!is.logical(x) || length(x) != n) {
    stop("'", name, "' must be a logical vector with one value per spectrum",
         call. = FALSE)
  }
  x[is.na(x)] <- FALSE
  x
}

.particle_material_vector <- function(x, n) {
  if (is.null(x)) return(NULL)
  if (length(x) != n) {
    stop("'material' must have one value per spectrum", call. = FALSE)
  }
  as.character(x)
}

.particle_source_vector <- function(metadata, n) {
  if (nrow(metadata) != n) {
    stop("particle metadata must have one row per spectrum", call. = FALSE)
  }
  # H5 inputs may reset x/y within each region, so region is part of the map
  # identity alongside the file identifiers. Mixed uploads legitimately lack
  # some fields row-wise, so encode missing values explicitly instead of
  # dropping a partially populated source column globally.
  available <- intersect(c("file_id", "file_name", "region"), names(metadata))
  informative <- available[vapply(available, function(name) {
    value <- trimws(as.character(metadata[[name]]))
    length(value) == n && any(!is.na(value) & nzchar(value))
  }, logical(1))]
  if (!length(informative)) return(rep("map", n))

  pieces <- lapply(informative, function(name) {
    value <- enc2utf8(as.character(metadata[[name]]))
    present <- !is.na(value) & nzchar(trimws(value))
    # Length-prefix each value so filenames containing separators cannot make
    # two source keys ambiguous. M and V distinguish a missing field from any
    # literal user value.
    ifelse(
      present,
      paste0(name, "=V", nchar(value, type = "bytes"), ":", value),
      paste0(name, "=M")
    )
  })
  do.call(paste, c(pieces, sep = "\034"))
}

.particle_scalar_number <- function(x, name, minimum = -Inf) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < minimum) {
    stop("'", name, "' must be one finite number no smaller than ", minimum,
         call. = FALSE)
  }
  value
}

.particle_positive_integer <- function(x, name) {
  value <- .particle_scalar_number(x, name, minimum = 1)
  if (value != floor(value)) {
    stop("'", name, "' must be a positive whole number", call. = FALSE)
  }
  as.integer(value)
}

.particle_integer <- function(x, name) {
  value <- .particle_scalar_number(x, name)
  if (value != floor(value) || value < -.Machine$integer.max ||
      value > .Machine$integer.max) {
    stop("'", name, "' must be one whole number", call. = FALSE)
  }
  as.integer(value)
}

.particle_connected_regions <- function(x, y, eligible, material, source,
                                        shape_kernel, close, close_kernel) {
  idx <- which(eligible)
  if (!length(idx)) return(rep(NA_character_, length(eligible)))
  if (length(source) != length(eligible) || anyNA(source)) {
    stop("connected particle partitioning requires one source per spectrum",
         call. = FALSE)
  }
  if (any(!is.finite(x[idx])) || any(!is.finite(y[idx]))) {
    stop("connected particle partitioning requires finite x/y coordinates",
         call. = FALSE)
  }
  coordinate_key <- paste(source[idx], x[idx], y[idx], sep = "\r")
  if (anyDuplicated(coordinate_key)) {
    stop(
      "connected particle partitioning requires unique x/y coordinates ",
      "within each source map", call. = FALSE
    )
  }
  labels <- if (is.null(material)) rep("eligible", length(eligible)) else material
  out <- rep(NA_character_, length(eligible))
  next_region <- 0L
  component_kernel <- mmand::shapeKernel(shape_kernel, type = "box")
  close_kernel_object <- if (isTRUE(close)) {
    mmand::shapeKernel(close_kernel, type = "box")
  } else {
    NULL
  }
  for (source_value in unique(source[idx])) {
    source_all <- which(source == source_value)
    source_idx <- idx[source[idx] == source_value]
    x_levels <- sort(unique(x[source_all]))
    y_levels <- sort(unique(y[source_all]))
    x_index <- match(x, x_levels)
    y_index <- match(y, y_levels)
    for (label in unique(labels[source_idx])) {
      label_idx <- source_idx[labels[source_idx] == label]
      binary <- matrix(FALSE, nrow = length(y_levels), ncol = length(x_levels))
      binary[cbind(y_index[label_idx], x_index[label_idx])] <- TRUE
      topology <- if (isTRUE(close)) {
        mmand::closing(binary, close_kernel_object)
      } else {
        binary
      }
      topology[is.na(topology) | is.infinite(topology)] <- FALSE
      components <- mmand::components(topology, component_kernel)
      component <- components[cbind(y_index[label_idx], x_index[label_idx])]
      component_levels <- unique(component[!is.na(component) & component > 0])
      if (!length(component_levels)) next
      normalized <- match(component, component_levels)
      valid <- !is.na(normalized)
      normalized[valid] <- normalized[valid] + next_region
      out[label_idx[valid]] <- sprintf("region_%06d", normalized[valid])
      next_region <- next_region + length(component_levels)
    }
  }
  out
}

.particle_shared_pca <- function(spectra, eligible, requested) {
  idx <- which(eligible)
  data <- t(spectra[, idx, drop = FALSE])
  if (any(!is.finite(data))) {
    stop("PCA clustering requires finite values in every eligible spectrum",
         call. = FALSE)
  }
  if (nrow(data) == 1L) {
    scores <- matrix(0, nrow = 1L, ncol = 1L,
                     dimnames = list(rownames(data), "PC1"))
    return(list(scores = scores, n_components = 1L))
  }
  effective <- max(1L, min(requested, nrow(data) - 1L, ncol(data)))
  fit <- stats::prcomp(data, center = TRUE, scale. = FALSE,
                       rank. = effective)
  available <- min(effective, ncol(fit$x))
  list(scores = fit$x[, seq_len(available), drop = FALSE],
       n_components = as.integer(available))
}

.particle_kmeans_groups <- function(scores, pixel_index, grouping, centers,
                                    seed) {
  if (length(grouping) != nrow(scores) || length(pixel_index) != nrow(scores)) {
    stop("internal particle clustering alignment failure", call. = FALSE)
  }
  if (anyNA(grouping)) {
    stop("internal particle clustering groups cannot be missing",
         call. = FALSE)
  }
  out <- rep(NA_character_, length(grouping))
  groups <- unique(grouping)
  group_code <- match(grouping, groups)
  group_rows <- unname(split(
    seq_along(group_code),
    factor(group_code, levels = seq_along(groups))
  ))
  effective <- integer(length(groups))
  names(effective) <- groups
  for (i in seq_along(groups)) {
    rows <- group_rows[[i]]
    group_scores <- scores[rows, , drop = FALSE]
    unique_rows <- !duplicated(as.data.frame(group_scores))
    k <- min(centers, length(rows), sum(unique_rows))
    effective[[i]] <- k
    if (k <= 1L) {
      cluster <- rep(1L, length(rows))
    } else if (k == length(rows)) {
      cluster <- seq_along(rows)
    } else {
      cluster <- .particle_local_seed(seed + i - 1L, {
        stats::kmeans(group_scores, centers = k, nstart = 10)$cluster
      })
      first_pixel <- tapply(pixel_index[rows], cluster, min)
      relabel <- match(cluster, order(first_pixel))
      cluster <- as.integer(relabel)
    }
    out[rows] <- sprintf("cluster_%06d", cluster)
  }
  list(cluster_id = out, effective_centers = effective)
}

.particle_local_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

.particle_collapse_function <- function(fun) {
  if (is.character(fun) && length(fun) == 1L && !is.na(fun)) {
    name <- tolower(gsub("[ _-]", "", fun))
    if (name %in% c("geometric", "geometricmean", "gmean")) {
      return(list(fun = .particle_geometric_mean, geometric = TRUE,
                  name = "geometric mean"))
    }
    if (identical(name, "mean")) {
      return(list(fun = base::mean, geometric = FALSE, name = "mean"))
    }
    if (identical(name, "median")) {
      return(list(fun = stats::median, geometric = FALSE, name = "median"))
    }
    stop("unknown particle collapse function: ", fun, call. = FALSE)
  }
  FUN <- match.fun(fun)
  # A custom reducer may legitimately contain log() and exp() without being a
  # geometric mean. Positivity is therefore an explicit contract, not a guess
  # based on the function's deparsed body.
  geometric <- identical(FUN, .particle_geometric_mean)
  name <- if (geometric) "geometric mean" else if (identical(FUN, base::mean)) {
    "mean"
  } else if (identical(FUN, stats::median)) {
    "median"
  } else {
    "custom"
  }
  list(fun = FUN, geometric = geometric, name = name)
}

.particle_geometric_mean <- function(x, ...) {
  if (any(!is.finite(x)) || any(x <= 0)) {
    stop("geometric mean collapse requires strictly positive finite values",
         call. = FALSE)
  }
  exp(mean(log(x), ...))
}

.collapse_particle_units <- function(display, mapping, fun, geometric) {
  if (!any(mapping$kept)) return(NULL)
  work <- filter_spec(display, mapping$kept)
  if (isTRUE(geometric) &&
      (any(!is.finite(work$spectra)) || any(work$spectra <= 0))) {
    stop("geometric mean collapse requires strictly positive finite values",
         call. = FALSE)
  }
  work_metadata <- data.table::as.data.table(work$metadata)
  unit_id <- unique(as.character(work_metadata$unit_id))
  unit_rows <- .particle_membership_rows(work_metadata$unit_id, unit_id)
  collapsed <- work
  collapsed$spectra <- .particle_reduce_unit_spectra(
    work$spectra, unit_rows, fun
  )
  colnames(collapsed$spectra) <- unit_id
  collapsed$metadata <- data.table::copy(
    work_metadata[vapply(unit_rows, `[[`, integer(1), 1L)]
  )
  # Retain metadata only when it is genuinely shared by every member. This
  # prevents a collapsed unit from inheriting an arbitrary pixel identity while
  # preserving source-, material-, and group-level values.
  for (name in names(work_metadata)) {
    collapsed$metadata[[name]] <- .particle_constant_unit_metadata(
      work_metadata[[name]], unit_rows
    )
  }
  unit_statistic <- function(values, FUN) {
    vapply(unit_rows, function(rows) FUN(values[rows]), numeric(1))
  }
  # These are feature-level scientific summaries, not constant provenance.
  # Recompute them from every retained pixel so a unit never inherits a stale
  # or first-pixel value from an earlier feature definition.
  if ("snr" %in% names(work_metadata)) {
    collapsed$metadata$mean_snr <- unit_statistic(
      work_metadata$snr, base::mean
    )
  }
  if ("max_cor_val" %in% names(work_metadata)) {
    collapsed$metadata$mean_cor <- unit_statistic(
      work_metadata$max_cor_val, base::mean
    )
  }
  if (all(c("r", "g", "b") %in% names(work_metadata))) {
    for (channel in c("r", "g", "b")) {
      collapsed$metadata[[paste0("mean_", channel)]] <- unit_statistic(
        work_metadata[[channel]], .rms_color_channel
      )
    }
  }
  # collapse_spec() retains metadata from the first member. Geometry produced
  # for an earlier feature definition is invalid after repartitioning, and
  # disconnected spectral clusters do not have one defensible hull. Keep the
  # recomputed centroid/count below and remove unsupported stale geometry.
  stale_geometry <- intersect(
    c("perimeter", "feret_min", "feret_max", "convex_hull_area",
      "first_x", "first_y", "rand_x", "rand_y"),
    names(collapsed$metadata)
  )
  if (length(stale_geometry)) {
    collapsed$metadata[, (stale_geometry) := NULL]
  }
  kept_mapping <- mapping[mapping$kept]
  if (nrow(kept_mapping) != nrow(work_metadata) ||
      !identical(as.character(kept_mapping$unit_id),
                 as.character(work_metadata$unit_id))) {
    stop("internal collapsed particle membership alignment failure",
         call. = FALSE)
  }
  unit_source <- vapply(unit_rows, function(rows) {
    values <- unique(kept_mapping$source_id[rows])
    if (length(values) != 1L || is.na(values)) {
      stop("a collapsed particle unit crossed source maps", call. = FALSE)
    }
    values
  }, character(1))
  unit_x <- vapply(unit_rows, function(rows) mean(kept_mapping$x[rows]),
                   numeric(1))
  unit_y <- vapply(unit_rows, function(rows) mean(kept_mapping$y[rows]),
                   numeric(1))
  connected_geometry <- if (all(is.na(kept_mapping$cluster_id))) {
    .particle_connected_unit_geometry(kept_mapping, unit_id, unit_rows)
  } else {
    NULL
  }

  collapsed$metadata$col_id <- unit_id
  collapsed$metadata$unit_id <- unit_id
  collapsed$metadata$feature_id <- collapsed$metadata$unit_id
  collapsed$metadata$partition_source <- unit_source
  collapsed$metadata$x <- unit_x
  collapsed$metadata$y <- unit_y
  collapsed$metadata$centroid_x <- unit_x
  collapsed$metadata$centroid_y <- unit_y
  collapsed$metadata$unit_index <- seq_len(ncol(collapsed$spectra))
  collapsed$metadata$area <- as.integer(lengths(unit_rows))
  collapsed$metadata$pixel_count <- collapsed$metadata$area
  if (!is.null(connected_geometry)) {
    geometry_index <- match(unit_id, connected_geometry$unit_id)
    for (name in setdiff(names(connected_geometry), "unit_id")) {
      collapsed$metadata[[name]] <- connected_geometry[[name]][geometry_index]
    }
    collapsed$metadata$x <- collapsed$metadata$centroid_x
    collapsed$metadata$y <- collapsed$metadata$centroid_y
  }
  collapsed
}

.particle_reduce_unit_spectra <- function(spectra, unit_rows, fun) {
  FUN <- match.fun(fun)
  out <- matrix(NA_real_, nrow = nrow(spectra), ncol = length(unit_rows))
  for (i in seq_along(unit_rows)) {
    block <- spectra[, unit_rows[[i]], drop = FALSE]
    if (identical(FUN, stats::median)) {
      out[, i] <- matrixStats::rowMedians(block)
    } else if (identical(FUN, base::mean)) {
      out[, i] <- rowMeans(block)
    } else if (identical(FUN, base::sum)) {
      out[, i] <- rowSums(block)
    } else {
      out[, i] <- apply(block, 1L, FUN)
    }
  }
  out
}

.particle_connected_unit_geometry <- function(mapping, unit_id, unit_rows) {
  mapping <- data.table::as.data.table(mapping)
  if (length(unit_rows) != length(unit_id)) {
    stop("internal connected particle membership alignment failure",
         call. = FALSE)
  }
  data.table::rbindlist(Map(function(id, rows) {
    members <- mapping[rows]
    points <- unique(members[, c("x", "y"), with = FALSE])
    if (!nrow(points) || any(!is.finite(points$x)) ||
        any(!is.finite(points$y))) {
      stop("connected particle geometry requires finite x/y coordinates",
           call. = FALSE)
    }
    first <- members[1L]
    if (nrow(points) == 1L) {
      return(data.table::data.table(
        unit_id = id, centroid_x = points$x, centroid_y = points$y,
        first_x = first$x, first_y = first$y, perimeter = 4,
        feret_min = 1, feret_max = 1, convex_hull_area = NA_real_
      ))
    }

    hull <- points[unique(grDevices::chull(points$x, points$y))]
    distances <- as.matrix(stats::dist(hull[, c("x", "y"), with = FALSE]))
    feret_max <- max(distances) + 1
    next_point <- c(seq.int(2L, nrow(hull)), 1L)
    perimeter <- sum(sqrt(
      (hull$x - hull$x[next_point])^2 +
        (hull$y - hull$y[next_point])^2
    ))
    convex_hull_area <- if (nrow(hull) < 3L) {
      0
    } else {
      abs(sum(
        hull$x * hull$y[next_point] - hull$y * hull$x[next_point]
      )) / 2
    }
    area <- nrow(points)
    data.table::data.table(
      unit_id = id,
      centroid_x = mean(points$x), centroid_y = mean(points$y),
      first_x = first$x, first_y = first$y,
      perimeter = perimeter, feret_min = area / feret_max,
      feret_max = feret_max, convex_hull_area = convex_hull_area
    )
  }, unit_id, unit_rows))
}

.particle_membership_rows <- function(membership, unit_id) {
  code <- match(as.character(membership), as.character(unit_id))
  if (length(code) != length(membership) || anyNA(code)) {
    stop("internal particle membership contains an unknown unit",
         call. = FALSE)
  }
  unname(split(
    seq_along(code),
    factor(code, levels = seq_along(unit_id))
  ))
}

.particle_constant_unit_metadata <- function(values, unit_rows) {
  if (is.list(values) && !is.object(values)) {
    out <- vector("list", length(unit_rows))
    for (i in seq_along(unit_rows)) {
      group <- values[unit_rows[[i]]]
      if (length(group) && all(vapply(group, identical, logical(1), group[[1L]]))) {
        out[[i]] <- group[[1L]]
      }
    }
    return(out)
  }
  out <- values[rep(NA_integer_, length(unit_rows))]
  for (i in seq_along(unit_rows)) {
    group <- values[unit_rows[[i]]]
    valid <- !is.na(group)
    if (is.character(group)) valid <- valid & nzchar(trimws(group))
    distinct <- unique(as.character(group[valid]))
    if (length(distinct) == 1L) out[[i]] <- group[which(valid)[[1L]]]
  }
  unname(out)
}

.process_for_particle_match <- function(x, library, process_args) {
  range <- if (is_OpenSpecy(library)) library$wavenumber else library$all_variables
  args <- utils::modifyList(list(
    conform_spec = TRUE,
    conform_spec_args = list(range = range, res = NULL),
    restrict_range = TRUE,
    restrict_range_args = list(min = c(750, 2420), max = c(2200, 4000))
  ), process_args, keep.null = TRUE)
  do.call(process_spec, c(list(x), args))
}

.append_particle_matches <- function(proc_map, library, material_col,
                                     library_id_col) {
  if (is_OpenSpecy(library)) {
    cors <- cor_spec(proc_map, library, compute = "base")
    max_cors <- max_cor_named(cors)
    proc_map$metadata$max_cor_val <- as.numeric(max_cors)
    proc_map$metadata$max_cor_name <- names(max_cors)
    lib_md <- data.table::as.data.table(library$metadata)
    if (all(c(library_id_col, material_col) %in% names(lib_md))) {
      idx <- match(proc_map$metadata$max_cor_name, lib_md[[library_id_col]])
      proc_map$metadata[[material_col]] <- lib_md[[material_col]][idx]
    }
    proc_map$metadata <- data.table::as.data.table(proc_map$metadata)
  } else {
    matches <- match_spec(proc_map, library)
    proc_map$metadata$max_cor_val <- matches$value
    proc_map$metadata[[material_col]] <- matches$name
  }
  proc_map
}

.particle_progress <- function(sample, stage, detail = NULL) {
  text <- paste0("Particle analysis [", sample, "]: ", stage)
  if (!is.null(detail) && nzchar(detail)) {
    text <- paste0(text, " (", detail, ")")
  }
  message(text)
  invisible(NULL)
}

.filter_particle_matches <- function(proc_map, material_col, cor_threshold,
                                     label_unknown, remove_materials,
                                     remove_unknown) {
  if (isTRUE(label_unknown) && material_col %in% names(proc_map$metadata)) {
    low <- proc_map$metadata$max_cor_val < cor_threshold
    low[is.na(low)] <- TRUE
    proc_map$metadata[[material_col]][low] <- "unknown"
  }
  keep <- rep(TRUE, nrow(proc_map$metadata))
  if (!is.null(remove_materials) && material_col %in% names(proc_map$metadata))
    keep <- keep & !proc_map$metadata[[material_col]] %in% remove_materials
  if (isTRUE(remove_unknown) && material_col %in% names(proc_map$metadata))
    keep <- keep & !proc_map$metadata[[material_col]] %in% "unknown"
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(proc_map)
  if (!all(keep)) proc_map <- filter_spec(proc_map, keep)
  proc_map
}

.join_particle_map_matches <- function(map, proc_map, material_col) {
  if (!"feature_id" %in% names(map$metadata) ||
      !"feature_id" %in% names(proc_map$metadata))
    return(map)
  add_cols <- intersect(c("feature_id", material_col, "max_cor_val"),
                        names(proc_map$metadata))
  add <- data.table::as.data.table(proc_map$metadata)[, add_cols, with = FALSE]
  md <- data.table::as.data.table(map$metadata)
  replace_cols <- setdiff(intersect(add_cols, names(md)), "feature_id")
  if (length(replace_cols)) md[, (replace_cols) := NULL]
  md$.row_id <- seq_len(nrow(md))
  md <- merge(md, unique(add),
              by = "feature_id", all.x = TRUE, sort = FALSE)
  data.table::setorder(md, .row_id)
  md$.row_id <- NULL
  map$metadata <- md
  map
}

.join_particle_display_matches <- function(map, proc_map, material_col) {
  if ("feature_id" %in% names(map$metadata) &&
      "feature_id" %in% names(proc_map$metadata)) {
    return(.join_particle_map_matches(map, proc_map, material_col))
  }
  .join_particle_cell_matches(map, proc_map, material_col)
}

.join_particle_cell_matches <- function(map, proc_map, material_col) {
  md <- data.table::as.data.table(map$metadata)
  md$.row_id <- seq_len(nrow(md))
  if (all(c("col_id") %in% names(md)) &&
      "col_id" %in% names(proc_map$metadata)) {
    add_cols <- intersect(c("col_id", "max_cor_val", material_col),
                          names(proc_map$metadata))
    add <- data.table::as.data.table(proc_map$metadata)[, add_cols,
                                                         with = FALSE]
    replace_cols <- setdiff(intersect(add_cols, names(md)), "col_id")
    if (length(replace_cols)) md[, (replace_cols) := NULL]
    md <- merge(md, unique(add), by = "col_id", all.x = TRUE, sort = FALSE)
  } else if (all(c("x", "y") %in% names(md)) &&
             all(c("x", "y") %in% names(proc_map$metadata))) {
    add_cols <- intersect(c("x", "y", "max_cor_val", material_col),
                          names(proc_map$metadata))
    add <- data.table::as.data.table(proc_map$metadata)[, add_cols,
                                                         with = FALSE]
    replace_cols <- setdiff(intersect(add_cols, names(md)), c("x", "y"))
    if (length(replace_cols)) md[, (replace_cols) := NULL]
    md <- merge(md, unique(add), by = c("x", "y"), all.x = TRUE,
                sort = FALSE)
  }
  data.table::setorder(md, .row_id)
  md$.row_id <- NULL
  map$metadata <- md
  map
}

.particle_details_table <- function(proc_map, sample_name, material_col,
                                    cor_threshold, pixel_length, origin) {
  dt <- data.table::copy(data.table::as.data.table(proc_map$metadata))
  dt$particle_id <- if ("feature_id" %in% names(dt)) dt$feature_id else
    colnames(proc_map$spectra)
  dt$sample_id <- sample_name
  if ("area" %in% names(dt)) dt$area_um2 <- dt$area * pixel_length^2
  if ("perimeter" %in% names(dt)) dt$perimeter_um <- dt$perimeter * pixel_length
  if ("feret_max" %in% names(dt)) dt$max_length_um <- dt$feret_max * pixel_length
  if ("feret_min" %in% names(dt)) dt$min_length_um <- dt$feret_min * pixel_length
  if ("centroid_x" %in% names(dt))
    dt$centroid_x <- dt$centroid_x * pixel_length + origin[1L]
  if ("centroid_y" %in% names(dt))
    dt$centroid_y <- dt$centroid_y * pixel_length + origin[2L]
  if ("first_x" %in% names(dt))
    dt$first_x <- dt$first_x * pixel_length + origin[1L]
  if ("first_y" %in% names(dt))
    dt$first_y <- dt$first_y * pixel_length + origin[2L]
  if ("max_cor_val" %in% names(dt))
    dt$bad_spectra <- dt$max_cor_val < cor_threshold
  if ("max_cor_val" %in% names(dt)) {
    dt$acc_analy_conf <- ifelse(dt$max_cor_val > 0.6, "confident",
                                ifelse(dt$max_cor_val < 0.3,
                                       "undetermined", "possible"))
  }
  if (all(c("max_length_um", "min_length_um") %in% names(dt)))
    dt$aspect_ratio <- dt$max_length_um / dt$min_length_um
  if (all(c("perimeter_um", "area_um2") %in% names(dt)))
    dt$circularity <- (dt$perimeter_um^2) / (4 * pi * dt$area_um2)
  if (all(c("mean_r", "mean_g", "mean_b") %in% names(dt))) {
    dt$r <- dt$mean_r
    dt$g <- dt$mean_g
    dt$b <- dt$mean_b
  }
  cols <- intersect(c("particle_id", "sample_id", "max_cor_val",
                      "bad_spectra", material_col,
                      "area_um2", "perimeter_um", "max_length_um",
                      "min_length_um", "aspect_ratio", "circularity",
                      "centroid_x", "centroid_y", "first_x", "first_y",
                      "acc_analy_conf", "max_cor_name", "mean_cor",
                      "mean_snr", "r", "g", "b"), names(dt))
  dt[, cols, with = FALSE]
}

.particle_summary_table <- function(proc_map, sample_name, material_col) {
  dt <- data.table::as.data.table(proc_map$metadata)
  if (!material_col %in% names(dt)) return(data.table::data.table())
  out <- dt[, .(count = .N), by = material_col]
  out$sample_id <- sample_name
  out
}

.particle_pre_match_plots <- function(map, sample_name, output_dir, outputs,
                                      pixel_length, origin, sn_threshold_min,
                                      sn_threshold_max) {
  out <- list()
  if ("particle_heatmap" %in% outputs) {
    out$particle_heatmap <- .particle_heatmap_data(
      map, "snr", pixel_length, origin, legend_title = "Signal/noise",
      title = "Particle Heatmap"
    )
    .write_particle_plot_file(
      output_dir, "particle_heatmap_", sample_name, ".png", "png",
      function() .draw_particle_plot_data(out$particle_heatmap)
    )
  }
  if ("particle_heatmap_thresholded" %in% outputs) {
    out$particle_heatmap_thresholded <- .particle_thresholded_heatmap_data(
      map, pixel_length, origin
    )
    .write_particle_plot_file(
      output_dir, "particle_heatmap_thresholded", sample_name, ".jpg",
      "jpeg",
      function() .draw_particle_plot_data(out$particle_heatmap_thresholded)
    )
  }
  if ("sn_histogram" %in% outputs) {
    out$sn_histogram <- .particle_histogram_data(
      map$metadata$snr, thresholds = c(sn_threshold_min, sn_threshold_max),
      main = "Signal/noise distribution", xlab = "Signal/noise"
    )
    .write_particle_plot_file(
      output_dir, "sn_histogram_", sample_name, ".png", "png",
      function() .draw_particle_plot_data(out$sn_histogram)
    )
  }
  out
}

.particle_post_match_plots <- function(map, proc_map, sample_name, output_dir,
                                       outputs, material_col, pixel_length,
                                       origin, cor_threshold) {
  out <- list()
  if ("particle_image" %in% outputs) {
    out$particle_image <- if (material_col %in% names(map$metadata)) {
      .particle_image_data(map, material_col, pixel_length, origin)
    } else {
      list(type = "empty",
           reason = paste0("the reference library has no '", material_col,
                           "' column, so particles have no material to ",
                           "color"))
    }
    .write_particle_plot_file(
      output_dir, "particle_image_", sample_name, ".png", "png",
      function() {
        if (identical(out$particle_image$type, "empty")) {
          .draw_particle_plot_data(out$particle_image)
        } else {
          cex <- if (is.null(visual_image(map))) 1 else 0.45
          particle_image(map, material_col = material_col,
                         pixel_length = pixel_length, origin = origin,
                         labels = FALSE, cex = cex)
        }
      }
    )
  }
  if ("cor_heatmap" %in% outputs) {
    out$cor_heatmap <- if ("max_cor_val" %in% names(map$metadata)) {
      .particle_correlation_heatmap_data(map, pixel_length, origin)
    } else {
      list(type = "empty",
           reason = "no particles matched the reference library")
    }
    .write_particle_plot_file(
      output_dir, "cor_heatmap_", sample_name, ".png", "png",
      function() .draw_particle_plot_data(out$cor_heatmap)
    )
  }
  if ("cor_histogram" %in% outputs) {
    out$cor_histogram <- if ("max_cor_val" %in% names(proc_map$metadata)) {
      .particle_histogram_data(
        proc_map$metadata$max_cor_val, thresholds = cor_threshold,
        main = "Maximum-correlation distribution", xlab = "Maximum correlation"
      )
    } else {
      list(type = "empty",
           reason = "no particles matched the reference library")
    }
    .write_particle_plot_file(
      output_dir, "cor_histogram_", sample_name, ".png", "png",
      function() .draw_particle_plot_data(out$cor_histogram)
    )
  }
  out
}

.particle_output_path <- function(output_dir, prefix, sample_name, ext) {
  if (is.null(output_dir)) return(NULL)
  file.path(output_dir, paste0(prefix, sample_name, ext))
}

.write_particle_plot_file <- function(output_dir, prefix, sample_name, ext,
                                      device, plot_fun, width = 850,
                                      height = 850, units = "px") {
  filename <- .particle_output_path(output_dir, prefix, sample_name, ext)
  if (is.null(filename)) return(invisible(NULL))
  if (identical(device, "jpeg")) {
    grDevices::jpeg(filename, width = width, height = height, units = units,
                    quality = 95)
  } else {
    grDevices::png(filename, width = width, height = height, units = units)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_fun()
  invisible(filename)
}

# Dispatch a plot-data list (as produced by the .particle_*_data() family)
# to the matching base-graphics drawing routine. Used both to render the
# static PNG/JPG kept for downloads and to replay a plot in
# plot.OpenSpecyParticleAnalysis().
.draw_particle_plot_data <- function(data, main = NULL) {
  if (is.null(data) || identical(data$type, "empty")) {
    graphics::plot.new()
    reason <- if (!is.null(data$reason)) data$reason else "no data available"
    graphics::title(main = if (!is.null(main)) main else "No data",
                    sub = reason, cex.sub = 0.9, col.sub = "grey30")
    return(invisible(data))
  }
  main <- if (!is.null(main)) main else data$title
  switch(
    data$type,
    heatmap = .draw_particle_heatmap(data, main),
    heatmap_binary = .draw_particle_binary_heatmap(data, main),
    heatmap_categorical = .draw_particle_categorical_heatmap(data, main),
    histogram = .draw_particle_histogram(data),
    stop("unknown particle plot data type: '", data$type, "'", call. = FALSE)
  )
  invisible(data)
}

.particle_heatmap_data <- function(map, value_col, pixel_length, origin,
                                   legend_title = value_col, title = NULL) {
  md <- data.table::as.data.table(map$metadata)
  values <- suppressWarnings(as.numeric(md[[value_col]]))
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  list(type = "heatmap", x = grid$x, y = grid$y, z = grid$z,
       value_col = value_col, legend_title = legend_title, title = title)
}

.draw_particle_heatmap <- function(data, main) {
  cols <- grDevices::hcl.colors(100, "Viridis")
  old_par <- graphics::par(mar = graphics::par("mar") + c(0, 0, 0, 6))
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::image(data$x, data$y, data$z, col = cols,
                  xlab = "X (um)", ylab = "Y (um)", main = main, asp = 1)
  .add_particle_continuous_legend(data$z, cols, title = data$legend_title)
  graphics::box()
  invisible(data)
}

.particle_thresholded_heatmap_data <- function(map, pixel_length, origin) {
  md <- data.table::as.data.table(map$metadata)
  values <- as.integer(isTRUE(md$threshold) | (!is.na(md$threshold) &
                                                md$threshold))
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  list(type = "heatmap_binary", x = grid$x, y = grid$y, z = grid$z,
       labels = c("Background", "Threshold"),
       title = "Thresholded Particle Heatmap")
}

.draw_particle_binary_heatmap <- function(data, main) {
  graphics::image(data$x, data$y, data$z, breaks = c(-0.5, 0.5, 1.5),
                  col = c("white", "black"), xlab = "X (um)",
                  ylab = "Y (um)", main = main, asp = 1)
  graphics::box()
  invisible(data)
}

.particle_correlation_heatmap_data <- function(map, pixel_length, origin) {
  md <- data.table::as.data.table(map$metadata)
  bins <- cut(suppressWarnings(as.numeric(md$max_cor_val)),
              c(0, 0.3, 0.6, 0.75, 1), include.lowest = TRUE)
  values <- as.integer(bins)
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  list(type = "heatmap_categorical", x = grid$x, y = grid$y, z = grid$z,
       levels = levels(bins), legend_title = "Correlation",
       title = "Correlation Heatmap")
}

.draw_particle_categorical_heatmap <- function(data, main) {
  cols <- grDevices::hcl.colors(length(data$levels), "Viridis")
  old_par <- graphics::par(mar = graphics::par("mar") + c(0, 0, 0, 6))
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::image(data$x, data$y, data$z,
                  breaks = seq(0.5, length(data$levels) + 0.5, by = 1),
                  col = cols, xlab = "X (um)", ylab = "Y (um)", main = main,
                  asp = 1)
  usr <- graphics::par("usr")
  old_xpd <- graphics::par(xpd = NA)
  on.exit(graphics::par(old_xpd), add = TRUE)
  graphics::legend(
    x = usr[[2L]] + 0.5 * graphics::par("cxy")[[1L]], y = usr[[4L]],
    legend = data$levels, fill = cols, title = data$legend_title,
    cex = 0.85, bty = "n", xjust = 0, yjust = 1
  )
  graphics::box()
  invisible(data)
}

.particle_image_data <- function(map, material_col, pixel_length, origin) {
  md <- data.table::as.data.table(map$metadata)
  material <- as.character(md[[material_col]])
  background <- .particle_background_material(material)
  levels <- sort(unique(material[!background]))
  if (!length(levels)) {
    return(list(type = "empty",
               reason = "every particle matched to background/unknown"))
  }
  palette <- .resolve_particle_palette(material[!background])
  values <- match(material, levels)
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  list(type = "heatmap_categorical", x = grid$x, y = grid$y, z = grid$z,
       levels = levels, legend_title = "Material",
       palette = palette[levels], title = "Particle Image")
}

.particle_histogram_data <- function(values, thresholds, main, xlab) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  thresholds <- unique(as.numeric(thresholds))
  thresholds <- thresholds[is.finite(thresholds)]
  list(type = "histogram", values = values, thresholds = thresholds,
       main = main, xlab = xlab,
       range = if (length(values)) range(values) else c(NA_real_, NA_real_))
}

.draw_particle_histogram <- function(data) {
  if (!length(data$values)) {
    graphics::plot.new()
    graphics::title(main = data$main, xlab = data$xlab)
    return(invisible(data))
  }
  graphics::hist(data$values, breaks = "Sturges", col = "grey80",
                 border = "white", main = data$main, xlab = data$xlab)
  if (length(data$thresholds)) {
    graphics::abline(v = data$thresholds, col = "#D62728", lwd = 2, lty = 2)
  }
  invisible(data)
}

.add_particle_continuous_legend <- function(values, cols, title) {
  scale <- .particle_continuous_scale(values)
  if (is.null(scale)) return(invisible(NULL))
  rng <- scale$range
  ticks <- scale$ticks
  usr <- graphics::par("usr")
  dy <- diff(usr[3:4])
  char_width <- graphics::par("cxy")[[1L]]
  old_xpd <- graphics::par(xpd = NA)
  on.exit(graphics::par(old_xpd), add = TRUE)
  xleft <- usr[[2L]] + 1.0 * char_width
  xright <- usr[[2L]] + 2.0 * char_width
  ybottom <- usr[[3L]] + 0.15 * dy
  ytop <- usr[[3L]] + 0.85 * dy
  gradient <- grDevices::as.raster(matrix(rev(cols), ncol = 1L))
  graphics::rasterImage(
    gradient, xleft, ybottom, xright, ytop, interpolate = TRUE
  )
  graphics::rect(xleft, ybottom, xright, ytop, border = "grey25")
  positions <- if (identical(rng[[1L]], rng[[2L]])) {
    rep((ybottom + ytop) / 2, length(ticks))
  } else {
    ybottom + (ticks - rng[[1L]]) / diff(rng) * (ytop - ybottom)
  }
  graphics::segments(
    xright, positions, xright + 0.3 * char_width, positions, col = "grey20"
  )
  graphics::text(
    xright + 0.4 * char_width, positions,
    labels = format(signif(ticks, 3), trim = TRUE),
    adj = c(0, 0.5), cex = 0.75, col = "grey10"
  )
  graphics::text(
    (xleft + xright) / 2, ytop + 0.06 * dy,
    labels = title, adj = c(0.5, 0), cex = 0.8, col = "grey10"
  )
  scale$legend <- "continuous_gradient"
  invisible(scale)
}

.particle_continuous_scale <- function(values) {
  finite <- suppressWarnings(as.numeric(values))
  finite <- finite[is.finite(finite)]
  if (!length(finite)) return(NULL)
  rng <- range(finite)
  ticks <- pretty(rng, n = 5)
  ticks <- ticks[ticks >= rng[1L] & ticks <= rng[2L]]
  list(range = rng, ticks = sort(unique(c(rng, ticks))))
}

.particle_map_grid <- function(md, values, pixel_length, origin) {
  xs <- sort(unique(md$x))
  ys <- sort(unique(md$y))
  z <- matrix(NA_real_, nrow = length(xs), ncol = length(ys))
  z[cbind(match(md$x, xs), match(md$y, ys))] <- values
  list(x = xs * pixel_length + origin[1L],
       y = ys * pixel_length + origin[2L],
       z = z)
}

.write_particle_outputs <- function(output_dir, sample_name, map, proc_map,
                                    details, summary, outputs, material_col,
                                    pixel_length, origin, elapsed) {
  if ("details" %in% outputs && !is.null(details))
    data.table::fwrite(details, file.path(output_dir,
                                          paste0("particle_details_",
                                                 sample_name, ".csv")))
  if ("summary" %in% outputs && !is.null(summary))
    data.table::fwrite(summary, file.path(output_dir,
                                          paste0("particle_summary_",
                                                 sample_name, ".csv")))
  if ("raw" %in% outputs)
    saveRDS(map, file.path(output_dir, paste0("particles_raw_", sample_name,
                                              ".rds")))
  if ("processed" %in% outputs)
    saveRDS(proc_map, file.path(output_dir, paste0("particles_", sample_name,
                                                   ".rds")))
  if ("time" %in% outputs)
    saveRDS(elapsed, file.path(output_dir, paste0("time_", sample_name,
                                                  ".rds")))
}

.write_particle_all_outputs <- function(output_dir, details, summary, outputs) {
  if ("details" %in% outputs && nrow(details)) {
    data.table::fwrite(details, file.path(output_dir,
                                          "particle_details_all.csv"))
  }
  if ("summary" %in% outputs && nrow(summary)) {
    data.table::fwrite(summary, file.path(output_dir,
                                          "particle_summary_all.csv"))
  }
}

.empty_particle_result <- function(sample_name, map, time_start, outputs,
                                   plot_outputs = list(), output_dir = NULL) {
  elapsed <- Sys.time() - time_start
  note <- paste(
    "no particles passed the current signal/noise, correlation, or area",
    "threshold settings"
  )
  details <- if ("details" %in% outputs) {
    data.table::data.table(sample_id = sample_name,
                           particle_id = NA_character_, note = note)
  } else {
    NULL
  }
  summary <- if ("summary" %in% outputs) {
    data.table::data.table(sample_id = sample_name,
                           material_class = NA_character_, count = 0L,
                           note = note)
  } else {
    NULL
  }
  if (!is.null(output_dir)) {
    if (!is.null(details)) {
      data.table::fwrite(details, file.path(
        output_dir, paste0("particle_details_", sample_name, ".csv")
      ))
    }
    if (!is.null(summary)) {
      data.table::fwrite(summary, file.path(
        output_dir, paste0("particle_summary_", sample_name, ".csv")
      ))
    }
    if ("time" %in% outputs) {
      saveRDS(elapsed, file.path(output_dir,
                                 paste0("time_", sample_name, ".rds")))
    }
  }
  empty_plot <- list(type = "empty", reason = note)
  list(sample_id = sample_name,
       particle_details_csv = details,
       particle_summary_csv = summary,
       particles_raw_rds = if ("raw" %in% outputs) map else NULL,
       particles_rds = NULL,
       particle_image = if ("particle_image" %in% outputs) empty_plot
         else NULL,
       particle_heatmap = plot_outputs$particle_heatmap,
       particle_heatmap_thresholded = plot_outputs$particle_heatmap_thresholded,
       cor_heatmap = if ("cor_heatmap" %in% outputs) empty_plot else NULL,
       sn_histogram = plot_outputs$sn_histogram,
       cor_histogram = if ("cor_histogram" %in% outputs) empty_plot else NULL,
       time_rds = if ("time" %in% outputs) elapsed else NULL)
}

.sample_particle_item <- function(x, name) {
  if (!name %in% names(x)) return(NULL)
  x[[name]]
}

.bind_particle_tables <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x)) return(data.table::data.table())
  data.table::rbindlist(x, fill = TRUE)
}
