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
#' @param spectral_smooth,sigma1 passed to `read_envi()`/`read_h5()` where
#' supported.
#' @param spatial_smooth,sigma2 passed to \code{\link{sig_noise}()} and
#' feature detection.
#' @param close,close_kernel passed to \code{\link{def_features}()}.
#' @param sn_threshold_min,sn_threshold_max signal/noise thresholds.
#' @param cor_threshold minimum match value for confident particle labels.
#' @param area_threshold minimum feature area in pixels.
#' @param label_unknown logical; label low-correlation matches as `"unknown"`.
#' @param remove_materials optional material labels to remove after matching.
#' @param remove_unknown logical; remove `"unknown"` after matching.
#' @param pixel_length map pixel length used for output dimensions.
#' @param metric,abs signal/noise arguments passed to \code{\link{sig_noise}()}.
#' @param collapse_function function used by \code{\link{collapse_spec}()}.
#' @param outputs character vector containing any of `"details"`, `"summary"`,
#' `"particle_image"`, `"particle_heatmap"`,
#' `"particle_heatmap_thresholded"`, `"cor_heatmap"`, `"raw"`,
#' `"processed"`, or `"time"`. Short aliases `"heatmap"`,
#' `"thresholded"`, and `"correlation"` are also accepted.
#' @param process_args optional named list overriding \code{\link{process_spec}()}
#' arguments for spectra before matching.
#' @param specs_steps,specs_centers compression controls for Specs-based
#' strategies.
#' @param \ldots catches removed legacy arguments and otherwise is reserved.
#'
#' @return
#' A list with `samples`, `particle_details_all_csv`, and
#' `particle_summary_all_csv`. Each per-sample entry uses names matching file
#' exports where applicable: `particle_details_csv`, `particle_summary_csv`,
#' `particles_raw_rds`, `particles_rds`, `particle_image_png`,
#' `particle_heatmap_png`, `particle_heatmap_thresholded_jpg`,
#' `cor_heatmap_png`, and `time_rds`. Image entries are recorded base-graphics
#' plots that can be replayed with \code{\link[grDevices]{replayPlot}()}.
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
    spatial_smooth = FALSE, sigma2 = c(3, 3), close = FALSE,
    close_kernel = c(4, 4), sn_threshold_min = 0.04,
    sn_threshold_max = Inf, cor_threshold = 0.7, area_threshold = 1,
    label_unknown = FALSE, remove_materials = NULL, remove_unknown = FALSE,
    pixel_length = 25, metric = "sig_times_noise", abs = FALSE,
    collapse_function = stats::median,
    outputs = c("details", "summary"),
    process_args = list(), specs_steps = c("pca", "kmeans"),
    specs_centers = NULL, ...) {

  .reject_removed_particle_args(list(...))
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
    map <- .read_particle_sample(samples[[i]], spectral_smooth = spectral_smooth,
                                 sigma = sigma1)
    map <- .attach_particle_image(map, images, bottom_left, top_right, i)

    origin <- .particle_origin(origins, i)
    snr <- sig_noise(
      restrict_range(map, min = c(750, 2420), max = c(2200, 4000),
                     make_rel = FALSE),
      metric = metric,
      spatial_smooth = spatial_smooth,
      sigma = sigma2,
      abs = abs
    )
    map$metadata$snr <- snr
    threshold <- snr > sn_threshold_min & snr < sn_threshold_max
    threshold[is.na(threshold)] <- FALSE
    map$metadata$threshold <- threshold
    plot_outputs <- .particle_pre_match_plots(
      map, sample_name, output_dir, outputs, pixel_length, origin
    )

    if (!any(threshold) && !particle_id_strategy %in% c("raw",
                                                        "nonspatial_collapse")) {
      sample_results[[i]] <- .empty_particle_result(sample_name, map,
                                                    time_start, outputs,
                                                    plot_outputs)
      next
    }

    if (identical(particle_id_strategy, "collapse")) {
      map <- def_features(
        map, threshold, shape_kernel = sigma2, close = close,
        close_kernel = close_kernel
      )
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
                                                    plot_outputs)
      next
    }
    proc_map <- strategy_result$processed
    display_map <- strategy_result$display

    if (is.null(proc_map) || ncol(proc_map$spectra) == 0L) {
      sample_results[[i]] <- .empty_particle_result(sample_name, map,
                                                    time_start, outputs,
                                                    plot_outputs)
      next
    }

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
        pixel_length, origin
      )
    )

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
      particle_image_png = plot_outputs$particle_image_png,
      particle_heatmap_png = plot_outputs$particle_heatmap_png,
      particle_heatmap_thresholded_jpg =
        plot_outputs$particle_heatmap_thresholded_jpg,
      cor_heatmap_png = plot_outputs$cor_heatmap_png,
      time_rds = if ("time" %in% outputs) elapsed else NULL
    )
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
  list(samples = sample_results, particle_details_all_csv = details_all,
       particle_summary_all_csv = summary_all)
}

.reject_removed_particle_args <- function(args) {
  removed <- intersect(names(args),
                       c("adj_map_baseline", "k", "k_weighting",
                         "vote_count"))
  if (length(removed)) {
    stop("Removed automate_particle_analysis argument(s): ",
         paste(removed, collapse = ", "),
         ". Use composable preprocessing, top-1 matching, or explicit ",
         "post-processing outside this workflow.", call. = FALSE)
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
  if (is.character(x) || is_OpenSpecy(x) || is_Specs(x)) x <- list(x)
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
  if (is_OpenSpecy(x)) return(as_OpenSpecy(x))
  if (is_Specs(x)) return(decompress_spec(x))
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
  read_any(x)
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
  if (identical(particle_id_strategy, "collapse")) {
    id_map <- if (all(c("feature_id", "area") %in% names(map$metadata))) {
      map
    } else {
      def_features(
        map, threshold, shape_kernel = sigma2, close = close,
        close_kernel = close_kernel
      )
    }
    proc <- collapse_spec(id_map, fun = collapse_function)
    keep <- proc$metadata$feature_id != "-88" &
      proc$metadata$area > area_threshold
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) return(NULL)
    return(.particle_strategy_result(
      .process_for_particle_match(filter_spec(proc, keep), library,
                                  process_args),
      id_map
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
    id_map <- def_features(map, material, close = close,
                           close_kernel = close_kernel)
    proc <- collapse_spec(id_map, fun = stats::median)
    keep <- !grepl("background", proc$metadata$feature_id) &
      proc$metadata$area > area_threshold
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) return(NULL)
    return(.particle_strategy_result(
      .process_for_particle_match(filter_spec(proc, keep), library,
                                  process_args),
      id_map
    ))
  }

  .particle_strategy_result(
    .specs_particle_strategy(map, threshold, library, particle_id_strategy,
                             specs_steps, specs_centers, close, close_kernel,
                             sigma2, process_args),
    map
  )
}

.particle_strategy_result <- function(processed, display) {
  list(processed = processed, display = display)
}

.specs_particle_strategy <- function(map, threshold, library, strategy,
                                     specs_steps, specs_centers, close,
                                     close_kernel, sigma2, process_args) {
  work <- if (identical(strategy, "nonspatial_collapse") && any(threshold)) {
    filter_spec(map, threshold)
  } else {
    map
  }
  centers <- specs_centers
  if (is.null(centers)) centers <- max(1L, min(50L, ncol(work$spectra)))
  specs <- as_Specs(work, steps = specs_steps, centers = centers, nstart = 5)
  if (identical(strategy, "partial_collapse")) {
    specs <- def_features(specs, threshold, shape_kernel = sigma2,
                          close = close, close_kernel = close_kernel)
    specs <- collapse_spec(specs, fun = mean, column = "feature_id")
  }
  proc <- decompress_spec(specs, expand = FALSE)
  .process_for_particle_match(proc, library, process_args)
}

.process_for_particle_match <- function(x, library, process_args) {
  range <- if (is_OpenSpecy(library)) library$wavenumber else library$all_variables
  args <- utils::modifyList(list(
    conform_spec = TRUE,
    conform_spec_args = list(range = range, res = NULL),
    restrict_range = TRUE,
    restrict_range_args = list(min = c(750, 2420), max = c(2200, 4000))
  ), process_args)
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
                                      pixel_length, origin) {
  out <- list()
  if ("particle_heatmap" %in% outputs) {
    out$particle_heatmap_png <- .capture_particle_plot(
      .particle_output_path(output_dir, "particle_heatmap_", sample_name,
                            ".png"),
      device = "png",
      plot_fun = function() {
        .plot_particle_heatmap(map, "snr", pixel_length, origin,
                               main = "Particle Heatmap")
      }
    )
  }
  if ("particle_heatmap_thresholded" %in% outputs) {
    out$particle_heatmap_thresholded_jpg <- .capture_particle_plot(
      .particle_output_path(output_dir, "particle_heatmap_thresholded",
                            sample_name, ".jpg"),
      device = "jpeg",
      plot_fun = function() {
        .plot_particle_thresholded_heatmap(map, pixel_length, origin)
      }
    )
  }
  out
}

.particle_post_match_plots <- function(map, proc_map, sample_name, output_dir,
                                       outputs, material_col, pixel_length,
                                       origin) {
  out <- list()
  if ("particle_image" %in% outputs &&
      material_col %in% names(map$metadata)) {
    out$particle_image_png <- .capture_particle_plot(
      .particle_output_path(output_dir, "particle_image_", sample_name,
                            ".png"),
      device = "png",
      plot_fun = function() {
        plot_map <- .particle_image_plot_map(map, material_col)
        cex <- if (is.null(visual_image(map))) 1 else 0.45
        particle_image(plot_map, material_col = material_col,
                       pixel_length = pixel_length, origin = origin,
                       labels = FALSE, cex = cex)
      }
    )
  }
  if ("cor_heatmap" %in% outputs && "max_cor_val" %in% names(map$metadata)) {
    out$cor_heatmap_png <- .capture_particle_plot(
      .particle_output_path(output_dir, "cor_heatmap_", sample_name, ".png"),
      device = "png",
      plot_fun = function() {
        .plot_particle_correlation_heatmap(map, pixel_length, origin)
      }
    )
  }
  out
}

.particle_image_plot_map <- function(map, material_col) {
  md <- data.table::as.data.table(map$metadata)
  if (!material_col %in% names(md)) return(map)
  material <- as.character(md[[material_col]])
  keep <- !is.na(material) & nzchar(material) &
    !tolower(material) %in% c("background", "na")
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(map)
  filter_spec(map, keep)
}

.particle_output_path <- function(output_dir, prefix, sample_name, ext) {
  if (is.null(output_dir)) return(NULL)
  file.path(output_dir, paste0(prefix, sample_name, ext))
}

.capture_particle_plot <- function(filename = NULL, device = "png", plot_fun,
                                   width = 850, height = 850,
                                   units = "px") {
  temp_file <- is.null(filename)
  if (temp_file) {
    filename <- tempfile(fileext = if (identical(device, "jpeg")) ".jpg" else ".png")
  }
  opened <- FALSE
  if (identical(device, "jpeg")) {
    grDevices::jpeg(filename, width = width, height = height, units = units,
                    quality = 95)
  } else {
    grDevices::png(filename, width = width, height = height, units = units)
  }
  opened <- TRUE
  on.exit({
    if (opened && grDevices::dev.cur() > 1L) grDevices::dev.off()
    if (temp_file) unlink(filename)
  }, add = TRUE)
  plot_fun()
  out <- grDevices::recordPlot()
  opened <- FALSE
  grDevices::dev.off()
  if (temp_file) unlink(filename)
  out
}

.plot_particle_heatmap <- function(map, value_col, pixel_length, origin,
                                   main) {
  md <- data.table::as.data.table(map$metadata)
  values <- suppressWarnings(as.numeric(md[[value_col]]))
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  graphics::image(grid$x, grid$y, grid$z,
                  col = grDevices::hcl.colors(100, "Viridis"),
                  xlab = "X (um)", ylab = "Y (um)", main = main, asp = 1)
  graphics::box()
}

.plot_particle_thresholded_heatmap <- function(map, pixel_length, origin) {
  md <- data.table::as.data.table(map$metadata)
  values <- as.integer(isTRUE(md$threshold) | (!is.na(md$threshold) &
                                                md$threshold))
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  graphics::image(grid$x, grid$y, grid$z, breaks = c(-0.5, 0.5, 1.5),
                  col = c("white", "black"),
                  xlab = "X (um)", ylab = "Y (um)",
                  main = "Thresholded Particle Heatmap", asp = 1)
  graphics::box()
}

.plot_particle_correlation_heatmap <- function(map, pixel_length, origin) {
  md <- data.table::as.data.table(map$metadata)
  bins <- cut(suppressWarnings(as.numeric(md$max_cor_val)),
              c(0, 0.3, 0.6, 0.75, 1), include.lowest = TRUE)
  values <- as.integer(bins)
  grid <- .particle_map_grid(md, values, pixel_length, origin)
  cols <- grDevices::hcl.colors(4, "Viridis")
  graphics::image(grid$x, grid$y, grid$z, breaks = seq(0.5, 4.5, by = 1),
                  col = cols, xlab = "X (um)", ylab = "Y (um)",
                  main = "Correlation Heatmap", asp = 1)
  graphics::legend("top", legend = levels(bins), fill = cols, horiz = TRUE,
                   cex = 0.65, bty = "n", inset = 0.01)
  graphics::box()
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
                                   plot_outputs = list()) {
  list(sample_id = sample_name,
       particle_details_csv = NULL,
       particle_summary_csv = NULL,
       particles_raw_rds = if ("raw" %in% outputs) map else NULL,
       particles_rds = NULL,
       particle_image_png = plot_outputs$particle_image_png,
       particle_heatmap_png = plot_outputs$particle_heatmap_png,
       particle_heatmap_thresholded_jpg =
         plot_outputs$particle_heatmap_thresholded_jpg,
       cor_heatmap_png = plot_outputs$cor_heatmap_png,
       time_rds = if ("time" %in% outputs) Sys.time() - time_start else NULL)
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
