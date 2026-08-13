if (file.exists("wasm-config.R")) {
  source("wasm-config.R", local = TRUE)
}

app_wasm_mode <- function() {
  env <- tolower(Sys.getenv("OPENSPECY_SHINY_WASM", ""))
  isTRUE(getOption("openspecy.shiny.wasm", FALSE)) ||
    env %in% c("1", "true", "yes", "on")
}

app_local_file_mode <- function() {
  if(app_wasm_mode()) return(FALSE)

  env <- tolower(trimws(Sys.getenv(
    "OPENSPECY_SHINY_LOCAL_FILES", ""
  )))
  isTRUE(getOption("openspecy.shiny.local_files", FALSE)) ||
    env %in% c("1", "true", "yes", "on")
}

app_filespec_cache_dir <- function() {
  configured <- trimws(Sys.getenv(
    "OPENSPECY_FILE_SPECS_CACHE", ""
  ))
  cache_dir <- if(nzchar(configured)) configured else
    file.path(tempdir(), "OpenSpecy-shiny-filespec-cache")

  if(file.exists(cache_dir) && !dir.exists(cache_dir)) {
    stop("The app FileSpecs cache path points to a file.", call. = FALSE)
  }
  if(!dir.exists(cache_dir) && !dir.create(
    cache_dir, recursive = TRUE, showWarnings = FALSE
  )) {
    stop("The app FileSpecs cache directory could not be created.",
         call. = FALSE)
  }
  if(file.access(cache_dir, mode = 2L) != 0L) {
    stop("The app FileSpecs cache directory is not writable.",
         call. = FALSE)
  }

  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

validate_wasm_package_version <- function() {
  if (!app_wasm_mode()) return(invisible(TRUE))

  expected <- getOption("openspecy.shiny.wasm.package_version", "")
  actual <- as.character(utils::packageVersion("OpenSpecy"))
  if (!nzchar(expected) || !identical(actual, expected)) {
    commit <- getOption("openspecy.shiny.wasm.package_sha", "unknown")
    stop(
      "The WebAssembly app loaded OpenSpecy ", actual,
      " but its pinned build requires ", expected,
      " from commit ", commit, ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#remotes::install_github("wincowgerDEV/OpenSpecy-package@vignettes")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(digest)
#library(curl)
#library(loggit)
library(bs4Dash)
library(ggplot2)
library(reshape2)

library(OpenSpecy)
validate_wasm_package_version()
#library(glmnet)

app_download_choices <- function(has_upload, identification,
                                 collapse = FALSE) {
  tests <- c("Test Data", "Test Map")
  metadata <- "User Metadata"
  if (!isTRUE(has_upload)) return(c(tests, metadata))

  choices <- if (isTRUE(identification)) {
    c("Top Matches", "Processed Spectra")
  } else {
    "Processed Spectra"
  }
  if (isTRUE(collapse)) choices <- c(choices, "Thresholded Particles")
  c(choices, tests, metadata)
}

app_download_label <- function(selection) {
  labels <- c(
    "Test Data" = "Download Test Data",
    "Test Map" = "Download Test Map",
    "Processed Spectra" = "Download Processed Spectra",
    "Top Matches" = "Download Top Matches",
    "Thresholded Particles" = "Download Thresholded Particles",
    "User Metadata" = "Download User Metadata"
  )
  if(length(selection) != 1L || is.na(selection) ||
     !selection %in% names(labels)) {
    return("Download selected")
  }
  unname(labels[[selection]])
}

app_upload_limit_bytes <- function(wasm = app_wasm_mode()) {
  2 * 1024^3
}

app_upload_limit_label <- function(wasm = app_wasm_mode()) {
  "2 GB"
}

app_upload_guidance <- function(wasm = app_wasm_mode(),
                                local_file = app_local_file_mode()) {
  local_route <- if(isTRUE(local_file) && !isTRUE(wasm)) {
    "Open Advanced and use Local H5 / ENVI source to bypass the browser copy."
  } else {
    paste(
      "Run the local OpenSpecy app with OpenSpecy::run_app(), then open",
      "Advanced and use Local H5 / ENVI source."
    )
  }
  paste0(
    "The browser upload limit is ", app_upload_limit_label(wasm),
    " total. ", local_route
  )
}

app_particle_output_choices <- function() {
  c(
    "Particle details" = "details",
    "Particle summaries" = "summary",
    "Processed particle object" = "processed",
    "Particle image" = "particle_image",
    "Signal heatmap" = "particle_heatmap",
    "Thresholded heatmap" = "particle_heatmap_thresholded",
    "Correlation heatmap" = "cor_heatmap",
    "Signal/noise histogram" = "sn_histogram",
    "Correlation histogram" = "cor_histogram",
    "Timing" = "time"
  )
}

app_particle_output_files <- function(output_dir, outputs) {
  if(!dir.exists(output_dir)) return(character())
  patterns <- c(
    details = "^particle_details_.*\\.csv$",
    summary = "^particle_summary_.*\\.csv$",
    raw = "^particles_raw_.*\\.rds$",
    processed = "^particles_(?!raw_).*\\.rds$",
    particle_image = "^particle_image_.*\\.png$",
    particle_heatmap = "^particle_heatmap_(?!thresholded).*\\.png$"
  )
  patterns[["particle_heatmap_thresholded"]] <-
    "^particle_heatmap_thresholded.*\\.jpg$"
  patterns[["cor_heatmap"]] <- "^cor_heatmap_.*\\.png$"
  patterns[["sn_histogram"]] <- "^sn_histogram.*\\.png$"
  patterns[["cor_histogram"]] <- "^cor_histogram.*\\.png$"
  patterns[["time"]] <- "^time_.*\\.rds$"
  files <- list.files(output_dir, full.names = TRUE)
  selected <- intersect(as.character(outputs), names(patterns))
  unique(unlist(lapply(selected, function(name) {
    files[grepl(patterns[[name]], basename(files), perl = TRUE)]
  }), use.names = FALSE))
}

app_write_particle_archive <- function(files, destination, root) {
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  relative <- substring(files, nchar(root) + 2L)
  if(!length(files) || any(!startsWith(files, paste0(root, "/"))) ||
     any(!nzchar(relative))) {
    stop("Particle archive files must be inside the completed output directory.",
         call. = FALSE)
  }
  zip::zipr(destination, files = files, root = root,
            include_directories = FALSE)
  invisible(destination)
}

app_draw_server_heatmap <- function(metadata, values, categorical = FALSE,
                                    title = "Map", selected = NULL) {
  if(!is.data.frame(metadata) || !all(c("x", "y") %in% names(metadata))) {
    stop("A map requires x and y metadata.", call. = FALSE)
  }
  if(length(values) != nrow(metadata)) {
    stop("Map values must align with metadata rows.", call. = FALSE)
  }
  x <- as.numeric(metadata$x)
  y <- as.numeric(metadata$y)
  finite_xy <- is.finite(x) & is.finite(y)
  if(!any(finite_xy)) stop("The map has no finite coordinates.", call. = FALSE)

  if(isTRUE(categorical)) {
    labels <- as.character(values)
    levels <- sort(unique(labels[!is.na(labels) & nzchar(labels)]))
    codes <- match(labels, levels)
    palette <- grDevices::hcl.colors(max(1L, length(levels)), "Dark 3")
    legend_labels <- levels
  } else {
    numeric_values <- suppressWarnings(as.numeric(values))
    finite_values <- numeric_values[is.finite(numeric_values)]
    if(!length(finite_values)) stop("The selected map has no finite values.",
                                    call. = FALSE)
    palette <- grDevices::hcl.colors(100L, "Viridis")
    limits <- range(finite_values)
    if(identical(limits[[1L]], limits[[2L]])) {
      codes <- ifelse(is.finite(numeric_values), 50L, NA_integer_)
    } else {
      codes <- floor((numeric_values - limits[[1L]]) / diff(limits) * 99) + 1L
      codes <- pmax(1L, pmin(100L, codes))
    }
    legend_labels <- signif(pretty(limits, n = 5), 3)
    legend_labels <- legend_labels[
      legend_labels >= limits[[1L]] & legend_labels <= limits[[2L]]
    ]
  }
  colors <- rep(NA_character_, length(codes))
  keep <- finite_xy & !is.na(codes)
  colors[keep] <- palette[codes[keep]]

  graphics::par(bg = app_theme$canvas, fg = app_theme$text,
                col.axis = app_theme$text, col.lab = app_theme$text,
                col.main = app_theme$text, mar = c(4.5, 4.8, 3.5, 1.5))
  xs <- sort(unique(x[finite_xy]))
  ys <- sort(unique(y[finite_xy]))
  regular <- length(xs) * length(ys) <= max(5e6, 4 * sum(finite_xy)) &&
    !anyDuplicated(data.frame(x = x[finite_xy], y = y[finite_xy]))
  if(regular) {
    z <- matrix(NA_real_, nrow = length(xs), ncol = length(ys))
    z[cbind(match(x[finite_xy], xs), match(y[finite_xy], ys))] <- codes[finite_xy]
    graphics::image(xs, ys, z, col = palette, xlab = "X", ylab = "Y",
                    main = title, asp = 1, useRaster = TRUE)
  } else {
    graphics::plot(x[finite_xy], y[finite_xy], col = colors[finite_xy],
                   pch = 15, cex = 0.7, xlab = "X", ylab = "Y",
                   main = title, asp = 1)
  }
  if(length(selected) == 1L && is.finite(selected) && selected >= 1L &&
     selected <= nrow(metadata) && finite_xy[[selected]]) {
    graphics::points(x[[selected]], y[[selected]], pch = 0, cex = 1.4,
                     lwd = 2, col = app_plot_palette$reference)
  }
  if(isTRUE(categorical) && length(legend_labels) &&
     length(legend_labels) <= 20L) {
    graphics::legend("topright", legend = legend_labels,
                     fill = palette[seq_along(legend_labels)], cex = 0.75,
                     bty = "n", text.col = app_theme$text)
  } else if(!isTRUE(categorical) && length(legend_labels)) {
    positions <- if(length(legend_labels) == 1L) 50L else
      round(seq(1, 100, length.out = length(legend_labels)))
    graphics::legend("topright", legend = legend_labels,
                     fill = palette[positions], cex = 0.75, bty = "n",
                     text.col = app_theme$text, title = title)
  }
  graphics::box(col = app_theme$text)
  invisible(NULL)
}

app_validate_upload_size <- function(file_info, wasm = app_wasm_mode()) {
  limit <- app_upload_limit_bytes(wasm)
  sizes <- if(is.data.frame(file_info) && "size" %in% names(file_info)) {
    suppressWarnings(as.numeric(file_info$size))
  } else {
    numeric()
  }
  sizes <- sizes[is.finite(sizes) & sizes >= 0]
  total <- sum(sizes)
  ok <- !length(sizes) || total <= limit
  guidance <- app_upload_guidance(
    wasm = wasm,
    local_file = !isTRUE(wasm) && app_local_file_mode()
  )
  list(ok = ok, size = total, limit = limit, message = guidance)
}

app_filespec_coordinates <- function(index) {
  if(!is.data.frame(index) || !nrow(index)) {
    stop("A FileSpecs preview requires a nonempty index.", call. = FALSE)
  }
  stage <- all(c("stage_x_nm", "stage_y_nm") %in% names(index)) &&
    all(is.finite(index$stage_x_nm)) && all(is.finite(index$stage_y_nm))
  if(stage) {
    list(
      x = as.numeric(index$stage_x_nm),
      y = as.numeric(index$stage_y_nm),
      xlab = "Stage X (nm)", ylab = "Stage Y (nm)"
    )
  } else {
    if(!all(c("x", "y") %in% names(index))) {
      stop("The FileSpecs index has no plottable coordinates.", call. = FALSE)
    }
    list(
      x = as.numeric(index$x), y = as.numeric(index$y),
      xlab = "X", ylab = "Y"
    )
  }
}

app_filespec_region_rows <- function(index, region = NULL) {
  if(!is.data.frame(index) || !nrow(index) || !"region" %in% names(index)) {
    stop("A FileSpecs preview requires indexed regions.", call. = FALSE)
  }
  regions <- unique(as.character(index$region))
  if(is.null(region) || !length(region) || is.na(region[[1L]]) ||
     !nzchar(region[[1L]])) region <- regions[[1L]]
  region <- as.character(region[[1L]])
  if(!region %in% regions) {
    stop("The selected FileSpecs region is unavailable.", call. = FALSE)
  }
  which(as.character(index$region) == region)
}

app_filespec_extent <- function(index, region = NULL) {
  rows <- app_filespec_region_rows(index, region)
  selected <- index[rows, , drop = FALSE]
  coordinates <- app_filespec_coordinates(selected)
  keep <- is.finite(coordinates$x) & is.finite(coordinates$y)
  if(!any(keep)) {
    stop("The selected FileSpecs region has no finite coordinates.",
         call. = FALSE)
  }
  expand_range <- function(value) {
    value <- range(value)
    if(diff(value) > 0) value else value + c(-0.5, 0.5)
  }
  c(
    xmin = expand_range(coordinates$x[keep])[[1L]],
    xmax = expand_range(coordinates$x[keep])[[2L]],
    ymin = expand_range(coordinates$y[keep])[[1L]],
    ymax = expand_range(coordinates$y[keep])[[2L]]
  )
}

app_filespec_viewport <- function(index, region = NULL, roi = NULL) {
  extent <- app_filespec_extent(index, region)
  if(is.null(roi)) return(extent)
  roi <- suppressWarnings(as.numeric(roi))
  if(length(roi) != 4L || any(!is.finite(roi))) {
    stop("A FileSpecs viewport must be c(xmin, xmax, ymin, ymax).",
         call. = FALSE)
  }
  roi <- c(
    xmin = max(min(roi[1:2]), extent[["xmin"]]),
    xmax = min(max(roi[1:2]), extent[["xmax"]]),
    ymin = max(min(roi[3:4]), extent[["ymin"]]),
    ymax = min(max(roi[3:4]), extent[["ymax"]])
  )
  if(roi[["xmin"]] >= roi[["xmax"]] ||
     roi[["ymin"]] >= roi[["ymax"]]) {
    stop("The FileSpecs viewport does not overlap a positive map area.",
         call. = FALSE)
  }
  roi
}

app_filespec_preview <- function(index, region = NULL, roi = NULL,
                                 max_width = 512L, max_height = 512L) {
  max_width <- suppressWarnings(as.integer(max_width))
  max_height <- suppressWarnings(as.integer(max_height))
  if(length(max_width) != 1L || is.na(max_width) || max_width < 1L ||
     length(max_height) != 1L || is.na(max_height) || max_height < 1L) {
    stop("Preview dimensions must be positive whole numbers.", call. = FALSE)
  }
  rows <- app_filespec_region_rows(index, region)
  selected <- index[rows, , drop = FALSE]
  coordinates <- app_filespec_coordinates(selected)
  viewport <- app_filespec_viewport(index, region, roi)
  finite <- is.finite(coordinates$x) & is.finite(coordinates$y)
  keep <- finite &
    coordinates$x >= viewport[["xmin"]] &
    coordinates$x <= viewport[["xmax"]] &
    coordinates$y >= viewport[["ymin"]] &
    coordinates$y <= viewport[["ymax"]]
  if(!any(keep)) {
    stop("The FileSpecs viewport contains no indexed pixels.",
         call. = FALSE)
  }
  x <- coordinates$x[keep]
  y <- coordinates$y[keep]
  x_range <- viewport[c("xmin", "xmax")]
  y_range <- viewport[c("ymin", "ymax")]
  x_unique <- length(unique(x))
  y_unique <- length(unique(y))
  width <- min(max_width, max(1L, x_unique))
  height <- min(max_height, max(1L, y_unique))
  x_bin <- if(diff(x_range) == 0) rep.int(1L, length(x)) else {
    pmin(width, floor((x - x_range[[1L]]) / diff(x_range) * width) + 1L)
  }
  y_bin <- if(diff(y_range) == 0) rep.int(1L, length(y)) else {
    pmin(height, floor((y - y_range[[1L]]) / diff(y_range) * height) + 1L)
  }
  counts <- matrix(
    tabulate((y_bin - 1L) * width + x_bin, nbins = width * height),
    nrow = height, ncol = width, byrow = TRUE
  )
  list(
    counts = counts,
    xlim = unname(x_range), ylim = unname(y_range),
    xlab = coordinates$xlab, ylab = coordinates$ylab,
    region = as.character(selected$region[[1L]]),
    spectra = length(x), total_spectra = sum(finite),
    viewport = viewport
  )
}

app_filespec_nearest_position <- function(index, region, x, y, roi = NULL) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  if(length(x) != 1L || length(y) != 1L || !is.finite(x) || !is.finite(y)) {
    return(integer())
  }
  rows <- app_filespec_region_rows(index, region)
  coordinates <- app_filespec_coordinates(index[rows, , drop = FALSE])
  viewport <- app_filespec_viewport(index, region, roi)
  keep <- coordinates$x >= viewport[["xmin"]] &
    coordinates$x <= viewport[["xmax"]] &
    coordinates$y >= viewport[["ymin"]] &
    coordinates$y <= viewport[["ymax"]]
  rows <- rows[keep]
  coordinates$x <- coordinates$x[keep]
  coordinates$y <- coordinates$y[keep]
  if(!length(rows)) return(integer())
  distance <- (coordinates$x - x)^2 + (coordinates$y - y)^2
  distance[!is.finite(distance)] <- Inf
  if(!any(is.finite(distance))) return(integer())
  as.integer(rows[[which.min(distance)]])
}

app_draw_filespec_preview <- function(preview, selected = NULL) {
  counts <- preview$counts
  density <- log1p(counts)
  maximum <- max(density, na.rm = TRUE)
  palette <- grDevices::colorRampPalette(c("#10243A", "#38BDF8", "#F0E442"))(
    64L
  )
  color_index <- if(is.finite(maximum) && maximum > 0) {
    pmax(1L, pmin(64L, ceiling(density / maximum * 64L)))
  } else {
    matrix(1L, nrow(counts), ncol(counts))
  }
  colors <- matrix("#050B14", nrow(counts), ncol(counts))
  colors[counts > 0] <- palette[color_index[counts > 0]]
  graphics::plot.new()
  graphics::plot.window(preview$xlim, preview$ylim, asp = 1)
  graphics::rasterImage(
    grDevices::as.raster(colors[nrow(colors):1L, , drop = FALSE]),
    preview$xlim[[1L]], preview$ylim[[1L]],
    preview$xlim[[2L]], preview$ylim[[2L]], interpolate = FALSE
  )
  graphics::axis(1, col = app_theme$axis, col.axis = app_theme$text)
  graphics::axis(2, col = app_theme$axis, col.axis = app_theme$text)
  graphics::box(col = app_theme$border)
  graphics::title(
    main = paste0(
      preview$region, " — ", format(preview$spectra, big.mark = ","),
      if(preview$spectra < preview$total_spectra) paste0(
        " of ", format(preview$total_spectra, big.mark = ","), " visible pixels"
      ) else " indexed pixels"
    ),
    xlab = preview$xlab, ylab = preview$ylab, col.main = app_theme$text,
    col.lab = app_theme$text
  )
  if(is.list(selected) && all(c("x", "y") %in% names(selected)) &&
     is.finite(selected$x) && is.finite(selected$y)) {
    graphics::points(selected$x, selected$y, pch = 4L, lwd = 2.5,
                     cex = 1.4, col = "#FB7185")
  }
  invisible(preview)
}

app_uploaded_metadata_cache <- function(x, signal_to_noise) {
  spectrum_ids <- colnames(x$spectra)
  metadata <- data.table::copy(data.table::as.data.table(x$metadata))
  if(is.null(spectrum_ids) || anyNA(spectrum_ids) || anyDuplicated(spectrum_ids) ||
     !"col_id" %in% names(metadata)) {
    stop("Uploaded spectra and metadata require unique identifiers.",
         call. = FALSE)
  }

  metadata_ids <- as.character(metadata$col_id)
  spectrum_index <- match(metadata_ids, spectrum_ids)
  if(nrow(metadata) != length(spectrum_ids) || anyNA(metadata_ids) ||
     anyDuplicated(metadata_ids) || anyNA(spectrum_index) ||
     length(signal_to_noise) != length(spectrum_ids)) {
    stop("Uploaded metadata does not align with the processed spectra.",
         call. = FALSE)
  }

  metadata$signal_to_noise <- signal_to_noise[spectrum_index]
  metadata <- metadata[
    , !vapply(metadata, OpenSpecy::is_empty_vector, logical(1)), with = FALSE
  ]
  metadata$.openspecy_index <- as.integer(spectrum_index)
  metadata$.openspecy_coord_key <- if(all(c("x", "y") %in% names(metadata))) {
    paste(metadata$x, metadata$y)
  } else {
    rep.int(NA_character_, nrow(metadata))
  }
  data.table::setcolorder(
    metadata,
    c(".openspecy_index", ".openspecy_coord_key",
      setdiff(names(metadata), c(".openspecy_index", ".openspecy_coord_key")))
  )
  metadata
}

app_uploaded_metadata_display <- function(metadata) {
  metadata[
    , !names(metadata) %in% c(".openspecy_index", ".openspecy_coord_key"),
    with = FALSE
  ]
}

app_uploaded_metadata_spectrum <- function(metadata, rows_selected) {
  row <- suppressWarnings(as.integer(rows_selected))
  row <- row[
    !is.na(row) & row >= 1L & row <= nrow(metadata)
  ]
  if(length(row) != 1L) return(integer())
  as.integer(metadata$.openspecy_index[[row]])
}

app_uploaded_metadata_row <- function(metadata, spectrum_index) {
  spectrum_index <- suppressWarnings(as.integer(spectrum_index))
  if(length(spectrum_index) != 1L || is.na(spectrum_index)) return(integer())
  row <- match(spectrum_index, metadata$.openspecy_index)
  if(is.na(row)) integer() else as.integer(row)
}

app_uploaded_metadata_table <- function(metadata) {
  DT::datatable(
    app_uploaded_metadata_display(metadata),
    escape = TRUE,
    options = list(
      searchHighlight = TRUE,
      scrollX = TRUE,
      sDom = '<"top">lrt<"bottom">ip',
      lengthChange = FALSE,
      pageLength = 5
    ),
    rownames = FALSE,
    filter = "top",
    caption = "Uploaded Metadata",
    style = "bootstrap",
    selection = "single"
  )
}

app_selected_metadata <- function(x, selected_match, signal_to_noise) {
  metadata <- app_uploaded_metadata_display(
    app_uploaded_metadata_cache(x, signal_to_noise)
  )
  selected_match <- data.table::copy(
    data.table::as.data.table(selected_match)
  )
  if("material_class" %in% names(metadata)) {
    metadata[, material_class := NULL]
  }

  result <- metadata[selected_match, on = c(col_id = "object_id")]
  result <- result[
    , !vapply(result, OpenSpecy::is_empty_vector, logical(1)), with = FALSE
  ] %>%
    dplyr::select(
      dplyr::any_of(c(
        "file_name", "col_id", "material_class", "spectrum_identity",
        "match_val", "signal_to_noise"
      )),
      dplyr::everything()
    )
  result
}

app_top_match_rows <- function(cor_matrix, top_n = 1L) {
  if(!is.matrix(cor_matrix) || !is.numeric(cor_matrix) ||
     !nrow(cor_matrix) || !ncol(cor_matrix)) {
    stop("Top Matches requires a nonempty numeric correlation matrix.",
         call. = FALSE)
  }
  if(is.null(rownames(cor_matrix)) || is.null(colnames(cor_matrix))) {
    stop("The correlation matrix must name references and uploaded spectra.",
         call. = FALSE)
  }
  if(anyDuplicated(rownames(cor_matrix)) ||
     anyDuplicated(colnames(cor_matrix))) {
    stop("Correlation matrix identifiers must be unique.", call. = FALSE)
  }
  top_n <- suppressWarnings(as.integer(top_n))
  if(length(top_n) != 1L || is.na(top_n) || top_n < 1L) top_n <- 1L
  top_n <- min(top_n, nrow(cor_matrix))

  # Rank each uploaded spectrum while the data are still a compact matrix.
  # Expanding every score before keeping Top N can require gigabytes for maps.
  indices <- matrix(NA_integer_, nrow = top_n, ncol = ncol(cor_matrix))
  for(column in seq_len(ncol(cor_matrix))) {
    indices[, column] <- utils::head(
      order(cor_matrix[, column], decreasing = TRUE, na.last = TRUE),
      top_n
    )
  }
  reference_index <- as.vector(indices)
  spectrum_index <- rep(seq_len(ncol(cor_matrix)), each = top_n)

  data.table::data.table(
    Var1 = rownames(cor_matrix)[reference_index],
    Var2 = colnames(cor_matrix)[spectrum_index],
    value = cor_matrix[cbind(reference_index, spectrum_index)]
  )
}

.app_metadata_probe <- function(metadata, key, sentinel) {
  values <- lapply(names(metadata), function(name) {
    if(identical(name, key)) return(sentinel)
    if(OpenSpecy::is_empty_vector(metadata[[name]])) NA else TRUE
  })
  names(values) <- names(metadata)
  data.table::as.data.table(values)
}

.app_top_matches_keep_names <- function(
    library_metadata, spectrum_metadata, quant_columns) {
  library_for_join <- library_metadata %>%
    dplyr::select(-dplyr::any_of(c("col_id", "file_name")))
  library_probe <- .app_metadata_probe(
    library_for_join, "sample_name", "__openspecy_probe__"
  )
  spectrum_probe <- .app_metadata_probe(
    spectrum_metadata, "col_id", "__openspecy_probe__"
  )
  spectrum_details_probe <- data.table::data.table(
    match_threshold = TRUE,
    signal_to_noise = TRUE,
    signal_threshold = TRUE,
    good_signal = TRUE
  ) %>%
    dplyr::bind_cols(spectrum_probe)

  probe <- data.table::data.table(
    Var1 = "__openspecy_probe__",
    Var2 = "__openspecy_probe__",
    value = TRUE
  ) %>%
    dplyr::left_join(
      library_probe, by = c("Var1" = "sample_name")
    ) %>%
    dplyr::left_join(
      spectrum_details_probe, by = c("Var2" = "col_id")
    ) %>%
    dplyr::rename(
      "sample_name" = "Var1",
      "col_id" = "Var2",
      "match_val" = "value"
    ) %>%
    dplyr::mutate(good_match_vals = TRUE, good_matches = TRUE)

  names(probe)[
    !vapply(probe, OpenSpecy::is_empty_vector, logical(1)) |
      names(probe) %in% quant_columns
  ]
}

app_top_matches_export <- function(
    cor_matrix, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold, signal_threshold, top_n = 1L,
    columns_selected = c("Simple", "All"), quant_columns = character()) {
  columns_selected <- match.arg(columns_selected)
  library_metadata <- data.table::as.data.table(library_metadata)
  spectrum_metadata <- data.table::as.data.table(spectrum_metadata)
  required_library <- c("sample_name", "material_class")
  required_spectrum <- c("file_name", "col_id")
  if(!all(required_library %in% names(library_metadata))) {
    stop("Reference metadata is missing Top Matches identifiers.",
         call. = FALSE)
  }
  if(!all(required_spectrum %in% names(spectrum_metadata))) {
    stop("Uploaded metadata is missing Top Matches identifiers.",
         call. = FALSE)
  }
  library_ids <- as.character(library_metadata$sample_name)
  spectrum_ids <- as.character(spectrum_metadata$col_id)
  if(anyDuplicated(library_ids) || anyDuplicated(spectrum_ids)) {
    stop("Top Matches identifiers must be unique.", call. = FALSE)
  }
  library_order <- match(rownames(cor_matrix), library_ids)
  spectrum_order <- match(colnames(cor_matrix), spectrum_ids)
  if(anyNA(library_order) || anyNA(spectrum_order)) {
    stop("Top Matches metadata does not align with the correlation matrix.",
         call. = FALSE)
  }
  library_metadata <- library_metadata[library_order]
  spectrum_metadata <- spectrum_metadata[spectrum_order]
  if(length(signal_to_noise) != ncol(cor_matrix)) {
    stop("Signal-to-noise values do not align with uploaded spectra.",
         call. = FALSE)
  }
  signal_to_noise <- signal_to_noise[spectrum_order]
  top_n <- suppressWarnings(as.integer(top_n))
  if(length(top_n) != 1L || is.na(top_n) || top_n < 1L) top_n <- 1L
  top_n <- min(top_n, nrow(cor_matrix))

  keep_names <- .app_top_matches_keep_names(
    library_metadata, spectrum_metadata, quant_columns
  )

  spectrum_details <- data.table::data.table(
    match_threshold = match_threshold,
    signal_to_noise = signal_to_noise,
    signal_threshold = signal_threshold,
    good_signal = signal_to_noise > signal_threshold
  ) %>%
    dplyr::bind_cols(spectrum_metadata)

  app_top_match_rows(cor_matrix, top_n) %>%
    dplyr::left_join(
      library_metadata %>%
        dplyr::select(-dplyr::any_of(c("col_id", "file_name"))),
      by = c("Var1" = "sample_name")
    ) %>%
    dplyr::left_join(spectrum_details, by = c("Var2" = "col_id")) %>%
    dplyr::rename(
      "sample_name" = "Var1",
      "col_id" = "Var2",
      "match_val" = "value"
    ) %>%
    dplyr::mutate(
      good_match_vals = match_val > match_threshold,
      good_matches = match_val > match_threshold &
        signal_to_noise > signal_threshold
    ) %>%
    {.[, names(.) %in% keep_names, with = FALSE]} %>%
    dplyr::select(file_name, col_id, material_class, spectrum_identity,
                  match_val, signal_to_noise, dplyr::everything()) %>%
    .[order(-match_val), utils::head(.SD, top_n), by = col_id] %>%
    {if(identical(columns_selected, "Simple")) {
      dplyr::select(., dplyr::any_of(c(
        "file_name", "col_id", "material_class", "match_val",
        "signal_to_noise", quant_columns
      )))
    } else .} %>%
    dplyr::mutate(
      material_class = ifelse(match_val < match_threshold, "unknown",
                              material_class)
    ) %>%
    data.table::as.data.table()
}

app_empty_ratio_definitions <- function() {
  data.frame(
    id = integer(),
    name = character(),
    column = character(),
    type = character(),
    numerator_min = numeric(),
    numerator_max = numeric(),
    denominator_min = numeric(),
    denominator_max = numeric(),
    stringsAsFactors = FALSE
  )
}

app_empty_measurement_definitions <- function() {
  data.frame(
    id = integer(),
    name = character(),
    column = character(),
    type = character(),
    minimum = numeric(),
    maximum = numeric(),
    stringsAsFactors = FALSE
  )
}

# Input IDs are kept as the exported column names so the settings snapshot is
# readable beside the app source without promising a future import contract.
app_user_metadata_input_ids <- c(
  # Preprocessing
  "active_preprocessing", "spike_decision", "spike_direction",
  "spike_residual_threshold", "spike_residual_window",
  "saturation_decision", "saturation_mode", "saturation_ceiling",
  "saturation_max_loss", "make_rel_decision", "smooth_decision",
  "smoother", "derivative_order", "smoother_window", "derivative_abs",
  "conform_decision", "conform_selection", "conform_res",
  "intensity_decision", "intensity_corr", "baseline_decision",
  "baseline_method", "baseline", "refit", "baseline_lambda",
  "baseline_hwi", "iterations", "range_decision", "range_automate",
  "range_artifact_ratio", "MinRange", "MaxRange", "co2_decision",
  "co2_automate", "co2_artifact_ratio", "MinFlat", "MaxFlat",
  # Identification
  "active_identification", "id_spec_type", "id_strategy", "lib_type",
  "filter_lib", "lib_org",
  # Advanced
  "active_advanced", "threshold_decision", "MinSNR", "MaxSNR",
  "signal_selection",
  "cor_threshold_decision", "MinCor", "spatial_decision", "sigma",
  "xy_grid", "collapse_decision", "collapse_type", "particle_id_strategy",
  "particle_area_threshold",
  # Quantification builder
  "active_quantification", "quant_ratio_name", "quant_ratio_type",
  "quant_numerator_area_min", "quant_numerator_area_max",
  "quant_denominator_area_min", "quant_denominator_area_max",
  "quant_numerator_peak", "quant_denominator_peak",
  "quant_measurement_name", "quant_measurement_type",
  "quant_measurement_area_min", "quant_measurement_area_max",
  "quant_measurement_wavenumber"
)

app_saturation_value <- function(mode = "auto", ceiling = NULL) {
  mode <- match.arg(mode, c("auto", "threshold"))
  if(identical(mode, "auto")) return("auto")
  if(!is.numeric(ceiling) || length(ceiling) != 1L ||
     !is.finite(ceiling)) {
    stop("Enter one finite detector ceiling for threshold saturation mode.",
         call. = FALSE)
  }
  as.numeric(ceiling)
}

app_apply_spectral_corrections <- function(
    x,
    spike = TRUE,
    spike_args = list(),
    saturation = "auto",
    saturation_args = list()) {
  if(!inherits(x, "OpenSpecy")) {
    stop("'x' must be an OpenSpecy object", call. = FALSE)
  }
  if(!is.list(spike_args) || !is.list(saturation_args)) {
    stop("Correction arguments must be supplied as lists.", call. = FALSE)
  }

  current <- x
  if(isTRUE(spike)) {
    current <- do.call(correct_spike, c(list(x = current), spike_args))
  }
  if(!is.null(saturation)) {
    current <- withCallingHandlers(
      do.call(
        restrict_range,
        c(list(x = current, saturation = saturation, make_rel = FALSE),
          saturation_args)
      ),
      warning = function(warning) invokeRestart("muffleWarning")
    )
  }
  attr(current, "app_automatic_correction_state") <- c(
    spike = isTRUE(spike), saturation = !is.null(saturation)
  )
  current
}

app_copy_correction_history <- function(from, to) {
  for(name in c(
      "automatic_spike", "saturation_restriction",
      "app_automatic_correction_state")) {
    value <- attr(from, name, exact = TRUE)
    if(!is.null(value)) attr(to, name) <- value
  }
  to
}

app_conform_axis <- function(x, resolution) {
  target <- conform_res(x$wavenumber, res = resolution)
  diagnostic <- attr(x, "saturation_restriction", exact = TRUE)
  excluded <- if(is.list(diagnostic) && isTRUE(diagnostic$applied)) {
    diagnostic$excluded_ranges
  } else {
    NULL
  }
  if(!is.null(excluded) && nrow(excluded)) {
    keep <- rep(TRUE, length(target))
    for(i in seq_len(nrow(excluded))) {
      keep <- keep & !(target >= excluded$region_min[[i]] &
                         target <= excluded$region_max[[i]])
    }
    target <- target[keep]
  }
  if(length(target) < 3L) {
    stop("Correction and conformation left fewer than three wavenumbers.",
         call. = FALSE)
  }
  target
}

app_attach_correction_metadata <- function(x) {
  x <- as_OpenSpecy(x)
  x$metadata <- data.table::copy(x$metadata)
  spike <- attr(x, "automatic_spike", exact = TRUE)
  if(is.list(spike)) {
    x$metadata$spike_correction_applied <- isTRUE(spike$applied)
    x$metadata$spike_correction_reason <- as.character(spike$reason)
    x$metadata$spike_corrected_region_count <-
      nrow(spike$corrected_regions)
  }
  saturation <- attr(x, "saturation_restriction", exact = TRUE)
  if(is.list(saturation)) {
    format_ranges <- function(ranges) {
      if(is.null(ranges) || !nrow(ranges)) return(NA_character_)
      paste0(
        format(ranges$region_min, trim = TRUE), "-",
        format(ranges$region_max, trim = TRUE),
        collapse = " | "
      )
    }
    x$metadata$saturation_restriction_applied <-
      isTRUE(saturation$applied)
    x$metadata$saturation_restriction_reason <-
      as.character(saturation$reason)
    x$metadata$saturation_loss_fraction <-
      as.numeric(saturation$saturation_loss_fraction)
    x$metadata$saturation_proposed_loss_fraction <-
      as.numeric(saturation$proposed_saturation_loss_fraction)
    x$metadata$saturation_excluded_ranges <-
      format_ranges(saturation$excluded_ranges)
    x$metadata$saturation_proposed_excluded_ranges <-
      format_ranges(saturation$proposed_excluded_ranges)
    x$metadata$saturation_detected_spectra <- paste(
      saturation$detected_spectra, collapse = " | "
    )
  }
  x
}

app_metadata_scalar <- function(value, separator = " | ") {
  if(is.null(value) || !length(value)) return(NA_character_)
  if(inherits(value, "POSIXt")) {
    value <- format(value, "%Y-%m-%d %H:%M:%S %z")
  }
  if(is.factor(value)) value <- as.character(value)
  if(is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if(!length(value)) return(NA_character_)
  if(length(value) == 1L && is.atomic(value)) return(value[[1L]])
  paste(as.character(value), collapse = separator)
}

app_saved_ratio_definitions <- function(definitions) {
  if(is.null(definitions) || !is.data.frame(definitions) ||
     !nrow(definitions)) {
    return(NA_character_)
  }
  required <- names(app_empty_ratio_definitions())
  if(!all(required %in% names(definitions))) {
    stop("Saved ratio definitions have an unexpected structure.",
         call. = FALSE)
  }
  paste(vapply(seq_len(nrow(definitions)), function(i) {
    definition <- definitions[i, required, drop = FALSE]
    paste(
      paste0("id=", definition$id[[1L]]),
      paste0("name=", definition$name[[1L]]),
      paste0("column=", definition$column[[1L]]),
      paste0("type=", definition$type[[1L]]),
      paste0("numerator_min=", definition$numerator_min[[1L]]),
      paste0("numerator_max=", definition$numerator_max[[1L]]),
      paste0("denominator_min=", definition$denominator_min[[1L]]),
      paste0("denominator_max=", definition$denominator_max[[1L]]),
      sep = "; "
    )
  }, character(1)), collapse = " || ")
}

app_saved_measurement_definitions <- function(definitions) {
  if(is.null(definitions) || !is.data.frame(definitions) ||
     !nrow(definitions)) {
    return(NA_character_)
  }
  required <- names(app_empty_measurement_definitions())
  if(!all(required %in% names(definitions))) {
    stop("Saved measurement definitions have an unexpected structure.",
         call. = FALSE)
  }
  paste(vapply(seq_len(nrow(definitions)), function(i) {
    definition <- definitions[i, required, drop = FALSE]
    paste(
      paste0("id=", definition$id[[1L]]),
      paste0("name=", definition$name[[1L]]),
      paste0("column=", definition$column[[1L]]),
      paste0("type=", definition$type[[1L]]),
      paste0("minimum=", definition$minimum[[1L]]),
      paste0("maximum=", definition$maximum[[1L]]),
      sep = "; "
    )
  }, character(1)), collapse = " || ")
}

app_user_metadata_snapshot <- function(settings, definitions, recorded_at,
                                       app_version, session_id,
                                       source = NULL, file_info = NULL,
                                       measurements =
                                         app_empty_measurement_definitions()) {
  if(!is.list(settings)) {
    stop("App settings must be supplied as a named list.", call. = FALSE)
  }

  uploaded <- !is.null(source)
  spectra_count <- if(uploaded) ncol(source$spectra) else NA_integer_
  wavenumber_count <- if(uploaded) length(source$wavenumber) else NA_integer_
  wavenumber_min <- if(uploaded && wavenumber_count) {
    min(source$wavenumber, na.rm = TRUE)
  } else NA_real_
  wavenumber_max <- if(uploaded && wavenumber_count) {
    max(source$wavenumber, na.rm = TRUE)
  } else NA_real_
  data_digest <- if(uploaded) {
    digest::digest(source, algo = "md5")
  } else NA_character_

  file_value <- function(name) {
    if(is.null(file_info) || !is.data.frame(file_info) ||
       !name %in% names(file_info)) return(NA_character_)
    app_metadata_scalar(file_info[[name]])
  }
  settings <- stats::setNames(lapply(app_user_metadata_input_ids, function(id) {
    app_metadata_scalar(settings[[id]])
  }), app_user_metadata_input_ids)

  snapshot <- c(
    list(
      recorded_at = app_metadata_scalar(recorded_at),
      app_version = app_metadata_scalar(app_version),
      session_id = app_metadata_scalar(session_id),
      data_uploaded = uploaded,
      data_file_name = file_value("name"),
      data_file_size_bytes = file_value("size"),
      data_file_type = file_value("type"),
      data_file_last_modified = file_value("lastModified"),
      data_digest_md5 = data_digest,
      data_spectrum_count = spectra_count,
      data_wavenumber_count = wavenumber_count,
      data_wavenumber_min = wavenumber_min,
      data_wavenumber_max = wavenumber_max
    ),
    settings,
    list(
      quant_saved_ratio_count = if(is.data.frame(definitions)) {
        nrow(definitions)
      } else 0L,
      quant_saved_ratio_definitions = app_saved_ratio_definitions(definitions),
      quant_saved_measurement_count = if(is.data.frame(measurements)) {
        nrow(measurements)
      } else 0L,
      quant_saved_measurement_definitions =
        app_saved_measurement_definitions(measurements)
    )
  )

  snapshot <- lapply(snapshot, app_metadata_scalar)
  if(any(lengths(snapshot) != 1L)) {
    stop("Every user metadata field must contain exactly one value.",
         call. = FALSE)
  }
  snapshot
}

app_quantification_source_value <- "displayed_processed_spectra"

app_ratio_column_name <- function(name, type) {
  if(!is.character(name) || length(name) != 1L || is.na(name) ||
     !nzchar(trimws(name))) {
    stop("Enter a nonempty ratio name before adding it.", call. = FALSE)
  }
  type <- match.arg(type, c("area", "peak"))
  plain <- iconv(trimws(name), to = "ASCII//TRANSLIT", sub = "")
  slug <- tolower(gsub("[^A-Za-z0-9]+", "_", plain))
  slug <- gsub("^_+|_+$", "", slug)
  if(is.na(slug) || !nzchar(slug)) {
    stop("The ratio name must contain at least one letter or number.",
         call. = FALSE)
  }
  paste0(type, "_ratio_", slug)
}

app_add_ratio_definition <- function(definitions, name, type, numerator,
                                     denominator, axis) {
  expected <- names(app_empty_ratio_definitions())
  if(!is.data.frame(definitions) || !identical(names(definitions), expected)) {
    stop("Ratio definitions have an unexpected structure.", call. = FALSE)
  }
  type <- match.arg(type, c("area", "peak"))
  name <- trimws(name)
  column <- app_ratio_column_name(name, type)
  if(column %in% definitions$column) {
    stop("A ratio with the same metadata name has already been added.",
         call. = FALSE)
  }

  axis <- sort(unique(as.numeric(axis)))
  axis <- axis[is.finite(axis)]
  if(!length(axis)) {
    stop("Upload and process a valid spectrum before adding ratios.",
         call. = FALSE)
  }
  normalize_selection <- function(value, expected_length, label) {
    if(!is.numeric(value) || length(value) != expected_length ||
       any(!is.finite(value))) {
      stop(label, " must contain ", expected_length,
           " finite wavenumber value", if(expected_length == 1L) "." else "s.",
           call. = FALSE)
    }
    sort(as.numeric(value))
  }

  if(identical(type, "area")) {
    numerator <- normalize_selection(numerator, 2L, "Numerator range")
    denominator <- normalize_selection(denominator, 2L, "Denominator range")
  } else {
    numerator <- rep(normalize_selection(numerator, 1L, "Numerator point"), 2L)
    denominator <- rep(normalize_selection(
      denominator, 1L, "Denominator point"
    ), 2L)
  }

  values <- c(numerator, denominator)
  if(any(values < axis[[1L]] | values > axis[[length(axis)]])) {
    stop(
      "Ratio selections must stay within the displayed processed wavenumber range.",
      call. = FALSE
    )
  }
  if(identical(type, "area") &&
     (!any(axis >= numerator[[1L]] & axis <= numerator[[2L]]) ||
      !any(axis >= denominator[[1L]] & axis <= denominator[[2L]]))) {
    stop(
      "Each area range must contain at least one displayed processed wavenumber.",
      call. = FALSE
    )
  }

  next_id <- if(nrow(definitions)) max(definitions$id) + 1L else 1L
  rbind(
    definitions,
    data.frame(
      id = next_id,
      name = name,
      column = column,
      type = type,
      numerator_min = numerator[[1L]],
      numerator_max = numerator[[2L]],
      denominator_min = denominator[[1L]],
      denominator_max = denominator[[2L]],
      stringsAsFactors = FALSE
    )
  )
}

app_ratio_definition_label <- function(definition) {
  if(identical(definition$type[[1L]], "area")) {
    paste0(
      definition$name[[1L]], " (area: ",
      format(definition$numerator_min[[1L]]), "-",
      format(definition$numerator_max[[1L]]), " / ",
      format(definition$denominator_min[[1L]]), "-",
      format(definition$denominator_max[[1L]]), " cm^-1)"
    )
  } else {
    paste0(
      definition$name[[1L]], " (peak: ",
      format(definition$numerator_min[[1L]]), " / ",
      format(definition$denominator_min[[1L]]), " cm^-1)"
    )
  }
}

app_quantification_defaults <- function(axis, type = c("area", "peak")) {
  type <- match.arg(type)
  axis <- sort(unique(as.numeric(axis)))
  axis <- axis[is.finite(axis)]
  if(length(axis) < 2L) {
    stop("Process a spectrum with at least two distinct wavenumbers.",
         call. = FALSE)
  }

  axis_min <- min(axis)
  axis_max <- max(axis)
  if(axis_min >= axis_max) {
    stop("The processed wavenumber range must contain at least two values.",
         call. = FALSE)
  }
  clamp_value <- function(value) {
    as.numeric(pmax(axis_min, pmin(axis_max, value)))
  }
  closest_value <- function(value) {
    as.numeric(axis[[which.min(abs(axis - value))]])
  }
  # Numeric inputs permit exact typed values. Points are resolved to measured
  # data by point_intensity() or peak_ratio() when quantification runs.
  step <- min(diff(axis)) / 10

  if(identical(type, "area")) {
    scenario <- c(1650, 1850, 1420, 1500)
    if(all(scenario >= axis_min & scenario <= axis_max)) {
      numerator <- sort(vapply(scenario[1:2], closest_value, numeric(1)))
      denominator <- sort(vapply(
        scenario[3:4], closest_value, numeric(1)
      ))
    } else {
      selections <- axis[pmax(
        1L,
        pmin(length(axis), round(c(.60, .78, .24, .42) * length(axis)))
      )]
      numerator <- sort(clamp_value(selections[1:2]))
      denominator <- sort(clamp_value(selections[3:4]))
    }
  } else {
    scenario <- c(1715, 1460)
    if(all(scenario >= axis_min & scenario <= axis_max)) {
      numerator <- closest_value(scenario[[1L]])
      denominator <- closest_value(scenario[[2L]])
    } else {
      numerator <- clamp_value(
        axis[[max(1L, round(.67 * length(axis)))]]
      )
      denominator <- clamp_value(
        axis[[max(1L, round(.33 * length(axis)))]]
      )
    }
  }

  list(
    min = axis_min,
    max = axis_max,
    step = step,
    numerator = numerator,
    denominator = denominator
  )
}

# Retained as an internal compatibility alias for saved app tests and sessions;
# the UI now uses numeric inputs, not sliders.
app_ratio_slider_defaults <- app_quantification_defaults

app_measurement_column_name <- function(name, type) {
  if(!is.character(name) || length(name) != 1L || is.na(name) ||
     !nzchar(trimws(name))) {
    stop("Enter a nonempty measurement name before adding it.", call. = FALSE)
  }
  type <- match.arg(type, c("area", "point"))
  plain <- iconv(trimws(name), to = "ASCII//TRANSLIT", sub = "")
  slug <- tolower(gsub("[^A-Za-z0-9]+", "_", plain))
  slug <- gsub("^_+|_+$", "", slug)
  if(is.na(slug) || !nzchar(slug)) {
    stop("The measurement name must contain at least one letter or number.",
         call. = FALSE)
  }
  paste0(if(identical(type, "area")) {
    "area_under_band_"
  } else {
    "point_intensity_"
  }, slug)
}

app_add_measurement_definition <- function(definitions, name, type, values,
                                           axis) {
  expected <- names(app_empty_measurement_definitions())
  if(!is.data.frame(definitions) || !identical(names(definitions), expected)) {
    stop("Measurement definitions have an unexpected structure.",
         call. = FALSE)
  }
  type <- match.arg(type, c("area", "point"))
  name <- trimws(name)
  column <- app_measurement_column_name(name, type)
  if(column %in% definitions$column) {
    stop("A measurement with the same metadata name has already been added.",
         call. = FALSE)
  }

  axis <- sort(unique(as.numeric(axis)))
  axis <- axis[is.finite(axis)]
  if(!length(axis)) {
    stop("Upload and process a valid spectrum before adding measurements.",
         call. = FALSE)
  }
  expected_length <- if(identical(type, "area")) 2L else 1L
  if(!is.numeric(values) || length(values) != expected_length ||
     any(!is.finite(values))) {
    stop(
      if(identical(type, "area")) {
        "Measurement area must contain two finite wavenumber values."
      } else {
        "Measurement point must contain one finite wavenumber value."
      },
      call. = FALSE
    )
  }
  values <- sort(as.numeric(values))
  if(identical(type, "point")) values <- rep(values, 2L)
  if(any(values < axis[[1L]] | values > axis[[length(axis)]])) {
    stop(
      "Measurement selections must stay within the displayed processed wavenumber range.",
      call. = FALSE
    )
  }
  if(identical(type, "area") &&
     !any(axis >= values[[1L]] & axis <= values[[2L]])) {
    stop(
      "The measurement area must contain at least one displayed processed wavenumber.",
      call. = FALSE
    )
  }

  next_id <- if(nrow(definitions)) max(definitions$id) + 1L else 1L
  rbind(
    definitions,
    data.frame(
      id = next_id,
      name = name,
      column = column,
      type = type,
      minimum = values[[1L]],
      maximum = values[[2L]],
      stringsAsFactors = FALSE
    )
  )
}

app_measurement_definition_label <- function(definition) {
  if(identical(definition$type[[1L]], "area")) {
    paste0(
      definition$name[[1L]], " (area: ",
      format(definition$minimum[[1L]]), "-",
      format(definition$maximum[[1L]]), " cm^-1)"
    )
  } else {
    paste0(
      definition$name[[1L]], " (intensity: ",
      format(definition$minimum[[1L]]), " cm^-1)"
    )
  }
}

app_ratio_definitions_text <- function(definitions) {
  if(!nrow(definitions)) return(character())
  paste(
    vapply(seq_len(nrow(definitions)), function(i) {
      app_ratio_definition_label(definitions[i, , drop = FALSE])
    }, character(1)),
    collapse = "; "
  )
}

app_measurement_definitions_text <- function(definitions) {
  if(!nrow(definitions)) return(character())
  paste(
    vapply(seq_len(nrow(definitions)), function(i) {
      app_measurement_definition_label(definitions[i, , drop = FALSE])
    }, character(1)),
    collapse = "; "
  )
}

app_quantification_definitions_text <- function(ratios, measurements) {
  parts <- c(
    if(nrow(ratios)) paste0("Ratios: ", app_ratio_definitions_text(ratios)),
    if(nrow(measurements)) paste0(
      "Measurements: ", app_measurement_definitions_text(measurements)
    )
  )
  paste(parts, collapse = "; ")
}

app_ratio_metadata_columns <- function(
    definitions,
    measurements = app_empty_measurement_definitions()) {
  if(!nrow(definitions) && !nrow(measurements)) return(character())
  c("quantification_source", "quantification_definitions",
    definitions$column, measurements$column)
}

app_area_ratio <- function(source, numerator, denominator) {
  source <- as_OpenSpecy(source)
  axis <- source$wavenumber
  named_na <- stats::setNames(
    rep(NA_real_, ncol(source$spectra)), colnames(source$spectra)
  )
  complete <- all(c(numerator, denominator) >= min(axis) &
                    c(numerator, denominator) <= max(axis)) &&
    any(axis >= numerator[[1L]] & axis <= numerator[[2L]]) &&
    any(axis >= denominator[[1L]] & axis <= denominator[[2L]])
  if(!complete) {
    warning("The source spectrum does not fully cover this area ratio; returning NA.",
            call. = FALSE)
    return(named_na)
  }
  numerator_values <- area_under_band(
    source, min = numerator[[1L]], max = numerator[[2L]]
  )
  denominator_values <- area_under_band(
    source, min = denominator[[1L]], max = denominator[[2L]]
  )
  values <- numerator_values / denominator_values
  invalid <- !is.finite(numerator_values) | !is.finite(denominator_values) |
    denominator_values == 0 | !is.finite(values)
  if(any(invalid)) {
    warning("One or more area ratios had a zero or non-finite value; returning NA for those spectra.",
            call. = FALSE)
    values[invalid] <- NA_real_
  }
  values
}

app_area_measurement <- function(source, bounds) {
  source <- as_OpenSpecy(source)
  axis <- source$wavenumber
  named_na <- stats::setNames(
    rep(NA_real_, ncol(source$spectra)), colnames(source$spectra)
  )
  complete <- length(bounds) == 2L && all(is.finite(bounds)) &&
    all(bounds >= min(axis) & bounds <= max(axis)) &&
    any(axis >= bounds[[1L]] & axis <= bounds[[2L]])
  if(!complete) {
    warning(
      "The source spectrum does not fully cover this area measurement; returning NA.",
      call. = FALSE
    )
    return(named_na)
  }
  area_under_band(source, min = bounds[[1L]], max = bounds[[2L]])
}

app_attach_quantification <- function(
    x,
    definitions,
    measurements = app_empty_measurement_definitions()) {
  x <- as_OpenSpecy(x)
  if(!nrow(definitions) && !nrow(measurements)) return(x)

  x$metadata <- data.table::copy(x$metadata)
  x$metadata$quantification_source <- app_quantification_source_value
  x$metadata$quantification_definitions <-
    app_quantification_definitions_text(definitions, measurements)
  for(i in seq_len(nrow(definitions))) {
    definition <- definitions[i, , drop = FALSE]
    values <- if(identical(definition$type[[1L]], "area")) {
      app_area_ratio(
        x,
        c(definition$numerator_min[[1L]], definition$numerator_max[[1L]]),
        c(definition$denominator_min[[1L]], definition$denominator_max[[1L]])
      )
    } else {
      peak_ratio(
        x,
        numerator = definition$numerator_min[[1L]],
        denominator = definition$denominator_min[[1L]],
        method = "nearest"
      )
    }
    if(length(values) != nrow(x$metadata)) {
      stop("Quantification returned an unexpected number of values for '",
           definition$name[[1L]], "'.", call. = FALSE)
    }
    x$metadata[[definition$column[[1L]]]] <- as.numeric(values)
  }
  for(i in seq_len(nrow(measurements))) {
    definition <- measurements[i, , drop = FALSE]
    values <- if(identical(definition$type[[1L]], "area")) {
      app_area_measurement(
        x,
        c(definition$minimum[[1L]], definition$maximum[[1L]])
      )
    } else {
      point_intensity(
        x,
        wavenumber = definition$minimum[[1L]],
        method = "nearest"
      )
    }
    if(length(values) != nrow(x$metadata)) {
      stop("Quantification returned an unexpected number of values for '",
           definition$name[[1L]], "'.", call. = FALSE)
    }
    x$metadata[[definition$column[[1L]]]] <- as.numeric(values)
  }
  x
}

.app_range_assessment <- function(x, check, correction_args = list()) {
  check <- match.arg(check, c("co2_region", "high_tail"))
  value_or <- function(name, default) {
    value <- correction_args[[name]]
    if(is.null(value)) default else value
  }

  artifact_ratio <- value_or("artifact_ratio", 3)
  tail_n <- value_or("tail_n", 5L)
  co2_region <- if(identical(check, "co2_region")) {
    min <- value_or("min", 2200)
    max <- value_or("max", 2400)
    if(length(min) != 1L || length(max) != 1L) {
      stop("automatic CO2 correction requires one flattening range",
           call. = FALSE)
    }
    c(min, max)
  } else {
    value_or("co2_region", c(2200, 2420))
  }

  issues <- assess_spec(
    x,
    checks = check,
    artifact_ratio = artifact_ratio,
    tail_n = tail_n,
    co2_region = co2_region
  )
  failures <- length(unique(issues$spectrum_index))
  total <- ncol(x$spectra)

  list(
    issues = issues,
    failures = failures,
    passes = total - failures,
    total = total
  )
}

.app_range_candidate_preserves_batch <- function(before, candidate) {
  if(!inherits(candidate, "OpenSpecy") ||
     ncol(candidate$spectra) != ncol(before$spectra) ||
     !identical(colnames(candidate$spectra), colnames(before$spectra)) ||
     !identical(candidate$metadata, before$metadata)) {
    return(FALSE)
  }

  original_attributes <- attributes(before)
  protected <- setdiff(names(original_attributes), c("names", "class"))
  all(vapply(protected, function(name) {
    identical(attr(candidate, name), original_attributes[[name]])
  }, logical(1)))
}

.app_range_diagnostic <- function(step, check, enabled, attempted, accepted,
                                  total, before, after, reason,
                                  message = "",
                                  original_range = c(NA_real_, NA_real_),
                                  applied_range = c(NA_real_, NA_real_)) {
  normalize_range <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    if(length(value) < 2L || any(!is.finite(value[1:2]))) {
      return(c(NA_real_, NA_real_))
    }
    range(value[1:2])
  }
  original_range <- normalize_range(original_range)
  applied_range <- normalize_range(applied_range)
  data.frame(
    step = step,
    check = check,
    enabled = enabled,
    attempted = attempted,
    accepted = accepted,
    total_spectra = as.integer(total),
    before_passes = as.integer(before),
    after_passes = as.integer(after),
    reason = reason,
    message = message,
    original_range_min = original_range[[1L]],
    original_range_max = original_range[[2L]],
    applied_range_min = applied_range[[1L]],
    applied_range_max = applied_range[[2L]],
    stringsAsFactors = FALSE
  )
}

.app_attempt_range_automation <- function(x, step, correction_args = list()) {
  check <- if(identical(step, "flatten_range")) "co2_region" else "high_tail"
  before <- .app_range_assessment(x, check, correction_args)
  if(before$failures == 0L) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, FALSE, FALSE, before$total,
        before$passes, before$passes, "no_failures"
      )
    ))
  }

  correction <- if(identical(step, "flatten_range")) {
    flatten_range
  } else {
    restrict_range
  }
  correction_args$x <- NULL
  correction_args$automate <- TRUE
  if(is.null(correction_args$make_rel)) correction_args$make_rel <- FALSE
  candidate <- tryCatch(
    do.call(correction, c(list(x = x), correction_args)),
    error = function(e) e
  )
  if(inherits(candidate, "error")) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "correction_error",
        conditionMessage(candidate)
      )
    ))
  }
  if(!.app_range_candidate_preserves_batch(x, candidate)) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "invalid_candidate",
        "candidate changed spectrum identifiers, metadata, or input attributes"
      )
    ))
  }

  after <- tryCatch(
    .app_range_assessment(candidate, check, correction_args),
    error = function(e) e
  )
  if(inherits(after, "error")) {
    return(list(
      data = x,
      diagnostic = .app_range_diagnostic(
        step, check, TRUE, TRUE, FALSE, before$total,
        before$passes, before$passes, "assessment_error",
        conditionMessage(after)
      )
    ))
  }

  accepted <- after$passes > before$passes
  correction_detail <- attr(
    candidate,
    if(identical(step, "flatten_range")) {
      "automatic_flatten"
    } else {
      "automatic_tail"
    },
    exact = TRUE
  )
  original_range <- if(is.list(correction_detail) &&
                            !is.null(correction_detail$original_range)) {
    correction_detail$original_range
  } else {
    range(x$wavenumber, na.rm = TRUE)
  }
  applied_range <- if(identical(step, "flatten_range")) {
    if(is.list(correction_detail) && !is.null(correction_detail$region)) {
      correction_detail$region
    } else {
      c(correction_args$min, correction_args$max)
    }
  } else if(is.list(correction_detail) &&
            !is.null(correction_detail$corrected_range)) {
    correction_detail$corrected_range
  } else {
    range(candidate$wavenumber, na.rm = TRUE)
  }
  list(
    data = if(accepted) candidate else x,
    diagnostic = .app_range_diagnostic(
      step, check, TRUE, TRUE, accepted, before$total,
      before$passes, after$passes,
      if(accepted) "improved" else "not_improved",
      original_range = original_range,
      applied_range = applied_range
    )
  )
}

app_apply_range_automation <- function(x, flatten = TRUE, restrict = TRUE,
                                       flatten_args = list(),
                                       restrict_args = list()) {
  if(!inherits(x, "OpenSpecy")) {
    stop("'x' must be an OpenSpecy object", call. = FALSE)
  }

  current <- x
  diagnostics <- list()
  steps <- list(
    list(name = "flatten_range", check = "co2_region", enabled = flatten,
         args = flatten_args),
    list(name = "restrict_range", check = "high_tail", enabled = restrict,
         args = restrict_args)
  )
  for(step in steps) {
    if(!isTRUE(step$enabled)) {
      total <- ncol(current$spectra)
      diagnostics[[length(diagnostics) + 1L]] <- .app_range_diagnostic(
        step$name, step$check, FALSE, FALSE, FALSE, total,
        NA_integer_, NA_integer_, "disabled"
      )
      next
    }

    result <- .app_attempt_range_automation(
      current, step$name, correction_args = step$args
    )
    current <- result$data
    diagnostics[[length(diagnostics) + 1L]] <- result$diagnostic
  }

  list(data = current, diagnostics = do.call(rbind, diagnostics))
}

app_theme <- list(
  canvas = "#050B14",
  panel = "#0B1929",
  panel_2 = "#10243A",
  border = "#168FC2",
  accent = "#38BDF8",
  success = "#22C55E",
  text = "#E6EDF7",
  muted = "#A9B8CB",
  grid = "#28536F",
  axis = "#6F86A3",
  raw = "#CBD5E1",
  reference = "#FB7185",
  spectrum = "#FFFFFF"
)

app_theme_css <- function(theme = app_theme) {
  required <- c(
    "canvas", "panel", "panel_2", "border", "accent", "success",
    "text", "muted", "grid", "axis", "raw", "spectrum"
  )
  if(!is.list(theme) || !all(required %in% names(theme))) {
    stop("The app theme is missing one or more required color tokens.",
         call. = FALSE)
  }

  values <- unlist(theme[required], use.names = TRUE)
  css_names <- gsub("_", "-", names(values), fixed = TRUE)
  paste0(
    ":root {\n",
    paste0("  --openspecy-", css_names, ": ", values, ";",
           collapse = "\n"),
    "\n}\n"
  )
}

app_plot_palette <- list(
  panel = app_theme$panel,
  grid = app_theme$grid,
  axis = app_theme$axis,
  text = app_theme$text,
  primary = app_theme$accent,
  raw = app_theme$raw,
  reference = app_theme$reference,
  spectrum = app_theme$spectrum
)

# Okabe-Ito hues, ordered from cool to warm and lifted away from the very dark
# end of common perceptually uniform scales so every value remains visible on
# the application's navy canvas.
app_heatmap_colorscale <- list(
  c(0.00, "#56B4E9"),
  c(0.20, "#44B9A8"),
  c(0.40, "#009E73"),
  c(0.60, "#F0E442"),
  c(0.80, "#E69F00"),
  c(1.00, "#CC79A7")
)

app_heatmap_legend_layout <- function(title = "Value") {
  title <- trimws(as.character(title))
  title <- if(length(title) && !is.na(title[[1L]]) && nzchar(title[[1L]])) {
    title[[1L]]
  } else {
    "Value"
  }
  list(
    colorbar = list(
      title = list(text = title, side = "top"),
      orientation = "h",
      x = 0.5, xanchor = "center",
      y = 1.03, yanchor = "bottom",
      len = 0.72,
      thickness = 14
    ),
    margin = list(t = 104, r = 32, b = 64, l = 72)
  )
}

app_category_colors <- c(
  "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7",
  "#D55E00", "#7FDBFF", "#98D8C8", "#F4A6C1", "#FDD17A"
)

app_category_palette <- function(values) {
  labels <- if(is.factor(values)) {
    levels(values)
  } else {
    sort(unique(as.character(values[!is.na(values)])))
  }
  if(!length(labels)) return(stats::setNames(character(), character()))
  stats::setNames(
    rep(app_category_colors, length.out = length(labels)),
    labels
  )
}

app_category_colorscale <- function(values) {
  palette <- app_category_palette(values)
  count <- length(palette)
  if(!count) return(app_heatmap_colorscale)
  if(count == 1L) {
    return(list(c(0, unname(palette[[1L]])),
                c(1, unname(palette[[1L]]))))
  }
  centers <- seq(0, 1, length.out = count)
  edges <- c(0, (centers[-1L] + centers[-count]) / 2, 1)
  unlist(lapply(seq_len(count), function(i) {
    list(
      c(edges[[i]], unname(palette[[i]])),
      c(edges[[i + 1L]], unname(palette[[i]]))
    )
  }), recursive = FALSE)
}

app_quality_checks <- c(
  "silent_region", "missing_values", "flat_spectrum",
  "negative_intensity"
)

app_automatic_quality_checks <- c(
  "spike", "saturation", "co2_region", "high_tail"
)

app_quality_success_description <- function(row) {
  if(!is.data.frame(row) || nrow(row) != 1L || !"check" %in% names(row)) {
    stop("A success description requires one quality-report row.",
         call. = FALSE)
  }
  check <- as.character(row$check[[1L]])
  switch(
    check,
    silent_region = {
      region <- if(all(c("region_min", "region_max") %in% names(row)) &&
                   is.finite(row$region_min[[1L]]) &&
                   is.finite(row$region_max[[1L]])) {
        paste0(
          " in ", format(row$region_min[[1L]], trim = TRUE), "-",
          format(row$region_max[[1L]], trim = TRUE), " cm^-1"
        )
      } else " in the configured silent region"
      paste0(
        "The maximum intensity", region,
        " stayed at or below the spectrum-wide high-quantile threshold."
      )
    },
    missing_values =
      "No NA, NaN, Inf, or -Inf intensity values were detected.",
    flat_spectrum = paste(
      "The finite intensity range exceeds the configured flat-spectrum",
      "tolerance, so the spectrum is not constant."
    ),
    negative_intensity = paste(
      "The minimum finite intensity stayed at or above the allowed negative",
      "threshold."
    ),
    as.character(row$description[[1L]])
  )
}

app_quality_ui_report <- function(report) {
  if(is.null(report) || !is.data.frame(report) || !nrow(report)) return(report)
  if(!all(c("status", "test_id", "check") %in% names(report))) {
    stop("Quality reports must include 'status', 'test_id', and 'check'.",
         call. = FALSE)
  }
  # assess_spec() returns a data.table. Convert before filtering so helper
  # arguments such as `status` cannot be shadowed by same-named columns in
  # data.table's non-standard evaluation.
  report <- as.data.frame(report, stringsAsFactors = FALSE)
  report <- report[!report$check %in% app_automatic_quality_checks, ,
                   drop = FALSE]
  report$status <- ifelse(
    report$status %in% c("pass", "success"), "success", "warning"
  )
  success_rows <- which(report$status == "success")
  for(i in success_rows) {
    report$description[[i]] <- app_quality_success_description(
      report[i, , drop = FALSE]
    )
  }
  report
}

app_quality_status_report <- function(report, status) {
  target_status <- match.arg(status, c("warning", "success"))
  if(is.null(report) || !is.data.frame(report) || !nrow(report)) {
    return(data.frame())
  }
  report <- app_quality_ui_report(report)
  report[
    report$status == target_status & !duplicated(report$test_id), ,
    drop = FALSE
  ]
}

app_threshold_quality_report <- function(
    spectrum_id,
    snr_value = NULL,
    snr_threshold = NULL,
    signal_metric = "run_sig_over_noise",
    correlation_value = NULL,
    correlation_threshold = NULL) {
  empty <- function() {
    data.frame(
      status = character(), test_id = character(), check = character(),
      description = character(), likely_cause = character(),
      potential_fix = character(), metric = character(), value = numeric(),
      threshold = numeric(), region_min = numeric(), region_max = numeric(),
      stringsAsFactors = FALSE
    )
  }
  if(length(spectrum_id) != 1L || is.na(spectrum_id) ||
     !nzchar(as.character(spectrum_id))) {
    stop("Threshold quality findings require one spectrum ID.",
         call. = FALSE)
  }

  signal_details <- switch(
    match.arg(signal_metric, c(
      "run_sig_over_noise", "sig_times_noise", "log_tot_sig"
    )),
    run_sig_over_noise = list(
      label = "Signal-to-noise ratio", metric = "snr"
    ),
    sig_times_noise = list(
      label = "Signal times noise", metric = "signal_times_noise"
    ),
    log_tot_sig = list(
      label = "Total signal", metric = "total_signal"
    )
  )

  make_row <- function(check, label, metric, observed, threshold,
                       warning_cause, warning_fix) {
    if(is.null(threshold)) return(NULL)
    if(!is.numeric(threshold) || length(threshold) != 1L ||
       !is.finite(threshold)) {
      stop(label, " threshold must be one finite number.", call. = FALSE)
    }
    if(!is.numeric(observed) || length(observed) != 1L) {
      stop(label, " value must be one numeric value.", call. = FALSE)
    }
    observed <- as.numeric(observed)
    threshold <- as.numeric(threshold)
    # Infinite SNR is a valid result when signal is positive and estimated
    # noise is zero. Compare infinities normally; only missing/NaN values are
    # unavailable.
    evaluable <- !is.na(observed)
    passed <- evaluable && observed > threshold
    relation <- if(!evaluable) {
      "could not be evaluated against"
    } else if(passed) {
      "is above"
    } else if(observed < threshold) {
      "is below"
    } else {
      "is equal to and does not exceed"
    }
    observed_text <- if(evaluable) {
      format(signif(observed, 5), trim = TRUE)
    } else {
      "an unavailable value"
    }
    data.frame(
      status = if(passed) "success" else "warning",
      test_id = paste("app_threshold", spectrum_id, check, sep = ":"),
      check = check,
      description = paste(
        label, observed_text, relation, "the configured threshold",
        paste0(format(signif(threshold, 5), trim = TRUE), ".")
      ),
      likely_cause = if(passed) NA_character_ else if(evaluable) {
        warning_cause
      } else {
        paste(label, "was unavailable for the selected spectrum.")
      },
      potential_fix = if(passed) "No action required." else warning_fix,
      metric = metric,
      value = observed,
      threshold = threshold,
      region_min = NA_real_,
      region_max = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  rows <- Filter(Negate(is.null), list(
    make_row(
      "snr_threshold", signal_details$label, signal_details$metric,
      snr_value, snr_threshold,
      "The selected spectrum has less signal separation than requested.",
      paste(
        "Review the SNR threshold and acquisition settings; consider",
        "recollecting the spectrum if the weak signal is unexpected."
      )
    ),
    make_row(
      "correlation_threshold", "Correlation", "correlation",
      correlation_value, correlation_threshold,
      "The selected spectrum's best library match is weaker than requested.",
      paste(
        "Review the correlation threshold, preprocessing, and candidate",
        "matches before interpreting the identification."
      )
    )
  ))
  if(!length(rows)) return(empty())
  do.call(rbind, rows)
}

app_quality_counts <- function(report) {
  statuses <- c("warning", "success")
  if(is.null(report) || !is.data.frame(report) || !nrow(report)) {
    return(stats::setNames(rep.int(0L, length(statuses)), statuses))
  }
  stats::setNames(
    vapply(statuses, function(status) {
      nrow(app_quality_status_report(report, status))
    }, integer(1)),
    statuses
  )
}

app_quality_evidence <- function(row) {
  parts <- character()
  if(length(row$metric) && !is.na(row$metric) && nzchar(row$metric)) {
    parts <- c(parts, paste0("Metric: ", row$metric))
  }
  if(length(row$value) && !is.na(row$value)) {
    parts <- c(parts, paste0("Observed: ", signif(row$value, 5)))
  }
  if(length(row$threshold) && is.finite(row$threshold)) {
    parts <- c(parts, paste0("Threshold: ", signif(row$threshold, 5)))
  }
  if(length(row$candidate_max) && is.finite(row$candidate_max)) {
    candidate_label <- if(identical(row$metric, "saturated_interval_count")) {
      "Detector ceiling"
    } else {
      "Candidate maximum"
    }
    parts <- c(parts, paste0(
      candidate_label, ": ", signif(row$candidate_max, 5)
    ))
  }
  if(length(row$control_max) && is.finite(row$control_max)) {
    parts <- c(parts, paste0(
      "Control maximum: ", signif(row$control_max, 5)
    ))
  }
  if(length(row$region_min) && length(row$region_max) &&
     is.finite(row$region_min) && is.finite(row$region_max)) {
    parts <- c(parts, paste0(
      "Region: ", format(row$region_min, trim = TRUE), "-",
      format(row$region_max, trim = TRUE), " cm^-1"
    ))
  }
  if(!length(parts)) "No numeric exception was recorded." else
    paste(parts, collapse = "; ")
}

app_quality_modal_content <- function(report, status) {
  status <- match.arg(status, c("warning", "success"))
  if(is.null(report)) {
    return(tags$p("Upload a spectrum to run the quality checks."))
  }
  if(!is.data.frame(report)) {
    stop("Quality modal content requires a data frame or NULL.", call. = FALSE)
  }
  rows <- app_quality_status_report(report, status)
  if(!nrow(rows)) {
    return(tags$p(paste0("No ", status, " findings for this spectrum.")))
  }
  tagList(lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    tags$section(
      class = paste("openspecy-quality-finding", paste0(
        "openspecy-quality-finding-", row$status[[1L]]
      )),
      `data-quality-status` = row$status[[1L]],
      `data-quality-test-id` = row$test_id[[1L]],
      tags$h4(gsub("_", " ", row$check[[1L]], fixed = TRUE)),
      tags$p(tags$strong("Finding: "), row$description[[1L]]),
      tags$p(tags$strong("Evidence: "), app_quality_evidence(row)),
      if(identical(status, "warning")) {
        tags$p(
          tags$strong("Interpretation: "),
          ifelse(
            is.na(row$likely_cause[[1L]]),
            "No likely cause was recorded.",
            row$likely_cause[[1L]]
          )
        )
      },
      if(identical(status, "warning")) {
        tags$p(tags$strong("Action: "), row$potential_fix[[1L]])
      }
    )
  }))
}

app_format_wavenumber_ranges <- function(ranges, maximum = 4L) {
  if(is.null(ranges) || !is.data.frame(ranges) || !nrow(ranges) ||
     !all(c("region_min", "region_max") %in% names(ranges))) {
    return("no recorded wavenumber range")
  }
  maximum <- suppressWarnings(as.integer(maximum))
  if(length(maximum) != 1L || is.na(maximum) || maximum < 1L) maximum <- 4L
  ranges <- unique(ranges[, c("region_min", "region_max"), drop = FALSE])
  ranges <- ranges[
    is.finite(ranges$region_min) & is.finite(ranges$region_max), , drop = FALSE
  ]
  if(!nrow(ranges)) return("no recorded wavenumber range")
  labels <- vapply(seq_len(min(nrow(ranges), maximum)), function(i) {
    bounds <- sort(c(ranges$region_min[[i]], ranges$region_max[[i]]))
    formatted <- format(signif(bounds, 7), trim = TRUE)
    if(isTRUE(all.equal(bounds[[1L]], bounds[[2L]]))) {
      paste0(formatted[[1L]], " cm^-1")
    } else {
      paste0(formatted[[1L]], "-", formatted[[2L]], " cm^-1")
    }
  }, character(1))
  if(nrow(ranges) > maximum) {
    labels <- c(labels, paste0("and ", nrow(ranges) - maximum, " more"))
  }
  paste(labels, collapse = ", ")
}

app_automatic_report <- function(
    x = NULL,
    diagnostics = data.frame(),
    enabled = c(spike = FALSE, saturation = FALSE,
                flatten = FALSE, tails = FALSE)) {
  enabled_names <- c("spike", "saturation", "flatten", "tails")
  enabled <- enabled[enabled_names]
  enabled[is.na(enabled)] <- FALSE
  enabled <- stats::setNames(as.logical(enabled), enabled_names)
  recorded_state <- if(is.null(x)) NULL else
    attr(x, "app_automatic_correction_state", exact = TRUE)
  if(is.logical(recorded_state) &&
     all(c("spike", "saturation") %in% names(recorded_state))) {
    enabled[c("spike", "saturation")] <-
      recorded_state[c("spike", "saturation")]
  }

  make_row <- function(step, label, is_enabled, applied, outcome, summary) {
    data.frame(
      step = step,
      label = label,
      enabled = isTRUE(is_enabled),
      applied = isTRUE(applied),
      outcome = outcome,
      summary = summary,
      stringsAsFactors = FALSE
    )
  }
  attr_or_null <- function(name) {
    if(is.null(x)) NULL else attr(x, name, exact = TRUE)
  }
  attr_row <- function(step, label, is_enabled, diagnostic,
                       applied_summary, clean_summary) {
    if(!isTRUE(is_enabled)) {
      return(make_row(step, label, FALSE, FALSE, "disabled",
                      "This automatic correction is disabled."))
    }
    if(is.null(x)) {
      return(make_row(step, label, TRUE, FALSE, "pending",
                      "Upload spectra to run this automatic check."))
    }
    if(is.null(diagnostic)) {
      return(make_row(step, label, TRUE, FALSE, "not_needed", clean_summary))
    }
    if(isTRUE(diagnostic$applied)) {
      return(make_row(step, label, TRUE, TRUE, "applied",
                      applied_summary(diagnostic)))
    }
    rejected <- if(is.data.frame(diagnostic$rejected_regions)) {
      diagnostic$rejected_regions
    } else data.frame()
    rejection_detail <- if(nrow(rejected) && "reason" %in% names(rejected)) {
      paste0(
        "; across correction passes, safeguards left ", nrow(rejected),
        " candidate region",
        if(nrow(rejected) == 1L) "" else "s", " unchanged (",
        paste(unique(gsub("_", " ", rejected$reason, fixed = TRUE)),
              collapse = ", "), ")"
      )
    } else ""
    make_row(
      step, label, TRUE, FALSE, "rejected",
      paste0(
        "A candidate correction was not applied (",
        gsub("_", " ", as.character(diagnostic$reason), fixed = TRUE),
        rejection_detail, ")."
      )
    )
  }

  spike <- attr_or_null("automatic_spike")
  saturation <- attr_or_null("saturation_restriction")
  rows <- list(
    attr_row(
      "spike", "Spike correction", enabled[["spike"]], spike,
      function(value) {
        corrected_count <- nrow(value$corrected_regions)
        rejected <- if(is.data.frame(value$rejected_regions)) {
          value$rejected_regions
        } else data.frame()
        remaining <- if(nrow(rejected)) paste0(
          " Across correction passes, safeguards left ", nrow(rejected),
          " candidate region",
          if(nrow(rejected) == 1L) "" else "s", " unchanged (",
          paste(unique(gsub("_", " ", rejected$reason, fixed = TRUE)),
                collapse = ", "), ")."
        ) else ""
        paste0(
          "Corrected ", corrected_count, " spike region",
          if(corrected_count == 1L) "" else "s", " across ",
          length(value$affected_spectra), " spectrum",
          if(length(value$affected_spectra) == 1L) "" else "s", " at ",
          app_format_wavenumber_ranges(value$corrected_regions), ".",
          remaining
        )
      },
      "No correctable spike regions were detected."
    ),
    attr_row(
      "saturation", "Saturation restriction", enabled[["saturation"]],
      saturation,
      function(value) paste0(
        "Removed ", value$excluded_interval_count, " shared saturated range",
        if(value$excluded_interval_count == 1L) "" else "s", " at ",
        app_format_wavenumber_ranges(value$excluded_ranges), " (",
        signif(100 * value$saturation_loss_fraction, 3),
        "% of the wavenumber span)."
      ),
      "No shared saturated ranges were detected."
    )
  )

  range_row <- function(step, check, label, is_enabled) {
    if(!isTRUE(is_enabled)) {
      return(make_row(step, label, FALSE, FALSE, "disabled",
                      "This automatic correction is disabled."))
    }
    if(is.null(x)) {
      return(make_row(step, label, TRUE, FALSE, "pending",
                      "Upload spectra to run this automatic check."))
    }
    row <- if(is.data.frame(diagnostics) && nrow(diagnostics)) {
      diagnostics[diagnostics$check == check, , drop = FALSE]
    } else data.frame()
    if(!nrow(row)) {
      return(make_row(step, label, TRUE, FALSE, "not_needed",
                      "No automatic correction was necessary."))
    }
    row <- row[nrow(row), , drop = FALSE]
    total <- row$total_spectra[[1L]]
    before <- total - row$before_passes[[1L]]
    after <- total - row$after_passes[[1L]]
    comparison <- paste0(
      "Problematic spectra: ", before, " of ", total, " before; ",
      after, " of ", total, " after the candidate correction."
    )
    if(isTRUE(row$accepted[[1L]])) {
      numeric_field <- function(name) {
        if(!name %in% names(row)) return(NA_real_)
        suppressWarnings(as.numeric(row[[name]][[1L]]))
      }
      format_range <- function(minimum, maximum) paste0(
        format(signif(minimum, 7), trim = TRUE), "-",
        format(signif(maximum, 7), trim = TRUE), " cm^-1"
      )
      applied_min <- numeric_field("applied_range_min")
      applied_max <- numeric_field("applied_range_max")
      original_min <- numeric_field("original_range_min")
      original_max <- numeric_field("original_range_max")
      range_detail <- if(identical(step, "flatten") &&
                         all(is.finite(c(applied_min, applied_max)))) {
        paste0("Flattened ", format_range(applied_min, applied_max), ".")
      } else if(identical(step, "tails") &&
                all(is.finite(c(
                  original_min, original_max, applied_min, applied_max
                )))) {
        paste0(
          "Restricted the shared wavenumber axis from ",
          format_range(original_min, original_max), " to ",
          format_range(applied_min, applied_max), "."
        )
      } else {
        "The corrected range was retained."
      }
      return(make_row(step, label, TRUE, TRUE, "applied", paste(
        range_detail, comparison, "The improved correction was retained."
      )))
    }
    if(identical(row$reason[[1L]], "no_failures")) {
      return(make_row(step, label, TRUE, FALSE, "not_needed", paste(
        comparison, "No correction was necessary."
      )))
    }
    detail <- if(nzchar(row$message[[1L]])) {
      paste0(" ", row$message[[1L]])
    } else ""
    make_row(step, label, TRUE, FALSE, "rejected", paste0(
      comparison, " The candidate was not retained (",
      gsub("_", " ", row$reason[[1L]], fixed = TRUE), ").", detail
    ))
  }
  rows[[3L]] <- range_row(
    "flatten", "co2_region", "CO2 flattening", enabled[["flatten"]]
  )
  rows[[4L]] <- range_row(
    "tails", "high_tail", "High-tail range restriction", enabled[["tails"]]
  )
  do.call(rbind, rows)
}

app_automatic_modal_content <- function(report) {
  if(is.null(report) || !is.data.frame(report) || !nrow(report)) {
    return(tags$p("Upload spectra to review automatic corrections."))
  }
  tagList(lapply(seq_len(nrow(report)), function(i) {
    row <- report[i, , drop = FALSE]
    tags$section(
      class = paste(
        "openspecy-quality-finding openspecy-quality-finding-automatic",
        paste0("openspecy-automatic-outcome-", row$outcome[[1L]]),
        if(isTRUE(row$applied[[1L]])) "openspecy-automatic-applied" else ""
      ),
      tags$h4(row$label[[1L]]),
      tags$p(tags$strong("Status: "),
             gsub("_", " ", row$outcome[[1L]], fixed = TRUE)),
      tags$p(tags$strong("Details: "), row$summary[[1L]])
    )
  }))
}

app_summary_row <- function(items) {
  if(!is.list(items)) {
    stop("Summary items must be supplied as a list.", call. = FALSE)
  }
  items <- Filter(function(item) !is.null(item), items)
  count <- length(items)
  if(count == 0L) return(NULL)

  widths <- rep.int(12L %/% count, count)
  remainder <- 12L %% count
  if(remainder > 0L) {
    widths[seq_len(remainder)] <- widths[seq_len(remainder)] + 1L
  }
  columns <- Map(
    function(item, width) {
      shiny::column(width, class = "openspecy-summary-panel", item)
    },
    items,
    widths
  )
  do.call(
    shiny::fluidRow,
    c(list(class = "openspecy-summary-grid"), unname(columns))
  )
}

# Even color steps across an integer 1..length(colors) domain, for indexed
# (categorical/binary) plotly heatmap traces.
app_indexed_colorscale <- function(colors) {
  count <- length(colors)
  if (!count) return(list(c(0, app_theme$muted), c(1, app_theme$muted)))
  if (count == 1L) return(list(c(0, unname(colors[[1L]])),
                               c(1, unname(colors[[1L]]))))
  centers <- seq(0, 1, length.out = count)
  edges <- c(0, (centers[-1L] + centers[-count]) / 2, 1)
  unlist(lapply(seq_len(count), function(i) {
    list(c(edges[[i]], unname(colors[[i]])),
         c(edges[[i + 1L]], unname(colors[[i]])))
  }), recursive = FALSE)
}

# Render one automate_particle_analysis() plot-data list (see
# R/automate_particle_analysis.R) as a themed, interactive plotly object.
# Mirrors the pre-FileSpecs heatmapA/MyPlotC theme via app_style_plotly()
# instead of the base-graphics rendering automate_particle_analysis() keeps
# for its own plot() method and static PNG/JPG downloads.
app_particle_plotly <- function(data, source = "heat_plot") {
  if (is.null(data) || identical(data$type, "empty")) {
    reason <- if (!is.null(data$reason)) data$reason else "no data available"
    plot <- plotly::plot_ly(source = source) |>
      plotly::layout(
        annotations = list(list(
          text = reason, showarrow = FALSE, x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          font = list(color = app_plot_palette$text, size = 13)
        )),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ) |>
      app_style_plotly()
    return(plotly::event_register(plot, "plotly_click"))
  }
  if (identical(data$type, "histogram")) {
    plot <- plotly::plot_ly(
      x = data$values, type = "histogram",
      marker = list(color = app_plot_palette$primary), source = source
    ) |>
      plotly::layout(
        title = data$main, xaxis = list(title = data$xlab),
        yaxis = list(title = "Count"),
        shapes = lapply(data$thresholds, function(v) list(
          type = "line", x0 = v, x1 = v, y0 = 0, y1 = 1, yref = "paper",
          line = list(color = app_theme$reference, width = 2, dash = "dash")
        ))
      ) |>
      app_style_plotly()
    return(plot)
  }

  legend_title <- if (isTruthy(data$legend_title)) data$legend_title else
    "Value"
  categorical <- data$type %in% c("heatmap_binary", "heatmap_categorical")
  if (identical(data$type, "heatmap_binary")) {
    z <- t(data$z) + 1L
    levels <- data$labels
    colorscale <- app_indexed_colorscale(
      c(app_theme$panel_2, app_theme$accent)
    )
  } else if (identical(data$type, "heatmap_categorical")) {
    z <- t(data$z)
    levels <- data$levels
    colors <- if (!is.null(data$palette)) data$palette[levels] else
      grDevices::hcl.colors(length(levels), "Viridis")
    colorscale <- app_indexed_colorscale(colors)
  } else {
    z <- t(data$z)
    colorscale <- app_heatmap_colorscale
  }

  plot <- plotly::plot_ly(source = source) |>
    plotly::add_trace(
      x = data$x, y = data$y, z = z, type = "heatmap",
      colorscale = colorscale,
      zmin = if (categorical) 0.5 else NULL,
      zmax = if (categorical) length(levels) + 0.5 else NULL,
      showscale = TRUE,
      colorbar = if (categorical) {
        list(tickmode = "array", tickvals = seq_along(levels),
             ticktext = levels, title = list(text = legend_title))
      } else {
        list(title = list(text = legend_title))
      }
    ) |>
    plotly::layout(
      title = data$title, xaxis = list(title = "X (um)"),
      yaxis = list(title = "Y (um)")
    ) |>
    app_style_plotly()
  plotly::event_register(plot, "plotly_click")
}

app_style_plotly <- function(plot) {
  plotly::layout(
    plot,
    plot_bgcolor = app_plot_palette$panel,
    paper_bgcolor = app_plot_palette$panel,
    font = list(color = app_plot_palette$text),
    xaxis = list(
      gridcolor = app_plot_palette$grid,
      zerolinecolor = app_plot_palette$grid,
      linecolor = app_plot_palette$axis,
      tickcolor = app_plot_palette$axis
    ),
    yaxis = list(
      gridcolor = app_plot_palette$grid,
      zerolinecolor = app_plot_palette$grid,
      linecolor = app_plot_palette$axis,
      tickcolor = app_plot_palette$axis
    ),
    hoverlabel = list(
      bgcolor = app_theme$panel_2,
      bordercolor = app_plot_palette$axis,
      font = list(color = app_plot_palette$text)
    )
  )
}

app_spectrum_legend_layout <- function(plot_width = NULL) {
  width <- suppressWarnings(as.numeric(plot_width))
  width <- if(length(width)) width[[1L]] else NA_real_
  if(!is.finite(width)) width <- 900
  if(width < 640) {
    return(list(
      legend = list(
        orientation = "h", x = 0, xanchor = "left",
        y = -0.22, yanchor = "top"
      ),
      margin = list(t = 28, r = 18, b = 105, l = 62)
    ))
  }
  list(
    legend = list(
      orientation = "v", x = 1.02, xanchor = "left",
      y = 1, yanchor = "top"
    ),
    margin = list(t = 28, r = 190, b = 64, l = 72)
  )
}

app_spectrum_plot <- function(active, raw = NULL, reference = NULL,
                              make_rel = FALSE, source = "B",
                              plot_width = NULL) {
  prepare_trace <- function(x, normalize = FALSE) {
    if(is.null(x)) return(NULL)
    x <- as_OpenSpecy(x)
    if(isTRUE(normalize)) x <- OpenSpecy::make_rel(x, na.rm = TRUE)
    if(ncol(x$spectra) < 1L) return(NULL)
    data.frame(
      wavenumber = x$wavenumber,
      intensity = as.numeric(as.matrix(x$spectra)[, 1L])
    )
  }
  add_spectrum <- function(plot, values, name, color, width, dash = "solid") {
    if(is.null(values)) return(plot)
    plotly::add_trace(
      plot,
      data = values,
      x = ~wavenumber,
      y = ~intensity,
      type = "scatter",
      mode = "lines",
      name = name,
      legendgroup = name,
      showlegend = TRUE,
      line = list(color = color, width = width, dash = dash),
      hovertemplate = paste0(
        name, "<br>%{x:.1f} cm<sup>-1</sup><br>",
        "%{y:.4g}<extra></extra>"
      ),
      inherit = FALSE
    )
  }

  plot <- plotly::plot_ly(source = source)
  plot <- add_spectrum(
    plot, prepare_trace(raw, normalize = make_rel), "Raw spectrum",
    "rgba(203, 213, 225, 0.24)", 1.2
  )
  plot <- add_spectrum(
    # Keep the active trace byte-for-byte on the final DataR() scale so that
    # displayed values and quantification use exactly the same processed data.
    plot, prepare_trace(active), "Active spectrum",
    app_plot_palette$spectrum, 2.4
  )
  plot <- add_spectrum(
    plot, prepare_trace(reference, normalize = make_rel),
    "Identification match",
    app_plot_palette$reference, 2.2, "dot"
  )
  legend_layout <- app_spectrum_legend_layout(plot_width)
  legend <- c(
    legend_layout$legend,
    list(
      bgcolor = "rgba(11, 25, 41, 0.82)",
      bordercolor = app_plot_palette$grid,
      borderwidth = 1,
      font = list(color = app_plot_palette$text)
    )
  )
  plotly::layout(
    plot,
    xaxis = list(
      title = "wavenumber [cm<sup>-1</sup>]",
      autorange = "reversed"
    ),
    yaxis = list(title = "intensity [-]"),
    legend = legend,
    margin = legend_layout$margin
  )
}

app_empty_spectrum_plot <- function() {
  plotly::plot_ly(type = "scatter", mode = "lines") |>
    plotly::layout(
      xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                   range = c(4000, 400)),
      yaxis = list(title = "intensity [-]", range = c(0, 1))
    ) |>
    app_style_plotly()
}

# App metadata ----
metadata_file <- ".openspecy-shiny-metadata.rds"

read_app_metadata <- function(path = metadata_file) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(...) NULL)
}

build_version_display <- function(metadata) {
  default_href <- "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/openspecy?tab=readme-ov-file#version-history"
  default_text <- paste0("Last Updated: ", format(Sys.Date()))
  default_title <- "Click here to view older versions of this app"

  if (is.null(metadata)) {
    return(list(text = default_text, href = default_href, title = default_title))
  }

  commit <- metadata$commit
  ref <- metadata$ref
  owner <- metadata$owner
  repo <- metadata$repo

  downloaded_time <- metadata$downloaded_at

  text <- paste0("App metadata date: ", downloaded_time)
  commit_display <- NULL
  if (!is.null(commit)) {
    commit_display <- substr(commit, 1, min(nchar(commit), 7))
    text <- paste0(text, " • Commit ", commit_display)
  }

  href <- default_href
  if (!is.null(owner) && !is.null(repo)) {
    href <- sprintf("https://github.com/%s/%s/commits", owner, repo)
    if (!is.null(ref)) {
      href <- sprintf("%s/%s", href, utils::URLencode(ref, reserved = TRUE))
    }
  }

  title <- default_title
  if (!is.null(downloaded_time) || !is.null(commit)) {
    parts <- c()
    if (!is.null(downloaded_time)) {
      parts <- c(parts, paste0("App metadata date ", downloaded_time))
    }
    if (!is.null(commit)) {
      parts <- c(parts, paste0("Commit ", commit))
    }
    if (length(parts)) {
      title <- paste(parts, collapse = " — ")
    }
  }

  list(text = text, href = href, title = title)
}

app_metadata <- read_app_metadata()
app_version_display <- build_version_display(app_metadata)

# The app now ships inside the OpenSpecy package. Override the historical
# remote-download metadata with package release metadata.
build_version_display <- function() {
  package_version <- tryCatch(
    as.character(utils::packageVersion("OpenSpecy")),
    error = function(...) "development"
  )

  list(
    text = paste0("OpenSpecy ", package_version),
    href = "https://github.com/wincowgerDEV/OpenSpecy-package/releases",
    title = "Click here to view OpenSpecy package releases"
  )
}

app_version_display <- build_version_display()

app_library_dir <- function() {
  configured <- Sys.getenv("OPENSPECY_SHINY_LIBRARY_PATH", "")
  if (!nzchar(configured)) {
    configured <- shiny::getShinyOption("library_path", default = "")
  }

  dir <- if (nzchar(configured) && !identical(configured, "system")) {
    configured
  } else {
    file.path(tools::R_user_dir("OpenSpecy", "cache"),
              "reference_libraries")
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

app_library_revisions <- c(
  medoid_derivative = "iThmNyMeUKhkWMvbBxQqpf1sESdQBFTs",
  medoid_nobaseline = "CLJCDpeFCMZw4hFUW4Y1QFT2cj23W1Yz",
  model_derivative = "Wk7H.Zjj4coxiMGlqQlXjV5smmZou.IH",
  model_nobaseline = "rtJY7zQTDzRISfGpvYrU0bcj8nnRYs26",
  nobaseline = "XHh26IfFkVgU6O011uKpGeXGoPNsB0_t",
  derivative = "k9DA01hqGk0dNudCu3ddhwQX.whPGrsp"
)

app_wasm_library_types <- function() {
  configured <- getOption("openspecy.shiny.wasm.libraries", character())
  if (length(configured)) return(configured)
  c("medoid_derivative", "medoid_nobaseline",
    "model_derivative", "model_nobaseline")
}

app_library_type_choices <- function() {
  if (app_wasm_mode()) {
    return(c("Medoid" = "medoid", "Multinomial" = "model"))
  }

  c("Full" = "full", "Medoid" = "medoid", "Multinomial" = "model")
}

app_validate_library_type <- function(type) {
  if (app_wasm_mode() && !type %in% app_wasm_library_types()) {
    stop(
      "The WebAssembly app only includes medoid and model libraries. ",
      "Requested unsupported library: ", type,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_app_library <- function(type) {
  app_validate_library_type(type)

  installed_library <- tryCatch(
    load_lib(type),
    error = function(e) e
  )

  if (!inherits(installed_library, "error")) {
    return(installed_library)
  }

  library_path <- app_library_dir()
  cached_library <- tryCatch(
    load_lib(type, path = library_path),
    error = function(e) e
  )

  if (!inherits(cached_library, "error")) {
    return(cached_library)
  }

  download_result <- tryCatch(
    get_lib(
      type,
      path = library_path,
      revision = unname(app_library_revisions[[type]]),
      aws = TRUE
    ),
    error = function(e) e,
    warning = function(w) w
  )

  if (inherits(download_result, c("error", "warning"))) {
    stop(
      "Unable to load the Open Specy reference library '", type,
      "' from the installed package or app cache, and downloading it failed: ",
      conditionMessage(download_result),
      ". Run get_lib(\"", type, "\") before run_app(), or check your ",
      "network connection.",
      call. = FALSE
    )
  }

  load_lib(type, path = library_path)
}

# Helper to create collapsible footnotes ----
footnote <- function(summary, ...) {
  content <- list(...)
  has_content <- length(content) && any(vapply(content, function(item) {
    text <- trimws(gsub("<[^>]+>", "", paste(as.character(item),
                                               collapse = " ")))
    nzchar(text)
  }, logical(1)))
  if(!has_content) {
    stop("Information disclosures require substantive details.",
         call. = FALSE)
  }

  tags$details(
    class = "openspecy-info-details",
    tags$summary(summary),
    tags$div(
      class = "openspecy-info-details-body",
      lapply(content, tags$p)
    )
  )
}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")

  intensity <- if(is.data.frame(raman_hdpe$spectra)) {
    raman_hdpe$spectra$intensity
  } else {
    as.numeric(raman_hdpe$spectra[, 1])
  }

  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                 intensity = intensity)

  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}

# Name keys for human readable column names ----

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))
citation <- HTML(
  'Cowger, W., Karapetrova, A., Lincoln, C., Chamas, A., Sherrod, H., Leong, N., Lasdin, K. S., 
  Knauss, C., Teofilović, V., Arienzo, M. M., Steinmetz, Z., Primpke, S., 
  Darjany, L., Murphy-Hagan, C., Moore, S., Moore, C., Lattin, G., 
  Gray, A., Kozloski, R., Bryksa, J., Maurer, B. (2025). 
  "Open Specy 1.0: Automated (Hyper)spectroscopy for Microplastics." 
  <i>Analytical Chemistry.</i> doi:
  <a href="https://doi.org/10.1021/acs.analchem.5c00962">10.1021/acs.analchem.5c00962</a>.'
)


# Define the custom theme
theme_black_minimal <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = app_plot_palette$panel,
                                     color = app_plot_palette$axis,
                                     linewidth = 0.6),
      panel.background = element_rect(fill = app_plot_palette$panel,
                                      color = NA),
      panel.border = element_rect(fill = NA, color = app_plot_palette$axis,
                                  linewidth = 0.5),
      panel.grid.major = element_line(color = app_plot_palette$grid,
                                      linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = app_plot_palette$axis),
      axis.ticks = element_line(color = app_plot_palette$axis),
      axis.text = element_text(color = app_plot_palette$text),
      axis.title = element_text(color = app_plot_palette$text),
      plot.title = element_text(color = app_plot_palette$text, hjust = 0.5),
      plot.subtitle = element_text(color = app_plot_palette$text, hjust = 0.5),
      plot.caption = element_text(color = app_plot_palette$text),
      legend.text = element_text(color = app_plot_palette$text),
      legend.title = element_text(color = app_plot_palette$text),
      legend.background = element_rect(fill = app_plot_palette$panel,
                                       color = NA),
      legend.key = element_rect(fill = app_plot_palette$panel, color = NA),
      strip.background = element_rect(fill = app_theme$panel_2,
                                      color = app_plot_palette$axis),
      strip.text = element_text(color = app_plot_palette$text)
    )
}
