if (file.exists("wasm-config.R")) {
  source("wasm-config.R", local = TRUE)
}

app_wasm_mode <- function() {
  env <- tolower(Sys.getenv("OPENSPECY_SHINY_WASM", ""))
  isTRUE(getOption("openspecy.shiny.wasm", FALSE)) ||
    env %in% c("1", "true", "yes", "on")
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

app_upload_limit_bytes <- function() {
  10 * 1024^3
}

app_max_request_size_bytes <- function() {
  app_upload_limit_bytes()
}

app_upload_limit_label <- function() {
  "10 GiB"
}

app_upload_guidance <- function() {
  paste0(
    "The upload ceiling is ", app_upload_limit_label(),
    " total. Choose fewer or smaller files and try again."
  )
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


app_validate_upload_size <- function(file_info) {
  limit <- app_upload_limit_bytes()
  if(is.null(file_info) || (is.data.frame(file_info) && !nrow(file_info))) {
    return(list(ok = TRUE, size = 0, limit = limit,
                message = app_upload_guidance()))
  }
  if(!is.data.frame(file_info) || !"size" %in% names(file_info)) {
    return(list(
      ok = FALSE, size = NA_real_, limit = limit,
      message = paste(
        "Open Specy could not verify the selected file sizes.",
        app_upload_guidance()
      )
    ))
  }
  sizes <- suppressWarnings(as.numeric(file_info$size))
  if(length(sizes) != nrow(file_info) || any(!is.finite(sizes) | sizes < 0)) {
    return(list(
      ok = FALSE, size = NA_real_, limit = limit,
      message = paste(
        "Every selected file must report a valid nonnegative size.",
        app_upload_guidance()
      )
    ))
  }
  total <- sum(sizes)
  ok <- total <= limit
  list(ok = ok, size = total, limit = limit,
       message = app_upload_guidance())
}

app_mounted_file_info <- function(value) {
  if(is.null(value) || !identical(value$transport, "workerfs")) {
    stop("Mounted file metadata is missing its WORKERFS transport marker.",
         call. = FALSE)
  }
  mount_id <- as.character(value$mount_id)
  if(length(mount_id) != 1L || !grepl("^[0-9a-f]{32}$", mount_id)) {
    stop("Mounted file metadata has an invalid session identifier.",
         call. = FALSE)
  }
  fields <- lapply(c("name", "size", "type", "datapath"), function(field) {
    unlist(value[[field]], use.names = FALSE)
  })
  names(fields) <- c("name", "size", "type", "datapath")
  count <- length(fields$name)
  if(!count || any(vapply(fields, length, integer(1)) != count)) {
    stop("Mounted file metadata columns do not align.", call. = FALSE)
  }
  names_safe <- nzchar(fields$name) &
    !fields$name %in% c(".", "..") &
    !grepl("[\\/[:cntrl:]]", fields$name)
  if(!all(names_safe) || anyDuplicated(tolower(fields$name))) {
    stop("Mounted files require safe, case-insensitively unique names.",
         call. = FALSE)
  }
  prefix <- paste0("/tmp/openspecy-upload-", mount_id, "/")
  expected_paths <- paste0(prefix, fields$name)
  if(!identical(as.character(fields$datapath), expected_paths)) {
    stop("Mounted file paths escaped their session directory.", call. = FALSE)
  }
  data.frame(
    name = as.character(fields$name),
    size = as.numeric(fields$size),
    type = as.character(fields$type),
    datapath = as.character(fields$datapath),
    stringsAsFactors = FALSE
  )
}

app_upload_failure_guidance <- function(elapsed_seconds, mounted = FALSE) {
  elapsed <- max(0, suppressWarnings(as.numeric(elapsed_seconds)[[1L]]))
  route <- if(isTRUE(mounted)) {
    paste(
      "The browser mount succeeded, but full in-memory reading or allocation",
      "did not. For an ENVI ZIP, try selecting the extracted HDR and DAT",
      "files together to avoid ZIP extraction overhead."
    )
  } else {
    paste(
      "For large hosted inputs, use Mount files in browser; local Shiny can",
      "continue to use the standard upload control."
    )
  }
  paste0(
    "The read/materialize phase failed after ", sprintf("%.1f", elapsed),
    " seconds. ", route,
    " Close other memory-heavy tabs or applications before retrying."
  )
}

# Vector-safe threshold truth table shared by map projection and direct tests.
# Correlation uses an inclusive minimum (equal passes); signal/noise uses strict
# interior bounds (equal to either bound fails).
app_threshold_rejection_mask <- function(values, enabled, minimum,
                                         maximum = NULL) {
  values <- suppressWarnings(as.numeric(values))
  if(!isTRUE(enabled)) return(rep(FALSE, length(values)))
  minimum <- suppressWarnings(as.numeric(minimum))
  if(length(minimum) != 1L || is.na(minimum)) {
    stop("A threshold minimum must be one numeric value.", call. = FALSE)
  }
  if(is.null(maximum)) return(is.na(values) | values < minimum)
  maximum <- suppressWarnings(as.numeric(maximum))
  if(length(maximum) != 1L || is.na(maximum)) {
    stop("A threshold maximum must be one numeric value.", call. = FALSE)
  }
  is.na(values) | values <= minimum | values >= maximum
}

# Grid-shaped plot data for an ordinary uploaded map's heatmap (Match
# Name/ID/Value, Signal/Noise), matching the same contract.
app_ordinary_heatmap_data <- function(metadata, values, categorical,
                                      legend_title, rejected = NULL,
                                      rejection_reason = NULL) {
  rejected <- if(is.null(rejected)) rep(FALSE, length(values)) else {
    out <- as.logical(rejected)
    if(length(out) != length(values)) {
      stop("The heatmap threshold mask does not align with its values.",
           call. = FALSE)
    }
    out[is.na(out)] <- FALSE
    out
  }
  if(is.null(rejection_reason)) {
    rejection_reason <- rep("active threshold", length(values))
  }
  if(length(rejection_reason) != length(values)) {
    stop("The heatmap rejection reasons do not align with its values.",
         call. = FALSE)
  }
  rejection_reason <- as.character(rejection_reason)
  rejection_reason[!rejected] <- NA_character_
  rejected_grid <- OpenSpecy:::.particle_map_grid(
    metadata, ifelse(rejected, 1, NA_real_), 1, c(0, 0)
  )$z
  reason_levels <- unique(rejection_reason[rejected & !is.na(rejection_reason)])
  reason_codes <- match(rejection_reason, reason_levels)
  reason_grid <- OpenSpecy:::.particle_map_grid(
    metadata, reason_codes, 1, c(0, 0)
  )$z
  reason_grid <- matrix(
    ifelse(is.na(reason_grid), NA_character_, reason_levels[reason_grid]),
    nrow = nrow(reason_grid), ncol = ncol(reason_grid)
  )
  if(categorical) {
    values <- droplevels(values)
    levels <- levels(values)
    grid <- OpenSpecy:::.particle_map_grid(metadata, as.integer(values), 1,
                                           c(0, 0))
    list(type = "heatmap_categorical", x = grid$x, y = grid$y, z = grid$z,
         levels = levels, legend_title = legend_title,
         palette = app_category_palette(levels), rejected = rejected_grid,
         rejection_reason = reason_grid)
  } else {
    grid <- OpenSpecy:::.particle_map_grid(metadata, values, 1, c(0, 0))
    list(type = "heatmap", x = grid$x, y = grid$y, z = grid$z,
         legend_title = legend_title, rejected = rejected_grid,
         rejection_reason = reason_grid)
  }
}

app_reference_for_query <- function(reference, query, preserve_axis = TRUE) {
  if(identical(reference$wavenumber, query$wavenumber)) {
    return(reference)
  }
  conform_spec(
    reference, range = query$wavenumber, res = NULL,
    allow_na = FALSE, type = if(isTRUE(preserve_axis)) "mean_up" else "roll"
  )
}

app_rejected_spectrum <- function(wavenumber) {
  axis <- as.numeric(wavenumber)
  spectra <- matrix(0, nrow = length(axis), ncol = 1L,
                    dimnames = list(NULL, "No retained particle"))
  as_OpenSpecy(
    axis, spectra = spectra,
    metadata = data.frame(selection = "No retained particle")
  )
}

app_particle_summary_table <- function(object) {
  md <- data.table::as.data.table(object$metadata)
  if(!nrow(md)) return(data.table::data.table())
  material <- if("material_class" %in% names(md)) {
    as.character(md$material_class)
  } else rep("unidentified", nrow(md))
  material[is.na(material) | !nzchar(material)] <- "unknown"
  area <- if("area" %in% names(md)) as.numeric(md$area) else rep(1, nrow(md))
  data.table::data.table(material_class = material, area = area)[, .(
    particle_count = .N,
    total_area_pixels = sum(area, na.rm = TRUE),
    mean_area_pixels = mean(area, na.rm = TRUE),
    median_area_pixels = stats::median(area, na.rm = TRUE)
  ), by = material_class][order(-particle_count, material_class)]
}

app_particle_size_plot <- function(object) {
  md <- data.table::as.data.table(object$metadata)
  area <- if("area" %in% names(md)) as.numeric(md$area) else numeric()
  ggplot2::ggplot(data.frame(size = sqrt(area)), ggplot2::aes(x = size)) +
    ggplot2::geom_histogram(
      bins = 30L, fill = app_plot_palette$primary,
      color = app_plot_palette$panel
    ) +
    theme_black_minimal(base_size = 15) +
    ggplot2::labs(x = "Nominal Particle Size (sqrt(area))", y = "Count")
}

app_material_summary_plot <- function(material, palette = NULL) {
  values <- as.character(material)
  values[is.na(values) | !nzchar(values)] <- "unknown"
  if(is.null(palette)) palette <- app_category_palette(values)
  missing_levels <- setdiff(unique(values), names(palette))
  if(length(missing_levels)) {
    palette <- c(palette, app_category_palette(missing_levels))
  }
  frame <- data.frame(material_class = factor(values, levels = names(palette)))
  ggplot2::ggplot(frame, ggplot2::aes(y = material_class,
                                      fill = material_class)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(
      values = palette, na.value = app_theme$muted, drop = FALSE
    ) +
    theme_black_minimal(base_size = 15) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Count", y = "Material Class")
}

app_histogram_ggplot <- function(values, thresholds = numeric(), xlab) {
  frame <- data.frame(value = as.numeric(values))
  frame <- frame[is.finite(frame$value), , drop = FALSE]
  plot <- ggplot2::ggplot(frame, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      bins = 30L, fill = app_plot_palette$primary,
      color = app_plot_palette$panel
    ) +
    theme_black_minimal(base_size = 15) +
    ggplot2::labs(x = xlab, y = "Count")
  if(nrow(frame)) {
    data_range <- range(frame$value)
    plot <- plot +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::coord_cartesian(xlim = data_range)
    for(value in thresholds[is.finite(thresholds)]) {
      clamped <- min(max(value, data_range[1L]), data_range[2L])
      plot <- plot + ggplot2::geom_vline(
        xintercept = clamped, color = app_theme$reference,
        linewidth = 0.8, linetype = "dashed"
      )
    }
  }
  plot
}

app_heatmap_ggplot <- function(data) {
  grid <- expand.grid(x = data$x, y = data$y)
  grid$value <- as.vector(data$z)
  rejected <- if(is.null(data$rejected)) rep(FALSE, nrow(grid)) else
    !is.na(as.vector(data$rejected))
  grid$rejected <- rejected
  categorical <- identical(data$type, "heatmap_categorical") ||
    identical(data$type, "heatmap_binary")
  if(categorical) {
    levels <- if(identical(data$type, "heatmap_binary")) data$labels else
      data$levels
    grid$value <- factor(levels[as.integer(grid$value)], levels = levels)
    palette <- if(!is.null(data$palette)) data$palette[levels] else
      app_category_palette(levels)
    plot <- ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = palette, na.value = app_theme$panel_2,
                                 drop = FALSE)
  } else {
    plot <- ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(
        colours = vapply(app_heatmap_colorscale, `[[`, "", 2L),
        na.value = app_theme$panel_2
      )
  }
  if(any(rejected)) {
    plot <- plot + ggplot2::geom_tile(
      data = grid[rejected, , drop = FALSE], fill = "black"
    )
  }
  plot <- plot + ggplot2::coord_equal() + theme_black_minimal(base_size = 13) +
    ggplot2::labs(x = "X (um)", y = "Y (um)", fill = data$legend_title)
  # Particle Unit and Match ID are per-particle identifiers -- essentially
  # as many categories as there are particles -- so a legend is never
  # useful for them, unlike Material Class's small fixed vocabulary.
  if(identical(data$legend_title, "Particle Unit") ||
     identical(data$legend_title, "Match ID")) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }
  plot
}

app_write_ggplot_png <- function(plot, path, width = 8, height = 6) {
  ggplot2::ggsave(
    filename = path, plot = plot, width = width, height = height,
    units = "in", dpi = 150, bg = app_plot_palette$panel
  )
  invisible(path)
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

# Metadata columns worth surfacing as a per-pixel "z variable": present, and
# not constant across every row. Excludes duplicated file/instrument-level
# metadata (file_name, organization, fixed acquisition settings, etc.) that
# repeats identically for every pixel and adds no spatial signal.
app_metadata_variable_columns <- function(metadata, exclude = character()) {
  candidates <- setdiff(names(metadata), exclude)
  keep <- vapply(candidates, function(col) {
    values <- metadata[[col]]
    length(unique(values[!is.na(values)])) > 1L
  }, logical(1))
  candidates[keep]
}

# Large sources get x/y/z/col_id plus any other per-pixel "z variable" only
# -- the columns a heatmap could be colored by -- dropping duplicated
# file-level metadata that would otherwise repeat unchanged across a huge
# number of rows. Smaller sources keep every column but move the same set to
# the front, so both cases surface what's spatially informative first.
app_uploaded_metadata_large_threshold <- 100000L

app_uploaded_metadata_display <- function(metadata, large = NULL) {
  display <- metadata[
    , !names(metadata) %in% c(".openspecy_index", ".openspecy_coord_key"),
    with = FALSE
  ]
  if(is.null(large)) {
    large <- nrow(display) > app_uploaded_metadata_large_threshold
  }
  front <- intersect(c("x", "y", "z", "col_id"), names(display))
  front <- c(front, app_metadata_variable_columns(display, exclude = front))
  if(isTRUE(large)) {
    return(display[, front, with = FALSE])
  }
  data.table::setcolorder(display, c(front, setdiff(names(display), front)))
  display
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
  large <- nrow(metadata) > app_uploaded_metadata_large_threshold
  caption <- if(isTRUE(large)) {
    paste0(
      "Uploaded Metadata (", format(nrow(metadata), big.mark = ","),
      " spectra: showing only x/y/z and other per-pixel columns that vary",
      " across spectra; unchanging file-level columns are hidden)"
    )
  } else {
    "Uploaded Metadata"
  }
  DT::datatable(
    app_uploaded_metadata_display(metadata, large = large),
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
    caption = caption,
    style = "bootstrap",
    selection = "single"
  )
}

app_selected_metadata <- function(x, selected_match, signal_to_noise) {
  # Downloaded/matched output keeps every column regardless of source size --
  # the large-source reduction in app_uploaded_metadata_display() is a
  # display-only convenience for the Uploaded Metadata tab.
  metadata <- app_uploaded_metadata_display(
    app_uploaded_metadata_cache(x, signal_to_noise), large = FALSE
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

app_matches_for_object <- function(matches, object_id) {
  matches <- data.table::as.data.table(matches)
  object_id <- as.character(object_id)
  if(!"object_id" %in% names(matches) || length(object_id) != 1L ||
     is.na(object_id)) {
    stop("A single valid spectrum identifier is required.", call. = FALSE)
  }
  selected_rows <- which(as.character(matches[["object_id"]]) == object_id)
  data.table::copy(matches[selected_rows])
}

# The Top Matches table shows the ranked candidate list for the SELECTED
# spectrum in real-library mode (matches_to_single_result already scoped to
# one spectrum), but AI mode's matches_to_single_result has one prediction
# row per spectrum in the whole dataset instead of ranked candidates, so it
# must be indexed down to the selection first. any_of() drops columns AI
# mode's narrower match_val/material_class shape doesn't have, instead of
# erroring on a literal select() of library-metadata columns that don't
# exist there.
app_top_matches_table <- function(matches_to_single_result, model_library,
                                  selected_index) {
  matches_to_single_result <- data.table::as.data.table(matches_to_single_result)
  if(isTRUE(model_library)) {
    matches_to_single_result[selected_index, ] %>%
      dplyr::select(dplyr::any_of(c(
        "match_val", "material_class", "spectrum_identity", "organization",
        "sample_name"
      )))
  } else {
    matches_to_single_result %>%
      dplyr::select("match_val", "material_class", "spectrum_identity",
                    "organization", "sample_name")
  }
}

# Project a one-pass pixel Top-N result onto collapsed units without averaging
# incomplete reference coverage into a synthetic correlation. Each retained
# value remains an actual member-pixel correlation and carries its provenance.
app_aggregate_unit_matches <- function(matches, mapping, unit_ids, library_ids,
                                       top_n = 10L) {
  matches <- data.table::copy(data.table::as.data.table(matches))
  mapping <- data.table::copy(data.table::as.data.table(mapping))
  if(!all(c("object_id", "library_id", "match_val") %in% names(matches)) ||
     !all(c("pixel_id", "unit_id", "pixel_index", "kept") %in%
          names(mapping))) {
    stop("Unit-match projection received incomplete matches or mapping.",
         call. = FALSE)
  }
  unit_ids <- as.character(unit_ids)
  library_ids <- as.character(library_ids)
  if(anyDuplicated(unit_ids) || anyDuplicated(library_ids)) {
    stop("Unit and library identifiers must be unique.", call. = FALSE)
  }
  top_n <- suppressWarnings(as.integer(top_n))
  if(length(top_n) != 1L || is.na(top_n) || top_n < 1L) top_n <- 1L
  top_n <- min(top_n, length(library_ids))

  membership <- mapping[
    kept & !is.na(unit_id),
    .(object_id = pixel_id, unit_id, pixel_index)
  ]
  joined <- merge(
    matches, membership, by = "object_id", all = FALSE, sort = FALSE,
    allow.cartesian = TRUE
  )
  if(!nrow(joined)) {
    return(data.table::data.table(
      object_id = character(), library_id = character(), match_val = numeric(),
      source_pixel_id = character()
    ))
  }
  joined[, `:=`(
    source_pixel_id = object_id,
    library_order = match(library_id, library_ids),
    unit_order = match(unit_id, unit_ids)
  )]
  if(anyNA(joined$library_order) || anyNA(joined$unit_order)) {
    stop("Unit-match projection identifiers do not align.", call. = FALSE)
  }
  data.table::setorder(
    joined, unit_order, -match_val, library_order, pixel_index, na.last = TRUE
  )
  ranked <- joined[, .SD[1L], by = .(unit_id, library_id)]
  data.table::setorder(
    ranked, unit_order, -match_val, library_order, pixel_index,
    na.last = TRUE
  )
  ranked[, .rank := seq_len(.N), by = unit_id]
  ranked <- ranked[.rank <= top_n]
  ranked[, .(
    object_id = unit_id, library_id, match_val, source_pixel_id
  )]
}

# Join and format the already-ranked blockwise result used by every app
# identification consumer. No correlation matrix is reconstructed here.
app_top_matches_export_compact <- function(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold, signal_threshold = c(-Inf, Inf), top_n = 10L,
    columns_selected = c("Simple", "All"), quant_columns = character()) {
  columns_selected <- match.arg(columns_selected)
  matches <- data.table::copy(data.table::as.data.table(matches))
  required_matches <- c("object_id", "library_id", "match_val")
  if(!all(required_matches %in% names(matches)) || !nrow(matches)) {
    stop("Top Matches requires a nonempty ranked match table.", call. = FALSE)
  }
  library_metadata <- data.table::as.data.table(library_metadata)
  spectrum_metadata <- data.table::as.data.table(spectrum_metadata)
  if(!all(c("sample_name", "material_class") %in%
          names(library_metadata))) {
    stop("Reference metadata is missing Top Matches identifiers.",
         call. = FALSE)
  }
  if(!all(c("file_name", "col_id") %in% names(spectrum_metadata))) {
    stop("Uploaded metadata is missing Top Matches identifiers.",
         call. = FALSE)
  }
  library_ids <- as.character(library_metadata$sample_name)
  spectrum_ids <- as.character(spectrum_metadata$col_id)
  if(anyDuplicated(library_ids) || anyDuplicated(spectrum_ids)) {
    stop("Top Matches identifiers must be unique.", call. = FALSE)
  }
  if(any(!matches$library_id %in% library_ids) ||
     any(!matches$object_id %in% spectrum_ids)) {
    stop("Top Matches metadata does not align with the ranked matches.",
         call. = FALSE)
  }

  top_n <- suppressWarnings(as.integer(top_n))
  if(length(top_n) != 1L || is.na(top_n) || top_n < 1L) top_n <- 1L
  top_n <- min(top_n, length(library_ids))
  matches[, .rank := seq_len(.N), by = object_id]
  matches <- matches[.rank <= top_n]
  matches[, .rank := NULL]

  thresholds <- suppressWarnings(as.numeric(signal_threshold))
  thresholds <- thresholds[!is.na(thresholds)]
  signal_min <- if(length(thresholds)) thresholds[[1L]] else -Inf
  signal_max <- if(length(thresholds) > 1L) thresholds[[2L]] else Inf
  if(length(signal_to_noise) != length(spectrum_ids)) {
    stop("Signal-to-noise values do not align with uploaded spectra.",
         call. = FALSE)
  }
  if(!is.null(names(signal_to_noise)) &&
     all(spectrum_ids %in% names(signal_to_noise))) {
    signal_to_noise <- signal_to_noise[spectrum_ids]
  }
  spectrum_details <- data.table::data.table(
    col_id = spectrum_ids,
    match_threshold = match_threshold,
    signal_to_noise = as.numeric(signal_to_noise),
    signal_threshold_min = signal_min,
    signal_threshold_max = signal_max,
    good_signal = is.finite(as.numeric(signal_to_noise)) &
      as.numeric(signal_to_noise) > signal_min &
      as.numeric(signal_to_noise) < signal_max
  )
  spectrum_details <- spectrum_details[spectrum_metadata, on = "col_id"]
  library_for_join <- data.table::copy(library_metadata)[
    , !names(library_metadata) %in% c("col_id", "file_name"), with = FALSE
  ]
  data.table::setnames(
    library_for_join, "material_class", ".reference_material_class"
  )
  if("spectrum_identity" %in% names(library_for_join)) {
    data.table::setnames(
      library_for_join, "spectrum_identity", ".reference_spectrum_identity"
    )
  } else {
    library_for_join[, .reference_spectrum_identity := NA_character_]
  }

  result <- matches %>%
    dplyr::left_join(library_for_join,
                     by = c("library_id" = "sample_name")) %>%
    dplyr::left_join(spectrum_details,
                     by = c("object_id" = "col_id")) %>%
    dplyr::rename(sample_name = library_id, col_id = object_id) %>%
    dplyr::mutate(
      good_match_vals = is.finite(match_val) & match_val >= match_threshold,
      good_matches = good_match_vals & good_signal,
      material_class = ifelse(
        good_match_vals & !is.na(.reference_material_class) &
          nzchar(.reference_material_class),
        .reference_material_class, "unknown"
      ),
      spectrum_identity = .reference_spectrum_identity
    ) %>%
    dplyr::select(-dplyr::starts_with(".reference_")) %>%
    data.table::as.data.table()

  keep <- !vapply(result, OpenSpecy::is_empty_vector, logical(1)) |
    names(result) %in% quant_columns
  result <- result[, keep, with = FALSE] %>%
    dplyr::select(
      dplyr::any_of(c(
        "file_name", "col_id", "material_class", "spectrum_identity",
        "match_val", "signal_to_noise"
      )),
      dplyr::everything()
    )
  if(identical(columns_selected, "Simple")) {
    result <- result %>%
      dplyr::select(dplyr::any_of(c(
        "file_name", "col_id", "material_class", "match_val",
        "signal_to_noise", quant_columns
      )))
  }
  data.table::as.data.table(result)
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
  "spike_decision", "spike_direction",
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
  "identification_active", "id_spec_type", "id_strategy", "lib_type",
  "top_n_input", "filter_lib", "lib_org",
  # Advanced
  "threshold_decision", "signal_basis", "MinSNR", "MaxSNR",
  "signal_selection",
  "cor_threshold_decision", "MinCor", "spatial_decision", "sigma",
  "xy_grid", "collapse_decision", "collapse_type", "particle_id_strategy",
  "particle_pca_components", "particle_cluster_k", "particle_area_threshold",
  # Quantification builder
  "quant_ratio_name", "quant_ratio_type",
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

# "Mean Up" is a resolution-aware conform strategy, not a fixed conform type:
# the uploaded spectra are only resampled to the requested resolution when
# that target is finer (a smaller cm^-1 step) than what was actually
# uploaded; otherwise the uploaded axis is left alone and the reference
# library is conformed onto it instead (see identify_blockwise's
# preserve_axis), since aggregating real uploaded data down to a coarser
# axis would discard information the library doesn't have to begin with.
app_conform_preserve_axis <- function(uploaded, conform_decision,
                                      conform_selection, conform_res) {
  if(!identical(conform_selection, "mean_up")) return(FALSE)
  if(!isTRUE(conform_decision)) return(TRUE)
  native_res <- spec_res(uploaded)
  !is.finite(native_res) || as.numeric(conform_res) >= native_res
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

app_heatmap_legend_layout <- function(title = NULL) {
  list(
    margin = list(t = 18, r = 24, b = 58, l = 66)
  )
}

app_heatmap_legend_model <- function(data, max_categories = 30L) {
  title <- if(isTruthy(data$legend_title)) data$legend_title else "Value"
  categorical <- identical(data$type, "heatmap_categorical") ||
    identical(data$type, "heatmap_binary")
  if(categorical) {
    levels <- if(identical(data$type, "heatmap_binary")) {
      as.character(data$labels)
    } else as.character(data$levels)
    colors <- if(!is.null(data$palette)) data$palette[levels] else
      app_category_palette(levels)[levels]
    return(list(
      title = title, categorical = TRUE, too_many = length(levels) > max_categories,
      levels = levels, colors = unname(colors), range = NULL
    ))
  }
  values <- as.numeric(data$z)
  values <- values[is.finite(values)]
  list(
    title = title, categorical = FALSE, too_many = FALSE,
    levels = NULL, colors = vapply(app_heatmap_colorscale, `[[`, "", 2L),
    range = if(length(values)) range(values) else c(NA_real_, NA_real_)
  )
}

app_heatmap_legend_content <- function(model) {
  if(isTRUE(model$too_many)) {
    return(tags$p(
      "More than 30 categories are present, so a categorical legend would not be useful. Use the heatmap hover information to inspect individual pixels."
    ))
  }
  if(isTRUE(model$categorical)) {
    return(tags$div(
      class = "openspecy-modal-legend-grid",
      lapply(seq_along(model$levels), function(i) tags$div(
        class = "openspecy-modal-legend-item",
        tags$span(
          `aria-hidden` = "true",
          style = paste0(
            "display:inline-block;width:1rem;height:1rem;margin-right:.55rem;",
            "vertical-align:middle;border:1px solid #d8e2ec;background:",
            model$colors[[i]], ";"
          )
        ),
        tags$span(model$levels[[i]])
      ))
    ))
  }
  labels <- if(all(is.finite(model$range))) {
    format(signif(model$range, 4), trim = TRUE)
  } else c("No finite values", "")
  tags$div(
    tags$div(
      `aria-hidden` = "true",
      style = paste0(
        "height:1.25rem;border:1px solid #d8e2ec;background:linear-gradient(90deg,",
        paste(model$colors, collapse = ","), ");"
      )
    ),
    tags$div(
      style = "display:flex;justify-content:space-between;margin-top:.35rem;",
      tags$span(labels[[1L]]), tags$span(labels[[2L]])
    )
  )
}

app_category_colors <- c(
  "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7",
  "#D55E00", "#7FDBFF", "#98D8C8", "#F4A6C1", "#FDD17A"
)

# One canonical color per label, shared by the app heatmap and Summary
# material-class bar chart, and consistent with particle_image()'s package
# static export. Known material-class names
# (R/particle_image.R's .particle_material_palette()) always get their fixed
# color; anything else cycles the app's categorical palette in sorted order.
app_category_palette <- function(values) {
  labels <- if(is.factor(values)) {
    levels(values)
  } else {
    sort(unique(as.character(values[!is.na(values)])))
  }
  if(!length(labels)) return(stats::setNames(character(), character()))
  known <- OpenSpecy:::.particle_material_palette()
  matched <- labels %in% names(known)
  colors <- rep(NA_character_, length(labels))
  colors[matched] <- unname(known[labels[matched]])
  if(any(!matched)) {
    colors[!matched] <- rep(app_category_colors,
                            length.out = sum(!matched))
  }
  stats::setNames(colors, labels)
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
  "negative_intensity", "co2_region", "high_tail", "spike", "saturation"
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
    co2_region = {
      region <- if(all(c("region_min", "region_max") %in% names(row)) &&
                   is.finite(row$region_min[[1L]]) &&
                   is.finite(row$region_max[[1L]])) {
        paste0(
          " in ", format(row$region_min[[1L]], trim = TRUE), "-",
          format(row$region_max[[1L]], trim = TRUE), " cm^-1"
        )
      } else " in the configured CO2 region"
      paste0(
        "The normalized maximum", region,
        " stayed below the artifact ratio threshold relative to the rest",
        " of the spectrum."
      )
    },
    high_tail = paste(
      "The normalized maximum in the first and last tail points stayed",
      "below the artifact ratio threshold relative to the rest of the",
      "spectrum."
    ),
    spike = "No isolated single-point spikes were detected.",
    low_snr = {
      metric <- if("metric" %in% names(row) &&
                   isTruthy(row$metric[[1L]])) {
        as.character(row$metric[[1L]])
      } else "signal-to-noise"
      paste0(
        "The ", metric,
        " metric stayed at or above the configured threshold."
      )
    },
    saturation = "No saturated spectral intervals were detected.",
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
  # spike/saturation/co2_region/high_tail (app_automatic_quality_checks)
  # used to be filtered out here on the assumption they'd only ever be
  # reported via Automatic Corrections Made. They're now also included in
  # Warnings/Successes for the viewed spectrum -- independent of whether
  # the matching correction is actually applied -- so no longer excluded.
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

# Per-cell hover text for a heatmap-family plot-data list. Returns a
# character matrix with the same dims as t(data$z), matching plotly's
# column-major (x-major) layout for a transposed z matrix.
app_heatmap_hover_text <- function(data, legend_title, levels = NULL) {
  xs <- data$x
  ys <- data$y
  z_t <- t(data$z)
  z_label <- if (!is.null(levels)) {
    ifelse(is.na(z_t), NA_character_, levels[z_t])
  } else {
    ifelse(is.na(z_t), NA_character_, format(signif(z_t, 3), trim = TRUE))
  }
  value_line <- ifelse(
    is.na(z_label), "no data", paste0(legend_title, ": ", z_label)
  )
  matrix(
    paste0("x: ", rep(xs, each = length(ys)), "<br>y: ",
           rep(ys, times = length(xs)), "<br>", value_line),
    nrow = length(ys), ncol = length(xs)
  )
}

# Render one automate_particle_analysis() plot-data list (see
# R/automate_particle_analysis.R) as a themed, interactive plotly object.
# Uses the shared heatmapA/MyPlotC Plotly theme: a heatmap trace
# (hover-only metadata, no click popover) plus a second, always-present
# marker trace that server.R moves via plotlyProxyInvoke("restyle", ...)
# on selection change instead of a full redraw. `select` is the currently
# selected point's data coordinates (list(x=, y=)) or NULL.
app_particle_plotly <- function(data, source = "heat_plot", select = NULL) {
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
    finite_values <- data$values[is.finite(data$values)]
    data_range <- if (length(finite_values)) range(finite_values) else c(0, 1)
    clamped_thresholds <- pmin(
      pmax(data$thresholds[is.finite(data$thresholds)], data_range[1L]),
      data_range[2L]
    )
    plot <- plotly::plot_ly(
      x = data$values, type = "histogram",
      marker = list(color = app_plot_palette$primary), source = source
    ) |>
      plotly::layout(
        xaxis = list(title = data$xlab, range = data_range),
        yaxis = list(title = "Count"),
        shapes = lapply(clamped_thresholds, function(v) list(
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
  levels <- NULL
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
      app_category_palette(levels)
    colorscale <- app_indexed_colorscale(colors[levels])
  } else {
    z <- t(data$z)
    colorscale <- app_heatmap_colorscale
  }
  # A continuous z matrix with no finite values (e.g. every pixel currently
  # rejected for this metric) leaves Plotly's automatic domain detection
  # nothing to work with, which throws "wasn't able to determine range of
  # domain" instead of just rendering an empty/fully-masked map.
  continuous_range <- if (categorical) {
    c(NA_real_, NA_real_)
  } else {
    finite_z <- z[is.finite(z)]
    if (length(finite_z)) {
      range(finite_z)
    } else {
      c(0, 1)
    }
  }
  if (!categorical && continuous_range[[1L]] == continuous_range[[2L]]) {
    continuous_range <- continuous_range + c(-0.5, 0.5)
  }
  hover_text <- app_heatmap_hover_text(data, legend_title, levels)
  rejected <- if(is.null(data$rejected)) {
    matrix(NA_real_, nrow = nrow(data$z), ncol = ncol(data$z))
  } else {
    data$rejected
  }
  rejected_z <- t(ifelse(is.na(rejected) | rejected == 0, NA_real_, 1))
  rejected_reason <- if(is.null(data$rejection_reason)) {
    matrix("active threshold", nrow = nrow(data$z), ncol = ncol(data$z))
  } else {
    data$rejection_reason
  }
  rejected_text <- t(ifelse(
    is.na(rejected) | rejected == 0,
    NA_character_,
    paste0("Rejected: ", rejected_reason)
  ))

  legend_layout <- app_heatmap_legend_layout(legend_title)

  select_x <- if (!is.null(select) && is.finite(select$x)) select$x else NA
  select_y <- if (!is.null(select) && is.finite(select$y)) select$y else NA

  plot <- plotly::plot_ly(source = source) |>
    plotly::add_trace(
      x = data$x, y = data$y, z = z, type = "heatmap",
      colorscale = colorscale,
      zmin = if (categorical) 0.5 else continuous_range[[1L]],
      zmax = if (categorical) length(levels) + 0.5 else continuous_range[[2L]],
      showscale = FALSE,
      hoverinfo = "text", text = hover_text, hoverongaps = FALSE
    ) |>
    plotly::add_trace(
      x = data$x, y = data$y, z = rejected_z, type = "heatmap",
      colorscale = list(c(0, "#000000"), c(1, "#000000")),
      zmin = 0, zmax = 1, showscale = FALSE,
      hoverinfo = "text", text = rejected_text, hoverongaps = FALSE,
      name = "Rejected"
    ) |>
    plotly::add_trace(
      x = select_x, y = select_y, type = "scatter", mode = "markers",
      marker = list(color = "#F59E0B", size = 14, opacity = 1,
                    line = list(color = "#FFF7ED", width = 2)),
      hoverinfo = "skip", showlegend = FALSE, name = "Selected"
    ) |>
    plotly::layout(
      xaxis = list(title = "X (um)"),
      yaxis = list(title = "Y (um)", scaleanchor = "x", scaleratio = 1),
      showlegend = FALSE, margin = legend_layout$margin
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
