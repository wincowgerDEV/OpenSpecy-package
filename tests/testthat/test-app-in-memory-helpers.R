.source_in_memory_app_helpers <- function() {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  setwd(app_path)
  on.exit(setwd(old_wd), add = TRUE)
  sys.source(file.path(app_path, "global.R"), envir = env)
  env
}

test_that("the in-memory upload cap is exactly 10 GiB in total", {
  env <- .source_in_memory_app_helpers()
  limit <- 10 * 1024^3

  expect_identical(env$app_upload_limit_bytes(), limit)
  expect_true(env$app_validate_upload_size(data.frame(size = limit))$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = limit + 1))$ok)
  expect_false(env$app_validate_upload_size(
    data.frame(size = c(limit - 1, 2))
  )$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = NA_real_))$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = Inf))$ok)
  expect_false(env$app_validate_upload_size(data.frame(size = -1))$ok)
  expect_false(env$app_validate_upload_size(data.frame(name = "map.h5"))$ok)
  expect_true(env$app_validate_upload_size(NULL)$ok)
})

test_that("compact Top Matches obeys requested and default Top N", {
  env <- .source_in_memory_app_helpers()
  library_ids <- paste0("lib", seq_len(12))
  object_ids <- c("query-1", "query-2")
  matches <- data.table::rbindlist(lapply(object_ids, function(object_id) {
    data.table::data.table(
      object_id = object_id,
      library_id = library_ids,
      match_val = seq(0.99, 0.44, length.out = length(library_ids))
    )
  }))
  library_metadata <- data.frame(
    sample_name = rev(library_ids),
    material_class = paste0("material-", rev(seq_along(library_ids))),
    spectrum_identity = paste0("identity-", rev(seq_along(library_ids))),
    organization = rep(c("A", "B"), each = 6L)
  )
  spectrum_metadata <- data.frame(
    col_id = rev(object_ids),
    file_name = c("second.csv", "first.csv"),
    x = c(2, 1),
    material_class = "stale-query-class",
    spectrum_identity = "stale-query-identity"
  )
  signal_to_noise <- c("query-1" = 11, "query-2" = 22)

  requested <- env$app_top_matches_export_compact(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold = 0.5, top_n = 3, simple = FALSE
  )
  requested_counts <- requested[, .N, by = col_id]
  expect_true(all(requested_counts$N <= 3L))
  expect_identical(sort(requested_counts$N), c(3L, 3L))
  expect_true(all(requested[col_id == "query-1", file_name] == "first.csv"))
  expect_true(all(requested[col_id == "query-2", file_name] == "second.csv"))
  expect_true(all(requested[col_id == "query-1", signal_to_noise] == 11))
  expect_true(all(requested[col_id == "query-2", signal_to_noise] == 22))
  expect_identical(
    requested[col_id == "query-1" & sample_name == "lib1", material_class],
    "material-1"
  )
  expect_identical(
    requested[col_id == "query-1" & sample_name == "lib1", spectrum_identity],
    "identity-1"
  )
  expect_false(any(grepl("\\.[xy]$", names(requested))))
  expect_false("x" %in% names(requested))

  grouped <- env$app_top_matches_export_compact(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold = 0.5, top_n = 2, top_n_by = "organization",
    simple = FALSE
  )
  grouped_counts <- grouped[, .N, by = .(col_id, organization)]
  expect_true(all(grouped_counts$N == 2L))
  expect_true(all(grouped[, .N, by = col_id]$N == 4L))

  default <- env$app_top_matches_export_compact(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold = 0.5, simple = FALSE
  )
  default_counts <- default[, .N, by = col_id]
  expect_true(all(default_counts$N <= 1L))
  expect_identical(sort(default_counts$N), c(1L, 1L))

  edge_matches <- data.table::data.table(
    object_id = rep("query-1", 2L),
    library_id = c("lib1", "lib2"),
    match_val = c(0.5, NA_real_)
  )
  edge <- env$app_top_matches_export_compact(
    edge_matches, library_metadata,
    spectrum_metadata[spectrum_metadata$col_id == "query-1", , drop = FALSE],
    signal_to_noise["query-1"], match_threshold = 0.5,
    top_n = 2, simple = FALSE
  )
  expect_true(edge[sample_name == "lib1", good_match_vals])
  expect_identical(edge[sample_name == "lib1", material_class], "material-1")
  expect_false(edge[sample_name == "lib2", good_match_vals])
  expect_identical(edge[sample_name == "lib2", material_class], "unknown")

  simple <- env$app_top_matches_export_compact(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold = 0.5, top_n = 1, simple = TRUE
  )
  expect_identical(names(simple), c(
    "Material Class", "Correlation", "Spectrum Identity", "Organization",
    "Signal to Noise", "File Name", "Column ID"
  ))
  expect_false(any(grepl(
    "^(X|Y|Area|Perimeter|Rectangular|Feret|Convex|Estimated Volume)",
    names(simple)
  )))
})

test_that("file-backed matching retains only one winner per eligible spectrum", {
  env <- .source_in_memory_app_helpers()
  directory <- tempfile("app-file-match-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(read_extdata("CA_tiny_map.zip"), exdir = directory)
  paths <- list.files(directory, full.names = TRUE)
  paths <- paths[grepl("\\.(dat|hdr)$", paths, ignore.case = TRUE)]
  source <- open_specs(paths, cache_dir = file.path(directory, "cache"))
  eligible <- rep(FALSE, specs_source_count(source))
  eligible[seq_len(11L)] <- TRUE
  library <- decompress_spec(source, index = c(1L, 6L, 11L))
  prepared <- env$app_prepare_correlation_reference(library)
  progress <- list()

  streamed <- env$app_stream_filespec_best_matches(
    source, eligible = eligible, chunk_size = 3L,
    process = identity,
    identify = function(query) {
      env$app_match_prepared_best(query, prepared, library_block_size = 2L)
    },
    progress = function(...) progress[[length(progress) + 1L]] <<- list(...)
  )
  eager_query <- decompress_spec(source, index = which(eligible))
  eager <- OpenSpecy:::.match_spec_blockwise(
    eager_query, library, top_n = 1L, block_size = 20L,
    conform = FALSE, type = "roll"
  )

  expect_equal(streamed, eager, ignore_attr = TRUE, tolerance = 1e-12)
  expect_identical(nrow(streamed), sum(eligible))
  expect_identical(attr(streamed, "chunk_size"), 3L)
  expect_identical(length(progress), 4L)
  expect_identical(progress[[4L]]$completed_spectra, 11L)
  expect_identical(progress[[4L]]$total_spectra, 11L)

  index <- OpenSpecy:::.filespec_index(source)
  values <- OpenSpecy:::.filespec_smoothed_values(
    source, index, which(eligible), bands = NULL, sigma1 = c(1, 1, 1)
  )
  smooth_query <- OpenSpecy:::.filespec_values_to_OpenSpecy(source, values)
  smooth_library <- filter_spec(
    smooth_query, logic = seq_len(ncol(smooth_query$spectra)) %in% c(1L, 6L, 11L)
  )
  smooth_prepared <- env$app_prepare_correlation_reference(smooth_library)
  smooth_streamed <- env$app_stream_filespec_best_matches(
    source, eligible = eligible, chunk_size = 3L, process = identity,
    identify = function(query) env$app_match_prepared_best(
      query, smooth_prepared, library_block_size = 2L
    ),
    spatial_smooth = TRUE, sigma = c(1, 1, 1)
  )
  smooth_eager <- OpenSpecy:::.match_spec_blockwise(
    smooth_query, smooth_library, top_n = 1L, block_size = 20L,
    conform = FALSE, type = "roll"
  )
  expect_equal(smooth_streamed, smooth_eager, ignore_attr = TRUE,
               tolerance = 1e-12)
})

test_that("Cluster Buster builds processed backgrounds and bounded decisions", {
  env <- .source_in_memory_app_helpers()
  processed <- as_OpenSpecy(
    1000:1003,
    spectra = matrix(
      c(1, 3, 5, 7, 3, 5, 7, 9), nrow = 4,
      dimnames = list(NULL, c("pixel-1", "pixel-2"))
    ),
    metadata = data.frame(col_id = c("pixel-1", "pixel-2"))
  )
  attr(processed, "preserve_uploaded_axis") <- TRUE
  background <- env$app_cluster_buster_background(processed)

  expect_equal(background$spectra[, 1L], rowMeans(processed$spectra))
  expect_identical(colnames(background$spectra), "background")
  expect_identical(background$metadata$organization,
                   "Temporary map background")
  expect_true(attr(background, "preserve_uploaded_axis", exact = TRUE))

  reference <- filter_spec(processed, logic = c(TRUE, FALSE))
  appended <- env$app_append_cluster_buster_background(reference, background)
  expect_identical(colnames(appended$spectra), c("pixel-1", "background"))
  expect_identical(appended$metadata$col_id, colnames(appended$spectra))
  expect_error(
    env$app_append_cluster_buster_background(appended, background),
    "already contains"
  )

  bounded <- env$app_match_bounded_best(
    processed, appended, block_size = 1L
  )
  eager <- OpenSpecy:::.match_spec_blockwise(
    processed, appended, top_n = 1L, block_size = 20L,
    conform = FALSE, type = "roll"
  )
  expect_equal(bounded, eager, ignore_attr = TRUE, tolerance = 1e-12)

  decisions <- env$app_cluster_buster_decisions(
    data.table::data.table(
      object_id = c("p1", "p2", "p3"),
      library_id = c("background", "library-a", "library-b"),
      match_val = c(0.99, 0.65, 0.95)
    ),
    pixel_ids = c("p1", "p2", "p3", "p4"),
    signal_keep = c(TRUE, TRUE, TRUE, FALSE),
    correlation_enabled = TRUE, minimum = 0.7
  )
  expect_identical(decisions$keep, c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(
    decisions$rejection_reason,
    c("background", "correlation", NA_character_, "signal/noise")
  )
})

test_that("Cluster Buster streams the mean after processing", {
  env <- .source_in_memory_app_helpers()
  directory <- tempfile("app-cluster-background-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(read_extdata("CA_tiny_map.zip"), exdir = directory)
  paths <- list.files(directory, full.names = TRUE)
  paths <- paths[grepl("\\.(dat|hdr)$", paths, ignore.case = TRUE)]
  source <- open_specs(paths, cache_dir = file.path(directory, "cache"))
  eligible <- rep(FALSE, specs_source_count(source))
  eligible[seq_len(7L)] <- TRUE
  process <- function(x) {
    x$spectra <- x$spectra^2
    attr(x, "preserve_uploaded_axis") <- TRUE
    x
  }

  streamed <- env$app_stream_filespec_processed_mean(
    source, eligible, process = process, chunk_size = 3L
  )
  eager <- process(decompress_spec(source, index = which(eligible)))

  expect_equal(streamed$spectra[, 1L], rowMeans(eager$spectra),
               tolerance = 1e-12)
  expect_true(attr(streamed, "preserve_uploaded_axis", exact = TRUE))
})

test_that("file-backed streaming reports processing that needs full memory", {
  env <- .source_in_memory_app_helpers()
  expect_length(env$app_file_stream_processing_issues(list()), 0L)
  expect_identical(
    env$app_file_stream_processing_issues(
      list(
        saturation_decision = TRUE,
        co2_decision = TRUE, co2_automate = TRUE,
        range_decision = TRUE, range_automate = TRUE
      ),
      spatial_smooth = TRUE
    ),
    c("turn off Saturation Correction", "turn off automatic CO2 flattening",
      "turn off automatic range restriction")
  )
})

test_that("raw signal/noise basis applies only the selected intensity conversion", {
  env <- .source_in_memory_app_helpers()
  spectrum <- as_OpenSpecy(
    1000:1003,
    spectra = matrix(c(20, 35, 50, 70), ncol = 1,
                     dimnames = list(NULL, "pixel-1"))
  )
  settings <- list(
    intensity_decision = TRUE, intensity_corr = "transmittance",
    make_rel_decision = TRUE
  )
  adjusted <- env$app_intensity_snr_basis(spectrum, settings)
  expected <- adj_intens(spectrum, type = "transmittance", make_rel = FALSE)
  normalized <- make_rel(expected)

  expect_equal(adjusted$spectra, expected$spectra)
  expect_false(isTRUE(all.equal(adjusted$spectra, normalized$spectra)))
  expect_identical(attr(adjusted, "intensity_unit"), "absorbance")
  expect_true(settings$make_rel_decision)
  expect_false(env$app_snr_processing_settings(settings)$make_rel_decision)
  expect_identical(
    env$app_intensity_snr_basis(
      spectrum, list(intensity_decision = FALSE, intensity_corr = "reflectance")
    ),
    spectrum
  )
})

test_that("single-spectrum Top Matches selection uses exact object IDs", {
  env <- .source_in_memory_app_helpers()
  matches <- data.table::data.table(
    object_id = c("pixel-1", "pixel-2", "pixel-1"),
    library_id = c("a", "b", "c"), match_val = c(0.9, 0.8, 0.7)
  )

  selected <- env$app_matches_for_object(matches, "pixel-1")
  expect_identical(selected$library_id, c("a", "c"))
  expect_identical(selected$object_id, c("pixel-1", "pixel-1"))
  expect_error(env$app_matches_for_object(matches, c("pixel-1", "pixel-2")))
})

test_that("heatmap colors expose identification fields only when enabled", {
  env <- .source_in_memory_app_helpers()

  expect_identical(
    unname(env$app_map_color_choices(FALSE, FALSE, FALSE)),
    "Signal/Noise"
  )
  expect_identical(
    unname(env$app_map_color_choices(FALSE, FALSE, TRUE)),
    c("Signal/Noise", "Particle Unit")
  )
  expect_identical(
    unname(env$app_map_color_choices(TRUE, FALSE, FALSE)),
    c("Material Class", "Match ID", "Match Value", "Signal/Noise")
  )
  expect_identical(
    unname(env$app_map_color_choices(TRUE, TRUE, FALSE)),
    c("Material Class", "Match Value", "Signal/Noise")
  )
  labelled <- env$app_map_color_choices(
    FALSE, FALSE, FALSE, signal_label = "Signal Times Noise"
  )
  expect_identical(unname(labelled), "Signal/Noise")
  expect_identical(names(labelled), "Signal Times Noise")
})

test_that("collapsed units reuse real member-pixel correlations", {
  env <- .source_in_memory_app_helpers()
  matches <- data.table::data.table(
    object_id = rep(c("p1", "p2"), each = 3L),
    library_id = c("a", "b", "c", "a", "b", "c"),
    match_val = c(0.9, 0.8, 0.1, 0.7, 0.6, 0.85)
  )
  mapping <- data.table::data.table(
    pixel_id = c("p1", "p2"), unit_id = c("u1", "u1"),
    pixel_index = 1:2, kept = TRUE
  )

  projected <- env$app_aggregate_unit_matches(
    matches, mapping, unit_ids = "u1", library_ids = c("a", "b", "c"),
    top_n = 2L
  )

  expect_identical(projected$object_id, c("u1", "u1"))
  expect_identical(projected$library_id, c("a", "c"))
  expect_equal(projected$match_val, c(0.9, 0.85))
  expect_identical(projected$source_pixel_id, c("p1", "p2"))
  expect_false(any(projected$match_val == mean(c(0.9, 0.7))))

  grouped <- env$app_aggregate_unit_matches(
    matches, mapping, unit_ids = "u1", library_ids = c("a", "b", "c"),
    top_n = 1L, library_groups = c("A", "A", "B")
  )
  expect_identical(grouped$library_id, c("a", "c"))

  split_membership <- data.table::data.table(
    pixel_id = c("p1", "p1"), unit_id = c("u1", "u2"),
    pixel_index = 1:2, kept = TRUE
  )
  split_projected <- env$app_aggregate_unit_matches(
    matches[object_id == "p1"], split_membership,
    unit_ids = c("u1", "u2"), library_ids = c("a", "b", "c"), top_n = 2L
  )
  expect_identical(unique(split_projected$object_id), c("u1", "u2"))
  expect_equal(nrow(split_projected), 4L)
})

test_that("uploaded-axis identification conforms only the reference", {
  env <- .source_in_memory_app_helpers()
  reference <- as_OpenSpecy(
    1:9, spectra = cbind(ref = 1:9),
    metadata = data.frame(label = "ref")
  )
  query <- as_OpenSpecy(
    c(2, 5, 8), spectra = cbind(query = c(2, 5, 8)),
    metadata = data.frame(label = "query")
  )

  conformed <- env$app_reference_for_query(reference, query, TRUE)
  expect_identical(conformed$wavenumber, query$wavenumber)
  expect_identical(query$wavenumber, c(2, 5, 8))
  expect_equal(conformed$spectra[, 1L], c(2, 5, 8))

  rejected <- env$app_rejected_spectrum(query$wavenumber)
  expect_identical(rejected$wavenumber, query$wavenumber)
  expect_true(all(rejected$spectra == 0))
})

test_that("identification block progress reports counts and percentages", {
  env <- .source_in_memory_app_helpers()
  initial <- env$app_identification_block_progress(
    query_count = 2501L, library_count = 12345L, block_size = 1000L
  )
  middle <- env$app_identification_block_progress(
    query_count = 2501L, library_count = 12345L, block_size = 1000L,
    completed_blocks = 1L, total_blocks = 3L
  )
  final <- env$app_identification_block_progress(
    query_count = 2501L, library_count = 12345L, block_size = 1000L,
    completed_blocks = 3L, total_blocks = 3L
  )

  expect_identical(initial$block_percent, 0L)
  expect_match(initial$message, "0% of blocks complete", fixed = TRUE)
  expect_match(initial$detail, "Completed 0 of 3 blocks", fixed = TRUE)
  expect_match(initial$detail, "2,501 spectra", fixed = TRUE)
  expect_match(initial$detail, "12,345 references", fixed = TRUE)
  expect_identical(middle$block_percent, 33L)
  expect_equal(middle$progress, 80)
  expect_identical(final$block_percent, 100L)
  expect_equal(final$progress, 88)
  expect_true(initial$progress < middle$progress)
  expect_true(middle$progress < final$progress)
})

test_that("heatmaps omit inline legends and build bounded modal legends", {
  env <- .source_in_memory_app_helpers()
  layout <- env$app_heatmap_legend_layout("Material Class")
  model <- env$app_heatmap_legend_model(list(
    type = "heatmap_categorical", legend_title = "Material Class",
    levels = c("PE", "PP"), palette = c(PE = "#112233", PP = "#445566")
  ))

  expect_null(layout$colorbar)
  expect_lte(layout$margin$t, 20)
  expect_true(model$categorical)
  expect_false(model$too_many)
  expect_identical(model$levels, c("PE", "PP"))

  continuous <- env$app_heatmap_legend_model(list(
    type = "heatmap", legend_title = "Match Value",
    z = matrix(c(0.12345, 0.98765), nrow = 1L)
  ))
  expect_length(continuous$ticks, 5L)
  expect_equal(continuous$ticks[c(1L, 5L)], c(0.12345, 0.98765))
  legend_html <- as.character(env$app_heatmap_legend_content(continuous))
  expect_match(legend_html, "0.123", fixed = TRUE)
  expect_match(legend_html, "0.988", fixed = TRUE)

  crowded <- env$app_heatmap_legend_model(list(
    type = "heatmap_categorical", legend_title = "Particle Unit",
    levels = as.character(seq_len(31L))
  ))
  expect_true(crowded$too_many)
  expect_match(as.character(env$app_heatmap_legend_content(crowded)),
               "More than 30 categories", fixed = TRUE)
})

test_that("source coordinate projection prefers H5 and ENVI calibration", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(x = c(0, 1), y = c(0, 1),
                         stage_x_nm = c(100, 125), stage_y_nm = c(500, 450))
  projected <- env$app_project_source_coordinates(list(), metadata, 9, "um")
  expect_identical(projected$unit, "nm")
  expect_equal(projected$metadata$x, c(100, 125))
  expect_equal(projected$metadata$grid_x, c(0, 1))

  source <- list()
  attr(source, "spatial_calibration") <- list(
    x_origin = 10, y_origin = 20, x_step = 2, y_step = -3,
    unit = "um", source = "map info"
  )
  projected <- env$app_project_source_coordinates(
    source, data.frame(x = c(0, 1), y = c(0, 1)), 9, "pixel"
  )
  expect_equal(projected$metadata$x, c(10, 12))
  expect_equal(projected$metadata$y, c(20, 17))
  expect_identical(projected$unit, "um")
})

test_that("User Metadata settings restore atomically and reset omissions", {
  env <- .source_in_memory_app_helpers()
  defaults <- stats::setNames(
    lapply(env$app_user_metadata_input_ids, function(id) {
      if(id %in% env$app_logical_setting_ids) FALSE else
        if(id %in% env$app_numeric_setting_ids) 1 else "default"
    }), env$app_user_metadata_input_ids
  )
  defaults$lib_org <- character()
  settings <- defaults
  settings$threshold_decision <- TRUE
  settings$MinSNR <- 4.567
  settings$lib_org <- c("polymer", "fiber")
  ratios <- data.frame(
    id = 1L, name = "Carbonyl", column = "ratio_carbonyl_area",
    type = "area", numerator_min = 1650, numerator_max = 1850,
    denominator_min = 1420, denominator_max = 1500
  )
  snapshot <- env$app_user_metadata_snapshot(
    settings, ratios, "now", "1.0.0", "session"
  )
  csv_row <- data.frame(snapshot, check.names = FALSE)
  csv_row$pixel_unit <- NULL
  csv_row$future_setting <- "ignored"
  parsed <- env$app_user_metadata_import(csv_row, defaults)

  expect_true(parsed$settings$threshold_decision)
  expect_equal(parsed$settings$MinSNR, 4.567)
  expect_identical(parsed$settings$lib_org, c("polymer", "fiber"))
  expect_identical(parsed$settings$pixel_unit, defaults$pixel_unit)
  expect_equal(parsed$ratios$name, "Carbonyl")
  expect_identical(parsed$unknown, "future_setting")

  invalid <- csv_row
  invalid$MinSNR <- "not numeric"
  expect_error(env$app_user_metadata_import(invalid, defaults), "MinSNR")
})

test_that("particle plotly places an image below an alpha heatmap", {
  env <- .source_in_memory_app_helpers()
  data <- list(
    type = "heatmap_categorical", x = 0:1, y = 0:1,
    z = matrix(c(1, 2, 2, 1), nrow = 2), levels = c("PE", "PP"),
    palette = c(PE = "#112233", PP = "#445566"),
    visual_image = array(0.5, dim = c(2, 2, 3)), overlay_opacity = 0.35
  )
  built <- suppressWarnings(plotly::plotly_build(env$app_particle_plotly(data)))
  expect_identical(built$x$data[[1L]]$type, "image")
  expect_identical(built$x$data[[2L]]$type, "heatmap")
  expect_equal(built$x$data[[2L]]$opacity, 0.35)
  expect_identical(built$x$data[[3L]]$name, "Rejected")
})

test_that("threshold-rejected heatmap pixels are black and gaps stay empty", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1))
  data <- env$app_ordinary_heatmap_data(
    metadata = metadata,
    values = c(0.2, 0.4, 0.6),
    categorical = FALSE,
    legend_title = "Match Value",
    rejected = c(FALSE, TRUE, FALSE),
    rejection_reason = c(NA, "below correlation threshold", NA)
  )

  expect_equal(sum(!is.na(data$z)), 3L)
  expect_equal(sum(!is.na(data$rejected)), 1L)

  widget <- env$app_particle_plotly(data)
  expect_contains(widget$x$shinyEvents, "plotly_click")
  built <- suppressWarnings(plotly::plotly_build(widget))
  rejected_trace <- built$x$data[[2L]]
  expect_identical(rejected_trace$type, "heatmap")
  expect_identical(rejected_trace$name, "Rejected")
  expect_false(isTRUE(rejected_trace$showscale))
  expect_true(all(vapply(
    rejected_trace$colorscale, `[[`, character(1), 2L
  ) == "#000000"))
  expect_equal(sum(is.finite(unlist(rejected_trace$z))), 1L)
  expect_equal(sum(is.finite(unlist(built$x$data[[1L]]$z))), 3L)
  expect_false(isTRUE(built$x$data[[1L]]$showscale))
  expect_null(built$x$layout$title)
})

test_that("a fully rejected continuous heatmap builds without a domain error", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1))
  data <- env$app_ordinary_heatmap_data(
    metadata = metadata,
    values = rep(NA_real_, 3),
    categorical = FALSE,
    legend_title = "Match Value",
    rejected = c(TRUE, TRUE, TRUE),
    rejection_reason = rep("below correlation threshold", 3)
  )
  expect_true(all(is.na(data$z)))

  widget <- env$app_particle_plotly(data)
  built <- expect_no_error(suppressWarnings(plotly::plotly_build(widget)))
  primary_trace <- built$x$data[[1L]]
  expect_true(is.finite(primary_trace$zmin))
  expect_true(is.finite(primary_trace$zmax))
  expect_lt(primary_trace$zmin, primary_trace$zmax)
})

test_that("mean_up conform preserves the uploaded axis only when appropriate", {
  env <- .source_in_memory_app_helpers()
  wavenumber <- seq(400, 4000, by = 2)
  spectra <- matrix(1, nrow = length(wavenumber), ncol = 1,
                    dimnames = list(NULL, "a"))
  uploaded <- as_OpenSpecy(wavenumber, spectra = spectra)

  # Not "mean_up": always resample regardless of the requested resolution.
  expect_false(env$app_conform_preserve_axis(uploaded, TRUE, "interp", 1))
  expect_false(env$app_conform_preserve_axis(uploaded, TRUE, "roll", 8))

  # Conform Wavenumbers turned off entirely: nothing touches the uploaded
  # axis either way, so mean_up always preserves it.
  expect_true(env$app_conform_preserve_axis(uploaded, FALSE, "mean_up", 1))

  # mean_up, conform on: a finer requested resolution (smaller cm^-1 step)
  # than the upload's native ~2 cm^-1 spacing resamples the upload up.
  expect_false(env$app_conform_preserve_axis(uploaded, TRUE, "mean_up", 1))

  # A coarser (or equal) requested resolution preserves the native axis and
  # defers to conforming the library instead.
  expect_true(env$app_conform_preserve_axis(uploaded, TRUE, "mean_up", 4))
  expect_true(env$app_conform_preserve_axis(uploaded, TRUE, "mean_up", 2))
})

test_that("threshold rejection masks are vector-safe at their boundaries", {
  env <- .source_in_memory_app_helpers()
  values <- c(NA_real_, 0.49, 0.5, 0.75, 1)

  expect_identical(
    env$app_threshold_rejection_mask(values, TRUE, minimum = 0.5),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    env$app_threshold_rejection_mask(values, TRUE, minimum = 0.5, maximum = 1),
    c(TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    env$app_threshold_rejection_mask(values, FALSE, minimum = 0.5),
    rep(FALSE, length(values))
  )
})

test_that("histograms draw threshold lines only for finite values", {
  env <- .source_in_memory_app_helpers()
  histogram <- list(
    type = "histogram", values = c(1, 2, 3), xlab = "Signal/noise",
    thresholds = c(-Inf, 2, Inf, NA_real_)
  )

  built <- plotly::plotly_build(env$app_particle_plotly(histogram))
  expect_length(built$x$layout$shapes, 1L)
  expect_identical(built$x$layout$shapes[[1L]]$x0, 2)
  expect_identical(built$x$layout$shapes[[1L]]$x1, 2)
})

test_that("histogram axis stays at the data range and clamps out-of-range thresholds", {
  env <- .source_in_memory_app_helpers()
  histogram <- list(
    type = "histogram", values = c(1, 2, 3), xlab = "Signal/noise",
    thresholds = c(-5, 10)
  )

  built <- plotly::plotly_build(env$app_particle_plotly(histogram))
  expect_identical(built$x$layout$xaxis$range, c(1, 3))
  expect_length(built$x$layout$shapes, 2L)
  shape_x0 <- vapply(built$x$layout$shapes, `[[`, numeric(1), "x0")
  expect_setequal(shape_x0, c(1, 3))

  ggplot <- env$app_histogram_ggplot(c(1, 2, 3), thresholds = c(-5, 10),
                                      xlab = "Signal/noise")
  built_range <- ggplot2::ggplot_build(ggplot)$layout$panel_params[[1L]]$x.range
  expect_equal(built_range, c(1, 3))
  vline_x <- vapply(ggplot$layers, function(layer) {
    if (inherits(layer$geom, "GeomVline")) layer$data$xintercept else NA_real_
  }, numeric(1))
  vline_x <- vline_x[!is.na(vline_x)]
  expect_setequal(vline_x, c(1, 3))
})

test_that("app spectrum plot explains a selected logistic class", {
  env <- .source_in_memory_app_helpers()
  model <- list(
    model_type = "logistic_regression",
    coefficients = data.table::data.table(
      dimensions_used = 1:3, dimension_units = c(-1, 0, 2),
      variable = 1L, name = "raman_polyethylene",
      names = c(500, 1000, 1500)
    )
  )
  spectrum <- as_OpenSpecy(
    c(500, 1000, 1500), spectra = data.frame(sample = c(0, 1, 0))
  )
  plot <- env$app_spectrum_plot(
    spectrum, model = model, model_class = "raman_polyethylene"
  )
  built <- plotly::plotly_build(plot)
  types <- vapply(built$x$data, function(trace) trace$type, character(1))
  expect_true("heatmap" %in% types)
  heat <- built$x$data[[which(types == "heatmap")[[1L]]]]
  expect_equal(heat$zmin, -2)
  expect_equal(heat$zmax, 2)
  expect_identical(built$x$layout$legend$orientation, "h")
  expect_gt(built$x$layout$legend$y, 1)
  expect_gt(heat$colorbar$x, 1)
  expect_identical(heat$colorbar$xanchor, "left")
  expect_equal(heat$colorbar$y, 0.5)
  expect_gte(built$x$layout$margin$r, 100)
  expect_match(
    heat$hovertemplate, "<extra>polyethylene</extra>", fixed = TRUE
  )
  expect_false(grepl("raman_polyethylene", heat$hovertemplate, fixed = TRUE))
})

test_that("model explanations follow the selected spectrum and Top Matches row", {
  env <- .source_in_memory_app_helpers()
  model_for <- function(classes, weights, type = "logistic_regression") {
    list(
      model_type = type,
      coefficients = data.table::rbindlist(lapply(seq_along(classes), function(i) {
        data.table::data.table(
          dimensions_used = 1:3,
          dimension_units = weights[[i]],
          variable = i,
          name = classes[[i]],
          names = c(800, 900, 1000)
        )
      }))
    )
  }
  ftir <- model_for(
    c("ftir_first", "ftir_second"),
    list(c(-2, 0, 1), c(1, 0, -3))
  )
  raman <- model_for("raman_first", list(c(0.5, 1, -0.5)))
  models <- structure(
    list(ftir = ftir, raman = raman),
    class = c("openspecy_typed_models", "list")
  )
  predictions <- data.table::data.table(
    spectrum_index = c(1L, 2L, 1L),
    prediction_rank = c(1L, 1L, 2L),
    material_class = c("ftir_first", "raman_first", "ftir_second"),
    spectrum_type = c("ftir", "raman", "ftir")
  )

  first <- env$app_selected_model_explanation(predictions, models, 1L, 1L)
  second <- env$app_selected_model_explanation(predictions, models, 1L, 2L)
  other_spectrum <- env$app_selected_model_explanation(
    predictions, models, 2L, 1L
  )
  expect_identical(first$model, ftir)
  expect_identical(first$model_class, "ftir_first")
  expect_identical(second$model, ftir)
  expect_identical(second$model_class, "ftir_second")
  expect_identical(other_spectrum$model, raman)
  expect_identical(other_spectrum$model_class, "raman_first")

  spectrum <- as_OpenSpecy(
    c(800, 900, 1000), spectra = data.frame(sample = c(0, 1, 0))
  )
  heat_weights <- function(explanation) {
    built <- plotly::plotly_build(env$app_spectrum_plot(
      spectrum, model = explanation$model,
      model_class = explanation$model_class
    ))
    types <- vapply(built$x$data, function(trace) trace$type, character(1))
    sort(unique(as.numeric(unlist(
      built$x$data[[which(types == "heatmap")[[1L]]]]$z
    ))))
  }
  expect_equal(heat_weights(first), c(-2, 0, 1))
  expect_equal(heat_weights(second), c(-3, 0, 1))

  models$ftir$model_type <- "random_forest"
  unsupported <- env$app_selected_model_explanation(
    predictions, models, 1L, 1L
  )
  expect_null(unsupported$model)
  expect_null(unsupported$model_class)
})

test_that("spatial calibration preserves geometry and creates unit-bearing metadata", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(
    file_name = "particle.dat", material_class = "raman_polyethylene",
    match_val = 0.912345, signal_to_noise = 12.3456,
    x = 2, y = 3, first_x = 1, first_y = 2, area = 4,
    perimeter = 8.7654, rectangular_min = 1.23456,
    feret_min = 2.34567, feret_max = 5.67891,
    convex_hull_area = 6.78912
  )
  original <- metadata

  calibration <- env$app_pixel_calibration(2, "\u00b5m")
  expect_identical(calibration$unit, "\u00b5m")
  expect_identical(calibration$length_suffix, "um")
  converted <- env$app_particle_metadata_units(metadata, 2, "\u00b5m")
  expect_true(all(c(
    "first_x_um", "first_y_um", "perimeter_um", "rectangular_min_um",
    "feret_min_um",
    "feret_max_um", "convex_hull_area_um2", "area_um2", "volume_um3"
  ) %in% names(converted)))
  expect_equal(converted$first_x_um, 2)
  expect_equal(converted$perimeter_um, signif(8.7654 * 2, 3))
  expect_equal(converted$rectangular_min_um, signif(1.23456 * 2, 3))
  expect_equal(converted$area_um2, 16)
  expect_equal(converted$convex_hull_area_um2, signif(6.78912 * 4, 3))
  expect_equal(converted$volume_um3, 64)
  expect_identical(metadata, original)
  expect_error(env$app_pixel_calibration(0, "um"), "positive finite")
})

test_that("simple selection metadata is friendly, ordered, and model-neutral", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(
    file_name = "particle.dat", col_id = "particle-1",
    material_class = "ftir_polyethylene", match_val = 0.912345,
    signal_to_noise = 12.3456, first_x = 1, first_y = 2, area = 4,
    perimeter = 8.7654, rectangular_min = 1.23456,
    feret_min = 2.34567, feret_max = 5.67891,
    convex_hull_area = 6.78912
  )
  simple <- env$app_selection_metadata_display(
    metadata, simple = TRUE, particle = TRUE,
    pixel_size = 2, pixel_unit = "um",
    match_label = "Probability", signal_label = "Signal Times Noise"
  )
  expect_identical(names(simple), c(
    "Material Class", "Probability", "Signal Times Noise", "Area (um^2)",
    "Perimeter (um)", "Rectangular Minimum (um)", "Feret Minimum (um)",
    "Feret Maximum (um)",
    "Convex Hull Area (um^2)", "Estimated Volume (um^3)",
    "First X (um)", "First Y (um)", "File Name", "Column ID"
  ))
  expect_identical(simple[["Material Class"]], "polyethylene")
  expect_identical(simple[["Probability"]], signif(0.912345, 3))
  expect_identical(simple[["Signal Times Noise"]], signif(12.3456, 3))
  expect_identical(simple[["Perimeter (um)"]], signif(8.7654 * 2, 3))
  expect_identical(simple[["Rectangular Minimum (um)"]],
                   signif(1.23456 * 2, 3))
  expect_identical(env$app_standardize_material_class(
    c("FTIR_polyethylene", "raman-polystyrene", "nir_other", "unknown")
  ), c("polyethylene", "polystyrene", "other", "unknown"))
  expect_identical(env$app_match_value_label(FALSE), "Correlation")
  expect_identical(env$app_match_value_label(TRUE), "Probability")
  expect_identical(
    env$app_signal_metric_label("sig_times_noise"), "Signal Times Noise"
  )

  particles <- as_OpenSpecy(
    1:3, spectra = matrix(1:6, nrow = 3),
    metadata = data.frame(area = c(4, 9))
  )
  summary <- env$app_particle_summary_table(
    particles, material = c("ftir_polyethylene", "raman_polystyrene")
  )
  expect_identical(summary$material_class, c("polyethylene", "polystyrene"))
  expect_error(
    env$app_particle_summary_table(particles, material = "polyethylene"),
    "do not align"
  )

  detailed <- env$app_selection_metadata_display(
    metadata, simple = FALSE, particle = TRUE,
    pixel_size = 1, pixel_unit = "pixel"
  )
  expect_true(all(c("first_x_pixel", "first_y_pixel", "area_pixel2") %in%
                    names(detailed)))
  expect_false("Material Class" %in% names(detailed))
})

test_that("material summary bars place the greatest count on top", {
  env <- .source_in_memory_app_helpers()
  plot <- env$app_material_summary_plot(c("common", "rare", "common"))
  expect_identical(levels(plot$data$material_class), c("rare", "common"))
})

test_that("exported heatmaps separate bounded legends from fixed canvases", {
  env <- .source_in_memory_app_helpers()
  data <- env$app_ordinary_heatmap_data(
    data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1)),
    c(0.1, 0.2, 0.3, 0.4), categorical = FALSE,
    legend_title = "Match Value"
  )
  components <- env$app_heatmap_export_components(data)
  expect_s3_class(components$heatmap, "ggplot")
  expect_s3_class(components$legend, "gtable")
  expect_identical(components$heatmap$theme$legend.position, "none")

  heatmap_path <- tempfile(fileext = ".png")
  legend_path <- tempfile(fileext = ".png")
  on.exit(unlink(c(heatmap_path, legend_path)), add = TRUE)
  env$app_write_ggplot_png(components$heatmap, heatmap_path, 8, 7)
  env$app_write_grob_png(components$legend, legend_path)
  expect_gt(file.info(heatmap_path)$size, 0)
  expect_gt(file.info(legend_path)$size, 0)
})

test_that("identity pixel mappings preserve compact Specs source IDs", {
  env <- .source_in_memory_app_helpers()
  object <- as_OpenSpecy(
    100:102,
    spectra = matrix(
      1:6, nrow = 3,
      dimnames = list(NULL, c("pixel-a", "pixel-b"))
    ),
    metadata = data.frame(
      file_name = c("map.dat", "map.dat"), x = c(4, 6), y = c(8, 8)
    )
  )
  compact <- as_Specs(object, steps = character())

  mapping <- env$app_identity_pixel_mapping(compact, c(TRUE, FALSE))

  expect_identical(mapping$pixel_id, c("pixel-a", "pixel-b"))
  expect_identical(mapping$pixel_index, 1:2)
  expect_equal(mapping$x, c(4, 6))
  expect_equal(mapping$y, c(8, 8))
  expect_identical(mapping$kept, c(TRUE, FALSE))
  expect_identical(mapping$unit_index, c(1L, NA_integer_))
  expect_error(
    env$app_identity_pixel_mapping(compact, TRUE),
    "eligibility must align"
  )
})

test_that("file-backed selection starts at the first retained pixel safely", {
  env <- .source_in_memory_app_helpers()
  mapping <- data.table::data.table(
    pixel_index = 1:4, kept = c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_identical(env$app_first_retained_pixel(mapping), 3L)
  expect_true(is.na(env$app_first_retained_pixel(mapping[kept == FALSE])))
  expect_error(
    env$app_first_retained_pixel(mapping[, .(pixel_index)]),
    "missing retained-selection columns"
  )
})

test_that("heatmap calibration labels axes and peak overlays use markers only", {
  env <- .source_in_memory_app_helpers()
  metadata <- data.frame(x = c(0, 1), y = c(0, 0))
  calibrated <- env$app_calibrate_spatial_metadata(metadata, 2.5, "um")
  expect_equal(calibrated$x, c(0, 2.5))
  heatmap <- env$app_ordinary_heatmap_data(
    calibrated, c(1, 2), FALSE, "Signal/Noise", axis_unit = "um"
  )
  built_heatmap <- suppressWarnings(
    plotly::plotly_build(env$app_particle_plotly(heatmap))
  )
  x_title <- built_heatmap$x$layout$xaxis$title
  y_title <- built_heatmap$x$layout$yaxis$title
  if(is.list(x_title)) x_title <- x_title$text
  if(is.list(y_title)) y_title <- y_title$text
  expect_identical(x_title, "X (um)")
  expect_identical(y_title, "Y (um)")
  expect_match(
    env$app_heatmap_hover_text(heatmap, "Signal/Noise")[[1L]],
    "x (um):", fixed = TRUE
  )

  spectrum <- as_OpenSpecy(
    101:105, spectra = matrix(c(0, 1, 3, 1, 0), ncol = 1)
  )
  peaks <- env$app_peak_positions(spectrum, 1L)
  built_spectrum <- plotly::plotly_build(
    env$app_spectrum_plot(spectrum, peaks = peaks)
  )
  peak_trace <- built_spectrum$x$data[[2L]]
  expect_identical(peak_trace$mode, "markers")
  expect_identical(peak_trace$marker$color, "#3B82F6")
  expect_null(peak_trace$textposition)
  expect_match(peak_trace$hovertemplate, "Peak rank", fixed = TRUE)
})

test_that("processed particle RDS metadata restores heatmap coordinates", {
  env <- .source_in_memory_app_helpers()
  particles <- as_OpenSpecy(
    100:102,
    spectra = matrix(seq_len(9), nrow = 3,
                     dimnames = list(NULL, paste0("unit_", 1:3))),
    metadata = data.frame(
      col_id = paste0("unit_", 1:3),
      x_pixel = c(2, 5, 8), y_pixel = c(3, 4, 7),
      area_pixel2 = c(4, 9, 16)
    )
  )
  particles$metadata[, c("x", "y") := NULL]

  restored <- env$app_restore_spatial_coordinates(particles)

  expect_equal(restored$metadata$x, particles$metadata$x_pixel)
  expect_equal(restored$metadata$y, particles$metadata$y_pixel)
  expect_identical(attr(restored, "openspecy_spatial_unit"), "pixel")
  expect_true(all(c("x_pixel", "y_pixel", "area_pixel2") %in%
                    names(restored$metadata)))
  mapping <- env$app_identity_pixel_mapping(restored)
  expect_equal(mapping$x, c(2, 5, 8))
  expect_equal(mapping$y, c(3, 4, 7))
  heatmap <- env$app_ordinary_heatmap_data(
    restored$metadata, c(1, 2, 3), FALSE, "Signal/Noise"
  )
  expect_identical(heatmap$type, "heatmap")
  expect_true(any(is.finite(heatmap$z)))
})
