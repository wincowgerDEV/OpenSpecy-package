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
    spectrum_identity = paste0("identity-", rev(seq_along(library_ids)))
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
    match_threshold = 0.5, top_n = 3, columns_selected = "All"
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

  default <- env$app_top_matches_export_compact(
    matches, library_metadata, spectrum_metadata, signal_to_noise,
    match_threshold = 0.5, columns_selected = "All"
  )
  default_counts <- default[, .N, by = col_id]
  expect_true(all(default_counts$N <= 10L))
  expect_identical(sort(default_counts$N), c(10L, 10L))

  edge_matches <- data.table::data.table(
    object_id = rep("query-1", 2L),
    library_id = c("lib1", "lib2"),
    match_val = c(0.5, NA_real_)
  )
  edge <- env$app_top_matches_export_compact(
    edge_matches, library_metadata,
    spectrum_metadata[spectrum_metadata$col_id == "query-1", , drop = FALSE],
    signal_to_noise["query-1"], match_threshold = 0.5,
    top_n = 2, columns_selected = "All"
  )
  expect_true(edge[sample_name == "lib1", good_match_vals])
  expect_identical(edge[sample_name == "lib1", material_class], "material-1")
  expect_false(edge[sample_name == "lib2", good_match_vals])
  expect_identical(edge[sample_name == "lib2", material_class], "unknown")
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
    c("Signal/Noise", "Spectrum Index")
  )
  expect_identical(
    unname(env$app_map_color_choices(FALSE, FALSE, TRUE)),
    c("Signal/Noise", "Particle Unit", "Spectrum Index")
  )
  expect_identical(
    unname(env$app_map_color_choices(TRUE, FALSE, FALSE)),
    c("Material Class", "Match ID", "Match Value", "Signal/Noise",
      "Spectrum Index")
  )
  expect_identical(
    unname(env$app_map_color_choices(TRUE, TRUE, FALSE)),
    c("Material Class", "Match Value", "Signal/Noise", "Spectrum Index")
  )
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

  crowded <- env$app_heatmap_legend_model(list(
    type = "heatmap_categorical", legend_title = "Particle Unit",
    levels = as.character(seq_len(31L))
  ))
  expect_true(crowded$too_many)
  expect_match(as.character(env$app_heatmap_legend_content(crowded)),
               "More than 30 categories", fixed = TRUE)
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
