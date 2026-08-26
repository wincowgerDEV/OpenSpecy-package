make_specs_test_os <- function(nx = 4, ny = 4) {
  variables <- seq(1000, 1070, by = 10)
  n <- nx * ny
  spectra <- vapply(seq_len(n), function(i) {
    sin(variables / 80 + i / 3) + i / 10 + variables / 5000
  }, FUN.VALUE = numeric(length(variables)))
  colnames(spectra) <- paste0("s", seq_len(n))
  metadata <- data.frame(
    x = rep(seq_len(nx) - 1L, times = ny),
    y = rep(seq_len(ny) - 1L, each = nx),
    group = rep(c("a", "b"), length.out = n)
  )
  as_OpenSpecy(variables, spectra = spectra, metadata = metadata)
}

test_that("Specs constructor and checks validate objects", {
  values <- matrix(1:6, nrow = 2,
                   dimnames = list(c("PC1", "PC2"), c("a", "b", "c")))
  coords <- data.table(x = 0:2, y = 0, source_id = c("a", "b", "c"),
                       value_id = c("a", "b", "c"))
  specs <- Specs(c("PC1", "PC2"), values, coords = coords)

  expect_s3_class(specs, "Specs")
  expect_true(is_Specs(specs))
  expect_true(check_Specs(specs))
  expect_equal(names(specs), c("variables", "values", "coords", "metadata"))

  Specs(c("PC1"), values) |> expect_error()
  expect_false(suppressWarnings(check_Specs(list())))
})

make_compact_specs_test_object <- function() {
  values <- matrix(
    c(1, 2, 3, 4, 5, 6), nrow = 3,
    dimnames = list(c("1000", "1010", "1020"), c("V1", "V2"))
  )
  regions <- data.table(
    name = "Region1", n = 6L, nx = 3L, ny = 2L,
    x_origin = 10, y_origin = 20, x_step = 2, y_step = 3,
    id_prefix = "px_"
  )
  coords <- .compact_specs_coords(regions, c(1L, 1L, 0L, 2L, 2L, 0L))
  source_md <- data.table(
    sample = rep("map", 6), group = rep(c("a", "b"), each = 3),
    note = paste0("n", seq_len(6))
  )
  Specs(
    variables = rownames(values), values = values, coords = coords,
    metadata = data.table(value_id = colnames(values)),
    attributes = list(
      source_metadata = .encode_specs_metadata(source_md),
      background = list(
        mask = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
        signal_to_noise = c(5, 5, 0, 6, 6, NA),
        reason = c(0L, 0L, 1L, 0L, 0L, 3L),
        reason_levels = c("foreground", "below_minimum", "above_maximum",
                          "nonfinite"),
        policy = specs_background_filter(minimum = 4)
      )
    )
  )
}

test_that("compact Specs preserve legacy compatibility and indexed access", {
  compact <- make_compact_specs_test_object()
  coords <- specs_coordinates(compact, c(1, 3, 6))
  metadata <- specs_metadata(compact, c(2, 5))

  expect_true(check_Specs(compact))
  expect_equal(specs_source_count(compact), 6L)
  expect_equal(coords$x, c(10, 14, 14))
  expect_equal(coords$y, c(20, 20, 23))
  expect_equal(coords$value_index, c(1L, 0L, 0L))
  expect_equal(metadata$sample, c("map", "map"))
  expect_equal(metadata$group, c("a", "b"))
  expect_equal(specs_background_mask(compact),
               c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(unname(specs_source_values(compact, c(3, 4))[, 1]), c(0, 0, 0))

  legacy <- Specs(compact$variables, compact$values)
  legacy$coords[, value_index := NULL]
  attr(legacy, "specs_version") <- "0.1.0"
  expect_true(check_Specs(legacy))
})

test_that("weighted compact PCA matches an expanded foreground oracle", {
  compact <- make_compact_specs_test_object()
  model <- fit_specs_pca(compact, n_components = 1)
  expanded <- t(compact$values[, c(1, 1, 2, 2), drop = FALSE])
  oracle <- stats::prcomp(expanded, rank. = 1)

  expect_equal(model$center, oracle$center, tolerance = 1e-12)
  expect_equal(abs(model$rotation), abs(oracle$rotation[, 1, drop = FALSE]),
               tolerance = 1e-12)

  transformed <- as_Specs(
    compact, steps = c("pca", "hilbert"), n_components = 1,
    bits_per_variable = 4
  )
  expect_equal(specs_source_values(transformed, c(3, 6)),
               matrix(0, nrow = 2, ncol = 2,
                      dimnames = list(c("hilbert_hi", "hilbert_lo"),
                                      c("px_20_14", "px_23_14"))))
  expect_equal(unname(decompress_spec(transformed, index = c(3, 6))$spectra),
               matrix(0, nrow = 3, ncol = 2))
})

test_that("weighted compact K-means excludes background and preserves counts", {
  compact <- make_compact_specs_test_object()
  initial <- rbind(c(1, 2, 3), c(4, 5, 6))
  expanded <- t(compact$values[, c(1, 1, 2, 2), drop = FALSE])
  oracle <- stats::kmeans(expanded, centers = initial, algorithm = "Lloyd")
  clustered <- as_Specs(
    compact, steps = "kmeans", centers = initial, algorithm = "Lloyd"
  )

  expect_equal(unname(t(clustered$values)), unname(oracle$centers),
               tolerance = 1e-12)
  expect_equal(clustered$metadata$cluster_size, oracle$size)
  expect_equal(specs_coordinates(clustered)$value_index,
               c(1L, 1L, 0L, 2L, 2L, 0L))
  expect_equal(specs_source_values(clustered, c(3, 6)),
               matrix(0, nrow = 3, ncol = 2,
                      dimnames = list(c("1000", "1010", "1020"),
                                      c("px_20_14", "px_23_14"))))
})

test_that("precomputed background classification preserves foreground spectra", {
  exact <- make_specs_test_os(nx = 3, ny = 2) |>
    as_Specs(steps = character())
  policy <- specs_background_filter(minimum = 4, maximum = 9)
  classified <- .apply_specs_background_result(
    exact, policy, c(5, 3, 6, Inf, 7, 9), basis = "fully_processed"
  )

  expect_equal(specs_background_mask(classified),
               c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(
    unname(decompress_spec(classified, index = c(2, 4, 6))$spectra),
    matrix(0, nrow = nrow(exact$values), ncol = 3)
  )
  expect_equal(unname(classified$values),
               unname(exact$values[, c(1, 3, 5), drop = FALSE]))
  expect_identical(attr(classified, "background")$basis, "fully_processed")
  expect_true(check_Specs(classified))
  expect_error(
    .apply_specs_background_result(classified, policy, rep(5, 6)),
    "already"
  )
})

test_that("fit_specs_pca() and as_Specs() compress OpenSpecy objects", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  specs <- as_Specs(os, model, steps = "pca")

  expect_s3_class(model, "SpecsPCA")
  expect_equal(model$variables, paste0("PC", 1:3))
  expect_s3_class(specs, "Specs")
  expect_equal(dim(specs$values), c(3, ncol(os$spectra)))
  expect_equal(specs$variables, model$variables)
  expect_equal(specs$coords$source_id, colnames(os$spectra))
  expect_equal(specs$coords$value_id, colnames(os$spectra))
  expect_equal(attr(specs, "variable_model")$model_id, model$model_id)
  expect_true(check_Specs(specs))
})

test_that("as_Specs() defaults to PCA followed by Hilbert compression", {
  os <- make_specs_test_os()
  specs <- as_Specs(os)

  expect_s3_class(specs, "Specs")
  expect_equal(specs$variables, c("hilbert_hi", "hilbert_lo"))
  expect_equal(nrow(specs$values), 2)
  expect_equal(attr(specs, "variable_model")$model_type, "pca")
  expect_equal(attr(specs, "hilbert_model")$model_type, "hilbert")
  expect_lte(attr(specs, "hilbert_model")$total_bits, 64)
  expect_true(check_Specs(specs))
})

test_that("decompress_spec() reconstructs OpenSpecy objects", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  specs <- as_Specs(os, model)

  expanded <- decompress_spec(specs, expand = TRUE)
  coerced <- as_OpenSpecy(specs)
  active <- decompress_spec(specs, expand = FALSE)

  expect_s3_class(expanded, "OpenSpecy")
  expect_true(check_OpenSpecy(expanded))
  expect_equal(coerced, expanded)
  expect_equal(nrow(expanded$spectra), length(os$wavenumber))
  expect_equal(ncol(expanded$spectra), nrow(specs$coords))
  expect_equal(expanded$metadata$source_id, specs$coords$source_id)

  expect_s3_class(active, "OpenSpecy")
  expect_equal(ncol(active$spectra), ncol(specs$values))

  single <- decompress_spec(specs, index = 5)
  subset <- decompress_spec(specs, index = c(2, 5))
  active_single <- decompress_spec(specs, expand = FALSE, index = 3)

  expect_equal(ncol(single$spectra), 1)
  expect_equal(single$metadata$source_id, specs$coords$source_id[5])
  expect_equal(single$spectra[, 1], expanded$spectra[, 5])
  expect_equal(subset$metadata$source_id, specs$coords$source_id[c(2, 5)])
  expect_equal(subset$spectra, expanded$spectra[, c(2, 5)])
  expect_equal(active_single$spectra[, 1], active$spectra[, 3])
  expect_error(decompress_spec(specs, index = nrow(specs$coords) + 1),
               "outside x\\$coords")
  expect_error(decompress_spec(specs, index = c(1, 1)), "duplicate")
})

test_that("compact Specs are accepted by OpenSpecy particle partitioning", {
  specs <- make_compact_specs_test_object()
  partition <- .partition_particle_map(
    specs, eligible = !specs_background_mask(specs), strategy = "collapse",
    area_threshold = 1
  )

  expect_equal(nrow(partition$pixel_to_unit), specs_source_count(specs))
  expect_equal(ncol(partition$display$spectra), specs_source_count(specs))
  expect_equal(nrow(partition$display$metadata), specs_source_count(specs))
  expect_false(any(partition$pixel_to_unit$kept[
    specs_background_mask(specs)
  ]))
})

test_that("Hilbert helpers support Hilbert-only compression and validation", {
  os <- make_specs_test_os()
  raw_specs <- as_Specs(os, steps = character())
  hilbert <- encode_specs_hilbert(raw_specs, bits_per_variable = 4)
  decoded <- decode_specs_hilbert(hilbert)
  expanded <- decompress_spec(hilbert)

  expect_equal(hilbert$variables, c("hilbert_hi", "hilbert_lo"))
  expect_equal(decoded$variables, as.character(os$wavenumber))
  expect_equal(expanded$wavenumber, os$wavenumber)
  expect_equal(ncol(expanded$spectra), ncol(os$spectra))
  expect_true(check_Specs(hilbert))
  expect_true(check_Specs(decoded))

  too_many <- Specs(paste0("v", seq_len(9)),
                    matrix(seq_len(18), nrow = 9))
  expect_error(
    encode_specs_hilbert(too_many, bits_per_variable = 8),
    "no more than 8 variables"
  )
})

test_that("as_Specs() can apply K-means compression", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  set.seed(42)
  specs <- as_Specs(os, model, steps = c("pca", "kmeans"),
                    centers = 2, nstart = 5)

  expect_equal(ncol(specs$values), 2)
  expect_true(all(specs$coords$value_id %in% colnames(specs$values)))
  expect_equal(sum(specs$metadata$cluster_size), ncol(os$spectra))
  expect_equal(attr(specs, "spectrum_compression")$method, "kmeans")
  expect_true(check_Specs(specs))
})

test_that("K-means can run at legal Specs compression positions", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)

  set.seed(42)
  before <- as_Specs(os, steps = c("kmeans", "pca", "hilbert"),
                     n_components = 2, centers = 3, nstart = 5)
  set.seed(42)
  middle <- as_Specs(os, model, steps = c("pca", "kmeans", "hilbert"),
                     centers = 3, nstart = 5)
  set.seed(42)
  after <- as_Specs(os, model, steps = c("pca", "hilbert", "kmeans"),
                    centers = 3, nstart = 5)

  expect_true(check_Specs(before))
  expect_true(check_Specs(middle))
  expect_true(check_Specs(after))
  expect_equal(before$variables, c("hilbert_hi", "hilbert_lo"))
  expect_equal(middle$variables, c("hilbert_hi", "hilbert_lo"))
  expect_equal(after$variables, c("hilbert_hi", "hilbert_lo"))
  expect_equal(ncol(before$values), 3)
  expect_equal(ncol(middle$values), 3)
  expect_equal(ncol(after$values), 3)

  expect_error(as_Specs(os, steps = c("hilbert", "pca")),
               "PCA cannot run after Hilbert")
})

test_that("Specs latent matching works and validates model compatibility", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  library <- as_Specs(os, model, steps = "pca")
  unknown <- as_Specs(os, model, steps = c("pca", "kmeans"),
                      centers = 2, nstart = 5)

  cors <- cor_spec(unknown, library)
  expect_equal(dim(cors), c(ncol(library$values), ncol(unknown$values)))

  matches <- match_spec(unknown, library, top_n = 1, expand = FALSE)
  expect_s3_class(matches, "data.table")
  expect_equal(nrow(matches), ncol(unknown$values))
  expect_contains(names(matches), c("object_id", "library_id", "match_val"))

  expanded <- match_spec(unknown, library, top_n = 1, expand = TRUE)
  expect_gt(nrow(expanded), nrow(matches))
  expect_contains(names(expanded), c("active_value_id", "source_id", "x", "y"))

  bad <- library
  model_meta <- attr(bad, "variable_model")
  model_meta$model_id <- "different"
  attr(bad, "variable_model") <- model_meta
  cor_spec(unknown, bad) |> expect_error()
})

test_that("Specs Hilbert matching uses compatible code distance", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  pca_specs <- as_Specs(os, model, steps = "pca")
  library <- encode_specs_hilbert(pca_specs, bits_per_variable = 8)
  unknown <- encode_specs_hilbert(pca_specs,
                                  limits = attr(library, "hilbert_model"))

  matches <- match_spec(unknown, library, top_n = 1)
  expect_s3_class(matches, "data.table")
  expect_contains(names(matches), c("object_id", "library_id", "match_val",
                                    "match_distance"))
  expect_true(all(matches$match_distance == 0))
  expect_setequal(matches$object_id, matches$library_id)
  cor_spec(unknown, library) |> expect_error("distance matching")

  bad <- library
  bad_model <- attr(bad, "hilbert_model")
  bad_model$bits_per_variable <- 4L
  attr(bad, "hilbert_model") <- bad_model
  expect_error(match_spec(unknown, bad, top_n = 1), "same variables")
})

test_that("def_features() and collapse_spec() work with Specs coords", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  specs <- as_Specs(os, model, steps = c("pca", "kmeans"),
                    centers = 3, nstart = 5)

  features <- specs$coords$y == 0
  id_specs <- def_features(specs, features)
  expect_s3_class(id_specs, "Specs")
  expect_true("feature_id" %in% names(id_specs$coords))

  collapsed <- collapse_spec(id_specs, column = "feature_id")
  expect_s3_class(collapsed, "Specs")
  expect_true(check_Specs(collapsed))
  expect_equal(ncol(collapsed$values), length(unique(id_specs$coords$feature_id)))
  expect_equal(collapsed$coords$value_id,
               as.character(id_specs$coords$feature_id))
})

test_that("write_specs() and read_specs() roundtrip Specs objects", {
  os <- make_specs_test_os()
  model <- fit_specs_pca(os, n_components = 3)
  specs <- as_Specs(os, model, steps = c("pca", "kmeans"),
                    centers = 2, nstart = 5)
  file <- tempfile(fileext = ".rds")

  write_specs(specs, file) |> expect_silent()
  read <- read_specs(file)

  expect_s3_class(read, "Specs")
  expect_equal(read, specs)
  suppressWarnings(read_specs(tempfile())) |> expect_error()
})
