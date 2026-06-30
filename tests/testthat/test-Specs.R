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
  active <- decompress_spec(specs, expand = FALSE)

  expect_s3_class(expanded, "OpenSpecy")
  expect_true(check_OpenSpecy(expanded))
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
