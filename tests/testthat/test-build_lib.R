tiny_build_lib <- function() {
  wavenumber <- seq(100, 700, by = 100)
  base_a <- c(1, 2, 4, 8, 4, 2, 1)
  base_b <- c(1, 1, 2, 3, 5, 8, 13)
  spectra <- sapply(seq_len(8), function(i) {
    if (i <= 4) base_a + i / 20 else base_b + i / 20
  })
  colnames(spectra) <- paste0("s", seq_len(ncol(spectra)))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      source = c("A", "B", "C", "C", "A", "B", "C", "C"),
      label = c("nylon 6", "polyamides", "plastic", "missing",
                "pet", "polyesters", "plastic", "missing"),
      material_class = rep(c("class_a", "class_b"), each = 4),
      spectrum_type = rep("ftir", 8)
    )
  )
}

test_that("make_lib_lookup_template() returns or writes deduplicated templates", {
  lib <- tiny_build_lib()

  template <- make_lib_lookup_template(lib, columns = "source",
                                       add = "LibraryType")
  expect_s3_class(template, "data.table")
  expect_equal(sort(template$source), c("a", "b", "c"))
  expect_true("LibraryType" %in% names(template))
  expect_true(all(is.na(template$LibraryType)))

  tmp <- tempfile(fileext = ".csv")
  invisible(make_lib_lookup_template(lib, columns = "source",
                                     add = "LibraryType", path = tmp))
  expect_true(file.exists(tmp))
})

test_that("join_lib_metadata() reports incomplete and duplicate joins", {
  lib <- tiny_build_lib()
  lookup <- data.table::data.table(source = c("a", "b"),
                                   LibraryType = c("type_a", "type_b"))

  expect_warning(
    joined <- join_lib_metadata(lib, lookup, by = "source"),
    "unmatched_metadata_key"
  )
  expect_true(check_OpenSpecy(joined))
  expect_equal(nrow(joined$metadata), ncol(joined$spectra))
  expect_true("LibraryType" %in% names(joined$metadata))
  expect_error(
    join_lib_metadata(lib, lookup, by = "source", require_complete = TRUE),
    "unmatched_metadata_key"
  )

  dup_lookup <- data.table::data.table(source = c("a", "a"),
                                       LibraryType = c("x", "y"))
  expect_error(join_lib_metadata(lib, dup_lookup, by = "source"),
               "unique")
})

test_that("join_material_hierarchy() matches user-specified levels", {
  lib <- tiny_build_lib()
  hierarchy <- data.table::data.table(
    material = c("nylon 6", "pet"),
    material_class = c("polyamides", "polyesters"),
    material_type = c("plastic", "plastic")
  )

  expect_warning(
    joined <- join_material_hierarchy(
      lib,
      hierarchy = hierarchy,
      key_col = "label",
      levels = c("material", "material_class", "material_type"),
      output_names = c(material = "joined_material",
                       material_class = "joined_class",
                       material_type = "joined_type")
    ),
    "unmatched_hierarchy_key"
  )

  expect_true(check_OpenSpecy(joined))
  expect_equal(joined$metadata$joined_material[1], "nylon 6")
  expect_equal(joined$metadata$joined_class[2], "polyamides")
  expect_equal(joined$metadata$joined_type[3], "plastic")
  expect_true(is.na(joined$metadata$joined_type[4]))
})

test_that("standardize_lib_metadata() coalesces aliases", {
  lib <- tiny_build_lib()
  lib$metadata$SourceAlias <- ifelse(is.na(lib$metadata$source), "fallback",
                                     lib$metadata$source)
  lib$metadata$source <- NA_character_

  standardized <- standardize_lib_metadata(
    lib,
    columns = c("source", "SourceAlias"),
    metadata_aliases = list(source = "SourceAlias")
  )
  expect_true(check_OpenSpecy(standardized))
  expect_equal(unique(standardized$metadata$source), c("a", "b", "c"))
})

test_that("dedupe_spec() keeps identifiers aligned", {
  lib <- tiny_build_lib()
  lib$spectra[, 2] <- lib$spectra[, 1]

  deduped <- dedupe_spec(lib, conform_args = NULL, smooth_args = NULL)
  expect_true(check_OpenSpecy(deduped))
  expect_equal(ncol(deduped$spectra), 7)
  expect_identical(colnames(deduped$spectra),
                   deduped$metadata$sample_name)
})

test_that("reduce_lib() returns medoid ids or reduced OpenSpecy objects", {
  skip_if_not_installed("cluster")
  lib <- tiny_build_lib()

  ids <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
                    exclude_range = NULL, return = "ids")
  expect_equal(length(ids), 4)

  reduced <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
                        exclude_range = NULL)
  expect_true(check_OpenSpecy(reduced))
  expect_equal(ncol(reduced$spectra), 4)
})

test_that("build_model_lib() returns the model library artifact structure", {
  skip_if_not_installed("glmnet")
  lib <- tiny_build_lib()

  model <- suppressWarnings(
    build_model_lib(lib, type_col = NULL, range = c(100, 700),
                    exclude_range = NULL, min_n = 2, nlambda = 3)
  )
  expect_named(model, c("model", "dimension_conversion", "accuracy",
                        "confusion", "coefficients", "class_names",
                        "class_num", "observation_count",
                        "overall_accuracy", "class_accuracy",
                        "overall_accuracy2", "variable_num",
                        "all_variables", "variables_in"))
  expect_true(all(c("factor_num", "name") %in%
                    names(model$dimension_conversion)))
})

test_that("build_lib() applies named recipes to merged sources", {
  lib <- tiny_build_lib()
  built <- build_lib(
    list(lib),
    recipes = list(raw = list(),
                   relative = function(x) make_rel(x, na.rm = TRUE)),
    dedupe = FALSE
  )

  expect_named(built, c("raw", "relative"))
  expect_true(check_OpenSpecy(built$raw))
  expect_true(check_OpenSpecy(built$relative))
})
