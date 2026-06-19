tiny_build_lib <- function() {
  wavenumber <- seq(100, 6100, by = 100)
  base_a <- dnorm(seq(-3, 3, length.out = length(wavenumber)))
  base_b <- rev(cumsum(seq_along(wavenumber)) / sum(seq_along(wavenumber)))
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
  expect_equal(sort(template$source), c("A", "B", "C"))
  expect_true("LibraryType" %in% names(template))
  expect_true(all(is.na(template$LibraryType)))

  tmp <- tempfile(fileext = ".csv")
  invisible(make_lib_lookup_template(lib, columns = "source",
                                     add = "LibraryType", path = tmp))
  expect_true(file.exists(tmp))
})

test_that("join_lib_metadata() reports incomplete and duplicate joins", {
  lib <- tiny_build_lib()
  lookup <- data.table::data.table(source = c("A", "B"),
                                   LibraryType = c("type_a", "type_b"))

  expect_warning(
    joined <- join_lib_metadata(lib, lookup, by = "source"),
    "unmatched_metadata_key"
  )
  expect_true(check_OpenSpecy(joined))
  expect_equal(nrow(joined$metadata), ncol(joined$spectra))
  expect_true("source" %in% names(joined$metadata))
  expect_false(any(c("source.x", "source.y") %in% names(joined$metadata)))
  expect_true("LibraryType" %in% names(joined$metadata))
  expect_error(
    join_lib_metadata(lib, lookup, by = "source", require_complete = TRUE),
    "unmatched_metadata_key"
  )

  dup_lookup <- data.table::data.table(source = c("A", "A"),
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

test_that("dedupe_spec() keeps identifiers aligned", {
  lib <- tiny_build_lib()
  lib$spectra[, 2] <- lib$spectra[, 1]

  deduped <- dedupe_spec(lib)
  expect_true(check_OpenSpecy(deduped))
  expect_equal(ncol(deduped$spectra), 7)
  expect_identical(colnames(deduped$spectra),
                   deduped$metadata$sample_name)
})

test_that("reduce_lib() returns medoid ids or reduced OpenSpecy objects", {
  skip_if_not_installed("cluster")
  lib <- tiny_build_lib()

  ids <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
                    return = "ids")
  expect_equal(length(ids), 4)

  reduced <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2)
  expect_true(check_OpenSpecy(reduced))
  expect_equal(ncol(reduced$spectra), 4)
})

test_that("build_model_lib() returns the model library artifact structure", {
  skip_if_not_installed("glmnet")
  lib <- tiny_build_lib()

  model <- suppressWarnings(
    build_model_lib(lib, type_col = NULL, min_n = 2, nlambda = 3)
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
    dedupe = FALSE,
    signal_noise = FALSE
  )

  expect_named(built, c("raw", "relative"))
  expect_true(check_OpenSpecy(built$raw))
  expect_true(check_OpenSpecy(built$relative))
})

test_that("build_lib() cleans and coalesces metadata column names", {
  lib <- tiny_build_lib()
  lib$metadata[["UserName"]] <- c("alias_a", "alias_b", rep(NA, 6))
  lib$metadata[["user name"]] <- c(NA, "canonical_b", rep(NA, 6))
  lib$metadata[["NumberofAccumulations"]] <- c(10L, rep(NA_integer_, 7))
  lib$metadata[["Number of sample scans"]] <- c(20L, 30L,
                                                rep(NA_integer_, 6))
  lib$metadata[["CAS REGISTRY NO"]] <- rep("25038-54-4", 8)
  lib$metadata[["Laser (%)"]] <- rep(75, 8)

  name_lookup <- data.table::rbindlist(list(
    lib_metadata_name_lookup(),
    data.table::data.table(
      canonical_name = "project_code",
      source_name = "Campaign ID"
    )
  ))
  lib$metadata[["Campaign.ID"]] <- rep("campaign_a", 8)

  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_name_lookup = name_lookup,
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expect_true(all(grepl("^[a-z0-9]+(?:_[a-z0-9]+)*$",
                        names(built$metadata))))
  expect_equal(built$metadata$user_name[1:2],
               c("alias_a", "canonical_b"))
  expect_equal(built$metadata$number_of_accumulations[1:2], c(10L, 30L))
  expect_equal(built$metadata$cas_number, rep("25038-54-4", 8))
  expect_equal(built$metadata$laser_perc, rep(75, 8))
  expect_equal(built$metadata$project_code, rep("campaign_a", 8))
  expect_false(any(c("username", "numberofaccumulations",
                     "number_of_sample_scans") %in% names(built$metadata)))
})

test_that("build_lib() runs default joins, processing, SNR, and assessment", {
  lib <- tiny_build_lib()
  lib$spectra[1, 1] <- -1
  source_lookup <- data.table::data.table(
    Source = c("A", "B", "C"),
    Material = c("mat_a", "mat_b", "mat_c")
  )
  hierarchy <- data.table::data.table(
    Material = c("mat_a", "mat_b", "mat_c"),
    `Material Class` = c("class_a", "class_b", "class_c"),
    `Material Type` = rep("material", 3)
  )

  built <- suppressWarnings(build_lib(
    lib,
    metadata_lookups = source_lookup,
    material_hierarchy = hierarchy,
    assess = TRUE,
    dedupe = FALSE
  ))

  expect_named(built, c("raw", "derivative", "nobaseline"))
  expect_true(all(vapply(built, check_OpenSpecy, logical(1))))
  expect_true(all(c("material", "material_class", "material_type", "sn",
                    "assessment_flag", "assessment_issue_count",
                    "assessment_checks", "assessment_issues",
                    "assessment_potential_fixes") %in%
                  names(built$raw$metadata)))
  expect_equal(attr(built$derivative, "derivative_order"), "1")
  expect_equal(attr(built$nobaseline, "baseline"), "nobaseline")
  expect_true(built$raw$metadata$assessment_flag[1])
})

test_that("build_lib() preserves full source ranges through NA-aware recipes", {
  lib <- tiny_build_lib()
  left <- lib
  left$wavenumber <- lib$wavenumber[1:40]
  left$spectra <- lib$spectra[1:40, 1:4, drop = FALSE]
  left$metadata <- data.table::copy(lib$metadata[1:4])
  right <- lib
  right$wavenumber <- lib$wavenumber[22:61]
  right$spectra <- lib$spectra[22:61, 5:8, drop = FALSE]
  right$metadata <- data.table::copy(lib$metadata[5:8])

  built <- build_lib(list(left, right), dedupe = FALSE, signal_noise = FALSE)

  expect_true(all(diff(built$raw$wavenumber) == 6))
  expect_true(anyNA(built$raw$spectra))
  expect_true(anyNA(built$derivative$spectra))
  expect_true(any(is.finite(built$derivative$spectra[, 1])))
  expect_true(any(is.finite(built$nobaseline$spectra[, 8])))
})

test_that("extdata files combine into a mini library", {
  mini_files <- c(
    read_extdata("raman_hdpe.csv"),
    read_extdata("ftir_ldpe_soil.asp"),
    read_extdata("raman_atacamit.spc")
  )

  mini <- read_any(mini_files, c_spec_args = list(range = "common", res = 10))
  expect_true(check_OpenSpecy(mini))
  expect_equal(ncol(mini$spectra), 3)

  lookup <- data.table::data.table(
    file_name = basename(mini_files),
    material = c("hdpe", "ldpe in soil", "atacamite"),
    material_type = c("plastic", "plastic", "mineral")
  )
  mini <- join_lib_metadata(mini, lookup, by = "file_name",
                            require_complete = TRUE)
  built <- build_lib(mini, recipes = list(raw = list()), dedupe = FALSE,
                     signal_noise = FALSE)
  expect_true(check_OpenSpecy(built$raw))
  expect_true(all(c("material", "material_type") %in% names(built$raw$metadata)))
})
