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
      spectrum_type = rep("ftir", 8),
      intensity_units = rep("absorbance", 8)
    ),
    attributes = list(intensity_unit = "absorbance")
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

test_that("build_lib() converts metadata intensity units before recipes", {
  lib <- tiny_build_lib()
  lib$spectra <- matrix(
    rep(c(50, 25, 0.5, 0.25, 2), each = nrow(lib$spectra)),
    nrow = nrow(lib$spectra),
    dimnames = list(NULL, paste0("u", 1:5))
  )
  lib$metadata <- data.table::data.table(
    sample_name = colnames(lib$spectra),
    intensity_units = c(
      "Reflectance (%)", "transmittance", "absorbance", "mystery", NA
    )
  )
  attr(lib, "intensity_unit") <- NULL
  original <- lib$spectra

  expect_warning(
    built <- build_lib(
      list(lib),
      recipes = list(raw = list()),
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "skipped 2 spectrum/s.*<missing> \\(1\\).*mystery \\(1\\)|skipped 2 spectrum/s.*mystery \\(1\\).*<missing> \\(1\\)"
  )

  expect_equal(
    built$spectra[, 1],
    adj_intens(original[, 1], type = "reflectance", make_rel = FALSE)
  )
  expect_equal(
    built$spectra[, 2],
    adj_intens(original[, 2], type = "transmittance", make_rel = FALSE)
  )
  expect_equal(built$spectra[, 3:5], original[, 3:5])
  expect_equal(
    built$metadata$intensity_units,
    c("absorbance", "absorbance", "absorbance", "mystery", NA)
  )
  expect_null(attr(built, "intensity_unit"))
  expect_true(check_OpenSpecy(built))
})

test_that("build_lib() treats intensity_unit attribute as primary truth", {
  lib <- tiny_build_lib()
  lib$spectra[,] <- 50
  lib$metadata$intensity_units <- "transmittance"
  attr(lib, "intensity_unit") <- "reflectance"

  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expect_equal(
    built$spectra,
    adj_intens(lib$spectra, type = "reflectance", make_rel = FALSE)
  )
  expect_equal(built$metadata$intensity_units, rep("absorbance", 8))
  expect_equal(attr(built, "intensity_unit"), "absorbance")

  attr(lib, "intensity_unit") <- "absorbance"
  unchanged <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw
  expect_equal(unchanged$spectra, lib$spectra)
  expect_equal(unchanged$metadata$intensity_units, rep("absorbance", 8))
})

test_that("build_lib() can preserve declared intensity units", {
  lib <- tiny_build_lib()
  lib$spectra[,] <- 50
  lib$metadata$intensity_units <- "reflectance"
  attr(lib, "intensity_unit") <- "reflectance"

  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )$raw

  expect_equal(built$spectra, lib$spectra)
  expect_equal(built$metadata$intensity_units, rep("reflectance", 8))
  expect_equal(attr(built, "intensity_unit"), "reflectance")
})

test_that("build_lib() restricts a one-object source list when requested", {
  lib <- tiny_build_lib()
  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    restrict_range_args = list(
      min = c(100, 2500),
      max = c(2000, 4000)
    ),
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  keep <- lib$wavenumber <= 2000 |
    (lib$wavenumber >= 2500 & lib$wavenumber <= 4000)
  expect_equal(built$wavenumber, lib$wavenumber[keep])
  expect_equal(built$spectra, lib$spectra[keep, , drop = FALSE])
  expect_error(build_lib(lib), "bare OpenSpecy")
  expect_error(
    build_lib(list(lib), restrict_range_args = list(c(100, 2000))),
    "named list"
  )
})

test_that("build_lib() converts each source before merging", {
  left <- filter_spec(tiny_build_lib(), 1:2)
  right <- filter_spec(tiny_build_lib(), 3:4)
  left$spectra[,] <- 50
  right$spectra[,] <- 0.5
  attr(left, "intensity_unit") <- "reflectance"
  attr(right, "intensity_unit") <- NULL
  right$metadata$intensity_units <- "transmittance"

  built <- build_lib(
    list(left, right),
    recipes = list(raw = list()),
    range = NULL,
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expected <- cbind(
    adj_intens(left$spectra, type = "reflectance", make_rel = FALSE),
    adj_intens(right$spectra, type = "transmittance", make_rel = FALSE)
  )
  colnames(expected) <- colnames(built$spectra)
  expect_equal(built$spectra, expected)
  expect_equal(attr(built, "intensity_unit"), "absorbance")
  expect_equal(built$metadata$intensity_units, rep("absorbance", 4))
})

test_that("metadata name helpers support smart and extensible matching", {
  expect_equal(
    lib_clean_name(c(" User Name ", "Laser (%)", "Method...3")),
    c("user_name", "laser_perc", "method_3")
  )

  name_lookup <- lib_metadata_name_lookup(
    campaign_code = "campaign id",
    project_code = character(),
    review_note = character(),
    regex = list(instrument_mode = "^method_[0-9]+$")
  )
  expect_false(any(c("username", "samplename", "librarytype") %in%
                     name_lookup$source_name, na.rm = TRUE))

  metadata <- data.table::data.table(
    UserName = c("alias_a", NA),
    user_name = c(NA, "canonical_b"),
    ProjectCodes = c("p1", "p2"),
    `Review Notes` = c("check", "keep"),
    Campaign.ID = c("campaign_a", "campaign_b"),
    Method.42 = c("ftir", "raman")
  )
  cleaned <- lib_clean_metadata(metadata, name_lookup)

  expect_equal(cleaned$user_name, c("alias_a", "canonical_b"))
  expect_equal(cleaned$project_code, c("p1", "p2"))
  expect_equal(cleaned$campaign_code, c("campaign_a", "campaign_b"))
  expect_equal(cleaned$review_note, c("check", "keep"))
  expect_equal(cleaned$instrument_mode, c("ftir", "raman"))
  expect_false("campaign_id" %in% names(cleaned))

  strict_lookup <- lib_metadata_name_lookup(
    project_code = character(),
    defaults = FALSE,
    match_without_underscores = FALSE,
    match_singular_plural = FALSE
  )
  strict <- lib_clean_metadata(
    data.table::data.table(ProjectCodes = "p1"),
    strict_lookup
  )
  expect_named(strict, "projectcodes")
})

test_that("metadata regex lookup reports overlapping patterns", {
  name_lookup <- lib_metadata_name_lookup(
    defaults = FALSE,
    regex = list(
      campaign = "^campaign",
      identifier = "_id$"
    )
  )

  expect_error(
    lib_clean_metadata(
      data.table::data.table(Campaign.ID = "campaign_a"),
      name_lookup
    ),
    "Multiple metadata name regular expressions.*Campaign.ID"
  )
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

  name_lookup <- lib_metadata_name_lookup(project_code = "Campaign ID")
  lib$metadata[["Campaign.ID"]] <- rep("campaign_a", 8)

  built <- build_lib(
    list(lib),
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
    list(lib),
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

test_that("build_lib() applies baseline recipes across source-specific NA tails", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  lib$spectra[1:5, 1] <- NA_real_
  lib$spectra[57:61, 2] <- NA_real_

  built <- build_lib(
    list(lib),
    recipes = list(nobaseline = list(
      conform_spec = FALSE,
      smooth_intens = FALSE,
      subtr_baseline = TRUE,
      make_rel = TRUE
    )),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )$nobaseline
  expected <- manage_na(lib, fun = subtr_baseline)

  expect_equal(built$spectra, expected$spectra, tolerance = 1e-12)
  expect_equal(attr(built, "baseline"), "nobaseline")
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
  built <- build_lib(
    mini_files,
    recipes = list(raw = list()),
    metadata_lookups = lookup,
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )
  expect_true(check_OpenSpecy(built$raw))
  expect_true(all(c("material", "material_type") %in% names(built$raw$metadata)))
})
