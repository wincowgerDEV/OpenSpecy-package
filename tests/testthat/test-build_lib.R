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

test_that("build_lib() uses legacy source-stage hashes for sample_name", {
  wavenumber <- seq(50, 5000, by = 5)
  spectra <- cbind(
    sin(wavenumber / 200) + 2,
    cos(wavenumber / 250) + 2
  )
  colnames(spectra) <- c("raw_a", "raw_b")
  lib <- as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(sample_name = colnames(spectra)),
    attributes = list(intensity_unit = "absorbance")
  )

  legacy_hash <- function(x, range = NULL, short_value = NULL) {
    x <- manage_na(x, type = "remove")
    spec <- conform_spec(x, range = range, res = 8)
    if (!is.null(short_value) && nrow(spec$spectra) < 3) {
      return(rep(short_value, ncol(x$spectra)))
    }
    spec <- smooth_intens(spec)
    vapply(seq_len(ncol(spec$spectra)), function(i) {
      digest::digest(
        list(as.integer(spec$wavenumber),
             as.integer(spec$spectra[, i] * 100)),
        algo = "md5"
      )
    }, FUN.VALUE = character(1))
  }

  expected <- legacy_hash(lib)
  expected_old <- legacy_hash(lib, range = c(100, 4000),
                              short_value = "new format")
  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = TRUE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw

  expect_equal(built$metadata$sample_name, expected)
  expect_equal(built$metadata$sample_name_old, expected_old)
  expect_equal(colnames(built$spectra), expected)

  excluded <- build_lib(
    lib,
    recipes = list(raw = list()),
    exclude_ids = expected_old[1],
    dedupe = TRUE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_equal(excluded$metadata$sample_name, expected[2])
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

  lib$metadata$intensity_units <- "transmittance"
  preserved <- build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_equal(preserved$spectra, lib$spectra)
  expect_equal(preserved$metadata$intensity_units, rep("transmittance", 8))
  expect_equal(attr(preserved, "intensity_unit"), "reflectance")
})

test_that("build_lib() accepts and restricts one OpenSpecy object", {
  lib <- tiny_build_lib()
  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      restrict_range_args = list(
        min = c(100, 2500),
        max = c(2000, 4000)
      ),
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "using one in-memory OpenSpecy source"
  )

  keep <- lib$wavenumber <= 2000 |
    (lib$wavenumber >= 2500 & lib$wavenumber <= 4000)
  expect_equal(built$wavenumber, lib$wavenumber[keep])
  expect_equal(built$spectra, lib$spectra[keep, , drop = FALSE])
  expect_silent(build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  ))
  expect_error(
    build_lib(list(lib), restrict_range_args = list(c(100, 2000))),
    "named list"
  )
})

test_that("build_lib() reads one or many OpenSpecy objects from each RDS", {
  left <- filter_spec(tiny_build_lib(), 1:2)
  right <- filter_spec(tiny_build_lib(), 3:4)
  single_file <- tempfile(fileext = ".rds")
  list_file <- tempfile(fileext = ".RDS")
  invalid_file <- tempfile(fileext = ".rds")
  saveRDS(left, single_file)
  saveRDS(list(left, right), list_file)
  saveRDS(list(left, "not OpenSpecy"), invalid_file)

  single <- build_lib(
    single_file,
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  combined <- build_lib(
    list_file,
    recipes = list(raw = list()),
    range = NULL,
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw

  expect_equal(single$spectra, left$spectra)
  expect_equal(ncol(combined$spectra), 4)
  expect_equal(combined$metadata$sample_name, paste0("s", 1:4))
  expect_error(
    build_lib(invalid_file, progress = FALSE),
    "File path 1 must contain one OpenSpecy object or a nonempty list"
  )
})

test_that("build_lib() bulk-prepares legacy same-axis source lists", {
  lib <- tiny_build_lib()
  sources <- split_spec(list(lib))
  sources <- lapply(sources, function(x) {
    x$spectra <- data.table::as.data.table(x$spectra)
    x
  })

  expect_silent(
    built <- build_lib(
      sources,
      recipes = list(raw = list()),
      range = NULL,
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    )$raw
  )

  expect_equal(built$wavenumber, lib$wavenumber)
  expect_equal(built$spectra, lib$spectra)
  expect_equal(built$metadata$sample_name, lib$metadata$sample_name)
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

  cleaned_values <- lib_clean_metadata(
    data.table::data.table(
      Organization = c(" Monterey Bay Aquarium Research Institute ", "NULL"),
      SpectrumType = c("Raman", " not available "),
      numeric_value = c(1, 2)
    ),
    clean_values = TRUE
  )
  expect_equal(
    cleaned_values$organization,
    c("monterey bay aquarium research institute", NA)
  )
  expect_equal(cleaned_values$spectrum_type, c("raman", NA))
  expect_equal(cleaned_values$numeric_value, c(1, 2))

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

test_that("build_lib() skips metadata lookups with no shared key", {
  lib <- tiny_build_lib()
  no_shared <- data.table::data.table(
    missing_key = "not_present",
    joined_value = "skipped"
  )
  no_overlap <- data.table::data.table(
    source = "Z",
    joined_value = "skipped"
  )
  output_overlap <- data.table::data.table(
    source = c("A", "B", "C"),
    library_type = c("polymers", "polymers", "paints"),
    spectrum_type = c("ftir", "raman", "ftir")
  )
  ambiguous <- data.table::data.table(
    source = "A",
    sample_name = "s1",
    joined_value = "ambiguous"
  )

  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = no_shared,
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "skipping metadata lookup 1/1"
  )
  expect_false("joined_value" %in% names(built$metadata))

  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = no_overlap,
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "no usable shared key values"
  )
  expect_false("joined_value" %in% names(built$metadata))

  lib$metadata$library_type <- c("polymers", "paints", "polymers", "paints",
                                "polymers", "paints", "polymers", "paints")
  lib$metadata$spectrum_type <- "Raman"
  bad_utf8 <- rawToChar(as.raw(0xff))
  Encoding(bad_utf8) <- "UTF-8"
  lib$metadata$library_type[1] <- bad_utf8
  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = output_overlap,
    dedupe = FALSE,
    signal_noise = FALSE,
    clean_metadata_values = TRUE,
    progress = FALSE
  )$raw
  expect_false(any(c("library_type.x", "library_type.y",
                     "spectrum_type.x", "spectrum_type.y") %in%
                     names(built$metadata)))
  expect_equal(built$metadata$library_type[1:3],
               c("polymers", "polymers", "paints"))
  expect_equal(built$metadata$spectrum_type[1:3],
               c("ftir", "raman", "ftir"))

  expect_error(
    build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = ambiguous,
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    ),
    "Candidate columns were"
  )
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
