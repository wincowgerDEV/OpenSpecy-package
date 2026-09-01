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
  lib <- tiny_build_lib()

  ids <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
                    return = "ids")
  expect_equal(length(ids), 4)

  reduced <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2)
  expect_true(check_OpenSpecy(reduced))
  expect_equal(ncol(reduced$spectra), 4)
})

test_that("reduce_lib() uses cluster PAM medoids and reports useful progress", {
  lib <- tiny_build_lib()
  ids <- .lib_ids(lib, "sample_name")
  reduction_obj <- lib
  reduction_obj$spectra <- .matrix_mean_replace(
    make_rel(lib$spectra, na.rm = TRUE)
  )
  groups <- do.call(paste, c(lib$metadata[, "material_class", with = FALSE],
                             sep = "_"))

  expected <- unlist(lapply(split(seq_along(groups), groups), function(idx) {
    if (length(idx) <= 2L) return(ids[idx])
    x <- filter_spec(reduction_obj, idx)
    cors <- cor_spec(x, x, compute = "optimized")
    cors[is.na(cors)] <- 0
    cors <- pmax(pmin(cors, 1), -1)
    diag(cors) <- 1
    distance <- stats::as.dist(1 - cors)
    ids[idx][cluster::pam(distance, k = 2, diss = TRUE, pamonce = 6)$id.med]
  }), use.names = FALSE)

  messages <- capture.output(
    actual <- reduce_lib(
      lib, group_cols = "material_class", k = 2, min_n = 2,
      return = "ids", progress = TRUE
    ),
    type = "message"
  )
  expect_equal(actual, expected)
  messages <- paste(messages, collapse = "\n")
  expect_match(messages, "spectra in .* groups")
  expect_match(messages, "PAM group 1/2 starting")
  expect_match(messages, "correlation complete")
  expect_match(messages, "PAM complete")
  expect_match(messages, "kept=4/8")

  expect_silent(
    reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
               return = "ids", progress = FALSE)
  )
  expect_error(reduce_lib(lib, progress = NA), "'progress'")
})

test_that("build_model_lib() returns the model library artifact structure", {
  skip_if_not_installed("glmnet")
  lib <- tiny_build_lib()

  model <- suppressWarnings(
    build_model_lib(lib, type_col = NULL, min_n = 2, nlambda = 3)
  )
  expect_named(model, c("model", "dimension_conversion", "tests",
                        "coefficients", "class_names", "class_num",
                        "observation_count", "variable_num",
                        "all_variables", "variables_in"))
  expect_true(all(c("factor_num", "name") %in%
                    names(model$dimension_conversion)))
  expect_true(all(c("spectrum_id", "technique", "expected_class",
                    "predicted_class", "correct", "score", "split",
                    "provenance") %in% names(model$tests)))
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

test_that("build_lib() requires explicit source inputs", {
  expect_error(
    build_lib(),
    "'x' must specify the source library file path"
  )
})

test_that("build_lib() converts metadata intensity units before recipes", {
  lib <- tiny_build_lib()
  lib$spectra <- matrix(
    rep(c(50, 25, 0.5, 0.25, 2), each = nrow(lib$spectra)),
    nrow = nrow(lib$spectra),
    dimnames = list(NULL, paste0("u", 1:5))
  )
  lib$spectra[1, 2] <- NA_real_
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
    adj_intens(
      original[, 2], type = "transmittance", make_rel = FALSE,
      na.rm = TRUE
    )
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

test_that("metadata harmonization coalesces reviewed aliases only", {
  metadata <- data.table::data.table(
    spectrum_identity = c("canonical", NA),
    interpretation = c("ignored", "interpreted"),
    form_factor = c("film", NA),
    shape = c(NA, "fiber"),
    datatype = c("absorbance", "raman shift"),
    xunits = c("cm-1", NA),
    x_unit = c(NA, "1/cm"),
    spectrumid = c("a", "b"),
    locationdescription = c("left", "right"),
    name = c("source name", "source name 2"),
    names = c("other meaning", "other meaning 2"),
    file = c("raw path", "raw path 2"),
    file_name = c("display", "display 2"),
    sample = c("source sample", "source sample 2"),
    sample_name = c("stable-a", "stable-b")
  )

  cleaned <- lib_clean_metadata(metadata)

  expect_equal(cleaned$spectrum_identity, c("canonical", "interpreted"))
  expect_equal(cleaned$material_form, c("film", "fiber"))
  expect_equal(cleaned$data_type, c("absorbance", "raman shift"))
  expect_equal(cleaned$wavenumber_units, c("cm-1", "1/cm"))
  expect_equal(cleaned$spectrum_id, c("a", "b"))
  expect_equal(cleaned$location_description, c("left", "right"))
  expect_true(all(c("name", "names", "file", "file_name", "sample",
                    "sample_name") %in% names(cleaned)))
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

test_that("source metadata invalid bytes are normalized before merging", {
  invalid <- rawToChar(as.raw(c(0x66, 0x80, 0x6f)))
  Encoding(invalid) <- "unknown"
  expect_false(validUTF8(invalid))
  normalized <- OpenSpecy:::.lib_normalize_metadata_encoding(
    data.table::data.table(value = invalid)
  )
  expect_true(validUTF8(normalized$value))
  expect_equal(normalized$value, "fo")
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

test_that("build_lib() standardizes source keys before external lookups", {
  lib <- filter_spec(tiny_build_lib(), 1:3)
  lib$metadata$organization <- c("source org", NA, NA)
  lib$metadata$user_name <- c("known", "fallback user", "unmapped")
  lib$metadata$library_type <- c("metadata type", NA, NA)
  source_lookup <- data.table::data.table(
    organization = c("source org", "fallback user"),
    library_type = c("organization type", "user type"),
    spectrum_type = "raman"
  )

  built <- suppressWarnings(build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = list(
      lookup = source_lookup,
      by = "organization",
      fill_only = TRUE
    ),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw)

  expect_equal(built$metadata$library_type,
               c("metadata type", "user type", NA))
  expect_equal(built$metadata$spectrum_type, rep("ftir", 3))
  expect_equal(built$metadata$organization,
               c("source org", "fallback user", "unmapped"))
  expect_equal(
    attr(built, "metadata_lookup_reports")$canonical_source_keys[
      problem == "filled_canonical_key", n
    ],
    2L
  )
})

test_that("build_lib() cleans filename-derived spectrum identities and keys", {
  lib <- tiny_build_lib()
  lib$metadata$spectrum_identity <- c(
    "C:\\incoming\\Sample.CSV", "/tmp/Other.SPC",
    "relative/folder/Third.HDF5", "compound pe/pa/pe",
    "name.spc.csv", "plain identity", "opus.10", "unsupported.foo"
  )
  lookup <- data.table::data.table(
    spectrum_identity = c(
      "sample.csv", "other.spc", "third.hdf5", "compound pe/pa/pe",
      "name", "plain identity", "opus.10", "unsupported.foo"
    ),
    material = paste0("material_", seq_len(8))
  )

  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = list(lookup = lookup, by = "spectrum_identity"),
    dedupe = FALSE,
    signal_noise = FALSE,
    clean_metadata_values = TRUE,
    progress = FALSE
  )$raw

  expect_identical(
    built$metadata$spectrum_identity,
    c("sample", "other", "third", "compound pe/pa/pe", "name",
      "plain identity", "opus", "unsupported.foo")
  )
  expect_identical(built$metadata$material, lookup$material)
  report <- attr(built, "spectrum_identity_cleanup_report")
  expect_s3_class(report, "data.table")
  expect_equal(sum(report$n), 5L)
  expect_true(all(c("original", "spectrum_identity", "n") %in%
                    names(report)))
})

test_that("build_lib() rejects lookup collisions after identity cleanup", {
  lib <- tiny_build_lib()
  lib$metadata$spectrum_identity <- rep("same", 8)
  lookup <- data.table::data.table(
    spectrum_identity = c("same.csv", "same.spc"),
    material = c("first", "second")
  )

  expect_error(
    build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = list(lookup = lookup, by = "spectrum_identity"),
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    ),
    "Lookup keys must be unique"
  )
})

test_that("prune_lib() orders classes, preserves floors, and audits removals", {
  wn <- seq(500, 3500, length.out = 80)
  shape_a <- dnorm(seq(-3, 3, length.out = length(wn)))
  shape_b <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1)
  spectra <- cbind(
    shape_a, shape_a * 1.01, shape_a * 0.99, shape_a + 0.002,
    shape_b,
    shape_b * 1.01, shape_b * 0.99, shape_b + 0.002,
    rev(shape_a), rev(shape_a) * 1.01
  )
  colnames(spectra) <- paste0("id", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(rep("large", 5), rep("medium", 3), rep("small", 2)),
      material_type = "plastic",
      spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 2, return = "report", progress = FALSE)

  expect_true(check_OpenSpecy(report$object))
  expect_equal(report$schedule$material_class, c("large", "medium", "small"))
  expect_equal(report$schedule$initial_n, c(5L, 3L, 2L))
  expect_true(nrow(report$removals) >= 1)
  expect_equal(report$summary$reassigned, 0L)
  expect_true(all(table(report$object$metadata$material_class) >= 2))
  expect_identical(colnames(report$object$spectra),
                   report$object$metadata$sample_name)
  expect_identical(report$retained_ids, colnames(report$object$spectra))
})

test_that("prune_lib() retains unclassified spectra outside matching", {
  lib <- tiny_build_lib()
  lib$metadata$material_type <- "plastic"
  lib$metadata$material_class[1:2] <- "unclassified"
  protected_ids <- lib$metadata$sample_name[1:2]

  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_true(all(protected_ids %in% report$retained_ids))
  expect_false("unclassified" %in% report$schedule$material_class)
  expect_false(any(report$removals$prior_class == "unclassified"))
  expect_false(any(report$removals$matched_class == "unclassified"))
})

test_that("prune_lib() reassigns generic classes and tolerates no candidates", {
  lib <- filter_spec(tiny_build_lib(), 1:4)
  lib$metadata$material_class <- c("polymer a", "other plastic",
                                   "other material", "other plastic")
  lib$metadata$material_type <- c("plastic", "plastic", "mineral", "plastic")
  lib$metadata$spectrum_type <- c("ftir", "ftir", "raman", NA)
  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(report$object$metadata$material_class[2], "polymer a")
  expect_equal(report$object$metadata$material_class[3], "other material")
  expect_equal(report$object$metadata$material_class[4], "other plastic")
  expect_equal(nrow(report$reassignments), 1)
  expect_equal(report$summary$removed, 0L)
})

test_that("prune_lib() constrains generic reassignment and updates type", {
  wn <- seq(500, 3500, length.out = 80)
  plastic <- dnorm(seq(-3, 3, length.out = length(wn)))
  organic <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1)
  mineral <- rev(cumsum(seq_along(wn)))
  spectra <- cbind(plastic, organic, mineral, plastic, organic, mineral)
  colnames(spectra) <- paste0("generic", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(
        "polymer a", "organic matter", "mineral",
        "other plastic", "other material", "other"
      ),
      material_type = c(
        "plastic", "not plastic", "not plastic", "plastic", "not plastic",
        "other"
      ),
      spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(
    report$object$metadata$material_class[4:6],
    c("polymer a", "organic matter", "mineral")
  )
  expect_equal(
    report$object$metadata$material_type[4:6],
    c("plastic", "not plastic", "not plastic")
  )
  expect_equal(nrow(report$reassignments), 3L)
  expect_true(all(report$reassignments$reason == "nearest_eligible_class"))
})

test_that("prune_lib() preserves non-generic class labels and empty audits", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  lib$metadata$material_class <- c("Polymer A", "Polymer A")
  lib$metadata$material_type <- "plastic"
  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(report$object$metadata$material_class,
               c("Polymer A", "Polymer A"))
  expect_s3_class(report$reassignments, "data.table")
  expect_s3_class(report$removals, "data.table")
  expect_equal(nrow(report$reassignments), 0)
  expect_equal(nrow(report$removals), 0)
  expect_equal(report$summary$reassigned, 0L)
  expect_equal(report$summary$removed, 0L)
})

test_that("build_lib() applies pruning only to named recipes", {
  lib <- tiny_build_lib()
  lib$metadata$material_type <- "plastic"
  built <- build_lib(
    lib,
    recipes = list(raw = list(), processed = list()),
    prune = list(processed = list(min_n = 1, progress = FALSE)),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )

  expect_null(attr(built$raw, "prune_report"))
  expect_true(is.list(attr(built$processed, "prune_report")))
})

test_that("reference workflow tables encode reviewed taxonomy and source rules", {
  data_path <- function(file) {
    roots <- c(
      testthat::test_path("..", ".."),
      Sys.getenv("GITHUB_WORKSPACE", unset = "")
    )
    roots <- unique(roots[nzchar(roots)])
    candidates <- file.path(roots, "workflows", "data", file)
    existing <- candidates[file.exists(candidates)]
    testthat::skip_if(
      length(existing) == 0L,
      "repository-only reference workflow tables are not installed"
    )
    existing[[1L]]
  }
  classes <- data.table::fread(data_path("classes_reference.csv"))
  regex_classes <- data.table::fread(data_path("classes_regex.csv"))
  hierarchy <- data.table::fread(data_path("material_hierarchy.csv"))
  types <- data.table::fread(data_path("library_types.csv"))
  drops <- data.table::fread(data_path("metadata_drop_columns.csv"))

  expect_false(anyNA(classes$spectrum_identity))
  expect_false(any(classes$spectrum_identity == ""))
  expect_identical(anyDuplicated(classes$spectrum_identity), 0L)
  expect_false(any(grepl("^regex:", classes$spectrum_identity)))
  expect_named(regex_classes, c("pattern", "material"))
  expect_false(anyNA(regex_classes$pattern))
  expect_false(any(regex_classes$pattern == ""))
  expect_identical(anyDuplicated(regex_classes$pattern), 0L)
  class_audit <- predict_class_reference(
    classes, regex_classes, return = "report"
  )
  expect_gt(class_audit$summary$predicted, 0L)
  expect_equal(class_audit$summary$clashes, 0L)
  expect_gt(class_audit$summary$overlaps, 0L)
  expect_true(all(!is.na(regex_classes$material)))
  expect_identical(anyDuplicated(hierarchy$material), 0L)
  expect_equal(hierarchy[material == "other", material_class], "other")
  expect_equal(hierarchy[material == "other", material_type], "other")
  expect_equal(classes[spectrum_identity == "pa", material], "polyamides")
  exact_classes <- classes[!grepl("^regex:", spectrum_identity)]
  expect_identical(
    OpenSpecy:::.lib_clean_spectrum_identity(exact_classes$spectrum_identity),
    exact_classes$spectrum_identity
  )
  expect_true(all(
    hierarchy[grepl("adipate", material), material_class] == "polyesters"
  ))
  expect_false("polyamides (polylactams)" %in% hierarchy$material_class)
  expect_true(all(c("polyamides", "polyacrylamides") %in%
                    hierarchy$material_class))
  expect_equal(
    classes[spectrum_identity == "plc004_kn95 outer layer_pp", material],
    "poly(propylene)"
  )
  expect_equal(
    classes[spectrum_identity == "plc008_label tape_unknown", material],
    "other plastic"
  )
  organic_recommendations <- c(
    "1,5-pentanediol", "11-aminoundecanoic acid", "1-bromobutane",
    "1-vinyl-2-pyrolidinone", "2-bromopropanoic acid", "2-butanone",
    "2-chloro-4-methylpentane", "2-methyl-2-pentanol", "butyl acrylate",
    "ethyl (a-chloromethyl)acrylate", "ethyl acrylate",
    "n,n-diethyl-m-toluamide", "n,n-dimethyl-m-toluamide",
    "n-butyl benzoate", "n-decyl methacrylate", "n-vinylformamide",
    "p-phenylenediamine", "p-vinylbenzyl chloride", "p-xylene",
    "propionic acid", "sec-butyl benzoate", "styrene",
    "tamoxifen (lot #bcbt3163)"
  )
  expect_true(all(
    classes[spectrum_identity %in% organic_recommendations, material] ==
      "organic matter"
  ))
  expect_true(all(
    classes[spectrum_identity %in% c(
      "c2. blue fiber", "c4. green fiber", "c6. red fiber",
      "c8. pink fiber bundle", "c9. grey fiber"
    ), material] == "other"
  ))
  expect_equal(
    classes[spectrum_identity == "fibre_polyamide_6_p6", material],
    "nylon 6,6 - poly(hexamethylene adipamide)"
  )
  expect_equal(
    classes[spectrum_identity == "ps 16. purple lego fragment", material],
    "acrylonitrile butadiene styrene (abs)"
  )
  expect_true(all(
    classes[spectrum_identity %in% c("polyesterurethane", "polyetherurethane"),
            material] == "polyurethanes (isocyanates)"
  ))
  expect_true(all(
    classes[spectrum_identity %in% c("tylose", "tylose2"), material] ==
      "methyl cellulose"
  ))
  expect_true(all(c("microplastix", "nist", "hcmr", "cnr", "vliz",
                    "nicolas coca") %in% types$organization))
  expect_false("user_name" %in% names(types))
  expect_equal(types[organization == "nist", spectrum_type], "nir")
  expect_equal(
    types[organization == "monterey bay aquarium research institute",
          spectrum_type],
    "raman"
  )
  expect_false(anyNA(types$spectrum_type))
  expect_false(any(types$spectrum_type == ""))
  expect_true(all(c("interpretation", "form_factor", "shape", "x_unit",
                    "spectrumid", "locationdescription", "v1",
                    "3997_91411", "polymer_hit_3_labs") %in%
                  drops$metadata_column))
})

test_that("predict_class_reference() fills only blanks and audits overlaps", {
  classes <- data.table::data.table(
    spectrum_identity = c(
      "nylon exact override", "nylon fiber", "nylon blend", "unknown"
    ),
    material = c(
      "manual material", NA_character_, NA_character_, NA_character_
    )
  )
  rules <- data.table::data.table(
    pattern = c("^nylon", "blend$"),
    material = c("polyamides", "polyesters")
  )

  report <- predict_class_reference(classes, rules, return = "report")
  exact <- report$data

  expect_equal(exact[spectrum_identity == "nylon exact override", material],
               "manual material")
  expect_equal(exact[spectrum_identity == "nylon fiber", material],
               "polyamides")
  expect_true(is.na(exact[spectrum_identity == "nylon blend", material]))
  expect_true(is.na(exact[spectrum_identity == "unknown", material]))
  expect_equal(report$summary$predicted, 1L)
  expect_equal(report$summary$clashes, 1L)
  expect_equal(report$summary$unmatched, 2L)
  expect_equal(report$summary$overlaps, 1L)
  expect_equal(report$clashes$spectrum_identity, "nylon blend")
  expect_match(report$clashes$materials, "polyamides")
  expect_match(report$clashes$materials, "polyesters")
  expect_equal(report$overlaps$spectrum_identity, "nylon exact override")
  expect_false(report$overlaps$agreement)
  expect_equal(report$predictions$spectrum_identity, "nylon fiber")
})

test_that("reference class completion resolves reviewed wrappers and audits uncertainty", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    spectrum_identity = c(
      "known", "pa_ref.csv", "mffrc001_nylon (pa6)_maker.0",
      "cellulose_like_ref.csv", NA_character_, "known_2", "known_3",
      "known_4"
    ),
    user_name = c(
      NA_character_, "gicquel et al. 2024",
      "elise granek and kellie teague", "gicquel et al. 2024",
      NA_character_, NA_character_, NA_character_, NA_character_
    ),
    material = NA_character_,
    material_type = NA_character_
  )]
  lib$metadata$material_class <- c(
    "known class", rep(NA_character_, 3), rep("known class", 4)
  )
  classes <- data.table::data.table(
    spectrum_identity = c("pa", "nylon", "cellulose_like"),
    material = c("polyamides", "polyamides", "organic matter")
  )
  hierarchy <- data.table::data.table(
    material = c("nylon 6 - poly(caprolactam)", "organic matter"),
    material_class = c("polyamides", "organic matter"),
    material_type = c("plastic", "not plastic")
  )

  completed <- .lib_complete_reference_classes(lib, classes, hierarchy)
  report <- attr(completed, "class_coverage_report")

  expect_true(check_OpenSpecy(completed))
  expect_false(anyNA(completed$metadata$material_class))
  expect_equal(completed$metadata$material_class[2:3],
               rep("polyamides", 2))
  expect_equal(completed$metadata$class_lookup_key[2:3], c("pa", "nylon"))
  expect_equal(completed$metadata$class_assignment_reason[2:3],
               rep("reviewed_source_key", 2))
  expect_equal(completed$metadata$material_class[4], "organic matter")
  expect_equal(completed$metadata$class_assignment_reason[4],
               "reviewed_source_key")
  expect_identical(completed$metadata$spectrum_identity,
                   lib$metadata$spectrum_identity)
  expect_equal(report[stage == "after", populated_class], nrow(lib$metadata))
  expect_equal(report[stage == "after", reviewed_source_key], 3L)
  expect_equal(report[stage == "after", unclassified], 0L)
  expect_equal(report[stage == "after", other], 0L)
})

test_that("reference class completion labels and caps unresolved other", {
  source <- tiny_build_lib()
  ids <- paste0("coverage", seq_len(100L))
  spectra <- matrix(
    rep(source$spectra[, 1L], 100L), nrow = nrow(source$spectra)
  )
  colnames(spectra) <- ids
  metadata <- data.table::data.table(
    sample_name = ids, col_id = ids,
    spectrum_identity = c(rep("known", 99L), "unknown"),
    user_name = NA_character_,
    material = c(rep("known", 99L), NA_character_),
    material_class = c(rep("known class", 99L), NA_character_),
    material_type = c(rep("plastic", 99L), NA_character_),
    spectrum_type = "ftir"
  )
  lib <- as_OpenSpecy(source$wavenumber, spectra, metadata = metadata)
  classes <- data.table::data.table(
    spectrum_identity = character(), material = character()
  )
  hierarchy <- data.table::data.table(
    material = "known", material_class = "known class",
    material_type = "plastic"
  )

  completed <- .lib_complete_reference_classes(lib, classes, hierarchy)
  report <- attr(completed, "class_coverage_report")
  expect_equal(completed$metadata$material[100L], "other")
  expect_equal(completed$metadata$material_class[100L], "other")
  expect_equal(completed$metadata$material_type[100L], "other")
  expect_equal(report[stage == "after", unresolved_other], 1L)
  expect_equal(report[stage == "after", other_fraction], 0.01)

  lib$metadata[99:100, `:=`(
    spectrum_identity = c("unknown_2", "unknown"),
    material = NA_character_, material_class = NA_character_,
    material_type = NA_character_
  )]
  expect_error(
    .lib_complete_reference_classes(lib, classes, hierarchy),
    "above the reviewed 1% maximum",
    fixed = TRUE
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

test_that("source-stage hashes support spectra with no shared finite rows", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  midpoint <- floor(nrow(lib$spectra) / 2)
  lib$spectra[seq_len(midpoint), 1] <- NA_real_
  lib$spectra[seq.int(midpoint + 1L, nrow(lib$spectra)), 2] <- NA_real_

  built <- build_lib(
    lib, recipes = list(raw = list()), signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_true(check_OpenSpecy(built))
  expect_equal(ncol(built$spectra), 2L)
  expect_false(anyNA(built$metadata$sample_name))
  expect_identical(colnames(built$spectra), built$metadata$sample_name)
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

test_that("build_lib() discovers helper data and reuses one artifact bundle", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    spectrum_identity = label,
    organization = source,
    user_name = source,
    spectrum_id = sample_name
  )]
  workflow_root <- file.path(
    tempdir(), paste0("workflow-", sample.int(1e8, 1))
  )
  workflow_data <- file.path(workflow_root, "data")
  output_dir <- file.path(tempdir(), paste0("output-", sample.int(1e8, 1)))
  dir.create(workflow_data, recursive = TRUE)
  old_working_dir <- setwd(workflow_root)
  on.exit(setwd(old_working_dir), add = TRUE)
  data.table::fwrite(data.table::data.table(
    spectrum_identity = unique(lib$metadata$spectrum_identity),
    material = paste0("material_", seq_along(unique(
      lib$metadata$spectrum_identity
    )))
  ), file.path(workflow_data, "classes_reference.csv"))
  data.table::fwrite(data.table::data.table(
    pattern = "^never[0-9]+$", material = "other material"
  ), file.path(workflow_data, "classes_regex.csv"))
  data.table::fwrite(data.table::data.table(
    organization = c("a", "b", "c"), library_type = "test",
    spectrum_type = "ftir"
  ), file.path(workflow_data, "library_types.csv"))
  classes <- data.table::fread(
    file.path(workflow_data, "classes_reference.csv")
  )
  data.table::fwrite(data.table::data.table(
    material = classes$material,
    material_class = rep(c("class_a", "class_b"), length.out = nrow(classes)),
    material_type = "plastic"
  ), file.path(workflow_data, "material_hierarchy.csv"))
  data.table::fwrite(data.table::data.table(sample_name = character()),
                     file.path(workflow_data, "known_bad_ids.csv"))
  data.table::fwrite(data.table::data.table(
    metadata_column = "unused_legacy_column"
  ), file.path(workflow_data, "metadata_drop_columns.csv"))

  first <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE
  ))
  expect_named(first, c("libraries", "medoids", "models", "assessments"))
  expect_named(first$libraries, c("raw", "derivative", "nobaseline"))
  expect_named(first$medoids, c("derivative", "nobaseline"))
  expect_named(first$models, c("derivative", "nobaseline"))
  expect_true(all(vapply(first$libraries, check_OpenSpecy, logical(1))))
  expect_true(all(vapply(first$medoids, check_OpenSpecy, logical(1))))
  expect_true(all(c(
    "build_summary", "class_prediction", "class_coverage",
    "type_coverage", "pruning", "metadata_drop", "output_manifest"
  ) %in% names(first$assessments)))
  release_dir <- attr(first, "output_dir")
  expect_true(all(file.exists(file.path(
    release_dir,
    c("raw.rds", "derivative.rds", "nobaseline.rds",
      "medoid_derivative.rds", "medoid_nobaseline.rds",
      "model_derivative.rds", "model_nobaseline.rds",
      "reference_library_build.rds")
  ))))

  second <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE, reuse = TRUE
  ))
  expect_true(any(second$assessments$output_manifest$status == "reused"))
  expect_equal(second$libraries$raw$spectra, first$libraries$raw$spectra)

  rebuilt <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE, reuse = FALSE
  ))
  expect_false(any(rebuilt$assessments$output_manifest$status == "reused"))
})

test_that("combined reference splits prevent old-new identity leakage", {
  old <- tiny_build_lib()
  new <- old
  new$metadata$sample_name_old <- new$metadata$sample_name
  new$metadata$sample_name <- paste0("rebuilt_", new$metadata$sample_name)
  colnames(new$spectra) <- new$metadata$sample_name
  split <- OpenSpecy:::.lib_combined_split(
    new, old, artifact = "raw", seed = 71, holdout = 0.25
  )
  expect_identical(anyDuplicated(split$manifest$group_id), 0L)
  expect_true(all(split$manifest$new_present & split$manifest$old_present))
  expect_true(all(split$manifest$split %in% c("train", "test")))
  expect_length(intersect(
    split$manifest[split == "train", group_id],
    split$manifest[split == "test", group_id]
  ), 0L)

  messages <- capture.output(
    tests <- OpenSpecy:::.lib_reference_holdout_test(
      new, split, artifact = "raw", source = "new", progress = TRUE
    ),
    type = "message"
  )
  expect_match(paste(messages, collapse = "\n"), "full correlation complete")
  expect_true(all(tests$split == "test"))
  expect_true(all(tests$provenance == "reference_holdout"))
  test_ids <- new$metadata$sample_name_old[
    new$metadata$sample_name %in% tests$spectrum_id
  ]
  expect_false(any(test_ids %in%
                     split$manifest[split == "train", group_id]))
})

test_that("complete old-new assessments cover every artifact and held-out model", {
  skip_if_not_installed("glmnet")
  small <- tiny_build_lib()
  spectra <- do.call(cbind, lapply(seq_len(5), function(i) {
    small$spectra + i / 1000
  }))
  metadata <- data.table::rbindlist(lapply(seq_len(5), function(i) {
    out <- data.table::copy(small$metadata)
    out$sample_name <- paste0(out$sample_name, "_", i)
    out
  }))
  colnames(spectra) <- metadata$sample_name
  lib <- as_OpenSpecy(
    small$wavenumber, spectra = spectra, metadata = metadata,
    attributes = list(intensity_unit = "absorbance")
  )
  model_input <- restrict_range(
    lib, min = 800, max = 3200, make_rel = FALSE
  )
  model <- suppressWarnings(build_model_lib(model_input))
  model_set <- list(both = model, ftir = model, raman = NULL)
  build <- list(
    libraries = list(raw = lib, derivative = lib, nobaseline = lib),
    medoids = list(derivative = lib, nobaseline = lib),
    models = list(derivative = model_set, nobaseline = model_set),
    assessments = list()
  )
  previous <- file.path(tempdir(), paste0("previous-", sample.int(1e8, 1)))
  dir.create(previous, recursive = TRUE)
  saveRDS(lib, file.path(previous, "raw.rds"))
  saveRDS(lib, file.path(previous, "derivative.rds"))
  saveRDS(lib, file.path(previous, "nobaseline.rds"))
  saveRDS(lib, file.path(previous, "medoid_derivative.rds"))
  saveRDS(lib, file.path(previous, "medoid_nobaseline.rds"))
  saveRDS(model_set, file.path(previous, "model_derivative.rds"))
  saveRDS(model_set, file.path(previous, "model_nobaseline.rds"))

  comparison <- suppressWarnings(OpenSpecy:::.lib_compare_reference_build(
    build, previous_library_dir = previous,
    seed = 211, holdout = 0.25, progress = FALSE
  ))
  expect_true(all(c(
    "models", "split_manifest", "library_identification",
    "model_identification", "assess_spec_shifts", "old_new_compatibility"
  ) %in% names(comparison)))
  expect_equal(unique(comparison$split_manifest$artifact), c(
    "raw", "derivative", "nobaseline", "medoid_derivative",
    "medoid_nobaseline"
  ))
  expect_true(all(
    comparison$split_manifest$new_present &
      comparison$split_manifest$old_present
  ))
  expect_gt(nrow(comparison$library_identification), 0L)
  expect_gt(nrow(comparison$model_identification), 0L)
  expect_gt(nrow(comparison$assess_spec_shifts), 0L)
  expect_true(all(
    comparison$models$derivative$both$tests$provenance ==
      "production_model_split_reference"
  ))
})

test_that("reference regex table contains only genuinely variable rules", {
  expect_true(OpenSpecy:::.lib_regex_is_exact_literal(
    "^poly\\(amide\\)$"
  ))
  expect_false(OpenSpecy:::.lib_regex_is_exact_literal(
    "^olefin[[:space:]]*\\(pe(?:[[:space:]]|\\x2c|\\)|$)"
  ))
  expect_false(OpenSpecy:::.lib_regex_is_exact_literal(
    "^(?:cotton|wool|silk)$"
  ))
  regex_reference <- data.table::fread(
    file.path("..", "..", "workflows", "data", "classes_regex.csv")
  )
  expect_false(any(vapply(
    regex_reference$pattern,
    OpenSpecy:::.lib_regex_is_exact_literal,
    logical(1)
  )))
  exact <- data.table::fread(
    file.path("..", "..", "workflows", "data", "classes_reference.csv")
  )
  expect_true(all(c(
    "epoxide", "poly 1-butene isotactic", "poly 4-methyl-1-pentene",
    "poly(amide)", "poly(styrene)", "poly(vinylchloride)",
    "polyethylene glycol"
  ) %in% exact$spectrum_identity))
})
