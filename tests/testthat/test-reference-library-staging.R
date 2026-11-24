reference_staging_helpers <- local({
  roots <- c(
    testthat::test_path("..", ".."),
    Sys.getenv("GITHUB_WORKSPACE", unset = "")
  )
  candidates <- file.path(
    unique(roots[nzchar(roots)]), "workflows", "reference_library_staging.R"
  )
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    NULL
  } else {
    staging_path <- normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
    workflow_root <- dirname(staging_path)
    helper_environment <- new.env(parent = environment())
    sys.source(staging_path, envir = helper_environment)
    patch_path <- file.path(
      workflow_root, "patch_reference_library_metadata.R"
    )
    if (file.exists(patch_path)) {
      sys.source(patch_path, envir = helper_environment)
    }
    attr(helper_environment, "workflow_root") <- workflow_root
    helper_environment
  }
})

staging_fixture <- function(prefix, wavenumber, offset = 0) {
  spectra <- cbind(
    seq_along(wavenumber) + offset,
    rev(seq_along(wavenumber)) + offset
  )
  colnames(spectra) <- paste0(prefix, "_", seq_len(ncol(spectra)))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      spectrum_identity = paste0("polyethylene_", seq_len(ncol(spectra))),
      organization = prefix,
      user_name = prefix,
      spectrum_type = "ftir",
      intensity_units = "absorbance",
      origin = "experimental"
    ),
    attributes = list(intensity_unit = "absorbance")
  )
}

staging_dataset_manifest <- function(root, slug, object) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  source <- file.path(root, "CleanedOS_raw.rds")
  saveRDS(object, source)
  canonical <- data.table::data.table(
    slug = slug,
    library_name = paste("Descriptive", toupper(slug), "Reference Library"),
    detailed_origin = if (slug == "b") "computed_molecule" else "experimental",
    spectrum_origin = if (slug == "b") "Computed" else "Observed",
    metadata_rule_version = "canonical_dataset_metadata_v1"
  )
  object <- reference_staging_helpers$reference_apply_dataset_metadata(
    object, dataset_metadata = canonical, source_slug = slug, block_size = 1L
  )$object
  artifact_rows <- lapply(
    c("raw", "derivative", "nobaseline"), function(recipe) {
      path <- file.path(root, paste0(recipe, "_ftir.rds"))
      saveRDS(object, path)
      data.table::data.table(
        recipe = recipe,
        spectrum_type = "ftir",
        spectra = ncol(object$spectra),
        wavenumbers = length(object$wavenumber),
        metadata_columns = ncol(object$metadata),
        path = normalizePath(path, winslash = "/", mustWork = TRUE),
        size = as.numeric(file.info(path)$size),
        checksum_algorithm = "sha256",
        checksum = reference_staging_helpers$.reference_sha256(path)
      )
    }
  )
  assessment <- file.path(root, "assessments.rds")
  quarantine <- file.path(root, "quarantined_spectra.rds")
  saveRDS(list(), assessment)
  saveRDS(list(), quarantine)
  list(
    schema = "OpenSpecy_dataset_library_manifest_v1",
    slug = slug,
    source = data.table::data.table(
      path = normalizePath(source, winslash = "/", mustWork = TRUE),
      size = as.numeric(file.info(source)$size),
      checksum_algorithm = "sha256",
      checksum = reference_staging_helpers$.reference_sha256(source)
    ),
    build_signature = paste0("fixture-", slug),
    policy = list(
      medoids = FALSE, models = FALSE,
      canonical_metadata = as.list(canonical)
    ),
    artifacts = data.table::rbindlist(artifact_rows),
    assessments = data.table::data.table(
      path = normalizePath(assessment, winslash = "/", mustWork = TRUE),
      size = as.numeric(file.info(assessment)$size),
      checksum_algorithm = "sha256",
      checksum = reference_staging_helpers$.reference_sha256(assessment)
    ),
    quarantine = data.table::data.table(
      path = normalizePath(quarantine, winslash = "/", mustWork = TRUE),
      removals = 0L,
      size = as.numeric(file.info(quarantine)$size),
      checksum_algorithm = "sha256",
      checksum = reference_staging_helpers$.reference_sha256(quarantine)
    )
  )
}

test_that("final staging join retains every dataset-local spectrum", {
  skip_if(is.null(reference_staging_helpers),
          "repository-only staging helpers are unavailable")
  skip_if_not_installed("hdf5r")
  root <- file.path(tempdir(), paste0("final-staging-", sample.int(1e8, 1)))
  first <- staging_fixture("dataset_a", seq(1000, 1060, by = 6))
  second <- staging_fixture("dataset_b", seq(1012, 1072, by = 6), offset = 20)
  colnames(second$spectra) <- colnames(first$spectra)
  second$metadata$sample_name <- colnames(first$spectra)
  manifests <- list(
    staging_dataset_manifest(file.path(root, "a"), "a", first),
    staging_dataset_manifest(file.path(root, "b"), "b", second)
  )
  old_options <- options(
    OpenSpecy.build_workers = 1L,
    OpenSpecy.source_hash_chunk_size = 2L,
    OpenSpecy.source_record_staging = TRUE,
    OpenSpecy.source_prefilter_staging = TRUE,
    OpenSpecy.compact_staged_metadata = TRUE,
    OpenSpecy.source_merge_file_backed = TRUE,
    OpenSpecy.source_merge_chunk_size = 2L,
    OpenSpecy.source_merge_gc_interval = 1L,
    OpenSpecy.metadata_join_chunk_size = 2L,
    OpenSpecy.metadata_join_report_value_limit = 2L,
    OpenSpecy.recipe_record_staging = TRUE,
    OpenSpecy.recipe_chunk_size = 2L
  )
  on.exit(options(old_options), add = TRUE)

  result <- reference_staging_helpers$reference_join_dataset_manifests(
    manifests, output_dir = file.path(root, "final"), progress = FALSE
  )

  expect_identical(result$schema, "OpenSpecy_final_reference_libraries_v1")
  expect_false(result$policy$final_join_dedupe)
  expect_false(result$policy$medoids)
  expect_false(result$policy$models)
  expect_named(
    stats::setNames(result$libraries$path, result$libraries$recipe),
    sort(c("derivative", "nobaseline", "raw")), ignore.order = TRUE
  )
  expect_true(all(file.exists(result$libraries$path)))
  expect_true(all(result$libraries$spectra == 4L))
  expect_true(all(result$typed_artifacts$cross_dataset_removals == 0L))
  expect_true(all(
    result$typed_artifacts$cross_dataset_id_collision_groups == 2L
  ))
  expect_true(all(
    result$typed_artifacts$cross_dataset_id_collision_rows == 4L
  ))
  expect_false(any(grepl(
    "medoid|model", list.files(file.path(root, "final"), recursive = TRUE)
  )))

  raw <- readRDS(result$libraries[recipe == "raw", path])
  expect_named(raw, "ftir")
  expect_true(check_OpenSpecy(raw$ftir))
  expect_identical(ncol(raw$ftir$spectra), 4L)
  expect_identical(raw$ftir$metadata$sample_name, colnames(raw$ftir$spectra))
  expect_identical(anyDuplicated(raw$ftir$metadata$sample_name), 0L)
  expect_identical(
    raw$ftir$metadata$source_dataset,
    rep(c("a", "b"), each = 2L)
  )
  expect_identical(
    raw$ftir$metadata$source_sample_name,
    rep(colnames(first$spectra), 2L)
  )
  expect_identical(
    raw$ftir$metadata$library_name,
    rep(c("Descriptive A Reference Library",
          "Descriptive B Reference Library"), each = 2L)
  )
  expect_identical(
    raw$ftir$metadata$spectrum_origin,
    rep(c("Observed", "Computed"), each = 2L)
  )
  expect_identical(
    raw$ftir$metadata$sample_name,
    paste(
      rep(c("a", "b"), each = 2L),
      rep(colnames(first$spectra), 2L),
      sep = "::"
    )
  )
  expect_identical(raw$ftir$wavenumber, seq(1002, 1068, by = 6))
  first_rows <- raw$ftir$wavenumber <= max(first$wavenumber)
  second_rows <- raw$ftir$wavenumber >= min(second$wavenumber)
  expected_first <- vapply(seq_len(ncol(first$spectra)), function(i) {
    stats::approx(
      first$wavenumber, first$spectra[, i],
      xout = raw$ftir$wavenumber[first_rows]
    )$y
  }, numeric(sum(first_rows)))
  expected_second <- vapply(seq_len(ncol(second$spectra)), function(i) {
    stats::approx(
      second$wavenumber, second$spectra[, i],
      xout = raw$ftir$wavenumber[second_rows]
    )$y
  }, numeric(sum(second_rows)))
  expect_equal(unname(raw$ftir$spectra[first_rows, 1:2]), expected_first,
               tolerance = 1e-12)
  expect_true(all(is.na(raw$ftir$spectra[!first_rows, 1:2])))
  expect_equal(unname(raw$ftir$spectra[second_rows, 3:4]), expected_second,
               tolerance = 1e-12)
  expect_true(all(is.na(raw$ftir$spectra[1:2, 3:4])))
})

test_that("canonical dataset metadata requires exact descriptive coverage", {
  skip_if(is.null(reference_staging_helpers),
          "repository-only staging helpers are unavailable")
  root <- file.path(tempdir(), paste0("metadata-map-", sample.int(1e8, 1)))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dictionary <- file.path(root, "dictionary.csv")
  data.table::fwrite(data.table::data.table(
    slug = c("observed", "computed"),
    library_name = c(
      "Observed Materials Reference Library",
      "Computed Molecules Reference Library"
    )
  ), dictionary)
  mapping <- reference_staging_helpers$reference_dataset_metadata(
    data.table::data.table(
      slug = c("observed", "computed"),
      origin = c("experimental", "computed_molecule")
    ),
    dictionary
  )
  expect_identical(mapping$spectrum_origin, c("Observed", "Computed"))
  expect_identical(mapping$library_name, c(
    "Observed Materials Reference Library",
    "Computed Molecules Reference Library"
  ))
  expect_error(
    reference_staging_helpers$reference_dataset_metadata(
      data.table::data.table(slug = "observed", origin = "experimental"),
      dictionary
    ),
    "coverage differs"
  )
})

test_that("metadata-only child releases preserve protected OpenSpecy content", {
  skip_if(is.null(reference_staging_helpers),
          "repository-only staging helpers are unavailable")
  root <- file.path(tempdir(), paste0("metadata-patch-", sample.int(1e8, 1)))
  parent_release <- file.path(root, "parent", "releases", "parent")
  dir.create(parent_release, recursive = TRUE, showWarnings = FALSE)
  object <- staging_fixture("parent", seq(1000, 1060, by = 6))
  object$metadata[, `:=`(
    source_dataset = c("observed", "computed"),
    library_name = c(NA_character_, "CMP"),
    retained_note = c("first", "second")
  )]
  original_spectra <- object$spectra
  original_wavenumber <- object$wavenumber
  original_ids <- object$metadata$sample_name
  original_note <- object$metadata$retained_note

  typed <- data.table::rbindlist(lapply(
    c("raw", "derivative", "nobaseline"), function(recipe) {
      path <- file.path(parent_release, paste0(recipe, "_ftir.rds"))
      saveRDS(object, path)
      data.table::data.table(
        recipe = recipe, spectrum_type = "ftir",
        spectra = ncol(object$spectra),
        wavenumbers = length(object$wavenumber),
        metadata_columns = ncol(object$metadata),
        path = normalizePath(path, winslash = "/", mustWork = TRUE),
        size = as.numeric(file.info(path)$size),
        checksum_algorithm = "sha256",
        checksum = reference_staging_helpers$.reference_sha256(path)
      )
    }
  ))
  bundles <- data.table::rbindlist(lapply(
    c("raw", "derivative", "nobaseline"), function(recipe) {
      path <- file.path(parent_release, paste0(recipe, ".rds"))
      saveRDS(list(ftir = object), path)
      data.table::data.table(
        recipe = recipe, spectra = ncol(object$spectra), types = 1L,
        path = normalizePath(path, winslash = "/", mustWork = TRUE),
        size = as.numeric(file.info(path)$size),
        checksum_algorithm = "sha256",
        checksum = reference_staging_helpers$.reference_sha256(path)
      )
    }
  ))
  parent_index <- list(
    schema = "OpenSpecy_final_reference_libraries_v1",
    build_signature = "parent-build",
    policy = list(medoids = FALSE, models = FALSE),
    sources = data.table::data.table(),
    typed_artifacts = typed, libraries = bundles,
    dataset_manifests = list()
  )
  index_path <- file.path(parent_release, "reference_library_build.rds")
  saveRDS(parent_index, index_path)
  pointer_path <- file.path(root, "parent", "current_reference_library.rds")
  saveRDS(list(
    schema = "OpenSpecy_final_reference_library_pointer_v1",
    build_signature = "parent-build",
    release_dir = normalizePath(
      parent_release, winslash = "/", mustWork = TRUE
    ),
    index_path = normalizePath(index_path, winslash = "/", mustWork = TRUE),
    index_checksum = reference_staging_helpers$.reference_sha256(index_path)
  ), pointer_path)

  manifest_path <- file.path(root, "datasets.csv")
  dictionary_path <- file.path(root, "dictionary.csv")
  data.table::fwrite(data.table::data.table(
    slug = c("observed", "computed"),
    origin = c("experimental", "computed_solid"),
    state = "admitted"
  ), manifest_path)
  data.table::fwrite(data.table::data.table(
    slug = c("observed", "computed"),
    library_name = c(
      "Observed Materials Reference Library",
      "Computed Minerals Reference Library"
    )
  ), dictionary_path)
  workflow_root <- attr(reference_staging_helpers, "workflow_root")
  child_root <- file.path(root, "child")
  contract <- reference_staging_helpers$reference_freeze_metadata_patch_contract(
      parent_pointer_path = pointer_path,
      admitted_manifest_path = manifest_path,
      dictionary_path = dictionary_path,
      output_dir = child_root,
      code_paths = file.path(workflow_root, c(
        "reference_library_staging.R",
        "patch_reference_library_metadata.R"
      )),
      metadata_block = 1L, spectral_block = 1L,
      minimum_free_disk_bytes = 0
    )
  child <- reference_staging_helpers$reference_patch_joined_library(
    contract, progress = FALSE
  )
  expect_identical(child$parent_build_signature, "parent-build")
  expect_true(child$policy$metadata_patch$enabled)
  expect_false(child$policy$metadata_patch$medoids)
  expect_false(child$policy$metadata_patch$models)
  expect_true(all(file.exists(child$typed_artifacts$path)))
  expect_true(all(file.exists(child$libraries$path)))
  expect_true(all(child$typed_artifacts$metadata_full_copy == FALSE))
  expect_true(all(child$typed_artifacts$changed_library_name == 2L))
  expect_true(all(child$typed_artifacts$changed_spectrum_origin == 2L))

  patched <- readRDS(child$typed_artifacts[recipe == "raw", path])
  expect_identical(patched$wavenumber, original_wavenumber)
  expect_identical(patched$spectra, original_spectra)
  expect_identical(patched$metadata$sample_name, original_ids)
  expect_identical(patched$metadata$retained_note, original_note)
  expect_identical(patched$metadata$library_name, c(
    "Observed Materials Reference Library",
    "Computed Minerals Reference Library"
  ))
  expect_identical(patched$metadata$spectrum_origin,
                   c("Observed", "Computed"))
  parent_after <- readRDS(typed[recipe == "raw", path])
  expect_true(is.na(parent_after$metadata$library_name[[1L]]))
  expect_false("spectrum_origin" %in% names(parent_after$metadata))

  reused <- reference_staging_helpers$reference_patch_joined_library(
    contract, progress = FALSE
  )
  expect_identical(reused$build_signature, child$build_signature)
  expect_false(any(grepl(
    "medoid|model", list.files(child_root, recursive = TRUE),
    ignore.case = TRUE
  )))
})
