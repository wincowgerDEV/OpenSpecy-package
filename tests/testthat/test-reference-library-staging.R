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
    helper_environment <- new.env(parent = environment())
    sys.source(existing[[1L]], envir = helper_environment)
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
    policy = list(medoids = FALSE, models = FALSE),
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
  withr::local_options(list(
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
  ))

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
