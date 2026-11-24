library_only_workflow_path <- function(file) {
  roots <- c(
    testthat::test_path("..", ".."),
    Sys.getenv("GITHUB_WORKSPACE", unset = "")
  )
  candidates <- file.path(unique(roots[nzchar(roots)]), "workflows", "data", file)
  existing <- candidates[file.exists(candidates)]
  testthat::skip_if(
    length(existing) == 0L,
    "repository-only reference workflow tables are not installed"
  )
  existing[[1L]]
}

library_only_official_fixture <- function() {
  classes <- data.table::fread(
    library_only_workflow_path("classes_reference.csv")
  )
  hierarchy <- data.table::fread(
    library_only_workflow_path("material_hierarchy.csv")
  )
  mapped <- merge(
    classes[, .(spectrum_identity, material)], hierarchy,
    by = "material", all = FALSE
  )[!is.na(spectrum_identity) & nzchar(spectrum_identity)]
  mapped <- unique(mapped, by = "spectrum_identity")[1:2]
  stopifnot(nrow(mapped) == 2L)

  wavenumber <- seq(100, 6100, by = 100)
  base_a <- dnorm(seq(-3, 3, length.out = length(wavenumber)))
  base_b <- rev(cumsum(seq_along(wavenumber)) / sum(seq_along(wavenumber)))
  spectra <- cbind(
    base_a, base_a + 0.01, base_b, base_b + 0.01
  )
  colnames(spectra) <- paste0("library_only_", seq_len(ncol(spectra)))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      spectrum_identity = rep(mapped$spectrum_identity, length.out = 4L),
      organization = "library-only fixture",
      user_name = "library-only fixture",
      spectrum_type = "ftir",
      intensity_units = "absorbance",
      origin = "experimental"
    ),
    attributes = list(intensity_unit = "absorbance")
  )
}

test_that("build_lib() library scope stops before downstream artifacts", {
  lib <- library_only_official_fixture()
  output_dir <- file.path(
    tempdir(), paste0("library-only-", sample.int(1e8, 1))
  )
  result <- suppressWarnings(build_lib(
    lib,
    recipes = list(raw = list()),
    output_dir = output_dir,
    artifact_scope = "libraries",
    previous_library_dir = file.path(output_dir, "must-not-be-read"),
    reuse = TRUE,
    dedupe = FALSE,
    signal_noise = FALSE,
    prune = list(),
    progress = FALSE,
    workflow_data = dirname(library_only_workflow_path(
      "classes_reference.csv"
    ))
  ))

  expect_s3_class(result, "OpenSpecyLibraryOnlyReferenceBuild")
  expect_named(result, c(
    "schema", "libraries", "assessments", "quarantine", "release_index"
  ))
  expect_identical(result$schema, "OpenSpecy_reference_libraries_only_v1")
  expect_named(result$libraries, "raw")
  expect_true(file.exists(result$libraries[["raw"]]))
  expect_false(any(c("medoids", "models") %in% names(result)))
  release_dir <- attr(result, "output_dir", exact = TRUE)
  release_files <- list.files(release_dir)
  expect_true(all(c(
    "raw.rds", "assessments.rds", "quarantined_spectra.rds",
    "reference_library_build.rds", "release_manifest.rds"
  ) %in% release_files))
  expect_false(any(grepl("medoid|model", release_files)))
  checkpoint_files <- list.files(file.path(output_dir, "checkpoints"))
  expect_false(any(grepl("medoid|model", checkpoint_files)))

  observed <- readRDS(result$libraries[["raw"]])
  expect_true(all(vapply(observed, check_OpenSpecy, logical(1L))))
  checkpoint <- readRDS(file.path(output_dir, "checkpoints", "libraries.rds"))
  expected <- OpenSpecy:::.lib_finalize_reference_metadata(
    checkpoint, list()
  )$libraries$raw
  expected <- lapply(expected, OpenSpecy:::.lib_slim_reference_object)
  expect_identical(observed, expected)
  release_index <- readRDS(result$release_index)
  expect_identical(
    release_index$schema, "OpenSpecy_reference_build_index_v1"
  )
  expect_true("raw" %in% names(release_index$artifacts))
  expect_false(any(grepl(
    "medoid|model", names(release_index$artifacts)
  )))
  expect_true(OpenSpecy:::.lib_validate_quarantine(result$quarantine))

  reused <- suppressWarnings(build_lib(
    lib,
    recipes = list(raw = list()),
    output_dir = output_dir,
    artifact_scope = "libraries",
    previous_library_dir = file.path(output_dir, "must-not-be-read"),
    reuse = TRUE,
    dedupe = FALSE,
    signal_noise = FALSE,
    prune = list(),
    progress = FALSE,
    workflow_data = dirname(library_only_workflow_path(
      "classes_reference.csv"
    ))
  ))
  expect_identical(reused, result)
})

test_that("library scope completes a forced file-backed build without models", {
  skip_if_not_installed("hdf5r")
  lib <- library_only_official_fixture()
  input_path <- tempfile("library-only-source-", fileext = ".rds")
  saveRDS(lib, input_path)
  output_dir <- file.path(
    tempdir(), paste0("library-only-staged-", sample.int(1e8, 1))
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
    OpenSpecy.recipe_chunk_size = 2L,
    OpenSpecy.staged_completion_threshold = 1L
  )
  on.exit(options(old_options), add = TRUE)

  result <- suppressWarnings(build_lib(
    input_path,
    recipes = list(raw = list()),
    output_dir = output_dir,
    artifact_scope = "libraries",
    previous_library_dir = NULL,
    reuse = FALSE,
    dedupe = FALSE,
    signal_noise = FALSE,
    prune = list(),
    progress = FALSE,
    workflow_data = dirname(library_only_workflow_path(
      "classes_reference.csv"
    ))
  ))

  expect_s3_class(result, "OpenSpecyLibraryOnlyReferenceBuild")
  expect_named(result$libraries, "raw")
  expect_true(file.exists(result$libraries[["raw"]]))
  expect_false(any(grepl(
    "medoid|model", list.files(output_dir, recursive = TRUE)
  )))
  observed <- readRDS(result$libraries[["raw"]])
  expect_true(all(vapply(observed, check_OpenSpecy, logical(1L))))
  expect_identical(
    result$quarantine$manifest$schema,
    "OpenSpecy_quarantined_spectra_v3"
  )
  expect_true(OpenSpecy:::.lib_validate_quarantine(result$quarantine))
})

test_that("reference workflow tables preserve UTF-8 class keys", {
  workflow_data <- tempfile("utf8-workflow-data-")
  dir.create(workflow_data)
  expected_key <- paste0("Iriodin", intToUtf8(174L), " Chroma Green")
  data.table::fwrite(
    data.table::data.table(
      spectrum_identity = expected_key,
      material = "pigment"
    ),
    file.path(workflow_data, "classes_reference.csv")
  )
  remaining_files <- c(
    "classes_regex.csv", "library_types.csv", "material_hierarchy.csv",
    "material_group_reference.csv", "known_bad_ids.csv",
    "metadata_drop_columns.csv", "material_form_regex.csv",
    "common_use_reference.csv"
  )
  for (file in remaining_files) {
    data.table::fwrite(
      data.table::data.table(fixture = character()),
      file.path(workflow_data, file)
    )
  }
  tables <- OpenSpecy:::.lib_reference_tables(workflow_data)
  key <- tables$classes_reference[
    grepl("iriodin.*chroma green", spectrum_identity, ignore.case = TRUE),
    spectrum_identity
  ]

  expect_length(key, 1L)
  expect_identical(key, expected_key)
  expect_identical(Encoding(key), "UTF-8")
  expect_true(174L %in% utf8ToInt(key))
})
