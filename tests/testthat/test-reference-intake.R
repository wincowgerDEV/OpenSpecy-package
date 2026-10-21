reference_workflow_path <- function(file) {
  path <- testthat::test_path("..", "..", "workflows", file)
  testthat::skip_if_not(
    file.exists(path),
    "repository workflow sources are not installed with the package"
  )
  path
}

reference_intake_environment <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(
    reference_workflow_path("reference_intake.R"),
    envir = environment
  )
  environment
}

test_that("reference intake validation enforces type, units, and visual review", {
  helpers <- reference_intake_environment()
  plot <- tempfile(fileext = ".png")
  file.create(plot)
  wavenumber <- seq(400, 800, by = 100)
  spectra <- cbind(a = 1:5, b = 5:1)
  object <- as_OpenSpecy(
    wavenumber, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra), spectrum_type = "ftir"
    )
  )
  object$metadata[, `:=`(
    spectrum_identity = paste0("identity_", seq_len(.N)),
    citation = "fixture", license = "CC BY",
    canonical_release = "fixture-release",
    source_declared_license = "CC BY", license_status = "verified_open",
    maintainer_use_basis = "source license", origin = "experimental",
    intensity_units = "transmittance", instrument_mode = NA_character_
  )]
  row <- data.table::data.table(
    slug = "fixture", expected_spectrum_type = "ftir",
    diagnostic_plot_path = plot, visual_review_status = "approved"
  )

  invalid <- helpers$reference_validate_cleaned_object(object, row)
  expect_false(invalid$valid)
  expect_match(invalid$problems, "transmission or ATR")

  object$metadata$instrument_mode <- "transmission"
  valid <- helpers$reference_validate_cleaned_object(object, row)
  expect_true(valid$valid)

  object$metadata$instrument_mode <- "atr"
  atr_valid <- helpers$reference_validate_cleaned_object(object, row)
  expect_true(atr_valid$valid)

  helpers$reference_write_diagnostic_plot(object, plot, sample_n = 2L)
  expect_gt(file.info(plot)$size, 0)
})

test_that("QMe14S-style chunk deduplication is stable across chunks", {
  helpers <- reference_intake_environment()
  target <- seq(400, 424, by = 6)
  deduper <- helpers$reference_new_hash_deduper(target, scale = 100)
  spectra <- cbind(a = 1:5, duplicate_a = 1:5, b = 5:1)

  first <- deduper$process(target, spectra, colnames(spectra))
  second <- deduper$process(target, spectra[, 3L, drop = FALSE], "b_again")

  expect_identical(first$ledger$action,
                   c("retained", "removed_duplicate", "retained"))
  expect_identical(ncol(first$spectra), 2L)
  expect_identical(second$ledger$action, "removed_duplicate")
  expect_identical(ncol(second$spectra), 0L)
  expect_identical(deduper$count(), 2L)
})

test_that("missing admitted outputs are source failures, not quarantine", {
  helpers <- reference_intake_environment()
  root <- tempfile("OpenSpecy-reference-intake-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  cleaner <- file.path(root, "clean_fixture.R")
  writeLines(
    "clean_reference_dataset <- function(dataset_dir, output_path) NULL",
    cleaner
  )
  manifest <- data.table::data.table(
    candidate_id = 1L, slug = "fixture", title = "Fixture",
    canonical_release = "fixture-release", dataset_dir = "fixture",
    cleaner = "clean_fixture.R",
    output = "fixture/Processed/CleanedOS_raw.rds", state = "admitted",
    source_declared_license = "CC BY", license_status = "verified_open",
    maintainer_use_basis = "source license", expected_spectrum_type = "ftir",
    citation = "fixture citation", origin = "experimental",
    diagnostic_plot = "fixture/Processed/diagnostic.png",
    visual_review_status = "approved", downloaded_at = "2026-10-19",
    raw_sha256 = "raw-fixture",
    cleaner_sha256 = digest::digest(
      file = cleaner, algo = "sha256", serialize = FALSE
    ),
    output_sha256 = "missing-output"
  )
  manifest_path <- file.path(root, "datasets.csv")
  data.table::fwrite(manifest, manifest_path)

  result <- helpers$reference_collect_admitted_inputs(root, manifest_path)

  expect_false(result$validation$valid)
  expect_true("missing_cleaned_output" %in% result$source_failures$reason)
  expect_length(result$files, 1L)
})

test_that("reference runner forwards an approved visual review to cleaners", {
  helpers <- reference_intake_environment()
  root <- tempfile("OpenSpecy-reference-cleaner-")
  dataset <- file.path(root, "fixture")
  processed <- file.path(dataset, "Processed")
  dir.create(processed, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cleaner <- file.path(root, "clean_fixture.R")
  writeLines(c(
    "clean_reference_dataset <- function(dataset_dir, output_path,",
    "                                    visual_review_status) {",
    "  writeLines(visual_review_status, file.path(dataset_dir, 'review.txt'))",
    "}"
  ), cleaner)
  spectra <- cbind(a = 1:5, b = 5:1)
  object <- as_OpenSpecy(
    seq(400, 800, by = 100), spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      spectrum_identity = c("a", "b"), spectrum_type = "raman",
      intensity_units = "arbitrary units", citation = "fixture",
      license = "CC BY", canonical_release = "fixture-release",
      source_declared_license = "CC BY", license_status = "verified_open",
      maintainer_use_basis = "source license", origin = "experimental"
    )
  )
  output <- file.path(processed, "CleanedOS_raw.rds")
  plot <- file.path(processed, "diagnostic.png")
  saveRDS(object, output)
  file.create(plot)
  manifest <- data.table::data.table(
    candidate_id = 1L, slug = "fixture", title = "Fixture",
    canonical_release = "fixture-release", dataset_dir = "fixture",
    cleaner = "clean_fixture.R",
    output = "fixture/Processed/CleanedOS_raw.rds", state = "admitted",
    source_declared_license = "CC BY", license_status = "verified_open",
    maintainer_use_basis = "source license", expected_spectrum_type = "raman",
    citation = "fixture", origin = "experimental",
    diagnostic_plot = "fixture/Processed/diagnostic.png",
    visual_review_status = "approved", downloaded_at = "2026-10-19",
    raw_sha256 = "raw-fixture",
    cleaner_sha256 = digest::digest(
      file = cleaner, algo = "sha256", serialize = FALSE
    ),
    output_sha256 = digest::digest(
      file = output, algo = "sha256", serialize = FALSE
    )
  )
  manifest_path <- file.path(root, "datasets.csv")
  data.table::fwrite(manifest, manifest_path)

  result <- helpers$reference_collect_admitted_inputs(
    root, manifest_path, run_cleaners = TRUE
  )

  expect_identical(readLines(file.path(dataset, "review.txt")), "approved")
  expect_true(result$validation$valid)
})

test_that("reference workflow uses manifest-selected cleaned outputs", {
  workflow <- readLines(
    reference_workflow_path("OpenSpecy_reference_library.R"),
    warn = FALSE
  )
  code <- paste(workflow, collapse = "\n")

  expect_match(code, "reference_collect_admitted_inputs", fixed = TRUE)
  expect_match(code, "reference_intake_validation.csv", fixed = TRUE)
  expect_match(code, "reference_intake_source_failures.csv", fixed = TRUE)
  expect_equal(
    length(gregexpr(
      "reference_library_build <- build_lib\\(", code, perl = TRUE
    )[[1L]]),
    1L
  )
})
