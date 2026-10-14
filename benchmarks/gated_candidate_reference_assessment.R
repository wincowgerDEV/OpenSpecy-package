# Build a candidate-aligned complete-reference assessment for a released gated
# model. This script never fits or selects a model. It pins the serialized model
# and medoid hashes, identifies the exact training-reference spectra, and emits
# context that complements (but never replaces) source-held OOF diagnostics.
#
# Example:
#   Rscript benchmarks/gated_candidate_reference_assessment.R \
#     --model=C:/path/model_gated_hierarchical_logistic_regression_derivative.rds \
#     --medoid=C:/path/medoid_derivative.rds \
#     --expected-model-sha256=... --expected-medoid-sha256=... \
#     --output=C:/path/gated_candidate_reference_assessment.rds

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest", "devtools")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

runner_version <- "gated-candidate-reference-assessment-v1"

script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot resolve the benchmark script path")
  normalizePath(sub("^--file=", "", argument), winslash = "/", mustWork = TRUE)
}

normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(
    model = "", medoid = "", expected_model_sha256 = "",
    expected_medoid_sha256 = "", output = "", artifact = "derivative",
    technique = "ftir"
  )
  for (argument in args) {
    if (!grepl("^--[^=]+=", argument)) {
      stop("Arguments must use --name=value: ", argument)
    }
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", argument))
    if (!key %in% names(out)) stop("Unknown argument: --", key)
    out[[key]] <- sub("^--[^=]+=", "", argument)
  }
  required <- c(
    "model", "medoid", "expected_model_sha256",
    "expected_medoid_sha256", "output"
  )
  missing <- required[!nzchar(unlist(out[required], use.names = FALSE))]
  if (length(missing)) {
    stop("Missing required argument(s): --", paste(missing, collapse = ", --"))
  }
  out$model <- normalize_path(out$model)
  out$medoid <- normalize_path(out$medoid)
  out$output <- normalize_path(out$output, must_work = FALSE)
  out
}

atomic_save_rds <- function(value, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = directory)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3)
  if (file.exists(path) && !file.remove(path)) stop("Cannot replace: ", path)
  if (!file.rename(temporary, path)) stop("Cannot finalize: ", path)
  invisible(path)
}

atomic_write_csv <- function(value, path) {
  directory <- dirname(path)
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = directory)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  data.table::fwrite(value, temporary, quote = TRUE, na = "NA")
  if (file.exists(path) && !file.remove(path)) stop("Cannot replace: ", path)
  if (!file.rename(temporary, path)) stop("Cannot finalize: ", path)
  invisible(path)
}

cli <- parse_cli()
model_sha256 <- sha256_file(cli$model)
medoid_sha256 <- sha256_file(cli$medoid)
if (!identical(tolower(model_sha256), tolower(cli$expected_model_sha256))) {
  stop("Candidate model SHA-256 differs from --expected-model-sha256")
}
if (!identical(tolower(medoid_sha256), tolower(cli$expected_medoid_sha256))) {
  stop("Candidate medoid SHA-256 differs from --expected-medoid-sha256")
}

root <- dirname(dirname(script_path()))
devtools::load_all(root, quiet = TRUE)

model_collection <- readRDS(cli$model)
medoid_collection <- readRDS(cli$medoid)
if (!is.list(model_collection) || is_OpenSpecy(model_collection) ||
    !cli$technique %in% names(model_collection)) {
  stop("Candidate model collection lacks technique: ", cli$technique)
}
if (!is.list(medoid_collection) || is_OpenSpecy(medoid_collection) ||
    !cli$technique %in% names(medoid_collection) ||
    !is_OpenSpecy(medoid_collection[[cli$technique]])) {
  stop("Candidate medoid collection lacks OpenSpecy technique: ", cli$technique)
}
model <- model_collection[[cli$technique]]
problems <- OpenSpecy:::.lib_gated_model_problems(model, "assessment model")
if (length(problems)) {
  stop("Candidate model is incomplete: ", paste(problems, collapse = "; "))
}
query <- OpenSpecy:::.lib_restrict_model_range(
  medoid_collection[[cli$technique]], cli$technique
)
if (!identical(as.numeric(query$wavenumber), as.numeric(model$all_variables))) {
  stop("Candidate medoid and model predictor axes are not identical")
}
if (ncol(query$spectra) != nrow(query$metadata)) {
  stop("Candidate medoid spectra and metadata are not aligned")
}

tests <- OpenSpecy:::.lib_model_complete_test(
  model, query, cli$artifact, cli$technique,
  algorithm = "gated_hierarchical_logistic_regression", source = "new",
  provenance = "candidate_aligned_complete_dataset"
)
identification <- OpenSpecy:::.lib_identification_summary(tests)
accuracy <- OpenSpecy:::.lib_accuracy_review(identification)
confusion <- OpenSpecy:::.lib_confusion_review(
  OpenSpecy:::.lib_confusion_table(tests)
)
if (!"expected_class_pct" %in% names(confusion) && !nrow(confusion)) {
  confusion[, expected_class_pct := numeric()]
}
required_confusion_columns <- c(
  "algorithm", "artifact", "model", "technique", "source", "provenance",
  "expected_class", "predicted_class", "spectra",
  "expected_class_spectra", "expected_class_pct"
)
if (nrow(accuracy) != 1L ||
    !all(required_confusion_columns %in% names(confusion))) {
  stop("Candidate reference assessment is incomplete")
}

provenance <- list(
  schema = "OpenSpecy_gated_candidate_reference_assessment_v1",
  runner_version = runner_version,
  model_path = cli$model,
  model_sha256 = model_sha256,
  model_object_sha256 = digest::digest(model, algo = "sha256"),
  medoid_path = cli$medoid,
  medoid_sha256 = medoid_sha256,
  predictor_count = length(model$all_variables),
  observation_count = ncol(query$spectra),
  axis_sha256 = digest::digest(as.numeric(query$wavenumber), algo = "sha256"),
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
assessment <- list(
  model = list(accuracy = accuracy, confusion = confusion),
  evidence = list(complete_dataset_tests = tests)
)
attr(assessment, "candidate_provenance") <- provenance
atomic_save_rds(assessment, cli$output)

stem <- sub("[.]rds$", "", cli$output, ignore.case = TRUE)
atomic_write_csv(accuracy, paste0(stem, "_accuracy.csv"))
atomic_write_csv(confusion, paste0(stem, "_confusion.csv"))
atomic_write_csv(
  data.table::as.data.table(provenance), paste0(stem, "_provenance.csv")
)
message("CANDIDATE REFERENCE ASSESSMENT COMPLETE: ", cli$output)
