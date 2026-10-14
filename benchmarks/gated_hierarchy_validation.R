# Validate a staged strict three-level gated FTIR model against frozen OS1
# and plan-047 positive-control evidence. This runner never fits a model.
#
# Examples:
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=help
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=check-baselines
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=fit \
#     --current-medoid=C:/path/to/rebuilt/medoid_derivative.rds \
#     --current-model=C:/path/to/released/gated_derivative_model.rds
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=probe
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=validate
#   Rscript benchmarks/gated_hierarchy_validation.R --stage=compare

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest", "glmnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

path_is_within <- function(path, parent) {
  path <- normalize_path(path, must_work = FALSE)
  parent <- sub("/+$", "", normalize_path(parent, must_work = FALSE))
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) {
    return(normalize_path("benchmarks/gated_hierarchy_validation.R"))
  }
  normalize_path(sub("^--file=", "", file_arg[[1L]]))
}

repo_root <- function() {
  root <- dirname(dirname(script_path()))
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not resolve the OpenSpecy package repository root")
  }
  root
}

legacy <- new.env(parent = globalenv())
sys.source(
  file.path(dirname(script_path()), "positive_control_medoid_model_validation.R"),
  envir = legacy
)

runner_version <- "gated-hierarchy-validation-v4"
candidate_key <- "gated_three_level"
fit_runtime_ceiling_seconds <- 3600
baseline_lock_sha256 <-
  "46a173f6c69dac09d5b2e9436acbd8eae80ca6973028cf4782aef344fd32c13a"
truth_file_sha256 <-
  "3ea4432cbadf27fc05f3ffc92770897dfdf7fa4e79a830469cf4f58d13515189"
truth_object_sha256 <-
  "f2ebfb3533de99b438a7fe11fffe22df96c7163317244789bce529ada0c31ce0"
map_input_manifest_sha256 <-
  "32e7cedd1408fa4e86033bd18d109778a3b996eaf02bdfe548929118368253f7"
excluded_samples <- c(
  "PMMA_15Nov223_control", "RedPETFibers_15Nov223_control"
)
polyethylene_truth_spec_id <- "poly(?:\\(ethylene\\)|olefin)"

is_polyethylene_truth <- function(value) {
  value <- trimws(as.character(value))
  !is.na(value) & value == polyethylene_truth_spec_id
}

usage <- function() {
  cat(paste0(
    "Strict gated hierarchy positive-control benchmark\n\n",
    "Stages:\n",
    "  help             Print this text; performs no fit or map run.\n",
    "  check-baselines  Verify frozen OS1/plan-047 hashes and Stage 1.\n",
    "  fit              Stage the supplied gated_three_level FTIR model.\n",
    "  probe            Run only three predeclared maps with the gated model.\n",
    "  validate         Run all 20 eligible maps with the gated model.\n",
    "  compare          Compare the completed gated run with frozen baselines.\n",
    "  all              Check, stage, probe, validate, then compare.\n\n",
    "Arguments use --name=value:\n",
    "  --current-medoid=PATH          Rebuilt medoid_derivative.rds.\n",
    "  --expected-medoid-sha256=HASH  Optional independent input pin.\n",
    "  --current-model=PATH           Released gated derivative model RDS.\n",
    "  --expected-model-sha256=HASH   Optional independent model pin.\n",
    "  --maps=all|probe|MAP,...       Used only by --stage=run.\n",
    "  --stage=run                    Run the requested gated maps only.\n",
    "  --workers=N                    Builder workers (default 5).\n",
    "  --baseline-root=PATH           Frozen hierarchical validation root.\n",
    "  --output-root=PATH             New gated validation root.\n"
  ))
  invisible(TRUE)
}

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (any(args %in% c("--help", "-h"))) args <- "--stage=help"
  out <- list(
    stage = "help",
    maps = "all",
    workers = "5",
    data_root = "C:/Users/winco/OneDrive/Documents/Positive_Controls",
    historical_root = "C:/Users/winco/OneDrive/Documents/OS1_Results",
    baseline_root = paste0(
      "C:/Users/winco/OneDrive/Documents/Positive_Controls/",
      "hierarchical_model_validation"
    ),
    current_medoid = Sys.getenv("OPENSPECY_GATED_MEDOID", unset = ""),
    expected_medoid_sha256 = "",
    current_model = Sys.getenv("OPENSPECY_GATED_MODEL", unset = ""),
    expected_model_sha256 = "",
    output_root = paste0(
      "C:/Users/winco/OneDrive/Documents/Positive_Controls/",
      "gated_hierarchy_validation"
    )
  )
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) {
      stop("Arguments must use --name=value: ", arg)
    }
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", arg))
    value <- sub("^--[^=]+=", "", arg)
    if (!key %in% names(out)) stop("Unknown argument: --", key)
    out[[key]] <- value
  }
  out$workers <- suppressWarnings(as.integer(out$workers))
  if (is.na(out$workers) || out$workers < 1L) {
    stop("--workers must be a positive integer")
  }
  stages <- c(
    "help", "check-baselines", "fit", "run", "probe", "validate",
    "compare", "all"
  )
  if (!out$stage %in% stages) stop("Unknown --stage=", out$stage)
  out
}

normalize_cli <- function(cli) {
  cli$data_root <- normalize_path(cli$data_root)
  cli$historical_root <- normalize_path(cli$historical_root)
  cli$baseline_root <- normalize_path(cli$baseline_root)
  cli$output_root <- normalize_path(cli$output_root, must_work = FALSE)
  if (nzchar(cli$current_medoid)) {
    cli$current_medoid <- normalize_path(cli$current_medoid)
  }
  if (nzchar(cli$current_model)) {
    cli$current_model <- normalize_path(cli$current_model)
  }
  forbidden_roots <- c(cli$historical_root, cli$baseline_root)
  if (nzchar(cli$current_medoid)) {
    forbidden_roots <- c(forbidden_roots, dirname(cli$current_medoid))
  }
  if (nzchar(cli$current_model)) {
    forbidden_roots <- c(forbidden_roots, dirname(cli$current_model))
  }
  if (identical(cli$output_root, cli$data_root) || any(vapply(
      unique(forbidden_roots),
      function(root) path_is_within(cli$output_root, root),
      logical(1L)
  ))) {
    stop(
      "--output-root must be separate from frozen inputs, baselines, and ",
      "release artifacts"
    )
  }
  cli
}

candidate_config <- function() {
  list(
    label = "Strict gated material type, polymer group, and class FTIR model",
    folder = "01_gated_three_level",
    hierarchy_col = c("material_type", "material_group"),
    selection_rule = "guardrailed"
  )
}

candidate_paths <- function(cli) {
  directory <- file.path(cli$output_root, candidate_config()$folder)
  list(
    directory = directory,
    full = file.path(directory, "model_full.rds"),
    deploy = file.path(directory, "model.rds"),
    manifest = file.path(directory, "model_fit_manifest.csv"),
    lambda = file.path(directory, "model_lambda_metrics.csv"),
    classes = file.path(directory, "model_class_metrics.csv"),
    hierarchy = file.path(directory, "model_hierarchy_metrics.csv"),
    source = file.path(directory, "model_source_metrics.csv"),
    nodes = file.path(directory, "model_node_support.csv"),
    folds = file.path(directory, "model_fold_assignments.csv"),
    routed = file.path(directory, "model_routed_oof_tests.csv"),
    vinyl = file.path(directory, "model_vinyl_family_oof.csv"),
    vinyl_summary = file.path(directory, "model_vinyl_family_oof_summary.csv"),
    vinyl_nodes = file.path(
      directory, "model_vinyl_family_source_holdout_nodes.csv"
    )
  )
}

candidate_diagnostic_names <- function() {
  c(
    "lambda", "classes", "hierarchy", "source", "nodes", "folds",
    "routed", "vinyl", "vinyl_summary", "vinyl_nodes"
  )
}

candidate_diagnostic_hashes <- function(paths) {
  names <- candidate_diagnostic_names()
  files <- unlist(paths[names], use.names = FALSE)
  if (any(!file.exists(files))) return(NULL)
  hash_names <- ifelse(names == "source", "source_metrics", names)
  stats::setNames(
    vapply(files, legacy$engine$sha256_file, character(1L)),
    paste0(hash_names, "_sha256")
  )
}

candidate_diagnostic_hashes_match <- function(manifest, paths) {
  hashes <- candidate_diagnostic_hashes(paths)
  if (is.null(hashes) || nrow(manifest) != 1L ||
      !all(names(hashes) %in% names(manifest))) {
    return(FALSE)
  }
  all(vapply(names(hashes), function(field) {
    identical(as.character(manifest[[field]][[1L]]), hashes[[field]])
  }, logical(1L)))
}

baseline_definitions <- function() {
  list(
    os1_medoid = list(folder = "01_os1_medoid", family = "os1"),
    os1_model = list(folder = "02_os1_model", family = "os1"),
    flat_macro = list(folder = "03_flat_macro", family = "plan047"),
    flat_guardrailed = list(
      folder = "04_flat_guardrailed", family = "plan047"
    ),
    hierarchical_macro = list(
      folder = "05_hierarchical_macro", family = "plan047"
    ),
    hierarchical_guardrailed = list(
      folder = "06_hierarchical_guardrailed", family = "plan047"
    )
  )
}

artifact_definitions <- function(cli) {
  paths <- candidate_paths(cli)
  expected <- NA_character_
  if (file.exists(paths$manifest)) {
    manifest <- data.table::fread(paths$manifest)
    if (nrow(manifest) == 1L && "deploy_sha256" %in% names(manifest)) {
      expected <- manifest$deploy_sha256[[1L]]
    }
  }
  list(gated_three_level = list(
    family = "candidate", representation = "model",
    label = candidate_config()$label, folder = candidate_config()$folder,
    path = paths$deploy, expected_sha256 = expected
  ))
}

legacy$artifact_definitions <- artifact_definitions
legacy$runner_version <- runner_version

source_manifest <- function(root) {
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    script_path(),
    file.path(dirname(script_path()), c(
      "positive_control_medoid_model_validation.R",
      "positive_control_library_validation.R"
    ))
  )
  files <- sort(unique(normalize_path(files)))
  data.table::data.table(
    file = substring(files, nchar(root) + 2L),
    sha256 = vapply(files, legacy$engine$sha256_file, character(1L)),
    bytes = unname(file.info(files)$size)
  )
}

legacy$source_manifest <- source_manifest

source_hash <- function(manifest) {
  legacy$engine$sha256_object(list(
    source = manifest[, .(file, sha256, bytes)],
    runner_version = runner_version
  ))
}

legacy$source_hash <- source_hash

baseline_required_files <- function(cli) {
  shared <- c(
    "library_manifest.csv", "input_manifest.csv", "metrics_summary.csv",
    "metrics_by_sample.csv", "metrics_by_material_type.csv",
    "metrics_by_size_stratum.csv", "metrics_particle_trace.csv"
  )
  os1 <- data.table::CJ(
    folder = c("01_os1_medoid", "02_os1_model"),
    file = c(
      shared, "stage1_compatibility_gate.csv",
      "stage1_compatibility_by_sample.csv",
      "stage1_compatibility_by_material_type.csv",
      "stage1_compatibility_by_size_stratum.csv"
    )
  )
  plan047 <- data.table::CJ(
    folder = c(
      "03_flat_macro", "04_flat_guardrailed", "05_hierarchical_macro",
      "06_hierarchical_guardrailed"
    ),
    file = c(shared, "model.rds", "model_full.rds", "model_fit_manifest.csv")
  )
  out <- data.table::rbindlist(list(os1, plan047))
  out[, relative_path := paste(folder, file, sep = "/")]
  data.table::setorder(out, relative_path)
  out[, path := file.path(cli$baseline_root, relative_path)]
  out
}

baseline_inventory <- function(cli) {
  inventory <- baseline_required_files(cli)
  missing <- inventory[!file.exists(path), relative_path]
  if (length(missing)) {
    stop("Frozen baseline is incomplete: ", paste(missing, collapse = ", "))
  }
  inventory[, sha256 := vapply(
    path, legacy$engine$sha256_file, character(1L)
  )]
  inventory[, bytes := unname(file.info(path)$size)]
  payload <- paste(
    inventory$relative_path, inventory$sha256, inventory$bytes,
    sep = "|", collapse = "\n"
  )
  lock <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  if (!identical(lock, baseline_lock_sha256)) {
    stop(
      "Frozen OS1/plan-047 baseline hash mismatch. Expected ",
      baseline_lock_sha256, "; found ", lock
    )
  }
  inventory[, path := NULL]
  inventory
}

read_baseline <- function(cli, key, filename) {
  definitions <- baseline_definitions()
  if (!key %in% names(definitions)) stop("Unknown frozen baseline: ", key)
  path <- file.path(cli$baseline_root, definitions[[key]]$folder, filename)
  if (!file.exists(path)) stop("Missing frozen baseline input: ", path)
  data.table::fread(path)
}

canonical_input_manifest <- function(x) {
  required <- c("sample_id", "bytes", "sha256", "excluded")
  if (!all(required %in% names(x))) {
    stop("Input manifest lacks: ", paste(setdiff(required, names(x)), collapse = ", "))
  }
  out <- data.table::data.table(
    sample_id = as.character(x$sample_id),
    bytes = as.numeric(x$bytes),
    sha256 = tolower(as.character(x$sha256)),
    excluded = as.logical(x$excluded)
  )
  data.table::setorder(out, sample_id)
  out
}

assert_stage1_table <- function(path) {
  x <- data.table::fread(path)
  if (!"within_one_percentage_point" %in% names(x) || !nrow(x) ||
      anyNA(x$within_one_percentage_point) ||
      !all(x$within_one_percentage_point)) {
    stop("Stage 1 compatibility is not passing: ", path)
  }
  deltas <- grep("(^|_)delta_pp$", names(x), value = TRUE)
  for (column in deltas) {
    value <- x[[column]]
    if (any(is.finite(value) & abs(value) > 1)) {
      stop("Stage 1 exceeds +/-1.0 percentage point in ", path, ": ", column)
    }
  }
  x
}

assert_truth_and_inputs <- function(cli) {
  truth_path <- file.path(cli$historical_root, "true_values2.csv")
  if (!file.exists(truth_path)) stop("Missing frozen truth file: ", truth_path)
  actual_truth_file <- legacy$engine$sha256_file(truth_path)
  if (!identical(actual_truth_file, truth_file_sha256)) {
    stop("Truth-file SHA-256 mismatch; refusing holdout comparison")
  }
  truth <- legacy$engine$truth_table(cli)
  actual_truth_object <- legacy$engine$sha256_object(truth)
  if (!identical(actual_truth_object, truth_object_sha256)) {
    stop("Scored truth-table SHA-256 mismatch; refusing holdout comparison")
  }
  if (any(truth$Map %in% excluded_samples) || nrow(truth) != 20L) {
    stop("Truth cohort does not contain exactly 20 eligible maps after exclusions")
  }
  expected <- canonical_input_manifest(
    read_baseline(cli, "os1_medoid", "input_manifest.csv")
  )
  expected_hash <- legacy$engine$sha256_file(file.path(
    cli$baseline_root, "01_os1_medoid", "input_manifest.csv"
  ))
  if (!identical(expected_hash, map_input_manifest_sha256)) {
    stop("Frozen map-input manifest SHA-256 mismatch")
  }
  inputs <- legacy$engine$input_files(cli)
  actual <- canonical_input_manifest(data.table::data.table(
    sample_id = names(inputs), bytes = unname(file.info(inputs)$size),
    sha256 = vapply(inputs, legacy$engine$sha256_file, character(1L)),
    excluded = FALSE
  ))
  if (!identical(expected, actual)) {
    stop("Current positive-control maps differ from the frozen input manifest")
  }
  invisible(list(truth = truth, inputs = actual))
}

assert_artifact_manifests <- function(cli) {
  for (key in names(baseline_definitions())) {
    manifest <- read_baseline(cli, key, "library_manifest.csv")
    if (nrow(manifest) != 1L || !file.exists(manifest$path[[1L]])) {
      stop("Frozen artifact manifest is invalid for ", key)
    }
    actual <- legacy$engine$sha256_file(manifest$path[[1L]])
    if (!identical(actual, manifest$sha256[[1L]])) {
      stop("Frozen artifact SHA-256 mismatch for ", key)
    }
    if (identical(baseline_definitions()[[key]]$family, "plan047")) {
      fit <- read_baseline(cli, key, "model_fit_manifest.csv")
      if (!identical(actual, fit$deploy_sha256[[1L]])) {
        stop("Plan-047 deploy hash disagrees with its fit manifest: ", key)
      }
      full_path <- file.path(
        cli$baseline_root, baseline_definitions()[[key]]$folder,
        "model_full.rds"
      )
      if (!identical(
        legacy$engine$sha256_file(full_path), fit$full_sha256[[1L]]
      )) {
        stop("Plan-047 full-model hash disagrees with its fit manifest: ", key)
      }
    }
  }
  invisible(TRUE)
}

write_baseline_evidence <- function(cli, inventory, stage1) {
  directory <- file.path(cli$output_root, "comparison")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  inventory <- data.table::copy(inventory)
  data.table::set(
    inventory, j = "baseline_lock_sha256",
    value = rep(baseline_lock_sha256, nrow(inventory))
  )
  legacy$engine$write_csv(
    inventory, file.path(directory, "frozen_baseline_hashes.csv")
  )
  legacy$engine$write_csv(
    stage1, file.path(directory, "stage1_gate_summary.csv")
  )
}

assert_stage1_complete <- function(cli, write = TRUE) {
  inventory <- baseline_inventory(cli)
  checks <- list()
  index <- 0L
  for (key in c("os1_medoid", "os1_model")) {
    folder <- baseline_definitions()[[key]]$folder
    files <- c(
      overall = "stage1_compatibility_gate.csv",
      sample = "stage1_compatibility_by_sample.csv",
      material_type = "stage1_compatibility_by_material_type.csv",
      size_stratum = "stage1_compatibility_by_size_stratum.csv"
    )
    for (scope in names(files)) {
      index <- index + 1L
      x <- assert_stage1_table(file.path(
        cli$baseline_root, folder, files[[scope]]
      ))
      delta_columns <- grep("(^|_)delta_pp$", names(x), value = TRUE)
      if (!length(delta_columns)) {
        stop("Stage 1 table has no percentage-point delta: ", files[[scope]])
      }
      delta_values <- unlist(x[, ..delta_columns], use.names = FALSE)
      finite_delta <- is.finite(delta_values)
      if (!any(finite_delta)) {
        stop("Stage 1 table has no finite percentage-point delta: ",
             files[[scope]])
      }
      maximum <- max(abs(delta_values[finite_delta]))
      checks[[index]] <- data.table::data.table(
        artifact = key, scope = scope, rows = nrow(x),
        maximum_absolute_delta_pp = maximum,
        tolerance_pp = 1, pass = TRUE
      )
    }
  }
  stage1 <- data.table::rbindlist(checks)
  manifests <- data.table::rbindlist(lapply(
    names(baseline_definitions()),
    function(key) read_baseline(cli, key, "library_manifest.csv")
  ), fill = TRUE)
  if (data.table::uniqueN(manifests$workflow_sha256) != 1L) {
    stop("Frozen runs do not share one workflow/settings hash")
  }
  input_hashes <- vapply(names(baseline_definitions()), function(key) {
    path <- file.path(
      cli$baseline_root, baseline_definitions()[[key]]$folder,
      "input_manifest.csv"
    )
    legacy$engine$sha256_file(path)
  }, character(1L))
  if (any(input_hashes != map_input_manifest_sha256)) {
    stop("Frozen runs do not share the locked map-input manifest")
  }
  assert_truth_and_inputs(cli)
  assert_artifact_manifests(cli)
  if (write) write_baseline_evidence(cli, inventory, stage1)
  invisible(list(
    inventory = inventory, stage1 = stage1,
    workflow_sha256 = unique(manifests$workflow_sha256)
  ))
}

legacy$assert_stage1_unlocked <- function(cli, definition, hashes) {
  locked <- assert_stage1_complete(cli, write = TRUE)
  if (!identical(hashes$workflow_sha256, locked$workflow_sha256[[1L]])) {
    stop("Stage 2 settings differ from the frozen OS1/plan-047 workflow")
  }
  invisible(TRUE)
}

resolve_medoid_path <- function(cli, require = TRUE) {
  path <- cli$current_medoid
  manifest_path <- candidate_paths(cli)$manifest
  if (!nzchar(path) && file.exists(manifest_path)) {
    manifest <- data.table::fread(manifest_path)
    path <- manifest$input_path[[1L]]
  }
  if (!nzchar(path)) {
    if (require) {
      stop(
        "Supply --current-medoid=PATH to the newly rebuilt ",
        "medoid_derivative.rds"
      )
    }
    return("")
  }
  normalize_path(path)
}

load_current_medoid <- function(cli) {
  path <- resolve_medoid_path(cli)
  sha256 <- legacy$engine$sha256_file(path)
  if (nzchar(cli$expected_medoid_sha256) && !identical(
    tolower(sha256), tolower(cli$expected_medoid_sha256)
  )) {
    stop("Rebuilt medoid SHA-256 differs from --expected-medoid-sha256")
  }
  object <- readRDS(path)
  if (is.list(object) && "ftir" %in% names(object) && !is_OpenSpecy(object)) {
    object <- object$ftir
  }
  if (!is_OpenSpecy(object)) stop("Rebuilt medoid is not an OpenSpecy object")
  if ("spectrum_type" %in% names(object$metadata)) {
    object <- filter_spec(
      object,
      !is.na(object$metadata$spectrum_type) &
        object$metadata$spectrum_type == "ftir"
    )
  }
  required <- c("material_class", "material_type", "material_group")
  missing <- setdiff(required, names(object$metadata))
  if (length(missing)) {
    stop("Rebuilt medoid metadata lacks: ", paste(missing, collapse = ", "))
  }
  for (column in required) {
    value <- as.character(object$metadata[[column]])
    if (anyNA(value) || any(!nzchar(trimws(value)))) {
      stop("Rebuilt medoid has incomplete ", column)
    }
  }
  if (ncol(object$spectra) != nrow(object$metadata) ||
      nrow(object$spectra) != length(object$wavenumber)) {
    stop("Rebuilt medoid violates OpenSpecy alignment")
  }
  list(object = object, path = path, sha256 = sha256)
}

resolve_model_path <- function(cli, require = TRUE) {
  path <- cli$current_model
  manifest_path <- candidate_paths(cli)$manifest
  if (!nzchar(path) && file.exists(manifest_path)) {
    manifest <- data.table::fread(manifest_path)
    if ("release_model_path" %in% names(manifest)) {
      path <- manifest$release_model_path[[1L]]
    }
  }
  if (!nzchar(path)) {
    medoid_path <- resolve_medoid_path(cli, require = FALSE)
    inferred <- if (nzchar(medoid_path)) file.path(
      dirname(medoid_path),
      "model_gated_hierarchical_logistic_regression_derivative.rds"
    ) else ""
    if (nzchar(inferred) && file.exists(inferred)) path <- inferred
  }
  if (!nzchar(path)) {
    if (require) {
      stop(
        "Supply --current-model=PATH to the released gated derivative ",
        "model collection"
      )
    }
    return("")
  }
  normalize_path(path)
}

load_current_model <- function(cli) {
  path <- resolve_model_path(cli)
  sha256 <- legacy$engine$sha256_file(path)
  if (nzchar(cli$expected_model_sha256) && !identical(
    tolower(sha256), tolower(cli$expected_model_sha256)
  )) {
    stop("Released model SHA-256 differs from --expected-model-sha256")
  }
  collection <- readRDS(path)
  model <- if (identical(
    collection$model_schema, "OpenSpecy_gated_hierarchy_v1"
  )) {
    collection
  } else if (is.list(collection) && "ftir" %in% names(collection)) {
    collection$ftir
  } else {
    stop("Released model collection does not contain an FTIR model")
  }
  validate_fitted_model(model, require_diagnostics = FALSE)
  object_sha256 <- legacy$engine$sha256_object(model)
  full_checkpoint <- resolve_full_model_checkpoint(path)
  list(
    model = model, path = path, sha256 = sha256,
    object_sha256 = object_sha256,
    full_checkpoint_path = full_checkpoint,
    full_checkpoint_sha256 = legacy$engine$sha256_file(full_checkpoint)
  )
}

resolve_full_model_checkpoint <- function(release_model_path) {
  release_directory <- dirname(release_model_path)
  build_root <- dirname(dirname(release_directory))
  path <- file.path(
    build_root, "checkpoints",
    paste0(
      "model_gated_hierarchical_logistic_regression_derivative_ftir_",
      "selection_guardrail_v1.rds"
    )
  )
  if (!file.exists(path)) {
    stop("Full released-model checkpoint is missing: ", path)
  }
  normalize_path(path)
}

fit_signature <- function(cli, medoid, release_model) {
  sources <- source_manifest(repo_root())
  legacy$engine$sha256_object(list(
    runner_version = runner_version,
    candidate = candidate_key,
    config = candidate_config(),
    medoid_sha256 = medoid$sha256,
    release_model_sha256 = release_model$sha256,
    release_model_object_sha256 = release_model$object_sha256,
    full_checkpoint_sha256 = release_model$full_checkpoint_sha256,
    source = sources[, .(file, sha256, bytes)],
    args = list(
      class_col = "material_class", type_col = "spectrum_type",
      min_n = 10L, alpha = 0.1, seed = 123L, grouped = TRUE,
      weights = TRUE, make_relative = TRUE,
      hierarchy_col = c("material_type", "material_group"),
      selection_rule = "guardrailed", method = "logistic_regression"
    )
  ))
}

write_diagnostic <- function(value, path) {
  if (is.null(value)) value <- data.table::data.table(status = "not_available")
  value <- data.table::as.data.table(value)
  if (!ncol(value)) value <- data.table::data.table(status = "not_available")
  legacy$engine$write_csv(value, path)
}

validate_fitted_model <- function(model, require_diagnostics = TRUE) {
  expected <- list(
    model_type = "gated_hierarchical_logistic_regression",
    model_schema = "OpenSpecy_gated_hierarchy_v1",
    probability_semantics = "terminal_conditional_probability",
    selection_rule = "guardrailed"
  )
  for (field in names(expected)) {
    if (!identical(model[[field]], expected[[field]])) {
      stop("Unexpected fitted model ", field, ": ", model[[field]] %||% "NULL")
    }
  }
  if (!identical(
    as.character(model$hierarchy_cols), c("material_type", "material_group")
  )) {
    stop("Fitted model does not use the requested ordered hierarchy")
  }
  if (!length(model$gated_nodes)) {
    stop("Fitted model did not retain gated runtime nodes")
  }
  if (!isTRUE(model$selected_lambda_converged)) {
    stop("Fitted model selected an unconverged lambda")
  }
  if (length(model$training_elapsed_seconds) != 1L ||
      !is.finite(model$training_elapsed_seconds) ||
      model$training_elapsed_seconds < 0) {
    stop("Fitted model did not retain a valid training runtime")
  }
  if (require_diagnostics &&
      (is.null(model$node_support) || !nrow(model$node_support))) {
    stop("Full fitted model did not retain gated node diagnostics")
  }
  invisible(TRUE)
}

assert_released_model_equivalence <- function(full, deploy, release_model) {
  validate_fitted_model(full)
  validate_fitted_model(deploy, require_diagnostics = FALSE)
  release_hash <- legacy$engine$sha256_object(release_model$model)
  deploy_hash <- legacy$engine$sha256_object(deploy)
  if (!identical(release_hash, release_model$object_sha256) ||
      !identical(deploy_hash, release_model$object_sha256)) {
    stop("Staged gated model is not the exact released FTIR model")
  }
  reslimmed <- OpenSpecy:::.lib_slim_model(full)
  reslimmed_hash <- legacy$engine$sha256_object(reslimmed)
  # data.table's non-serializable .internal.selfref pointer is recreated by
  # readRDS(), so base identical() can be false for byte-equivalent routing
  # tables. The stable serialized SHA-256 is the release identity contract.
  if (!identical(reslimmed_hash, release_model$object_sha256)) {
    stop("Released FTIR model differs from its complete training checkpoint")
  }
  invisible(list(
    release_object_sha256 = release_hash,
    deploy_object_sha256 = deploy_hash,
    reslimmed_object_sha256 = reslimmed_hash
  ))
}

untyped_model_label <- function(value) {
  value <- trimws(as.character(value))
  sub("^(ftir|raman|nir)_", "", value, ignore.case = TRUE)
}

vinyl_route_node_support <- function(model) {
  topology <- data.table::as.data.table(data.table::copy(model$gated_topology))
  support <- data.table::as.data.table(data.table::copy(model$node_support))
  required_topology <- c(
    "node_id", "parent_id", "incoming_label", "target_col"
  )
  required_support <- c(
    "node_id", "depth", "target_col", "node_role", "observations",
    "classes", "canonical_sources", "source_components", "source_eligible",
    "fold_mode", "folds"
  )
  if (!all(required_topology %in% names(topology)) ||
      !all(required_support %in% names(support))) {
    stop("Full gated model lacks vinyl-route node provenance")
  }
  if (anyDuplicated(topology$node_id) || anyDuplicated(support$node_id)) {
    stop("Full gated model has duplicate vinyl-route node provenance")
  }
  child_node <- function(parent, label, target) {
    value <- topology[
      parent_id == parent &
        untyped_model_label(incoming_label) == label &
        target_col == target,
      node_id
    ]
    if (length(value) != 1L) {
      stop("Gated model lacks one unambiguous ", label, " route node")
    }
    value[[1L]]
  }
  root <- as.character(model$root_node_id)
  if (length(root) != 1L || is.na(root) || !nzchar(root) ||
      !root %in% topology$node_id) {
    stop("Full gated model lacks one valid root node")
  }
  plastic <- child_node(root, "plastic", "material_group")
  vinyl <- child_node(
    plastic, "vinyl alcohol-acetate-acetal", "material_class"
  )
  route <- data.table::data.table(
    route_level = c("material_type", "material_group", "material_class"),
    node_id = c(root, plastic, vinyl)
  )
  route <- merge(
    route, support[, ..required_support], by = "node_id", all.x = TRUE,
    sort = FALSE
  )
  route[, route_order := match(route_level, c(
    "material_type", "material_group", "material_class"
  ))]
  data.table::setorder(route, route_order)
  route[, route_order := NULL]
  route[, source_held_out := fold_mode %in% c(
    "source_component_holdout", "deterministic"
  )]
  if (anyNA(route$fold_mode) || !all(route$source_held_out)) {
    failed <- route[is.na(source_held_out) | !source_held_out, .(
      route_level, node_id, fold_mode
    )]
    stop(
      "Vinyl-family routed OOF evidence is not source-held at: ",
      paste(failed$route_level, failed$fold_mode, sep = "=", collapse = ", ")
    )
  }
  route[]
}

vinyl_family_oof <- function(model) {
  tests <- data.table::as.data.table(data.table::copy(model$tests))
  folds <- data.table::as.data.table(data.table::copy(model$fold_assignments))
  required_tests <- c(
    "spectrum_id", "expected_class", "predicted_class", "correct",
    "split", "first_failed_gate", "gate_1_name", "gate_2_name"
  )
  required_folds <- c(
    "spectrum_id", "source_id", "source_component_id", "fold",
    "source_fold", "fold_mode", "source_fold_mode", "material_type",
    "material_group"
  )
  if (!all(required_tests %in% names(tests)) ||
      !all(required_folds %in% names(folds))) {
    stop("Full gated model lacks routed vinyl-family OOF evidence")
  }
  if (nrow(tests) != nrow(folds) || !identical(
    as.character(tests$spectrum_id), as.character(folds$spectrum_id)
  )) {
    stop("Vinyl-family tests and fold assignments are not row-aligned")
  }
  if (any(is.na(tests$split) | tests$split != "out_of_fold")) {
    stop("Vinyl-family evidence is not entirely out of fold")
  }
  route_support <- vinyl_route_node_support(model)
  detail <- cbind(
    tests[, ..required_tests],
    folds[, setdiff(required_folds, "spectrum_id"), with = FALSE]
  )
  data.table::setnames(
    detail, c("material_type", "material_group"),
    c("expected_material_type", "expected_material_group")
  )
  data.table::setnames(
    detail, c("gate_1_name", "gate_2_name"),
    c("predicted_material_type", "predicted_material_group")
  )
  detail[, `:=`(
    expected_material_type_taxonomy = untyped_model_label(
      expected_material_type
    ),
    expected_material_group_taxonomy = untyped_model_label(
      expected_material_group
    ),
    predicted_material_type_taxonomy = untyped_model_label(
      predicted_material_type
    ),
    predicted_material_group_taxonomy = untyped_model_label(
      predicted_material_group
    ),
    expected_class_taxonomy = untyped_model_label(expected_class),
    predicted_class_taxonomy = untyped_model_label(predicted_class)
  )]
  vinyl <- "vinyl alcohol-acetate-acetal"
  detail <- detail[
    expected_material_group_taxonomy == vinyl |
      predicted_material_group_taxonomy == vinyl
  ]
  if (!nrow(detail[expected_material_group_taxonomy == vinyl])) {
    stop("Gated model has no held-out vinyl-family reference evidence")
  }
  required_classes <- c(
    "polyethylene-vinyl acetate", "polyvinylalcohols", "polyvinylesters"
  )
  missing_classes <- setdiff(
    required_classes,
    unique(detail[
      expected_material_group_taxonomy == vinyl,
      expected_class_taxonomy
    ])
  )
  if (length(missing_classes)) {
    stop(
      "Source-held vinyl-family evidence lacks leaf class(es): ",
      paste(missing_classes, collapse = ", ")
    )
  }
  true_vinyl <- detail[expected_material_group_taxonomy == vinyl]
  incomplete_source <-
    is.na(true_vinyl$source_id) | !nzchar(trimws(true_vinyl$source_id)) |
    is.na(true_vinyl$source_component_id) |
    !nzchar(trimws(true_vinyl$source_component_id)) |
    !is.finite(true_vinyl$source_fold) |
    is.na(true_vinyl$source_fold_mode) |
    true_vinyl$source_fold_mode != "source_component_holdout"
  if (any(incomplete_source)) {
    stop("Vinyl-family OOF rows lack complete source-held fold provenance")
  }
  if (any(true_vinyl[, data.table::uniqueN(source_fold), by = source_id]$V1 !=
          1L) ||
      any(true_vinyl[, data.table::uniqueN(source_fold),
                     by = source_component_id]$V1 != 1L)) {
    stop("Vinyl-family sources or source components span held-out folds")
  }
  detail[, `:=`(
    route_fold_modes = paste(route_support$fold_mode, collapse = " -> "),
    source_held_route = all(route_support$source_held_out)
  )]
  detail[, route_role := data.table::fcase(
    expected_material_group_taxonomy == vinyl &
      predicted_material_group_taxonomy == vinyl,
    "retained_in_vinyl_group",
    expected_material_group_taxonomy == vinyl, "vinyl_group_false_negative",
    predicted_material_group_taxonomy == vinyl, "vinyl_group_false_positive"
  )]
  summary <- detail[, .(
    spectra = .N,
    leaf_accuracy = mean(correct, na.rm = TRUE),
    source_components = data.table::uniqueN(source_component_id)
  ), by = .(
    route_role, source_id, source_fold_mode,
    expected_material_group_taxonomy, expected_class_taxonomy,
    predicted_material_group_taxonomy, predicted_class_taxonomy,
    first_failed_gate
  )][order(route_role, -spectra, source_id)]
  list(detail = detail, summary = summary, node_support = route_support)
}

fit_candidate <- function(cli) {
  locked <- assert_stage1_complete(cli)
  legacy$engine$load_openspecy(repo_root())
  options(OpenSpecy.build_workers = cli$workers)
  medoid <- load_current_medoid(cli)
  release_model <- load_current_model(cli)
  paths <- candidate_paths(cli)
  dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
  signature <- fit_signature(cli, medoid, release_model)
  artifacts <- unlist(
    paths[setdiff(names(paths), "directory")], use.names = FALSE
  )
  if (any(file.exists(artifacts))) {
    if (!all(file.exists(artifacts))) {
      stop("Incomplete gated staging exists; move its candidate folder")
    }
    manifest <- data.table::fread(paths$manifest)
    reusable <- nrow(manifest) == 1L &&
      identical(manifest$fit_signature[[1L]], signature) &&
      identical(
        manifest$full_sha256[[1L]],
        legacy$engine$sha256_file(paths$full)
      ) && identical(
        manifest$deploy_sha256[[1L]],
        legacy$engine$sha256_file(paths$deploy)
      ) && candidate_diagnostic_hashes_match(manifest, paths)
    if (!reusable) {
      stop("Stale gated staging exists; move its candidate folder before restaging")
    }
    model <- readRDS(paths$full)
    deploy <- readRDS(paths$deploy)
    assert_released_model_equivalence(model, deploy, release_model)
    elapsed <- suppressWarnings(as.numeric(manifest$elapsed_seconds))
    if (length(elapsed) != 1L || !is.finite(elapsed) || elapsed < 0 ||
        elapsed >= fit_runtime_ceiling_seconds) {
      stop("Released gated training exceeded the one-hour runtime ceiling")
    }
    message("Reusing compatible staged gated_three_level model")
    return(invisible(manifest))
  }
  memory_before <- legacy$engine$system_memory()
  if (is.finite(memory_before$used_fraction) &&
      memory_before$used_fraction >= 0.80) {
    stop("System memory is already at or above 80% before model staging")
  }
  message(
    "Staging gated_three_level model for the frozen holdout: ",
    ncol(medoid$object$spectra), " spectra x ",
    nrow(medoid$object$spectra), " predictors"
  )
  full_checkpoint <- release_model$full_checkpoint_path
  model <- readRDS(full_checkpoint)
  deploy <- release_model$model
  model_hashes <- assert_released_model_equivalence(
    model, deploy, release_model
  )
  probe_audit_path <- file.path(
    dirname(release_model$path), "model_probe_audit.rds"
  )
  model_origin <- "staged_external_candidate"
  probe_audit_sha256 <- NA_character_
  if (file.exists(probe_audit_path)) {
    probe_audit <- tryCatch(readRDS(probe_audit_path), error = function(error) {
      stop("Candidate model-probe audit is unreadable: ", conditionMessage(error))
    })
    required_probe_fields <- c(
      "schema", "published_medoid_sha256", "full_checkpoint_file_sha256",
      "deploy_object_sha256", "predictors"
    )
    if (!is.list(probe_audit) ||
        !all(required_probe_fields %in% names(probe_audit)) ||
        !identical(
          probe_audit$schema, "OpenSpecy_CO2_FTIR_model_probe_audit_v1"
        ) || !identical(tolower(probe_audit$published_medoid_sha256),
                        tolower(medoid$sha256)) ||
        !identical(tolower(probe_audit$full_checkpoint_file_sha256),
                   tolower(release_model$full_checkpoint_sha256)) ||
        !identical(probe_audit$deploy_object_sha256,
                   release_model$object_sha256) ||
        !identical(as.integer(probe_audit$predictors), 363L)) {
      stop("Candidate model-probe audit does not bind the staged artifacts")
    }
    model_origin <- "axis_corrected_363bin_model_probe"
    probe_audit_path <- normalize_path(probe_audit_path)
    probe_audit_sha256 <- legacy$engine$sha256_file(probe_audit_path)
  } else {
    probe_audit_path <- NA_character_
  }
  elapsed <- as.numeric(model$training_elapsed_seconds)
  saveRDS(model, paths$full)
  saveRDS(deploy, paths$deploy)
  write_diagnostic(model$lambda_metrics, paths$lambda)
  write_diagnostic(model$class_metrics, paths$classes)
  write_diagnostic(model$hierarchy_metrics, paths$hierarchy)
  write_diagnostic(model$source_metrics, paths$source)
  write_diagnostic(model$node_support, paths$nodes)
  write_diagnostic(model$fold_assignments, paths$folds)
  write_diagnostic(model$tests, paths$routed)
  vinyl <- vinyl_family_oof(model)
  write_diagnostic(vinyl$detail, paths$vinyl)
  write_diagnostic(vinyl$summary, paths$vinyl_summary)
  write_diagnostic(vinyl$node_support, paths$vinyl_nodes)
  memory_after <- legacy$engine$system_memory()
  fold_modes <- sort(unique(as.character(model$node_support$fold_mode)))
  source_sha256 <- source_hash(source_manifest(repo_root()))
  manifest <- data.table::data.table(
    candidate = candidate_key, label = candidate_config()$label,
    model_type = model$model_type, model_schema = model$model_schema,
    hierarchy_cols = paste(model$hierarchy_cols, collapse = " -> "),
    selection_rule = model$selection_rule,
    selection_policy = model$selection_policy,
    probability_semantics = model$probability_semantics,
    accuracy_tolerance = model$selection_accuracy_tolerance,
    node_count = length(model$gated_nodes),
    deterministic_node_count = sum(vapply(
      model$gated_nodes, function(node) isTRUE(node$deterministic), logical(1L)
    )),
    fold_modes = paste(fold_modes, collapse = " | "),
    fit_signature = signature,
    model_origin = model_origin,
    model_probe_audit_path = probe_audit_path,
    model_probe_audit_sha256 = probe_audit_sha256,
    release_model_path = release_model$path,
    release_model_sha256 = release_model$sha256,
    release_model_object_sha256 = release_model$object_sha256,
    deploy_model_object_sha256 = model_hashes$deploy_object_sha256,
    reslimmed_model_object_sha256 = model_hashes$reslimmed_object_sha256,
    full_checkpoint_path = full_checkpoint,
    full_checkpoint_sha256 = release_model$full_checkpoint_sha256,
    input_path = medoid$path, input_sha256 = medoid$sha256,
    input_observations = ncol(medoid$object$spectra),
    input_predictors = nrow(medoid$object$spectra),
    truth_path = normalize_path(file.path(
      cli$historical_root, "true_values2.csv"
    )),
    truth_sha256 = truth_file_sha256,
    truth_object_sha256 = truth_object_sha256,
    map_input_manifest_sha256 = map_input_manifest_sha256,
    frozen_baseline_sha256 = baseline_lock_sha256,
    workflow_sha256 = locked$workflow_sha256[[1L]],
    source_sha256 = source_sha256,
    observations = model$observation_count,
    predictors = length(model$all_variables),
    classes = length(model$class_names), elapsed_seconds = elapsed,
    fit_runtime_ceiling_seconds = fit_runtime_ceiling_seconds,
    memory_used_fraction_before = memory_before$used_fraction,
    memory_used_fraction_after = memory_after$used_fraction,
    full_bytes = unname(file.info(paths$full)$size),
    deploy_bytes = unname(file.info(paths$deploy)$size),
    full_sha256 = legacy$engine$sha256_file(paths$full),
    deploy_sha256 = legacy$engine$sha256_file(paths$deploy),
    selected_lambda_converged = isTRUE(model$selected_lambda_converged),
    lambda_path_complete = isTRUE(model$lambda_path_complete),
    source_held_vinyl_spectra = vinyl$detail[
      expected_material_group_taxonomy == "vinyl alcohol-acetate-acetal", .N
    ],
    source_held_vinyl_components = vinyl$detail[
      expected_material_group_taxonomy == "vinyl alcohol-acetate-acetal",
      data.table::uniqueN(source_component_id)
    ],
    r_version = R.version.string,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  diagnostic_hashes <- candidate_diagnostic_hashes(paths)
  if (is.null(diagnostic_hashes)) {
    stop("Staged gated diagnostics are incomplete before manifest creation")
  }
  for (field in names(diagnostic_hashes)) {
    manifest[[field]] <- diagnostic_hashes[[field]]
  }
  legacy$engine$write_csv(manifest, paths$manifest)
  if (elapsed >= fit_runtime_ceiling_seconds) {
    stop(
      "Released gated training exceeded the one-hour ceiling: ",
      round(elapsed, 1), " seconds"
    )
  }
  if (is.finite(memory_after$used_fraction) &&
      memory_after$used_fraction >= 0.80) {
    stop("Gated model was staged but system memory reached 80%")
  }
  message(
    "Staged gated_three_level model trained in ",
    round(elapsed, 1), " seconds"
  )
  invisible(manifest)
}

require_fitted_candidate <- function(cli) {
  legacy$engine$load_openspecy(repo_root())
  paths <- candidate_paths(cli)
  required <- unlist(
    paths[setdiff(names(paths), "directory")], use.names = FALSE
  )
  if (!all(file.exists(required))) {
    stop("Stage gated_three_level before running positive-control maps")
  }
  manifest <- data.table::fread(paths$manifest)
  medoid_cli <- cli
  medoid_cli$current_medoid <- resolve_medoid_path(cli)
  medoid <- load_current_medoid(medoid_cli)
  model_cli <- cli
  model_cli$current_model <- resolve_model_path(cli)
  release_model <- load_current_model(model_cli)
  if (!identical(
        manifest$fit_signature[[1L]],
        fit_signature(cli, medoid, release_model)
      ) ||
      !identical(
        manifest$input_sha256[[1L]], medoid$sha256
      ) || !identical(
        manifest$full_sha256[[1L]], legacy$engine$sha256_file(paths$full)
      ) || !identical(
        manifest$deploy_sha256[[1L]], legacy$engine$sha256_file(paths$deploy)
      ) || !candidate_diagnostic_hashes_match(manifest, paths)) {
    stop("Gated staging no longer matches its source, input, or hashes")
  }
  assert_released_model_equivalence(
    readRDS(paths$full), readRDS(paths$deploy), release_model
  )
  invisible(manifest)
}

validate_map_runtimes <- function(runtime, expected_samples = NULL,
                                  context = "gated maps") {
  runtime <- data.table::as.data.table(runtime)
  required <- c("sample_id", "elapsed_seconds")
  if (!nrow(runtime) || !all(required %in% names(runtime))) {
    stop(context, " runtime evidence is missing or incomplete")
  }
  sample_id <- trimws(as.character(runtime$sample_id))
  elapsed <- suppressWarnings(as.numeric(runtime$elapsed_seconds))
  if (anyNA(sample_id) || any(!nzchar(sample_id)) || anyDuplicated(sample_id)) {
    stop(context, " runtime evidence has missing or duplicate map IDs")
  }
  if (any(!is.finite(elapsed)) || any(elapsed < 0)) {
    stop(context, " runtime evidence has invalid elapsed seconds")
  }
  if (!is.null(expected_samples)) {
    expected_samples <- as.character(expected_samples)
    if (anyNA(expected_samples) || any(!nzchar(expected_samples)) ||
        anyDuplicated(expected_samples) ||
        !setequal(sample_id, expected_samples)) {
      stop(context, " runtime evidence does not match the expected maps")
    }
  }
  if (any(elapsed >= 300)) {
    stop("At least one ", context, " exceeded the five-minute runtime ceiling")
  }
  runtime[, elapsed_seconds := elapsed][]
}

run_gated <- function(cli, maps = cli$maps) {
  require_fitted_candidate(cli)
  run_cli <- cli
  run_cli$artifact <- candidate_key
  run_cli$maps <- maps
  legacy$run_artifact(run_cli)
  inputs <- legacy$engine$input_files(run_cli)
  selected <- legacy$engine$selected_inputs(inputs, maps)
  invocation_path <- file.path(
    candidate_paths(cli)$directory, "runtime_invocation.csv"
  )
  invocation <- if (file.exists(invocation_path)) {
    data.table::fread(invocation_path)
  } else {
    data.table::data.table()
  }
  validate_map_runtimes(
    invocation, expected_samples = names(selected),
    context = "selected gated maps"
  )
  invisible(TRUE)
}

read_candidate <- function(cli, filename) {
  path <- file.path(candidate_paths(cli)$directory, filename)
  if (!file.exists(path)) stop("Missing gated comparison input: ", path)
  data.table::fread(path)
}

assert_candidate_compatible <- function(cli) {
  locked <- assert_stage1_complete(cli)
  require_fitted_candidate(cli)
  manifest <- read_candidate(cli, "library_manifest.csv")
  if (!identical(manifest$workflow_sha256[[1L]], locked$workflow_sha256[[1L]])) {
    stop("Gated maps do not use the frozen workflow/settings hash")
  }
  fit <- data.table::fread(candidate_paths(cli)$manifest)
  if (!identical(manifest$sha256[[1L]], fit$deploy_sha256[[1L]])) {
    stop("Gated map run did not use the staged released artifact")
  }
  input <- canonical_input_manifest(read_candidate(cli, "input_manifest.csv"))
  expected <- canonical_input_manifest(
    read_baseline(cli, "os1_medoid", "input_manifest.csv")
  )
  if (!identical(input, expected)) {
    stop("Gated maps do not use the frozen positive-control inputs")
  }
  by_sample <- read_candidate(cli, "metrics_by_sample.csv")
  if (nrow(by_sample) != 20L || anyDuplicated(by_sample$Map) ||
      data.table::uniqueN(by_sample$Map) != 20L) {
    stop("Comparison requires all 20 eligible gated maps")
  }
  runtimes <- list.files(
    candidate_paths(cli)$directory,
    pattern = "^runtime_.*[.]csv$", full.names = TRUE
  )
  runtimes <- runtimes[basename(runtimes) != "runtime_invocation.csv"]
  runtime <- data.table::rbindlist(lapply(runtimes, data.table::fread), fill = TRUE)
  runtime <- validate_map_runtimes(
    runtime, expected_samples = by_sample$Map, context = "completed gated maps"
  )
  fit <- data.table::fread(candidate_paths(cli)$manifest)
  if (nrow(fit) != 1L || !"elapsed_seconds" %in% names(fit) ||
      !is.finite(fit$elapsed_seconds[[1L]]) || fit$elapsed_seconds[[1L]] < 0 ||
      fit$elapsed_seconds[[1L]] >= fit_runtime_ceiling_seconds) {
    stop("Staged gated training exceeds the one-hour runtime ceiling")
  }
  invisible(list(manifest = manifest, runtime = runtime, fit = fit))
}

read_metric_set <- function(cli, filename) {
  keys <- names(baseline_definitions())
  baseline <- lapply(keys, function(key) {
    x <- read_baseline(cli, key, filename)
    x[, library := key]
    x
  })
  gated <- read_candidate(cli, filename)
  gated[, library := candidate_key]
  data.table::rbindlist(c(baseline, list(gated)), fill = TRUE)
}

strict_stage2_join <- function(left, right, by, left_label, right_label,
                               suffixes = c("_left", "_right")) {
  left <- data.table::as.data.table(data.table::copy(left))
  right <- data.table::as.data.table(data.table::copy(right))
  missing_left <- setdiff(by, names(left))
  missing_right <- setdiff(by, names(right))
  if (length(missing_left) || length(missing_right)) {
    stop(
      "Stage-2 join keys are missing (", left_label, ": ",
      paste(missing_left, collapse = ", "), "; ", right_label, ": ",
      paste(missing_right, collapse = ", "), ")"
    )
  }
  duplicate_left <- left[, .N, by = by][N > 1L]
  duplicate_right <- right[, .N, by = by][N > 1L]
  if (nrow(duplicate_left) || nrow(duplicate_right)) {
    stop(
      "Stage-2 join is not one-to-one for ", left_label, " versus ",
      right_label
    )
  }
  left[, .stage2_left_present := TRUE]
  right[, .stage2_right_present := TRUE]
  joined <- merge(
    left, right, by = by, suffixes = suffixes, all = TRUE, sort = FALSE
  )
  if (any(is.na(joined$.stage2_left_present)) ||
      any(is.na(joined$.stage2_right_present))) {
    stop(
      "Stage-2 join key sets differ for ", left_label, " versus ", right_label
    )
  }
  joined[, c(".stage2_left_present", ".stage2_right_present") := NULL]
  joined[]
}

accuracy_delta_table <- function(x, dimensions = character()) {
  reference <- x[library == "os1_model"]
  candidates <- setdiff(unique(x$library), "os1_model")
  data.table::rbindlist(lapply(candidates, function(key) {
    candidate <- x[library == key]
    by <- c(dimensions, "metric_key", "metric")
    joined <- strict_stage2_join(
      reference, candidate, by = by, left_label = "os1_model",
      right_label = key, suffixes = c("_os1", "_candidate")
    )
    joined[, `:=`(
      library = key,
      mean_delta_pp = mean_accuracy_candidate - mean_accuracy_os1,
      rsd_delta_pp = rsd_candidate - rsd_os1
    )]
    joined
  }), fill = TRUE)
}

particle_transitions <- function(traces, right_key) {
  columns <- c(
    "sample_id", "particle_id", "material_class_scored", "specific_correct",
    "plastic_correct"
  )
  left <- data.table::copy(traces[library == "os1_model", ..columns])
  right <- data.table::copy(traces[library == right_key, ..columns])
  data.table::setnames(
    left, setdiff(columns, c("sample_id", "particle_id")),
    paste0(setdiff(columns, c("sample_id", "particle_id")), "_os1")
  )
  data.table::setnames(
    right, setdiff(columns, c("sample_id", "particle_id")),
    paste0(setdiff(columns, c("sample_id", "particle_id")), "_candidate")
  )
  joined <- strict_stage2_join(
    left, right, by = c("sample_id", "particle_id"),
    left_label = "os1_model particles", right_label = paste(right_key, "particles"),
    suffixes = c("_os1", "_candidate")
  )
  joined[, `:=`(
    candidate = right_key,
    class_transition = paste(
      material_class_scored_os1, material_class_scored_candidate, sep = " -> "
    ),
    specific_transition = data.table::fcase(
      is.na(specific_correct_os1) & is.na(specific_correct_candidate),
      "missing-both",
      is.na(specific_correct_os1), "missing-os1",
      is.na(specific_correct_candidate), "missing-candidate",
      !specific_correct_os1 & specific_correct_candidate, "corrected",
      specific_correct_os1 & !specific_correct_candidate, "regressed",
      default = "unchanged"
    ),
    broad_transition = data.table::fcase(
      is.na(plastic_correct_os1) & is.na(plastic_correct_candidate),
      "missing-both",
      is.na(plastic_correct_os1), "missing-os1",
      is.na(plastic_correct_candidate), "missing-candidate",
      !plastic_correct_os1 & plastic_correct_candidate, "corrected",
      plastic_correct_os1 & !plastic_correct_candidate, "regressed",
      default = "unchanged"
    )
  )]
  joined
}

compare_results <- function(cli) {
  compatible <- assert_candidate_compatible(cli)
  fit <- compatible$fit
  directory <- file.path(cli$output_root, "comparison")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  summary <- read_metric_set(cli, "metrics_summary.csv")
  by_sample <- read_metric_set(cli, "metrics_by_sample.csv")
  by_material <- read_metric_set(cli, "metrics_by_material_type.csv")
  by_size <- read_metric_set(cli, "metrics_by_size_stratum.csv")
  traces <- read_metric_set(cli, "metrics_particle_trace.csv")
  truth <- legacy$engine$truth_table(cli)[, .(Map, material_type, size_stratum)]
  if (!"material_type" %in% names(traces)) {
    traces <- merge(
      traces, truth, by.x = "sample_id", by.y = "Map", all.x = TRUE,
      sort = FALSE
    )
  }
  recovery <- c("count_accuracy", "area_accuracy", "feret_accuracy")
  reference <- by_sample[
    library == "os1_medoid", c("Map", recovery), with = FALSE
  ]
  gated <- by_sample[
    library == candidate_key, c("Map", recovery), with = FALSE
  ]
  recovery_join <- strict_stage2_join(
    reference, gated, by = "Map", left_label = "os1_medoid recovery",
    right_label = "gated recovery", suffixes = c("_os1", "_gated")
  )
  recovery_audit <- data.table::rbindlist(lapply(recovery, function(metric) {
    reference_value <- recovery_join[[paste0(metric, "_os1")]]
    candidate_value <- recovery_join[[paste0(metric, "_gated")]]
    both_missing <- is.na(reference_value) & is.na(candidate_value)
    both_finite <- is.finite(reference_value) & is.finite(candidate_value)
    delta <- candidate_value - reference_value
    data.table::data.table(
      Map = recovery_join$Map, metric = metric,
      reference_value = reference_value, candidate_value = candidate_value,
      delta = delta, both_missing = both_missing,
      status = data.table::fcase(
        both_missing, "both_missing",
        both_finite & abs(delta) <= 1e-9, "identical",
        both_finite, "drift",
        default = "nonfinite_mismatch"
      ),
      pass = both_missing | (both_finite & abs(delta) <= 1e-9)
    )
  }), fill = TRUE)
  if (any(!recovery_audit$pass)) {
    failed <- unique(recovery_audit[pass == FALSE, metric])
    stop(
      "Particle recovery drift detected for gated model: ",
      paste(failed, collapse = ", ")
    )
  }
  recovery_summary <- recovery_audit[, .(
    maps = .N, both_missing = sum(both_missing),
    maximum_absolute_delta = if (any(is.finite(delta))) {
      max(abs(delta[is.finite(delta)]))
    } else {
      NA_real_
    },
    pass = all(pass)
  ), by = metric]
  overall_delta <- accuracy_delta_table(summary)
  material_delta <- accuracy_delta_table(by_material, "material_type")
  size_delta <- accuracy_delta_table(by_size, "size_stratum")
  paired <- data.table::rbindlist(lapply(
    setdiff(unique(by_sample$library), "os1_model"),
    function(key) legacy$engine$paired_library_differences(
      by_sample, "os1_model", key
    )
  ), fill = TRUE)
  paired_summary <- legacy$engine$paired_metric_summary(paired)
  vinyl_oof <- data.table::fread(candidate_paths(cli)$vinyl)
  vinyl_oof_summary <- data.table::fread(candidate_paths(cli)$vinyl_summary)
  vinyl_nodes <- data.table::fread(candidate_paths(cli)$vinyl_nodes)
  transitions <- particle_transitions(traces, candidate_key)
  transition_summary <- transitions[, .(particles = .N), by = .(
    candidate, specific_transition, broad_transition, class_transition
  )][order(-particles)]
  confusion <- traces[, .(particles = .N), by = .(
    library, sample_id, SpecID, PNID, material_class_scored,
    specific_correct, plastic_correct
  )][order(library, sample_id, -particles)]
  targeted <- traces[, .(
    particles = .N,
    specific_accuracy = 100 * mean(specific_correct, na.rm = TRUE),
    broad_accuracy = 100 * mean(plastic_correct, na.rm = TRUE),
    polyethylene_to_pva = sum(
      is_polyethylene_truth(SpecID) &
        grepl("polyvinyl[[:space:]]*alcohol", material_class_scored,
              ignore.case = TRUE),
      na.rm = TRUE
    ),
    nonplastic_to_plastic = sum(
      material_type == "non-plastic" & !plastic_correct, na.rm = TRUE
    )
  ), by = library]
  polyethylene <- traces[
    is_polyethylene_truth(SpecID),
    .(
      particles = .N,
      mean_accuracy = 100 * mean(specific_correct, na.rm = TRUE)
    ),
    by = library
  ]
  pe_value <- function(library_key, column, default) {
    value <- polyethylene[get("library") == library_key, get(column)]
    if (length(value) == 1L) value[[1L]] else default
  }
  pe_reference <- pe_value("os1_model", "mean_accuracy", NA_real_)
  pe_candidate <- pe_value(candidate_key, "mean_accuracy", NA_real_)
  pe_reference_n <- pe_value("os1_model", "particles", 0L)
  pe_candidate_n <- pe_value(candidate_key, "particles", 0L)
  new_gate <- data.table::rbindlist(list(
    overall_delta[
      library == candidate_key &
        metric_key %in% c("specific_accuracy", "plastic_accuracy"),
      .(scope = "overall", level = "all", metric_key, n_os1, n_candidate,
        mean_delta_pp)
    ],
    material_delta[
      library == candidate_key &
        metric_key %in% c("specific_accuracy", "plastic_accuracy"),
      .(scope = "material_type", level = material_type, metric_key,
        n_os1, n_candidate, mean_delta_pp)
    ],
    size_delta[
      library == candidate_key &
        metric_key %in% c("specific_accuracy", "plastic_accuracy"),
      .(scope = "size_stratum", level = size_stratum, metric_key,
        n_os1, n_candidate, mean_delta_pp)
    ]
  ), fill = TRUE)
  protected_metrics <- c("specific_accuracy", "plastic_accuracy")
  for (scope_name in c("overall", "material_type", "size_stratum")) {
    scope_rows <- new_gate[scope == scope_name]
    if (!nrow(scope_rows) ||
        anyDuplicated(scope_rows[, .(level, metric_key)]) ||
        any(scope_rows[, !setequal(metric_key, protected_metrics), by = level]$V1)) {
      stop("Stage-2 gate coverage is incomplete for scope: ", scope_name)
    }
  }
  new_gate[, applicable := is.finite(n_os1) & n_os1 > 0]
  new_gate[, gate_status := data.table::fcase(
    !applicable, "not_applicable",
    is.na(n_candidate) | n_candidate <= 0, "missing_candidate",
    n_candidate != n_os1, "support_mismatch",
    !is.finite(mean_delta_pp), "nonfinite_candidate",
    mean_delta_pp >= -1, "pass",
    default = "fail"
  )]
  new_gate[, `:=`(
    reference = "os1_model", candidate = candidate_key,
    threshold_pp = -1,
    pass = data.table::fifelse(applicable, gate_status == "pass", NA)
  )]
  pe_applicable <- is.finite(pe_reference_n) && pe_reference_n > 0
  pe_status <- if (!pe_applicable) {
    "not_applicable"
  } else if (!is.finite(pe_candidate_n) || pe_candidate_n <= 0) {
    "missing_candidate"
  } else if (pe_candidate_n != pe_reference_n) {
    "support_mismatch"
  } else if (!is.finite(pe_candidate) || !is.finite(pe_reference)) {
    "nonfinite_candidate"
  } else if (pe_candidate - pe_reference >= -1) {
    "pass"
  } else {
    "fail"
  }
  new_gate <- data.table::rbindlist(list(
    new_gate,
    data.table::data.table(
      scope = "polyethylene_particle_recall", level = "polyethylene",
      metric_key = "specific_accuracy",
      n_os1 = pe_reference_n,
      n_candidate = pe_candidate_n,
      mean_delta_pp = pe_candidate - pe_reference,
      reference = "os1_model", candidate = candidate_key,
      threshold_pp = -1, applicable = pe_applicable,
      gate_status = pe_status,
      pass = if (pe_applicable) pe_status == "pass" else NA
    )
  ), fill = TRUE)
  gate_decision <- new_gate[applicable == TRUE, .(
    checks = .N,
    failed = sum(is.na(pass) | pass == FALSE),
    worst_delta_pp = if (any(is.finite(mean_delta_pp))) {
      min(mean_delta_pp[is.finite(mean_delta_pp)])
    } else {
      NA_real_
    },
    pass = all(!is.na(pass) & pass == TRUE)
  )]
  outputs <- list(
    metrics_summary = summary,
    metrics_by_sample = by_sample,
    metrics_by_material_type = by_material,
    metrics_by_size_stratum = by_size,
    accuracy_delta_overall_vs_os1 = overall_delta,
    accuracy_delta_by_material_vs_os1 = material_delta,
    accuracy_delta_by_size_vs_os1 = size_delta,
    paired_map_differences_vs_os1 = paired,
    paired_map_summary_vs_os1 = paired_summary,
    particle_recovery_compatibility = recovery_audit,
    particle_recovery_compatibility_summary = recovery_summary,
    gated_noninferiority_gate = new_gate,
    gated_noninferiority_decision = gate_decision,
    holdout_confusion = confusion,
    targeted_failures = targeted,
    gated_particle_transitions_vs_os1 = transitions,
    gated_particle_transition_summary = transition_summary,
    gated_map_runtimes = compatible$runtime,
    gated_model_fit_manifest = compatible$fit,
    source_held_vinyl_family_oof = vinyl_oof,
    source_held_vinyl_family_oof_summary = vinyl_oof_summary,
    source_held_vinyl_family_nodes = vinyl_nodes
  )
  for (name in names(outputs)) {
    legacy$engine$write_csv(
      outputs[[name]], file.path(directory, paste0(name, ".csv"))
    )
  }
  headline <- summary[
    library %in% c("os1_model", "hierarchical_guardrailed", candidate_key) &
      metric_key %in% c("specific_accuracy", "plastic_accuracy"),
    .(library, metric, mean_accuracy, rsd)
  ]
  report <- c(
    "# Strict gated FTIR model validation",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Locked design",
    "",
    if (identical(
      as.character(fit$model_origin[[1L]]),
      "axis_corrected_363bin_model_probe"
    )) {
      paste0(
        "The `gated_three_level` derivative FTIR artifact is an axis-corrected ",
        "363-bin model probe staged unchanged with its accompanying full ",
        "checkpoint; this runner did not fit a model. Its FTIR medoid is the ",
        "prior full rebuild medoid restricted to the workflow axis without a ",
        "fresh parent-to-medoid closure pass, so this evidence diagnoses model ",
        "behavior and is not by itself a promotable full library release. OS1 ",
        "plus the four plan-047 candidates were read from frozen results under ",
        "aggregate SHA-256 `", baseline_lock_sha256, "`; none were refitted or ",
        "rerun."
      )
    } else {
      paste0(
        "The externally staged `gated_three_level` derivative FTIR candidate ",
        "was copied unchanged with its accompanying full checkpoint and run on ",
        "the locked controls; this runner did not fit a model. OS1 plus the ",
        "four plan-047 candidates were read from frozen results under aggregate ",
        "SHA-256 `", baseline_lock_sha256, "`; none were refitted or rerun."
      )
    },
    "",
    paste0(
      "The locked Stage-1 gate passed within +/-1.0 absolute percentage ",
      "point overall, per sample, by material type, and by size stratum for ",
      "both OS1 medoid and model evidence. The PMMA and red-PET noisy ",
      "controls remain excluded."
    ),
    "",
    "## Particle-recovery compatibility",
    "",
    legacy$engine$markdown_table(
      recovery_summary, names(recovery_summary), digits = 9L
    ),
    "",
    paste0(
      "Matching missing values represent maps where neither workflow ",
      "detected a particle; they are equivalent, not recovery drift."
    ),
    "",
    "## Accuracy and variability",
    "",
    legacy$engine$markdown_table(headline, names(headline), digits = 3L),
    "",
    "## Gated non-inferiority versus OS1 model",
    "",
    legacy$engine$markdown_table(
      gate_decision, names(gate_decision), digits = 3L
    ),
    "",
    legacy$engine$markdown_table(new_gate, names(new_gate), digits = 3L),
    "",
    paste0(
      "Every applicable overall, material-type, size-stratum, and ",
      "polyethylene-recall check must be no more than 1.0 absolute ",
      "percentage point below OS1. Missing, non-finite, or changed candidate ",
      "support fails the gate; non-applicable zero-support OS1 strata do not ",
      "pass it."
    ),
    "",
    "## Targeted failure counts",
    "",
    legacy$engine$markdown_table(targeted, names(targeted), digits = 3L),
    "",
    "## Source-held vinyl-family routing",
    "",
    paste0(
      "The material-type root, plastic-group gate, and vinyl terminal node ",
      "must each use source-component holdout (or be deterministic). True ",
      "held-out coverage is required for `polyethylene-vinyl acetate`, ",
      "`polyvinylalcohols`, and `polyvinylesters`."
    ),
    "",
    legacy$engine$markdown_table(vinyl_nodes, names(vinyl_nodes), digits = 3L),
    "",
    legacy$engine$markdown_table(
      vinyl_oof_summary, names(vinyl_oof_summary), digits = 3L
    ),
    "",
    "## Runtime",
    "",
    paste0(
      "Production training runtime recorded by the released artifact: ",
      round(compatible$fit$elapsed_seconds[[1L]], 2),
      " seconds (ceiling ", fit_runtime_ceiling_seconds,
      "). Slowest holdout map: ",
      round(max(compatible$runtime$elapsed_seconds, na.rm = TRUE), 2),
      " seconds (ceiling 300)."
    ),
    "",
    "## Interpretation boundary",
    "",
    paste0(
      "This holdout is a locked confirmation set. The report compares frozen ",
      "results only and does not tune the taxonomy, hierarchy, lambda policy, ",
      "or particle-analysis settings from holdout performance."
    )
  )
  legacy$engine$write_lines(
    report, file.path(directory, "gated_hierarchy_validation_report.md")
  )
  message("Gated hierarchy comparison written to: ", directory)
  failed_gates <- new_gate[
    applicable == TRUE & (is.na(pass) | pass == FALSE)
  ]
  if (nrow(failed_gates)) {
    labels <- paste(
      failed_gates$scope, failed_gates$level, failed_gates$metric_key,
      failed_gates$gate_status, sep = "/"
    )
    stop(
      "Gated non-inferiority failed; evidence was retained for: ",
      paste(labels, collapse = ", ")
    )
  }
  invisible(outputs)
}

main <- function() {
  cli <- parse_cli()
  if (identical(cli$stage, "help")) return(usage())
  cli <- normalize_cli(cli)
  dir.create(cli$output_root, recursive = TRUE, showWarnings = FALSE)
  switch(
    cli$stage,
    "check-baselines" = assert_stage1_complete(cli),
    "fit" = fit_candidate(cli),
    "run" = run_gated(cli, cli$maps),
    "probe" = run_gated(cli, "probe"),
    "validate" = run_gated(cli, "all"),
    "compare" = compare_results(cli),
    "all" = {
      assert_stage1_complete(cli)
      fit_candidate(cli)
      run_gated(cli, "probe")
      run_gated(cli, "all")
      compare_results(cli)
    }
  )
  invisible(TRUE)
}

if (identical(environment(), globalenv())) main()
