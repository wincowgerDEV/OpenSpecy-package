# Validate flat and hierarchical FTIR models with the frozen positive controls.
# The current medoid library is reused; no library-cleaning stages are rerun.
#
# Examples:
#   Rscript benchmarks/hierarchical_model_validation.R --stage=fit
#   Rscript benchmarks/hierarchical_model_validation.R --stage=score-saved
#   Rscript benchmarks/hierarchical_model_validation.R --stage=run --artifact=os1_medoid --maps=all
#   Rscript benchmarks/hierarchical_model_validation.R --stage=probe
#   Rscript benchmarks/hierarchical_model_validation.R --stage=validate
#   Rscript benchmarks/hierarchical_model_validation.R --stage=compare

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest", "glmnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) {
    return(normalize_path("benchmarks/hierarchical_model_validation.R"))
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
legacy$runner_version <- "hierarchical-model-validation-v1"

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(
    stage = "fit",
    artifact = NULL,
    candidate = "all",
    maps = "all",
    workers = "5",
    data_root = "C:/Users/winco/OneDrive/Documents/Positive_Controls",
    historical_root = "C:/Users/winco/OneDrive/Documents/OS1_Results",
    current_medoid = paste0(
      "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/",
      "reference-library-build-2.0.0/releases/bb8cd82c7ecb/",
      "medoid_derivative.rds"
    ),
    output_root = paste0(
      "C:/Users/winco/OneDrive/Documents/Positive_Controls/",
      "hierarchical_model_validation"
    )
  )
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Arguments must use --name=value: ", arg)
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", arg), fixed = TRUE)
    value <- sub("^--[^=]+=", "", arg)
    if (!key %in% names(out)) stop("Unknown argument: --", key)
    out[[key]] <- value
  }
  out$workers <- suppressWarnings(as.integer(out$workers))
  if (is.na(out$workers) || out$workers < 1L) stop("--workers must be positive")
  out
}

candidate_configs <- function() {
  list(
    flat_macro = list(
      label = "Current flat FTIR model selected by macro accuracy",
      folder = "03_flat_macro", hierarchy_col = NULL,
      selection_rule = "macro"
    ),
    flat_guardrailed = list(
      label = "Current flat FTIR model selected by accuracy guardrails",
      folder = "04_flat_guardrailed", hierarchy_col = NULL,
      selection_rule = "guardrailed"
    ),
    hierarchical_macro = list(
      label = "Current hierarchical FTIR model selected by macro accuracy",
      folder = "05_hierarchical_macro", hierarchy_col = "material_type",
      selection_rule = "macro"
    ),
    hierarchical_guardrailed = list(
      label = "Current hierarchical FTIR model selected by accuracy guardrails",
      folder = "06_hierarchical_guardrailed",
      hierarchy_col = "material_type", selection_rule = "guardrailed"
    )
  )
}

candidate_paths <- function(cli, key) {
  directory <- file.path(cli$output_root, candidate_configs()[[key]]$folder)
  list(
    directory = directory,
    full = file.path(directory, "model_full.rds"),
    deploy = file.path(directory, "model.rds"),
    manifest = file.path(directory, "model_fit_manifest.csv"),
    lambda = file.path(directory, "model_lambda_metrics.csv"),
    classes = file.path(directory, "model_class_metrics.csv"),
    hierarchy = file.path(directory, "model_hierarchy_metrics.csv"),
    folds = file.path(directory, "model_fold_assignments.csv")
  )
}

artifact_definitions <- function(cli) {
  out <- list(
    os1_medoid = list(
      family = "os1", representation = "medoid",
      label = "Open Specy 1.0 derivative medoid library",
      folder = "01_os1_medoid",
      path = file.path(cli$historical_root, "medoid_derivative.rds"),
      expected_sha256 =
        "e588f690966d226a28eae17541c22f9999d15544ad1b07cfa797e6b4afe3b5e7",
      saved_folder = "mediod"
    ),
    os1_model = list(
      family = "os1", representation = "model",
      label = "Open Specy 1.0 FTIR derivative classification model",
      folder = "02_os1_model",
      path = file.path(cli$historical_root, "model_derivative.rds"),
      expected_sha256 =
        "6f4e5d2035a07622efb39738569e45a7ef1fad832cc448cc94633802f5c9e811",
      saved_folder = "model"
    )
  )
  configs <- candidate_configs()
  for (key in names(configs)) {
    paths <- candidate_paths(cli, key)
    if (!file.exists(paths$deploy) || !file.exists(paths$manifest)) {
      expected <- NA_character_
    } else {
      manifest <- data.table::fread(paths$manifest)
      expected <- manifest$deploy_sha256[[1L]]
    }
    out[[key]] <- list(
      family = "candidate", representation = "model",
      label = configs[[key]]$label, folder = configs[[key]]$folder,
      path = paths$deploy, expected_sha256 = expected
    )
  }
  out
}

legacy$artifact_definitions <- artifact_definitions
legacy$runner_version <- "hierarchical-model-validation-v1"

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

current_medoid <- function(cli) {
  expected <- "6cb03c87cf6113c9bc6b2feaf315af9719b6bc4da7110013de774d2eb7bf05cc"
  actual <- legacy$engine$sha256_file(cli$current_medoid)
  if (!identical(tolower(actual), expected)) {
    stop("Current medoid SHA-256 mismatch; refusing to refit from changed input")
  }
  object <- readRDS(cli$current_medoid)
  if (is.list(object) && "ftir" %in% names(object) && !is_OpenSpecy(object)) {
    object <- object$ftir
  }
  if (!is_OpenSpecy(object)) stop("Current medoid is not an OpenSpecy object")
  if ("spectrum_type" %in% names(object$metadata)) {
    object <- filter_spec(
      object,
      !is.na(object$metadata$spectrum_type) &
        object$metadata$spectrum_type == "ftir"
    )
  }
  list(object = object, sha256 = actual)
}

fit_signature <- function(cli, key, medoid_sha256) {
  root <- repo_root()
  files <- c(
    file.path(root, "R", "build_lib.R"),
    file.path(root, "R", "match_spec.R"),
    script_path()
  )
  hashes <- vapply(files, legacy$engine$sha256_file, character(1L))
  legacy$engine$sha256_object(list(
    runner = legacy$runner_version,
    candidate = key,
    config = candidate_configs()[[key]],
    medoid_sha256 = medoid_sha256,
    source_sha256 = hashes,
    args = list(
      class_col = "material_class", type_col = "spectrum_type",
      min_n = 10L, alpha = 0.1, seed = 123L,
      grouped = TRUE, weights = TRUE, make_relative = TRUE
    )
  ))
}

write_optional_table <- function(value, path) {
  if (is.null(value)) return(invisible(FALSE))
  value <- data.table::as.data.table(value)
  if (!nrow(value) && !ncol(value)) return(invisible(FALSE))
  legacy$engine$write_csv(value, path)
  invisible(TRUE)
}

collect_model_diagnostics <- function(model, paths) {
  write_optional_table(model$lambda_metrics, paths$lambda)
  write_optional_table(model$class_metrics, paths$classes)
  write_optional_table(model$hierarchy_metrics, paths$hierarchy)
  write_optional_table(model$fold_assignments, paths$folds)
}

fit_candidate <- function(cli, key, medoid) {
  configs <- candidate_configs()
  if (!key %in% names(configs)) {
    stop("Unknown candidate: ", key)
  }
  config <- configs[[key]]
  paths <- candidate_paths(cli, key)
  dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
  signature <- fit_signature(cli, key, medoid$sha256)
  if (file.exists(paths$manifest) && file.exists(paths$full) &&
      file.exists(paths$deploy)) {
    manifest <- data.table::fread(paths$manifest)
    reusable <- identical(manifest$fit_signature[[1L]], signature) &&
      identical(
        manifest$full_sha256[[1L]], legacy$engine$sha256_file(paths$full)
      ) && identical(
        manifest$deploy_sha256[[1L]], legacy$engine$sha256_file(paths$deploy)
      )
    if (reusable) {
      message("Reusing compatible model fit: ", key)
      return(invisible(manifest))
    }
    stop(
      "Stale fitted model exists for ", key,
      ". Move that candidate folder before refitting changed source."
    )
  }
  memory_before <- legacy$engine$system_memory()
  if (is.finite(memory_before$used_fraction) &&
      memory_before$used_fraction >= 0.80) {
    stop("System memory is already at or above 80% before fitting ", key)
  }
  message(
    "Fitting ", key, " from reusable medoid: ",
    ncol(medoid$object$spectra), " spectra x ",
    nrow(medoid$object$spectra), " predictors"
  )
  started <- Sys.time()
  model <- train_spec_model(
    medoid$object,
    class_col = "material_class", type_col = "spectrum_type",
    min_n = 10L, alpha = 0.1, seed = 123L,
    grouped = TRUE, weights = TRUE, make_relative = TRUE,
    hierarchy_col = config$hierarchy_col,
    selection_rule = config$selection_rule,
    method = "logistic_regression"
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  deploy <- OpenSpecy:::.lib_slim_model(model)
  saveRDS(model, paths$full)
  saveRDS(deploy, paths$deploy)
  collect_model_diagnostics(model, paths)
  memory_after <- legacy$engine$system_memory()
  manifest <- data.table::data.table(
    candidate = key,
    label = config$label,
    topology = if (is.null(config$hierarchy_col)) "flat" else "hierarchical",
    hierarchy_col = if (is.null(config$hierarchy_col)) NA_character_ else
      config$hierarchy_col,
    selection_rule = config$selection_rule,
    accuracy_tolerance = 0.01,
    fit_signature = signature,
    input_path = normalize_path(cli$current_medoid),
    input_sha256 = medoid$sha256,
    observations = model$observation_count,
    predictors = length(model$all_variables),
    classes = length(model$class_names),
    elapsed_seconds = elapsed,
    memory_used_fraction_before = memory_before$used_fraction,
    memory_used_fraction_after = memory_after$used_fraction,
    full_bytes = unname(file.info(paths$full)$size),
    deploy_bytes = unname(file.info(paths$deploy)$size),
    full_sha256 = legacy$engine$sha256_file(paths$full),
    deploy_sha256 = legacy$engine$sha256_file(paths$deploy),
    selected_lambda_converged = isTRUE(model$selected_lambda_converged),
    r_version = R.version.string,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  legacy$engine$write_csv(manifest, paths$manifest)
  if (elapsed >= 600) {
    stop(
      "Candidate checkpoint was saved, but the 10-minute fit target was ",
      "exceeded by ", key, ": ", round(elapsed, 1), " seconds"
    )
  }
  if (is.finite(memory_after$used_fraction) &&
      memory_after$used_fraction >= 0.80) {
    stop("Candidate checkpoint was saved, but system memory reached 80%")
  }
  message("Fitted ", key, " in ", round(elapsed, 1), " seconds")
  invisible(manifest)
}

fit_candidates <- function(cli) {
  legacy$engine$load_openspecy(repo_root())
  options(OpenSpecy.build_workers = cli$workers)
  medoid <- current_medoid(cli)
  keys <- if (identical(cli$candidate, "all")) {
    names(candidate_configs())
  } else {
    cli$candidate
  }
  for (key in keys) fit_candidate(cli, key, medoid)
  invisible(TRUE)
}

stage1_files <- function(cli, key) {
  file.path(cli$output_root, artifact_definitions(cli)[[key]]$folder, c(
    "stage1_compatibility_gate.csv",
    "stage1_compatibility_by_sample.csv",
    "stage1_compatibility_by_material_type.csv",
    "stage1_compatibility_by_size_stratum.csv",
    "library_manifest.csv"
  ))
}

assert_stage1_complete <- function(cli) {
  files <- c(stage1_files(cli, "os1_medoid"), stage1_files(cli, "os1_model"))
  if (!all(file.exists(files))) {
    stop("Stage 2 locked: complete both OS1 medoid and model Stage 1 runs")
  }
  for (key in c("os1_medoid", "os1_model")) {
    key_files <- stage1_files(cli, key)
    gates <- lapply(key_files[1:4], data.table::fread)
    if (!all(vapply(gates, function(x) {
      all(x$within_one_percentage_point)
    }, logical(1L)))) {
      stop("Stage 2 locked: ", key, " compatibility is not passing")
    }
  }
  manifests <- lapply(c("os1_medoid", "os1_model"), function(key) {
    data.table::fread(tail(stage1_files(cli, key), 1L))
  })
  if (!identical(
    manifests[[1L]]$source_sha256[[1L]], manifests[[2L]]$source_sha256[[1L]]
  ) || !identical(
    manifests[[1L]]$workflow_sha256[[1L]],
    manifests[[2L]]$workflow_sha256[[1L]]
  )) {
    stop("Stage 2 locked: OS1 medoid and model gates use different source")
  }
  invisible(TRUE)
}

run_one <- function(cli, key, maps = cli$maps) {
  definitions <- artifact_definitions(cli)
  if (!key %in% names(definitions)) stop("Unknown artifact: ", key)
  if (!identical(definitions[[key]]$family, "os1")) {
    assert_stage1_complete(cli)
  }
  run_cli <- cli
  run_cli$artifact <- key
  run_cli$maps <- maps
  legacy$run_artifact(run_cli)
}

run_probe <- function(cli) {
  assert_stage1_complete(cli)
  for (key in names(candidate_configs())) run_one(cli, key, "probe")
  invisible(TRUE)
}

run_validation <- function(cli) {
  for (key in c("os1_medoid", "os1_model")) run_one(cli, key, "all")
  assert_stage1_complete(cli)
  for (key in names(candidate_configs())) run_one(cli, key, "all")
  invisible(TRUE)
}

read_result <- function(cli, key, filename) {
  definition <- artifact_definitions(cli)[[key]]
  legacy$read_artifact_result(cli, definition, filename)
}

all_runtime <- function(cli, keys) {
  data.table::rbindlist(lapply(keys, function(key) {
    folder <- file.path(cli$output_root, artifact_definitions(cli)[[key]]$folder)
    paths <- list.files(folder, pattern = "^runtime_.*[.]csv$", full.names = TRUE)
    paths <- paths[basename(paths) != "runtime_invocation.csv"]
    x <- data.table::rbindlist(lapply(paths, data.table::fread), fill = TRUE)
    x[, library := key]
    x
  }), fill = TRUE)
}

calibration_tables <- function(particles) {
  models <- particles[representation == "model"]
  models[, confidence_bin := cut(
    top1_score, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE,
    right = TRUE
  )]
  bins <- models[, .(
    particles = .N,
    mean_confidence = mean(top1_score, na.rm = TRUE),
    observed_accuracy = mean(specific_correct, na.rm = TRUE)
  ), by = .(library, confidence_bin)]
  summary <- bins[, .(
    particles = sum(particles),
    expected_calibration_error = sum(
      particles * abs(mean_confidence - observed_accuracy), na.rm = TRUE
    ) / sum(particles),
    top_class_brier = mean(
      (models[library == .BY$library]$top1_score -
         as.numeric(models[library == .BY$library]$specific_correct))^2,
      na.rm = TRUE
    )
  ), by = library]
  list(bins = bins, summary = summary)
}

noninferiority_table <- function(summary, by_material, by_size, traces) {
  candidates <- names(candidate_configs())
  metric_rows <- function(x, dimensions) {
    reference <- x[library == "os1_model"]
    data.table::rbindlist(lapply(candidates, function(key) {
      candidate <- x[library == key]
      joined <- merge(
        reference, candidate, by = c(dimensions, "metric_key", "metric"),
        suffixes = c("_os1", "_candidate"), sort = FALSE
      )
      joined[, `:=`(
        library = key,
        scope = if (length(dimensions)) dimensions[[1L]] else "overall",
        delta_pp = mean_accuracy_candidate - mean_accuracy_os1,
        threshold_pp = -1,
        pass = mean_accuracy_candidate - mean_accuracy_os1 >= -1
      )]
      joined
    }), fill = TRUE)
  }
  out <- data.table::rbindlist(list(
    metric_rows(
      summary[metric_key %in% c("specific_accuracy", "plastic_accuracy")],
      character()
    ),
    metric_rows(
      by_material[metric_key %in% c("specific_accuracy", "plastic_accuracy")],
      "material_type"
    ),
    metric_rows(
      by_size[metric_key %in% c("specific_accuracy", "plastic_accuracy")],
      "size_stratum"
    )
  ), fill = TRUE)
  pe <- traces[grepl("ethylene|olefin", SpecID, ignore.case = TRUE), .(
    pe_particles = .N,
    mean_accuracy_candidate = 100 * mean(specific_correct, na.rm = TRUE)
  ), by = library]
  pe_reference <- pe[library == "os1_model"]$mean_accuracy_candidate[[1L]]
  pe <- pe[library %in% candidates]
  pe[, `:=`(
    scope = "polyethylene_particle_recall",
    metric_key = "specific_accuracy",
    metric = "Polyethylene particle recall",
    mean_accuracy_os1 = pe_reference,
    delta_pp = mean_accuracy_candidate - pe_reference,
    threshold_pp = -1,
    pass = mean_accuracy_candidate - pe_reference >= -1
  )]
  data.table::rbindlist(list(out, pe), fill = TRUE)
}

builder_diagnostics <- function(cli) {
  metrics <- list()
  classes <- list()
  selected_nodes <- list()
  for (key in names(candidate_configs())) {
    model <- readRDS(candidate_paths(cli, key)$full)
    if (identical(model$model_type, "hierarchical_logistic_regression")) {
      metric <- data.table::copy(model$hierarchy_metrics)
    } else {
      metric <- data.table::copy(
        data.table::as.data.table(model$lambda_metrics)[selected == TRUE]
      )
      metric[, scope := "leaf"]
    }
    metric[, `:=`(
      candidate = key,
      topology = if (identical(
        model$model_type, "hierarchical_logistic_regression"
      )) "hierarchical" else "flat",
      selection_rule = model$selection_rule
    )]
    metrics[[key]] <- metric
    class_metric <- data.table::copy(
      data.table::as.data.table(model$class_metrics)
    )
    class_metric[, candidate := key]
    classes[[key]] <- class_metric
    node_metric <- data.table::copy(
      data.table::as.data.table(model$lambda_metrics)[selected == TRUE]
    )
    node_metric[, candidate := key]
    selected_nodes[[key]] <- node_metric
    rm(model)
    gc(verbose = FALSE)
  }
  list(
    metrics = data.table::rbindlist(metrics, fill = TRUE),
    classes = data.table::rbindlist(classes, fill = TRUE),
    selected_nodes = data.table::rbindlist(selected_nodes, fill = TRUE)
  )
}

write_report <- function(cli, summary, noninferiority, paired_summary,
                         calibration, failures, transitions, development, runtimes,
                         fit_manifests) {
  directory <- file.path(cli$output_root, "comparison")
  candidates <- names(candidate_configs())
  decision <- noninferiority[library %in% candidates, .(
    checks = .N, failed = sum(!pass), worst_delta_pp = min(delta_pp, na.rm = TRUE),
    pass = all(pass)
  ), by = library]
  accuracy <- summary[
    library %in% c("os1_model", candidates) &
      metric_key %in% c("specific_accuracy", "plastic_accuracy"),
    .(library, metric, mean_accuracy, rsd)
  ]
  runtime_summary <- runtimes[, .(
    maps = data.table::uniqueN(sample_id),
    maximum_seconds = max(elapsed_seconds, na.rm = TRUE),
    median_seconds = stats::median(elapsed_seconds, na.rm = TRUE),
    maximum_memory_fraction = max(memory_used_fraction_after, na.rm = TRUE)
  ), by = library]
  report <- c(
    "# Hierarchical FTIR model positive-control validation",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Frozen design",
    "",
    paste0(
      "All four candidates were fitted from the same saved current FTIR ",
      "medoid library. Cleaning, pruning, derivatives, and medoid selection ",
      "were not rerun. The positive-control maps, preprocessing, particle ",
      "segmentation, exclusions, and `automate_particle_analysis()` settings ",
      "were held fixed. `PMMA_15Nov223_control` and ",
      "`RedPETFibers_15Nov223_control` were excluded."
    ),
    "",
    paste0(
      "The guardrailed policy uses an absolute one-percentage-point window: ",
      "`abs(candidate accuracy - maximum accuracy) <= 0.01`; it is not a ",
      "relative one-percent change."
    ),
    "",
    "## Accuracy and variability",
    "",
    legacy$engine$markdown_table(accuracy, names(accuracy), digits = 3L),
    "",
    "## Predeclared non-inferiority decision",
    "",
    legacy$engine$markdown_table(decision, names(decision), digits = 3L),
    "",
    paste0(
      "A candidate passes only when overall specific and broad accuracy, every ",
      "material-type and size stratum, and polyethylene particle recall are no ",
      "more than 1.0 percentage point below OS1. Failed candidates remain ",
      "diagnostic results and are not tuned on this holdout cohort."
    ),
    "",
    "## Paired map effects versus OS1",
    "",
    legacy$engine$markdown_table(
      paired_summary, names(paired_summary), digits = 3L
    ),
    "",
    "## Grouped builder development metrics",
    "",
    legacy$engine$markdown_table(
      development[, .(
        candidate, topology, selection_rule, scope, overall_accuracy,
        macro_class_accuracy, log_loss, brier_score
      )],
      c("candidate", "topology", "selection_rule", "scope",
        "overall_accuracy", "macro_class_accuracy", "log_loss",
        "brier_score"), digits = 4L
    ),
    "",
    paste0(
      "These grouped out-of-fold results selected and froze the candidates ",
      "before positive-control scoring. They are reported alongside, but are ",
      "not recomputed or optimized from, the holdout maps."
    ),
    "",
    "## Calibration and targeted failures",
    "",
    legacy$engine$markdown_table(
      calibration, names(calibration), digits = 4L
    ),
    "",
    legacy$engine$markdown_table(failures, names(failures), digits = 0L),
    "",
    "Particle-level corrections and regressions versus OS1:",
    "",
    legacy$engine$markdown_table(
      transitions, names(transitions), digits = 0L
    ),
    "",
    "## Runtime",
    "",
    legacy$engine$markdown_table(
      runtime_summary, names(runtime_summary), digits = 3L
    ),
    "",
    "Model-fit timing and artifact sizes:",
    "",
    legacy$engine$markdown_table(
      fit_manifests[, .(
        candidate, topology, selection_rule, elapsed_seconds,
        deploy_bytes, selected_lambda_converged
      )],
      c("candidate", "topology", "selection_rule", "elapsed_seconds",
        "deploy_bytes", "selected_lambda_converged"), digits = 3L
    ),
    "",
    "## Interpretation boundary",
    "",
    paste0(
      "Selection diagnostics were generated only from grouped builder folds. ",
      "This holdout is a locked confirmation set. Promotion of a hierarchical ",
      "artifact requires the complete gate above and a separate maintainer ",
      "decision; this benchmark does not replace the historical model alias."
    )
  )
  legacy$engine$write_lines(
    report, file.path(directory, "hierarchical_model_validation_report.md")
  )
}

compare_results <- function(cli) {
  assert_stage1_complete(cli)
  definitions <- artifact_definitions(cli)
  keys <- names(definitions)
  directory <- file.path(cli$output_root, "comparison")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  manifests <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "library_manifest.csv")
  }), fill = TRUE)
  if (data.table::uniqueN(manifests$source_sha256) != 1L ||
      data.table::uniqueN(manifests$workflow_sha256) != 1L) {
    stop("Artifacts were not run with identical source and frozen workflow")
  }
  summary <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "metrics_summary.csv")
  }), fill = TRUE)
  by_sample <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "metrics_by_sample.csv")
  }), fill = TRUE)
  by_material <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "metrics_by_material_type.csv")
  }), fill = TRUE)
  by_size <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "metrics_by_size_stratum.csv")
  }), fill = TRUE)
  traces <- data.table::rbindlist(lapply(keys, function(key) {
    read_result(cli, key, "metrics_particle_trace.csv")
  }), fill = TRUE)
  if (!"material_type" %in% names(traces)) {
    truth <- legacy$engine$truth_table(cli)[, .(Map, material_type)]
    traces <- merge(
      traces, truth, by.x = "sample_id", by.y = "Map", all.x = TRUE,
      sort = FALSE
    )
  }
  counts <- by_sample[, .(maps = data.table::uniqueN(Map)), by = library]
  if (nrow(counts) != length(keys) || any(counts$maps != 20L)) {
    stop("Comparison requires 20 eligible maps for every artifact")
  }
  runtimes <- all_runtime(cli, keys)
  if (any(runtimes$elapsed_seconds >= 300, na.rm = TRUE)) {
    stop("At least one map exceeded the five-minute runtime ceiling")
  }
  recovery <- c("count_accuracy", "area_accuracy", "feret_accuracy")
  reference <- by_sample[library == "os1_medoid", c("Map", recovery), with = FALSE]
  for (key in setdiff(keys, "os1_medoid")) {
    candidate <- by_sample[library == key, c("Map", recovery), with = FALSE]
    joined <- merge(reference, candidate, by = "Map",
                    suffixes = c("_reference", "_candidate"))
    for (metric in recovery) {
      delta <- joined[[paste0(metric, "_candidate")]] -
        joined[[paste0(metric, "_reference")]]
      if (any(abs(delta[is.finite(delta)]) > 1e-9)) {
        stop("Recovery drift detected for ", key, ": ", metric)
      }
    }
  }
  candidates <- names(candidate_configs())
  paired <- data.table::rbindlist(lapply(candidates, function(key) {
    legacy$engine$paired_library_differences(by_sample, "os1_model", key)
  }), fill = TRUE)
  paired_summary <- legacy$engine$paired_metric_summary(paired)
  paired_material <- legacy$engine$paired_group_summary(
    paired, by_sample, "material_type"
  )
  paired_size <- legacy$engine$paired_group_summary(
    paired, by_sample, "size_stratum"
  )
  transitions <- data.table::rbindlist(lapply(candidates, function(key) {
    x <- legacy$pair_particle_traces(cli, definitions, "os1_model", key)
    x[, candidate := key]
    x
  }), fill = TRUE)
  transition_counts <- transitions[, .(
    specific_corrected = sum(specific_transition == "corrected"),
    specific_regressed = sum(specific_transition == "regressed"),
    broad_corrected = sum(plastic_transition == "corrected"),
    broad_regressed = sum(plastic_transition == "regressed")
  ), by = candidate]
  transition_classes <- transitions[, .(
    particles = .N
  ), by = .(
    candidate, specific_transition, plastic_transition, class_transition
  )][order(candidate, -particles)]
  margins <- legacy$match_margin_diagnostics(cli, definitions)
  calibration <- calibration_tables(margins$particles)
  development <- builder_diagnostics(cli)
  noninferiority <- noninferiority_table(
    summary, by_material, by_size, traces
  )
  failures <- traces[library %in% candidates, .(
    nonplastic_to_plastic = sum(
      material_type == "non-plastic" & !plastic_correct, na.rm = TRUE
    ),
    polyethylene_to_pva = sum(
      grepl("ethylene|olefin", SpecID, ignore.case = TRUE) &
        grepl("polyvinyl[[:space:]]*alcohol", material_class_scored,
              ignore.case = TRUE),
      na.rm = TRUE
    ),
    specific_errors = sum(!specific_correct, na.rm = TRUE)
  ), by = library]
  holdout_confusion <- traces[library %in% candidates, .(
    particles = .N
  ), by = .(
    library, sample_id, SpecID, PNID, material_class_scored,
    specific_correct, plastic_correct
  )][order(library, sample_id, -particles)]
  fit_manifests <- data.table::rbindlist(lapply(candidates, function(key) {
    data.table::fread(candidate_paths(cli, key)$manifest)
  }), fill = TRUE)
  stage1 <- data.table::rbindlist(lapply(c("os1_medoid", "os1_model"), function(key) {
    gate <- read_result(cli, key, "stage1_compatibility_gate.csv")
    data.table::data.table(
      artifact = key,
      maximum_absolute_mean_delta_pp = max(abs(gate$mean_delta_pp)),
      maximum_absolute_rsd_delta_pp = max(abs(gate$rsd_delta_pp)),
      pass = all(gate$within_one_percentage_point)
    )
  }))
  outputs <- list(
    artifact_manifests = manifests,
    metrics_summary = summary,
    metrics_by_sample = by_sample,
    metrics_by_material_type = by_material,
    metrics_by_size_stratum = by_size,
    paired_vs_os1 = paired,
    paired_summary_vs_os1 = paired_summary,
    paired_by_material_vs_os1 = paired_material,
    paired_by_size_vs_os1 = paired_size,
    noninferiority_gate = noninferiority,
    calibration_bins = calibration$bins,
    calibration_summary = calibration$summary,
    targeted_failures = failures,
    holdout_confusion_by_truth_pattern = holdout_confusion,
    builder_development_metrics = development$metrics,
    builder_class_metrics = development$classes,
    builder_selected_node_metrics = development$selected_nodes,
    particle_transitions_vs_os1 = transitions,
    particle_transition_summary = transition_classes,
    map_runtimes = runtimes,
    model_fit_manifests = fit_manifests,
    stage1_gate_summary = stage1
  )
  for (name in names(outputs)) {
    legacy$engine$write_csv(
      outputs[[name]], file.path(directory, paste0(name, ".csv"))
    )
  }
  write_report(
    cli, summary, noninferiority, paired_summary,
    calibration$summary, failures, transition_counts, development$metrics,
    runtimes,
    fit_manifests
  )
  message("Hierarchical model comparison written to: ", directory)
  invisible(outputs)
}

main <- function() {
  cli <- parse_cli()
  cli$data_root <- normalize_path(cli$data_root)
  cli$historical_root <- normalize_path(cli$historical_root)
  cli$current_medoid <- normalize_path(cli$current_medoid)
  cli$output_root <- normalize_path(cli$output_root, must_work = FALSE)
  dir.create(cli$output_root, recursive = TRUE, showWarnings = FALSE)
  switch(
    cli$stage,
    "fit" = fit_candidates(cli),
    "score-saved" = {
      invisible(legacy$score_saved(cli, "medoid"))
      invisible(legacy$score_saved(cli, "model"))
    },
    "run" = {
      if (is.null(cli$artifact)) stop("--artifact is required for --stage=run")
      run_one(cli, cli$artifact)
    },
    "probe" = run_probe(cli),
    "validate" = run_validation(cli),
    "compare" = compare_results(cli),
    stop("Unknown --stage=", cli$stage)
  )
}

if (identical(environment(), globalenv())) main()
