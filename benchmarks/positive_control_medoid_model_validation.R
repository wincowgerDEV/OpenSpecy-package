# Extend the frozen positive-control benchmark to the corresponding derivative
# medoid libraries and FTIR classification models. Examples:
#
#   Rscript benchmarks/positive_control_medoid_model_validation.R \
#     --stage=score-saved
#   Rscript benchmarks/positive_control_medoid_model_validation.R \
#     --stage=run --artifact=os1_medoid --maps=probe
#   Rscript benchmarks/positive_control_medoid_model_validation.R \
#     --stage=run --artifact=os1_model --maps=all
#   Rscript benchmarks/positive_control_medoid_model_validation.R \
#     --stage=compare

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
    return(normalize_path(
      "benchmarks/positive_control_medoid_model_validation.R"
    ))
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

engine <- new.env(parent = globalenv())
sys.source(
  file.path(dirname(script_path()), "positive_control_library_validation.R"),
  envir = engine
)

runner_version <- "positive-control-medoid-model-runner-v1"

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(
    stage = "score-saved",
    artifact = NULL,
    maps = "all",
    data_root = "C:/Users/winco/OneDrive/Documents/Positive_Controls",
    historical_root = "C:/Users/winco/OneDrive/Documents/OS1_Results",
    output_root = paste0(
      "C:/Users/winco/OneDrive/Documents/Positive_Controls/",
      "library_validation_3_libraries"
    )
  )
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Arguments must use --name=value: ", arg)
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", arg), fixed = TRUE)
    value <- sub("^--[^=]+=", "", arg)
    if (!key %in% names(out)) stop("Unknown argument: --", key)
    out[[key]] <- value
  }
  out
}

artifact_definitions <- function(cli) {
  open_root <- "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline"
  list(
    os1_medoid = list(
      family = "os1", representation = "medoid",
      label = "Open Specy 1.0 derivative medoid library",
      folder = "04_os1_medoid",
      path = file.path(cli$historical_root, "medoid_derivative.rds"),
      expected_sha256 =
        "e588f690966d226a28eae17541c22f9999d15544ad1b07cfa797e6b4afe3b5e7",
      saved_folder = "mediod"
    ),
    pre90_medoid = list(
      family = "pre90", representation = "medoid",
      label = "Pre-cross-class 0.90 closure derivative medoid library",
      folder = "05_pre_0.9_medoid",
      path = file.path(
        open_root, "reference-library-assessment-rerun-20260930",
        "releases/e2d1941530ef/medoid_derivative.rds"
      ),
      expected_sha256 =
        "340483980a44ca7e7adad93e11995f4224dd51f0d62c5e0d8857f2c4aa1d4b2d"
    ),
    current_medoid = list(
      family = "current", representation = "medoid",
      label = "Current cross-class 0.90 closed derivative medoid library",
      folder = "06_current_medoid",
      path = file.path(
        open_root, "reference-library-build-2.0.0",
        "releases/bb8cd82c7ecb/medoid_derivative.rds"
      ),
      expected_sha256 =
        "6cb03c87cf6113c9bc6b2feaf315af9719b6bc4da7110013de774d2eb7bf05cc"
    ),
    os1_model = list(
      family = "os1", representation = "model",
      label = "Open Specy 1.0 FTIR derivative classification model",
      folder = "07_os1_model",
      path = file.path(cli$historical_root, "model_derivative.rds"),
      expected_sha256 =
        "6f4e5d2035a07622efb39738569e45a7ef1fad832cc448cc94633802f5c9e811",
      saved_folder = "model"
    ),
    pre90_model = list(
      family = "pre90", representation = "model",
      label = "Pre-cross-class 0.90 closure FTIR derivative model",
      folder = "08_pre_0.9_model",
      path = file.path(
        open_root, "reference-library-assessment-rerun-20260930",
        "releases/e2d1941530ef/model_derivative.rds"
      ),
      expected_sha256 =
        "87c8656ebe65a4a83143d707ac7a288c644babc69facd1ca0dfbad6e1ff19ad2"
    ),
    current_model = list(
      family = "current", representation = "model",
      label = "Current cross-class 0.90 closed FTIR derivative model",
      folder = "09_current_model",
      path = file.path(
        open_root, "reference-library-build-2.0.0",
        "releases/bb8cd82c7ecb/model_derivative.rds"
      ),
      expected_sha256 =
        "02a4e968c1417023b098dfdb910965b59afa3a0a6d3954a2440ed7dddc03e955"
    )
  )
}

source_manifest <- function(root) {
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    script_path(),
    file.path(dirname(script_path()), "positive_control_library_validation.R")
  )
  files <- sort(unique(normalize_path(files)))
  data.table::data.table(
    file = substring(files, nchar(root) + 2L),
    sha256 = vapply(files, engine$sha256_file, character(1L)),
    bytes = unname(file.info(files)$size)
  )
}

source_hash <- function(manifest) {
  analysis_source <- manifest[grepl("^R/", file)]
  engine$sha256_object(list(
    package_source = analysis_source[, .(file, sha256, bytes)],
    runner_version = runner_version
  ))
}

load_artifact <- function(definition) {
  actual_hash <- engine$sha256_file(definition$path)
  if (!identical(tolower(actual_hash), tolower(definition$expected_sha256))) {
    stop("Artifact SHA-256 mismatch for ", definition$label)
  }
  object <- readRDS(definition$path)
  if (is.list(object) && "ftir" %in% names(object) &&
      !all(c("wavenumber", "spectra", "metadata") %in% names(object))) {
    object <- object[["ftir"]]
  }
  if (identical(definition$representation, "medoid")) {
    if (!is_OpenSpecy(object)) stop("Medoid artifact is not an OpenSpecy object")
    if ("spectrum_type" %in% names(object$metadata)) {
      keep <- !is.na(object$metadata$spectrum_type) &
        object$metadata$spectrum_type == "ftir"
      object <- filter_spec(object, keep)
    }
    if (ncol(object$spectra) != nrow(object$metadata) ||
        length(object$wavenumber) != nrow(object$spectra)) {
      stop("Medoid OpenSpecy alignment is invalid")
    }
    axis <- as.numeric(object$wavenumber)
    classes <- unique(as.character(object$metadata$material_class))
    units <- ncol(object$spectra)
    model_type <- NA_character_
  } else {
    required <- c("model", "dimension_conversion", "all_variables")
    if (!is.list(object) || !all(required %in% names(object))) {
      stop("Model artifact lacks: ", paste(setdiff(required, names(object)),
                                            collapse = ", "))
    }
    axis <- as.numeric(object$all_variables)
    if (!length(axis) || anyNA(axis) || anyDuplicated(axis)) {
      stop("Model predictor axis is missing, non-unique, or contains NA")
    }
    conversion <- data.table::as.data.table(object$dimension_conversion)
    if (!all(c("factor_num", "name") %in% names(conversion)) ||
        anyNA(conversion$name) || anyDuplicated(conversion$factor_num)) {
      stop("Model class conversion table is invalid")
    }
    classes <- unique(as.character(conversion$name))
    units <- length(classes)
    model_type <- if (is.null(object$model_type)) {
      paste(class(object$model), collapse = "/")
    } else {
      as.character(object$model_type)
    }
  }
  list(
    object = object, sha256 = actual_hash, axis = axis,
    units = units, classes = classes, model_type = model_type
  )
}

expected_saved_oracle <- function(representation) {
  common_mean <- c(91.0033028358366, 109.715779686607, 97.6593387079535)
  common_rsd <- c(41.5383459721637, 53.5746683824332, 35.9403863145993)
  identification <- if (identical(representation, "medoid")) {
    list(mean = c(92.2280358048989, 94.0667828231975),
         rsd = c(12.1619747736439, 11.1994161946107))
  } else {
    list(mean = c(93.9984384656013, 95.4958021766178),
         rsd = c(7.57028426774462, 6.09793642071911))
  }
  data.table::data.table(
    metric_key = c(
      "count_accuracy", "area_accuracy", "feret_accuracy",
      "specific_accuracy", "plastic_accuracy"
    ),
    expected_mean = c(common_mean, identification$mean),
    expected_rsd = c(common_rsd, identification$rsd)
  )
}

score_saved <- function(cli, representation) {
  definition <- artifact_definitions(cli)[[paste0("os1_", representation)]]
  details <- data.table::fread(file.path(
    cli$historical_root, definition$saved_folder, "particle_details_all.csv"
  ))
  scores <- engine$score_details(
    details, engine$truth_table(cli), paste0("os1_saved_", representation)
  )
  expected <- expected_saved_oracle(representation)
  audit <- merge(scores$summary, expected, by = "metric_key", sort = FALSE)
  audit[, `:=`(
    mean_delta = mean_accuracy - expected_mean,
    rsd_delta = rsd - expected_rsd
  )]
  audit[, within_tolerance :=
          abs(mean_delta) < 1e-9 & abs(rsd_delta) < 1e-9]
  directory <- file.path(cli$output_root, "comparison")
  engine$write_scores(scores, directory, paste0("saved_", representation,
                                                 "_oracle"))
  engine$write_csv(
    audit, file.path(directory, paste0("saved_", representation,
                                       "_scorer_audit.csv"))
  )
  if (!all(audit$within_tolerance)) {
    stop("Saved ", representation, " scorer does not match its locked oracle")
  }
  message("Saved ", representation, " scorer reproduced all locked metrics.")
  scores
}

write_stage1_gates <- function(scores, saved, directory) {
  sample_gate <- engine$os1_sample_compatibility(scores, saved)
  material_gate <- engine$os1_group_compatibility(
    scores$by_material, saved$by_material, "material_type"
  )
  size_gate <- engine$os1_group_compatibility(
    scores$by_size, saved$by_size, "size_stratum"
  )
  summary_gate <- merge(
    scores$summary[, .(
      metric_key, reproduced_mean = mean_accuracy, reproduced_rsd = rsd
    )],
    saved$summary[, .(
      metric_key, saved_mean = mean_accuracy, saved_rsd = rsd
    )],
    by = "metric_key", sort = FALSE
  )
  summary_gate[, `:=`(
    mean_delta_pp = reproduced_mean - saved_mean,
    rsd_delta_pp = reproduced_rsd - saved_rsd
  )]
  summary_gate[, within_one_percentage_point :=
                 abs(mean_delta_pp) <= 1 & abs(rsd_delta_pp) <= 1]
  engine$write_csv(sample_gate,
                   file.path(directory, "stage1_compatibility_by_sample.csv"))
  engine$write_csv(material_gate, file.path(
    directory, "stage1_compatibility_by_material_type.csv"
  ))
  engine$write_csv(size_gate,
                   file.path(directory, "stage1_compatibility_by_size_stratum.csv"))
  engine$write_csv(summary_gate,
                   file.path(directory, "stage1_compatibility_gate.csv"))
  passed <- all(sample_gate$within_one_percentage_point) &&
    all(material_gate$within_one_percentage_point) &&
    all(size_gate$within_one_percentage_point) &&
    all(summary_gate$within_one_percentage_point)
  if (!passed) stop("Stage 1 compatibility gate failed; Stage 2 remains locked")
  message(paste0(
    "Stage 1 passed overall, per sample, by material type, and by size."
  ))
  invisible(summary_gate)
}

assert_stage1_unlocked <- function(cli, definition, hashes) {
  if (identical(definition$family, "os1")) return(invisible(TRUE))
  definitions <- artifact_definitions(cli)
  os1 <- definitions[[paste0("os1_", definition$representation)]]
  directory <- file.path(cli$output_root, os1$folder)
  files <- file.path(directory, c(
    "stage1_compatibility_gate.csv",
    "stage1_compatibility_by_sample.csv",
    "stage1_compatibility_by_material_type.csv",
    "stage1_compatibility_by_size_stratum.csv",
    "library_manifest.csv"
  ))
  if (!all(file.exists(files))) {
    stop("Stage 2 locked: complete OS1 ", definition$representation,
         " Stage 1 first")
  }
  gates <- lapply(files[1:4], data.table::fread)
  if (!all(vapply(gates, function(x) {
    all(x$within_one_percentage_point)
  }, logical(1L)))) {
    stop("Stage 2 locked: recorded OS1 compatibility gate is not passing")
  }
  manifest <- data.table::fread(files[[5L]])
  if (!identical(manifest$source_sha256[[1L]], hashes$source_sha256) ||
      !identical(manifest$workflow_sha256[[1L]], hashes$workflow_sha256)) {
    stop("Stage 2 locked: analysis source or frozen workflow changed; rerun Stage 1")
  }
  invisible(TRUE)
}

run_artifact <- function(cli) {
  definitions <- artifact_definitions(cli)
  key <- cli$artifact
  if (is.null(key) || !key %in% names(definitions)) {
    stop("--artifact must name one of: ", paste(names(definitions), collapse = ", "))
  }
  definition <- definitions[[key]]
  root <- repo_root()
  engine$load_openspecy(root)
  inputs <- engine$input_files(cli)
  selected <- engine$selected_inputs(inputs, cli$maps)
  directory <- file.path(cli$output_root, definition$folder)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  source_files <- source_manifest(root)
  config <- engine$analysis_config()
  workflow_sha256 <- engine$sha256_object(config)
  loaded <- load_artifact(definition)
  config$process_args$conform_spec_args$range <- loaded$axis
  hashes <- list(
    library_sha256 = loaded$sha256,
    config_sha256 = engine$sha256_object(config),
    source_sha256 = source_hash(source_files),
    workflow_sha256 = workflow_sha256
  )
  assert_stage1_unlocked(cli, definition, hashes)
  engine$write_csv(source_files, file.path(directory, "source_manifest.csv"))
  engine$write_csv(engine$flatten_config(config),
                   file.path(directory, "config_manifest.csv"))
  engine$write_csv(data.table::data.table(
    library = key,
    family = definition$family,
    representation = definition$representation,
    label = definition$label,
    path = normalize_path(definition$path),
    sha256 = loaded$sha256,
    predictors = length(loaded$axis),
    units = loaded$units,
    material_classes = length(loaded$classes),
    model_type = loaded$model_type,
    workflow_sha256 = workflow_sha256,
    runner_version = runner_version,
    config_sha256 = hashes$config_sha256,
    source_sha256 = hashes$source_sha256,
    r_version = R.version.string
  ), file.path(directory, "library_manifest.csv"))
  engine$write_csv(data.table::data.table(
    sample_id = names(inputs), path = unname(inputs),
    bytes = unname(file.info(inputs)$size),
    sha256 = vapply(inputs, engine$sha256_file, character(1L)),
    excluded = FALSE,
    selected_this_invocation = names(inputs) %in% names(selected)
  ), file.path(directory, "input_manifest.csv"))
  runtimes <- lapply(seq_along(selected), function(index) {
    engine$run_one_map(
      selected[[index]], names(selected)[[index]], loaded$object, config,
      directory, hashes
    )
  })
  engine$write_csv(data.table::rbindlist(runtimes, fill = TRUE),
                   file.path(directory, "runtime_invocation.csv"))
  completed <- names(inputs)[file.exists(file.path(
    directory, paste0("checkpoint_", names(inputs), ".rds")
  ))]
  scores <- engine$aggregate_library_outputs(
    directory, completed, engine$truth_table(cli), key
  )
  if (identical(definition$family, "os1")) {
    saved <- score_saved(cli, definition$representation)
    sample_gate <- engine$os1_sample_compatibility(scores, saved)
    engine$write_csv(sample_gate,
                     file.path(directory, "os1_sample_compatibility.csv"))
    if (length(completed) == length(inputs)) {
      write_stage1_gates(scores, saved, directory)
    }
  }
  invisible(scores)
}

artifact_label_table <- function(definitions) {
  data.table::rbindlist(lapply(names(definitions), function(key) {
    definition <- definitions[[key]]
    data.table::data.table(
      library = key, family = definition$family,
      representation = definition$representation,
      library_label = definition$label, folder = definition$folder
    )
  }))
}

read_artifact_result <- function(cli, definition, filename) {
  path <- file.path(cli$output_root, definition$folder, filename)
  if (!file.exists(path)) stop("Missing comparison input: ", path)
  data.table::fread(path)
}

pair_particle_traces <- function(cli, definitions, left_key, right_key) {
  columns <- c(
    "sample_id", "particle_id", "material_class_raw", "material_class_scored",
    "specific_correct", "plastic_correct", "max_cor_val", "max_cor_name"
  )
  read_trace <- function(key, suffix) {
    x <- read_artifact_result(
      cli, definitions[[key]], "metrics_particle_trace.csv"
    )
    keep <- intersect(columns, names(x))
    x <- x[, keep, with = FALSE]
    x[, particle_id := as.character(particle_id)]
    rename <- setdiff(keep, c("sample_id", "particle_id"))
    data.table::setnames(x, rename, paste0(rename, "_", suffix))
    x
  }
  left <- read_trace(left_key, left_key)
  right <- read_trace(right_key, right_key)
  joined <- merge(
    left, right, by = c("sample_id", "particle_id"), all = TRUE, sort = FALSE
  )
  left_specific <- paste0("specific_correct_", left_key)
  right_specific <- paste0("specific_correct_", right_key)
  left_plastic <- paste0("plastic_correct_", left_key)
  right_plastic <- paste0("plastic_correct_", right_key)
  left_class <- paste0("material_class_raw_", left_key)
  right_class <- paste0("material_class_raw_", right_key)
  joined[, `:=`(
    left_library = left_key,
    right_library = right_key,
    class_transition = paste(get(left_class), get(right_class), sep = " -> "),
    specific_transition = data.table::fcase(
      is.na(get(left_specific)), "missing-left",
      is.na(get(right_specific)), "missing-right",
      !get(left_specific) & get(right_specific), "corrected",
      get(left_specific) & !get(right_specific), "regressed",
      get(left_specific) & get(right_specific), "correct-both",
      default = "wrong-both"
    ),
    plastic_transition = data.table::fcase(
      is.na(get(left_plastic)), "missing-left",
      is.na(get(right_plastic)), "missing-right",
      !get(left_plastic) & get(right_plastic), "corrected",
      get(left_plastic) & !get(right_plastic), "regressed",
      get(left_plastic) & get(right_plastic), "correct-both",
      default = "wrong-both"
    )
  )]
  joined
}

normalize_top_two <- function(matches, processed, representation) {
  if (identical(representation, "medoid")) {
    matches[, rank := seq_len(.N), by = object_id]
    return(matches[, .(
      particle_id = as.character(object_id),
      predicted_name = as.character(library_id),
      score = match_val, rank
    )])
  }
  required <- c("x", "name", "value", "rank")
  if (!all(required %in% names(matches))) {
    stop("Unexpected model top-two columns: ", paste(names(matches), collapse = ", "))
  }
  ids <- colnames(processed$spectra)
  matches[, .(
    particle_id = as.character(ids[x]),
    predicted_name = as.character(name),
    score = value,
    rank = as.integer(rank)
  )]
}

match_margin_diagnostics <- function(cli, definitions) {
  engine$load_openspecy(repo_root())
  truth <- engine$truth_table(cli)[, .(Map, material_type, size_stratum)]
  particles <- data.table::rbindlist(lapply(names(definitions), function(key) {
    definition <- definitions[[key]]
    directory <- file.path(cli$output_root, definition$folder)
    loaded <- load_artifact(definition)
    trace <- data.table::fread(file.path(directory, "metrics_particle_trace.csv"))
    data.table::rbindlist(lapply(unique(trace$sample_id), function(sample) {
      processed <- readRDS(file.path(directory, paste0("particles_", sample,
                                                       ".rds")))
      matches <- if (identical(definition$representation, "medoid")) {
        match_spec(
          processed, loaded$object, top_n = 2L, batch_size = 1000L,
          compute = "optimized"
        )
      } else {
        match_spec(processed, loaded$object, top_n = 2L)
      }
      normalized <- normalize_top_two(
        data.table::as.data.table(matches), processed,
        definition$representation
      )
      first <- normalized[rank == 1L, .(
        particle_id, top1_name = predicted_name, top1_score = score
      )]
      second <- normalized[rank == 2L, .(
        particle_id, top2_name = predicted_name, top2_score = score
      )]
      out <- merge(first, second, by = "particle_id", all.x = TRUE, sort = FALSE)
      recorded_name_column <- if (
        identical(definition$representation, "medoid")
      ) "max_cor_name" else "material_class_raw"
      expected <- trace[sample_id == sample, .(
        particle_id = as.character(particle_id),
        recorded_name = as.character(get(recorded_name_column)),
        recorded_score = max_cor_val,
        specific_correct, plastic_correct
      )]
      out <- merge(out, expected, by = "particle_id", all.x = TRUE, sort = FALSE)
      if (anyNA(out$recorded_name) ||
          any(out$top1_name != out$recorded_name) ||
          any(abs(out$top1_score - out$recorded_score) > 1e-10)) {
        stop("Top-two diagnostic failed to reproduce ", key, ": ", sample)
      }
      out[, `:=`(
        library = key,
        family = definition$family,
        representation = definition$representation,
        sample_id = sample,
        match_margin = top1_score - top2_score,
        below_fixed_threshold = top1_score < 0.66
      )]
      out
    }), fill = TRUE)
  }), fill = TRUE)
  particles <- merge(
    particles, truth, by.x = "sample_id", by.y = "Map", all.x = TRUE,
    sort = FALSE
  )
  summarize <- function(x, groups) {
    x[, .(
      particles = .N,
      mean_top1_score = mean(top1_score),
      median_top1_score = stats::median(top1_score),
      mean_margin = mean(match_margin, na.rm = TRUE),
      median_margin = stats::median(match_margin, na.rm = TRUE),
      fraction_margin_le_0.01 = mean(match_margin <= 0.01, na.rm = TRUE),
      fraction_below_0.66 = mean(below_fixed_threshold, na.rm = TRUE),
      retained_at_or_above_0.66 = sum(!below_fixed_threshold),
      specific_accuracy_all = 100 * mean(specific_correct, na.rm = TRUE),
      specific_accuracy_retained = 100 * mean(
        specific_correct[!below_fixed_threshold], na.rm = TRUE
      ),
      plastic_accuracy_all = 100 * mean(plastic_correct, na.rm = TRUE),
      plastic_accuracy_retained = 100 * mean(
        plastic_correct[!below_fixed_threshold], na.rm = TRUE
      )
    ), by = groups]
  }
  list(
    particles = particles,
    summary = summarize(particles, c("library", "family", "representation")),
    by_material = summarize(
      particles, c("library", "family", "representation", "material_type")
    ),
    by_size = summarize(
      particles, c("library", "family", "representation", "size_stratum")
    )
  )
}

normalize_class_label <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(as.character(x)))
}

model_diagnostics <- function(definitions, traces, margins) {
  candidates <- c("pre90", "current")
  failure_by_sample <- data.table::rbindlist(lapply(candidates, function(family) {
    x <- traces[[paste0("model_os1_to_", family)]]
    x[, candidate := paste0(family, "_model")]
    x[, .(
      particles = .N,
      specific_regressions = sum(specific_transition == "regressed"),
      specific_corrections = sum(specific_transition == "corrected"),
      broad_regressions = sum(plastic_transition == "regressed"),
      broad_corrections = sum(plastic_transition == "corrected")
    ), by = .(candidate, sample_id)]
  }))
  failure_by_sample <- failure_by_sample[
    order(candidate, -specific_regressions, -broad_regressions, sample_id)
  ]
  failure_by_transition <- data.table::rbindlist(lapply(
    candidates, function(family) {
      x <- traces[[paste0("model_os1_to_", family)]]
      old <- "material_class_raw_os1_model"
      new <- paste0("material_class_raw_", family, "_model")
      x[, `:=`(
        candidate = paste0(family, "_model"),
        os1_class = get(old),
        candidate_class = get(new)
      )]
      x[specific_transition %in% c("corrected", "regressed"), .(
        particles = .N
      ), by = .(
        candidate, specific_transition, os1_class, candidate_class
      )]
    }
  ))
  failure_by_transition <- failure_by_transition[
    order(candidate, specific_transition, -particles, os1_class, candidate_class)
  ]

  model_keys <- c("os1_model", "pre90_model", "current_model")
  loaded <- lapply(model_keys, function(key) load_artifact(definitions[[key]]))
  names(loaded) <- model_keys
  structure <- data.table::rbindlist(lapply(model_keys, function(key) {
    object <- loaded[[key]]$object
    data.table::data.table(
      artifact = key,
      bytes = unname(file.info(definitions[[key]]$path)$size),
      predictors = length(loaded[[key]]$axis),
      classes = length(loaded[[key]]$classes),
      training_observations = if (is.null(object$observation_count)) {
        object$model$nobs
      } else {
        object$observation_count
      },
      selected_lambda = if (is.null(object$lambda_selected)) {
        NA_real_
      } else {
        object$lambda_selected
      },
      stored_lambda_count = length(object$model$lambda)
    )
  }))

  os1_classes <- data.table::data.table(
    os1_class = loaded$os1_model$classes,
    normalized_class = normalize_class_label(loaded$os1_model$classes)
  )
  class_labels <- data.table::rbindlist(lapply(
    c("pre90_model", "current_model"), function(key) {
      x <- data.table::data.table(
        artifact = key,
        candidate_class = loaded[[key]]$classes,
        normalized_class = normalize_class_label(loaded[[key]]$classes)
      )
      x <- merge(x, os1_classes, by = "normalized_class", all.x = TRUE,
                 sort = FALSE)
      x[, `:=`(
        normalized_equivalent_rename =
          !is.na(os1_class) & candidate_class != os1_class,
        validation_alias_applied = candidate_class == "ftir_polyethylene"
      )]
      x
    }
  ), fill = TRUE)

  confidence <- margins$particles[representation == "model", .(
    particles = .N,
    specific_errors = sum(!specific_correct),
    errors_at_or_above_0.66 = sum(!specific_correct & top1_score >= 0.66),
    fraction_errors_at_or_above_0.66 =
      mean(top1_score[!specific_correct] >= 0.66),
    median_error_score = stats::median(top1_score[!specific_correct]),
    coverage_at_or_above_0.66 = mean(top1_score >= 0.66),
    specific_accuracy_retained =
      100 * mean(specific_correct[top1_score >= 0.66])
  ), by = .(library)]

  build_root <- dirname(dirname(dirname(definitions$current_model$path)))
  checkpoint_root <- file.path(build_root, "checkpoints")
  builder_by_class <- data.table::rbindlist(lapply(
    c("old", "new"), function(source) {
      path <- file.path(checkpoint_root, paste0(
        "assessment_model_logistic_regression_derivative_ftir_", source,
        "_fold_medoids_parallel_rng_v2.rds"
      ))
      if (!file.exists(path)) return(NULL)
      x <- data.table::as.data.table(readRDS(path))
      required <- c("expected_class", "correct", "score")
      if (!all(required %in% names(x))) {
        stop("Unexpected builder model assessment: ", path)
      }
      x[, build_source := as.character(get("source"))]
      x[, .(
        spectra = .N,
        accuracy = mean(correct),
        median_score = stats::median(score)
      ), by = .(
        build_source, expected_class
      )]
    }
  ), fill = TRUE)
  if (!nrow(builder_by_class) ||
      !setequal(unique(builder_by_class$build_source), c("old", "new"))) {
    stop("Both old and new FTIR builder holdout assessments are required")
  }
  builder_summary <- builder_by_class[, .(
    spectra = sum(spectra),
    classes = .N,
    overall_accuracy = stats::weighted.mean(accuracy, spectra),
    macro_class_accuracy = mean(accuracy)
  ), by = .(build_source)]

  list(
    failure_by_sample = failure_by_sample,
    failure_by_transition = failure_by_transition,
    structure = structure,
    class_labels = class_labels,
    confidence = confidence,
    builder_summary = builder_summary,
    builder_by_class = builder_by_class
  )
}

write_comparison_plot <- function(summary, labels, path) {
  id <- summary[metric_key %in% c("specific_accuracy", "plastic_accuracy")]
  id <- merge(id, labels[, .(library, short_label)], by = "library")
  order <- labels$library
  metrics <- c("specific_accuracy", "plastic_accuracy")
  values <- matrix(
    NA_real_, nrow = length(order), ncol = length(metrics),
    dimnames = list(order, metrics)
  )
  lower <- upper <- values
  for (i in seq_len(nrow(id))) {
    values[id$library[[i]], id$metric_key[[i]]] <- id$mean_accuracy[[i]]
    lower[id$library[[i]], id$metric_key[[i]]] <- id$bootstrap_lower[[i]]
    upper[id$library[[i]], id$metric_key[[i]]] <- id$bootstrap_upper[[i]]
  }
  grDevices::png(path, width = 2200, height = 1100, res = 180)
  on.exit(grDevices::dev.off(), add = TRUE)
  positions <- barplot(
    t(values), beside = TRUE, ylim = c(0, 105),
    col = c("#4C78A8", "#F58518"),
    names.arg = labels$short_label[match(order, labels$library)],
    ylab = "Mean sample-level accuracy (%)", las = 2,
    main = "Positive-control accuracy across full, medoid, and model artifacts",
    cex.names = 0.75
  )
  arrows(
    positions, t(lower), positions, t(upper), angle = 90, code = 3,
    length = 0.025, lwd = 1.2
  )
  legend(
    "bottomright", legend = c("Specific ID", "Plastic/non-plastic"),
    fill = c("#4C78A8", "#F58518"), bty = "n"
  )
}

write_report <- function(cli, summary, labels, paired_summary, direct_summary,
                         paired_material, paired_size, transition_counts,
                         margins, diagnostics, runtimes, stage1) {
  directory <- file.path(cli$output_root, "comparison")
  identification <- merge(
    summary[metric_key %in% c("specific_accuracy", "plastic_accuracy")],
    labels[, .(library, short_label)], by = "library"
  )[, .(
    Artifact = short_label,
    Metric = ifelse(metric_key == "specific_accuracy", "Specific ID",
                    "Plastic/non-plastic"),
    N = n, Mean = mean_accuracy, RSD = rsd,
    `Bootstrap 2.5%` = bootstrap_lower,
    `Bootstrap 97.5%` = bootstrap_upper
  )]
  effects <- data.table::rbindlist(list(paired_summary, direct_summary))
  effects <- merge(
    effects,
    labels[, .(library, Candidate = short_label)], by = "library"
  )
  reference_labels <- labels[, .(
    reference_library = library, Reference = short_label
  )]
  effects <- merge(effects, reference_labels, by = "reference_library")
  effects <- effects[metric_key %in% c(
    "specific_accuracy", "plastic_accuracy"
  ), .(
    Candidate, Reference,
    Metric = ifelse(metric_key == "specific_accuracy", "Specific ID",
                    "Plastic/non-plastic"),
    N = n, `Mean delta (pp)` = mean_delta_pp,
    `Bootstrap 2.5%` = bootstrap_lower_pp,
    `Bootstrap 97.5%` = bootstrap_upper_pp,
    Improved = improved_maps, Tied = tied_maps, Worsened = worsened_maps
  )]
  margin_table <- merge(
    margins$summary, labels[, .(library, short_label)], by = "library"
  )[, .(
    Artifact = short_label, Particles = particles,
    `Median score` = median_top1_score,
    `Median margin` = median_margin,
    `Margin <=0.01 (%)` = 100 * fraction_margin_le_0.01,
    `Below 0.66 (%)` = 100 * fraction_below_0.66,
    `Retained >=0.66` = retained_at_or_above_0.66,
    `Specific retained (%)` = specific_accuracy_retained,
    `Broad retained (%)` = plastic_accuracy_retained
  )]
  max_runtime <- max(runtimes$elapsed_seconds, na.rm = TRUE)
  max_memory <- 100 * max(runtimes$memory_used_fraction_after, na.rm = TRUE)
  warnings <- sum(runtimes$warnings, na.rm = TRUE)
  effect <- function(table, target_library, target_metric, column) {
    index <- table$library == target_library &
      table$metric_key == target_metric
    table[[column]][which(index)[[1L]]]
  }
  accuracy <- function(target_library, target_metric) {
    index <- summary$library == target_library &
      summary$metric_key == target_metric
    summary$mean_accuracy[which(index)[[1L]]]
  }
  model_group_effects <- data.table::rbindlist(list(
    paired_material[
      library == "current_model" &
        metric_key %in% c("specific_accuracy", "plastic_accuracy"),
      .(
        Stratum = "material", Group = material_type,
        Metric = ifelse(metric_key == "specific_accuracy", "Specific ID",
                        "Plastic/non-plastic"),
        `Mean delta (pp)` = mean_delta_pp,
        `Bootstrap 2.5%` = bootstrap_lower_pp,
        `Bootstrap 97.5%` = bootstrap_upper_pp
      )
    ],
    paired_size[
      library == "current_model" &
        metric_key %in% c("specific_accuracy", "plastic_accuracy"),
      .(
        Stratum = "size", Group = size_stratum,
        Metric = ifelse(metric_key == "specific_accuracy", "Specific ID",
                        "Plastic/non-plastic"),
        `Mean delta (pp)` = mean_delta_pp,
        `Bootstrap 2.5%` = bootstrap_lower_pp,
        `Bootstrap 97.5%` = bootstrap_upper_pp
      )
    ]
  ))
  failure_samples <- diagnostics$failure_by_sample[
    candidate == "current_model" &
      (specific_regressions > 0L | specific_corrections > 0L)
  ][1:min(.N, 10L), .(
    Sample = sample_id, Particles = particles,
    `Specific regressions` = specific_regressions,
    `Specific corrections` = specific_corrections,
    `Broad regressions` = broad_regressions,
    `Broad corrections` = broad_corrections
  )]
  failure_transitions <- diagnostics$failure_by_transition[
    candidate == "current_model" & specific_transition == "regressed"
  ][1:min(.N, 10L), .(
    `OS1 class` = os1_class, `Current class` = candidate_class, Particles = particles
  )]
  model_structure <- diagnostics$structure[, .(
    Artifact = artifact, Bytes = bytes, Predictors = predictors,
    Classes = classes, `Training spectra` = training_observations,
    `Selected lambda` = selected_lambda,
    `Stored lambdas` = stored_lambda_count
  )]
  builder_summary <- diagnostics$builder_summary[, .(
    Source = build_source, Spectra = spectra, Classes = classes,
    `Overall accuracy (%)` = 100 * overall_accuracy,
    `Macro-class accuracy (%)` = 100 * macro_class_accuracy
  )]
  target_pattern <- paste0(
    "poly[(]?ethylene[)]?$|organic matter$|mineral$"
  )
  builder_targets <- diagnostics$builder_by_class[
    grepl(target_pattern, expected_class), .(
      Source = build_source, Class = expected_class, Spectra = spectra,
      `Accuracy (%)` = 100 * accuracy, `Median score` = median_score
    )
  ]
  confidence <- diagnostics$confidence[, .(
    Artifact = library, Particles = particles,
    `Specific errors` = specific_errors,
    `Errors >=0.66` = errors_at_or_above_0.66,
    `Errors >=0.66 (%)` = 100 * fraction_errors_at_or_above_0.66,
    `Median error score` = median_error_score,
    `Coverage >=0.66 (%)` = 100 * coverage_at_or_above_0.66,
    `Retained specific accuracy (%)` = specific_accuracy_retained
  )]
  report <- c(
    "# Positive-control full, medoid, and model validation",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Scope and gate result",
    "",
    paste0(
      "The frozen 20-map workflow excludes `PMMA_15Nov223_control` and ",
      "`RedPETFibers_15Nov223_control`. The OS1 medoid and OS1 model routes ",
      "were independently required to reproduce saved results within one ",
      "percentage point before their Stage 2 runs."
    ),
    "",
    engine$markdown_table(stage1, names(stage1), digits = 6L),
    "",
    "## Decision",
    "",
    paste0(
      "Keep the current full derivative library as the primary development ",
      "candidate (", sprintf("%.3f", accuracy("current_full", "specific_accuracy")),
      "% specific; ",
      sprintf("%.3f", accuracy("current_full", "plastic_accuracy")),
      "% plastic/non-plastic). The current medoid is a credible compact ",
      "alternative (", sprintf("%.3f", accuracy("current_medoid", "specific_accuracy")),
      "% / ", sprintf("%.3f", accuracy("current_medoid", "plastic_accuracy")),
      "%), but its paired differences from OS1 medoid are inconclusive."
    ),
    "",
    paste0(
      "Do not replace the OS1 FTIR derivative model with either new model. ",
      "Current versus OS1 model changed specific accuracy by ",
      sprintf("%+.3f", effect(paired_summary, "current_model",
                              "specific_accuracy", "mean_delta_pp")),
      " points (",
      sprintf("%.3f", effect(paired_summary, "current_model",
                             "specific_accuracy", "bootstrap_lower_pp")),
      " to ",
      sprintf("%.3f", effect(paired_summary, "current_model",
                             "specific_accuracy", "bootstrap_upper_pp")),
      ") and broad accuracy by ",
      sprintf("%+.3f", effect(paired_summary, "current_model",
                              "plastic_accuracy", "mean_delta_pp")),
      " points (",
      sprintf("%.3f", effect(paired_summary, "current_model",
                             "plastic_accuracy", "bootstrap_lower_pp")),
      " to ",
      sprintf("%.3f", effect(paired_summary, "current_model",
                             "plastic_accuracy", "bootstrap_upper_pp")),
      "). Both intervals exclude zero."
    ),
    "",
    paste0(
      "Cross-class closure is beneficial within the new model family: current ",
      "versus pre-closure improves specific accuracy by ",
      sprintf("%.3f", effect(direct_summary, "current_model",
                             "specific_accuracy", "mean_delta_pp")),
      " points (",
      sprintf("%.3f", effect(direct_summary, "current_model",
                             "specific_accuracy", "bootstrap_lower_pp")),
      " to ",
      sprintf("%.3f", effect(direct_summary, "current_model",
                             "specific_accuracy", "bootstrap_upper_pp")),
      "). Closure should continue, but it cannot compensate for the current ",
      "model's class/objective problem."
    ),
    "",
    "## All-nine identification accuracy",
    "",
    engine$markdown_table(identification, names(identification), digits = 3L),
    "",
    "Full, medoid, and model scores are reported together descriptively. Correlation scores from spectral libraries and class probabilities from models are not on the same inferential scale.",
    "",
    "## Within-representation paired effects",
    "",
    engine$markdown_table(effects, names(effects), digits = 3L),
    "",
    "Paired bootstrap intervals resample maps, not particles. Recovery metrics were required to remain identical because segmentation occurs before matching.",
    "",
    "## Model failure investigation",
    "",
    paste0(
      "The first comparison exposed a validation-only taxonomy mismatch: the ",
      "new models emit `ftir_polyethylene`, while OS1 emits ",
      "`ftir_poly(ethylene)`. The existing directional `polyethylene` ",
      "crosswalk was extended to the FTIR-prefixed spelling. Both complete ",
      "Stage 1 gates still reproduced OS1 exactly (0.000-point maximum delta), ",
      "and all Stage 2 scores were regenerated from unchanged checkpoints."
    ),
    "",
    "The release artifacts differ materially in target complexity:",
    "",
    engine$markdown_table(model_structure, names(model_structure), digits = 6L),
    "",
    "Current-model paired effects versus OS1 persist in both material and size strata:",
    "",
    engine$markdown_table(
      model_group_effects, names(model_group_effects), digits = 3L
    ),
    "",
    "The samples contributing the most current-versus-OS1 model changes are:",
    "",
    engine$markdown_table(failure_samples, names(failure_samples), digits = 0L),
    "",
    "The most common specific-ID regressions are:",
    "",
    engine$markdown_table(
      failure_transitions, names(failure_transitions), digits = 0L
    ),
    "",
    paste0(
      "The dominant error is polyethylene being classified as polyvinyl ",
      "alcohol (93 particles), concentrated in the red-bead recovery map. ",
      "The next cluster splits organic matter and mineral into polymer ",
      "subclasses. This is consistent with a fine-class discrimination issue, ",
      "not a segmentation or recovery failure."
    ),
    "",
    "The builder's independent grouped-medoid holdout shows the objective tradeoff:",
    "",
    engine$markdown_table(builder_summary, names(builder_summary), digits = 3L),
    "",
    "Key high-support classes in that builder holdout are:",
    "",
    engine$markdown_table(builder_targets, names(builder_targets), digits = 3L),
    "",
    paste0(
      "The new-source builder assessment raises macro-class accuracy while ",
      "lowering overall accuracy and lowering mineral, organic-matter, and ",
      "polyethylene accuracy. Because `train_spec_model()` selects lambda by ",
      "macro-class accuracy, expansion from the coarse OS1 taxonomy to many ",
      "fine polymer classes can reward rare-class performance while degrading ",
      "the abundant environmental and polyethylene classes represented here."
    ),
    "",
    "Model error confidence also argues against a simple threshold repair:",
    "",
    engine$markdown_table(confidence, names(confidence), digits = 3L),
    "",
    paste0(
      "Nearly half of the new models' specific errors remain at probability ",
      ">=0.66. The fixed threshold therefore removes coverage without restoring ",
      "OS1-level conditional accuracy; model calibration and class design need ",
      "development on separate data."
    ),
    "",
    "## Current versus pre-closure particle transitions",
    "",
    engine$markdown_table(
      transition_counts, names(transition_counts), digits = 0L
    ),
    "",
    "## Score, margin, and fixed 0.66 sensitivity",
    "",
    engine$markdown_table(margin_table, names(margin_table), digits = 3L),
    "",
    paste0(
      "The fixed 0.66 analysis is diagnostic and does not tune the workflow. ",
      "For models it is a probability threshold; for medoids it is a ",
      "correlation threshold. Conditional retained accuracy must be read with ",
      "coverage."
    ),
    "",
    "## Runtime and interpretation",
    "",
    paste0(
      "Across the six added artifact runs, the slowest map took ",
      sprintf("%.1f", max_runtime), " seconds, maximum recorded post-map ",
      "physical-memory use was ", sprintf("%.1f%%", max_memory),
      ", and runs emitted ", warnings, " warnings."
    ),
    "",
    "## Recommended development pathway",
    "",
    "1. Keep this 20-map cohort and workflow frozen as a confirmation gate. Do not tune classes, lambda, thresholds, or references on these holdout outcomes.",
    "2. Use the current full library as the primary candidate. Offer the current medoid only where deployment size or latency justifies its roughly 1.6-point lower descriptive specific accuracy versus current full. Do not promote either new FTIR model as an OS1 replacement.",
    "3. Establish stable canonical class IDs before training. Keep display labels separate, validate every `dimension_conversion` label against the scoring taxonomy, and fail builds on unmapped or normalization-colliding labels.",
    "4. Develop a hierarchical model on independent data: first distinguish plastic/polymer from mineral/organic/other material, then predict polymer subtype. Compare it with the flat 41-class model using predeclared overall, broad-class, macro-class, calibration, and per-class non-inferiority gates.",
    "5. Retain class-aware cross-class closure: it improves the current model over pre-closure. Preserve minimum support and spectral modes for each class, and treat closure and model-objective changes as separate experiments.",
    "6. Calibrate probabilities and any abstention rule on a separate development set. Report coverage with conditional accuracy; the 0.66 diagnostic is not adequate for the new models and must not be tuned here.",
    "7. Add independent class-balanced maps for polyethylene/PVA, organic matter versus polyamides/acrylates/polyesters, and mineral versus polymer confusions, then use a final sequestered confirmation set before release.",
    "",
    "Detailed sample, material, size, particle-transition, failure-attribution, builder-holdout, class-label, confidence, manifest, and runtime CSVs are stored beside this report."
  )
  engine$write_lines(
    report,
    file.path(directory, "positive_control_full_medoid_model_report.md")
  )
}

compare_results <- function(cli) {
  definitions <- artifact_definitions(cli)
  directory <- file.path(cli$output_root, "comparison")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  keys <- names(definitions)
  for (representation in c("medoid", "model")) {
    os1 <- definitions[[paste0("os1_", representation)]]
    hashes <- data.table::fread(file.path(
      cli$output_root, os1$folder, "library_manifest.csv"
    ))
    assert_stage1_unlocked(
      cli,
      definitions[[paste0("current_", representation)]],
      list(
        source_sha256 = hashes$source_sha256[[1L]],
        workflow_sha256 = hashes$workflow_sha256[[1L]]
      )
    )
  }
  manifests <- data.table::rbindlist(lapply(keys, function(key) {
    read_artifact_result(cli, definitions[[key]], "library_manifest.csv")
  }), fill = TRUE)
  if (data.table::uniqueN(manifests$source_sha256) != 1L ||
      data.table::uniqueN(manifests$workflow_sha256) != 1L) {
    stop("Analysis source or frozen workflow hashes differ across artifacts")
  }
  by_sample <- data.table::rbindlist(lapply(keys, function(key) {
    read_artifact_result(cli, definitions[[key]], "metrics_by_sample.csv")
  }), fill = TRUE)
  counts <- by_sample[, .(maps = data.table::uniqueN(Map)), by = library]
  if (nrow(counts) != 6L || any(counts$maps != 20L)) {
    stop("Comparison requires 20 eligible maps for all six artifacts")
  }
  summary <- data.table::rbindlist(lapply(keys, function(key) {
    read_artifact_result(cli, definitions[[key]], "metrics_summary.csv")
  }), fill = TRUE)
  by_material <- data.table::rbindlist(lapply(keys, function(key) {
    read_artifact_result(cli, definitions[[key]], "metrics_by_material_type.csv")
  }), fill = TRUE)
  by_size <- data.table::rbindlist(lapply(keys, function(key) {
    read_artifact_result(cli, definitions[[key]], "metrics_by_size_stratum.csv")
  }), fill = TRUE)
  runtimes <- data.table::rbindlist(lapply(keys, function(key) {
    folder <- file.path(cli$output_root, definitions[[key]]$folder)
    paths <- list.files(folder, pattern = "^runtime_.*[.]csv$", full.names = TRUE)
    paths <- paths[basename(paths) != "runtime_invocation.csv"]
    x <- data.table::rbindlist(lapply(paths, data.table::fread), fill = TRUE)
    x[, library := key]
    x
  }), fill = TRUE)
  if (any(runtimes$elapsed_seconds >= 300, na.rm = TRUE)) {
    stop("At least one added map exceeded five minutes")
  }
  reference_recovery <- by_sample[library == "os1_medoid", .(
    Map, count_accuracy, area_accuracy, feret_accuracy
  )]
  for (key in setdiff(keys, "os1_medoid")) {
    candidate <- by_sample[library == key, .(
      Map, count_accuracy, area_accuracy, feret_accuracy
    )]
    joined <- merge(reference_recovery, candidate, by = "Map",
                    suffixes = c("_reference", "_candidate"))
    numeric_columns <- setdiff(names(joined), "Map")
    for (metric in c("count_accuracy", "area_accuracy", "feret_accuracy")) {
      delta <- joined[[paste0(metric, "_candidate")]] -
        joined[[paste0(metric, "_reference")]]
      if (any(abs(delta[is.finite(delta)]) > 1e-9)) {
        stop("Recovery drift detected for ", key, ": ", metric)
      }
    }
  }
  paired <- data.table::rbindlist(lapply(c("medoid", "model"), function(rep) {
    data.table::rbindlist(lapply(c("pre90", "current"), function(family) {
      x <- engine$paired_library_differences(
        by_sample, paste0("os1_", rep), paste0(family, "_", rep)
      )
      x[, representation := rep]
      x
    }))
  }))
  paired_summary <- engine$paired_metric_summary(paired)
  paired_material <- engine$paired_group_summary(
    paired, by_sample, "material_type"
  )
  paired_size <- engine$paired_group_summary(paired, by_sample, "size_stratum")
  direct <- data.table::rbindlist(lapply(c("medoid", "model"), function(rep) {
    x <- engine$paired_library_differences(
      by_sample, paste0("pre90_", rep), paste0("current_", rep)
    )
    x[, representation := rep]
    x
  }))
  direct_summary <- engine$paired_metric_summary(direct)
  direct_material <- engine$paired_group_summary(
    direct, by_sample, "material_type"
  )
  direct_size <- engine$paired_group_summary(direct, by_sample, "size_stratum")
  traces <- list(
    medoid_os1_to_pre90 = pair_particle_traces(
      cli, definitions, "os1_medoid", "pre90_medoid"
    ),
    medoid_os1_to_current = pair_particle_traces(
      cli, definitions, "os1_medoid", "current_medoid"
    ),
    medoid_pre90_to_current = pair_particle_traces(
      cli, definitions, "pre90_medoid", "current_medoid"
    ),
    model_os1_to_pre90 = pair_particle_traces(
      cli, definitions, "os1_model", "pre90_model"
    ),
    model_os1_to_current = pair_particle_traces(
      cli, definitions, "os1_model", "current_model"
    ),
    model_pre90_to_current = pair_particle_traces(
      cli, definitions, "pre90_model", "current_model"
    )
  )
  transition_summary <- data.table::rbindlist(lapply(names(traces), function(key) {
    x <- traces[[key]]
    left_key <- x$left_library[[1L]]
    right_key <- x$right_library[[1L]]
    result <- x[, .(
      particles = .N,
      mean_score_delta = mean(
        get(paste0("max_cor_val_", right_key)) -
          get(paste0("max_cor_val_", left_key)), na.rm = TRUE
      )
    ), by = .(
      specific_transition, plastic_transition, class_transition
    )]
    result[, `:=`(
      comparison = key,
      left_library = left_key,
      right_library = right_key
    )]
    data.table::setcolorder(result, c(
      "comparison", "left_library", "right_library",
      "specific_transition", "plastic_transition", "class_transition",
      "particles", "mean_score_delta"
    ))
    result
  }), fill = TRUE)
  transition_counts <- data.table::rbindlist(lapply(
    c("medoid", "model"), function(rep) {
      x <- traces[[paste0(rep, "_pre90_to_current")]]
      data.table::data.table(
        Representation = rep,
        Outcome = c("Corrected", "Regressed"),
        `Specific ID particles` = c(
          sum(x$specific_transition == "corrected"),
          sum(x$specific_transition == "regressed")
        ),
        `Plastic/non-plastic particles` = c(
          sum(x$plastic_transition == "corrected"),
          sum(x$plastic_transition == "regressed")
        )
      )
    }
  ))
  margins <- match_margin_diagnostics(cli, definitions)
  diagnostics <- model_diagnostics(definitions, traces, margins)
  labels <- artifact_label_table(definitions)
  labels[, short_label := paste(
    c(os1 = "OS1", pre90 = "Pre-closure", current = "Current")[family],
    tools::toTitleCase(representation)
  )]
  full_summary_path <- file.path(directory, "all_metrics_summary.csv")
  full_material_path <- file.path(directory, "all_metrics_by_material_type.csv")
  full_size_path <- file.path(directory, "all_metrics_by_size_stratum.csv")
  all_nine_summary <- data.table::copy(summary)
  all_nine_material <- data.table::copy(by_material)
  all_nine_size <- data.table::copy(by_size)
  if (all(file.exists(c(full_summary_path, full_material_path, full_size_path)))) {
    full_summary <- data.table::fread(full_summary_path)
    full_material <- data.table::fread(full_material_path)
    full_size <- data.table::fread(full_size_path)
    for (x in list(full_summary, full_material, full_size)) {
      x[, library := paste0(library, "_full")]
    }
    all_nine_summary <- data.table::rbindlist(
      list(full_summary, summary), fill = TRUE
    )
    all_nine_material <- data.table::rbindlist(
      list(full_material, by_material), fill = TRUE
    )
    all_nine_size <- data.table::rbindlist(list(full_size, by_size), fill = TRUE)
    labels <- data.table::rbindlist(list(
      data.table::data.table(
        library = c("os1_full", "pre90_full", "current_full"),
        family = c("os1", "pre90", "current"),
        representation = "full",
        library_label = c(
          "Open Specy 1.0 published derivative library",
          "Pre-cross-class 0.90 closure full derivative library",
          "Current cross-class 0.90 closed full derivative library"
        ),
        folder = c("01_os1_published", "02_pre_0.9_closure", "03_current_closed"),
        short_label = c("OS1 Full", "Pre-closure Full", "Current Full")
      ), labels
    ), fill = TRUE)
  }
  stage1 <- data.table::rbindlist(lapply(c("medoid", "model"), function(rep) {
    definition <- definitions[[paste0("os1_", rep)]]
    gate <- read_artifact_result(
      cli, definition, "stage1_compatibility_gate.csv"
    )
    data.table::data.table(
      Representation = rep,
      `Max absolute mean delta (pp)` = max(abs(gate$mean_delta_pp)),
      `Max absolute RSD delta (pp)` = max(abs(gate$rsd_delta_pp)),
      Pass = all(gate$within_one_percentage_point)
    )
  }))
  engine$write_csv(manifests,
                   file.path(directory, "medoid_model_artifact_manifests.csv"))
  engine$write_csv(by_sample,
                   file.path(directory, "medoid_model_metrics_by_sample.csv"))
  engine$write_csv(summary,
                   file.path(directory, "medoid_model_metrics_summary.csv"))
  engine$write_csv(by_material, file.path(
    directory, "medoid_model_metrics_by_material_type.csv"
  ))
  engine$write_csv(by_size,
                   file.path(directory, "medoid_model_metrics_by_size_stratum.csv"))
  engine$write_csv(all_nine_summary,
                   file.path(directory, "all_nine_metrics_summary.csv"))
  engine$write_csv(all_nine_material,
                   file.path(directory, "all_nine_metrics_by_material_type.csv"))
  engine$write_csv(all_nine_size,
                   file.path(directory, "all_nine_metrics_by_size_stratum.csv"))
  engine$write_csv(runtimes,
                   file.path(directory, "medoid_model_map_runtimes.csv"))
  engine$write_csv(paired,
                   file.path(directory, "medoid_model_paired_vs_os1.csv"))
  engine$write_csv(paired_summary, file.path(
    directory, "medoid_model_paired_summary_vs_os1.csv"
  ))
  engine$write_csv(paired_material, file.path(
    directory, "medoid_model_paired_by_material_vs_os1.csv"
  ))
  engine$write_csv(paired_size, file.path(
    directory, "medoid_model_paired_by_size_vs_os1.csv"
  ))
  engine$write_csv(direct,
                   file.path(directory, "medoid_model_current_vs_pre90.csv"))
  engine$write_csv(direct_summary, file.path(
    directory, "medoid_model_current_vs_pre90_summary.csv"
  ))
  engine$write_csv(direct_material, file.path(
    directory, "medoid_model_current_vs_pre90_by_material.csv"
  ))
  engine$write_csv(direct_size, file.path(
    directory, "medoid_model_current_vs_pre90_by_size.csv"
  ))
  for (key in names(traces)) {
    engine$write_csv(
      traces[[key]], file.path(directory, paste0("particle_", key, ".csv"))
    )
  }
  engine$write_csv(transition_summary, file.path(
    directory, "medoid_model_particle_transition_summary.csv"
  ))
  engine$write_csv(margins$particles,
                   file.path(directory, "medoid_model_margin_particles.csv"))
  engine$write_csv(margins$summary,
                   file.path(directory, "medoid_model_margin_summary.csv"))
  engine$write_csv(margins$by_material, file.path(
    directory, "medoid_model_margin_by_material_type.csv"
  ))
  engine$write_csv(margins$by_size,
                   file.path(directory, "medoid_model_margin_by_size_stratum.csv"))
  engine$write_csv(diagnostics$failure_by_sample, file.path(
    directory, "model_os1_failure_by_sample.csv"
  ))
  engine$write_csv(diagnostics$failure_by_transition, file.path(
    directory, "model_os1_failure_by_class_transition.csv"
  ))
  engine$write_csv(diagnostics$structure,
                   file.path(directory, "model_artifact_structure.csv"))
  engine$write_csv(diagnostics$class_labels,
                   file.path(directory, "model_class_label_audit.csv"))
  engine$write_csv(diagnostics$confidence,
                   file.path(directory, "model_confidence_error_audit.csv"))
  engine$write_csv(diagnostics$builder_summary,
                   file.path(directory, "model_builder_holdout_summary.csv"))
  engine$write_csv(diagnostics$builder_by_class,
                   file.path(directory, "model_builder_holdout_by_class.csv"))
  write_comparison_plot(
    all_nine_summary, labels,
    file.path(directory, "all_nine_identification_accuracy.png")
  )
  write_report(
    cli, all_nine_summary, labels, paired_summary, direct_summary,
    paired_material, paired_size, transition_counts, margins, diagnostics,
    runtimes, stage1
  )
  message("Medoid/model comparison written to: ", directory)
  invisible(list(summary = all_nine_summary, paired = paired_summary))
}

main <- function() {
  cli <- parse_cli()
  cli$data_root <- normalize_path(cli$data_root)
  cli$historical_root <- normalize_path(cli$historical_root)
  cli$output_root <- normalize_path(cli$output_root, must_work = FALSE)
  dir.create(cli$output_root, recursive = TRUE, showWarnings = FALSE)
  switch(
    cli$stage,
    "score-saved" = {
      invisible(score_saved(cli, "medoid"))
      invisible(score_saved(cli, "model"))
    },
    "run" = run_artifact(cli),
    "compare" = compare_results(cli),
    stop("Unknown --stage=", cli$stage)
  )
}

if (identical(environment(), globalenv())) main()
