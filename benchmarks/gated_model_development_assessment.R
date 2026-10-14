# Summarize a completed strict-gated reference build and frozen positive-control
# validation for model-development decisions. This script is deliberately
# read-only with respect to models: it never fits, predicts, or changes the
# canonical validation inputs. It writes only model_development_* artifacts to
# the completed validation comparison directory.
#
# Example:
#   Rscript benchmarks/gated_model_development_assessment.R \
#     --assessments=C:/path/to/release/assessments.rds \
#     --validation-root=C:/path/to/gated_hierarchy_validation

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

runner_version <- "gated-model-development-assessment-v2"

normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

usage <- function() {
  cat(paste0(
    "Read-only gated-model development assessment\n\n",
    "Required arguments use --name=value:\n",
    "  --assessments=PATH       Candidate-aligned assessments.rds.\n",
    "  --validation-root=PATH   Completed gated validation root.\n\n",
    "Optional arguments:\n",
    "  --candidate-folder=NAME  Staged model folder (default ",
    "01_gated_three_level).\n",
    "  --candidate=KEY          Candidate key (default gated_three_level).\n",
    "  --reference=KEY          Holdout reference (default os1_model).\n",
    "  --algorithm=NAME         Release assessment algorithm filter.\n",
    "  --artifact=NAME          Release assessment artifact (default ",
    "derivative).\n",
    "  --model=NAME             Release assessment model (default ftir).\n",
    "  --technique=NAME         Release assessment technique (default ftir).\n",
    "  --top-n=N                Rows retained in ranked diagnostics ",
    "(default 25).\n"
  ))
  invisible(TRUE)
}

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (any(args %in% c("--help", "-h"))) return(list(help = TRUE))
  out <- list(
    help = FALSE,
    assessments = "",
    validation_root = "",
    candidate_folder = "01_gated_three_level",
    candidate = "gated_three_level",
    reference = "os1_model",
    algorithm = "gated_hierarchical_logistic_regression",
    artifact = "derivative",
    model = "ftir",
    technique = "ftir",
    top_n = "25"
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
  if (!nzchar(out$assessments) || !nzchar(out$validation_root)) {
    stop("--assessments and --validation-root are required")
  }
  out$top_n <- suppressWarnings(as.integer(out$top_n))
  if (is.na(out$top_n) || out$top_n < 1L || out$top_n > 100L) {
    stop("--top-n must be an integer from 1 through 100")
  }
  out$assessments <- normalize_path(out$assessments)
  out$validation_root <- normalize_path(out$validation_root)
  out
}

require_columns <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(label, " lacks required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

read_csv <- function(path, columns = NULL, label = basename(path)) {
  if (!file.exists(path) || is.na(file.info(path)$size) ||
      file.info(path)$size <= 0) {
    stop("Missing or empty input: ", path)
  }
  out <- data.table::fread(path, na.strings = c("", "NA"))
  if (!is.null(columns)) require_columns(out, columns, label)
  out
}

as_flag <- function(x) {
  if (is.logical(x)) return(x)
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("true", "t", "1", "yes")] <- TRUE
  out[value %in% c("false", "f", "0", "no")] <- FALSE
  out
}

one_row <- function(x, label) {
  if (nrow(x) != 1L) stop(label, " must contain exactly one row")
  x
}

untype_label <- function(x) {
  sub("^(ftir|raman|nir)_", "", trimws(as.character(x)), ignore.case = TRUE)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

rank_rows <- function(x) {
  x[, rank := seq_len(.N)]
  data.table::setcolorder(x, c("rank", setdiff(names(x), "rank")))
  x[]
}

head_by <- function(x, by, n) {
  groups <- split(x, x[[by]], drop = TRUE)
  data.table::rbindlist(lapply(groups, utils::head, n = n), fill = TRUE)
}

markdown_table <- function(x, columns = names(x), digits = 3L) {
  x <- as.data.frame(x[, ..columns])
  format_one <- function(value) {
    if (is.numeric(value)) {
      value <- ifelse(
        is.na(value), "NA",
        format(round(value, digits), trim = TRUE, scientific = FALSE)
      )
    } else if (is.logical(value)) {
      value <- ifelse(is.na(value), "NA", ifelse(value, "yes", "no"))
    } else {
      value <- ifelse(is.na(value), "NA", as.character(value))
    }
    gsub("\\|", "\\\\|", value)
  }
  formatted <- lapply(x, format_one)
  rows <- if (nrow(x)) {
    apply(as.data.frame(formatted, check.names = FALSE), 1L, function(row) {
      paste0("| ", paste(row, collapse = " | "), " |")
    })
  } else {
    "| (none) |"
  }
  c(
    paste0("| ", paste(columns, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(columns)), collapse = " | "), " |"),
    rows
  )
}

write_csv <- function(x, path) {
  if (!startsWith(basename(path), "model_development_")) {
    stop("Refusing to write a non-assessment CSV: ", path)
  }
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  data.table::fwrite(x, tmp, na = "NA", quote = TRUE)
  if (file.exists(path) && !file.remove(path)) stop("Cannot replace: ", path)
  if (!file.rename(tmp, path)) stop("Cannot finalize: ", path)
  invisible(path)
}

write_report <- function(lines, path) {
  if (!identical(basename(path), "model_development_assessment.md")) {
    stop("Refusing to write an unexpected report: ", path)
  }
  tmp <- tempfile(".model-development-report-", tmpdir = dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE)
  if (file.exists(path) && !file.remove(path)) stop("Cannot replace: ", path)
  if (!file.rename(tmp, path)) stop("Cannot finalize: ", path)
  invisible(path)
}

cli <- parse_cli()
if (isTRUE(cli$help)) {
  usage()
  quit(save = "no", status = 0L)
}

comparison_dir <- file.path(cli$validation_root, "comparison")
candidate_dir <- file.path(cli$validation_root, cli$candidate_folder)
canonical_report <- file.path(
  comparison_dir, "gated_hierarchy_validation_report.md"
)
if (!dir.exists(comparison_dir) || !dir.exists(candidate_dir) ||
    !file.exists(canonical_report)) {
  stop(
    "Validation is not complete: candidate, comparison, and canonical report ",
    "must all exist"
  )
}

comparison_files <- c(
  gate = "gated_noninferiority_gate.csv",
  decision = "gated_noninferiority_decision.csv",
  metrics = "metrics_by_sample.csv",
  transitions = "gated_particle_transitions_vs_os1.csv",
  transition_summary = "gated_particle_transition_summary.csv",
  targeted = "targeted_failures.csv",
  recovery = "particle_recovery_compatibility_summary.csv",
  runtime = "gated_map_runtimes.csv",
  fit = "gated_model_fit_manifest.csv",
  vinyl = "source_held_vinyl_family_oof.csv"
)
comparison_paths <- stats::setNames(
  file.path(comparison_dir, unname(comparison_files)), names(comparison_files)
)
candidate_files <- c(
  hierarchy = "model_hierarchy_metrics.csv",
  classes = "model_class_metrics.csv",
  source = "model_source_metrics.csv",
  lambda = "model_lambda_metrics.csv",
  nodes = "model_node_support.csv",
  folds = "model_fold_assignments.csv",
  routed = "model_routed_oof_tests.csv",
  vinyl = "model_vinyl_family_oof.csv"
)
candidate_paths <- stats::setNames(
  file.path(candidate_dir, unname(candidate_files)), names(candidate_files)
)
input_paths <- c(
  assessments = cli$assessments,
  canonical_report = canonical_report,
  comparison_paths,
  candidate_paths
)
missing_inputs <- input_paths[
  !file.exists(input_paths) | is.na(file.info(input_paths)$size) |
    file.info(input_paths)$size <= 0
]
if (length(missing_inputs)) {
  stop("Missing or empty required input(s): ", paste(missing_inputs, collapse = ", "))
}

assessment <- readRDS(cli$assessments)
if (!is.list(assessment) || !is.list(assessment$model)) {
  stop("Released assessments.rds lacks the model assessment section")
}
assessment_provenance <- attr(
  assessment, "candidate_provenance", exact = TRUE
)
if (!is.list(assessment_provenance) ||
    length(assessment_provenance$model_sha256) != 1L ||
    is.na(assessment_provenance$model_sha256) ||
    !nzchar(assessment_provenance$model_sha256) ||
    length(assessment_provenance$medoid_sha256) != 1L ||
    is.na(assessment_provenance$medoid_sha256) ||
    !nzchar(assessment_provenance$medoid_sha256)) {
  stop(
    paste(
      "Candidate assessment lacks pinned model and medoid hashes in",
      "candidate_provenance"
    )
  )
}
release_accuracy <- data.table::as.data.table(
  data.table::copy(assessment$model$accuracy)
)
release_confusion <- data.table::as.data.table(
  data.table::copy(assessment$model$confusion)
)
assessment_keys <- c(
  "algorithm", "artifact", "model", "technique", "source", "provenance"
)
require_columns(
  release_accuracy,
  c(assessment_keys, "spectra", "class_count", "overall_accuracy_pct",
    "macro_class_accuracy_pct"),
  "release model accuracy"
)
require_columns(
  release_confusion,
  c(assessment_keys, "expected_class", "predicted_class", "spectra",
    "expected_class_spectra", "expected_class_pct"),
  "release model confusion"
)
release_filter <- function(x) {
  x[
    tolower(algorithm) == tolower(cli$algorithm) &
      tolower(artifact) == tolower(cli$artifact) &
      tolower(model) == tolower(cli$model) &
      tolower(technique) == tolower(cli$technique)
  ]
}
release_accuracy <- one_row(
  release_filter(release_accuracy), "selected release assessment accuracy"
)
release_accuracy[, evidence_scope := "complete_dataset_assessment_not_oof"]
release_confusion <- release_filter(release_confusion)
release_confusion <- release_confusion[
  expected_class != predicted_class
][order(-spectra, -expected_class_pct)]
release_confusion <- rank_rows(utils::head(release_confusion, cli$top_n))

hierarchy <- read_csv(
  candidate_paths[["hierarchy"]],
  c("scope", "depth", "overall_accuracy", "macro_class_accuracy",
    "minimum_class_recall", "route_coverage", "log_loss", "brier_score",
    "worst_source_accuracy", "supported_sources"),
  "OOF hierarchy metrics"
)
data.table::setorder(hierarchy, depth)

class_metrics <- read_csv(
  candidate_paths[["classes"]],
  c("class", "spectra", "recall", "node_id", "depth", "target_col",
    "node_role", "fold_mode"),
  "OOF class metrics"
)
class_metrics[, taxonomy_level := data.table::fcase(
  target_col == "material_type", "material type",
  target_col == "material_group", "material group",
  target_col == "material_class", "material class",
  default = target_col
)]
class_metrics[, recall_gap := 1 - recall]
class_metrics[, risk := data.table::fcase(
  !is.finite(recall), "unavailable",
  recall < 0.50, "critical",
  recall < 0.70, "high",
  recall < 0.80, "moderate",
  default = "monitor"
)]
recall_risks <- class_metrics[
  !is.finite(recall) | recall < 0.80
][order(recall, -spectra)]
recall_risks <- rank_rows(utils::head(recall_risks, cli$top_n))

tests <- read_csv(
  candidate_paths[["routed"]],
  c("spectrum_id", "expected_class", "predicted_class", "correct", "split",
    "first_failed_gate", "gate_1_name", "gate_2_name"),
  "routed OOF tests"
)
folds <- read_csv(
  candidate_paths[["folds"]],
  c("spectrum_id", "source_id", "source_component_id", "fold",
    "source_fold", "fold_mode", "source_fold_mode", "material_type",
    "material_group", "final_class"),
  "OOF fold assignments"
)
if (anyDuplicated(tests$spectrum_id) || anyDuplicated(folds$spectrum_id) ||
    !setequal(tests$spectrum_id, folds$spectrum_id)) {
  stop("Routed OOF tests and fold assignments are not one-to-one")
}
if (any(is.na(tests$split) | tests$split != "out_of_fold")) {
  stop("Routed model diagnostics include non-OOF predictions")
}
oof <- merge(tests, folds, by = "spectrum_id", all = FALSE, sort = FALSE)
if (nrow(oof) != nrow(tests)) stop("OOF diagnostic join lost rows")
oof[, `:=`(
  expected_type = untype_label(material_type),
  predicted_type = untype_label(gate_1_name),
  expected_group = untype_label(material_group),
  predicted_group = untype_label(gate_2_name),
  expected_leaf = untype_label(expected_class),
  predicted_leaf = untype_label(predicted_class)
)]

confusion_for <- function(x, level, expected, predicted) {
  out <- x[
    !is.na(get(expected)) & !is.na(get(predicted)),
    .(spectra = .N, sources = data.table::uniqueN(source_id)),
    by = .(expected = get(expected), predicted = get(predicted))
  ]
  support <- out[, .(expected_spectra = sum(spectra)), by = expected]
  out <- merge(out, support, by = "expected", sort = FALSE)
  out[, `:=`(
    level = level,
    expected_pct = 100 * spectra / expected_spectra
  )]
  out[expected != predicted]
}
oof_confusions <- data.table::rbindlist(list(
  confusion_for(oof, "material_type", "expected_type", "predicted_type"),
  confusion_for(oof, "material_group", "expected_group", "predicted_group"),
  confusion_for(oof, "material_class", "expected_leaf", "predicted_leaf")
), fill = TRUE)
data.table::setorder(oof_confusions, level, -spectra, -expected_pct)
oof_confusions <- head_by(oof_confusions, "level", cli$top_n)
oof_confusions <- oof_confusions[
  , rank := seq_len(.N), by = level
][, c("level", "rank", setdiff(names(oof_confusions),
                                c("level", "rank"))), with = FALSE]

leaf_errors <- oof[expected_leaf != predicted_leaf]
leaf_errors[, `:=`(
  class_a = pmin(expected_leaf, predicted_leaf),
  class_b = pmax(expected_leaf, predicted_leaf),
  same_terminal_route = expected_type == predicted_type &
    expected_group == predicted_group
)]
bidirectional <- leaf_errors[, .(
  spectra = .N,
  class_a_to_b = sum(expected_leaf == class_a[1L]),
  class_b_to_a = sum(expected_leaf == class_b[1L]),
  same_route_a_to_b = sum(
    expected_leaf == class_a[1L] & same_terminal_route, na.rm = TRUE
  ),
  same_route_b_to_a = sum(
    expected_leaf == class_b[1L] & same_terminal_route, na.rm = TRUE
  ),
  upstream_route_errors = sum(!same_terminal_route, na.rm = TRUE),
  sources = data.table::uniqueN(source_id),
  source_components = data.table::uniqueN(source_component_id),
  same_route_sources = data.table::uniqueN(
    source_id[same_terminal_route %in% TRUE]
  )
), by = .(class_a, class_b)]
bidirectional[, `:=`(
  bidirectional = class_a_to_b > 0 & class_b_to_a > 0,
  merge_review_candidate = same_route_a_to_b > 0 & same_route_b_to_a > 0 &
    same_route_sources >= 2L,
  directional_balance = pmin(class_a_to_b, class_b_to_a) /
    pmax(class_a_to_b, class_b_to_a)
)]
data.table::setorder(
  bidirectional, -merge_review_candidate, -bidirectional, -spectra,
  -directional_balance
)
bidirectional <- rank_rows(utils::head(bidirectional, cli$top_n))

failure_gates <- data.table::copy(oof)
failure_gates[, first_failed_gate := data.table::fifelse(
  is.na(first_failed_gate) | !nzchar(first_failed_gate),
  "correct_path", first_failed_gate
)]
failure_gates <- failure_gates[, .(spectra = .N), by = first_failed_gate]
failure_gates[, percent := 100 * spectra / sum(spectra)]
data.table::setorder(failure_gates, -spectra)

source_metrics <- read_csv(
  candidate_paths[["source"]],
  c("source", "observations", "classes", "accuracy", "route_coverage",
    "scope", "depth", "supported", "minimum_observations", "evidence"),
  "source metrics"
)
source_metrics[, supported := as_flag(supported)]
source_metrics <- merge(
  source_metrics,
  hierarchy[, .(scope, scope_overall_accuracy = overall_accuracy)],
  by = "scope", all.x = TRUE, sort = FALSE
)
source_metrics[, gap_from_scope_overall := accuracy - scope_overall_accuracy]
supported_sources <- source_metrics[supported == TRUE]
data.table::setorder(supported_sources, scope, accuracy, -observations)
source_robustness <- head_by(
  supported_sources, "scope", min(cli$top_n, 10L)
)
data.table::setorder(source_robustness, accuracy, -observations, scope)

node_support <- read_csv(
  candidate_paths[["nodes"]],
  c("node_id", "depth", "target_col", "node_role", "observations", "classes",
    "canonical_sources", "source_components", "source_eligible", "fold_mode",
    "folds"),
  "node support"
)
node_support[, source_eligible := as_flag(source_eligible)]

lambda <- read_csv(
  candidate_paths[["lambda"]],
  c("lambda", "macro_class_accuracy", "overall_accuracy", "log_loss",
    "brier_score", "minimum_class_recall", "worst_source_accuracy",
    "overall_eligible", "minimum_class_eligible", "source_eligible",
    "macro_eligible", "loss_eligible", "brier_eligible", "selected",
    "node_id", "depth", "target_col", "node_role", "fold_mode"),
  "lambda metrics"
)
flag_columns <- c(
  "overall_eligible", "minimum_class_eligible", "source_eligible",
  "macro_eligible", "loss_eligible", "brier_eligible", "selected"
)
lambda[, (flag_columns) := lapply(.SD, as_flag), .SDcols = flag_columns]
count_true <- function(x) {
  if (all(is.na(x))) return(NA_integer_)
  sum(x %in% TRUE)
}
lambda_feasibility <- lambda[, {
  chosen <- .SD[selected == TRUE]
  if (nrow(chosen) != 1L) {
    stop("Each fitted node must have exactly one selected lambda: ", node_id[1L])
  }
  list(
    depth = depth[1L], target_col = target_col[1L],
    node_role = node_role[1L], fold_mode = fold_mode[1L],
    lambda_candidates = .N,
    lambda_min = min(lambda), lambda_max = max(lambda),
    selected_lambda = chosen$lambda,
    selected_at_path_boundary = chosen$lambda == min(lambda) |
      chosen$lambda == max(lambda),
    overall_eligible_n = count_true(overall_eligible),
    minimum_class_eligible_n = count_true(minimum_class_eligible),
    source_eligible_n = count_true(source_eligible),
    macro_eligible_n = count_true(macro_eligible),
    loss_eligible_n = count_true(loss_eligible),
    brier_eligible_n = count_true(brier_eligible),
    selected_overall_accuracy = chosen$overall_accuracy,
    selected_macro_accuracy = chosen$macro_class_accuracy,
    selected_minimum_recall = chosen$minimum_class_recall,
    selected_worst_source_accuracy = chosen$worst_source_accuracy,
    selected_log_loss = chosen$log_loss,
    selected_brier_score = chosen$brier_score
  )
}, by = node_id]
lambda_feasibility[, c("depth", "target_col", "node_role", "fold_mode") := NULL]
lambda_feasibility <- merge(
  node_support[, .(
    node_id, depth, target_col, node_role, observations, classes,
    canonical_sources, source_components, source_eligible, fold_mode
  )],
  lambda_feasibility, by = "node_id", all.x = TRUE, sort = FALSE
)
lambda_feasibility[, fit_status := data.table::fcase(
  classes <= 1L, "deterministic",
  is.finite(selected_lambda), "fitted",
  default = "missing_lambda"
)]
if (nrow(lambda_feasibility[fit_status == "missing_lambda"])) {
  stop("A non-deterministic node lacks lambda feasibility evidence")
}
data.table::setorder(lambda_feasibility, depth, node_id)

gate <- read_csv(
  comparison_paths[["gate"]],
  c("scope", "level", "metric_key", "n_os1", "n_candidate",
    "mean_delta_pp", "applicable", "gate_status", "reference", "candidate",
    "threshold_pp", "pass"),
  "non-inferiority gate"
)
decision <- one_row(read_csv(
  comparison_paths[["decision"]],
  c("checks", "failed", "worst_delta_pp", "pass"),
  "non-inferiority decision"
), "non-inferiority decision")
gate[, `:=`(applicable = as_flag(applicable), pass = as_flag(pass))]
decision[, pass := as_flag(pass)]
if (!all(gate$candidate == cli$candidate) ||
    !all(gate$reference == cli$reference)) {
  stop("Non-inferiority evidence does not match candidate/reference arguments")
}

metrics <- read_csv(
  comparison_paths[["metrics"]],
  c("Map", "SpecID", "PNID", "count", "material_type", "size_stratum",
    "detected_count", "library", "specific_accuracy", "plastic_accuracy"),
  "holdout sample metrics"
)
map_columns <- c(
  "Map", "SpecID", "PNID", "count", "material_type", "size_stratum",
  "detected_count", "specific_accuracy", "plastic_accuracy"
)
reference_maps <- metrics[library == cli$reference, ..map_columns]
candidate_maps <- metrics[library == cli$candidate, ..map_columns]
if (anyDuplicated(reference_maps$Map) || anyDuplicated(candidate_maps$Map) ||
    !setequal(reference_maps$Map, candidate_maps$Map)) {
  stop("Candidate and reference map metrics are not one-to-one")
}
holdout_maps <- merge(
  reference_maps, candidate_maps, by = "Map", suffixes = c("_reference",
                                                            "_candidate"),
  sort = FALSE
)
holdout_maps[, `:=`(
  specific_delta_pp = specific_accuracy_candidate - specific_accuracy_reference,
  plastic_delta_pp = plastic_accuracy_candidate - plastic_accuracy_reference
)]

transitions <- read_csv(
  comparison_paths[["transitions"]],
  c("sample_id", "particle_id", "material_class_scored_os1",
    "specific_correct_os1", "plastic_correct_os1",
    "material_class_scored_candidate", "specific_correct_candidate",
    "plastic_correct_candidate", "candidate", "class_transition",
    "specific_transition", "broad_transition"),
  "holdout particle transitions"
)
transitions <- transitions[candidate == cli$candidate]
if (!nrow(transitions)) stop("No particle transitions for selected candidate")
transition_states <- c(
  "corrected", "regressed", "unchanged", "missing-both", "missing-os1",
  "missing-candidate"
)
unexpected_transition_states <- setdiff(
  unique(c(transitions$specific_transition, transitions$broad_transition)),
  transition_states
)
if (length(unexpected_transition_states)) {
  stop(
    "Unexpected particle-transition state(s): ",
    paste(unexpected_transition_states, collapse = ", ")
  )
}
particle_keys <- transitions[, .N, by = .(sample_id, particle_id)][N != 1L]
if (nrow(particle_keys)) {
  stop("Holdout particle transitions contain duplicate particle keys")
}
if (!setequal(unique(transitions$sample_id), candidate_maps$Map)) {
  stop("Holdout particle transitions do not cover the candidate map set")
}
correctness_columns <- c(
  "specific_correct_os1", "specific_correct_candidate",
  "plastic_correct_os1", "plastic_correct_candidate"
)
transitions[, (correctness_columns) := lapply(.SD, as_flag),
            .SDcols = correctness_columns]
derive_transition <- function(reference, candidate) {
  data.table::fcase(
    is.na(reference) & is.na(candidate), "missing-both",
    is.na(reference), "missing-os1",
    is.na(candidate), "missing-candidate",
    !reference & candidate, "corrected",
    reference & !candidate, "regressed",
    default = "unchanged"
  )
}
expected_specific_transition <- derive_transition(
  transitions$specific_correct_os1, transitions$specific_correct_candidate
)
expected_broad_transition <- derive_transition(
  transitions$plastic_correct_os1, transitions$plastic_correct_candidate
)
expected_class_transition <- paste(
  transitions$material_class_scored_os1,
  transitions$material_class_scored_candidate,
  sep = " -> "
)
if (!identical(transitions$specific_transition, expected_specific_transition) ||
    !identical(transitions$broad_transition, expected_broad_transition) ||
    !identical(transitions$class_transition, expected_class_transition)) {
  stop("Saved particle-transition labels do not match particle correctness")
}
transition_summary <- read_csv(
  comparison_paths[["transition_summary"]],
  c("candidate", "specific_transition", "broad_transition", "class_transition",
    "particles"),
  "holdout transition summary"
)[candidate == cli$candidate]
recalculated_transition_summary <- transitions[, .(particles = .N), by = .(
  candidate, specific_transition, broad_transition, class_transition
)]
transition_check <- merge(
  transition_summary, recalculated_transition_summary,
  by = c("candidate", "specific_transition", "broad_transition",
         "class_transition"),
  all = TRUE, suffixes = c("_saved", "_recalculated")
)
if (any(is.na(transition_check$particles_saved)) ||
    any(is.na(transition_check$particles_recalculated)) ||
    any(transition_check$particles_saved != transition_check$particles_recalculated)) {
  stop("Saved particle-transition summary does not match particle-level evidence")
}
map_transitions <- transitions[, .(
  matched_particles = .N,
  specific_regressed_particles = sum(specific_transition == "regressed"),
  specific_improved_particles = sum(specific_transition == "corrected"),
  broad_regressed_particles = sum(broad_transition == "regressed"),
  broad_improved_particles = sum(broad_transition == "corrected")
), by = .(Map = sample_id)]
holdout_maps <- merge(holdout_maps, map_transitions, by = "Map", all.x = TRUE)
data.table::setorder(
  holdout_maps, specific_delta_pp, plastic_delta_pp, Map
)

transition_regressions <- transitions[
  specific_transition == "regressed" | broad_transition == "regressed",
  .(
    particles = .N,
    maps = data.table::uniqueN(sample_id),
    specific_regressions = sum(specific_transition == "regressed"),
    broad_regressions = sum(broad_transition == "regressed"),
    map_examples = paste(utils::head(sort(unique(sample_id)), 3L), collapse = " | ")
  ),
  by = class_transition
]
transition_regressions[, percent_of_all_particles := 100 * particles /
                         nrow(transitions)]
data.table::setorder(
  transition_regressions, -broad_regressions, -specific_regressions, -particles
)
transition_regressions <- rank_rows(
  utils::head(transition_regressions, cli$top_n)
)

targeted <- read_csv(
  comparison_paths[["targeted"]],
  c("library", "particles", "specific_accuracy", "broad_accuracy",
    "polyethylene_to_pva", "nonplastic_to_plastic"),
  "targeted failures"
)
target_candidate <- one_row(
  targeted[library == cli$candidate], "candidate targeted failures"
)
target_reference <- one_row(
  targeted[library == cli$reference], "reference targeted failures"
)
target_reference_join <- data.table::copy(target_reference)[, join_key := 1L]
target_candidate_join <- data.table::copy(target_candidate)[, join_key := 1L]
targeted_delta <- merge(
  target_reference_join, target_candidate_join, by = "join_key",
  suffixes = c("_reference", "_candidate")
)
targeted_delta[, join_key := NULL]
targeted_delta[, `:=`(
  specific_delta_pp = specific_accuracy_candidate - specific_accuracy_reference,
  broad_delta_pp = broad_accuracy_candidate - broad_accuracy_reference,
  polyethylene_to_pva_delta = polyethylene_to_pva_candidate -
    polyethylene_to_pva_reference,
  nonplastic_to_plastic_delta = nonplastic_to_plastic_candidate -
    nonplastic_to_plastic_reference
)]

vinyl <- read_csv(
  candidate_paths[["vinyl"]],
  c("spectrum_id", "source_id", "source_component_id", "correct", "route_role",
    "expected_material_group_taxonomy", "expected_class_taxonomy",
    "predicted_material_group_taxonomy", "predicted_class_taxonomy",
    "first_failed_gate"),
  "source-held vinyl OOF evidence"
)
vinyl[, correct := as_flag(correct)]
vinyl_summary <- vinyl[, .(
  spectra = .N,
  leaf_accuracy = mean(correct, na.rm = TRUE),
  sources = data.table::uniqueN(source_id),
  source_components = data.table::uniqueN(source_component_id)
), by = .(route_role, expected_class_taxonomy)]
data.table::setorder(vinyl_summary, route_role, expected_class_taxonomy)

recovery <- read_csv(
  comparison_paths[["recovery"]],
  c("metric", "maps", "both_missing", "maximum_absolute_delta", "pass"),
  "particle recovery compatibility"
)
recovery[, pass := as_flag(pass)]
if (any(is.na(recovery$pass) | !recovery$pass)) {
  stop("Particle recovery compatibility must pass before assessment")
}
runtime <- read_csv(
  comparison_paths[["runtime"]],
  c("sample_id", "elapsed_seconds", "warnings"),
  "gated map runtimes"
)
fit <- one_row(read_csv(
  comparison_paths[["fit"]],
  c("candidate", "model_type", "model_schema", "selection_rule",
    "probability_semantics", "observations", "predictors", "classes",
    "elapsed_seconds", "selected_lambda_converged", "lambda_path_complete",
    "release_model_sha256", "full_checkpoint_sha256", "input_sha256",
    "model_origin"),
  "model fit manifest"
), "model fit manifest")
fit[, `:=`(
  selected_lambda_converged = as_flag(selected_lambda_converged),
  lambda_path_complete = as_flag(lambda_path_complete)
)]
if (!identical(as.character(fit$candidate[[1L]]), cli$candidate)) {
  stop("Fit manifest candidate does not match --candidate")
}
diagnostic_hash_field <- function(name) {
  paste0(if (identical(name, "source")) "source_metrics" else name, "_sha256")
}
diagnostic_fields <- vapply(
  names(candidate_paths), diagnostic_hash_field, character(1L)
)
missing_diagnostic_hashes <- setdiff(diagnostic_fields, names(fit))
if (length(missing_diagnostic_hashes)) {
  stop(
    "Fit manifest lacks diagnostic hash field(s): ",
    paste(missing_diagnostic_hashes, collapse = ", ")
  )
}
diagnostic_hashes_match <- vapply(names(candidate_paths), function(name) {
  identical(
    tolower(sha256_file(candidate_paths[[name]])),
    tolower(as.character(fit[[diagnostic_hash_field(name)]][[1L]]))
  )
}, logical(1L))
if (!all(diagnostic_hashes_match)) {
  stop(
    "Candidate OOF diagnostic hash mismatch: ",
    paste(names(diagnostic_hashes_match)[!diagnostic_hashes_match],
          collapse = ", ")
  )
}
if (!identical(
  tolower(as.character(assessment_provenance$model_sha256)),
  tolower(as.character(fit$release_model_sha256[[1L]]))
)) {
  stop(
    "Candidate assessment model hash does not match the frozen validation fit"
  )
}
if (!identical(
  tolower(as.character(assessment_provenance$medoid_sha256)),
  tolower(as.character(fit$input_sha256[[1L]]))
)) {
  stop(
    "Candidate assessment medoid hash does not match the frozen validation fit"
  )
}
if (anyDuplicated(runtime$sample_id) ||
    !setequal(runtime$sample_id, candidate_maps$Map)) {
  stop("Runtime evidence is not one-to-one with candidate maps")
}

root_metric <- one_row(
  hierarchy[scope == "gate:material_type"], "material-type OOF metrics"
)
group_metric <- one_row(
  hierarchy[scope == "gate:material_group"], "material-group OOF metrics"
)
group_rows <- class_metrics[target_col == "material_group"]
leaf_rows <- class_metrics[target_col == "material_class"]
if (!nrow(group_rows) || !nrow(leaf_rows) || !nrow(supported_sources)) {
  stop("Training diagnostics lack group, leaf, or supported-source evidence")
}
worst_group <- group_rows[order(recall)][1L]
worst_leaf <- leaf_rows[order(recall)][1L]
worst_source <- supported_sources[order(accuracy)][1L]
boundary_nodes <- lambda_feasibility[selected_at_path_boundary == TRUE, .N]
failed_checks <- gate[applicable == TRUE & (is.na(pass) | pass == FALSE), .N]
decision_pass <- isTRUE(decision$pass[[1L]])
probe_only <- identical(
  as.character(fit$model_origin[[1L]]),
  "axis_corrected_363bin_model_probe"
)
if (!identical(as.integer(decision$failed[[1L]]), as.integer(failed_checks)) ||
    decision_pass != (failed_checks == 0L)) {
  stop("Saved gate decision does not match the detailed gate rows")
}

recommendations <- data.table::rbindlist(list(
  data.table::data.table(
    priority = 1L,
    area = "promotion decision",
    evidence = sprintf(
      "%.0f of %.0f applicable frozen gates failed; worst delta %.2f pp.",
      failed_checks, decision$checks[[1L]], decision$worst_delta_pp[[1L]]
    ),
    action = if (decision_pass && probe_only) {
      paste(
        "The axis-corrected probe passed this locked run. Before promotion,",
        "produce a complete release with fresh parent-to-medoid closure and",
        "confirm that the exact rebuilt artifact retains the result."
      )
    } else if (decision_pass) {
      paste(
        "The staged artifact passed this locked run, but promote only this",
        "unchanged hash; any subsequent change needs new confirmation evidence."
      )
    } else if (probe_only) {
      paste(
        "Do not promote this diagnostic probe. Keep the staged probe hash as",
        "diagnostic evidence while development returns to training/source-held",
        "evidence. Its restricted medoid did not undergo a fresh",
        "parent-to-medoid closure pass."
      )
    } else {
      paste(
        "Do not promote this artifact. Keep the staged hash as a diagnosed",
        "candidate while development returns to training/source-held evidence."
      )
    },
    acceptance_boundary = paste(
      "Freeze taxonomy, preprocessing, hierarchy, and selection before",
      "evaluating a successor on a fresh untouched holdout."
    )
  ),
  data.table::data.table(
    priority = 2L,
    area = "material-type gate",
    evidence = sprintf(
      "Source-held OOF accuracy %.3f, minimum recall %.3f; %.0f broad holdout regressions.",
      root_metric$overall_accuracy[[1L]], root_metric$minimum_class_recall[[1L]],
      sum(transitions$broad_transition == "regressed")
    ),
    action = paste(
      "Improve source-diverse plastic/non-plastic representation and audit",
      "normalization/domain shift at the root; retain strict top-1 routing."
    ),
    acceptance_boundary = paste(
      "Choose changes only with source-component OOF root recall, worst-source",
      "accuracy, log loss, and Brier evidence, never this holdout."
    )
  ),
  data.table::data.table(
    priority = 3L,
    area = "polymer-group gate",
    evidence = sprintf(
      "Source-held OOF accuracy %.3f; worst group '%s' recall %.3f.",
      group_metric$overall_accuracy[[1L]], worst_group$class[[1L]],
      worst_group$recall[[1L]]
    ),
    action = paste(
      "Audit the group crosswalk and source coverage for low-recall routes.",
      "Reassign or split a route only when chemistry and cross-source evidence agree."
    ),
    acceptance_boundary = paste(
      "Do not create a broad vinyl or catch-all merge merely to improve the",
      "current holdout; predeclare group changes before fresh confirmation."
    )
  ),
  data.table::data.table(
    priority = 4L,
    area = "leaf taxonomy",
    evidence = sprintf(
      "Worst OOF leaf '%s' recall %.3f; same-route bidirectional pairs are exported.",
      worst_leaf$class[[1L]], worst_leaf$recall[[1L]]
    ),
    action = paste(
      "Review same-group bidirectional OOF pairs with multiple independent sources.",
      "Combine leaves only when chemically defensible; otherwise improve labels",
      "or training support."
    ),
    acceptance_boundary = paste(
      "A holdout transition can trigger investigation, but cannot justify a",
      "class merge or select among alternatives."
    )
  ),
  data.table::data.table(
    priority = 5L,
    area = "source robustness",
    evidence = sprintf(
      "Worst supported source/scope is '%s' at %.3f (%s).",
      worst_source$source[[1L]], worst_source$accuracy[[1L]],
      worst_source$scope[[1L]]
    ),
    action = paste(
      "Inspect source-specific axes, preprocessing, labels, and duplicate groups;",
      "use source-balanced sampling or weights only in training-side experiments."
    ),
    acceptance_boundary = paste(
      "Retain identity grouping and source-component holdout, and protect",
      "worst-source accuracy during lambda/model selection."
    )
  ),
  data.table::data.table(
    priority = 6L,
    area = "lambda feasibility",
    evidence = sprintf(
      "%.0f fitted node(s) selected a lambda-path boundary; path complete=%s.",
      boundary_nodes,
      ifelse(isTRUE(fit$lambda_path_complete[[1L]]), "yes", "no")
    ),
    action = paste(
      "Expand the lambda path only for boundary nodes and rerun grouped OOF",
      "guardrails; diagnose label/support limits where recall feasibility collapses."
    ),
    acceptance_boundary = paste(
      "Select lambda lexicographically from training OOF metrics; do not use",
      "positive-control outcomes as a tie-breaker."
    )
  ),
  data.table::data.table(
    priority = 7L,
    area = "vinyl/PVA route",
    evidence = sprintf(
      paste0(
        "PE-to-PVA changed from %.0f to %.0f particles, while overall ",
        "specific accuracy changed %.2f pp."
      ),
      target_reference$polyethylene_to_pva[[1L]],
      target_candidate$polyethylene_to_pva[[1L]],
      targeted_delta$specific_delta_pp[[1L]]
    ),
    action = paste(
      "Preserve the narrow vinyl alcohol/acetate/acetal correction if it remains",
      "source-held robust, but repair upstream PE and environmental routing rather",
      "than weakening the PVA boundary."
    ),
    acceptance_boundary = paste(
      "Require source-held PVOH/PVB/PVAc/EVA evidence and a fresh holdout with",
      "true vinyl-family examples; the current controls contain no true PVA."
    )
  )
), fill = TRUE)

input_manifest <- data.table::data.table(
  input = names(input_paths),
  path = vapply(input_paths, normalize_path, character(1L)),
  bytes = unname(file.info(input_paths)$size),
  modified_utc = format(
    file.info(input_paths)$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  ),
  sha256 = vapply(input_paths, sha256_file, character(1L)),
  runner_version = runner_version
)

outputs <- list(
  release_assessment = release_accuracy,
  release_confusions = release_confusion,
  oof_hierarchy_metrics = hierarchy,
  recall_risks = recall_risks,
  oof_route_confusions = oof_confusions,
  oof_bidirectional_confusions = bidirectional,
  oof_failure_gates = failure_gates,
  source_robustness = source_robustness,
  lambda_feasibility = lambda_feasibility,
  holdout_map_regressions = holdout_maps,
  holdout_transition_regressions = transition_regressions,
  vinyl_oof = vinyl_summary,
  targeted_change = targeted_delta,
  recommendations = recommendations,
  input_manifest = input_manifest
)
for (name in names(outputs)) {
  write_csv(
    outputs[[name]],
    file.path(comparison_dir, paste0("model_development_", name, ".csv"))
  )
}

failed_gate_table <- gate[
  applicable == TRUE & (is.na(pass) | pass == FALSE)
][order(mean_delta_pp)]
report <- c(
  "# Gated FTIR model-development assessment",
  "",
  paste0("Runner: `", runner_version, "`."),
  "",
  "## Decision",
  "",
  if (decision_pass && probe_only) {
    paste(
      "The unchanged axis-corrected probe passed every applicable locked",
      "non-inferiority check. It is not independently promotable because its",
      "restricted medoid did not undergo a fresh parent-to-medoid closure pass."
    )
  } else if (decision_pass) {
    paste(
      "The unchanged staged candidate passed every applicable locked",
      "non-inferiority check."
    )
  } else {
    paste0(
      "**Do not promote this artifact.** ", failed_checks, " of ",
      decision$checks[[1L]], " applicable locked non-inferiority checks failed; ",
      "the worst mean change was ",
      sprintf("%.2f", decision$worst_delta_pp[[1L]]), " percentage points."
    )
  },
  if (!decision_pass && probe_only) {
    paste(
      "This is an axis-corrected diagnostic probe, not a promotable full",
      "library release: its restricted medoid did not undergo a fresh",
      "parent-to-medoid closure pass."
    )
  } else {
    character()
  },
  "",
  paste(
    "This is a read-only diagnosis of frozen evidence. No model was fitted or",
    "selected, no taxonomy was changed, and the canonical validation workflow",
    "was not sourced or modified."
  ),
  "",
  "## Evidence boundary",
  "",
  paste(
    "The current positive controls are a locked confirmation set. Their map and",
    "transition failures may locate problems, but must not select a taxonomy,",
    "feature, lambda, threshold, or class merge. Develop the successor using",
    "source-component OOF evidence, freeze every choice, and assess promotion on",
    "a fresh independent holdout. The present controls may remain regression",
    "monitoring, but are not unbiased promotion evidence after this diagnosis."
  ),
  "",
  "## Candidate-aligned reference assessment context",
  "",
  paste(
    "The candidate-hash-bound assessment below uses the complete reference",
    "dataset and is not OOF generalization evidence. It is reported only to",
    "reconcile within-reference confusions with the source-held diagnostics."
  ),
  "",
  markdown_table(
    release_accuracy,
    c("artifact", "model", "technique", "spectra", "class_count",
      "overall_accuracy_pct", "macro_class_accuracy_pct", "evidence_scope")
  ),
  "",
  "## Training OOF diagnostics",
  "",
  paste(
    "Nodes use source-component holdout when support permits and retain their",
    "named grouped fallback mode otherwise; the exported node and recall tables",
    "make that distinction explicit."
  ),
  "",
  markdown_table(
    hierarchy,
    c("scope", "overall_accuracy", "macro_class_accuracy",
      "minimum_class_recall", "worst_source_accuracy", "log_loss",
      "brier_score", "supported_sources")
  ),
  "",
  "Worst class/group recall:",
  "",
  markdown_table(
    utils::head(recall_risks, 10L),
    c("taxonomy_level", "class", "spectra", "recall", "risk", "fold_mode")
  ),
  "",
  "Worst supported sources:",
  "",
  markdown_table(
    utils::head(source_robustness, 10L),
    c("scope", "source", "observations", "classes", "accuracy",
      "gap_from_scope_overall")
  ),
  "",
  paste0(
    "Lambda selection was complete and converged: ",
    ifelse(isTRUE(fit$lambda_path_complete[[1L]]) &&
             isTRUE(fit$selected_lambda_converged[[1L]]), "yes", "no"),
    ". Boundary selections: ", boundary_nodes, ". Training time: ",
    sprintf("%.2f", fit$elapsed_seconds[[1L]]), " seconds."
  ),
  "",
  "## Frozen holdout regressions",
  "",
  "Failed non-inferiority checks:",
  "",
  markdown_table(
    failed_gate_table,
    c("scope", "level", "metric_key", "n_os1", "n_candidate",
      "mean_delta_pp", "threshold_pp", "gate_status")
  ),
  "",
  "Worst maps:",
  "",
  markdown_table(
    utils::head(holdout_maps, 10L),
    c("Map", "material_type_reference", "size_stratum_reference",
      "specific_delta_pp", "plastic_delta_pp", "specific_regressed_particles",
      "broad_regressed_particles")
  ),
  "",
  "Largest regressed class transitions:",
  "",
  markdown_table(
    utils::head(transition_regressions, 10L),
    c("class_transition", "particles", "maps", "specific_regressions",
      "broad_regressions", "map_examples")
  ),
  "",
  paste0(
    "Particle-recovery compatibility passed for ", nrow(recovery),
    " metrics. The slowest completed map took ",
    sprintf("%.2f", max(runtime$elapsed_seconds, na.rm = TRUE)),
    " seconds; total recorded warnings: ", sum(runtime$warnings, na.rm = TRUE),
    "."
  ),
  "",
  "## Development pathway",
  "",
  markdown_table(
    recommendations,
    c("priority", "area", "evidence", "action", "acceptance_boundary")
  ),
  "",
  "## Reproducibility",
  "",
  paste0(
    "Runner: `", runner_version, "`. Candidate: `", cli$candidate,
    "`; reference: `", cli$reference, "`. The exact paths, sizes, timestamps,",
    " and SHA-256 hashes for every consumed file are in",
    " `model_development_input_manifest.csv`."
  )
)
report_path <- file.path(comparison_dir, "model_development_assessment.md")
write_report(report, report_path)
message("Model-development assessment written to: ", report_path)
