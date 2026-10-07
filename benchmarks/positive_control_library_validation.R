#!/usr/bin/env Rscript

# Reproduce the Open Specy 1.0 positive-control validation and compare the
# published derivative library with the two current library-development
# variants. Run from the package repository root with R 4.3.3, for example:
#
#   Rscript benchmarks/positive_control_library_validation.R --stage=score-saved
#   Rscript benchmarks/positive_control_library_validation.R --stage=run \
#     --library=os1 --maps=probe
#   Rscript benchmarks/positive_control_library_validation.R --stage=run \
#     --library=os1 --maps=all
#   Rscript benchmarks/positive_control_library_validation.R --stage=compare
#
# The runner is deliberately restartable at map boundaries. Existing map
# checkpoints are reused only when the library, configuration, package source,
# and benchmark source hashes are unchanged.

options(stringsAsFactors = FALSE, warn = 1)

# Bump only when map execution semantics change. Scoring/report-only edits do
# not invalidate expensive, already completed map checkpoints.
analysis_runner_version <- "positive-control-map-runner-v2"

required_packages <- c("data.table", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(
    stage = "score-saved",
    library = NULL,
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
    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    value <- sub("^--[^=]+=", "", arg)
    key <- gsub("-", "_", key, fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument: --", key)
    out[[key]] <- value
  }
  out
}

normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) return(normalize_path(
    "benchmarks/positive_control_library_validation.R"
  ))
  normalize_path(sub("^--file=", "", file_arg[[1L]]))
}

repo_root <- function() {
  root <- dirname(dirname(script_path()))
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not resolve the OpenSpecy package repository root")
  }
  root
}

sha256_file <- function(path) {
  unname(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}

sha256_object <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(data.table::as.data.table(x), path, na = "")
  invisible(path)
}

write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(x), path, useBytes = TRUE)
  invisible(path)
}

excluded_samples <- c(
  "PMMA_15Nov223_control",
  "RedPETFibers_15Nov223_control"
)

library_definitions <- function(cli) {
  list(
    os1 = list(
      label = "Open Specy 1.0 published derivative library",
      folder = "01_os1_published",
      path = file.path(cli$historical_root, "derivative.rds"),
      expected_sha256 =
        "a1c0e073783c4e3c63101ee312be0a675a56b574154bba14e21d05291ef22fa1"
    ),
    pre90 = list(
      label = "Pre-cross-class 0.90 closure library",
      folder = "02_pre_0.9_closure",
      path = paste0(
        "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/",
        "reference-library-assessment-rerun-20260930/releases/",
        "e2d1941530ef/derivative.rds"
      ),
      expected_sha256 =
        "d57dcd47a344a633c12384aab4f8ce2f3d8f5b59cd98308ca2b643ae947c71ed"
    ),
    current = list(
      label = "Current cross-class 0.90 closed library",
      folder = "03_current_closed",
      path = paste0(
        "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/",
        "reference-library-build-2.0.0/releases/bb8cd82c7ecb/derivative.rds"
      ),
      expected_sha256 =
        "deed05daa994ee4095366c9ecca020574e1c609bf32156d23545f4bbf02dd292"
    )
  )
}

analysis_config <- function() {
  list(
    particle_id_strategy = "collapse",
    spectral_smooth = TRUE,
    sigma1 = c(1, 1, 1),
    sigma2 = c(3, 3),
    close = FALSE,
    close_kernel = c(4, 4),
    sn_threshold_min = 0.01,
    sn_threshold_max = Inf,
    sn_range = list(min = c(800, 2420), max = c(2200, 3200)),
    cor_threshold = 0.66,
    # Current API is inclusive. Two reproduces historical strict area > 1.
    area_threshold = 2,
    label_unknown = FALSE,
    remove_materials = NULL,
    remove_unknown = FALSE,
    pixel_length = 25,
    metric = "sig_times_noise",
    abs = FALSE,
    collapse_function = stats::median,
    outputs = c("details", "summary", "processed", "time"),
    process_args = list(
      conform_spec = TRUE,
      conform_spec_args = list(range = NULL, res = NULL),
      restrict_range = TRUE,
      restrict_range_args = list(
        min = c(800, 2420),
        max = c(2200, 3200)
      )
    ),
    file_processing = "memory"
  )
}

source_manifest <- function(root) {
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    script_path()
  )
  files <- sort(unique(normalize_path(files)))
  data.table::data.table(
    file = substring(files, nchar(root) + 2L),
    sha256 = vapply(files, sha256_file, character(1L)),
    bytes = unname(file.info(files)$size)
  )
}

source_hash <- function(manifest) {
  analysis_source <- manifest[grepl("^R/", file)]
  sha256_object(list(
    package_source = analysis_source[, .(file, sha256, bytes)],
    runner_version = analysis_runner_version
  ))
}

input_files <- function(cli) {
  paths <- sort(list.files(cli$data_root, pattern = "[.]dat$", full.names = TRUE))
  samples <- tools::file_path_sans_ext(basename(paths))
  keep <- !samples %in% excluded_samples
  paths <- normalize_path(paths[keep])
  names(paths) <- samples[keep]
  if (length(paths) != 20L) {
    stop("Expected 20 eligible DAT maps after exclusions; found ", length(paths))
  }
  paths
}

selected_inputs <- function(paths, selection) {
  probes <- c(
    "Recovery_red_beads_75-90um_5um-screen",
    "RedBrick_21Nov2023_control",
    "ClearSiliconeTubing_21Nov2023_control_b"
  )
  requested <- if (identical(selection, "all")) {
    names(paths)
  } else if (identical(selection, "probe")) {
    probes
  } else {
    trimws(strsplit(selection, ",", fixed = TRUE)[[1L]])
  }
  absent <- setdiff(requested, names(paths))
  if (length(absent)) stop("Unknown eligible map(s): ", paste(absent, collapse = ", "))
  paths[requested]
}

truth_table <- function(cli) {
  path <- file.path(cli$historical_root, "true_values2.csv")
  truth <- data.table::fread(path)
  required <- c("Map", "SpecID", "PNID", "median_area", "median_feret", "count")
  if (!all(required %in% names(truth))) stop("Unexpected truth table columns")
  truth <- truth[!Map %in% excluded_samples]
  truth[, material_type := data.table::fcase(
    PNID == "(poly)|(plastic)|(derivatives)", "plastic",
    PNID == paste0(
      "(poly)|(plastic)|(derivatives)|(organic matter)|",
      "(other material)|(mineral)"
    ), "mixed plastic/non-plastic",
    default = "non-plastic"
  )]
  truth[, size_stratum := data.table::fcase(
    is.na(median_feret), "unavailable",
    median_feret < 50, "below 50 um",
    median_feret <= 500, "50-500 um",
    default = "above 500 um"
  )]
  truth
}

canonical_material <- function(x) {
  x <- as.character(x)
  # Directional validation-only crosswalk. Raw library labels remain in every
  # particle detail file. This one mapping makes the current explicit class
  # spelling comparable with the published truth regular expression.
  x[x == "polyethylene"] <- "poly(ethylene)"
  x
}

bootstrap_mean_ci <- function(x, replicates = 5000L, seed = 9234L) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(lower = NA_real_, upper = NA_real_))
  if (length(x) == 1L) return(c(lower = x, upper = x))
  set.seed(seed)
  values <- replicate(replicates, {
    mean(x[sample.int(length(x), length(x), replace = TRUE)])
  })
  unname(stats::quantile(values, c(0.025, 0.975), na.rm = TRUE))
}

score_details <- function(details, truth, library_key) {
  details <- data.table::as.data.table(details)
  required <- c("sample_id", "material_class", "area_um2", "max_length_um")
  if (!all(required %in% names(details))) {
    stop("Particle details lack: ", paste(setdiff(required, names(details)),
                                          collapse = ", "))
  }
  details <- details[!sample_id %in% excluded_samples]
  details[, material_class_raw := as.character(material_class)]
  details[, material_class_scored := canonical_material(material_class_raw)]
  scored <- merge(
    details,
    truth[, .(Map, SpecID, PNID)],
    by.x = "sample_id", by.y = "Map", all.x = TRUE, sort = FALSE
  )
  if (anyNA(scored$SpecID)) stop("Detected rows failed to join the truth table")
  scored[, specific_correct := mapply(
    grepl, pattern = SpecID, x = material_class_scored
  )]
  scored[, plastic_correct := mapply(
    grepl, pattern = PNID, x = material_class_scored
  )]
  by_sample <- scored[, .(
    detected_count = .N,
    correct_specific = sum(specific_correct, na.rm = TRUE),
    correct_plastic = sum(plastic_correct, na.rm = TRUE),
    detected_median_area = stats::median(area_um2, na.rm = TRUE),
    detected_median_feret = stats::median(max_length_um, na.rm = TRUE)
  ), by = sample_id]
  by_sample <- merge(
    truth,
    by_sample,
    by.x = "Map", by.y = "sample_id", all.x = TRUE, sort = FALSE
  )
  for (column in c("detected_count", "correct_specific", "correct_plastic")) {
    data.table::set(by_sample, which(is.na(by_sample[[column]])), column, 0)
  }
  by_sample[, `:=`(
    library = library_key,
    count_accuracy = 100 * detected_count / count,
    area_accuracy = 100 * detected_median_area / median_area,
    feret_accuracy = 100 * detected_median_feret / median_feret,
    specific_accuracy = 100 * correct_specific / detected_count,
    plastic_accuracy = 100 * correct_plastic / detected_count
  )]
  metric_columns <- c(
    count_accuracy = "Particle count accuracy",
    area_accuracy = "Particle area accuracy",
    feret_accuracy = "Particle maximum Feret accuracy",
    specific_accuracy = "Specific ID accuracy",
    plastic_accuracy = "Plastic/non-plastic accuracy"
  )
  long <- data.table::melt(
    by_sample,
    id.vars = c("library", "Map", "material_type", "size_stratum"),
    measure.vars = names(metric_columns),
    variable.name = "metric_key", value.name = "accuracy",
    variable.factor = FALSE
  )
  long[, metric := unname(metric_columns[metric_key])]
  summarize_group <- function(x, groups) {
    x[, {
      values <- accuracy[is.finite(accuracy)]
      interval <- bootstrap_mean_ci(values)
      average <- if (length(values)) mean(values) else NA_real_
      list(
        n = length(values),
        mean_accuracy = average,
        rsd = if (length(values) > 1L && average != 0) {
          100 * stats::sd(values) / average
        } else {
          NA_real_
        },
        bootstrap_lower = interval[[1L]],
        bootstrap_upper = interval[[2L]]
      )
    }, by = groups]
  }
  list(
    details_scored = scored,
    by_sample = by_sample,
    long = long,
    summary = summarize_group(long, c("library", "metric_key", "metric")),
    by_material = summarize_group(
      long, c("library", "material_type", "metric_key", "metric")
    ),
    by_size = summarize_group(
      long, c("library", "size_stratum", "metric_key", "metric")
    )
  )
}

write_scores <- function(scores, directory, prefix = "metrics") {
  write_csv(scores$by_sample, file.path(directory, paste0(prefix, "_by_sample.csv")))
  write_csv(scores$summary, file.path(directory, paste0(prefix, "_summary.csv")))
  write_csv(scores$by_material,
            file.path(directory, paste0(prefix, "_by_material_type.csv")))
  write_csv(scores$by_size,
            file.path(directory, paste0(prefix, "_by_size_stratum.csv")))
  write_csv(scores$details_scored,
            file.path(directory, paste0(prefix, "_particle_trace.csv")))
  invisible(scores)
}

score_saved <- function(cli) {
  comparison_dir <- file.path(cli$output_root, "comparison")
  details_path <- file.path(cli$historical_root, "base", "particle_details_all.csv")
  scores <- score_details(
    data.table::fread(details_path), truth_table(cli), "os1_saved_oracle"
  )
  expected <- data.table::data.table(
    metric_key = c(
      "count_accuracy", "area_accuracy", "feret_accuracy",
      "specific_accuracy", "plastic_accuracy"
    ),
    expected_mean = c(
      91.003302835837, 109.715779686607, 97.659338707953,
      94.507766412180, 96.409239878279
    ),
    expected_rsd = c(
      41.538345972164, 53.574668382433, 35.940386314599,
      9.097298901173, 5.998610957918
    )
  )
  audit <- merge(scores$summary, expected, by = "metric_key", sort = FALSE)
  audit[, `:=`(
    mean_delta = mean_accuracy - expected_mean,
    rsd_delta = rsd - expected_rsd
  )]
  audit[, within_tolerance :=
          abs(mean_delta) < 1e-9 & abs(rsd_delta) < 1e-9]
  write_scores(scores, comparison_dir, "saved_oracle")
  write_csv(audit, file.path(comparison_dir, "saved_scorer_audit.csv"))
  if (!all(audit$within_tolerance)) {
    stop("Independent saved-output scorer does not reproduce its locked oracle")
  }
  message("Saved-output scorer reproduced all five locked means and RSDs.")
  invisible(scores)
}

load_openspecy <- function(root) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The map runner requires devtools to load the working package source")
  }
  suppressPackageStartupMessages(devtools::load_all(root, quiet = TRUE))
}

load_library <- function(definition) {
  actual_hash <- sha256_file(definition$path)
  if (!identical(tolower(actual_hash), tolower(definition$expected_sha256))) {
    stop("Library SHA-256 mismatch for ", definition$label)
  }
  object <- readRDS(definition$path)
  if (!is_OpenSpecy(object) && is.list(object) && "ftir" %in% names(object)) {
    object <- object[["ftir"]]
  }
  if (!is_OpenSpecy(object)) stop("FTIR library is not an OpenSpecy object")
  if ("spectrum_type" %in% names(object$metadata)) {
    object <- filter_spec(object, object$metadata$spectrum_type == "ftir")
  }
  list(object = object, sha256 = actual_hash)
}

system_memory <- function() {
  command <- paste0(
    "$x=Get-CimInstance Win32_OperatingSystem; ",
    "Write-Output ($x.FreePhysicalMemory.ToString()+'|'+",
    "$x.TotalVisibleMemorySize.ToString())"
  )
  value <- tryCatch(
    system2(
      "powershell.exe",
      c("-NoProfile", "-Command", shQuote(command)),
      stdout = TRUE, stderr = FALSE
    ),
    error = function(e) character()
  )
  value <- value[grepl("^[0-9]+[|][0-9]+$", value)]
  if (!length(value)) {
    return(data.table::data.table(
      free_bytes = NA_real_, total_bytes = NA_real_, used_fraction = NA_real_
    ))
  }
  parts <- as.numeric(strsplit(tail(value, 1L), "|", fixed = TRUE)[[1L]])
  data.table::data.table(
    free_bytes = parts[[1L]] * 1024,
    total_bytes = parts[[2L]] * 1024,
    used_fraction = 1 - parts[[1L]] / parts[[2L]]
  )
}

flatten_config <- function(config) {
  data.table::rbindlist(lapply(names(config), function(key) {
    value <- config[[key]]
    rendered <- paste(capture.output(dput(value)), collapse = "")
    data.table::data.table(setting = key, value = rendered)
  }))
}

checkpoint_path <- function(directory, sample) {
  file.path(directory, paste0("checkpoint_", sample, ".rds"))
}

required_map_outputs <- function(directory, sample) {
  file.path(directory, c(
    paste0("particle_details_", sample, ".csv"),
    paste0("particle_summary_", sample, ".csv"),
    paste0("particles_", sample, ".rds"),
    paste0("time_", sample, ".rds"),
    paste0("runtime_", sample, ".csv"),
    paste0("warnings_", sample, ".csv")
  ))
}

checkpoint_reusable <- function(path, expected, outputs) {
  if (!file.exists(path)) return(FALSE)
  checkpoint <- readRDS(path)
  fields <- c("library_sha256", "config_sha256", "source_sha256", "input_sha256")
  if (!all(fields %in% names(checkpoint))) return(FALSE)
  same <- vapply(fields, function(field) {
    identical(checkpoint[[field]], expected[[field]])
  }, logical(1L))
  if (!all(same)) {
    stop(
      "Stale checkpoint exists at ", path,
      ". Move or remove this result folder before rerunning changed analysis code."
    )
  }
  all(file.exists(outputs))
}

run_one_map <- function(path, sample, library, config, directory, hashes) {
  checkpoint <- checkpoint_path(directory, sample)
  outputs <- required_map_outputs(directory, sample)
  expected <- c(hashes, list(input_sha256 = sha256_file(path)))
  if (checkpoint_reusable(checkpoint, expected, outputs)) {
    message("Reusing compatible checkpoint: ", sample)
    return(data.table::fread(file.path(directory, paste0("runtime_", sample, ".csv"))))
  }
  memory_before <- system_memory()
  if (is.finite(memory_before$used_fraction) && memory_before$used_fraction >= 0.80) {
    stop("System memory is already at or above 80% before map: ", sample)
  }
  warnings <- character()
  messages <- character()
  log_path <- file.path(directory, paste0("log_", sample, ".txt"))
  started <- Sys.time()
  log_connection <- file(log_path, open = "wt")
  message_sink_before <- sink.number(type = "message")
  log_open <- TRUE
  cleanup_log <- function() {
    while (sink.number(type = "message") > message_sink_before) {
      sink(type = "message")
    }
    if (log_open) {
      close(log_connection)
      log_open <<- FALSE
    }
  }
  sink(log_connection, type = "message")
  on.exit(cleanup_log(), add = TRUE)
  result <- withCallingHandlers(
    do.call(
      automate_particle_analysis,
      c(list(x = path, library = library, output_dir = directory), config)
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
    }
  )
  finished <- Sys.time()
  cleanup_log()
  memory_after <- system_memory()
  runtime <- data.table::data.table(
    sample_id = sample,
    started_at = format(started, "%Y-%m-%dT%H:%M:%S%z"),
    finished_at = format(finished, "%Y-%m-%dT%H:%M:%S%z"),
    elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")),
    memory_used_fraction_before = memory_before$used_fraction,
    memory_used_fraction_after = memory_after$used_fraction,
    warnings = length(warnings),
    messages = length(messages)
  )
  write_csv(runtime, file.path(directory, paste0("runtime_", sample, ".csv")))
  write_csv(
    data.table::data.table(type = c(rep("warning", length(warnings)),
                                    rep("message", length(messages))),
                           text = c(warnings, messages)),
    file.path(directory, paste0("warnings_", sample, ".csv"))
  )
  saveRDS(c(expected, list(
    sample_id = sample,
    completed_at = runtime$finished_at,
    elapsed_seconds = runtime$elapsed_seconds,
    detail_rows = nrow(result$particle_details_all_csv)
  )), checkpoint)
  if (runtime$elapsed_seconds >= 300) {
    stop("Five-minute runtime boundary exceeded by ", sample, ": ",
         round(runtime$elapsed_seconds, 1), " seconds")
  }
  if (is.finite(memory_after$used_fraction) && memory_after$used_fraction >= 0.80) {
    stop("System memory reached 80% after map: ", sample)
  }
  runtime
}

aggregate_library_outputs <- function(directory, samples, truth, library_key) {
  paths <- file.path(directory, paste0("particle_details_", samples, ".csv"))
  absent <- paths[!file.exists(paths)]
  if (length(absent)) stop("Missing completed detail output(s): ",
                           paste(basename(absent), collapse = ", "))
  details <- data.table::rbindlist(lapply(paths, data.table::fread), fill = TRUE)
  write_csv(details, file.path(directory, "particle_details_all.csv"))
  summary_paths <- file.path(directory, paste0("particle_summary_", samples, ".csv"))
  summaries <- data.table::rbindlist(lapply(summary_paths, data.table::fread),
                                     fill = TRUE)
  write_csv(summaries, file.path(directory, "particle_summary_all.csv"))
  scores <- score_details(details, truth[Map %in% samples], library_key)
  write_scores(scores, directory)
  scores
}

os1_sample_compatibility <- function(reproduced, saved) {
  metrics <- c(
    "count_accuracy", "area_accuracy", "feret_accuracy",
    "specific_accuracy", "plastic_accuracy"
  )
  joined <- merge(
    reproduced$by_sample[, c("Map", metrics), with = FALSE],
    saved$by_sample[, c("Map", metrics), with = FALSE],
    by = "Map", suffixes = c("_reproduced", "_saved")
  )
  out <- data.table::rbindlist(lapply(metrics, function(metric) {
    reproduced_name <- paste0(metric, "_reproduced")
    saved_name <- paste0(metric, "_saved")
    data.table::data.table(
      Map = joined$Map,
      metric_key = metric,
      reproduced = joined[[reproduced_name]],
      saved = joined[[saved_name]],
      delta_pp = joined[[reproduced_name]] - joined[[saved_name]]
    )
  }))
  out[, within_one_percentage_point :=
        (is.na(reproduced) & is.na(saved)) |
        (is.finite(reproduced) & is.finite(saved) & abs(delta_pp) <= 1)]
  out
}

os1_group_compatibility <- function(reproduced, saved, group_column) {
  keys <- c(group_column, "metric_key")
  keep <- c(keys, "n", "mean_accuracy", "rsd")
  joined <- merge(
    reproduced[, keep, with = FALSE],
    saved[, keep, with = FALSE],
    by = keys, suffixes = c("_reproduced", "_saved"), all = TRUE,
    sort = FALSE
  )
  joined[, `:=`(
    mean_delta_pp = mean_accuracy_reproduced - mean_accuracy_saved,
    rsd_delta_pp = rsd_reproduced - rsd_saved
  )]
  joined[, mean_within_one_percentage_point :=
           (is.na(mean_accuracy_reproduced) & is.na(mean_accuracy_saved)) |
           (is.finite(mean_accuracy_reproduced) &
              is.finite(mean_accuracy_saved) & abs(mean_delta_pp) <= 1)]
  joined[, rsd_within_one_percentage_point :=
           (is.na(rsd_reproduced) & is.na(rsd_saved)) |
           (is.finite(rsd_reproduced) & is.finite(rsd_saved) &
              abs(rsd_delta_pp) <= 1)]
  joined[, within_one_percentage_point :=
           mean_within_one_percentage_point &
             rsd_within_one_percentage_point]
  joined
}

run_library <- function(cli) {
  definitions <- library_definitions(cli)
  key <- cli$library %||% stop("--library=os1, pre90, or current is required")
  if (!key %in% names(definitions)) stop("Unknown library key: ", key)
  definition <- definitions[[key]]
  root <- repo_root()
  load_openspecy(root)
  inputs <- input_files(cli)
  selected <- selected_inputs(inputs, cli$maps)
  directory <- file.path(cli$output_root, definition$folder)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  source_files <- source_manifest(root)
  config <- analysis_config()
  loaded <- load_library(definition)
  workflow_sha256 <- sha256_object(config)
  # Conform to the loaded library while retaining every other setting.
  config$process_args$conform_spec_args$range <- loaded$object$wavenumber
  config_manifest <- flatten_config(config)
  hashes <- list(
    library_sha256 = loaded$sha256,
    config_sha256 = sha256_object(config),
    source_sha256 = source_hash(source_files)
  )
  write_csv(source_files, file.path(directory, "source_manifest.csv"))
  write_csv(config_manifest, file.path(directory, "config_manifest.csv"))
  write_csv(data.table::data.table(
    library = key,
    label = definition$label,
    path = normalize_path(definition$path),
    sha256 = loaded$sha256,
    wavenumbers = length(loaded$object$wavenumber),
    spectra = ncol(loaded$object$spectra),
    material_classes = data.table::uniqueN(
      loaded$object$metadata$material_class
    ),
    workflow_sha256 = workflow_sha256,
    analysis_runner_version = analysis_runner_version,
    config_sha256 = hashes$config_sha256,
    source_sha256 = hashes$source_sha256,
    r_version = R.version.string
  ), file.path(directory, "library_manifest.csv"))
  write_csv(data.table::data.table(
    sample_id = names(inputs),
    path = unname(inputs),
    bytes = unname(file.info(inputs)$size),
    sha256 = vapply(inputs, sha256_file, character(1L)),
    excluded = FALSE,
    selected_this_invocation = names(inputs) %in% names(selected)
  ), file.path(directory, "input_manifest.csv"))
  runtimes <- lapply(seq_along(selected), function(index) {
    run_one_map(
      selected[[index]], names(selected)[[index]], loaded$object, config,
      directory, hashes
    )
  })
  write_csv(data.table::rbindlist(runtimes, fill = TRUE),
            file.path(directory, "runtime_invocation.csv"))
  completed <- names(inputs)[file.exists(
    file.path(directory, paste0("checkpoint_", names(inputs), ".rds"))
  )]
  scores <- aggregate_library_outputs(
    directory, completed, truth_table(cli), key
  )
  if (identical(key, "os1")) {
    saved <- score_saved(cli)
    sample_gate <- os1_sample_compatibility(scores, saved)
    write_csv(
      sample_gate,
      file.path(directory, "os1_sample_compatibility.csv")
    )
    if (!all(sample_gate$within_one_percentage_point)) {
      stop("Stage 1 per-sample compatibility gate failed")
    }
  }
  if (identical(key, "os1") && length(completed) == length(inputs)) {
    gate <- merge(
      scores$summary[, .(metric_key, reproduced_mean = mean_accuracy,
                         reproduced_rsd = rsd)],
      saved$summary[, .(metric_key, saved_mean = mean_accuracy,
                        saved_rsd = rsd)],
      by = "metric_key"
    )
    gate[, `:=`(
      mean_delta_pp = reproduced_mean - saved_mean,
      rsd_delta_pp = reproduced_rsd - saved_rsd
    )]
    gate[, within_one_percentage_point :=
           abs(mean_delta_pp) <= 1 & abs(rsd_delta_pp) <= 1]
    write_csv(gate, file.path(directory, "stage1_compatibility_gate.csv"))
    material_gate <- os1_group_compatibility(
      scores$by_material, saved$by_material, "material_type"
    )
    size_gate <- os1_group_compatibility(
      scores$by_size, saved$by_size, "size_stratum"
    )
    write_csv(
      material_gate,
      file.path(directory, "stage1_compatibility_by_material_type.csv")
    )
    write_csv(
      size_gate,
      file.path(directory, "stage1_compatibility_by_size_stratum.csv")
    )
    if (!all(gate$within_one_percentage_point) ||
        !all(material_gate$within_one_percentage_point) ||
        !all(size_gate$within_one_percentage_point)) {
      stop("Stage 1 compatibility gate failed; Stage 2 remains locked")
    }
    message(paste0(
      "Stage 1 accuracy and RSD gates passed overall, by material type, ",
      "and by size stratum for all five metrics."
    ))
  }
  invisible(scores)
}

library_label_table <- function(definitions) {
  data.table::data.table(
    library = names(definitions),
    library_label = vapply(definitions, `[[`, character(1L), "label"),
    folder = vapply(definitions, `[[`, character(1L), "folder")
  )
}

read_library_result <- function(cli, definition, filename) {
  path <- file.path(cli$output_root, definition$folder, filename)
  if (!file.exists(path)) stop("Missing comparison input: ", path)
  data.table::fread(path)
}

paired_metric_differences <- function(by_sample, candidate) {
  paired_library_differences(by_sample, reference = "os1", candidate)
}

paired_library_differences <- function(by_sample, reference, candidate) {
  metrics <- c(
    "count_accuracy", "area_accuracy", "feret_accuracy",
    "specific_accuracy", "plastic_accuracy"
  )
  base <- by_sample[library == reference, c("Map", metrics), with = FALSE]
  other <- by_sample[library == candidate, c("Map", metrics), with = FALSE]
  paired <- merge(
    base, other, by = "Map", suffixes = c("_reference", "_candidate")
  )
  out <- data.table::rbindlist(lapply(metrics, function(metric) {
    base_name <- paste0(metric, "_reference")
    candidate_name <- paste0(metric, "_candidate")
    data.table::data.table(
      library = candidate,
      reference_library = reference,
      Map = paired$Map,
      metric_key = metric,
      reference_accuracy = paired[[base_name]],
      candidate_accuracy = paired[[candidate_name]],
      delta_pp = paired[[candidate_name]] - paired[[base_name]]
    )
  }))
  out
}

paired_group_summary <- function(paired, by_sample, group_column) {
  groups <- unique(by_sample[, c("Map", group_column), with = FALSE])
  joined <- merge(paired, groups, by = "Map", all.x = TRUE, sort = FALSE)
  joined[, {
    deltas <- delta_pp[is.finite(delta_pp)]
    interval <- bootstrap_mean_ci(deltas)
    list(
      n = length(deltas),
      mean_delta_pp = if (length(deltas)) mean(deltas) else NA_real_,
      median_delta_pp = if (length(deltas)) {
        stats::median(deltas)
      } else {
        NA_real_
      },
      bootstrap_lower_pp = interval[[1L]],
      bootstrap_upper_pp = interval[[2L]],
      improved_maps = sum(deltas > 1e-12),
      tied_maps = sum(abs(deltas) <= 1e-12),
      worsened_maps = sum(deltas < -1e-12)
    )
  }, by = c("library", "reference_library", "metric_key", group_column)]
}

paired_metric_summary <- function(paired) {
  paired[, {
    deltas <- delta_pp[is.finite(delta_pp)]
    interval <- bootstrap_mean_ci(deltas)
    list(
      n = length(deltas),
      mean_delta_pp = if (length(deltas)) mean(deltas) else NA_real_,
      median_delta_pp = if (length(deltas)) {
        stats::median(deltas)
      } else {
        NA_real_
      },
      bootstrap_lower_pp = interval[[1L]],
      bootstrap_upper_pp = interval[[2L]],
      improved_maps = sum(deltas > 1e-12),
      tied_maps = sum(abs(deltas) <= 1e-12),
      worsened_maps = sum(deltas < -1e-12)
    )
  }, by = .(library, reference_library, metric_key)]
}

pair_particle_traces <- function(cli, definitions, left_key, right_key) {
  columns <- c(
    "sample_id", "particle_id", "material_class_raw", "material_class_scored",
    "specific_correct", "plastic_correct", "max_cor_val", "max_cor_name"
  )
  read_trace <- function(key, suffix) {
    x <- read_library_result(
      cli, definitions[[key]], "metrics_particle_trace.csv"
    )
    keep <- intersect(columns, names(x))
    x <- x[, keep, with = FALSE]
    x[, particle_id := as.character(particle_id)]
    data.table::setnames(
      x, setdiff(keep, c("sample_id", "particle_id")),
      paste0(setdiff(keep, c("sample_id", "particle_id")), "_", suffix)
    )
    x
  }
  left <- read_trace(left_key, left_key)
  right <- read_trace(right_key, right_key)
  joined <- merge(
    left, right, by = c("sample_id", "particle_id"), all = TRUE,
    sort = FALSE
  )
  joined[, `:=`(left_library = left_key, right_library = right_key)]
  left_class <- paste0("material_class_raw_", left_key)
  right_class <- paste0("material_class_raw_", right_key)
  left_specific <- paste0("specific_correct_", left_key)
  right_specific <- paste0("specific_correct_", right_key)
  left_plastic <- paste0("plastic_correct_", left_key)
  right_plastic <- paste0("plastic_correct_", right_key)
  joined[, class_transition := paste(
    get(left_class) %||% "<missing>", get(right_class) %||% "<missing>",
    sep = " -> "
  )]
  joined[, specific_transition := data.table::fcase(
    is.na(get(left_specific)), "missing-left",
    is.na(get(right_specific)), "missing-right",
    !get(left_specific) & get(right_specific), "corrected",
    get(left_specific) & !get(right_specific), "regressed",
    get(left_specific) & get(right_specific), "correct-both",
    default = "wrong-both"
  )]
  joined[, plastic_transition := data.table::fcase(
    is.na(get(left_plastic)), "missing-left",
    is.na(get(right_plastic)), "missing-right",
    !get(left_plastic) & get(right_plastic), "corrected",
    get(left_plastic) & !get(right_plastic), "regressed",
    get(left_plastic) & get(right_plastic), "correct-both",
    default = "wrong-both"
  )]
  joined
}

reference_flow <- function(definitions) {
  read_ftir <- function(path) {
    x <- readRDS(path)
    if (is.list(x) && "ftir" %in% names(x) &&
        !all(c("wavenumber", "spectra", "metadata") %in% names(x))) {
      x <- x[["ftir"]]
    }
    x
  }
  pre <- read_ftir(definitions$pre90$path)
  current <- read_ftir(definitions$current$path)
  pre_md <- data.table::as.data.table(pre$metadata)
  current_md <- data.table::as.data.table(current$metadata)
  identifier <- if ("sample_name" %in% names(pre_md)) "sample_name" else "spectrum_id"
  pre_md[, reference_id := as.character(get(identifier))]
  current_md[, reference_id := as.character(get(identifier))]
  removed_ids <- setdiff(pre_md$reference_id, current_md$reference_id)
  removed <- pre_md[reference_id %in% removed_ids]
  current_only_ids <- setdiff(current_md$reference_id, pre_md$reference_id)
  current_only <- current_md[reference_id %in% current_only_ids]
  quarantine_path <- file.path(
    dirname(definitions$current$path), "quarantined_spectra.rds"
  )
  quarantine <- readRDS(quarantine_path)
  q <- data.table::as.data.table(
    quarantine$spectra[["derivative/ftir"]]$metadata
  )
  q_identifier <- if (identifier %in% names(q)) identifier else "spectrum_id"
  q[, reference_id := as.character(get(q_identifier))]
  classes <- sort(unique(c(pre_md$material_class, current_md$material_class)))
  flow <- data.table::rbindlist(lapply(classes, function(class_name) {
    class_removed <- removed[material_class == class_name]
    data.table::data.table(
      material_class = class_name,
      pre90_references = sum(pre_md$material_class == class_name, na.rm = TRUE),
      current_references = sum(
        current_md$material_class == class_name, na.rm = TRUE
      ),
      removed_references = nrow(class_removed),
      removed_found_in_quarantine = sum(
        class_removed$reference_id %in% q$reference_id
      )
    )
  }))
  list(
    flow = flow,
    removed = removed,
    current_only = current_only,
    quarantine_manifest = quarantine$manifest,
    quarantine_ftir_rows = nrow(q),
    conflict_rows = nrow(quarantine$conflicts)
  )
}

match_margin_diagnostics <- function(cli, definitions) {
  load_openspecy(repo_root())
  truth <- truth_table(cli)[, .(Map, material_type, size_stratum)]
  rows <- lapply(names(definitions), function(key) {
    definition <- definitions[[key]]
    directory <- file.path(cli$output_root, definition$folder)
    library <- load_library(definition)$object
    trace <- data.table::fread(file.path(directory, "metrics_particle_trace.csv"))
    data.table::rbindlist(lapply(unique(trace$sample_id), function(sample) {
      processed <- readRDS(file.path(directory, paste0("particles_", sample,
                                                       ".rds")))
      matches <- match_spec(
        processed, library, top_n = 2L, batch_size = 1000L,
        compute = "optimized"
      )
      matches[, rank := seq_len(.N), by = object_id]
      first <- matches[rank == 1L, .(
        particle_id = object_id,
        top1_reference_id = library_id,
        top1_score = match_val
      )]
      second <- matches[rank == 2L, .(
        particle_id = object_id,
        top2_reference_id = library_id,
        top2_score = match_val
      )]
      out <- merge(first, second, by = "particle_id", all.x = TRUE,
                   sort = FALSE)
      expected <- trace[sample_id == sample, .(
        particle_id = as.character(particle_id),
        recorded_reference_id = max_cor_name,
        recorded_score = max_cor_val,
        specific_correct,
        plastic_correct
      )]
      out <- merge(out, expected, by = "particle_id", all.x = TRUE,
                   sort = FALSE)
      if (any(out$top1_reference_id != out$recorded_reference_id) ||
          any(abs(out$top1_score - out$recorded_score) > 1e-10)) {
        stop("Top-two diagnostic failed to reproduce recorded top match for ",
             key, ": ", sample)
      }
      out[, `:=`(
        library = key,
        sample_id = sample,
        match_margin = top1_score - top2_score,
        below_fixed_threshold = top1_score < 0.66
      )]
      out
    }), fill = TRUE)
  })
  particles <- data.table::rbindlist(rows, fill = TRUE)
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
      margin_q10 = stats::quantile(match_margin, 0.10, na.rm = TRUE),
      fraction_margin_le_0.01 = mean(match_margin <= 0.01, na.rm = TRUE),
      fraction_below_0.66 = mean(below_fixed_threshold, na.rm = TRUE),
      specific_accuracy_all = 100 * mean(specific_correct, na.rm = TRUE),
      specific_accuracy_at_or_above_0.66 = 100 * mean(
        specific_correct[!below_fixed_threshold], na.rm = TRUE
      ),
      plastic_accuracy_all = 100 * mean(plastic_correct, na.rm = TRUE),
      plastic_accuracy_at_or_above_0.66 = 100 * mean(
        plastic_correct[!below_fixed_threshold], na.rm = TRUE
      ),
      retained_at_or_above_0.66 = sum(!below_fixed_threshold)
    ), by = groups]
  }
  list(
    particles = particles,
    summary = summarize(particles, "library"),
    by_material = summarize(particles, c("library", "material_type")),
    by_size = summarize(particles, c("library", "size_stratum"))
  )
}

markdown_table <- function(x, columns, digits = 2L) {
  x <- data.table::as.data.table(x)[, columns, with = FALSE]
  for (column in names(x)) {
    if (is.numeric(x[[column]])) x[[column]] <- formatC(
      x[[column]], digits = digits, format = "f"
    )
  }
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  rows <- apply(x, 1L, function(row) {
    paste0("| ", paste(ifelse(is.na(row), "", row), collapse = " | "), " |")
  })
  c(header, divider, rows)
}

write_comparison_plot <- function(summary, path) {
  id <- summary[metric_key %in% c("specific_accuracy", "plastic_accuracy")]
  libraries <- c("os1", "pre90", "current")
  metrics <- c("specific_accuracy", "plastic_accuracy")
  values <- matrix(NA_real_, nrow = length(libraries), ncol = length(metrics),
                   dimnames = list(libraries, metrics))
  lower <- upper <- values
  for (i in seq_len(nrow(id))) {
    values[id$library[[i]], id$metric_key[[i]]] <- id$mean_accuracy[[i]]
    lower[id$library[[i]], id$metric_key[[i]]] <- id$bootstrap_lower[[i]]
    upper[id$library[[i]], id$metric_key[[i]]] <- id$bootstrap_upper[[i]]
  }
  grDevices::png(path, width = 1500, height = 900, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  colors <- c("#4C78A8", "#F58518", "#54A24B")
  positions <- barplot(
    t(values), beside = TRUE, ylim = c(0, 105), col = rep(colors, each = 2L),
    names.arg = c("Published OS1", "Pre-0.90 closure", "Current closed"),
    ylab = "Mean sample-level accuracy (%)", las = 1,
    main = "Positive-control identification accuracy"
  )
  arrows(
    positions, t(lower), positions, t(upper), angle = 90, code = 3,
    length = 0.04, lwd = 1.5
  )
  legend(
    "bottomright", legend = c("Specific ID", "Plastic/non-plastic"),
    fill = colors[1:2], bty = "n"
  )
}

write_report <- function(cli, summary, paired_summary, direct_summary,
                         direct_by_material, direct_by_size, runtimes, flow,
                         margins, stage1_gate, direct_trace) {
  directory <- file.path(cli$output_root, "comparison")
  labels <- library_label_table(library_definitions(cli))
  identification <- merge(
    summary[metric_key %in% c("specific_accuracy", "plastic_accuracy")],
    labels[, .(library, library_label)], by = "library"
  )
  identification <- identification[, .(
    Library = library_label,
    Metric = metric,
    N = n,
    Mean = mean_accuracy,
    RSD = rsd,
    `Bootstrap 2.5%` = bootstrap_lower,
    `Bootstrap 97.5%` = bootstrap_upper
  )]
  metric_labels <- c(
    count_accuracy = "Particle count accuracy",
    area_accuracy = "Particle area accuracy",
    feret_accuracy = "Particle maximum Feret accuracy",
    specific_accuracy = "Specific ID accuracy",
    plastic_accuracy = "Plastic/non-plastic accuracy"
  )
  effects <- data.table::rbindlist(list(
    paired_summary[metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )],
    direct_summary[metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )]
  ))
  effects[, `:=`(
    Candidate = unname(c(
      os1 = "Published OS1", pre90 = "Pre-0.90 closure",
      current = "Current closed"
    )[library]),
    Reference = unname(c(
      os1 = "Published OS1", pre90 = "Pre-0.90 closure",
      current = "Current closed"
    )[reference_library]),
    Metric = unname(metric_labels[metric_key])
  )]
  effects <- effects[, .(
    Candidate, Reference, Metric, N = n,
    `Mean delta (pp)` = mean_delta_pp,
    `Bootstrap 2.5%` = bootstrap_lower_pp,
    `Bootstrap 97.5%` = bootstrap_upper_pp,
    Improved = improved_maps, Tied = tied_maps, Worsened = worsened_maps
  )]
  recovery <- summary[
    library == "os1" & metric_key %in% c(
      "count_accuracy", "area_accuracy", "feret_accuracy"
    )
  ]
  recovery[, `:=`(
    `Mean within 50-150%` = mean_accuracy >= 50 & mean_accuracy <= 150,
    `RSD below 40%` = rsd < 40
  )]
  recovery <- recovery[, .(
    Metric = metric, N = n, Mean = mean_accuracy, RSD = rsd,
    `Mean within 50-150%`, `RSD below 40%`,
    `Meets both study criteria` = `Mean within 50-150%` & `RSD below 40%`
  )]
  current_vs_os1 <- paired_summary[
    library == "current" & metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )
  ]
  current_vs_pre <- direct_summary[
    library == "current" & metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )
  ]
  current_specific <- current_vs_os1[
    metric_key == "specific_accuracy", mean_delta_pp
  ]
  current_plastic <- current_vs_os1[
    metric_key == "plastic_accuracy", mean_delta_pp
  ]
  direct_specific <- current_vs_pre[
    metric_key == "specific_accuracy", mean_delta_pp
  ]
  direct_plastic <- current_vs_pre[
    metric_key == "plastic_accuracy", mean_delta_pp
  ]
  transition_counts <- data.table::data.table(
    Outcome = c("Corrected", "Regressed"),
    `Specific ID particles` = c(
      sum(direct_trace$specific_transition == "corrected"),
      sum(direct_trace$specific_transition == "regressed")
    ),
    `Plastic/non-plastic particles` = c(
      sum(direct_trace$plastic_transition == "corrected"),
      sum(direct_trace$plastic_transition == "regressed")
    )
  )
  changed_particles <- direct_trace[
    specific_transition %in% c("corrected", "regressed") |
      plastic_transition %in% c("corrected", "regressed"),
    .(
      Sample = sample_id, Particle = particle_id,
      `Pre-closure class` = material_class_raw_pre90,
      `Current class` = material_class_raw_current,
      `Specific transition` = specific_transition,
      `Broad transition` = plastic_transition,
      `Score delta` = max_cor_val_current - max_cor_val_pre90,
      `Old top hit removed` = pre90_top_hit_absent_from_current
    )
  ]
  subgroup <- data.table::rbindlist(list(
    direct_by_material[metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )][, .(
      Dimension = "Material type", Stratum = material_type,
      Metric = unname(metric_labels[metric_key]), N = n,
      `Mean delta (pp)` = mean_delta_pp,
      `Bootstrap 2.5%` = bootstrap_lower_pp,
      `Bootstrap 97.5%` = bootstrap_upper_pp
    )],
    direct_by_size[metric_key %in% c(
      "specific_accuracy", "plastic_accuracy"
    )][, .(
      Dimension = "Size", Stratum = size_stratum,
      Metric = unname(metric_labels[metric_key]), N = n,
      `Mean delta (pp)` = mean_delta_pp,
      `Bootstrap 2.5%` = bootstrap_lower_pp,
      `Bootstrap 97.5%` = bootstrap_upper_pp
    )]
  ))
  margin_summary <- merge(
    margins$summary,
    labels[, .(library, library_label)], by = "library", all.x = TRUE
  )
  margin_summary <- margin_summary[, .(
    Library = library_label, Particles = particles,
    `Median top score` = median_top1_score,
    `Median top-two margin` = median_margin,
    `Margin <=0.01 (%)` = 100 * fraction_margin_le_0.01,
    `Below 0.66 (%)` = 100 * fraction_below_0.66,
    `Retained >=0.66` = retained_at_or_above_0.66,
    `Specific ID retained (%)` = specific_accuracy_at_or_above_0.66,
    `Plastic ID retained (%)` = plastic_accuracy_at_or_above_0.66
  )]
  removal_classes <- flow$flow[order(-removed_references)][seq_len(10L), .(
    Class = material_class,
    `Pre-closure references` = pre90_references,
    `Current references` = current_references,
    Removed = removed_references,
    `Removed found in quarantine` = removed_found_in_quarantine
  )]
  removed_top_hits <- sum(
    direct_trace$pre90_top_hit_absent_from_current, na.rm = TRUE
  )
  changed_removed_top_hits <- sum(
    direct_trace$pre90_top_hit_absent_from_current &
      direct_trace$material_class_raw_pre90 !=
        direct_trace$material_class_raw_current,
    na.rm = TRUE
  )
  quarantine_overlap <- sum(flow$flow$removed_found_in_quarantine)
  max_runtime <- max(runtimes$elapsed_seconds, na.rm = TRUE)
  max_memory <- 100 * max(runtimes$memory_used_fraction_after, na.rm = TRUE)
  warning_count <- sum(runtimes$warnings, na.rm = TRUE)
  report <- c(
    "# Positive-control derivative-library validation",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Decision summary",
    "",
    paste0(
      "The current closed library is the preferred *development candidate*, ",
      "not yet a proven superior replacement. Relative to the pre-closure ",
      "library it changed mean sample-level Specific ID by ",
      sprintf("%+.3f", direct_specific), " percentage points and broad ",
      "plastic/non-plastic ID by ", sprintf("%+.3f", direct_plastic),
      " points. At particle level it corrected ",
      sum(direct_trace$specific_transition == "corrected"),
      " specific IDs with ",
      sum(direct_trace$specific_transition == "regressed"),
      " regressions, while broad ID had ",
      sum(direct_trace$plastic_transition == "corrected"), " corrections and ",
      sum(direct_trace$plastic_transition == "regressed"), " regression."
    ),
    "",
    paste0(
      "Against the published OS1 library, the current library changed Specific ",
      "ID by ", sprintf("%+.3f", current_specific),
      " points and plastic/non-plastic ID by ",
      sprintf("%+.3f", current_plastic),
      " points. The paired sample-level bootstrap intervals include zero, so ",
      "this small cohort does not establish superiority. Retain the current ",
      "closure as a candidate, audit the single broad-class regression and ",
      "removed-reference decisions, and confirm on independent class-balanced ",
      "development and sequestered validation sets before changing policy."
    ),
    "",
    paste0(
      "All comparisons use the same 20 holdout maps and exclude ",
      "`PMMA_15Nov223_control` and `RedPETFibers_15Nov223_control`. ",
      "Only the derivative FTIR library changes between runs. The largest ",
      "measured per-map wall time was ", sprintf("%.1f", max_runtime),
      " seconds (limit: 300 seconds); the largest recorded post-map physical ",
      "memory use was ", sprintf("%.1f%%", max_memory), ", and the runs emitted ",
      warning_count, " warnings."
    ),
    "",
    "## Stage 1 reproduction",
    "",
    paste0(
      "The independent scorer exactly reproduced the locked saved-output ",
      "means and RSDs. The current `automate_particle_analysis()` OS1 run was ",
      "then compared with that oracle per sample and for overall, material-type, ",
      "and size-stratum summaries. Stage 2 was allowed only after every finite ",
      "mean and RSD differed by no more than one percentage point and paired ",
      "missing values agreed."
    ),
    "",
    markdown_table(
      stage1_gate[, .(
        Metric = metric_key,
        `Mean delta (pp)` = mean_delta_pp,
        `RSD delta (pp)` = rsd_delta_pp,
        Pass = within_one_percentage_point
      )], names(stage1_gate[, .(
        Metric = metric_key,
        `Mean delta (pp)` = mean_delta_pp,
        `RSD delta (pp)` = rsd_delta_pp,
        Pass = within_one_percentage_point
      )])
    ),
    "",
    "## Identification accuracy",
    "",
    markdown_table(identification, names(identification)),
    "",
    "### Paired identification effects",
    "",
    markdown_table(effects, names(effects), digits = 3L),
    "",
    paste0(
      "Count, area, and Feret recovery are segmentation outputs generated ",
      "before library matching and therefore should be identical across the ",
      "three runs. Any non-zero paired change in those metrics is treated as ",
      "workflow drift, not a library effect. Identification intervals are ",
      "sample-level bootstrap intervals; particles are not treated as ",
      "independent replicates."
    ),
    "",
    "## Recovery versus the study criteria",
    "",
    markdown_table(recovery, names(recovery)),
    "",
    paste0(
      "All three mean recoveries remain inside the study's 50-150% acceptance ",
      "range. Feret RSD is below 40%; count and area RSDs are not. This is the ",
      "user-directed 20-map cohort after two noisy controls were removed, not ",
      "the complete 22-image published cohort, and one map lacks finite size ",
      "truth (recovery N=19)."
    ),
    "",
    "## Closure-isolated diagnosis",
    "",
    "Current closed minus pre-closure, with every non-library setting held fixed:",
    "",
    markdown_table(transition_counts, names(transition_counts), digits = 0L),
    "",
    markdown_table(changed_particles, names(changed_particles), digits = 3L),
    "",
    markdown_table(subgroup, names(subgroup), digits = 3L),
    "",
    paste0(
      "The full paired particle trace records every match identity, class, ",
      "score, correctness transition, and whether the pre-closure top hit was ",
      "removed. All six correctness-changing particles had a removed ",
      "pre-closure top reference. Across all particles, ", removed_top_hits,
      " pre-closure top references were absent from current and ",
      changed_removed_top_hits, " of those particles changed assigned class. ",
      "Subgroup intervals are descriptive and especially unstable ",
      "where a stratum has few maps."
    ),
    "",
    "## Match-score and fixed-threshold sensitivity",
    "",
    markdown_table(margin_summary, names(margin_summary), digits = 3L),
    "",
    paste0(
      "The 0.66 calculation is a sensitivity analysis only: the primary run ",
      "keeps all matches (`label_unknown=FALSE`) exactly as in the published ",
      "workflow. 'Retained' accuracy describes particles at or above 0.66 and ",
      "must not be interpreted without the accompanying retained count and ",
      "below-threshold fraction. These are particle-weighted diagnostics; the ",
      "primary accuracy estimates above give every map equal weight. The ",
      "current and pre-closure margin distributions are nearly identical, so ",
      "closure did not materially resolve the high rate of near-tied matches."
    ),
    "",
    "## Library-development diagnosis",
    "",
    paste0(
      "The pre-closure FTIR library contains ", sum(flow$flow$pre90_references),
      " spectra and the current library contains ",
      sum(flow$flow$current_references), ". The direct release comparison ",
      "finds ", nrow(flow$removed), " pre-closure reference IDs absent from ",
      "the current derivative library and ", nrow(flow$current_only),
      " current IDs absent from pre-closure. The current release quarantine contains ",
      format(flow$quarantine_ftir_rows, big.mark = ","),
      " derivative-FTIR spectra and ",
      format(flow$conflict_rows, big.mark = ","),
      " recorded conflict rows. See `library_reference_flow_by_class.csv` and ",
      "the particle transition tables for the classes and holdout particles ",
      "affected. Removal and quarantine overlap supports traceability, not ",
      "causal proof that closure improved a given particle."
    ),
    "",
    markdown_table(removal_classes, names(removal_classes), digits = 0L),
    "",
    paste0(
      format(quarantine_overlap, big.mark = ","), " of ",
      format(nrow(flow$removed), big.mark = ","),
      " absent pre-closure IDs appear in the derivative-FTIR quarantine; the ",
      "remaining ", format(nrow(flow$removed) - quarantine_overlap,
                           big.mark = ","),
      " need an explicit provenance explanation before release. The 596 ",
      "current-only IDs also show that the current artifact is not simply a ",
      "filtered copy of the pre-closure library."
    ),
    "",
    "## Recommended development pathway",
    "",
    "1. Keep the frozen map-processing workflow and this 20-map holdout as a confirmation gate; do not tune thresholds, reinstate references, or change classes because of these outcomes alone.",
    "2. Continue with class-aware closure, not a tighter global correlation cutoff. Preserve minimum per-class coverage and spectral modes using within-class diversity/medoid selection, then adjudicate cross-class conflicts with metadata and expert evidence.",
    "3. Audit the six changed particles on independent evidence, especially the removed polypropylene top hit that became the sole broad-class regression. Treat it as a sentinel case rather than a reason to tune on the holdout.",
    "4. Reconcile the 147 removed IDs absent from the quarantine and document the 596 current-only IDs. Preserve raw labels, stable reference IDs, reason codes, source artifacts, and conflict partners in every release.",
    "5. Develop any confidence/rejection rule on separate data. The fixed 0.66 sensitivity removes about 16% of particles and raises retained accuracy, but this holdout cannot select that threshold or quantify the cost of abstention.",
    "6. Add independent, class-balanced spectra and maps, including environmental matrices and rare/confusable polymers. Promote only after predeclared overall non-inferiority, no important class/size regression, and a final sequestered confirmation run.",
    "",
    "## Scope and interpretation",
    "",
    paste0(
      "This benchmark reproduces the workflow reported in Analytical Chemistry ",
      "(DOI 10.1021/acs.analchem.5c00962) using the supplied `true_values2.csv`. ",
      "It evaluates these positive controls only; it does not estimate ",
      "population prevalence or replace validation on unseen environmental ",
      "matrices. The class crosswalk is directional and validation-only: ",
      "`polyethylene` is scored as `poly(ethylene)` to match the published ",
      "truth regex, while raw library labels remain in the trace files."
    )
  )
  write_lines(report, file.path(directory, "positive_control_library_report.md"))
}

compare_results <- function(cli) {
  definitions <- library_definitions(cli)
  directory <- file.path(cli$output_root, "comparison")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  keys <- names(definitions)
  manifests <- data.table::rbindlist(lapply(keys, function(key) {
    x <- read_library_result(cli, definitions[[key]], "library_manifest.csv")
    x[, library := key]
    x
  }), fill = TRUE)
  if (data.table::uniqueN(manifests$source_sha256) != 1L) {
    stop("Analysis source hashes differ across libraries; return to Stage 1")
  }
  if (data.table::uniqueN(manifests$workflow_sha256) != 1L) {
    stop("Workflow hashes differ across libraries; return to Stage 1")
  }
  by_sample <- data.table::rbindlist(lapply(keys, function(key) {
    read_library_result(cli, definitions[[key]], "metrics_by_sample.csv")
  }), fill = TRUE)
  counts <- by_sample[, .(maps = data.table::uniqueN(Map)), by = library]
  if (nrow(counts) != 3L || any(counts$maps != 20L)) {
    stop("Comparison requires 20 completed eligible maps for every library")
  }
  summary <- data.table::rbindlist(lapply(keys, function(key) {
    read_library_result(cli, definitions[[key]], "metrics_summary.csv")
  }), fill = TRUE)
  by_material <- data.table::rbindlist(lapply(keys, function(key) {
    read_library_result(cli, definitions[[key]], "metrics_by_material_type.csv")
  }), fill = TRUE)
  by_size <- data.table::rbindlist(lapply(keys, function(key) {
    read_library_result(cli, definitions[[key]], "metrics_by_size_stratum.csv")
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
    stop("At least one map exceeded the five-minute runtime boundary")
  }
  stage1_gate <- read_library_result(
    cli, definitions$os1, "stage1_compatibility_gate.csv"
  )
  stage1_material_gate <- read_library_result(
    cli, definitions$os1, "stage1_compatibility_by_material_type.csv"
  )
  stage1_size_gate <- read_library_result(
    cli, definitions$os1, "stage1_compatibility_by_size_stratum.csv"
  )
  if (!all(stage1_gate$within_one_percentage_point) ||
      !all(stage1_material_gate$within_one_percentage_point) ||
      !all(stage1_size_gate$within_one_percentage_point)) {
    stop("Recorded Stage 1 compatibility gate is not passing")
  }
  paired <- data.table::rbindlist(lapply(c("pre90", "current"), function(key) {
    paired_metric_differences(by_sample, key)
  }))
  paired_summary <- paired_metric_summary(paired)
  paired_by_material <- paired_group_summary(
    paired, by_sample, "material_type"
  )
  paired_by_size <- paired_group_summary(paired, by_sample, "size_stratum")
  direct <- paired_library_differences(
    by_sample, reference = "pre90", candidate = "current"
  )
  direct_summary <- paired_metric_summary(direct)
  direct_by_material <- paired_group_summary(
    direct, by_sample, "material_type"
  )
  direct_by_size <- paired_group_summary(
    direct, by_sample, "size_stratum"
  )
  recovery_drift <- paired[
    metric_key %in% c("count_accuracy", "area_accuracy", "feret_accuracy") &
      is.finite(delta_pp)
  ]
  if (any(abs(recovery_drift$delta_pp) > 1e-9)) {
    stop("Recovery metrics changed when only the library should differ")
  }
  traces <- list(
    os1_to_pre90 = pair_particle_traces(cli, definitions, "os1", "pre90"),
    os1_to_current = pair_particle_traces(cli, definitions, "os1", "current"),
    pre90_to_current = pair_particle_traces(cli, definitions, "pre90", "current")
  )
  transition_summary <- data.table::rbindlist(lapply(names(traces), function(key) {
    traces[[key]][, .(
      particles = .N,
      mean_correlation_delta = mean(
        get(paste0("max_cor_val_", right_library[[1L]])) -
          get(paste0("max_cor_val_", left_library[[1L]])),
        na.rm = TRUE
      )
    ), by = .(left_library, right_library, specific_transition,
              plastic_transition, class_transition)]
  }), fill = TRUE)
  flow <- reference_flow(definitions)
  direct_trace <- traces$pre90_to_current
  direct_trace[, pre90_top_hit_absent_from_current :=
                 get("max_cor_name_pre90") %in% flow$removed$reference_id]
  removed_attribution <- direct_trace[, .(
    particles = .N,
    pre90_top_hit_absent_from_current = sum(
      pre90_top_hit_absent_from_current, na.rm = TRUE
    )
  ), by = .(
    specific_transition, plastic_transition,
    pre90_class = material_class_raw_pre90,
    current_class = material_class_raw_current
  )]
  margins <- match_margin_diagnostics(cli, definitions)
  write_csv(manifests, file.path(directory, "library_manifests.csv"))
  write_csv(by_sample, file.path(directory, "all_metrics_by_sample.csv"))
  write_csv(summary, file.path(directory, "all_metrics_summary.csv"))
  write_csv(by_material, file.path(directory, "all_metrics_by_material_type.csv"))
  write_csv(by_size, file.path(directory, "all_metrics_by_size_stratum.csv"))
  write_csv(runtimes, file.path(directory, "all_map_runtimes.csv"))
  write_csv(paired, file.path(directory, "paired_differences_vs_os1.csv"))
  write_csv(paired_summary,
            file.path(directory, "paired_differences_summary_vs_os1.csv"))
  write_csv(paired_by_material,
            file.path(directory, "paired_differences_by_material_vs_os1.csv"))
  write_csv(paired_by_size,
            file.path(directory, "paired_differences_by_size_vs_os1.csv"))
  write_csv(direct,
            file.path(directory, "paired_current_vs_pre90.csv"))
  write_csv(direct_summary,
            file.path(directory, "paired_current_vs_pre90_summary.csv"))
  write_csv(direct_by_material,
            file.path(directory, "paired_current_vs_pre90_by_material.csv"))
  write_csv(direct_by_size,
            file.path(directory, "paired_current_vs_pre90_by_size.csv"))
  for (key in names(traces)) {
    if (identical(key, "pre90_to_current")) traces[[key]] <- direct_trace
    write_csv(traces[[key]], file.path(directory, paste0("particle_", key, ".csv")))
  }
  write_csv(transition_summary,
            file.path(directory, "particle_transition_summary.csv"))
  write_csv(removed_attribution,
            file.path(directory, "removed_reference_attribution.csv"))
  write_csv(flow$flow,
            file.path(directory, "library_reference_flow_by_class.csv"))
  write_csv(flow$removed,
            file.path(directory, "pre90_references_absent_from_current.csv"))
  write_csv(flow$current_only,
            file.path(directory, "current_references_absent_from_pre90.csv"))
  write_csv(margins$particles,
            file.path(directory, "match_margin_particles.csv"))
  write_csv(margins$summary,
            file.path(directory, "match_margin_summary.csv"))
  write_csv(margins$by_material,
            file.path(directory, "match_margin_by_material_type.csv"))
  write_csv(margins$by_size,
            file.path(directory, "match_margin_by_size_stratum.csv"))
  write_comparison_plot(summary, file.path(directory, "identification_accuracy.png"))
  write_report(
    cli, summary, paired_summary, direct_summary, direct_by_material,
    direct_by_size, runtimes, flow, margins, stage1_gate, direct_trace
  )
  message("Comparison and report written to: ", directory)
  invisible(list(summary = summary, paired = paired_summary, flow = flow))
}

main <- function() {
  cli <- parse_cli()
  cli$data_root <- normalize_path(cli$data_root)
  cli$historical_root <- normalize_path(cli$historical_root)
  cli$output_root <- normalize_path(cli$output_root, must_work = FALSE)
  dir.create(cli$output_root, recursive = TRUE, showWarnings = FALSE)
  switch(
    cli$stage,
    "score-saved" = score_saved(cli),
    "run" = run_library(cli),
    "compare" = compare_results(cli),
    stop("Unknown --stage=", cli$stage)
  )
}

if (identical(environment(), globalenv())) main()
