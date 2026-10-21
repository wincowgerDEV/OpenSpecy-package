tiny_build_lib <- function() {
  wavenumber <- seq(100, 6100, by = 100)
  base_a <- dnorm(seq(-3, 3, length.out = length(wavenumber)))
  base_b <- rev(cumsum(seq_along(wavenumber)) / sum(seq_along(wavenumber)))
  spectra <- sapply(seq_len(8), function(i) {
    if (i <= 4) base_a + i / 20 else base_b + i / 20
  })
  colnames(spectra) <- paste0("s", seq_len(ncol(spectra)))
  as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      source = c("A", "B", "C", "C", "A", "B", "C", "C"),
      label = c("nylon 6", "polyamides", "plastic", "missing",
                "pet", "polyesters", "plastic", "missing"),
      material_class = rep(c("class_a", "class_b"), each = 4),
      material_type = rep("plastic", 8),
      material_group = rep("fixture polymer", 8),
      spectrum_type = rep("ftir", 8),
      intensity_units = rep("absorbance", 8)
    ),
    attributes = list(intensity_unit = "absorbance")
  )
}

reference_workflow_data_path <- function(file) {
  roots <- c(
    testthat::test_path("..", ".."),
    Sys.getenv("GITHUB_WORKSPACE", unset = "")
  )
  roots <- unique(roots[nzchar(roots)])
  candidates <- file.path(roots, "workflows", "data", file)
  existing <- candidates[file.exists(candidates)]
  testthat::skip_if(
    length(existing) == 0L,
    "repository-only reference workflow tables are not installed"
  )
  existing[[1L]]
}

test_that("make_lib_lookup_template() returns or writes deduplicated templates", {
  lib <- tiny_build_lib()

  template <- make_lib_lookup_template(lib, columns = "source",
                                       add = "LibraryType")
  expect_s3_class(template, "data.table")
  expect_equal(sort(template$source), c("A", "B", "C"))
  expect_true("LibraryType" %in% names(template))
  expect_true(all(is.na(template$LibraryType)))

  tmp <- tempfile(fileext = ".csv")
  invisible(make_lib_lookup_template(lib, columns = "source",
                                     add = "LibraryType", path = tmp))
  expect_true(file.exists(tmp))
})

test_that("join_lib_metadata() reports incomplete and duplicate joins", {
  lib <- tiny_build_lib()
  lookup <- data.table::data.table(source = c("A", "B"),
                                   LibraryType = c("type_a", "type_b"))

  expect_warning(
    joined <- join_lib_metadata(lib, lookup, by = "source"),
    "unmatched_metadata_key"
  )
  expect_true(check_OpenSpecy(joined))
  expect_equal(nrow(joined$metadata), ncol(joined$spectra))
  expect_true("source" %in% names(joined$metadata))
  expect_false(any(c("source.x", "source.y") %in% names(joined$metadata)))
  expect_true("LibraryType" %in% names(joined$metadata))
  expect_error(
    join_lib_metadata(lib, lookup, by = "source", require_complete = TRUE),
    "unmatched_metadata_key"
  )

  dup_lookup <- data.table::data.table(source = c("A", "A"),
                                       LibraryType = c("x", "y"))
  expect_error(join_lib_metadata(lib, dup_lookup, by = "source"),
               "unique")
})

test_that("join_material_hierarchy() matches user-specified levels", {
  lib <- tiny_build_lib()
  hierarchy <- data.table::data.table(
    material = c("nylon 6", "pet"),
    material_class = c("polyamides", "polyesters"),
    material_type = c("plastic", "plastic")
  )

  expect_warning(
    joined <- join_material_hierarchy(
      lib,
      hierarchy = hierarchy,
      key_col = "label",
      levels = c("material", "material_class", "material_type"),
      output_names = c(material = "joined_material",
                       material_class = "joined_class",
                       material_type = "joined_type")
    ),
    "unmatched_hierarchy_key"
  )

  expect_true(check_OpenSpecy(joined))
  expect_equal(joined$metadata$joined_material[1], "nylon 6")
  expect_equal(joined$metadata$joined_class[2], "polyamides")
  expect_equal(joined$metadata$joined_type[3], "plastic")
  expect_true(is.na(joined$metadata$joined_type[4]))
})

test_that("dedupe_spec() keeps identifiers aligned", {
  lib <- tiny_build_lib()
  lib$spectra[, 2] <- lib$spectra[, 1]

  deduped <- dedupe_spec(lib)
  expect_true(check_OpenSpecy(deduped))
  expect_equal(ncol(deduped$spectra), 7)
  expect_identical(colnames(deduped$spectra),
                   deduped$metadata$sample_name)
})

test_that("build_lib() uses legacy source-stage hashes for sample_name", {
  wavenumber <- seq(50, 5000, by = 5)
  spectra <- cbind(
    sin(wavenumber / 200) + 2,
    cos(wavenumber / 250) + 2
  )
  colnames(spectra) <- c("raw_a", "raw_b")
  lib <- as_OpenSpecy(
    wavenumber,
    spectra = spectra,
    metadata = data.table::data.table(sample_name = colnames(spectra)),
    attributes = list(intensity_unit = "absorbance")
  )

  legacy_hash <- function(x, range = NULL, short_value = NULL) {
    x <- manage_na(x, type = "remove")
    spec <- conform_spec(x, range = range, res = 8)
    if (!is.null(short_value) && nrow(spec$spectra) < 3) {
      return(rep(short_value, ncol(x$spectra)))
    }
    spec <- smooth_intens(spec)
    vapply(seq_len(ncol(spec$spectra)), function(i) {
      digest::digest(
        list(as.integer(spec$wavenumber),
             as.integer(spec$spectra[, i] * 100)),
        algo = "md5"
      )
    }, FUN.VALUE = character(1))
  }

  expected <- legacy_hash(lib)
  expected_old <- legacy_hash(lib, range = c(100, 4000),
                              short_value = "new format")
  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = TRUE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw

  expect_equal(built$metadata$sample_name, expected)
  expect_equal(built$metadata$sample_name_old, expected_old)
  expect_equal(colnames(built$spectra), expected)

  excluded <- build_lib(
    lib,
    recipes = list(raw = list()),
    exclude_ids = expected_old[1],
    dedupe = TRUE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_equal(excluded$metadata$sample_name, expected[2])
})

test_that("reduce_lib() returns medoid ids or reduced OpenSpecy objects", {
  lib <- tiny_build_lib()

  ids <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
                    return = "ids")
  expect_equal(length(ids), 4)

  reduced <- reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2)
  expect_true(check_OpenSpecy(reduced))
  expect_equal(ncol(reduced$spectra), 4)
})

test_that("reduce_lib() uses cluster PAM medoids and reports useful progress", {
  lib <- tiny_build_lib()
  ids <- .lib_ids(lib, "sample_name")
  reduction_obj <- lib
  reduction_obj$spectra <- make_rel(
    mean_replace(lib$spectra, na.rm = TRUE), na.rm = TRUE
  )
  reduction_obj$spectra[!is.finite(reduction_obj$spectra)] <- 0
  groups <- do.call(paste, c(lib$metadata[, "material_class", with = FALSE],
                             sep = "_"))

  expected <- unlist(lapply(split(seq_along(groups), groups), function(idx) {
    if (length(idx) <= 2L) return(ids[idx])
    x <- filter_spec(reduction_obj, idx)
    cors <- cor_spec(x, x, compute = "optimized")
    cors[is.na(cors)] <- 0
    cors <- pmax(pmin(cors, 1), -1)
    diag(cors) <- 1
    distance <- stats::as.dist(1 - cors)
    set.seed(OpenSpecy:::.lib_pam_seed(ids[idx]))
    ids[idx][cluster::pam(
      distance, k = 2, diss = TRUE, variant = "faster"
    )$id.med]
  }), use.names = FALSE)

  messages <- capture.output(
    actual <- reduce_lib(
      lib, group_cols = "material_class", k = 2, min_n = 2,
      return = "ids", progress = TRUE
    ),
    type = "message"
  )
  expect_equal(actual, expected)
  messages <- paste(messages, collapse = "\n")
  expect_match(messages, "spectra in .* groups")
  expect_match(messages, "PAM group 1/2 starting")
  expect_match(messages, "correlation complete")
  expect_match(messages, "PAM complete")
  expect_match(messages, "variant=faster")
  expect_match(messages, "kept=4/8")

  expect_silent(
    reduce_lib(lib, group_cols = "material_class", k = 2, min_n = 2,
               return = "ids", progress = FALSE)
  )
  expect_error(reduce_lib(lib, progress = NA), "'progress'")
})

test_that("dense correlation views preserve medoid IDs for stable-ID subsets", {
  old_options <- options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )
  on.exit(options(old_options), add = TRUE)

  lib <- tiny_build_lib()
  ids <- as.character(lib$metadata$sample_name)
  normalized <- OpenSpecy:::.lib_prune_normalize(
    lib$spectra, lib$wavenumber, exclude = NULL
  )
  view <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids = ids, pools = rep("ftir", length(ids)),
    wavenumber = lib$wavenumber, state = "medoid-test"
  )

  subset <- filter_spec(lib, c(8L, 6L, 5L, 4L, 2L, 1L))
  legacy_ids <- reduce_lib(
    subset, group_cols = "material_class", k = 2, min_n = 2,
    return = "ids", progress = FALSE
  )
  dense_ids <- OpenSpecy:::.lib_reduce_lib(
    subset, group_cols = "material_class", k = 2, min_n = 2,
    return = "ids", progress = FALSE, correlation_view = view
  )

  expect_identical(dense_ids, legacy_ids)
  stats <- OpenSpecy:::.lib_correlation_view_stats(view)
  expect_equal(stats$dense_builds, 1L)
  expect_equal(stats$dense_queries, 2L)
  expect_equal(stats$bounded_queries, 0L)
})

test_that("oversized PAM groups use deterministic sampled correlation medoids", {
  set.seed(42)
  wn <- seq(800, 3200, length.out = 60)
  spectra <- matrix(runif(length(wn) * 120L), nrow = length(wn))
  colnames(spectra) <- paste0("large_", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(sample_name = colnames(spectra))
  )

  messages <- capture.output(
    first <- OpenSpecy:::.pam_large_group_ids(
      lib, id_col = "sample_name", k = 5L, progress = TRUE,
      group_label = "test", samples = 3L, sample_size = 60L
    ),
    type = "message"
  )
  second <- OpenSpecy:::.pam_large_group_ids(
    lib, id_col = "sample_name", k = 5L, progress = FALSE,
    group_label = "test", samples = 3L, sample_size = 60L
  )

  expect_identical(first, second)
  expect_length(first, 5L)
  expect_true(all(first %in% colnames(spectra)))
  expect_match(paste(messages, collapse = "\n"), "sampled PAM complete")
})

test_that("build_model_lib() returns the model library artifact structure", {
  skip_if_not_installed("glmnet")
  lib <- tiny_build_lib()

  model <- suppressWarnings(
    build_model_lib(lib, type_col = NULL, min_n = 2, nlambda = 3)
  )
  expect_named(model, c("model", "model_type", "lambda_selected",
                        "lambda_path_complete", "selected_lambda_converged",
                        "convergence_error", "training_groups", "selection_metric",
                        "selection_rule", "selection_accuracy_tolerance",
                        "lambda_metrics", "class_metrics", "fold_assignments",
                        "dimension_conversion", "tests",
                        "coefficients", "class_names", "class_num",
                        "observation_count", "fill", "support",
                        "class_support", "fill_method", "fill_replaced",
                        "variable_num",
                        "all_variables", "variables_in"))
  expect_true(is_OpenSpecy(model$fill))
  expect_identical(model$model_type, "logistic_regression")
  expect_identical(
    model$selection_metric, "guardrailed_overall_macro_log_loss"
  )
  expect_identical(model$selection_rule, "guardrailed")
  expect_equal(model$selection_accuracy_tolerance, 0.01)
  expect_identical(model$fill_method, "wavenumber_mean")
  expect_true(all(c("lambda", "macro_class_accuracy", "overall_accuracy",
                    "log_loss", "brier_score", "overall_eligible",
                    "macro_eligible", "selected", "selection_scope",
                    "selection_rule", "accuracy_tolerance") %in%
                    names(model$lambda_metrics)))
  expect_true(all(c("factor_num", "name") %in%
                    names(model$dimension_conversion)))
  expect_true(all(c("spectrum_id", "technique", "expected_class",
                    "predicted_class", "correct", "score", "split",
                    "provenance") %in% names(model$tests)))
  expect_null(model$model$call)

  before <- suppressWarnings(match_spec(lib, library = model))
  full_bytes <- length(serialize(model$model, NULL))
  slim <- OpenSpecy:::.lib_slim_model(model)
  after <- suppressWarnings(match_spec(lib, library = slim))
  expect_length(slim$model$lambda, 1L)
  expect_lt(length(serialize(slim$model, NULL)), full_bytes)
  expect_equal(after, before, tolerance = 1e-12)
})

test_that("release sanitizers retain runtime state and remove assessments", {
  lib <- tiny_build_lib()
  attr(lib, "derivative_order") <- "1"
  attr(lib, "transformations") <- list(list(method = "derivative"))
  attr(lib, "quality_control_report") <- data.table::data.table(value = 1)
  attr(lib, "prune_report") <- list(summary = data.table::data.table(value = 1))
  attr(lib$metadata, "join_report") <- data.table::data.table(value = 1)

  slim <- OpenSpecy:::.lib_slim_reference_object(lib)
  expect_true(check_OpenSpecy(slim))
  expect_identical(attr(slim, "derivative_order"), "1")
  expect_equal(attr(slim, "transformations"), list(list(method = "derivative")))
  expect_null(attr(slim, "quality_control_report", exact = TRUE))
  expect_null(attr(slim, "prune_report", exact = TRUE))
  expect_null(attr(slim$metadata, "join_report", exact = TRUE))
  expect_identical(colnames(slim$spectra), slim$metadata$sample_name)

  model <- list(
    model = structure(list(), class = "mock_model"),
    model_type = "logistic_regression", lambda_selected = 0.1,
    dimension_conversion = data.table::data.table(factor_num = 1L, name = "a"),
    coefficients = data.table::data.table(), class_names = "a", class_num = 1L,
    observation_count = 8L, fill = lib, fill_method = "wavenumber_mean",
    variable_num = nrow(lib$spectra), all_variables = lib$wavenumber,
    variables_in = lib$wavenumber,
    tests = data.table::data.table(correct = TRUE),
    lambda_metrics = data.table::data.table(lambda = 0.1),
    support = data.table::data.table(spectrum_id = "s1")
  )
  attr(model, "training_warnings") <- "diagnostic warning"
  slim_model <- OpenSpecy:::.lib_slim_model(model)
  expect_true(all(names(slim_model) %in%
                    OpenSpecy:::.lib_runtime_model_fields()))
  expect_false(any(c("tests", "lambda_metrics", "class_metrics",
                     "fold_assignments", "support") %in%
                     names(slim_model)))
  expect_null(attr(slim_model, "training_warnings", exact = TRUE))
  expect_true(check_OpenSpecy(slim_model$fill))
  expect_null(attr(slim_model$fill, "quality_control_report", exact = TRUE))
  expect_true(OpenSpecy:::.lib_selected_lambda_converged(list(
    model = structure(list(jerr = 0L), class = "glmnet")
  )))
  expect_false(OpenSpecy:::.lib_selected_lambda_converged(list(
    model = structure(list(jerr = -3L), class = "glmnet")
  )))
  expect_false(OpenSpecy:::.lib_selected_lambda_converged(list(
    model = structure(list(jerr = 0L), class = "glmnet"),
    selected_lambda_converged = FALSE
  )))
})

test_that("build source provenance is independent of the working directory", {
  root_paths <- OpenSpecy:::.lib_build_source_paths()
  expect_true(all(c("DESCRIPTION", "NAMESPACE") %in% basename(root_paths)))
  expect_true(any(grepl("[/\\\\]R[/\\\\]", root_paths)))

  old_dir <- setwd(tempdir())
  other_paths <- tryCatch(
    OpenSpecy:::.lib_build_source_paths(),
    finally = setwd(old_dir)
  )
  expect_identical(other_paths, root_paths)
  signatures <- OpenSpecy:::.lib_build_source_signatures()
  expect_identical(nrow(signatures), length(other_paths))
  expect_false(anyNA(signatures$checksum))
  expect_true(all(nzchar(signatures$checksum)))
})

test_that("aggregate gated checkpoints require complete converged models", {
  lib <- tiny_build_lib()
  model <- list(
    model = structure(list(jerr = 0L), class = "mock_model"),
    model_type = "gated_hierarchical_logistic_regression",
    class_names = c("class_a", "class_b"), class_num = 2L,
    fill = lib, all_variables = lib$wavenumber,
    model_schema = "OpenSpecy_gated_hierarchy_v1",
    hierarchy_cols = c("material_type", "material_group"),
    root_node_id = "root", gated_nodes = list(
      root = list(
        model = NULL, deterministic = TRUE, lambda_selected = NA_real_,
        dimension_conversion = data.table::data.table(
          factor_num = 1L, name = "plastic"
        ),
        class_names = "plastic", lambda_path_complete = TRUE,
        selected_lambda_converged = TRUE
      )
    ),
    gated_topology = data.table::data.table(
      node_id = "root", parent_id = NA_character_,
      incoming_label = NA_character_, depth = 0L,
      target_col = "material_type", terminal = FALSE
    ),
    gated_edges = data.table::data.table(
      node_id = character(), label = character(), child_node_id = character()
    ),
    training_elapsed_seconds = 1,
    selected_lambda_converged = TRUE
  )
  medoids <- list(derivative = list(ftir = lib))
  models <- list(gated_hierarchical_logistic_regression = list(
    derivative = list(ftir = model)
  ))

  expect_length(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(models, medoids), 0L
  )
  release_signature <- OpenSpecy:::.lib_release_signature("assessment", models)
  expect_identical(
    OpenSpecy:::.lib_release_signature("assessment", models),
    release_signature
  )
  no_runtime <- models
  no_runtime$gated_hierarchical_logistic_regression$derivative$ftir$
    training_elapsed_seconds <- NULL
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(no_runtime, medoids),
    "incomplete|invalid training runtime"
  )
  changed_runtime <- models
  changed_runtime$gated_hierarchical_logistic_regression$derivative$ftir$
    training_elapsed_seconds <- 2
  expect_false(identical(
    OpenSpecy:::.lib_release_signature("assessment", changed_runtime),
    release_signature
  ))
  unconverged <- models
  unconverged$gated_hierarchical_logistic_regression$derivative$ftir[[
    "selected_lambda_converged"
  ]] <- FALSE
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(unconverged, medoids),
    "selected lambda unconverged"
  )
  missing <- models
  missing$gated_hierarchical_logistic_regression$derivative$ftir <- NULL
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(missing, medoids),
    "derivative/ftir missing"
  )
  malformed <- models
  malformed$gated_hierarchical_logistic_regression$derivative$ftir$
    gated_nodes <- list(root = 1)
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(malformed, medoids),
    "incomplete gated nodes"
  )

  flat_model <- list(
    model = structure(list(jerr = 0L), class = "mock_model"),
    model_type = "logistic_regression",
    class_names = c("class_a", "class_b"), class_num = 2L,
    fill = lib, all_variables = lib$wavenumber,
    training_elapsed_seconds = 1,
    selected_lambda_converged = TRUE
  )
  flat_models <- list(logistic_regression = list(
    derivative = list(ftir = flat_model)
  ))
  expect_length(
    OpenSpecy:::.lib_flat_model_checkpoint_problems(flat_models, medoids), 0L
  )
  expect_length(
    OpenSpecy:::.lib_official_model_checkpoint_problems(
      flat_models, medoids, "logistic_regression"
    ),
    0L
  )
  expect_identical(
    OpenSpecy:::.lib_official_model_checkpoint_key("logistic_regression"),
    "models_flat_logistic_v1"
  )
  expect_match(
    OpenSpecy:::.lib_official_model_checkpoint_problems(
      models, medoids, "logistic_regression"
    ),
    "does not match requested method"
  )
})

test_that("build_lib validates the official model method", {
  expect_error(
    build_lib(tiny_build_lib(), model_method = "random_forest"),
    "'arg' should be one of"
  )
})

test_that("aggregate model warnings are reconstructed from checkpoints", {
  first <- list(value = 1)
  second <- list(value = 2)
  attr(first, "training_warnings") <- c("first warning", "second warning")
  attr(second, "training_warnings") <- "third warning"
  models <- list(
    gated_hierarchical_logistic_regression = list(
      derivative = list(ftir = first, raman = second)
    )
  )

  warnings <- OpenSpecy:::.lib_collect_model_warnings(models)
  expect_equal(nrow(warnings), 3L)
  expect_setequal(
    warnings$warning,
    c("first warning", "second warning", "third warning")
  )
  expect_identical(
    OpenSpecy:::.lib_collect_model_warnings(list()),
    OpenSpecy:::.lib_warning_schema()
  )
})

test_that("official builders can opt into host workers without changing defaults", {
  previous <- options("OpenSpecy.build_workers")
  on.exit(options(previous), add = TRUE)

  options(OpenSpecy.build_workers = NULL)
  expect_identical(OpenSpecy:::.lib_build_workers(), 1L)
  options(OpenSpecy.build_workers = 8L)
  expect_identical(OpenSpecy:::.lib_build_workers(), 8L)
  options(OpenSpecy.build_workers = -1L)
  expect_identical(OpenSpecy:::.lib_build_workers(), 1L)
})

test_that("official model workers are scoped and restored after errors", {
  skip_on_cran()
  skip_if_not_installed("doFuture")
  skip_if_not_installed("doRNG")
  skip_if_not_installed("future")
  skip_if_not_installed("foreach")
  previous_options <- options("OpenSpecy.build_workers")
  previous_plan <- future::plan()
  on.exit({
    future::plan(previous_plan)
    foreach::registerDoSEQ()
    options(previous_options)
  }, add = TRUE)
  options(OpenSpecy.build_workers = 2L)
  foreach::registerDoSEQ()
  observed <- new.env(parent = emptyenv())

  expect_error(
    OpenSpecy:::.lib_with_model_workers({
      observed$backend <- foreach::getDoParName()
      observed$workers <- future::nbrOfWorkers()
      stop("model fit stopped", call. = FALSE)
    }),
    "model fit stopped"
  )

  expect_identical(observed$backend, "doFuture")
  expect_identical(observed$workers, 2L)
  expect_identical(foreach::getDoParName(), "doSEQ")
  expect_identical(future::nbrOfWorkers(), 1L)
})

test_that("build_model_lib() trains and deploys a balanced probability random forest", {
  skip_if_not_installed("ranger")
  lib <- tiny_build_lib()
  lib$spectra[1:3, 1] <- NA_real_
  lib$metadata$material_class <- c(rep("class_a", 6), rep("class_b", 2))

  model <- build_model_lib(
    lib, type_col = NULL, min_n = 2, method = "random_forest",
    num.trees = 30L, min.node.size = 1L, mtry = 4L
  )
  expect_identical(model$model_type, "random_forest")
  expect_s3_class(model$model, "ranger")
  expect_true(model$fill_replaced >= 3L)
  expect_true(all(c("accuracy", "macro_class_accuracy", "mean_score",
                    "brier_score") %in% names(model$oob_metrics)))
  expect_true(all(c("wavenumber", "importance") %in%
                    names(model$feature_importance)))
  expect_equal(model$class_weights$weight, c(2 / 3, 2))
  expect_true(all(model$class_weights$application == "case_sampling"))
  expect_identical(
    model$training_parameters$balance_method,
    "inverse_frequency_case_sampling"
  )
  expect_identical(model$training_parameters$num_threads, 1L)
  expect_equal(nrow(model$tests), ncol(lib$spectra))

  prediction <- suppressWarnings(match_spec(lib, library = model))
  expect_equal(nrow(prediction), ncol(lib$spectra))
  expect_true(all(prediction$name %in% c("class_a", "class_b")))
})

test_that("official model building records fit warnings and progress", {
  lib <- tiny_build_lib()
  messages <- character()
  local_mocked_bindings(
    train_spec_model = function(...) {
      warning("mock convergence warning", call. = FALSE)
      list(
        observation_count = 4L, class_num = 2L,
        lambda_selected = 0.1
      )
    },
    .package = "OpenSpecy"
  )

  result <- OpenSpecy:::.lib_build_models(
    libraries = list(),
    medoids = list(derivative = list(nir = lib)),
    report = function(message) messages <<- c(messages, message)
  )
  expect_identical(
    result$warnings,
    data.table::data.table(
      algorithm = "logistic_regression", artifact = "derivative", model = "nir",
      warning = "mock convergence warning"
    )
  )
  expect_identical(
    attr(result$models$logistic_regression$derivative$nir,
         "training_warnings"),
    "mock convergence warning"
  )
  expect_true(any(grepl("model warning", messages, fixed = TRUE)))
  expect_true(any(grepl("model complete", messages, fixed = TRUE)))
})

test_that("official random forests train on full libraries rather than medoids", {
  skip_if_not_installed("ranger")
  base <- tiny_build_lib()
  full <- base
  full$spectra <- do.call(cbind, lapply(1:3, function(copy) {
    values <- base$spectra + copy / 1000
    colnames(values) <- paste0(colnames(values), "_", copy)
    values
  }))
  full$metadata <- data.table::rbindlist(lapply(1:3, function(copy) {
    metadata <- data.table::copy(base$metadata)
    metadata$sample_name <- paste0(metadata$sample_name, "_", copy)
    metadata
  }))
  full <- as_OpenSpecy(full)

  result <- OpenSpecy:::.lib_build_models(
    libraries = list(raw = list(ftir = full)), medoids = list(),
    report = function(...) NULL,
    methods = "random_forest",
    random_forest_args = list(num.trees = 20L, min.node.size = 1L, mtry = 4L)
  )
  model <- result$models$random_forest$raw$ftir
  expect_identical(model$model_type, "random_forest")
  expect_equal(model$observation_count, ncol(full$spectra))
  expect_equal(model$training_parameters$num_trees, 20L)
})

test_that("typed random forests do not repeat FTIR and Raman in a combined fit", {
  lib <- tiny_build_lib()
  local_mocked_bindings(
    train_spec_model = function(x, ...) {
      list(
        model_type = "random_forest",
        observation_count = ncol(x$spectra), class_num = 2L,
        fill_replaced = 0L,
        oob_metrics = data.table::data.table(macro_class_accuracy = 0.5),
        training_parameters = data.table::data.table(
          num_trees = 20L, mtry = 2L
        )
      )
    },
    .package = "OpenSpecy"
  )

  result <- OpenSpecy:::.lib_build_models(
    libraries = list(raw = list(ftir = lib, raman = lib)),
    medoids = list(), report = function(...) NULL,
    methods = "random_forest"
  )

  expect_named(result$models$random_forest$raw, c("ftir", "raman"))
})

test_that("combined logistic models use the exact FTIR and Raman axis intersection", {
  ftir <- tiny_build_lib()
  raman <- tiny_build_lib()
  raman$metadata$spectrum_type <- "raman"
  local_mocked_bindings(
    train_spec_model = function(x, ...) {
      list(
        observation_count = ncol(x$spectra), class_num = 2L,
        fill_replaced = 0L, lambda_selected = 0.1,
        selection_metric = "mock",
        training_wavenumber = x$wavenumber,
        identification_ranges = attr(x, "identification_ranges", exact = TRUE)
      )
    },
    .package = "OpenSpecy"
  )

  result <- OpenSpecy:::.lib_build_models(
    libraries = list(),
    medoids = list(derivative = list(ftir = ftir, raman = raman)),
    report = function(...) NULL,
    methods = "logistic_regression"
  )$models$logistic_regression$derivative

  expect_named(result, c("ftir", "raman", "both"))
  expect_identical(result$both$training_wavenumber,
                   result$ftir$training_wavenumber)
  expect_gt(length(result$raman$training_wavenumber),
            length(result$both$training_wavenumber))
  expect_true(any(
    result$raman$training_wavenumber > 2200 &
      result$raman$training_wavenumber < 2420
  ))
  expect_false(any(
    result$both$training_wavenumber > 2200 &
      result$both$training_wavenumber < 2420
  ))
  expect_equal(
    result$both$identification_ranges,
    data.table::data.table(
      minimum = c(800, 2420), maximum = c(2200, 3200)
    )
  )
  expect_identical(result$both$observation_count, 16L)
})

test_that("build_lib() applies named recipes to merged sources", {
  lib <- tiny_build_lib()
  built <- build_lib(
    list(lib),
    recipes = list(raw = list(),
                   relative = function(x) make_rel(x, na.rm = TRUE)),
    dedupe = FALSE,
    signal_noise = FALSE
  )

  expect_named(built, c("raw", "relative"))
  expect_true(check_OpenSpecy(built$raw))
  expect_true(check_OpenSpecy(built$relative))
})

test_that("build_lib() requires explicit source inputs", {
  expect_error(
    build_lib(),
    "'x' must specify the source library file path"
  )
})

test_that("build_lib() converts metadata intensity units before recipes", {
  lib <- tiny_build_lib()
  lib$spectra <- matrix(
    rep(c(50, 25, 0.5, 0.25, 2), each = nrow(lib$spectra)),
    nrow = nrow(lib$spectra),
    dimnames = list(NULL, paste0("u", 1:5))
  )
  lib$spectra[1, 2] <- NA_real_
  lib$metadata <- data.table::data.table(
    sample_name = colnames(lib$spectra),
    intensity_units = c(
      "Reflectance (%)", "transmittance", "absorbance", "mystery", NA
    )
  )
  attr(lib, "intensity_unit") <- NULL
  original <- lib$spectra

  expect_warning(
    built <- build_lib(
      list(lib),
      recipes = list(raw = list()),
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "skipped 2 spectrum/s.*<missing> \\(1\\).*mystery \\(1\\)|skipped 2 spectrum/s.*mystery \\(1\\).*<missing> \\(1\\)"
  )

  expect_equal(
    built$spectra[, 1],
    adj_intens(original[, 1], type = "reflectance", make_rel = FALSE)
  )
  expect_equal(
    built$spectra[, 2],
    adj_intens(
      original[, 2], type = "transmittance", make_rel = FALSE,
      na.rm = TRUE
    )
  )
  expect_equal(built$spectra[, 3:5], original[, 3:5])
  expect_equal(
    built$metadata$intensity_units,
    c("absorbance", "absorbance", "absorbance", "mystery", NA)
  )
  expect_null(attr(built, "intensity_unit"))
  expect_true(check_OpenSpecy(built))
})

test_that("build_lib() treats intensity_unit attribute as primary truth", {
  lib <- tiny_build_lib()
  lib$spectra[,] <- 50
  lib$metadata$intensity_units <- "transmittance"
  attr(lib, "intensity_unit") <- "reflectance"

  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expect_equal(
    built$spectra,
    adj_intens(lib$spectra, type = "reflectance", make_rel = FALSE)
  )
  expect_equal(built$metadata$intensity_units, rep("absorbance", 8))
  expect_equal(attr(built, "intensity_unit"), "absorbance")

  attr(lib, "intensity_unit") <- "absorbance"
  unchanged <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw
  expect_equal(unchanged$spectra, lib$spectra)
  expect_equal(unchanged$metadata$intensity_units, rep("absorbance", 8))
})

test_that("build_lib() can preserve declared intensity units", {
  lib <- tiny_build_lib()
  lib$spectra[,] <- 50
  lib$metadata$intensity_units <- "reflectance"
  attr(lib, "intensity_unit") <- "reflectance"

  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )$raw

  expect_equal(built$spectra, lib$spectra)
  expect_equal(built$metadata$intensity_units, rep("reflectance", 8))
  expect_equal(attr(built, "intensity_unit"), "reflectance")

  lib$metadata$intensity_units <- "transmittance"
  preserved <- build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_equal(preserved$spectra, lib$spectra)
  expect_equal(preserved$metadata$intensity_units, rep("transmittance", 8))
  expect_equal(attr(preserved, "intensity_unit"), "reflectance")
})

test_that("build_lib() accepts and restricts one OpenSpecy object", {
  lib <- tiny_build_lib()
  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      restrict_range_args = list(
        min = c(100, 2500),
        max = c(2000, 4000)
      ),
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "using one in-memory OpenSpecy source"
  )

  keep <- lib$wavenumber <= 2000 |
    (lib$wavenumber >= 2500 & lib$wavenumber <= 4000)
  expect_equal(built$wavenumber, lib$wavenumber[keep])
  expect_equal(built$spectra, lib$spectra[keep, , drop = FALSE])
  expect_silent(build_lib(
    lib,
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  ))
  expect_error(
    build_lib(list(lib), restrict_range_args = list(c(100, 2000))),
    "named list"
  )
})

test_that("build_lib() reads one or many OpenSpecy objects from each RDS", {
  left <- filter_spec(tiny_build_lib(), 1:2)
  right <- filter_spec(tiny_build_lib(), 3:4)
  single_file <- tempfile(fileext = ".rds")
  list_file <- tempfile(fileext = ".RDS")
  invalid_file <- tempfile(fileext = ".rds")
  saveRDS(left, single_file)
  saveRDS(list(left, right), list_file)
  saveRDS(list(left, "not OpenSpecy"), invalid_file)

  single <- build_lib(
    single_file,
    recipes = list(raw = list()),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw
  combined <- build_lib(
    list_file,
    recipes = list(raw = list()),
    range = NULL,
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw

  expect_equal(single$spectra, left$spectra)
  expect_equal(ncol(combined$spectra), 4)
  expect_equal(combined$metadata$sample_name, paste0("s", 1:4))
  expect_error(
    build_lib(invalid_file, progress = FALSE),
    "File path 1 must contain one OpenSpecy object or a nonempty list"
  )
})

test_that("build_lib() bulk-prepares legacy same-axis source lists", {
  lib <- tiny_build_lib()
  sources <- split_spec(list(lib))
  sources <- lapply(sources, function(x) {
    x$spectra <- data.table::as.data.table(x$spectra)
    x
  })

  expect_silent(
    built <- build_lib(
      sources,
      recipes = list(raw = list()),
      range = NULL,
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    )$raw
  )

  expect_equal(built$wavenumber, lib$wavenumber)
  expect_equal(built$spectra, lib$spectra)
  expect_equal(built$metadata$sample_name, lib$metadata$sample_name)
})

test_that("build_lib() converts each source before merging", {
  left <- filter_spec(tiny_build_lib(), 1:2)
  right <- filter_spec(tiny_build_lib(), 3:4)
  left$spectra[,] <- 50
  right$spectra[,] <- 0.5
  attr(left, "intensity_unit") <- "reflectance"
  attr(right, "intensity_unit") <- NULL
  right$metadata$intensity_units <- "transmittance"

  built <- build_lib(
    list(left, right),
    recipes = list(raw = list()),
    range = NULL,
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expected <- cbind(
    adj_intens(left$spectra, type = "reflectance", make_rel = FALSE),
    adj_intens(right$spectra, type = "transmittance", make_rel = FALSE)
  )
  colnames(expected) <- colnames(built$spectra)
  expect_equal(built$spectra, expected)
  expect_equal(attr(built, "intensity_unit"), "absorbance")
  expect_equal(built$metadata$intensity_units, rep("absorbance", 4))
})

test_that("metadata name helpers support smart and extensible matching", {
  expect_equal(
    lib_clean_name(c(" User Name ", "Laser (%)", "Method...3")),
    c("user_name", "laser_perc", "method_3")
  )

  name_lookup <- lib_metadata_name_lookup(
    campaign_code = "campaign id",
    project_code = character(),
    review_note = character(),
    regex = list(instrument_mode = "^method_[0-9]+$")
  )
  expect_false(any(c("username", "samplename", "librarytype") %in%
                     name_lookup$source_name, na.rm = TRUE))

  metadata <- data.table::data.table(
    UserName = c("alias_a", NA),
    user_name = c(NA, "canonical_b"),
    ProjectCodes = c("p1", "p2"),
    `Review Notes` = c("check", "keep"),
    Campaign.ID = c("campaign_a", "campaign_b"),
    Method.42 = c("ftir", "raman")
  )
  cleaned <- lib_clean_metadata(metadata, name_lookup)

  expect_equal(cleaned$user_name, c("alias_a", "canonical_b"))
  expect_equal(cleaned$project_code, c("p1", "p2"))
  expect_equal(cleaned$campaign_code, c("campaign_a", "campaign_b"))
  expect_equal(cleaned$review_note, c("check", "keep"))
  expect_equal(cleaned$instrument_mode, c("ftir", "raman"))
  expect_false("campaign_id" %in% names(cleaned))

  cleaned_values <- lib_clean_metadata(
    data.table::data.table(
      Organization = c(" Monterey Bay Aquarium Research Institute ", "NULL"),
      SpectrumType = c("Raman", " not available "),
      numeric_value = c(1, 2)
    ),
    clean_values = TRUE
  )
  expect_equal(
    cleaned_values$organization,
    c("monterey bay aquarium research institute", NA)
  )
  expect_equal(cleaned_values$spectrum_type, c("raman", NA))
  expect_equal(cleaned_values$numeric_value, c(1, 2))

  strict_lookup <- lib_metadata_name_lookup(
    project_code = character(),
    defaults = FALSE,
    match_without_underscores = FALSE,
    match_singular_plural = FALSE
  )
  strict <- lib_clean_metadata(
    data.table::data.table(ProjectCodes = "p1"),
    strict_lookup
  )
  expect_named(strict, "projectcodes")
})

test_that("metadata harmonization coalesces reviewed aliases only", {
  metadata <- data.table::data.table(
    spectrum_identity = c("canonical", NA),
    interpretation = c("ignored", "interpreted"),
    form_factor = c("film", NA),
    shape = c(NA, "fiber"),
    datatype = c("absorbance", "raman shift"),
    xunits = c("cm-1", NA),
    x_unit = c(NA, "1/cm"),
    spectrumid = c("a", "b"),
    locationdescription = c("left", "right"),
    name = c("source name", "source name 2"),
    names = c("other meaning", "other meaning 2"),
    file = c("raw path", "raw path 2"),
    file_name = c("display", "display 2"),
    sample = c("source sample", "source sample 2"),
    sample_name = c("stable-a", "stable-b")
  )

  cleaned <- lib_clean_metadata(metadata)

  expect_equal(cleaned$spectrum_identity, c("canonical", "interpreted"))
  expect_equal(cleaned$material_form, c("film", "fiber"))
  expect_equal(cleaned$data_type, c("absorbance", "raman shift"))
  expect_equal(cleaned$wavenumber_units, c("cm-1", "1/cm"))
  expect_equal(cleaned$spectrum_id, c("a", "b"))
  expect_equal(cleaned$location_description, c("left", "right"))
  expect_true(all(c("name", "names", "file", "file_name", "sample",
                    "sample_name") %in% names(cleaned)))
})

test_that("metadata regex lookup reports overlapping patterns", {
  name_lookup <- lib_metadata_name_lookup(
    defaults = FALSE,
    regex = list(
      campaign = "^campaign",
      identifier = "_id$"
    )
  )

  expect_error(
    lib_clean_metadata(
      data.table::data.table(Campaign.ID = "campaign_a"),
      name_lookup
    ),
    "Multiple metadata name regular expressions.*Campaign.ID"
  )
})

test_that("build_lib() cleans and coalesces metadata column names", {
  lib <- tiny_build_lib()
  lib$metadata[["UserName"]] <- c("alias_a", "alias_b", rep(NA, 6))
  lib$metadata[["user name"]] <- c(NA, "canonical_b", rep(NA, 6))
  lib$metadata[["NumberofAccumulations"]] <- c(10L, rep(NA_integer_, 7))
  lib$metadata[["Number of sample scans"]] <- c(20L, 30L,
                                                rep(NA_integer_, 6))
  lib$metadata[["CAS REGISTRY NO"]] <- rep("25038-54-4", 8)
  lib$metadata[["Laser (%)"]] <- rep(75, 8)

  name_lookup <- lib_metadata_name_lookup(project_code = "Campaign ID")
  lib$metadata[["Campaign.ID"]] <- rep("campaign_a", 8)

  built <- build_lib(
    list(lib),
    recipes = list(raw = list()),
    metadata_name_lookup = name_lookup,
    dedupe = FALSE,
    signal_noise = FALSE
  )$raw

  expect_true(all(grepl("^[a-z0-9]+(?:_[a-z0-9]+)*$",
                        names(built$metadata))))
  expect_equal(built$metadata$user_name[1:2],
               c("alias_a", "canonical_b"))
  expect_equal(built$metadata$number_of_accumulations[1:2], c(10L, 30L))
  expect_equal(built$metadata$cas_number, rep("25038-54-4", 8))
  expect_equal(built$metadata$laser_perc, rep(75, 8))
  expect_equal(built$metadata$project_code, rep("campaign_a", 8))
  expect_false(any(c("username", "numberofaccumulations",
                     "number_of_sample_scans") %in% names(built$metadata)))
})

test_that("source metadata invalid bytes are normalized before merging", {
  invalid <- rawToChar(as.raw(c(0x66, 0x80, 0x6f)))
  Encoding(invalid) <- "unknown"
  expect_false(validUTF8(invalid))
  normalized <- OpenSpecy:::.lib_normalize_metadata_encoding(
    data.table::data.table(value = invalid)
  )
  expect_true(validUTF8(normalized$value))
  expect_equal(normalized$value, "fo")
})

test_that("build_lib() runs default joins, processing, SNR, and assessment", {
  lib <- tiny_build_lib()
  lib$spectra[1, 1] <- -1
  source_lookup <- data.table::data.table(
    Source = c("A", "B", "C"),
    Material = c("mat_a", "mat_b", "mat_c")
  )
  hierarchy <- data.table::data.table(
    Material = c("mat_a", "mat_b", "mat_c"),
    `Material Class` = c("class_a", "class_b", "class_c"),
    `Material Type` = rep("material", 3)
  )

  built <- suppressWarnings(build_lib(
    list(lib),
    metadata_lookups = source_lookup,
    material_hierarchy = hierarchy,
    assess = TRUE,
    dedupe = FALSE
  ))

  expect_named(built, c("raw", "derivative", "nobaseline"))
  valid <- vapply(built, check_OpenSpecy, logical(1))
  expect_true(all(valid), info = paste("invalid recipes:",
                                       paste(names(valid)[!valid],
                                             collapse = ", ")))
  finite <- vapply(built, function(x) all(is.finite(x$spectra)), logical(1))
  expect_true(all(finite), info = paste("non-finite recipes:",
                                        paste(names(finite)[!finite],
                                              collapse = ", ")))
  expect_true(all(c("material", "material_class", "material_type", "sn",
                    "assessment_flag", "assessment_issue_count",
                    "assessment_checks", "assessment_issues",
                    "assessment_potential_fixes") %in%
                  names(built$raw$metadata)))
  expect_equal(attr(built$derivative, "derivative_order"), "1")
  expect_equal(attr(built$nobaseline, "baseline"), "nobaseline")
  expect_true(built$raw$metadata$assessment_flag[1])
})

test_that("build_lib() skips metadata lookups with no shared key", {
  lib <- tiny_build_lib()
  no_shared <- data.table::data.table(
    missing_key = "not_present",
    joined_value = "skipped"
  )
  no_overlap <- data.table::data.table(
    source = "Z",
    joined_value = "skipped"
  )
  output_overlap <- data.table::data.table(
    source = c("A", "B", "C"),
    library_type = c("polymers", "polymers", "paints"),
    spectrum_type = c("ftir", "raman", "ftir")
  )
  ambiguous <- data.table::data.table(
    source = "A",
    sample_name = "s1",
    joined_value = "ambiguous"
  )

  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = no_shared,
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "skipping metadata lookup 1/1"
  )
  expect_false("joined_value" %in% names(built$metadata))

  expect_message(
    built <- build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = no_overlap,
      dedupe = FALSE,
      signal_noise = FALSE
    )$raw,
    "no usable shared key values"
  )
  expect_false("joined_value" %in% names(built$metadata))

  lib$metadata$library_type <- c("polymers", "paints", "polymers", "paints",
                                "polymers", "paints", "polymers", "paints")
  lib$metadata$spectrum_type <- "Raman"
  bad_utf8 <- rawToChar(as.raw(0xff))
  Encoding(bad_utf8) <- "UTF-8"
  lib$metadata$library_type[1] <- bad_utf8
  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = output_overlap,
    dedupe = FALSE,
    signal_noise = FALSE,
    clean_metadata_values = TRUE,
    progress = FALSE
  )$raw
  expect_false(any(c("library_type.x", "library_type.y",
                     "spectrum_type.x", "spectrum_type.y") %in%
                     names(built$metadata)))
  expect_equal(built$metadata$library_type[1:3],
               c("polymers", "polymers", "paints"))
  expect_equal(built$metadata$spectrum_type[1:3],
               c("ftir", "raman", "ftir"))

  expect_error(
    build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = ambiguous,
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    ),
    "Candidate columns were"
  )
})

test_that("build_lib() standardizes source keys before external lookups", {
  lib <- filter_spec(tiny_build_lib(), 1:3)
  lib$metadata$organization <- c("source org", NA, NA)
  lib$metadata$user_name <- c("known", "fallback user", "unmapped")
  lib$metadata$library_type <- c("metadata type", NA, NA)
  source_lookup <- data.table::data.table(
    organization = c("source org", "fallback user"),
    library_type = c("organization type", "user type"),
    spectrum_type = "raman"
  )

  built <- suppressWarnings(build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = list(
      lookup = source_lookup,
      by = "organization",
      fill_only = TRUE
    ),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw)

  expect_equal(built$metadata$library_type,
               c("metadata type", "user type", NA))
  expect_equal(built$metadata$spectrum_type, rep("ftir", 3))
  expect_equal(built$metadata$organization,
               c("source org", "fallback user", "unmapped"))
  expect_equal(built$metadata$library_name,
               c("source org", "fallback user", "unmapped"))
  expect_equal(
    attr(built, "metadata_lookup_reports")$canonical_source_keys[
      problem == "filled_canonical_key", n
    ],
    2L
  )
})

test_that("official library types retain source values and audit provenance fallbacks", {
  metadata <- data.table::data.table(
    library_type = c("polymers", NA, ""),
    origin = c("experimental", "computed_solid", "computed_molecule")
  )

  completed <- .lib_complete_reference_library_type(metadata)

  expect_equal(
    completed$metadata$library_type,
    c(
      "polymers", "computed solid reference collection",
      "computed molecular reference collection"
    )
  )
  expect_equal(completed$assessment$source_reported, 1L)
  expect_equal(completed$assessment$provenance_fallback, 2L)
  expect_match(completed$assessment$fallback_values, "computed solid")
  expect_match(completed$assessment$fallback_values, "computed molecular")
})

test_that("build_lib() cleans filename-derived spectrum identities and keys", {
  lib <- tiny_build_lib()
  lib$metadata$spectrum_identity <- c(
    "C:\\incoming\\Sample.CSV", "/tmp/Other.SPC",
    "relative/folder/Third.HDF5", "compound pe/pa/pe",
    "name.spc.csv", "plain identity", "opus.10", "unsupported.foo"
  )
  lookup <- data.table::data.table(
    spectrum_identity = c(
      "sample.csv", "other.spc", "third.hdf5", "compound pe/pa/pe",
      "name", "plain identity", "opus.10", "unsupported.foo"
    ),
    material = paste0("material_", seq_len(8))
  )

  built <- build_lib(
    lib,
    recipes = list(raw = list()),
    metadata_lookups = list(lookup = lookup, by = "spectrum_identity"),
    dedupe = FALSE,
    signal_noise = FALSE,
    clean_metadata_values = TRUE,
    progress = FALSE
  )$raw

  expect_identical(
    built$metadata$spectrum_identity,
    c("sample", "other", "third", "compound pe/pa/pe", "name",
      "plain identity", "opus", "unsupported.foo")
  )
  expect_identical(built$metadata$material, lookup$material)
  report <- attr(built, "spectrum_identity_cleanup_report")
  expect_s3_class(report, "data.table")
  expect_equal(sum(report$n), 5L)
  expect_true(all(c("original", "spectrum_identity", "n") %in%
                    names(report)))
})

test_that("build_lib() rejects lookup collisions after identity cleanup", {
  lib <- tiny_build_lib()
  lib$metadata$spectrum_identity <- rep("same", 8)
  lookup <- data.table::data.table(
    spectrum_identity = c("same.csv", "same.spc"),
    material = c("first", "second")
  )

  expect_error(
    build_lib(
      lib,
      recipes = list(raw = list()),
      metadata_lookups = list(lookup = lookup, by = "spectrum_identity"),
      dedupe = FALSE,
      signal_noise = FALSE,
      progress = FALSE
    ),
    "Lookup keys must be unique"
  )
})

test_that("prune_lib() orders classes, preserves floors, and audits removals", {
  wn <- seq(500, 3500, length.out = 80)
  shape_a <- dnorm(seq(-3, 3, length.out = length(wn)))
  shape_b <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1)
  spectra <- cbind(
    shape_a, shape_a * 1.01, shape_a * 0.99, shape_a + 0.002,
    shape_b,
    shape_b * 1.01, shape_b * 0.99, shape_b + 0.002,
    rev(shape_a), rev(shape_a) * 1.01
  )
  colnames(spectra) <- paste0("id", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(rep("large", 5), rep("medium", 3), rep("small", 2)),
      material_type = "plastic",
      spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 2, return = "report", progress = FALSE)

  expect_true(check_OpenSpecy(report$object))
  expect_equal(report$schedule$material_class, c("large", "medium", "small"))
  expect_equal(report$schedule$initial_n, c(5L, 3L, 2L))
  expect_true(nrow(report$removals) >= 1)
  expect_equal(report$summary$reassigned, 0L)
  expect_equal(nrow(report$excluded_classes), 0L)
  expect_named(
    report$excluded_classes,
    names(OpenSpecy:::.lib_prune_excluded_class_schema())
  )
  expect_true(all(table(report$object$metadata$material_class) >= 2))
  expect_identical(colnames(report$object$spectra),
                   report$object$metadata$sample_name)
  expect_identical(report$retained_ids, colnames(report$object$spectra))

  query <- which(lib$metadata$material_class == "large")
  candidates <- seq_len(ncol(lib$spectra))
  normalized <- OpenSpecy:::.lib_prune_normalize(
    lib$spectra, lib$wavenumber, c(2200, 2420)
  )
  legacy <- OpenSpecy:::.lib_prune_correlations(
    lib, query, candidates, c(2200, 2420), lib$metadata$sample_name
  )
  actual <- OpenSpecy:::.lib_prune_best_match(
    query, candidates, normalized, lib$metadata$sample_name,
    exclude_self = TRUE, block_size = 2L
  )
  bounded_full <- OpenSpecy:::.lib_prune_best_match(
    query, candidates, normalized, lib$metadata$sample_name,
    exclude_self = TRUE, block_size = length(query)
  )
  expect_equal(actual, bounded_full)

  # Proportional synthetic spectra create several effectively exact ties.
  # The bounded implementation deliberately resolves those ties by stable ID;
  # BLAS operation shape can make the legacy full matrix select a different
  # tied column. Verify that every bounded choice has the same scientific score.
  legacy_best <- matrixStats::rowMaxs(legacy)
  actual_columns <- match(actual$index, candidates)
  legacy_at_actual <- legacy[cbind(seq_along(query), actual_columns)]
  tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(legacy_best))
  expect_true(all(abs(legacy_at_actual - legacy_best) <= tolerance))
})

test_that("cross-class pruning closes internal conflicts before independent evidence", {
  normalized <- rbind(
    high = c(1, 0, 0, 0),
    b_l2 = c(1, 0, 0, 0),
    b_l3 = c(1, 0, 0, 0),
    tie_c = c(0, 1, 0, 0),
    tie_d = c(0, 1, 0, 0),
    same_source_e = c(0, 0, 1, 0),
    same_source_f = c(0, 0, 1, 0),
    generic = c(1, 0, 0, 0)
  )
  ids <- rownames(normalized)
  classes <- c("class_a", "class_b", "class_b", "class_c", "class_d",
               "class_e", "class_f", "other plastic")
  libraries <- c("lib_1", "lib_2", "lib_3", "lib_4", "lib_5",
                 "lib_6", "lib_6", "lib_7")
  pools <- rep("infrared", length(ids))

  first <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, normalized, ids, libraries, threshold = 0.9,
    block_size = 2L
  )
  second <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, normalized, ids, libraries, threshold = 0.9,
    block_size = 5L
  )

  expect_identical(first$removed_rows, c(1L, 4L, 5L, 6L, 7L))
  expect_equal(first$removals, second$removals)
  expect_equal(first$removals[spectrum_id == "high", evidence_libraries], 2L)
  expect_equal(first$removals[spectrum_id == "high", decision_round], 1L)
  expect_true(all(
    first$removals[spectrum_id %in% c("tie_c", "tie_d"), reason] ==
      "cross_class_equal_library_evidence"
  ))
  expect_true(all(
    first$removals[spectrum_id %in% c("same_source_e", "same_source_f"),
                   phase] == "internal"
  ))
  expect_true(all(
    first$removals[spectrum_id %in% c("same_source_e", "same_source_f"),
                   reason] == "cross_class_equal_internal_degree"
  ))
  expect_false("generic" %in% first$removals$spectrum_id)
})

test_that("cross-class scans collect every reconstructed query block", {
  normalized <- rbind(
    suspect = c(1, 0, 0, 0),
    reference_1 = c(1, 0, 0, 0),
    reference_2 = c(1, 0, 0, 0),
    tie_a = c(0, 1, 0, 0),
    tie_b = c(0, 1, 0, 0),
    distinct = c(0, 0, 1, 0)
  )
  args <- list(
    classes = c("class_a", "class_b", "class_b", "class_c", "class_d",
                "class_e"),
    pools = rep("ftir", nrow(normalized)), normalized = normalized,
    ids = rownames(normalized),
    library_names = c("lib_1", "lib_2", "lib_3", "lib_4", "lib_5",
                      "lib_6"),
    threshold = 0.9, block_size = 2L
  )
  expected <- do.call(OpenSpecy:::.lib_prune_cross_class_conflicts, args)
  correlation_values <- OpenSpecy:::.lib_correlation_values
  query_calls <- 0L
  collections <- logical()
  local_mocked_bindings(
    .lib_correlation_values = function(...) {
      query_calls <<- query_calls + 1L
      correlation_values(...)
    },
    .lib_compaction_gc = function(full = FALSE) {
      collections <<- c(collections, isTRUE(full))
      invisible(NULL)
    },
    .package = "OpenSpecy"
  )

  actual <- do.call(OpenSpecy:::.lib_prune_cross_class_conflicts, args)
  expect_equal(actual, expected, tolerance = 0)
  expect_gt(query_calls, 0L)
  expect_identical(length(collections), query_calls)
  expect_false(any(collections))
})

test_that("correlation validation collects every reconstructed query block", {
  normalized <- rbind(
    ftir_a = c(1, 0, 0, 0),
    ftir_b = c(0, 1, 0, 0),
    ftir_c = c(0, 0, 1, 0),
    raman_a = c(1, 0, 0, 0),
    raman_b = c(0, 1, 0, 0),
    raman_c = c(0, 0, 1, 0)
  )
  ids <- rownames(normalized)
  pools <- rep(c("ftir", "raman"), each = 3L)
  classes <- rep(c("class_a", "class_b", "class_c"), 2L)
  view <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids = ids, pools = pools, wavenumber = seq_len(ncol(normalized))
  )
  object <- as_OpenSpecy(
    seq_len(ncol(normalized)), t(normalized),
    metadata = data.table::data.table(
      sample_name = ids, material_class = classes, spectrum_type = pools
    )
  )
  correlation_values <- OpenSpecy:::.lib_correlation_values
  query_calls <- 0L
  collections <- logical()
  local_mocked_bindings(
    .lib_correlation_values = function(...) {
      query_calls <<- query_calls + 1L
      correlation_values(...)
    },
    .lib_compaction_gc = function(full = FALSE) {
      collections <<- c(collections, isTRUE(full))
      invisible(NULL)
    },
    .package = "OpenSpecy"
  )

  summary <- OpenSpecy:::.lib_correlation_validation_summary(
    view, object, classes, threshold = 0.9, block_size = 2L
  )
  expect_s3_class(summary, "OpenSpecyCorrelationValidation")
  expect_identical(query_calls, 4L)
  expect_identical(length(collections), query_calls + 1L)
  expect_false(any(collections))
})

test_that("internal cross-class majority removes the active high-degree spectrum", {
  normalized <- rbind(
    suspect = c(1, 0, 0, 0),
    reference_1 = c(1, 0, 0, 0),
    reference_2 = c(1, 0, 0, 0),
    reference_3 = c(1, 0, 0, 0),
    tie_a = c(0, 1, 0, 0),
    tie_b = c(0, 1, 0, 0)
  )
  first <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes = c("class_a", rep("class_b", 3), "class_c", "class_d"),
    pools = rep("ftir", 6), normalized = normalized,
    ids = rownames(normalized), library_names = rep("one_library", 6),
    threshold = 0.9, min_n = 1, block_size = 2L
  )
  second <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes = c("class_a", rep("class_b", 3), "class_c", "class_d"),
    pools = rep("ftir", 6), normalized = normalized,
    ids = rownames(normalized), library_names = rep("one_library", 6),
    threshold = 0.9, min_n = 1, block_size = 5L
  )

  expect_identical(first$removed_rows, c(1L, 5L, 6L))
  expect_equal(first$removals, second$removals)
  expect_equal(first$removals[spectrum_id == "suspect", active_degree], 3L)
  expect_equal(first$removals[spectrum_id == "suspect", decision_round], 1L)
  expect_true(all(first$removals[spectrum_id %in% c("tie_a", "tie_b"),
                                 decision_round] == 2L))
})

test_that("internal cross-class support counts span the complete database", {
  normalized <- rbind(
    a_source_1 = c(1, 0, 0, 0),
    b_source_1a = c(1, 0, 0, 0),
    b_source_1b = c(1, 0, 0, 0),
    a_source_2 = c(0, 0, 1, 0),
    a_source_3 = c(0, 0, 0, 1)
  )
  result <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes = c("class_a", "class_b", "class_b", "class_a", "class_a"),
    pools = rep("ftir", 5), support_groups = rep("ftir", 5),
    normalized = normalized, ids = rownames(normalized),
    library_names = c("lib_1", "lib_1", "lib_1", "lib_2", "lib_3"),
    threshold = 0.9, min_n = 2
  )

  expect_identical(result$removed_rows, 1L)
  expect_equal(result$removals$class_n_before, 3L)
  expect_equal(result$removals$class_n_after, 2L)
  expect_equal(result$removals$review_status, "automatic")

  at_risk <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes = c("class_a", "class_b", "class_b", "class_a", "class_a"),
    pools = rep("ftir", 5), support_groups = rep("ftir", 5),
    normalized = normalized, ids = rownames(normalized),
    library_names = c("lib_1", "lib_1", "lib_1", "lib_2", "lib_3"),
    threshold = 0.9, min_n = 3
  )
  expect_equal(at_risk$removals$review_status, "global_support_at_risk")
})

test_that("quarantine bundles retain spectra and review metadata", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    material_class = rep(c("class_a", "class_b"), 4),
    material_type = "plastic", spectrum_type = "ftir",
    library_name = rep(c("lib_a", "lib_b"), 4)
  )]
  ids <- as.character(lib$metadata$sample_name)
  removal <- OpenSpecy:::.lib_prune_cross_class_schema()[0]
  removal <- data.table::data.table(
    phase = "internal", correlation_view = "typed_full",
    component_id = "internal:one", spectrum_id = ids[[1L]],
    prior_class = "class_a", library_name = "lib_a",
    matched_id = ids[[2L]], matched_class = "class_b",
    matched_library = "lib_b", correlation = 0.95, pool = "ftir",
    active_degree = 3L, matched_active_degree = 1L,
    evidence_libraries = 0L, conflicting_spectra = 3L,
    matched_evidence_libraries = 0L, decision_round = 1L,
    threshold = 0.9, class_n_before = 4L, class_n_after = 3L,
    schedule_order = NA_integer_, quarantine_status = "excluded",
    review_status = "automatic",
    reason = "cross_class_more_internal_conflicts"
  )
  part <- OpenSpecy:::.lib_quarantine_part(
    lib, removal, "derivative", "ftir", min_n = 3
  )
  bundle <- OpenSpecy:::.lib_quarantine_bundle(
    list(part), build_signature = "fixture", thresholds = 0.9
  )
  output <- tempfile("OpenSpecy-quarantine-review-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  OpenSpecy:::.lib_write_quarantine_review(bundle, output)
  restored <- readRDS(file.path(output, "review", "quarantined_spectra.rds"))

  expect_s3_class(restored, "OpenSpecyQuarantine")
  expect_named(
    restored,
    c("spectra", "removals", "conflicts", "source_failures", "manifest")
  )
  expect_identical(
    restored$manifest$schema, "OpenSpecy_quarantined_spectra_v2"
  )
  expect_identical(restored$manifest$removed_spectra, 1L)
  expect_true(check_OpenSpecy(restored$spectra[["derivative/ftir"]]))
  expect_equal(
    restored$spectra[["derivative/ftir"]]$metadata$quarantine_reason,
    "cross_class_more_internal_conflicts"
  )
  expect_true(all(file.exists(file.path(
    output, "review",
    c("quarantined_spectra_metadata.csv",
      "quarantined_spectra_removals.csv",
      "quarantined_spectra_conflicts.csv",
      "quarantined_source_failures.csv")
  ))))
})

test_that("quarantine captures duplicate removals with unique payload IDs", {
  lib <- tiny_build_lib()
  duplicate <- filter_spec(lib, c(1L, 1L, 2L))
  duplicate$metadata$sample_name <- c("same", "same", "other")
  colnames(duplicate$spectra) <- duplicate$metadata$sample_name
  duplicate$metadata$col_id <- colnames(duplicate$spectra)

  parts <- OpenSpecy:::.lib_quarantine_typed_capture(
    duplicate, c(TRUE, TRUE, FALSE), artifact = "source",
    stage = "source_deduplication", rule = "content_hash",
    reason = "duplicate_content_remove_all"
  )
  bundle <- OpenSpecy:::.lib_quarantine_bundle(parts)

  expect_true(OpenSpecy:::.lib_validate_quarantine(bundle))
  expect_identical(bundle$manifest$removed_spectra, 2L)
  expect_identical(anyDuplicated(bundle$removals$quarantine_key), 0L)
  expect_identical(
    unique(bundle$removals$quarantine_original_id), "same"
  )
  expect_identical(
    anyDuplicated(colnames(bundle$spectra[["source/ftir"]]$spectra)), 0L
  )
})

test_that("build_lib records source deduplication in quarantine", {
  lib <- tiny_build_lib()
  lib$spectra[, 2L] <- lib$spectra[, 1L]
  built <- build_lib(
    lib, recipes = list(raw = list()), res = 100,
    dedupe = TRUE, signal_noise = FALSE, progress = FALSE
  )
  bundle <- OpenSpecy:::.lib_quarantine_bundle(
    attr(built, "quarantine_parts", exact = TRUE)
  )

  expect_gt(bundle$manifest$removed_spectra, 0L)
  expect_equal(
    ncol(built$raw$spectra) + bundle$manifest$removed_spectra,
    ncol(lib$spectra)
  )
  expect_identical(unique(bundle$removals$quarantine_stage),
                   "source_deduplication")
  expect_identical(unique(bundle$removals$quarantine_rule), "content_hash")
})

test_that("typed closure filters parents before medoid construction", {
  wn <- seq(800, 3200, length.out = 60)
  peak <- dnorm(seq(-3, 3, length.out = length(wn)))
  other_1 <- dnorm(seq(-3, 3, length.out = length(wn)), mean = -1.5)
  other_2 <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1.5)
  spectra <- cbind(peak, peak * 1.01, peak * 0.99, other_1, other_2)
  colnames(spectra) <- c("suspect", "b1", "b2", "a2", "a3")
  parent <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c("class_a", "class_b", "class_b", "class_a", "class_a"),
      material_type = "plastic", spectrum_type = "ftir",
      library_name = c("lib_1", "lib_1", "lib_1", "lib_2", "lib_3")
    )
  )
  closed <- OpenSpecy:::.lib_close_typed_reference_libraries(
    list(derivative = list(ftir = parent)),
    prune_spec = list(derivative = list(
      cross_class = TRUE, cross_class_threshold = 0.9, min_n = 2
    )),
    progress = FALSE
  )

  expect_false("suspect" %in%
                 closed$libraries$derivative$ftir$metadata$sample_name)
  expect_equal(unname(table(
    closed$libraries$derivative$ftir$metadata$material_class
  )["class_a"]), 2L)
  expect_true(nrow(closed$removals) >= 1L)
  expect_true(any(closed$removals$correlation_view == "typed_full"))
  expect_length(closed$quarantine_parts, 1L)
  expect_true(check_OpenSpecy(closed$quarantine_parts[[1L]]))
  expect_length(closed$correlation_validation_cache, 1L)

  closed_parent <- closed$libraries$derivative$ftir
  medoid_rows <- !duplicated(closed_parent$metadata$material_class)
  medoid <- OpenSpecy:::.lib_restrict_model_range(
    filter_spec(closed_parent, medoid_rows), "ftir"
  )
  prune_spec <- list(derivative = list(
    cross_class = TRUE, cross_class_threshold = 0.9, min_n = 2
  ))
  bounded_validation <- OpenSpecy:::.lib_validate_medoid_parent_closure(
    closed$libraries, list(derivative = list(ftir = medoid)),
    prune_spec = prune_spec
  )
  cached_validation <- OpenSpecy:::.lib_validate_medoid_parent_closure(
    closed$libraries, list(derivative = list(ftir = medoid)),
    prune_spec = prune_spec,
    correlation_cache = closed$correlation_validation_cache
  )
  expect_equal(cached_validation, bounded_validation, tolerance = 0)

  stale <- closed$correlation_validation_cache
  stale[[1L]]$numeric_key <- "stale"
  expect_error(
    OpenSpecy:::.lib_validate_medoid_parent_closure(
      closed$libraries, list(derivative = list(ftir = medoid)),
      prune_spec = prune_spec, correlation_cache = stale
    ),
    "Stale correlation validation cache"
  )
})

test_that("medoid-parent validation bounds fallback blocks and cached hashes", {
  wn <- seq(800, 3200, length.out = 60)
  axis <- seq(-3, 3, length.out = length(wn))
  spectra <- cbind(
    a1 = dnorm(axis, mean = -1.1),
    a2 = dnorm(axis, mean = -1),
    a3 = dnorm(axis, mean = -0.9),
    b1 = dnorm(axis, mean = 0.9),
    b2 = dnorm(axis, mean = 1.1)
  )
  parent <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c("class_a", "class_a", "class_a", "class_b", "class_b"),
      material_type = "plastic", spectrum_type = "ftir",
      library_name = c("lib_1", "lib_2", "lib_3", "lib_1", "lib_2")
    )
  )
  parent_model_range <- OpenSpecy:::.lib_restrict_model_range(parent, "ftir")
  medoid <- filter_spec(
    parent_model_range, c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  libraries <- list(derivative = list(ftir = parent))
  medoids <- list(derivative = list(ftir = medoid))
  prune_spec <- list(derivative = list(
    cross_class = TRUE, cross_class_threshold = 0.999999
  ))
  collections <- logical()
  local_mocked_bindings(
    .lib_compaction_gc = function(full = FALSE) {
      collections <<- c(collections, isTRUE(full))
      invisible(NULL)
    },
    .package = "OpenSpecy"
  )

  fallback <- OpenSpecy:::.lib_validate_medoid_parent_closure(
    libraries, medoids, prune_spec = prune_spec, block_size = 2L
  )
  expect_equal(fallback$wrong_class_pairs_above_threshold, 0L)
  expect_identical(
    length(collections), as.integer(ceiling(ncol(parent$spectra) / 2) + 1L)
  )
  expect_false(any(collections))

  parent_ids <- as.character(parent_model_range$metadata$sample_name)
  cache <- stats::setNames(list(structure(list(
    schema = "OpenSpecy_medoid_correlation_validation_v1",
    view_key = "test",
    numeric_key = OpenSpecy:::.lib_numeric_view_hash(
      parent_model_range$spectra, parent_model_range$wavenumber,
      parent_ids, state = "model_range"
    ),
    ids = parent_ids,
    classes = as.character(parent_model_range$metadata$material_class),
    wavenumber = parent_model_range$wavenumber,
    threshold = 0.999999,
    max_wrong_class_correlation = rep(-Inf, length(parent_ids)),
    wrong_class_pairs_above_threshold = integer(length(parent_ids))
  ), class = c("OpenSpecyCorrelationValidation", "list"))), "derivative\rftir")
  collections <- logical()
  cached <- OpenSpecy:::.lib_validate_medoid_parent_closure(
    libraries, medoids, prune_spec = prune_spec, correlation_cache = cache
  )
  expect_equal(cached$wrong_class_pairs_above_threshold, 0L)
  expect_identical(collections, FALSE)
})

test_that("cross-class pruning uses a strict threshold", {
  normalized <- rbind(c(1, 0), c(0.9, sqrt(1 - 0.9^2)))
  args <- list(
    classes = c("class_a", "class_b"), pools = c("infrared", "infrared"),
    normalized = normalized, ids = c("a", "b"),
    library_names = c("lib_a", "lib_b")
  )

  exact <- do.call(
    OpenSpecy:::.lib_prune_cross_class_conflicts,
    c(args, list(threshold = 0.9))
  )
  below <- do.call(
    OpenSpecy:::.lib_prune_cross_class_conflicts,
    c(args, list(threshold = 0.899))
  )

  expect_length(exact$removed_rows, 0L)
  expect_identical(below$removed_rows, 1:2)
})

test_that("dense and compact views equal bounded pruning and survive masks", {
  old_options <- options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )
  on.exit(options(old_options), add = TRUE)
  normalized <- rbind(
    suspect = c(1, 0, 0, 0),
    reference_a = c(1, 0, 0, 0),
    reference_b = c(1, 0, 0, 0),
    tie_a = c(0, 1, 0, 0),
    tie_b = c(0, 1, 0, 0),
    distinct = c(0, 0, 1, 0),
    generic = c(0, 0, 1, 0)
  )
  ids <- rownames(normalized)
  pools <- rep("ftir", length(ids))
  classes <- c(
    "class_a", "class_b", "class_b", "class_c", "class_d", "class_e",
    "other plastic"
  )
  libraries <- c("lib_1", "lib_2", "lib_3", "lib_4", "lib_5", "lib_6",
                 "lib_7")
  dense <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, seq_len(ncol(normalized)), state = "test"
  )
  dense_result <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, dense, ids, libraries, threshold = 0.9,
    block_size = 2L
  )
  bounded_result <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, normalized, ids, libraries, threshold = 0.9,
    block_size = 2L
  )
  expect_identical(dense_result$removed_rows, bounded_result$removed_rows)
  expect_equal(dense_result$removals, bounded_result$removals, tolerance = 0)
  expect_equal(dense_result$conflicts, bounded_result$conflicts, tolerance = 0)
  compact <- OpenSpecy:::.lib_compact_threshold_correlation_view(
    dense, threshold = 0.9, block_size = 2L
  )
  compact_result <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, compact, ids, libraries, threshold = 0.9,
    block_size = 2L
  )
  expect_identical(compact_result$removed_rows, bounded_result$removed_rows)
  expect_equal(compact_result$removals, bounded_result$removals, tolerance = 0)
  expect_equal(compact_result$conflicts, bounded_result$conflicts, tolerance = 0)
  streamed_compact <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, seq_len(ncol(normalized)), state = "test",
    block_size = 2L, compact_threshold = 0.9
  )
  streamed_result <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    classes, pools, streamed_compact, ids, libraries, threshold = 0.9,
    block_size = 2L
  )
  expect_identical(streamed_result$removed_rows, bounded_result$removed_rows)
  expect_equal(streamed_result$removals, bounded_result$removals, tolerance = 0)
  expect_equal(streamed_result$conflicts, bounded_result$conflicts, tolerance = 0)
  expect_true(all(vapply(
    streamed_compact$correlations,
    OpenSpecy:::.lib_is_threshold_correlation_pool,
    logical(1L)
  )))
  expect_equal(
    OpenSpecy:::.lib_correlation_view_stats(streamed_compact)$
      minimum_retained_correlation,
    0.9
  )

  keep <- c(1L, 2L, 4L, 5L, 6L, 7L)
  relabeled <- classes[keep]
  relabeled[relabeled == "other plastic"] <- "class_f"
  dense_masked <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    relabeled, pools[keep], dense, ids[keep], libraries[keep],
    threshold = 0.9, block_size = 3L
  )
  bounded_masked <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    relabeled, pools[keep], normalized[keep, , drop = FALSE], ids[keep],
    libraries[keep], threshold = 0.9, block_size = 3L
  )
  expect_identical(dense_masked$removed_rows, bounded_masked$removed_rows)
  expect_equal(dense_masked$removals, bounded_masked$removals, tolerance = 0)
  expect_equal(dense_masked$conflicts, bounded_masked$conflicts, tolerance = 0)
  compact_masked <- OpenSpecy:::.lib_prune_cross_class_conflicts(
    relabeled, pools[keep], compact, ids[keep], libraries[keep],
    threshold = 0.9, block_size = 3L
  )
  expect_identical(compact_masked$removed_rows, bounded_masked$removed_rows)
  expect_equal(compact_masked$removals, bounded_masked$removals, tolerance = 0)
  expect_equal(compact_masked$conflicts, bounded_masked$conflicts, tolerance = 0)
  stats <- OpenSpecy:::.lib_correlation_view_stats(dense)
  compact_stats <- OpenSpecy:::.lib_correlation_view_stats(compact)
  expect_equal(stats$dense_builds, 1L)
  expect_gt(stats$dense_queries, 1L)
  expect_equal(stats$bounded_queries, 0L)
  expect_gt(compact_stats$compacted_queries, 1L)
})

test_that("streamed compaction releases dense aliases between unequal pools", {
  old_options <- options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )
  on.exit(options(old_options), add = TRUE)
  normalized <- rbind(
    ftir_anchor = c(1, 0, 0),
    ftir_above = c(0.91, sqrt(1 - 0.91^2), 0),
    ftir_exact = c(0.9, sqrt(1 - 0.9^2), 0),
    ftir_distinct = c(0, 0, 1),
    raman_anchor = c(0, 1, 0),
    raman_above = c(0, 0.95, sqrt(1 - 0.95^2)),
    raman_distinct = c(1, 0, 0)
  )
  ids <- rownames(normalized)
  pools <- c(rep("ftir", 4L), rep("raman", 3L))
  observed <- list()
  collections <- logical()
  compact_pool <- OpenSpecy:::.lib_compact_threshold_correlation_pool
  compact_gc <- OpenSpecy:::.lib_compaction_gc
  local_mocked_bindings(
    .lib_compact_threshold_correlation_pool = function(
        dense, threshold, block_size = 128L) {
      caller <- parent.frame()
      has_result <- exists("result", envir = caller, inherits = FALSE)
      observed[[length(observed) + 1L]] <<- list(
        spectra = nrow(dense),
        retained_result_binding = has_result,
        result_aliases_dense = has_result &&
          identical(get("result", envir = caller, inherits = FALSE), dense)
      )
      compact_pool(dense, threshold = threshold, block_size = block_size)
    },
    .lib_compaction_gc = function(full = FALSE) {
      collections <<- c(collections, isTRUE(full))
      compact_gc(full = full)
    },
    .package = "OpenSpecy"
  )

  view <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, seq_len(ncol(normalized)),
    state = "unequal-pool-lifecycle", block_size = 2L,
    compact_threshold = 0.9
  )

  expect_identical(
    vapply(observed, `[[`, integer(1L), "spectra"),
    c(4L, 3L)
  )
  expect_false(any(vapply(
    observed, `[[`, logical(1L), "retained_result_binding"
  )))
  expect_false(any(vapply(
    observed, `[[`, logical(1L), "result_aliases_dense"
  )))
  expect_equal(view$metrics$dense_builds, 2L)
  # One collection after hashing, one after each dense product, and one after
  # each single-block compaction bounds temporary Vcells across both pools.
  # Small views avoid the fixed cost of a full collection; production views
  # record and request it once either normalized or dense storage is material.
  expect_identical(collections, rep(FALSE, 5L))
  expect_false(any(view$preflight$full_boundary_gc))
  expect_null(view$normalized)
  expect_true(all(vapply(
    view$correlations,
    OpenSpecy:::.lib_is_threshold_correlation_pool,
    logical(1L)
  )))

  for (pool in unique(pools)) {
    rows <- which(pools == pool)
    expected <- tcrossprod(normalized[rows, , drop = FALSE])
    keep <- is.finite(expected) & expected > 0.9
    diag(keep) <- FALSE
    expected[!keep] <- -Inf
    dimnames(expected) <- NULL
    actual <- OpenSpecy:::.lib_correlation_values(
      view, ids[rows], seq_along(rows), seq_along(rows)
    )
    expect_equal(actual, expected, tolerance = 0)
  }
})

test_that("correlation view keys invalidate numeric views and guard fallback", {
  normalized <- rbind(a = c(1, 0), b = c(0.9, sqrt(1 - 0.9^2)))
  ids <- rownames(normalized)
  pools <- rep("ftir", 2L)
  old_options <- options(
    OpenSpecy.correlation_cache = "dense",
    OpenSpecy.correlation_cache_max_bytes = Inf
  )
  on.exit(options(old_options), add = TRUE)
  first <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, c(1, 2), state = "test"
  )
  changed_axis <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, c(1, 3), state = "test"
  )
  changed_values <- normalized
  changed_values[2, 2] <- changed_values[2, 2] + 1e-8
  changed_numeric <- OpenSpecy:::.lib_new_correlation_view(
    changed_values, ids, pools, c(1, 2), state = "test"
  )
  expect_false(identical(first$key, changed_axis$key))
  expect_false(identical(first$key, changed_numeric$key))
  expect_error(
    OpenSpecy:::.lib_correlation_values(first, c("missing"), 1L, 1L),
    "stale"
  )

  options(
    OpenSpecy.correlation_cache = "auto",
    OpenSpecy.correlation_cache_max_bytes = 1
  )
  fallback <- OpenSpecy:::.lib_new_correlation_view(
    normalized, ids, pools, c(1, 2), state = "fallback"
  )
  fallback_values <- OpenSpecy:::.lib_correlation_values(
    fallback, ids, 1:2, 1:2
  )
  expect_equal(fallback_values, tcrossprod(normalized), tolerance = 0)
  fallback_stats <- OpenSpecy:::.lib_correlation_view_stats(fallback)
  expect_equal(fallback_stats$dense_builds, 0L)
  expect_equal(fallback_stats$bounded_queries, 1L)
  expect_equal(fallback$preflight$matrix_bytes, 32)
  expect_equal(fallback$preflight$mode, "bounded")
})

test_that("prune_lib() applies cross-class removal before generic reassignment", {
  wn <- seq(500, 3500, length.out = 80)
  shape <- dnorm(seq(-3, 3, length.out = length(wn)))
  spectra <- cbind(shape, shape * 1.01, shape * 0.99, shape * 1.02)
  colnames(spectra) <- c("suspect", "reference_1", "reference_2", "generic")
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c("class_a", "class_b", "class_b", "other plastic"),
      material_type = "plastic", spectrum_type = "ftir",
      library_name = c("lib_1", "lib_2", "lib_3", "lib_4")
    )
  )
  attr(lib, "test_marker") <- "preserved"

  report <- prune_lib(
    lib, min_n = 1, cross_class = TRUE, cross_class_threshold = 0.9,
    return = "report", progress = FALSE
  )

  expect_identical(report$cross_class_removals$spectrum_id, "suspect")
  expect_equal(report$summary$cross_class_removed, 1L)
  expect_false("suspect" %in% report$retained_ids)
  expect_equal(
    report$object$metadata[sample_name == "generic", material_class],
    "class_b"
  )
  expect_identical(colnames(report$object$spectra),
                   report$object$metadata$sample_name)
  expect_identical(report$object$wavenumber, lib$wavenumber)
  expect_identical(attr(report$object, "test_marker"), "preserved")
  expect_equal(report$object$spectra,
               lib$spectra[, report$retained_ids, drop = FALSE])

  recovered <- OpenSpecy:::.lib_recover_library_assessments(
    list(derivative = report$object), list()
  )
  expect_equal(recovered$pruning_removals$spectrum_id, "suspect")
})

test_that("prune_lib() validates cross-class policy inputs", {
  lib <- tiny_build_lib()
  lib$metadata$material_type <- "plastic"

  expect_error(prune_lib(lib, cross_class = NA), "TRUE or FALSE")
  expect_error(prune_lib(lib, cross_class_threshold = 1.01), "in \\[0, 1\\]")
  expect_error(
    prune_lib(lib, cross_class = TRUE),
    "requires populated library_name"
  )
})

test_that("prune_lib() reassigns undersupported classes by spectrum type", {
  wn <- seq(500, 3500, length.out = 40)
  spectra <- vapply(seq_len(7), function(i) {
    dnorm(seq(-3, 3, length.out = length(wn)), mean = i / 20)
  }, numeric(length(wn)))
  colnames(spectra) <- c(
    "ftir_a1", "ftir_a2", "ftir_rare", "raman_rare1", "raman_rare2",
    "ftir_unclassified", "ftir_missing"
  )
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(
        "stable", "stable", "rare", "rare", "rare", "unclassified", NA
      ),
      material_type = "plastic",
      spectrum_type = c(rep("ftir", 3), rep("raman", 2), "ftir", "ftir")
    )
  )

  messages <- capture.output(
    report <- prune_lib(
      lib, min_n = 2, return = "report", progress = TRUE
    ),
    type = "message"
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "minimum support gate reassigned 1 and removed 1 spectrum/spectra"
  )

  expect_equal(
    report$excluded_classes[, c(
      "spectrum_type", "material_class", "observed_n", "minimum_spectra",
      "shortfall", "destination_class", "spectra_removed", "action", "reason"
    ), with = FALSE],
    data.table::data.table(
      spectrum_type = c("ftir", "ftir"),
      material_class = c("rare", "unclassified"),
      observed_n = c(1L, 1L), minimum_spectra = c(2L, 2L),
      shortfall = c(1L, 1L), destination_class = c("stable", NA_character_),
      spectra_removed = c(0L, 1L), action = c("reassigned", "dropped"),
      reason = c("class_below_min_n_reassigned",
                 "class_below_min_n_no_eligible_destination")
    )
  )
  expect_true(all(c("raman_rare1", "raman_rare2") %in% report$retained_ids))
  expect_true("ftir_missing" %in% report$retained_ids)
  expect_true("ftir_rare" %in% report$retained_ids)
  expect_false("ftir_unclassified" %in% report$retained_ids)
  expect_equal(
    report$object$metadata[sample_name == "ftir_rare", material_class],
    "stable"
  )
  expect_equal(
    report$reassignments[spectrum_id == "ftir_rare", reason],
    "class_below_min_n_reassigned"
  )
  threshold_removals <- report$removals[reason == "class_below_min_n"]
  expect_equal(threshold_removals$spectrum_id, "ftir_unclassified")
  expect_true(all(is.na(threshold_removals$matched_id)))
  expect_type(threshold_removals$matched_id, "character")
  expect_equal(report$summary$classes_excluded, 1L)
  expect_equal(report$summary$threshold_removed, 1L)
  expect_true(check_OpenSpecy(report$object))
  expect_identical(colnames(report$object$spectra),
                   report$object$metadata$sample_name)

  recovered <- OpenSpecy:::.lib_recover_library_assessments(
    list(raw = lib, derivative = report$object),
    list(metadata_drop = data.table::data.table(metadata_column = character()))
  )
  expect_named(
    recovered$pruning_excluded_classes,
    names(OpenSpecy:::.lib_prune_excluded_assessment_schema())
  )
  expect_equal(unique(recovered$pruning_excluded_classes$artifact),
               "derivative")
  expect_equal(nrow(recovered$pruning_excluded_classes), 2L)
})

test_that("prune_lib() resolves generic labels before minimum support", {
  wn <- seq(500, 3500, length.out = 40)
  shape <- dnorm(seq(-3, 3, length.out = length(wn)))
  spectra <- cbind(shape, shape * 1.01)
  colnames(spectra) <- c("rare", "generic")
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c("polymer rare", "other plastic"),
      material_type = "plastic", spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 2, return = "report", progress = FALSE)

  expect_equal(report$object$metadata$material_class,
               rep("polymer rare", 2))
  expect_equal(nrow(report$reassignments), 1L)
  expect_equal(nrow(report$excluded_classes), 0L)
  expect_equal(report$schedule$initial_n, 2L)
})

test_that("prune_lib() sends an undersupported class to one correlated class", {
  wn <- seq(500, 3500, length.out = 40)
  shape_a <- dnorm(seq(-3, 3, length.out = length(wn)), mean = -0.5)
  shape_b <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1)
  spectra <- cbind(
    shape_a, shape_a * 1.01, shape_a * 0.99,
    shape_b, shape_b * 1.01, shape_b * 0.99,
    shape_a * 1.02, shape_a * 0.98
  )
  colnames(spectra) <- paste0("whole_", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(rep("class_a", 3), rep("class_b", 3), rep("rare", 2)),
      material_type = "plastic", spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 3, return = "report", progress = FALSE)
  reassigned <- report$reassignments[prior_class == "rare"]

  expect_equal(nrow(reassigned), 2L)
  expect_identical(unique(reassigned$material_class), "class_a")
  expect_true(all(report$object$metadata$sample_name %in%
                    colnames(lib$spectra)))
  expect_false("rare" %in% report$object$metadata$material_class)
  expect_equal(
    report$excluded_classes[material_class == "rare", action],
    "reassigned"
  )
})

test_that("prune_lib() retains unclassified spectra outside matching", {
  lib <- tiny_build_lib()
  lib$metadata$material_type <- "plastic"
  lib$metadata$material_class[1:2] <- "unclassified"
  protected_ids <- lib$metadata$sample_name[1:2]

  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_true(all(protected_ids %in% report$retained_ids))
  expect_false("unclassified" %in% report$schedule$material_class)
  expect_false(any(report$removals$prior_class == "unclassified"))
  expect_false(any(report$removals$matched_class == "unclassified"))
})

test_that("prune_lib() reassigns generic classes and tolerates no candidates", {
  lib <- filter_spec(tiny_build_lib(), 1:4)
  lib$metadata$material_class <- c("polymer a", "other plastic",
                                   "other material", "other plastic")
  lib$metadata$material_type <- c("plastic", "plastic", "mineral", "plastic")
  lib$metadata$spectrum_type <- c("ftir", "ftir", "raman", NA)
  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(report$object$metadata$material_class[2], "polymer a")
  expect_equal(report$object$metadata$material_class[3], "other material")
  expect_equal(report$object$metadata$material_class[4], "other plastic")
  expect_equal(nrow(report$reassignments), 1)
  expect_equal(report$summary$removed, 0L)
})

test_that("prune_lib() constrains generic reassignment and updates type", {
  wn <- seq(500, 3500, length.out = 80)
  plastic <- dnorm(seq(-3, 3, length.out = length(wn)))
  organic <- dnorm(seq(-3, 3, length.out = length(wn)), mean = 1)
  mineral <- rev(cumsum(seq_along(wn)))
  spectra <- cbind(plastic, organic, mineral, plastic, organic, mineral)
  colnames(spectra) <- paste0("generic", seq_len(ncol(spectra)))
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c(
        "polymer a", "organic matter", "mineral",
        "other plastic", "other material", "other"
      ),
      material_type = c(
        "plastic", "not plastic", "not plastic", "plastic", "not plastic",
        "other"
      ),
      spectrum_type = "ftir"
    )
  )

  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(
    report$object$metadata$material_class[4:6],
    c("polymer a", "organic matter", "mineral")
  )
  expect_equal(
    report$object$metadata$material_type[4:6],
    c("plastic", "not plastic", "not plastic")
  )
  expect_equal(nrow(report$reassignments), 3L)
  expect_true(all(report$reassignments$reason == "nearest_eligible_class"))
  expect_equal(nrow(report$generic_reassignment_audit), 3L)
  expect_true(all(report$generic_reassignment_audit$action == "reassigned"))
})

test_that("generic reassignment audits no-eligible and no-finite candidates", {
  wn <- seq(500, 3500, length.out = 80)
  varying <- dnorm(seq(-3, 3, length.out = length(wn)))
  spectra <- cbind(varying, rep(1, length(wn)), rep(1, length(wn)))
  colnames(spectra) <- c("ftir_target", "ftir_generic", "raman_generic")
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      material_class = c("polymer target", "other plastic", "other plastic"),
      material_type = "plastic",
      spectrum_type = c("ftir", "ftir", "raman")
    )
  )

  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)
  audit <- report$generic_reassignment_audit[order(spectrum_id)]

  expect_identical(
    audit$action,
    c("retained_no_finite_candidate", "retained_no_eligible_candidate")
  )
  expect_identical(
    audit$reason, c("no_finite_candidate", "no_eligible_candidate")
  )
  expect_true(all(is.na(audit$matched_id)))
  expect_equal(nrow(report$reassignments), 0L)
  expect_setequal(
    report$object$metadata$sample_name,
    c("ftir_target", "ftir_generic", "raman_generic")
  )
})

test_that("official pruning reattaches resolved generic review audits", {
  wn <- seq(500, 3500, length.out = 80)
  target <- dnorm(seq(-3, 3, length.out = length(wn)))
  spectra <- cbind(target, target * 0.99, target * 1.01, rev(target))
  colnames(spectra) <- c(
    "target_1", "target_2", "generic_match", "generic_no_candidate"
  )
  lib <- as_OpenSpecy(
    wn, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      spectrum_identity = c("target identity 1", "target identity 2",
                            "generic match identity",
                            "generic no-candidate identity"),
      material = c("target material", "target material", "other plastic",
                   "other plastic"),
      material_form = c(
        NA_character_, NA_character_, "source paint descriptor",
        "source composite descriptor"
      ),
      library_type = "fixture", library_name = "fixture",
      spectrum_type = c("ftir", "ftir", "ftir", "raman"),
      organization = "fixture", user_name = "fixture"
    )
  )
  hierarchy <- data.table::data.table(
    material = c("target material", "other plastic"),
    material_class = c("polymer target", "other plastic"),
    material_type = "plastic"
  )
  common_use <- data.table::data.table(
    material_class = hierarchy$material_class, common_use = NA_character_,
    consumer_mass_share = NA_real_, industrial_mass_share = NA_real_,
    unclassified_mass_share = NA_real_, basis_year = NA_integer_,
    geography = NA_character_, source_url = NA_character_,
    retrieved_date = NA_character_, evidence_notes = NA_character_
  )
  tables <- list(
    classes_reference = data.table::data.table(
      spectrum_identity = lib$metadata$spectrum_identity,
      material = lib$metadata$material
    ),
    classes_regex = data.table::data.table(
      pattern = "unlikely_regex_token", material = "other material"
    ),
    material_hierarchy = hierarchy,
    material_group_reference = data.table::data.table(
      material_class = hierarchy$material_class,
      material_type = hierarchy$material_type,
      material_group = c("fixture polymer", "unresolved polymer")
    ),
    material_form_regex = data.table::data.table(
      pattern = "\\bpaint\\b", material_form = "paint"
    ),
    common_use_reference = common_use,
    metadata_drop = data.table::data.table(metadata_column = character())
  )

  captured <- NULL
  local_mocked_bindings(
    .lib_partition_reference_libraries = function(libraries, report = NULL) {
      captured <<- if (is.environment(libraries)) {
        libraries$libraries
      } else {
        libraries
      }
      stop("capture_after_pruning", call. = FALSE)
    },
    .package = "OpenSpecy"
  )
  expect_error(
    suppressWarnings(OpenSpecy:::.lib_complete_reference_build(
      list(raw = lib), tables = tables,
      prune = list(raw = list(
        min_n = 2L, cross_class = FALSE, progress = FALSE
      )),
      remove_other = TRUE, progress = FALSE, report = function(...) NULL
    )),
    "capture_after_pruning"
  )

  expect_true(check_OpenSpecy(captured$raw))
  attached <- attr(captured$raw, "other_review_report", exact = TRUE)
  recovered <- OpenSpecy:::.lib_recover_library_assessments(captured, tables)
  expect_equal(
    recovered$other_review[order(spectrum_id)],
    attached[order(spectrum_id)], tolerance = 0
  )

  matched <- recovered$other_review[spectrum_id == "generic_match"]
  expect_identical(matched$action, "reassigned")
  expect_identical(matched$matched_id, "target_1")
  expect_identical(matched$matched_material_class, "polymer target")
  expect_identical(matched$matched_material_type, "plastic")
  expect_identical(matched$original_material_form, "source paint descriptor")
  expect_true(is.finite(matched$match_correlation))
  expect_identical(matched$match_pool, "ftir_nir")
  expect_identical(
    captured$raw$metadata[
      sample_name == "generic_match", material_form
    ],
    "paint"
  )

  unmatched <- recovered$other_review[
    spectrum_id == "generic_no_candidate"
  ]
  expect_identical(unmatched$action, "retained_no_eligible_candidate")
  expect_true("generic_no_candidate" %in% captured$raw$metadata$sample_name)
  expect_true(is.na(unmatched$matched_id))
  expect_true(is.na(unmatched$matched_material_class))
  expect_true(is.na(unmatched$matched_material_type))
  expect_identical(
    unmatched$original_material_form, "source composite descriptor"
  )
  expect_true(is.na(unmatched$match_correlation))
  expect_identical(unmatched$match_pool, "raman")
  expect_true(is.na(captured$raw$metadata[
    sample_name == "generic_no_candidate", material_form
  ]))
})

test_that("prune_lib() preserves non-generic class labels and empty audits", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  lib$metadata$material_class <- c("Polymer A", "Polymer A")
  lib$metadata$material_type <- "plastic"
  report <- prune_lib(lib, min_n = 1, return = "report", progress = FALSE)

  expect_equal(report$object$metadata$material_class,
               c("Polymer A", "Polymer A"))
  expect_s3_class(report$reassignments, "data.table")
  expect_s3_class(report$generic_reassignment_audit, "data.table")
  expect_s3_class(report$removals, "data.table")
  expect_equal(nrow(report$reassignments), 0)
  expect_equal(nrow(report$generic_reassignment_audit), 0)
  expect_equal(nrow(report$removals), 0)
  expect_equal(report$summary$reassigned, 0L)
  expect_equal(report$summary$removed, 0L)
})

test_that("build_lib() applies pruning only to named recipes", {
  lib <- tiny_build_lib()
  lib$metadata$material_type <- "plastic"
  built <- build_lib(
    lib,
    recipes = list(raw = list(), processed = list()),
    prune = list(processed = list(min_n = 1, progress = FALSE)),
    dedupe = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )

  expect_null(attr(built$raw, "prune_report"))
  expect_true(is.list(attr(built$processed, "prune_report")))
})

test_that("reference workflow tables encode reviewed taxonomy and source rules", {
  classes <- data.table::fread(
    reference_workflow_data_path("classes_reference.csv")
  )
  regex_classes <- data.table::fread(
    reference_workflow_data_path("classes_regex.csv")
  )
  hierarchy <- data.table::fread(
    reference_workflow_data_path("material_hierarchy.csv")
  )
  types <- data.table::fread(
    reference_workflow_data_path("library_types.csv")
  )
  drops <- data.table::fread(
    reference_workflow_data_path("metadata_drop_columns.csv")
  )
  known_bad <- data.table::fread(
    reference_workflow_data_path("known_bad_ids.csv")
  )
  form_regex <- data.table::fread(
    reference_workflow_data_path("material_form_regex.csv")
  )
  common_use <- data.table::fread(
    reference_workflow_data_path("common_use_reference.csv")
  )
  material_groups <- data.table::fread(
    reference_workflow_data_path("material_group_reference.csv")
  )

  expect_false(anyNA(classes$spectrum_identity))
  expect_false(any(classes$spectrum_identity == ""))
  expect_identical(anyDuplicated(classes$spectrum_identity), 0L)
  expect_false(any(grepl(
    "^([0-9]{1,2}-[A-Za-z]{3}|[A-Za-z]{3}-[0-9]{1,2})$",
    classes$spectrum_identity
  )))
  expect_false(any(grepl("^regex:", classes$spectrum_identity)))
  expect_named(regex_classes, c("pattern", "material"))
  expect_false(anyNA(regex_classes$pattern))
  expect_false(any(regex_classes$pattern == ""))
  expect_identical(anyDuplicated(regex_classes$pattern), 0L)
  class_audit <- predict_class_reference(
    classes, regex_classes, return = "report"
  )
  expect_gte(class_audit$summary$predicted, 0L)
  expect_equal(class_audit$summary$clashes, 0L)
  expect_gt(class_audit$summary$overlaps, 0L)
  expect_true(all(!is.na(regex_classes$material)))
  expect_identical(anyDuplicated(known_bad$sample_name), 0L)
  expect_true(all(grepl("^[[:xdigit:]]{32}$", known_bad$sample_name)))
  expect_identical(anyDuplicated(hierarchy$material), 0L)
  expect_silent(OpenSpecy:::.lib_validate_material_form_regex(form_regex))
  expect_silent(OpenSpecy:::.lib_validate_common_use_reference(
    common_use, hierarchy
  ))
  expect_setequal(
    common_use$material_class, unique(hierarchy$material_class)
  )
  expect_identical(anyDuplicated(material_groups$material_class), 0L)
  expect_setequal(
    material_groups$material_class, unique(hierarchy$material_class)
  )
  hierarchy_types <- unique(hierarchy[, .(material_class, material_type)])
  expect_equal(
    material_groups$material_type,
    hierarchy_types$material_type[match(
      material_groups$material_class, hierarchy_types$material_class
    )]
  )
  expect_equal(
    material_groups[material_class == "polyvinylalcohols", material_group],
    "vinyl alcohol-acetate-acetal"
  )
  reviewed_group_routes <- c(
    "poly(ethylene-propylene-diene) rubber" = "diene-elastomer",
    "polyacrylonitrile-butadiene" = "diene-elastomer",
    "polydienes" = "diene-elastomer",
    "polyisobutylene-isoprene" = "diene-elastomer",
    "polyolefins" = "olefinic",
    "polycyanoacrylates" = "acrylic",
    "polyhydroxy(meth)acrylates" = "acrylic",
    "polystyrene-acrylonitrile" = "styrenic",
    "polystyrene-maleic" = "styrenic",
    "polyhydroxyalkanoates" = "ester-carbonate",
    "polyhalodienes" = "specialty",
    "polyhalostyrenes" = "specialty"
  )
  expect_identical(
    stats::setNames(
      material_groups$material_group[match(
        names(reviewed_group_routes), material_groups$material_class
      )],
      names(reviewed_group_routes)
    ),
    reviewed_group_routes
  )
  expect_false("vinyl" %in% material_groups$material_group)
  expect_true(all(c("consumer", "industrial", "mixed") %in%
                    stats::na.omit(common_use$common_use)))
  expect_equal(hierarchy[material == "other", material_class], "other")
  expect_equal(hierarchy[material == "other", material_type], "other")
  expect_equal(classes[spectrum_identity == "pa", material], "polyamides")
  exact_classes <- classes[!grepl("^regex:", spectrum_identity)]
  expect_identical(
    OpenSpecy:::.lib_clean_spectrum_identity(exact_classes$spectrum_identity),
    exact_classes$spectrum_identity
  )
  expect_true(all(
    hierarchy[grepl("adipate", material), material_class] == "polyesters"
  ))
  expect_false("polyamides (polylactams)" %in% hierarchy$material_class)
  expect_true(all(c("polyamides", "polyacrylamides") %in%
                    hierarchy$material_class))
  expect_equal(
    classes[spectrum_identity == "plc004_kn95 outer layer_pp", material],
    "poly(propylene)"
  )
  expect_equal(
    classes[spectrum_identity == "plc008_label tape_unknown", material],
    "other plastic"
  )
  expect_true(all(
    classes[spectrum_identity %in% c(
      "crumb rubber from used tires", "r 4. black bridgestone tire fragment",
      "r 5. black michelin tire fragment", "r 6. black bike tire fragment",
      "r 8. white bike tire fragment"
    ), material] == "styrene-butadiene"
  ))
  expect_equal(classes[spectrum_identity == "pva", material], "other plastic")
  expect_equal(
    hierarchy[material == "poly(4-vinyl phenol)", material_class],
    "polystyrenes"
  )
  expect_equal(
    hierarchy[material == "poly(4-vinylpyridine)", material_class],
    "polyvinylpyridines"
  )
  expect_equal(
    hierarchy[material == "poly(vinyl pyrrolidone)", material_class],
    "polyvinylpyrrolidones"
  )
  expect_true(all(
    hierarchy[material %in% c(
      "poly(ethyleneketone)", "poly(propyleneketone)"
    ), material_class] == "polyketones"
  ))
  expect_equal(
    classes[spectrum_identity == "poly(ethylene:propylene:diene)", material],
    "epdm rubber (ethylene propylene diene monomer rubber)"
  )
  expect_equal(
    classes[spectrum_identity == "lahmian medium acrylic paint", material],
    "polyacrylates"
  )
  expect_true(all(
    classes[spectrum_identity %in% c("alkyd varnish", "alkyd_varnish"),
            material] == "polyesters"
  ))
  binder_classes <- c(
    "acrylic paint" = "polyacrylates",
    "acrylic-alkyd paint" = "polyesters",
    "acrylic-epoxy paint" = "polydiglycidyl ethers",
    "acrylic-urethane paint" = "polyurethanes",
    "alkyd paint" = "polyesters",
    "chlorosulfonated polyethylene paint" = "polyhaloolefins",
    "ethylene-acrylate paint" = "polyolefins",
    "nitrocellulose paint" = "polycellulose derivatives",
    "other paint" = "other plastic",
    "polyester paint" = "polyesters",
    "polyvinyl formal paint" = "polyvinylalcohols",
    "styrene-acrylic paint" = "polyacrylates",
    "urethane paint" = "polyurethanes",
    "urethane-alkyd paint" = "polyesters",
    "vinyl acetate paint" = "polyvinylesters",
    "vinyl chloride paint" = "polyhaloolefins",
    "vinyl chloride-vinyl acetate paint" = "polyhaloolefins",
    "vinyl-acrylic paint" = "polyacrylates"
  )
  actual_binders <- classes[
    spectrum_identity %in% names(binder_classes),
    stats::setNames(material, spectrum_identity)
  ]
  expect_identical(unname(actual_binders[names(binder_classes)]),
                   unname(binder_classes))
  paint_classes <- c("paint", "acrylic paint", "alkyd paint", "urethane paint")
  expect_false(any(hierarchy$material_class %in% paint_classes))
  expect_false(any(common_use$material_class %in% paint_classes))
  organic_recommendations <- c(
    "1,5-pentanediol", "11-aminoundecanoic acid", "1-bromobutane",
    "1-vinyl-2-pyrolidinone", "2-bromopropanoic acid", "2-butanone",
    "2-chloro-4-methylpentane", "2-methyl-2-pentanol", "butyl acrylate",
    "ethyl (a-chloromethyl)acrylate", "ethyl acrylate",
    "n,n-diethyl-m-toluamide", "n,n-dimethyl-m-toluamide",
    "n-butyl benzoate", "n-decyl methacrylate", "n-vinylformamide",
    "p-phenylenediamine", "p-vinylbenzyl chloride", "p-xylene",
    "propionic acid", "sec-butyl benzoate", "styrene",
    "tamoxifen (lot #bcbt3163)", "acrylamide", "benzoic acid",
    "benzoic acid (lot #058k0006)", "ethylene glycol", "phenol"
  )
  expect_true(all(
    classes[spectrum_identity %in% organic_recommendations, material] ==
      "organic matter"
  ))
  expect_true(all(
    classes[spectrum_identity %in% c(
      "c2. blue fiber", "c4. green fiber", "c6. red fiber",
      "c8. pink fiber bundle", "c9. grey fiber"
    ), material] == "other"
  ))
  expect_equal(
    classes[spectrum_identity == "fibre_polyamide_6_p6", material],
    "nylon 6,6 - poly(hexamethylene adipamide)"
  )
  expect_equal(
    classes[spectrum_identity == "ps 16. purple lego fragment", material],
    "acrylonitrile butadiene styrene (abs)"
  )
  expect_true(all(
    classes[spectrum_identity %in% c("polyesterurethane", "polyetherurethane"),
            material] == "polyurethanes"
  ))
  expect_true(all(
    classes[spectrum_identity %in% c("tylose", "tylose2"), material] ==
      "methyl cellulose"
  ))
  expect_true(all(c("microplastix", "nist", "hcmr", "cnr", "vliz",
                    "nicolas coca") %in% types$organization))
  expect_false("user_name" %in% names(types))
  expect_equal(types[organization == "nist", spectrum_type], "nir")
  expect_equal(
    types[organization == "monterey bay aquarium research institute",
          spectrum_type],
    "raman"
  )
  expect_equal(
    types[organization == "walters art museum pigment library",
          c(library_type, spectrum_type)],
    c("pigments", "raman")
  )
  expect_false(anyNA(types$spectrum_type))
  expect_false(any(types$spectrum_type == ""))
  expect_true(all(c("interpretation", "form_factor", "shape", "x_unit",
                    "spectrumid", "locationdescription", "v1",
                    "3997_91411", "polymer_hit_3_labs") %in%
                  drops$metadata_column))
})

test_that("reference metadata enrichment is ordered and conservative", {
  form_regex <- data.table::fread(
    reference_workflow_data_path("material_form_regex.csv")
  )
  common_use <- data.table::fread(
    reference_workflow_data_path("common_use_reference.csv")
  )
  metadata <- data.table::data.table(
    sample_name = paste0("s", seq_len(7)),
    spectrum_identity = c(
      "paint chip", "clear plastic film", "fiber pellet", "profile edge",
      "hard plastic", "EPDM gasket", "tyre fragment"
    ),
    material_form = c("rubber", rep(NA_character_, 6)),
    notes = c("paint", "sheet", "fiber and pellet", "filmography",
              "rigid plastic", "seal", "road wear"),
    material_class = c(
      "poly(styrene-butadiene) rubber", "polyethylene", "polypropylene",
      "other plastic", "polyethylene",
      "poly(ethylene-propylene-diene) rubber",
      "poly(styrene-butadiene) rubber"
    )
  )

  form <- OpenSpecy:::.lib_enrich_material_form(metadata, form_regex)
  expect_identical(
    form$data$material_form,
    c("rubber", "film plastic", NA, NA, "hard plastic", "rubber", NA)
  )
  expect_equal(form$summary[metric == "conflicting", value], 2L)
  expect_equal(form$summary[metric == "unmatched", value], 1L)
  expect_true(all(c("matched_patterns", "evidence", "conflict") %in%
                    names(form$matches)))
  expect_match(form$matches[row_id == 1L, evidence], "material_form=rubber")

  expect_silent(empty_form <- OpenSpecy:::.lib_enrich_material_form(
    data.table::data.table(spectrum_identity = "profile"), form_regex
  ))
  expect_identical(empty_form$summary$metric,
                   c("total", "populated", "unmatched", "conflicting"))

  use <- OpenSpecy:::.lib_enrich_common_use(form$data, common_use)
  expect_identical(
    use$data$common_use,
    c("mixed", "consumer", "consumer", NA, "consumer", "mixed", "mixed")
  )
  expect_true(all(c(
    "consumer_mass_share", "industrial_mass_share", "source_url",
    "retrieved_date"
  ) %in% names(use$coverage)))
  expect_silent(empty_use <- OpenSpecy:::.lib_enrich_common_use(
    data.table::data.table(material_class = "other plastic"), common_use
  ))
  expect_identical(empty_use$summary$metric,
                   c("total", "populated", "unmatched"))

  mixed_hierarchy <- data.table::data.table(
    material = "example", material_class = "example", material_type = "plastic"
  )
  mixed_reference <- data.table::data.table(
    material_class = "example", common_use = "mixed",
    consumer_mass_share = 0.4, industrial_mass_share = 0.4,
    unclassified_mass_share = 0.2, basis_year = 2020,
    geography = "global", source_url = "https://example.org/source",
    retrieved_date = "2026-09-29",
    evidence_notes = "review fixture"
  )
  expect_silent(OpenSpecy:::.lib_validate_common_use_reference(
    mixed_reference, mixed_hierarchy
  ))
  mixed_reference$consumer_mass_share <- 0.2
  expect_error(
    OpenSpecy:::.lib_validate_common_use_reference(
      mixed_reference, mixed_hierarchy
    ),
    "valid threshold shares"
  )
  qualitative_reference <- data.table::copy(mixed_reference)
  qualitative_reference[, `:=`(
    consumer_mass_share = NA_real_, industrial_mass_share = NA_real_,
    unclassified_mass_share = NA_real_, basis_year = NA_integer_,
    geography = "general"
  )]
  expect_silent(OpenSpecy:::.lib_validate_common_use_reference(
    qualitative_reference, mixed_hierarchy
  ))
  qualitative_reference$source_url <- NA_character_
  expect_error(
    OpenSpecy:::.lib_validate_common_use_reference(
      qualitative_reference, mixed_hierarchy
    ),
    "source provenance"
  )
  partial_reference <- data.table::copy(qualitative_reference)
  partial_reference$source_url <- "https://example.org/source"
  partial_reference$consumer_mass_share <- 0.6
  expect_error(
    OpenSpecy:::.lib_validate_common_use_reference(
      partial_reference, mixed_hierarchy
    ),
    "either all missing or all present"
  )
})

test_that("predict_class_reference() fills only blanks and audits overlaps", {
  classes <- data.table::data.table(
    spectrum_identity = c(
      "nylon exact override", "nylon fiber", "nylon blend", "unknown"
    ),
    material = c(
      "manual material", NA_character_, NA_character_, NA_character_
    )
  )
  rules <- data.table::data.table(
    pattern = c("^nylon", "blend$"),
    material = c("polyamides", "polyesters")
  )

  report <- predict_class_reference(classes, rules, return = "report")
  exact <- report$data

  expect_equal(exact[spectrum_identity == "nylon exact override", material],
               "manual material")
  expect_equal(exact[spectrum_identity == "nylon fiber", material],
               "polyamides")
  expect_true(is.na(exact[spectrum_identity == "nylon blend", material]))
  expect_true(is.na(exact[spectrum_identity == "unknown", material]))
  expect_equal(report$summary$predicted, 1L)
  expect_equal(report$summary$clashes, 1L)
  expect_equal(report$summary$unmatched, 2L)
  expect_equal(report$summary$overlaps, 1L)
  expect_equal(report$clashes$spectrum_identity, "nylon blend")
  expect_match(report$clashes$materials, "polyamides")
  expect_match(report$clashes$materials, "polyesters")
  expect_equal(report$overlaps$spectrum_identity, "nylon exact override")
  expect_false(report$overlaps$agreement)
  expect_equal(report$predictions$spectrum_identity, "nylon fiber")
})

test_that("reference class completion resolves reviewed wrappers and audits uncertainty", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    spectrum_identity = c(
      "known", "pa_ref.csv", "mffrc001_nylon (pa6)_maker.0",
      "cellulose_like_ref.csv", NA_character_, "known_2", "known_3",
      "known_4"
    ),
    user_name = c(
      NA_character_, "gicquel et al. 2024",
      "elise granek and kellie teague", "gicquel et al. 2024",
      NA_character_, NA_character_, NA_character_, NA_character_
    ),
    material = NA_character_,
    material_type = NA_character_
  )]
  lib$metadata$material_class <- c(
    "known class", rep(NA_character_, 3), rep("known class", 4)
  )
  classes <- data.table::data.table(
    spectrum_identity = c("pa", "nylon", "cellulose_like"),
    material = c("polyamides", "polyamides", "organic matter")
  )
  hierarchy <- data.table::data.table(
    material = c("nylon 6 - poly(caprolactam)", "organic matter"),
    material_class = c("polyamides", "organic matter"),
    material_type = c("plastic", "not plastic")
  )

  completed <- .lib_complete_reference_classes(lib, classes, hierarchy)
  report <- attr(completed, "class_coverage_report")

  expect_true(check_OpenSpecy(completed))
  expect_false(anyNA(completed$metadata$material_class))
  expect_equal(completed$metadata$material_class[2:3],
               rep("polyamides", 2))
  expect_equal(completed$metadata$class_lookup_key[2:3], c("pa", "nylon"))
  expect_equal(completed$metadata$class_assignment_reason[2:3],
               rep("reviewed_source_key", 2))
  expect_equal(completed$metadata$material_class[4], "organic matter")
  expect_equal(completed$metadata$class_assignment_reason[4],
               "reviewed_source_key")
  expect_identical(completed$metadata$spectrum_identity,
                   lib$metadata$spectrum_identity)
  expect_equal(report[stage == "after", populated_class], nrow(lib$metadata))
  expect_equal(report[stage == "after", reviewed_source_key], 3L)
  expect_equal(report[stage == "after", unclassified], 0L)
  expect_equal(report[stage == "after", other], 0L)
})

test_that("reference class completion labels and caps unresolved other", {
  source <- tiny_build_lib()
  ids <- paste0("coverage", seq_len(100L))
  spectra <- matrix(
    rep(source$spectra[, 1L], 100L), nrow = nrow(source$spectra)
  )
  colnames(spectra) <- ids
  metadata <- data.table::data.table(
    sample_name = ids, col_id = ids,
    spectrum_identity = c(rep("known", 99L), "unknown"),
    user_name = NA_character_,
    material = c(rep("known", 99L), NA_character_),
    material_class = c(rep("known class", 99L), NA_character_),
    material_type = c(rep("plastic", 99L), NA_character_),
    spectrum_type = "ftir"
  )
  lib <- as_OpenSpecy(source$wavenumber, spectra, metadata = metadata)
  classes <- data.table::data.table(
    spectrum_identity = character(), material = character()
  )
  hierarchy <- data.table::data.table(
    material = "known", material_class = "known class",
    material_type = "plastic"
  )

  completed <- .lib_complete_reference_classes(lib, classes, hierarchy)
  report <- attr(completed, "class_coverage_report")
  expect_equal(completed$metadata$material[100L], "other")
  expect_equal(completed$metadata$material_class[100L], "other")
  expect_equal(completed$metadata$material_type[100L], "other")
  expect_equal(report[stage == "after", unresolved_other], 1L)
  expect_equal(report[stage == "after", other_fraction], 0.01)

  lib$metadata[99:100, `:=`(
    spectrum_identity = c("unknown_2", "unknown"),
    material = NA_character_, material_class = NA_character_,
    material_type = NA_character_
  )]
  expect_error(
    .lib_complete_reference_classes(lib, classes, hierarchy),
    "above the reviewed 1% maximum",
    fixed = TRUE
  )
})

test_that("official other policy retains generic rows for correlation reassignment", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    spectrum_identity = c(NA_character_, paste0("identity_", 2:8)),
    sample_id = paste0("sample_", 1:8),
    file_name = paste0("file_", 1:8, ".csv"),
    organization = source,
    user_name = source,
    citation = "citation",
    other_info = "note",
    material = c("other", "other plastic", "polyamide", rep("polyester", 5)),
    material_type = c("other", "plastic", "other material", rep("plastic", 5))
  )]
  lib$metadata$material_class <- c(
    "other", "other plastic", "other material", rep("polyesters", 5)
  )

  removed <- OpenSpecy:::.lib_apply_other_policy(
    list(raw = lib), remove_other = TRUE, report = NULL
  )
  expect_identical(colnames(removed$libraries$raw$spectra), paste0("s", 1:8))
  expect_identical(nrow(removed$libraries$raw$metadata), 8L)
  expect_named(removed$review, names(OpenSpecy:::.lib_other_review_schema()))
  expect_named(removed$summary, names(OpenSpecy:::.lib_other_filter_schema()))
  expect_type(removed$review$source_row, "integer")
  expect_type(removed$summary$removed, "integer")
  expect_equal(removed$review$reason,
               c("missing_spectrum_identity", rep("reviewed_broad_category", 2)))
  expect_equal(
    removed$review$action,
    rep("retained_for_correlation_reassignment", 3)
  )
  expect_equal(removed$summary[, .(candidates, removed, after)],
               data.table::data.table(candidates = 3L, removed = 0L, after = 8L))
  expect_true(check_OpenSpecy(removed$libraries$raw))

  retained <- OpenSpecy:::.lib_apply_other_policy(
    list(raw = lib), remove_other = FALSE, report = NULL
  )
  expect_identical(ncol(retained$libraries$raw$spectra), 8L)
  expect_true(all(
    retained$review$action == "retained_for_correlation_reassignment"
  ))
  expect_identical(retained$summary$removed, 0L)
  expect_identical(formals(build_lib)$remove_other, TRUE)
})

test_that("official other policy quarantines unidentified nongeneric rows", {
  lib <- tiny_build_lib()
  lib$metadata[, `:=`(
    spectrum_identity = c(NA_character_, paste0("identity_", 2:8)),
    material = rep("polyamide", 8), material_class = rep("polyamides", 8),
    material_type = rep("plastic", 8), library_name = source
  )]

  result <- OpenSpecy:::.lib_apply_other_policy(
    list(raw = lib), remove_other = TRUE, report = NULL
  )
  bundle <- OpenSpecy:::.lib_quarantine_bundle(result$quarantine_parts)

  expect_identical(ncol(result$libraries$raw$spectra), 7L)
  expect_identical(bundle$manifest$removed_spectra, 1L)
  expect_identical(bundle$removals$quarantine_stage, "class_completion")
  expect_identical(bundle$removals$quarantine_reason,
                   "missing_spectrum_identity")
})

test_that("generic review finalization distinguishes every no-op disposition", {
  lib <- tiny_build_lib()
  libraries <- list(
    raw = filter_spec(lib, 1:2),
    derivative = filter_spec(lib, 3:4)
  )
  review <- data.table::data.table(
    artifact = c("raw", "derivative", "derivative", "derivative"),
    spectrum_id = c("s1", "s2", "s3", "s4"),
    action = "retained_for_correlation_reassignment"
  )
  attempted <- data.table::data.table(
    artifact = "derivative", spectrum_id = "s3",
    material_class = "other plastic", material_type = "plastic",
    matched_id = NA_character_, correlation = NA_real_, pool = "ftir_nir",
    action = "retained_no_finite_candidate"
  )

  finalized <- OpenSpecy:::.lib_finalize_other_review(
    review, libraries, prune_targets = "derivative", attempted = attempted
  )

  expect_identical(
    finalized$action,
    c(
      "retained_reassignment_not_attempted", "removed_before_reassignment",
      "retained_no_finite_candidate", "reassignment_audit_missing"
    )
  )
  expect_identical(finalized$match_pool[[3L]], "ftir_nir")
  expect_error(
    OpenSpecy:::.lib_finalize_other_review(
      review, libraries, prune_targets = "derivative",
      attempted = data.table::rbindlist(list(attempted, attempted))
    ),
    "duplicate artifact/spectrum"
  )
})

test_that("source-library retention reports complete drops with a stage reason", {
  stages <- list(
    data.table::data.table(
      stage = "prepared", artifact = "raw",
      library_name = c("kept", "dropped"), spectra = c(4L, 2L)
    ),
    data.table::data.table(
      stage = "post_exclusion", artifact = "raw",
      library_name = c("kept", "dropped"), spectra = c(4L, 2L)
    ),
    data.table::data.table(
      stage = "core", artifact = "raw",
      library_name = c("kept", "dropped"), spectra = c(4L, 2L)
    ),
    data.table::data.table(
      stage = "post_other", artifact = "raw",
      library_name = "kept", spectra = 4L
    ),
    data.table::data.table(
      stage = c("post_quality", "post_prune", "post_transform", "final"),
      artifact = "raw", library_name = "kept", spectra = 4L
    )
  )
  report <- OpenSpecy:::.lib_library_retention_report(stages)

  expect_equal(report[library_name == "kept", status], "retained")
  expect_equal(report[library_name == "dropped", status], "dropped")
  expect_equal(
    report[library_name == "dropped", reason],
    "unresolved_other_policy"
  )
  expect_equal(report[library_name == "dropped", final_n], 0L)
})

test_that("confusion tables rank the largest misidentifications first", {
  tests <- data.table::data.table(
    algorithm = "logistic_regression", artifact = "derivative",
    model = "raman", source = "new",
    technique = "raman", provenance = "candidate",
    expected_class = c(rep("polyethylene", 10), rep("polypropylene", 4)),
    predicted_class = c(rep("polyethylene", 5), rep("polypropylene", 5),
                        rep("polypropylene", 3), "polyethylene")
  )
  confusion <- OpenSpecy:::.lib_confusion_table(tests)
  expect_named(confusion, c(
    "algorithm", "artifact", "model", "source", "technique", "provenance",
    "expected_class", "predicted_class", "spectra", "misidentified",
    "expected_class_spectra", "expected_class_fraction"
  ), ignore.order = TRUE)
  expect_true(confusion$misidentified[[1L]])
  expect_equal(confusion$spectra[[1L]], 5L)
  expect_equal(
    confusion[expected_class == "polyethylene", expected_class_fraction],
    c(0.5, 0.5)
  )
  expect_type(confusion$expected_class_fraction, "double")
  expect_named(OpenSpecy:::.lib_confusion_table(tests[0]), names(confusion))
})

test_that("model assessment compares error-mode accuracy percentages", {
  tests <- data.table::data.table(
    algorithm = "logistic_regression", artifact = "derivative",
    model = "raman", source = "new", technique = "raman",
    spectrum_id = paste0("s", 1:8),
    correct = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  flags <- data.table::data.table(
    artifact = "derivative", technique = "raman",
    spectrum_id = paste0("s", 5:8), error_mode = "low_snr"
  )
  result <- OpenSpecy:::.lib_model_assessment_correlations(tests, flags)

  expect_named(
    result, names(OpenSpecy:::.lib_model_assessment_correlation_schema())
  )
  expect_identical(result$error_mode[[1L]], "low_snr")
  expect_equal(result$with_error_accuracy_pct, 0)
  expect_equal(result$without_error_accuracy_pct, 100)
  expect_equal(result$accuracy_difference_pct, -100)

  empty <- OpenSpecy:::.lib_model_assessment_correlations(tests[0], flags)
  expect_named(empty, names(result))
  expect_type(empty$with_error_accuracy_pct, "double")
})

test_that("reference metadata is ordered by missingness after all-NA removal", {
  x <- tiny_build_lib()
  x$metadata <- data.table::data.table(
    sample_name = colnames(x$spectra),
    complete = seq_len(ncol(x$spectra)),
    two_missing = c(NA, NA, rep("x", ncol(x$spectra) - 2L)),
    one_missing = c(NA, rep("y", ncol(x$spectra) - 1L)),
    entirely_missing = rep(NA_character_, ncol(x$spectra))
  )
  attr(x, "metadata_order_marker") <- "preserved"

  finalized <- OpenSpecy:::.lib_finalize_reference_metadata(
    libraries = list(raw = list(ftir = x)),
    medoids = list(derivative = list(ftir = x))
  )
  expected <- c("sample_name", "complete", "one_missing", "two_missing")
  expect_identical(names(finalized$libraries$raw$ftir$metadata), expected)
  expect_identical(names(finalized$medoids$derivative$ftir$metadata), expected)
  expect_true(check_OpenSpecy(finalized$libraries$raw$ftir))
  expect_identical(
    attr(finalized$libraries$raw$ftir, "metadata_order_marker"), "preserved"
  )
  expect_equal(finalized$assessment$dropped_columns, c(1L, 1L))
  expect_equal(finalized$assessment$dropped_names,
               rep("entirely_missing", 2L))
  expect_equal(finalized$assessment$maximum_missing, c(2L, 2L))
  expect_named(
    finalized$assessment,
    names(OpenSpecy:::.lib_metadata_finalization_schema())
  )
})

test_that("build_lib() preserves full source ranges through NA-aware recipes", {
  lib <- tiny_build_lib()
  left <- lib
  left$wavenumber <- lib$wavenumber[1:40]
  left$spectra <- lib$spectra[1:40, 1:4, drop = FALSE]
  left$metadata <- data.table::copy(lib$metadata[1:4])
  right <- lib
  right$wavenumber <- lib$wavenumber[22:61]
  right$spectra <- lib$spectra[22:61, 5:8, drop = FALSE]
  right$metadata <- data.table::copy(lib$metadata[5:8])

  built <- build_lib(list(left, right), dedupe = FALSE, signal_noise = FALSE)

  expect_true(all(diff(built$raw$wavenumber) == 6))
  expect_true(anyNA(built$raw$spectra))
  expect_true(anyNA(built$derivative$spectra))
  expect_true(any(is.finite(built$derivative$spectra[, 1])))
  expect_true(any(is.finite(built$nobaseline$spectra[, 8])))
})

test_that("streamed variable-axis paths match c_spec() interpolation", {
  lib <- tiny_build_lib()
  left <- filter_spec(lib, 1:4)
  left$wavenumber <- lib$wavenumber[1:40]
  left$spectra <- left$spectra[1:40, , drop = FALSE]
  right <- filter_spec(lib, 5:8)
  right$wavenumber <- lib$wavenumber[22:61]
  right$spectra <- right$spectra[22:61, , drop = FALSE]
  paths <- c(tempfile(fileext = ".rds"), tempfile(fileext = ".rds"))
  saveRDS(left, paths[[1L]])
  saveRDS(right, paths[[2L]])

  expected <- c_spec(list(left, right), range = "full", res = 6)
  actual <- build_lib(
    paths,
    recipes = list(raw = list()),
    range = "full",
    res = 6,
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE,
    progress = FALSE
  )$raw

  expect_equal(actual$wavenumber, expected$wavenumber)
  expect_equal(actual$spectra, expected$spectra, tolerance = 1e-12)
  expect_equal(actual$metadata$sample_name, expected$metadata$sample_name)
  expect_true(check_OpenSpecy(actual))
})

test_that("source-stage hashes support spectra with no shared finite rows", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  midpoint <- floor(nrow(lib$spectra) / 2)
  lib$spectra[seq_len(midpoint), 1] <- NA_real_
  lib$spectra[seq.int(midpoint + 1L, nrow(lib$spectra)), 2] <- NA_real_

  built <- build_lib(
    lib, recipes = list(raw = list()), signal_noise = FALSE,
    progress = FALSE
  )$raw
  expect_true(check_OpenSpecy(built))
  expect_equal(ncol(built$spectra), 2L)
  expect_false(anyNA(built$metadata$sample_name))
  expect_identical(colnames(built$spectra), built$metadata$sample_name)
})

test_that("build_lib() applies baseline recipes across source-specific NA tails", {
  lib <- filter_spec(tiny_build_lib(), 1:2)
  lib$spectra[1:5, 1] <- NA_real_
  lib$spectra[57:61, 2] <- NA_real_

  built <- build_lib(
    list(lib),
    recipes = list(nobaseline = list(
      conform_spec = FALSE,
      smooth_intens = FALSE,
      subtr_baseline = TRUE,
      make_rel = TRUE
    )),
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )$nobaseline
  expected <- manage_na(lib, fun = subtr_baseline)

  expect_equal(built$spectra, expected$spectra, tolerance = 1e-12)
  expect_equal(attr(built, "baseline"), "nobaseline")
})

test_that("extdata files combine into a mini library", {
  mini_files <- c(
    read_extdata("raman_hdpe.csv"),
    read_extdata("ftir_ldpe_soil.asp"),
    read_extdata("raman_atacamit.spc")
  )

  mini <- read_any(mini_files, c_spec_args = list(range = "common", res = 10))
  expect_true(check_OpenSpecy(mini))
  expect_equal(ncol(mini$spectra), 3)

  lookup <- data.table::data.table(
    file_name = basename(mini_files),
    material = c("hdpe", "ldpe in soil", "atacamite"),
    material_type = c("plastic", "plastic", "mineral")
  )
  mini <- join_lib_metadata(mini, lookup, by = "file_name",
                            require_complete = TRUE)
  built <- build_lib(
    mini_files,
    recipes = list(raw = list()),
    metadata_lookups = lookup,
    dedupe = FALSE,
    convert_intensity = FALSE,
    signal_noise = FALSE
  )
  expect_true(check_OpenSpecy(built$raw))
  expect_true(all(c("material", "material_type") %in% names(built$raw$metadata)))
})

test_that("build_lib() discovers helper data and reuses one artifact bundle", {
  # This exercises the complete multi-artifact reference-library workflow.
  skip_on_cran()

  lib <- tiny_build_lib()
  lib$spectra[30, ] <- lib$spectra[30, ] + 10
  lib$metadata[, `:=`(
    spectrum_identity = label,
    organization = source,
    user_name = source,
    spectrum_id = sample_name
  )]
  spectra <- do.call(cbind, lapply(seq_len(10), function(copy) {
    out <- lib$spectra + copy / 1000
    colnames(out) <- paste0(colnames(out), "_", copy)
    out
  }))
  metadata <- data.table::rbindlist(lapply(seq_len(10), function(copy) {
    out <- data.table::copy(lib$metadata)
    out[, `:=`(
      sample_name = paste0(sample_name, "_", copy),
      spectrum_id = paste0(spectrum_id, "_", copy)
    )]
    out
  }))
  lib <- as_OpenSpecy(
    lib$wavenumber, spectra = spectra, metadata = metadata,
    attributes = list(intensity_unit = "absorbance")
  )
  workflow_root <- file.path(
    tempdir(), paste0("workflow-", sample.int(1e8, 1))
  )
  workflow_data <- file.path(workflow_root, "data")
  output_dir <- file.path(tempdir(), paste0("output-", sample.int(1e8, 1)))
  dir.create(workflow_data, recursive = TRUE)
  old_working_dir <- setwd(workflow_root)
  on.exit(setwd(old_working_dir), add = TRUE)
  data.table::fwrite(data.table::data.table(
    spectrum_identity = unique(lib$metadata$spectrum_identity),
    material = paste0("material_", seq_along(unique(
      lib$metadata$spectrum_identity
    )))
  ), file.path(workflow_data, "classes_reference.csv"))
  data.table::fwrite(data.table::data.table(
    pattern = "^never[0-9]+$", material = "other material"
  ), file.path(workflow_data, "classes_regex.csv"))
  data.table::fwrite(data.table::data.table(
    organization = c("a", "b", "c"), library_type = "test",
    spectrum_type = "ftir"
  ), file.path(workflow_data, "library_types.csv"))
  classes <- data.table::fread(
    file.path(workflow_data, "classes_reference.csv")
  )
  data.table::fwrite(data.table::data.table(
    material = classes$material,
    material_class = rep(c("polyclass_a", "polyclass_b"),
                         length.out = nrow(classes)),
    material_type = "plastic"
  ), file.path(workflow_data, "material_hierarchy.csv"))
  data.table::fwrite(data.table::data.table(
    material_class = c("polyclass_a", "polyclass_b"),
    material_type = "plastic", material_group = "fixture polymer"
  ), file.path(workflow_data, "material_group_reference.csv"))
  data.table::fwrite(data.table::data.table(sample_name = character()),
                     file.path(workflow_data, "known_bad_ids.csv"))
  data.table::fwrite(data.table::data.table(
    metadata_column = "unused_legacy_column"
  ), file.path(workflow_data, "metadata_drop_columns.csv"))
  data.table::fwrite(data.table::data.table(
    pattern = "(^|[^[:alnum:]])paint([^[:alnum:]]|$)",
    material_form = "paint"
  ), file.path(workflow_data, "material_form_regex.csv"))
  use_classes <- sort(unique(data.table::fread(
    file.path(workflow_data, "material_hierarchy.csv")
  )$material_class))
  data.table::fwrite(data.table::data.table(
    material_class = use_classes,
    common_use = rep(NA_character_, length(use_classes)),
    consumer_mass_share = rep(NA_real_, length(use_classes)),
    industrial_mass_share = rep(NA_real_, length(use_classes)),
    unclassified_mass_share = rep(NA_real_, length(use_classes)),
    basis_year = rep(NA_integer_, length(use_classes)),
    geography = rep(NA_character_, length(use_classes)),
    source_url = rep(NA_character_, length(use_classes)),
    retrieved_date = rep(NA_character_, length(use_classes)),
    evidence_notes = rep(NA_character_, length(use_classes))
  ), file.path(workflow_data, "common_use_reference.csv"))
  fixture_prune <- list(
    derivative = list(min_n = 10, progress = FALSE),
    nobaseline = list(min_n = 10, progress = FALSE)
  )

  first <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE,
    prune = fixture_prune,
    recipes = list(raw = list(), derivative = list(), nobaseline = list())
  ))
  expect_named(first, c("libraries", "medoids", "models", "assessments"))
  expect_named(first$libraries, c("raw", "derivative", "nobaseline"))
  expect_named(first$medoids, c("derivative", "nobaseline"))
  expect_named(
    first$models,
    "gated_hierarchical_logistic_regression"
  )
  expect_true(all(vapply(first$libraries, function(recipe) {
    all(vapply(recipe, check_OpenSpecy, logical(1)))
  }, logical(1))))
  expect_true(all(vapply(first$medoids, function(recipe) {
    all(vapply(recipe, check_OpenSpecy, logical(1)))
  }, logical(1))))
  expect_named(first$assessments,
               c("cleanup", "ref_lib", "medoid", "model", "functionality"))
  expect_lte(sum(lengths(first$assessments)), 10L)
  expect_true(all(lengths(first$assessments) > 0L))
  expect_true(all(vapply(
    unlist(first$assessments, recursive = FALSE), nrow, integer(1L)
  ) > 0L))
  expect_identical(attr(first$assessments, "assessment_schema_version"),
                   "2.0.0")
  expect_named(first$assessments$cleanup$dropped_spectrum_identities,
               "spectrum_identity")
  retention <- first$assessments$cleanup$summary[
    assessment_kind == "library_retention"
  ]
  expect_gt(nrow(retention), 0L)
  expect_true(all(c("library_name", "prepared_n", "final_n", "status",
                    "reason") %in% names(retention)))
  expect_true(all(vapply(first$libraries$raw, function(object) {
    all(c("library_name", "material_form", "common_use") %in%
          names(object$metadata))
  }, logical(1))))
  expect_true(all(vapply(first$libraries$raw, function(object) {
    is.list(attr(object, "material_form_enrichment_report", exact = TRUE)) &&
      is.list(attr(object, "common_use_enrichment_report", exact = TRUE))
  }, logical(1))))
  upstream <- attr(first$assessments, "upstream_assessments", exact = TRUE)
  expect_true(all(c(
    "material_form_enrichment", "material_form_matches",
    "material_form_clashes", "common_use_enrichment", "common_use_coverage",
    "pruning_removals"
  ) %in% names(upstream)))
  release_dir <- attr(first, "output_dir")
  expect_true(all(file.exists(file.path(
    release_dir,
    c("raw.rds", "derivative.rds", "nobaseline.rds",
      "medoid_derivative.rds", "medoid_nobaseline.rds",
      "model_gated_hierarchical_logistic_regression_derivative.rds",
      "model_gated_hierarchical_logistic_regression_nobaseline.rds",
      "assessments.rds", "quarantined_spectra.rds",
      "reference_library_build.rds")
  ))))
  quarantine <- readRDS(file.path(release_dir, "quarantined_spectra.rds"))
  expect_s3_class(quarantine, "OpenSpecyQuarantine")
  expect_identical(quarantine$manifest$schema,
                   "OpenSpecy_quarantined_spectra_v2")
  expect_identical(
    quarantine$manifest$removed_spectra,
    as.integer(nrow(quarantine$removals))
  )
  expect_true(OpenSpecy:::.lib_validate_quarantine(quarantine))
  release_index <- readRDS(file.path(release_dir, "reference_library_build.rds"))
  expect_identical(
    release_index$schema, "OpenSpecy_reference_build_index_v1"
  )
  expect_false(any(c("libraries", "medoids", "models") %in%
                     names(release_index)))
  expect_lt(file.info(file.path(
    release_dir, "reference_library_build.rds"
  ))$size, 1e6)

  release_assessments <- readRDS(file.path(release_dir, "assessments.rds"))
  expect_identical(
    attr(release_assessments, "assessment_schema_version"), "2.0.0"
  )
  expect_false(any(c("scope", "expected_class") %in%
                     names(release_assessments$ref_lib$accuracy)))
  expect_true("model_training_tests" %in%
                names(attr(release_assessments, "evidence")))

  report_attributes <- c(
    "join_report", "metadata_lookup_reports",
    "spectrum_identity_cleanup_report", "build_stage_report",
    "dropped_spectrum_identities", "class_prediction_report",
    "class_coverage_report", "other_review_report", "other_filter_report",
    "quality_control_report", "prune_report", "identification_support",
    "identification_dropped_ids", "identification_minimum_observed",
    "identification_finite_coverage", "range_flat_drops",
    "library_retention_stages"
  )
  for (component in c("raw", "derivative", "nobaseline",
                      "medoid_derivative", "medoid_nobaseline")) {
    artifact <- readRDS(file.path(release_dir, paste0(component, ".rds")))
    for (object in artifact) {
      expect_true(check_OpenSpecy(object))
      expect_length(intersect(names(attributes(object)), report_attributes), 0L)
      expect_null(attr(object$metadata, "join_report", exact = TRUE))
    }
  }
  model_fields <- OpenSpecy:::.lib_runtime_model_fields()
  for (component in c(
    "model_gated_hierarchical_logistic_regression_derivative",
    "model_gated_hierarchical_logistic_regression_nobaseline"
  )) {
    artifact <- readRDS(file.path(release_dir, paste0(component, ".rds")))
    for (model in artifact) {
      expect_true(all(names(model) %in% model_fields))
      expect_false(any(c(
        "tests", "lambda_metrics", "oob_metrics", "oob_class_accuracy",
        "feature_importance", "training_parameters", "class_weights",
        "class_metrics", "hierarchy_metrics", "source_metrics",
        "node_support", "fold_assignments",
        "support", "class_support"
      ) %in% names(model)))
      expect_null(attr(model, "training_warnings", exact = TRUE))
    }
  }

  resolved <- OpenSpecy:::.lib_resolve_rebuild_input(
    file.path(release_dir, "reference_library_build.rds")
  )
  expect_named(resolved$libraries, c("raw", "derivative", "nobaseline"))
  expect_named(resolved$medoids, c("derivative", "nobaseline"))
  expect_named(
    resolved$models,
    "gated_hierarchical_logistic_regression"
  )
  expect_s3_class(resolved$quarantine, "OpenSpecyQuarantine")
  expect_identical(
    resolved$quarantine$manifest$schema,
    "OpenSpecy_quarantined_spectra_v2"
  )

  second <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE, reuse = TRUE,
    prune = fixture_prune,
    recipes = list(raw = list(), derivative = list(), nobaseline = list())
  ))
  second_evidence <- attr(second$assessments, "evidence")
  expect_true(all(
    second_evidence$output_manifest$status == "available"
  ))
  expect_equal(second$libraries$raw$ftir$spectra,
               first$libraries$raw$ftir$spectra)

  rebuilt <- suppressWarnings(build_lib(
    lib, output_dir = output_dir,
    previous_library_dir = NULL, dedupe = FALSE, signal_noise = FALSE,
    progress = FALSE, reuse = FALSE,
    model_method = "logistic_regression",
    prune = fixture_prune,
    recipes = list(raw = list(), derivative = list(), nobaseline = list())
  ))
  expect_named(rebuilt$assessments,
               c("cleanup", "ref_lib", "medoid", "model", "functionality"))
  expect_named(rebuilt$models, "logistic_regression")
  flat_release_dir <- attr(rebuilt, "output_dir")
  expect_false(identical(flat_release_dir, release_dir))
  expect_true(all(file.exists(file.path(
    flat_release_dir,
    c("model_logistic_regression_derivative.rds",
      "model_logistic_regression_nobaseline.rds",
      "model_derivative.rds", "model_nobaseline.rds")
  ))))
})

test_that("source-local reference splits prevent self-match leakage", {
  old <- tiny_build_lib()
  new <- old
  new$metadata$sample_name_old <- new$metadata$sample_name
  new$metadata$sample_name <- paste0("rebuilt_", new$metadata$sample_name)
  colnames(new$spectra) <- new$metadata$sample_name
  split <- OpenSpecy:::.lib_source_split(
    new, artifact = "raw", source = "new", seed = 71, holdout = 0.25
  )
  expect_identical(anyDuplicated(split$manifest$group_id), 0L)
  expect_true(all(split$manifest$source == "new"))
  expect_true(all(split$manifest$split %in% c("train", "test")))
  expect_equal(
    sum(split$manifest$split == "test"),
    round(nrow(split$manifest) * 0.25)
  )
  expect_length(intersect(
    split$manifest[split == "train", group_id],
    split$manifest[split == "test", group_id]
  ), 0L)

  messages <- capture.output(
    tests <- OpenSpecy:::.lib_reference_holdout_test(
      reference = new, data = new, split = split,
      artifact = "raw", source = "new", progress = TRUE
    ),
    type = "message"
  )
  expect_match(paste(messages, collapse = "\n"), "full correlation complete")
  expect_true(all(tests$split == "test"))
  expect_true(all(tests$provenance == "source_local_reference_holdout"))

  old_split <- OpenSpecy:::.lib_source_split(
    old, artifact = "raw", source = "old", seed = 72, holdout = 0.25
  )
  old_tests <- OpenSpecy:::.lib_reference_holdout_test(
    reference = old, data = old, split = old_split,
    artifact = "raw", source = "old", progress = FALSE
  )
  expect_true(all(old_split$manifest$source == "old"))
  expect_true(all(old_tests$source == "old"))
})

test_that("medoid assessment searches the complete corresponding library", {
  lib <- tiny_build_lib()
  medoid <- filter_spec(lib, c(1L, 3L))
  messages <- capture.output(
    tests <- OpenSpecy:::.lib_reference_complete_test(
      medoid, lib, artifact = "medoid_derivative_ftir", source = "new",
      progress = TRUE, block_size = 2L
    ),
    type = "message"
  )
  expect_match(paste(messages, collapse = "\n"),
               "medoid identifying complete dataset")
  expect_equal(nrow(tests), ncol(lib$spectra))
  expect_setequal(tests$spectrum_id, lib$metadata$sample_name)
  expect_true(all(tests$split == "complete"))
  expect_true(all(tests$provenance == "complete_medoid_reference_search"))
})

test_that("complete old-new assessments directly test deployed models", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("ranger")
  small <- tiny_build_lib()
  spectra <- do.call(cbind, lapply(seq_len(5), function(i) {
    small$spectra + i / 1000
  }))
  metadata <- data.table::rbindlist(lapply(seq_len(5), function(i) {
    out <- data.table::copy(small$metadata)
    out$sample_name <- paste0(out$sample_name, "_", i)
    out
  }))
  colnames(spectra) <- metadata$sample_name
  lib <- as_OpenSpecy(
    small$wavenumber, spectra = spectra, metadata = metadata,
    attributes = list(intensity_unit = "absorbance")
  )
  model_input <- restrict_range(
    lib, min = 800, max = 3200, make_rel = FALSE
  )
  model <- suppressWarnings(build_model_lib(model_input))
  rf_model <- train_spec_model(
    model_input, method = "random_forest", num.trees = 20L,
    min.node.size = 1L, mtry = 4L
  )
  model_set <- list(both = model, ftir = model, raman = NULL)
  build <- list(
    libraries = list(
      raw = list(ftir = lib), derivative = list(ftir = lib),
      nobaseline = list(ftir = lib)
    ),
    medoids = list(
      derivative = list(ftir = model_input),
      nobaseline = list(ftir = model_input)
    ),
    models = list(
      logistic_regression = list(
        derivative = model_set, nobaseline = model_set
      ),
      random_forest = list(raw = list(ftir = rf_model))
    ),
    assessments = list()
  )
  previous <- file.path(tempdir(), paste0("previous-", sample.int(1e8, 1)))
  dir.create(previous, recursive = TRUE)
  typed_lib <- list(ftir = lib)
  typed_medoid <- list(ftir = model_input)
  saveRDS(typed_lib, file.path(previous, "raw.rds"))
  saveRDS(typed_lib, file.path(previous, "derivative.rds"))
  saveRDS(typed_lib, file.path(previous, "nobaseline.rds"))
  saveRDS(typed_medoid, file.path(previous, "medoid_derivative.rds"))
  saveRDS(typed_medoid, file.path(previous, "medoid_nobaseline.rds"))
  saveRDS(model_set, file.path(previous, "model_derivative.rds"))
  saveRDS(model_set, file.path(previous, "model_nobaseline.rds"))

  local_mocked_bindings(
    train_spec_model = function(...) {
      stop("assessment must not retrain a model", call. = FALSE)
    },
    .package = "OpenSpecy"
  )

  comparison <- suppressWarnings(OpenSpecy:::.lib_compare_reference_build(
    build, previous_library_dir = previous,
    seed = 211, holdout = 0.25, progress = FALSE
  ))
  expect_true(all(c(
    "models", "split_manifest", "library_identification",
    "model_identification", "model_assessment_correlations",
    "assess_spec_shifts", "old_new_compatibility"
  ) %in% names(comparison)))
  expect_equal(unique(comparison$split_manifest$artifact), c(
    "raw_ftir", "derivative_ftir", "nobaseline_ftir"
  ))
  expect_setequal(unique(comparison$split_manifest$source), c("new", "old"))
  expect_gt(nrow(comparison$library_identification), 0L)
  expect_gt(nrow(comparison$model_identification), 0L)
  expect_setequal(
    unique(comparison$model_identification$algorithm),
    c("logistic_regression", "random_forest")
  )
  expect_equal(nrow(comparison$model_assessment_correlations), 0L)
  expect_gt(nrow(comparison$assess_spec_shifts), 0L)
  expect_equal(
    comparison$library_tests[
      artifact == "medoid_derivative_ftir" & source == "new", .N
    ],
    ncol(lib$spectra)
  )
  expect_true(all(comparison$library_tests[
    grepl("^medoid_", artifact), split
  ] == "complete"))
  expect_true(all(
    comparison$models$logistic_regression$derivative$ftir$tests$provenance ==
      "new_logistic_regression_complete_dataset"
  ))
  expect_equal(
    nrow(comparison$models$logistic_regression$derivative$ftir$tests),
    ncol(lib$spectra)
  )
  expect_true(all(
    comparison$models$random_forest$raw$ftir$tests$provenance ==
      "new_random_forest_complete_dataset"
  ))
  expect_equal(
    comparison$models$random_forest$raw$ftir$observation_count,
    ncol(model_input$spectra)
  )
})

test_that("assessment review is process nested, compact, and nonsparse", {
  identification <- data.table::data.table(
    artifact = rep(c("raw_ftir", "medoid_derivative_ftir"), each = 2L),
    source = rep(c("old", "new"), 2L), technique = "ftir",
    provenance = "grouped", macro_class_accuracy = c(.5, .8, .9, .7),
    coverage = 1, overall_accuracy = c(.6, .85, .91, .75),
    spectra = 20L, evaluated = 20L, classes = 2L,
    evaluated_classes = 2L, mean_score = .8
  )
  confusion <- data.table::data.table(
    artifact = rep("raw_ftir", 4L), source = rep(c("old", "new"), 2L),
    technique = "ftir", provenance = "grouped",
    expected_class = c("a", "a", "b", "b"),
    predicted_class = c("b", "b", "a", "a"), misidentified = TRUE,
    spectra = c(2L, 7L, 1L, 4L), expected_class_spectra = 10L,
    expected_class_fraction = c(.2, .7, .1, .4)
  )
  model_identification <- data.table::copy(identification[artifact == "raw_ftir"])
  model_identification[, `:=`(
    algorithm = "logistic_regression", artifact = "derivative",
    model = "ftir"
  )]
  correlations <- data.table::data.table(
    algorithm = "logistic_regression", artifact = "derivative",
    model = "ftir", technique = "ftir", error_mode = "low_snr",
    with_error_accuracy_pct = 50,
    without_error_accuracy_pct = 90,
    accuracy_difference_pct = -40
  )
  reviewed <- OpenSpecy:::.lib_assessment_review(list(
    build_summary = data.table::data.table(artifact = "raw_ftir", spectra = 20L),
    dropped_spectrum_identities = data.table::data.table(
      spectrum_identity = c("z", "a", "z")
    ),
    library_identification = identification,
    library_class_accuracy = data.table::data.table(
      artifact = "raw_ftir", source = "new", technique = "ftir",
      provenance = "grouped", expected_class = "class_a",
      spectra = 10L, evaluated = 10L, coverage = 1,
      class_accuracy = 0.5
    ),
    library_confusion = confusion,
    model_identification = model_identification,
    model_class_accuracy = data.table::data.table(
      algorithm = "logistic_regression", artifact = "derivative",
      model = "ftir", source = "new", technique = "ftir",
      provenance = "grouped", expected_class = "class_a",
      spectra = 10L, evaluated = 10L, coverage = 1,
      class_accuracy = 0.5
    ),
    model_confusion = data.table::copy(confusion)[, `:=`(
      algorithm = "logistic_regression", artifact = "derivative", model = "ftir"
    )],
    model_assessment_correlations = correlations,
    assess_spec_shifts = data.table::data.table(
      artifact = "raw_ftir", check = "signal", rate_old = .2,
      rate_new = .1, rate_shift = -.1
    ),
    old_new_compatibility = data.table::data.table(
      artifact = "raw_ftir", spectra_old = 10L, spectra_new = 20L,
      spectra_shift = 10L
    ),
    output_manifest = data.table::data.table(
      component = "raw", status = "available", path = "raw.rds", size = 1
    )
  ))

  expect_named(reviewed,
               c("cleanup", "ref_lib", "medoid", "model", "functionality"))
  expect_lte(sum(lengths(reviewed)), 10L)
  expect_true(all(lengths(reviewed) > 0L))
  expect_identical(
    reviewed$cleanup$dropped_spectrum_identities$spectrum_identity,
    c("a", "z")
  )
  accuracy_names <- names(reviewed$ref_lib$accuracy)
  expect_false(any(c("scope", "expected_class", "class_accuracy") %in%
                     accuracy_names))
  expect_true(all(c("source", "overall_accuracy_pct",
                    "macro_class_accuracy_pct", "class_count") %in%
                    accuracy_names))
  expect_false(any(c("coverage_pct", "mean_score",
                     "evaluated_classes") %in% accuracy_names))
  expect_identical(reviewed$ref_lib$accuracy$source, c("old", "new"))
  expect_equal(reviewed$ref_lib$accuracy$macro_class_accuracy_pct, c(50, 80))
  expect_identical(reviewed$ref_lib$accuracy$class_count, c(2L, 2L))
  expect_identical(
    reviewed$model$error_mode_accuracy$accuracy_difference_pct, -40
  )
  sparse <- unlist(lapply(
    unlist(reviewed, recursive = FALSE),
    function(value) vapply(value, function(column) mean(is.na(column)),
                           numeric(1L))
  ))
  expect_true(all(sparse <= 0.10))
})

test_that("assessment shifts merge findings, omit passes, and rank rate shifts", {
  summary <- data.table::data.table(
    artifact = rep("raw_ftir", 6L), source = rep(c("old", "new"), each = 3L),
    check = rep(c("signal", "signal", "range"), 2L),
    status = rep(c("pass", "warning", "error"), 2L),
    count = c(90L, 5L, 1L, 80L, 10L, 4L),
    finding_count = c(0L, 5L, 1L, 0L, 10L, 4L),
    example_ids = c("", "a", "b", "", "c", "d"), spectra = 100L
  )
  shifts <- OpenSpecy:::.lib_assessment_shift_table(summary)
  expect_false("status" %in% names(shifts))
  expect_equal(shifts[check == "signal", count_old], 5L)
  expect_equal(shifts[check == "signal", count_new], 10L)
  expect_true(all(diff(shifts$rate_shift) <= 0))
})

test_that("stable groups connect physical IDs and exact spectral content", {
  x <- tiny_build_lib()
  x$metadata$sample_name_old <- paste0("legacy_", seq_len(nrow(x$metadata)))
  x$metadata$sample_name_old[2] <- x$metadata$sample_name_old[1]
  x$spectra[, 4] <- x$spectra[, 3]
  groups <- OpenSpecy:::.lib_stable_group_info(x)
  expect_equal(groups$group_id[1], groups$group_id[2])
  expect_equal(groups$group_id[3], groups$group_id[4])
  split <- OpenSpecy:::.lib_source_split(
    x, artifact = "raw", source = "new", seed = 1, holdout = .25
  )
  expect_identical(
    split$manifest[, data.table::uniqueN(split), by = group_id][, max(V1)],
    1L
  )
})

test_that("immutable promotion rejects a changed payload", {
  path <- tempfile(fileext = ".rds")
  first <- OpenSpecy:::.lib_promote_rds(list(value = 1L), path)
  expect_equal(first$status, "promoted")
  second <- OpenSpecy:::.lib_promote_rds(list(value = 1L), path)
  expect_equal(second$status, "verified_existing")

  semantic_path <- tempfile(fileext = ".rds")
  saveRDS(list(value = 1L), semantic_path, compress = FALSE)
  semantic_checksum <- OpenSpecy:::.lib_sha256_file(semantic_path)
  semantic <- OpenSpecy:::.lib_promote_rds(
    list(value = 1L), semantic_path
  )
  expect_equal(semantic$status, "verified_existing")
  expect_identical(
    OpenSpecy:::.lib_sha256_file(semantic_path), semantic_checksum
  )
  expect_error(
    OpenSpecy:::.lib_promote_rds(list(value = 2L), path),
    "Immutable release artifact differs"
  )
})

test_that("completed release manifests authorize immutable byte reuse", {
  path <- tempfile(fileext = ".rds")
  manifest <- OpenSpecy:::.lib_promote_rds(list(value = 1L), path)
  attr(manifest, "build_signature") <- "signed-build"

  verified <- OpenSpecy:::.lib_verify_manifested_release_artifact(
    path, manifest
  )
  expect_equal(verified$status, "verified_existing")
  expect_identical(verified$checksum, manifest$checksum)

  saveRDS(list(value = 2L), path)
  expect_null(OpenSpecy:::.lib_verify_manifested_release_artifact(
    path, manifest
  ))
})

test_that("file signature caching preserves hashes and invalidates on metadata", {
  path <- tempfile(fileext = ".txt")
  writeLines("first", path)
  first <- OpenSpecy:::.lib_file_signatures(path)
  second <- OpenSpecy:::.lib_file_signatures(path)
  expect_identical(first$checksum, second$checksum)
  writeLines("a different sized payload", path)
  changed <- OpenSpecy:::.lib_file_signatures(path)
  expect_false(identical(first$checksum, changed$checksum))
})

test_that("support and fill helpers use the intended axes", {
  x <- tiny_build_lib()
  x$spectra[,] <- NA_real_
  x$spectra[seq_len(6), 1] <- seq_len(6)
  x$spectra[seq_len(7), 2] <- seq_len(7)
  support <- OpenSpecy:::.lib_filter_spectral_support(x, min_fraction = 0.1)
  expect_equal(support$audit$minimum_observed, rep(7L, 8))
  expect_false(support$audit$retained[1])
  expect_true(support$audit$retained[2])

  spectra <- matrix(c(1, NA, 3, 10, 20, NA), nrow = 3)
  spectrum_fill <- OpenSpecy:::.lib_spectrum_mean_replace(spectra)
  expect_equal(spectrum_fill[, 1], c(1, 2, 3))
  expect_equal(spectrum_fill[, 2], c(10, 20, 15))
  wave_fill <- OpenSpecy:::.lib_wavenumber_mean_replace(spectra)
  expect_equal(wave_fill$means, c(5.5, 20, 3))
  expect_equal(wave_fill$spectra[, 1], c(1, 20, 3))
  expect_equal(wave_fill$spectra[, 2], c(10, 20, 3))
})

test_that("medoid selection restores original missing values", {
  x <- tiny_build_lib()
  x$spectra[10:20, 1] <- NA_real_
  ids <- reduce_lib(
    x, group_cols = "material_class", k = 2, min_n = 2,
    return = "ids", progress = FALSE
  )
  restored <- filter_spec(x, ids)
  if ("s1" %in% ids) {
    expect_true(anyNA(restored$spectra[, match("s1", ids)]))
  }
  flat <- x
  flat$spectra[, 1] <- NA_real_
  flat$spectra[seq_len(7), 1] <- 1
  expect_no_error(reduce_lib(
    flat, group_cols = "material_class", k = 2, min_n = 2,
    return = "ids", progress = FALSE
  ))
})

test_that("medoid groups preserve library fallback source diversity", {
  set.seed(844)
  wavenumber <- seq(800, 1200, length.out = 40L)
  spectra <- vapply(seq_len(102L), function(i) {
    sin(wavenumber / (55 + i / 20)) +
      cos(wavenumber / (80 + i / 30)) + i / 10000
  }, numeric(length(wavenumber)))
  colnames(spectra) <- paste0("fallback_", seq_len(ncol(spectra)))
  x <- as_OpenSpecy(
    wavenumber, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra), material_class = "polyvinyl esters",
      spectrum_type = "ftir", organization = NA_character_,
      library_name = rep(c("library_a", "library_b"), each = 51L)
    )
  )

  medoid <- OpenSpecy:::.lib_build_single_medoid(
    x, artifact = "derivative", type = "ftir",
    report = NULL, progress = FALSE
  )

  expect_equal(ncol(medoid$spectra), 100L)
  expect_equal(
    medoid$metadata[, .N, by = library_name][order(library_name)]$N,
    c(50L, 50L)
  )
  expect_false(".OpenSpecy_medoid_source" %in% names(medoid$metadata))
})

test_that("macro lambda metrics choose class-balanced accuracy", {
  outcome <- c(1L, 1L, 1L, 2L)
  predictions <- array(-1, dim = c(4, 2, 2))
  predictions[cbind(seq_len(4), c(1, 1, 1, 1), 1)] <- 1
  predictions[cbind(seq_len(4), c(1, 2, 2, 2), 2)] <- 1
  metrics <- OpenSpecy:::.lib_macro_lambda_metrics(
    predictions, outcome, lambda = c(1, 0.1), selection_rule = "macro"
  )
  expect_equal(metrics$macro_class_accuracy, c(0.5, 2 / 3))
  expect_equal(metrics$overall_accuracy, c(0.75, 0.5))
  expect_true(all(metrics$log_loss >= 0))
  expect_true(all(metrics$brier_score >= 0 & metrics$brier_score <= 2))
  expect_identical(which(metrics$selected), 2L)
})

test_that("gated worst-source metrics require 25 observations", {
  outcome <- rep(c(1L, 2L), length.out = 49L)
  sources <- c(rep("under-supported", 24L), rep("supported", 25L))
  predicted <- 3L - outcome
  predicted[25:44] <- outcome[25:44]
  predictions <- array(-10, dim = c(length(outcome), 2L, 1L))
  predictions[cbind(seq_along(outcome), predicted, 1L)] <- 10

  metrics <- OpenSpecy:::.lib_macro_lambda_metrics(
    predictions, outcome, lambda = 0.1,
    selection_rule = "guardrailed", source_groups = sources,
    selection_policy = "gated"
  )

  expect_equal(metrics$worst_source_accuracy, 0.8)
  expect_equal(metrics$supported_sources, 1L)
  expect_equal(metrics$source_min_observations, 25L)
})

test_that("multinomial cross-validation links become probabilities", {
  links <- matrix(c(10, 0, -10, -3, -3, -3), nrow = 2, byrow = TRUE)
  probabilities <- OpenSpecy:::.lib_multinomial_link_probabilities(links)
  expect_equal(rowSums(probabilities), c(1, 1), tolerance = 1e-12)
  expect_true(all(probabilities >= 0 & probabilities <= 1))
  expect_equal(probabilities[2, ], rep(1 / 3, 3), tolerance = 1e-12)
})

test_that("guardrailed lambda selection uses absolute one-point windows", {
  metrics <- data.table::data.table(
    lambda = c(1, 0.5, 0.1),
    overall_accuracy = c(0.900, 0.890, 0.870),
    macro_class_accuracy = c(0.700, 0.790, 0.900),
    log_loss = c(0.50, 0.40, 0.30),
    brier_score = c(0.30, 0.25, 0.20)
  )
  selected <- OpenSpecy:::.lib_select_lambda_metrics(
    metrics, selection_rule = "guardrailed", accuracy_tolerance = 0.01
  )
  expect_equal(selected$overall_eligible, c(TRUE, TRUE, FALSE))
  expect_equal(selected$macro_eligible, c(FALSE, TRUE, FALSE))
  expect_identical(which(selected$selected), 2L)
  expect_equal(selected$accuracy_tolerance, rep(0.01, 3))
})

test_that("gated lambda selection applies ordered one-point guardrails", {
  metrics <- data.table::data.table(
    lambda = c(1, 0.5, 0.1, 0.05),
    overall_accuracy = c(0.900, 0.890, 0.895, 0.890),
    minimum_class_recall = c(0.70, 0.80, 0.80, 0.80),
    worst_source_accuracy = c(0.90, 0.70, 0.82, 0.81),
    supported_sources = 6L,
    macro_class_accuracy = c(0.90, 0.90, 0.75, 0.80),
    log_loss = c(0.2, 0.2, 0.2, 0.2),
    brier_score = c(0.1, 0.1, 0.1, 0.1)
  )
  selected <- OpenSpecy:::.lib_select_gated_lambda_metrics(
    metrics, selection_rule = "guardrailed", accuracy_tolerance = 0.01
  )
  expect_equal(selected$overall_eligible, rep(TRUE, 4))
  expect_equal(selected$minimum_class_eligible,
               c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(selected$source_eligible, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(selected$macro_eligible, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(which(selected$selected), 4L)
})

test_that("source-component folds keep sources and stable identities together", {
  source <- rep(paste0("source_", 1:7), each = 2)
  labels <- rep(c("class_a", "class_b"), 7)
  groups <- paste0("group_", seq_along(labels))
  groups[[3L]] <- groups[[1L]]
  labels[[3L]] <- labels[[1L]]
  first <- OpenSpecy:::.lib_model_source_fold_assignment(
    groups, labels, source, seed = 812L
  )
  second <- OpenSpecy:::.lib_model_source_fold_assignment(
    groups, labels, source, seed = 812L
  )
  expect_identical(first$foldid, second$foldid)
  expect_identical(first$fold_mode, "source_component_holdout")
  audit <- data.table::data.table(
    source, groups, component = first$source_component_id,
    fold = first$foldid
  )
  expect_true(all(audit[, data.table::uniqueN(fold), by = source]$V1 == 1L))
  expect_true(all(audit[, data.table::uniqueN(fold), by = groups]$V1 == 1L))
  expect_true(all(audit[, data.table::uniqueN(fold), by = component]$V1 == 1L))
})

test_that("gated source eligibility counts organizations before fold components", {
  skip_if_not_installed("glmnet")
  wavenumber <- seq(800, 990, by = 10)
  labels <- rep(c("class_a", "class_b"), 50L)
  spectra <- vapply(labels, function(label) {
    if (label == "class_a") sin(wavenumber / 100) else cos(wavenumber / 100)
  }, numeric(length(wavenumber)))
  colnames(spectra) <- paste0("source_gate_", seq_along(labels))
  make_fixture <- function(organizations) {
    as_OpenSpecy(
      wavenumber, spectra,
      metadata = data.table::data.table(
        sample_name = colnames(spectra), material_class = labels,
        material_type = "plastic",
        material_group = ifelse(labels == "class_a", "group_a", "group_b"),
        spectrum_type = "ftir", organization = organizations,
        library_name = paste0("library_", seq_along(labels))
      ),
      attributes = list(intensity_unit = "absorbance")
    )
  }
  fit <- function(organizations) suppressMessages(suppressWarnings(
    train_spec_model(
      make_fixture(organizations),
      hierarchy_col = c("material_type", "material_group"),
      min_n = 3L, nlambda = 3L
    )
  ))

  five_sources <- fit(rep(paste0("organization_", 1:5), each = 20L))
  five_group <- five_sources$node_support[
    target_col == "material_group" & classes > 1L
  ]
  expect_true(five_group$source_eligible)
  expect_equal(five_group$canonical_sources, 5L)
  expect_equal(five_group$minimum_child_sources, 5L)
  expect_equal(five_group$source_components, 1L)
  expect_identical(
    five_group$fold_mode, "identity_grouped_source_fold_unavailable"
  )

  two_sources <- fit(rep(paste0("organization_", 1:2), each = 50L))
  two_group <- two_sources$node_support[
    target_col == "material_group" & classes > 1L
  ]
  expect_false(two_group$source_eligible)
  expect_equal(two_group$canonical_sources, 2L)
  expect_identical(
    two_group$fold_mode, "identity_grouped_undersupported_source"
  )
})

test_that("gated hierarchical logistic models route without pooling probabilities", {
  skip_if_not_installed("glmnet")
  set.seed(8102)
  wavenumber <- seq(800, 1190, by = 10)
  classes <- rep(c("polyethylene", "polyvinylalcohols",
                   "organic matter", "mineral", "cellulose"), each = 6)
  broad <- ifelse(
    classes %in% c("polyethylene", "polyvinylalcohols"), "plastic",
    ifelse(classes == "cellulose", "natural", "not plastic")
  )
  groups <- data.table::fcase(
    classes %in% c("polyethylene", "polyvinylalcohols"), "polymer test",
    classes %in% c("organic matter", "mineral"), "non-polymer",
    default = "cellulose"
  )
  prototypes <- list(
    polyethylene = sin(seq(0, pi, length.out = length(wavenumber))),
    polyvinylalcohols = cos(seq(0, pi, length.out = length(wavenumber))),
    `organic matter` = seq(0, 1, length.out = length(wavenumber)),
    mineral = rev(seq(0, 1, length.out = length(wavenumber))),
    cellulose = sin(seq(0, 2 * pi, length.out = length(wavenumber)))
  )
  spectra <- vapply(seq_along(classes), function(i) {
    prototypes[[classes[[i]]]] + stats::rnorm(length(wavenumber), sd = 0.01)
  }, numeric(length(wavenumber)))
  colnames(spectra) <- paste0("hierarchy_", seq_along(classes))
  x <- as_OpenSpecy(
    wavenumber, spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra), material_class = classes,
      material_type = broad, material_group = groups, spectrum_type = "ftir"
    ),
    attributes = list(intensity_unit = "absorbance")
  )

  node_cache <- new.env(parent = emptyenv())
  checkpoint_reads <- new.env(parent = emptyenv())
  checkpoint_writes <- new.env(parent = emptyenv())
  node_checkpoint <- function(run) {
    force(run)
    checkpoint_reads[[run]] <- character()
    checkpoint_writes[[run]] <- character()
    list(
      get = function(node_id) {
        checkpoint_reads[[run]] <- c(checkpoint_reads[[run]], node_id)
        if (exists(node_id, envir = node_cache, inherits = FALSE)) {
          get(node_id, envir = node_cache, inherits = FALSE)
        } else {
          NULL
        }
      },
      put = function(node_id, value) {
        checkpoint_writes[[run]] <- c(checkpoint_writes[[run]], node_id)
        assign(node_id, value, envir = node_cache)
        invisible(NULL)
      }
    )
  }

  model <- suppressMessages(suppressWarnings(OpenSpecy:::.lib_train_spec_model(
    x, hierarchy_col = c("material_type", "material_group"),
    min_n = 3, nlambda = 4,
    selection_rule = "guardrailed",
    node_checkpoint = node_checkpoint("first")
  )))
  expect_identical(model$model_type,
                   "gated_hierarchical_logistic_regression")
  expect_identical(model$model_schema, "OpenSpecy_gated_hierarchy_v1")
  expect_identical(model$hierarchy_cols,
                   c("material_type", "material_group"))
  expect_true(length(model$gated_nodes) >= 6L)
  expect_true(any(vapply(model$gated_nodes, function(node) {
    isTRUE(node$deterministic)
  }, logical(1L))))
  non_deterministic <- names(model$gated_nodes)[!vapply(
    model$gated_nodes, function(node) isTRUE(node$deterministic), logical(1L)
  )]
  expect_gt(length(non_deterministic), 0L)
  expect_setequal(checkpoint_reads$first, non_deterministic)
  expect_setequal(checkpoint_writes$first, non_deterministic)

  cached_model <- suppressMessages(suppressWarnings(
    OpenSpecy:::.lib_train_spec_model(
      x, hierarchy_col = c("material_type", "material_group"),
      min_n = 3, nlambda = 4, selection_rule = "guardrailed",
      node_checkpoint = node_checkpoint("second")
    )
  ))
  expect_setequal(checkpoint_reads$second, non_deterministic)
  expect_length(checkpoint_writes$second, 0L)
  expect_identical(names(cached_model$gated_nodes), names(model$gated_nodes))
  expect_identical(cached_model$root_node_id, model$root_node_id)
  expect_identical(cached_model$tests$terminal_node,
                   model$tests$terminal_node)
  expect_true(all(c("gate:material_type", "gate:material_group",
                     "routed_leaf") %in% model$hierarchy_metrics$scope))
  expect_false(any(model$fold_assignments[,
    data.table::uniqueN(fold), by = group_id]$V1 > 1L, na.rm = TRUE))

  ranked <- suppressWarnings(match_spec(x, model, top_n = 5L))
  cached_ranked <- suppressWarnings(match_spec(x, cached_model, top_n = 5L))
  expect_equal(cached_ranked, ranked, tolerance = 0)
  expect_equal(ranked[, sum(value), by = x]$V1, rep(1, ncol(x$spectra)),
               tolerance = 1e-10)
  expect_setequal(ranked$name, paste0("ftir_", unique(classes)))
  expect_true(all(c(
    "routed_material_type", "material_type_probability",
    "routed_material_group", "material_group_probability", "terminal_node"
  ) %in% names(ranked)))
  expect_setequal(unique(ranked$routed_material_type), unique(broad))
  expect_setequal(unique(ranked$routed_material_group), unique(groups))
  expect_false(any(grepl(
    "^(ftir|raman|nir)_", ranked$routed_material_type, ignore.case = TRUE
  )))
  expect_false(any(grepl(
    "^(ftir|raman|nir)_", ranked$routed_material_group, ignore.case = TRUE
  )))
  expect_setequal(model$hierarchy$material_type, unique(broad))
  expect_setequal(model$hierarchy$material_group, unique(groups))

  slim <- OpenSpecy:::.lib_slim_model(model)
  expect_equal(
    suppressWarnings(match_spec(x, slim, top_n = 5L)), ranked,
    tolerance = 1e-12
  )
  appended <- OpenSpecy:::.append_particle_matches(
    x, slim, material_col = "material", library_id_col = "sample_name"
  )
  winners <- ranked[rank == 1L][order(x)]$name
  expect_equal(appended$metadata$material, winners)

  incomplete <- x
  incomplete$metadata$material_type[[1L]] <- NA_character_
  expect_error(
    train_spec_model(
      incomplete,
      hierarchy_col = c("material_type", "material_group"),
      min_n = 3, nlambda = 3
    ),
    "without 'material_type'"
  )
  expect_error(
    train_spec_model(
      x, hierarchy_col = c("material_type", "material_group"), min_n = 3,
      method = "random_forest"
    ),
    "supports logistic regression only"
  )
  expect_error(
    train_spec_model(
      x, hierarchy_col = c("material_type", "material_class"),
      min_n = 3, nlambda = 3
    ),
    "cannot include 'class_col' or 'type_col'"
  )
})

test_that("downstream rebuild signatures cover actual libraries and medoids", {
  parent <- tiny_build_lib()
  parent$metadata$organization <- NA_character_
  parent$metadata$library_name <- rep(c("source_a", "source_b"), each = 4)
  libraries <- list(
    derivative = list(ftir = parent),
    nobaseline = list(ftir = parent)
  )
  medoids <- OpenSpecy:::.lib_build_medoids(
    libraries, report = function(...) NULL, progress = FALSE
  )

  signature <- OpenSpecy:::.lib_rebuild_checkpoint_signature(
    libraries, medoids, input_signature = "fixture"
  )
  expect_identical(
    OpenSpecy:::.lib_rebuild_checkpoint_signature(
      libraries, medoids, input_signature = "fixture"
    ),
    signature
  )

  changed_library <- libraries
  changed_library$derivative$ftir$spectra[1L, 1L] <-
    changed_library$derivative$ftir$spectra[1L, 1L] + 1e-8
  expect_false(identical(
    OpenSpecy:::.lib_rebuild_checkpoint_signature(
      changed_library, medoids, input_signature = "fixture"
    ),
    signature
  ))

  changed_medoid <- medoids
  changed_medoid$derivative$ftir$metadata$material_class[[1L]] <-
    "changed class"
  expect_false(identical(
    OpenSpecy:::.lib_rebuild_checkpoint_signature(
      libraries, changed_medoid, input_signature = "fixture"
    ),
    signature
  ))
})

test_that("supplied medoids require current parent and source provenance", {
  parent <- tiny_build_lib()
  parent$metadata$organization <- NA_character_
  parent$metadata$library_name <- rep(c("source_a", "source_b"), each = 4)
  libraries <- list(
    derivative = list(ftir = parent),
    nobaseline = list(ftir = parent)
  )
  medoids <- OpenSpecy:::.lib_build_medoids(
    libraries, report = function(...) NULL, progress = FALSE
  )

  expect_length(
    OpenSpecy:::.lib_supplied_medoid_problems(libraries, medoids), 0L
  )
  expect_true(nzchar(attr(
    medoids$derivative$ftir, "medoid_parent_signature", exact = TRUE
  )))
  expect_identical(
    attr(
      OpenSpecy:::.lib_slim_reference_object(medoids$derivative$ftir),
      "medoid_parent_signature", exact = TRUE
    ),
    attr(
      medoids$derivative$ftir, "medoid_parent_signature", exact = TRUE
    )
  )

  legacy <- medoids
  attr(legacy$derivative$ftir, "medoid_parent_signature") <- NULL
  expect_match(
    OpenSpecy:::.lib_supplied_medoid_problems(libraries, legacy),
    "stale parent/source provenance"
  )

  changed_source <- libraries
  changed_source$derivative$ftir$metadata$library_name[[1L]] <- "source_c"
  expect_match(
    OpenSpecy:::.lib_supplied_medoid_problems(changed_source, medoids),
    "stale parent/source provenance"
  )

  changed_values <- medoids
  changed_values$derivative$ftir$spectra[1L, 1L] <-
    changed_values$derivative$ftir$spectra[1L, 1L] + 1e-8
  expect_match(
    OpenSpecy:::.lib_supplied_medoid_problems(libraries, changed_values),
    "unchanged parent subset"
  )

  one_library <- libraries["derivative"]
  stale_prebuilt <- medoids["derivative"]
  attr(
    stale_prebuilt$derivative$ftir, "medoid_parent_signature"
  ) <- NULL
  rebuilt_prebuilt <- OpenSpecy:::.lib_build_medoids(
    one_library, report = function(...) NULL, progress = FALSE,
    prebuilt = stale_prebuilt
  )
  expect_length(
    OpenSpecy:::.lib_supplied_medoid_problems(
      one_library, rebuilt_prebuilt
    ),
    0L
  )

  writes <- character()
  stale_cached <- stale_prebuilt$derivative$ftir
  checkpoint <- list(
    get = function(stage) stale_cached,
    put = function(stage, value) {
      writes <<- c(writes, stage)
      invisible(value)
    }
  )
  rebuilt_cached <- OpenSpecy:::.lib_build_medoids(
    one_library, report = function(...) NULL, checkpoints = checkpoint,
    progress = FALSE
  )
  expect_identical(writes, "medoid_derivative_ftir")
  expect_length(
    OpenSpecy:::.lib_supplied_medoid_problems(
      one_library, rebuilt_cached
    ),
    0L
  )
})

test_that("rebuild_lib_artifacts reuses completed libraries downstream", {
  skip_if_not_installed("glmnet")
  base <- tiny_build_lib()
  libraries <- lapply(seq_len(3), function(i) {
    spectra <- do.call(cbind, lapply(seq_len(3), function(copy) {
      base$spectra + copy / 1000
    }))
    metadata <- data.table::rbindlist(lapply(seq_len(3), function(copy) {
      out <- data.table::copy(base$metadata)
      out$sample_name <- paste0(out$sample_name, "_", copy)
      out
    }))
    colnames(spectra) <- metadata$sample_name
    as_OpenSpecy(base$wavenumber, spectra = spectra, metadata = metadata)
  })
  names(libraries) <- c("raw", "derivative", "nobaseline")
  libraries <- lapply(libraries, function(x) list(ftir = x))
  libraries$derivative$ftir$spectra[10:20, 1] <- NA_real_
  input <- tempfile(fileext = ".rds")
  saveRDS(list(libraries = libraries, assessments = list()), input)
  output <- tempfile("downstream-build-")

  first <- suppressWarnings(rebuild_lib_artifacts(
    input, output_dir = output, previous_library_dir = NULL,
    reuse = TRUE, progress = FALSE
  ))
  expect_named(first, c("libraries", "medoids", "models", "assessments"))
  expect_true(anyNA(first$medoids$derivative$ftir$spectra))
  expect_true(
    first$models$gated_hierarchical_logistic_regression$
      derivative$ftir$fill_replaced > 0L
  )
  expect_true(
    is.finite(first$models$gated_hierarchical_logistic_regression$
      derivative$ftir$training_elapsed_seconds)
  )
  expect_named(first$assessments,
               c("cleanup", "ref_lib", "medoid", "model", "functionality"))
  second <- suppressWarnings(rebuild_lib_artifacts(
    input, output_dir = output, previous_library_dir = NULL,
    reuse = TRUE, progress = FALSE
  ))
  second_evidence <- attr(second$assessments, "evidence")
  expect_true(all(
    second_evidence$output_manifest$status == "available"
  ))
})

test_that("reference quality schemas and type ranges are stable", {
  expect_named(OpenSpecy:::.lib_warning_schema(),
               c("algorithm", "artifact", "model", "warning"))
  expect_type(OpenSpecy:::.lib_warning_schema()$warning, "character")

  x <- tiny_build_lib()
  x$metadata$spectrum_type <- rep(c("ftir", "raman"), each = 4)
  typed <- OpenSpecy:::.lib_partition_reference_libraries(
    list(raw = x), report = NULL
  )$raw
  expect_equal(range(typed$ftir$wavenumber), c(400, 4000))
  expect_equal(range(typed$raman$wavenumber), c(200, 4000))
})

test_that("range restriction removes spectra that become flat", {
  x <- tiny_build_lib()
  x$metadata$spectrum_type <- "raman"
  x$spectra[, 1] <- 1
  outside_partition <- x$wavenumber > 4000
  x$spectra[outside_partition, 1] <- seq_len(sum(outside_partition))

  typed <- OpenSpecy:::.lib_partition_reference_libraries(
    list(raw = x), report = NULL
  )$raw$raman
  expect_false("s1" %in% typed$metadata$sample_name)
  partition_drops <- attr(typed, "range_flat_drops", exact = TRUE)
  expect_equal(partition_drops$spectrum_id, "s1")
  expect_equal(partition_drops$reason, "post_partition_flat_spectrum")

  model_source <- tiny_build_lib()
  model_source$metadata$spectrum_type <- "raman"
  model_source$spectra[, 1] <- 1
  outside_model <- model_source$wavenumber > 3200
  model_source$spectra[outside_model, 1] <- seq_len(sum(outside_model))
  medoids <- OpenSpecy:::.lib_build_medoids(
    list(derivative = list(raman = model_source)),
    report = function(...) NULL, progress = FALSE
  )$derivative$raman
  expect_false("s1" %in% medoids$metadata$sample_name)
  model_drops <- attr(medoids, "range_flat_drops", exact = TRUE)
  expect_equal(model_drops$spectrum_id, "s1")
  expect_equal(model_drops$reason, "post_model_range_flat_spectrum")
})

test_that("identification ranges retain partial spectra and drop empty ones", {
  axis <- seq(400, 4000, by = 6)
  spectra <- cbind(
    complete = sin(axis / 100),
    partial = ifelse(axis < 1200, NA_real_, cos(axis / 90)),
    outside_only = ifelse(axis < 800, 1, NA_real_)
  )
  object <- as_OpenSpecy(
    axis, spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra),
      spectrum_type = "ftir",
      material_class = "plastic"
    )
  )

  restricted <- OpenSpecy:::.lib_restrict_model_range(object, "ftir")
  expect_equal(range(restricted$wavenumber), c(802, 3196))
  expect_false(any(
    restricted$wavenumber > 2200 & restricted$wavenumber < 2420
  ))
  expect_equal(
    attr(restricted, "identification_ranges"),
    data.table::data.table(
      minimum = c(800, 2420), maximum = c(2200, 3200)
    )
  )
  expect_identical(colnames(restricted$spectra), c("complete", "partial"))
  expect_identical(
    attr(restricted, "identification_dropped_ids"), "outside_only"
  )
  expect_true(anyNA(restricted$spectra[, "partial"]))

  raman <- OpenSpecy:::.lib_restrict_model_range(object, "raman")
  expect_true(any(
    raman$wavenumber > 2200 & raman$wavenumber < 2420
  ))
  expect_equal(
    attr(raman, "identification_ranges"),
    data.table::data.table(minimum = 800, maximum = 3200)
  )
})

test_that("reference regex table contains only genuinely variable rules", {
  expect_true(OpenSpecy:::.lib_regex_is_exact_literal(
    "^poly\\(amide\\)$"
  ))
  expect_false(OpenSpecy:::.lib_regex_is_exact_literal(
    "^olefin[[:space:]]*\\(pe(?:[[:space:]]|\\x2c|\\)|$)"
  ))
  expect_false(OpenSpecy:::.lib_regex_is_exact_literal(
    "^(?:cotton|wool|silk)$"
  ))
  regex_reference <- data.table::fread(
    reference_workflow_data_path("classes_regex.csv")
  )
  expect_false(any(vapply(
    regex_reference$pattern,
    OpenSpecy:::.lib_regex_is_exact_literal,
    logical(1)
  )))
  exact <- data.table::fread(
    reference_workflow_data_path("classes_reference.csv")
  )
  expect_true(all(c(
    "epoxide", "poly 1-butene isotactic", "poly 4-methyl-1-pentene",
    "poly(amide)", "poly(styrene)", "poly(vinylchloride)",
    "polyethylene glycol"
  ) %in% exact$spectrum_identity))
  predicted <- predict_class_reference(
    data.table::data.table(
      spectrum_identity = c(
        "sample sbr rubber", "sample epdm gasket", "sample acrylic paint",
        "sample urethane paint", "sample alkyd varnish"
      ),
      material = NA_character_
    ),
    regex_reference, return = "table"
  )
  expect_identical(
    predicted$material,
    c(
      "styrene-butadiene",
      "epdm rubber (ethylene propylene diene monomer rubber)",
      "polyacrylates", "polyurethanes", "polyesters"
    )
  )
})

test_that("official material hierarchy uses concise reviewed polymer classes", {
  hierarchy <- data.table::fread(
    reference_workflow_data_path("material_hierarchy.csv")
  )
  expect_identical(anyDuplicated(hierarchy$material), 0L)
  expect_false(anyNA(hierarchy$material_class))
  expect_false(any(!nzchar(trimws(hierarchy$material_class))))
  expect_identical(
    hierarchy[material == "poly(ethylene)", material_class], "polyethylene"
  )
  expect_identical(
    hierarchy[material == "poly(propylene)", material_class], "polypropylene"
  )
  expect_identical(
    hierarchy[material == "poly(tetrafluoroethylene)", material_class],
    "polytetrafluoroethylene"
  )
  expect_identical(
    hierarchy[material == "poly(ethylene-vinyl acetate) (EVA)",
              material_class],
    "polyethylene-vinyl acetate"
  )
  expect_identical(
    hierarchy[material == "styrene-butadiene", material_class],
    "poly(styrene-butadiene) rubber"
  )
  expect_identical(
    hierarchy[
      material == "epdm rubber (ethylene propylene diene monomer rubber)",
      material_class
    ],
    "poly(ethylene-propylene-diene) rubber"
  )
  expect_identical(
    hierarchy[material == "poly(ethylene glycol)", material_class],
    "polyethers"
  )
  parenthetical_classes <- unique(
    hierarchy[grepl("[()]", material_class), material_class]
  )
  expect_setequal(parenthetical_classes, c(
    "poly(ethylene-propylene-diene) rubber",
    "poly(styrene-butadiene) rubber",
    "polyhydroxy(meth)acrylates"
  ))
  paint_classes <- c("paint", "acrylic paint", "alkyd paint", "urethane paint")
  expect_false(any(hierarchy$material_class %in% paint_classes))
  exceptions <- c(
    "other plastic"
  )
  polymer_classes <- unique(hierarchy[material_type == "plastic", material_class])
  expect_true(all(startsWith(setdiff(polymer_classes, exceptions), "poly")))
})
