test_that("gated source identity prefers organization with documented fallbacks", {
  metadata <- data.table::data.table(
    organization = c(" primary a ", "", NA_character_, "primary d"),
    library_name = c("library a", "library b", "library c", "library d")
  )

  expect_identical(
    OpenSpecy:::.lib_model_source_groups(metadata),
    c("primary a", "library b", "library c", "primary d")
  )
  metadata$library_name[[3L]] <- NA_character_
  expect_identical(
    OpenSpecy:::.lib_model_source_groups(metadata, paste0("g", 1:4)),
    c("primary a", "library b", "group:g3", "primary d")
  )
})

test_that("small gated CV nodes avoid parallel dispatch overhead", {
  small <- list(x = matrix(0, nrow = 340, ncol = 400), y = rep(1:2, 170))
  root <- list(x = matrix(0, nrow = 987, ncol = 400), y = rep(1:2, 494)[1:987])
  group <- list(x = matrix(0, nrow = 907, ncol = 400), y = rep(1:12, 76)[1:907])

  expect_false(OpenSpecy:::.lib_cv_parallel_work(small))
  expect_true(OpenSpecy:::.lib_cv_parallel_work(root))
  expect_true(OpenSpecy:::.lib_cv_parallel_work(group))
})

test_that("aggregate gated checkpoints require every expected model", {
  root_node <- OpenSpecy:::.lib_logistic_node_runtime(
    OpenSpecy:::.lib_deterministic_logistic_node("polyethylene", 1L)
  )
  model <- list(
    model = NULL, class_names = "polyethylene", class_num = 1L,
    fill = raman_hdpe, all_variables = raman_hdpe$wavenumber,
    model_schema = "OpenSpecy_gated_hierarchy_v1",
    hierarchy_cols = c("material_type", "material_group"),
    root_node_id = "root",
    gated_nodes = list(root = root_node),
    gated_topology = data.table::data.table(
      node_id = "root", parent_id = NA_character_,
      incoming_label = NA_character_, depth = 0L,
      target_col = "material_class", terminal = TRUE
    ),
    gated_edges = data.table::data.table(
      node_id = character(), label = character(),
      child_node_id = character()
    ),
    selected_lambda_converged = TRUE, training_elapsed_seconds = 1
  )
  medoids <- list(derivative = list(ftir = raman_hdpe))
  models <- list(gated_hierarchical_logistic_regression = list(
    derivative = list(ftir = model)
  ))

  expect_length(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(models, medoids), 0L
  )
  missing <- models
  missing$gated_hierarchical_logistic_regression$derivative$ftir <- NULL
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(missing, medoids),
    "derivative/ftir missing"
  )
  unconverged <- models
  unconverged$gated_hierarchical_logistic_regression$derivative$ftir$
    selected_lambda_converged <- FALSE
  expect_match(
    OpenSpecy:::.lib_gated_model_checkpoint_problems(unconverged, medoids),
    "selected lambda unconverged"
  )

  attr(
    models$gated_hierarchical_logistic_regression$derivative$ftir,
    "training_warnings"
  ) <- "fixture warning"
  warnings <- OpenSpecy:::.lib_collect_model_warnings(models)
  expect_equal(nrow(warnings), 1L)
  expect_identical(warnings$warning, "fixture warning")
})

test_that("reference checkpoint signatures cover all executed package code", {
  paths <- OpenSpecy:::.lib_build_source_paths()
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  workflow <- file.path(root, "workflows", "OpenSpecy_reference_library.R")
  source_layout <- dir.exists(file.path(root, "R")) && file.exists(workflow)

  expect_true(all(c("DESCRIPTION", "NAMESPACE") %in% basename(paths)))
  expect_true(any(grepl("[/\\\\]R[/\\\\]", paths)))
  if (source_layout) {
    expected_r <- sort(normalizePath(
      list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
      mustWork = TRUE
    ))
    expect_setequal(intersect(paths, expected_r), expected_r)
    expect_true(normalizePath(file.path(root, "DESCRIPTION")) %in% paths)
    expect_true(normalizePath(file.path(root, "NAMESPACE")) %in% paths)
    expect_true(normalizePath(workflow) %in% paths)
    expect_true(normalizePath(file.path(
      root, "workflows", "reference_intake.R"
    )) %in% paths)
  }
  signatures <- OpenSpecy:::.lib_build_source_signatures()
  expect_equal(nrow(signatures), length(paths))
  expect_false(anyNA(signatures$checksum))
})

test_that("OOF routing carries deterministic foreign rows to the terminal", {
  train <- matrix(c(1, 2), ncol = 1)
  rownames(train) <- c("own", "foreign")
  specifications <- list(
    root = list(rows = 1:2), group = list(rows = 1L),
    leaf = list(rows = 1L)
  )
  fitted_nodes <- list(
    root = OpenSpecy:::.lib_deterministic_logistic_node("rootgroup", 2L),
    group = OpenSpecy:::.lib_deterministic_logistic_node("groupA", 1L),
    leaf = OpenSpecy:::.lib_deterministic_logistic_node("leafA", 1L)
  )
  topology <- data.table::data.table(
    node_id = c("root", "group", "leaf"),
    parent_id = c(NA_character_, "root", "group"),
    incoming_label = c(NA_character_, "rootgroup", "groupA"),
    depth = 0:2,
    target_col = c("material_type", "material_group", "material_class"),
    terminal = c(FALSE, FALSE, TRUE)
  )
  edges <- data.table::data.table(
    node_id = c("root", "group"),
    label = c("rootgroup", "groupA"),
    child_node_id = c("group", "leaf")
  )
  leaves <- data.table::data.table(factor_num = 1L, name = "leafA")

  routed <- OpenSpecy:::.lib_route_fitted_gates(
    train, specifications, fitted_nodes, topology, edges, leaves,
    c("material_type", "material_group"), use_oof = TRUE
  )

  expect_identical(routed$leaf, rep("leafA", 2L))
  expect_identical(unname(routed$gate_names[, 1L]), rep("rootgroup", 2L))
  expect_identical(
    unname(routed$gate_names[, 2L]), rep("groupA", 2L)
  )
})

test_that("OOF routing uses fold-held child predictions for foreign rows", {
  train <- matrix(c(1, 2), ncol = 1)
  specifications <- list(
    root = list(rows = 1:2), leaf = list(rows = 1L)
  )
  leaf_conversion <- data.table::data.table(
    factor_num = 1:2, name = c("leafA", "leafB")
  )
  fitted_nodes <- list(
    root = OpenSpecy:::.lib_deterministic_logistic_node("plastic", 2L),
    leaf = list(
      deterministic = FALSE,
      dimension_conversion = leaf_conversion,
      class_names = leaf_conversion$name,
      routed_oof_probabilities = matrix(
        c(0.9, 0.1, 0.2, 0.8), nrow = 2L, byrow = TRUE,
        dimnames = list(NULL, c("1", "2"))
      )
    )
  )
  topology <- data.table::data.table(
    node_id = c("root", "leaf"), parent_id = c(NA_character_, "root"),
    incoming_label = c(NA_character_, "plastic"), depth = 0:1,
    target_col = c("material_type", "material_class"),
    terminal = c(FALSE, TRUE)
  )
  edges <- data.table::data.table(
    node_id = "root", label = "plastic", child_node_id = "leaf"
  )

  routed <- OpenSpecy:::.lib_route_fitted_gates(
    train, specifications, fitted_nodes, topology, edges, leaf_conversion,
    "material_type", use_oof = TRUE
  )

  expect_identical(routed$leaf, c("leafA", "leafB"))
  expect_equal(routed$leaf_value, c(0.9, 0.8))
  expect_false(anyNA(routed$terminal_node))
})

test_that("gated foreign-row cross-fitting matches explicit fold paths", {
  skip_if_not_installed("glmnet")
  set.seed(8124)
  observations <- 72L
  variables <- 18L
  classes <- 3L
  train <- matrix(
    stats::rnorm(observations * variables),
    nrow = observations, ncol = variables
  )
  outcome <- rep(seq_len(classes), length.out = observations)
  train[cbind(seq_len(observations), outcome)] <-
    train[cbind(seq_len(observations), outcome)] + 1.5
  foldid <- rep(rep(seq_len(3L), each = classes), length.out = observations)
  weights <- as.numeric(
    1 / (table(outcome)[as.character(outcome)] / length(outcome))
  )
  glmnet_args <- list(
    x = train, y = outcome, weights = weights, alpha = 0.1,
    family = "multinomial", intercept = FALSE,
    type.multinomial = "grouped", maxit = 1000000L, nlambda = 12L
  )
  model <- do.call(glmnet::glmnet, glmnet_args)
  selected_index <- min(7L, length(model$lambda))
  selected_lambda <- model$lambda[[selected_index]]

  foreign <- matrix(stats::rnorm(6L * variables), nrow = 6L)
  evaluation_train <- rbind(train, foreign)
  evaluation_foldid <- c(foldid, 1L, 2L, 3L, 1L, 2L, 3L)
  training_rows <- seq_len(observations)
  oof_probabilities <- matrix(
    rep(c(0.2, 0.3, 0.5), observations),
    nrow = observations, byrow = TRUE,
    dimnames = list(NULL, as.character(seq_len(classes)))
  )
  fold_info <- list(
    foldid = foldid, nfolds = 3L,
    evaluation_foldid = evaluation_foldid
  )

  observed <- OpenSpecy:::.lib_gated_cross_fitted_probabilities(
    model = model, glmnet_args = glmnet_args, train = train,
    outcome = outcome, fold_info = fold_info,
    lambda = selected_lambda, selected_index = selected_index,
    oof_probabilities = oof_probabilities,
    evaluation_train = evaluation_train, training_rows = training_rows
  )

  expected <- OpenSpecy:::.lib_probability_matrix(
    predict(
      model, newx = evaluation_train, s = selected_lambda,
      type = "response"
    ),
    n = nrow(evaluation_train)
  )
  colnames(expected) <- colnames(oof_probabilities)
  path_end <- min(length(model$lambda), max(2L, selected_index))
  for (fold in seq_len(3L)) {
    retain <- foldid != fold
    score_rows <- which(evaluation_foldid == fold)
    fold_model <- glmnet::glmnet(
      x = train[retain, , drop = FALSE], y = outcome[retain],
      weights = weights[retain], alpha = 0.1, family = "multinomial",
      intercept = FALSE, type.multinomial = "grouped", maxit = 1000000L,
      lambda = model$lambda[seq_len(path_end)]
    )
    fold_probability <- OpenSpecy:::.lib_probability_matrix(
      predict(
        fold_model,
        newx = evaluation_train[score_rows, , drop = FALSE],
        s = selected_lambda, type = "response"
      ),
      n = length(score_rows)
    )
    colnames(fold_probability) <- colnames(expected)
    expected[score_rows, ] <- fold_probability
  }
  expected[training_rows, ] <- oof_probabilities

  expect_equal(observed, expected, tolerance = 1e-12)
  expect_identical(observed[training_rows, , drop = FALSE], oof_probabilities)

  root_oof <- oof_probabilities[seq_len(6L), , drop = FALSE]
  expect_identical(
    OpenSpecy:::.lib_gated_cross_fitted_probabilities(
      model = NULL, glmnet_args = list(), train = evaluation_train[1:6, ],
      outcome = outcome[1:6],
      fold_info = list(evaluation_foldid = rep(1L, 6L)),
      lambda = NA_real_, selected_index = NA_integer_,
      oof_probabilities = root_oof,
      evaluation_train = evaluation_train[1:6, ], training_rows = 1:6
    ),
    root_oof
  )
})

test_that("gated diagnostics isolate fold evidence and selected-node metrics", {
  node_support <- data.table::data.table(
    node_id = c("root", "group_fit", "group_fixed", "leaf_source",
                "leaf_identity", "fixed"),
    target_col = c("material_type", "material_group", "material_group",
                   "material_class", "material_class", "fixed_target"),
    observations = c(100L, 60L, 40L, 70L, 30L, 10L),
    classes = c(2L, 2L, 1L, 2L, 2L, 1L),
    folds = c(5L, 4L, 0L, 5L, 3L, 0L),
    fold_mode = c(
      "source_component_holdout", "source_component_holdout", "deterministic",
      "source_component_holdout", "identity_grouped_undersupported_source",
      "deterministic"
    )
  )

  expect_identical(
    OpenSpecy:::.lib_gated_scope_evidence(node_support, "material_type"),
    "source_component_holdout"
  )
  expect_identical(
    OpenSpecy:::.lib_gated_scope_evidence(node_support, "material_group"),
    "source_component_holdout"
  )
  expect_identical(
    OpenSpecy:::.lib_gated_scope_evidence(node_support, "material_class"),
    "hybrid_node_oof"
  )
  expect_identical(
    OpenSpecy:::.lib_gated_scope_evidence(
      node_support, c("material_type", "material_class")
    ),
    "hybrid_node_oof"
  )
  expect_identical(
    OpenSpecy:::.lib_gated_scope_evidence(node_support, "fixed_target"),
    "deterministic"
  )
  expect_identical(
    OpenSpecy:::.lib_gated_routed_fold_summary(
      node_support, c("material_type", "material_group")
    ),
    list(folds = 4L, maximum_node_folds = 5L)
  )
  expect_identical(
    OpenSpecy:::.lib_gated_routed_fold_summary(
      node_support, c("material_type", "material_group", "material_class")
    ),
    list(folds = 3L, maximum_node_folds = 5L)
  )

  lambda_metrics <- data.table::data.table(
    node_id = c("root", "root", "group_fit", "leaf_source",
                "leaf_identity"),
    target_col = c("material_type", "material_type", "material_group",
                   "material_class", "material_class"),
    selected = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    log_loss = c(0.10, 9, 0.20, 0.30, 0.50),
    brier_score = c(0.05, 9, 0.10, 0.20, 0.40)
  )
  group_metrics <- OpenSpecy:::.lib_gated_conditional_metrics(
    node_support, lambda_metrics, "material_group"
  )
  expect_equal(group_metrics$log_loss, 0.12)
  expect_equal(group_metrics$brier_score, 0.06)
  leaf_metrics <- OpenSpecy:::.lib_gated_conditional_metrics(
    node_support, lambda_metrics, "material_class"
  )
  expect_equal(leaf_metrics$log_loss, 0.36)
  expect_equal(leaf_metrics$brier_score, 0.26)
  expect_true(all(is.finite(unlist(c(group_metrics, leaf_metrics)))))
})

test_that("gated fits report every route scope by organization", {
  skip_if_not_installed("glmnet")
  set.seed(8102)
  wavenumber <- seq(800, 1190, by = 10)
  classes <- rep(c("polyethylene", "polyvinylalcohols",
                   "organic matter", "mineral", "cellulose"), each = 6)
  material_type <- ifelse(
    classes %in% c("polyethylene", "polyvinylalcohols"), "plastic",
    ifelse(classes == "cellulose", "natural", "not plastic")
  )
  material_group <- data.table::fcase(
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
  organizations <- paste0("organization_", 1:6)
  x <- as_OpenSpecy(
    wavenumber, spectra = spectra,
    metadata = data.table::data.table(
      sample_name = colnames(spectra), material_class = classes,
      material_type = material_type, material_group = material_group,
      spectrum_type = "ftir",
      organization = rep(organizations, times = 5),
      library_name = paste0("library_", seq_along(classes))
    ),
    attributes = list(intensity_unit = "absorbance")
  )

  model <- suppressMessages(suppressWarnings(OpenSpecy:::.lib_train_spec_model(
    x, hierarchy_col = c("material_type", "material_group"),
    min_n = 3, nlambda = 4, selection_rule = "guardrailed"
  )))

  expected_scopes <- c(
    "gate:material_type", "gate:material_group", "routed_leaf"
  )
  expect_setequal(model$hierarchy_metrics$scope, expected_scopes)
  expect_setequal(unique(model$source_metrics$scope), expected_scopes)
  expect_setequal(unique(model$source_metrics$source), organizations)
  expect_true(all(model$source_metrics[, data.table::uniqueN(source),
                                       by = scope]$V1 == length(organizations)))
  expect_false(any(startsWith(model$source_metrics$source, "library_")))

  evidence <- stats::setNames(
    model$hierarchy_metrics$fold_evidence, model$hierarchy_metrics$scope
  )
  expect_identical(evidence[["gate:material_type"]], "identity_grouped")
  expect_identical(evidence[["gate:material_group"]], "identity_grouped")
  expect_identical(evidence[["routed_leaf"]], "identity_grouped")
  expect_identical(
    unique(model$source_metrics[scope == "gate:material_group", evidence]),
    "identity_grouped"
  )

  expect_true(all(is.finite(model$hierarchy_metrics$log_loss)))
  expect_true(all(is.finite(model$hierarchy_metrics$brier_score)))
  expect_equal(
    model$hierarchy_metrics[scope == "gate:material_group", log_loss], 0
  )
  expect_equal(
    model$hierarchy_metrics[scope == "gate:material_group", brier_score], 0
  )
  expect_true(all(
    model$hierarchy_metrics$probability_metric_scope ==
      "weighted_selected_node_conditional"
  ))
  expect_identical(unique(model$tests$split), "out_of_fold")
  expect_false(anyNA(model$tests$predicted_class))
  expect_true(all(model$hierarchy_metrics$route_coverage == 1))
  expect_true(all(
    model$hierarchy_metrics$folds <=
      model$hierarchy_metrics$maximum_node_folds
  ))
})
