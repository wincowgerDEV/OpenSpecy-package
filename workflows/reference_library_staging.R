# Restart-safe helpers for the updatedv1 reference-library overhaul.
#
# These helpers deliberately remain workflow code: build_lib() owns scientific
# processing, while this file owns one-dataset-at-a-time orchestration and the
# final no-filter assembly of already completed recipe/type artifacts.

.reference_internal <- function(name) {
  getFromNamespace(name, "OpenSpecy")
}

.reference_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.reference_read_rds <- function(path) {
  warnings <- character()
  object <- withCallingHandlers(
    readRDS(path),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  if (length(warnings)) {
    stop("Serialized artifact emitted warning(s): ", path, ": ",
         paste(unique(warnings), collapse = "; "), call. = FALSE)
  }
  object
}

.reference_valid_object <- function(x) {
  warnings <- character()
  valid <- is_OpenSpecy(x) && isTRUE(withCallingHandlers(
    check_OpenSpecy(x),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  ))
  valid && !length(warnings) &&
    ncol(x$spectra) == nrow(x$metadata) &&
    length(x$wavenumber) == nrow(x$spectra) &&
    all(is.finite(x$wavenumber)) && !anyDuplicated(x$wavenumber) &&
    !is.unsorted(x$wavenumber, strictly = TRUE)
}

.reference_recipe_types <- function(path, recipe, release_dir) {
  promote <- .reference_internal(".lib_promote_rds")
  object <- .reference_read_rds(path)
  if (!is.list(object) || !length(object) ||
      !all(vapply(object, .reference_valid_object, logical(1L)))) {
    stop("Completed recipe is not a nonempty typed OpenSpecy list: ", path,
         call. = FALSE)
  }
  rows <- lapply(names(object), function(type) {
    type_key <- tolower(type)
    observed <- unique(tolower(as.character(
      object[[type]]$metadata$spectrum_type
    )))
    observed <- observed[!is.na(observed) & nzchar(observed)]
    if (length(observed) != 1L || !identical(observed, type_key)) {
      stop("Recipe/type artifact has inconsistent spectrum_type: ",
           recipe, "/", type, call. = FALSE)
    }
    target <- file.path(release_dir, paste0(recipe, "_", type_key, ".rds"))
    promoted <- promote(object[[type]], target)
    data.table::data.table(
      recipe = recipe,
      spectrum_type = type_key,
      spectra = as.integer(ncol(object[[type]]$spectra)),
      wavenumbers = as.integer(length(object[[type]]$wavenumber)),
      metadata_columns = as.integer(ncol(object[[type]]$metadata)),
      path = as.character(promoted$path),
      size = as.numeric(promoted$size),
      checksum_algorithm = "sha256",
      checksum = as.character(promoted$checksum)
    )
  })
  rm(object)
  gc(verbose = FALSE, full = TRUE)
  data.table::rbindlist(rows, fill = TRUE)
}

reference_build_dataset <- function(input_path, slug, workflow_data,
                                    build_fun = build_lib,
                                    source_checksum = NULL,
                                    source_spectra = NULL,
                                    progress = TRUE) {
  if (!is.character(input_path) || length(input_path) != 1L ||
      !file.exists(input_path) || basename(input_path) != "CleanedOS_raw.rds" ||
      basename(dirname(input_path)) != "Processed") {
    stop("Dataset input must be an existing Processed/CleanedOS_raw.rds",
         call. = FALSE)
  }
  if (!is.character(slug) || length(slug) != 1L || is.na(slug) ||
      !nzchar(slug)) {
    stop("'slug' must be one nonempty dataset identifier", call. = FALSE)
  }
  if (!is.null(source_checksum) &&
      (!is.character(source_checksum) || length(source_checksum) != 1L ||
       is.na(source_checksum) || !grepl("^[0-9a-f]{64}$", source_checksum))) {
    stop("'source_checksum' must be NULL or one lowercase SHA-256 value",
         call. = FALSE)
  }
  if (!is.null(source_spectra) &&
      (!is.numeric(source_spectra) || length(source_spectra) != 1L ||
       !is.finite(source_spectra) || source_spectra < 1 ||
       source_spectra != floor(source_spectra) ||
       source_spectra > .Machine$integer.max)) {
    stop("'source_spectra' must be NULL or one positive integer",
         call. = FALSE)
  }
  output_dir <- file.path(dirname(input_path), "OpenSpecy_reference_library")
  result <- build_fun(
    x = input_path,
    output_dir = output_dir,
    artifact_scope = "libraries",
    previous_library_dir = NULL,
    reuse = TRUE,
    remove_other = TRUE,
    dedupe = TRUE,
    dedupe_correlation_threshold = 0.99,
    prune = list(
      derivative = list(cross_class = FALSE, min_n = 10L),
      nobaseline = list(cross_class = FALSE, min_n = 10L)
    ),
    progress = progress,
    workflow_data = workflow_data
  )
  if (!inherits(result, "OpenSpecyLibraryOnlyReferenceBuild") ||
      !identical(sort(names(result$libraries)),
                 sort(c("raw", "derivative", "nobaseline")))) {
    stop("Dataset build did not return all three library-only recipes: ", slug,
         call. = FALSE)
  }
  release_dir <- attr(result, "output_dir", exact = TRUE)
  artifact_rows <- lapply(names(result$libraries), function(recipe) {
    .reference_recipe_types(
      result$libraries[[recipe]], recipe = recipe, release_dir = release_dir
    )
  })
  artifacts <- data.table::rbindlist(artifact_rows, fill = TRUE)
  data.table::setorder(artifacts, recipe, spectrum_type)
  expected <- unique(artifacts[, .(recipe, spectrum_type)])
  if (!all(c("raw", "derivative", "nobaseline") %in% expected$recipe)) {
    stop("Promoted dataset artifacts are incomplete: ", slug, call. = FALSE)
  }
  source_info <- file.info(input_path)
  quarantine_path <- file.path(release_dir, "quarantined_spectra.rds")
  assessments_path <- file.path(release_dir, "assessments.rds")
  manifest <- list(
    schema = "OpenSpecy_dataset_library_manifest_v1",
    slug = slug,
    source = data.table::data.table(
      path = normalizePath(input_path, winslash = "/", mustWork = TRUE),
      size = as.numeric(source_info$size),
      spectra = if (is.null(source_spectra)) NA_integer_ else
        as.integer(source_spectra),
      checksum_algorithm = "sha256",
      checksum = if (is.null(source_checksum)) {
        .reference_sha256(input_path)
      } else source_checksum
    ),
    build_signature = attr(result, "build_signature", exact = TRUE),
    policy = list(
      artifact_scope = "libraries",
      correlation_threshold = 0.99,
      correlation_operator = ">",
      prune = list(
        derivative = list(cross_class = FALSE, min_n = 10L),
        nobaseline = list(cross_class = FALSE, min_n = 10L)
      ),
      medoids = FALSE,
      models = FALSE
    ),
    artifacts = artifacts,
    assessments = data.table::data.table(
      path = normalizePath(assessments_path, winslash = "/", mustWork = TRUE),
      size = as.numeric(file.info(assessments_path)$size),
      checksum_algorithm = "sha256",
      checksum = .reference_sha256(assessments_path)
    ),
    quarantine = data.table::data.table(
      path = normalizePath(quarantine_path, winslash = "/", mustWork = TRUE),
      removals = as.integer(nrow(result$quarantine$removals)),
      size = as.numeric(file.info(quarantine_path)$size),
      checksum_algorithm = "sha256",
      checksum = .reference_sha256(quarantine_path)
    )
  )
  promote <- .reference_internal(".lib_promote_rds")
  manifest_path <- file.path(release_dir, "dataset_manifest.rds")
  manifest_row <- promote(manifest, manifest_path)
  pointer <- list(
    schema = "OpenSpecy_dataset_library_pointer_v1",
    slug = slug,
    source_checksum = manifest$source$checksum,
    build_signature = manifest$build_signature,
    manifest_path = normalizePath(
      manifest_path, winslash = "/", mustWork = TRUE
    ),
    manifest_checksum = as.character(manifest_row$checksum)
  )
  .reference_internal(".lib_atomic_saveRDS")(
    pointer, file.path(output_dir, "current_dataset_manifest.rds")
  )
  rm(result)
  gc(verbose = FALSE, full = TRUE)
  manifest
}

reference_validate_dataset_manifest <- function(manifest, input_path = NULL,
                                                slug = NULL,
                                                verify_hashes = TRUE) {
  if (!is.list(manifest) ||
      !identical(manifest$schema, "OpenSpecy_dataset_library_manifest_v1") ||
      !is.character(manifest$slug) || length(manifest$slug) != 1L ||
      !is.data.frame(manifest$source) || nrow(manifest$source) != 1L ||
      !is.data.frame(manifest$artifacts) || !nrow(manifest$artifacts)) {
    stop("Dataset library manifest is malformed", call. = FALSE)
  }
  if (!is.null(slug) && !identical(manifest$slug, slug)) {
    stop("Dataset library manifest slug differs: ", slug, call. = FALSE)
  }
  if (!is.null(input_path)) {
    observed <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
    if (!identical(as.character(manifest$source$path), observed)) {
      stop("Dataset library manifest source path differs: ", manifest$slug,
           call. = FALSE)
    }
  }
  expected_recipes <- c("raw", "derivative", "nobaseline")
  if (!all(expected_recipes %in% manifest$artifacts$recipe) ||
      anyDuplicated(manifest$artifacts[, c("recipe", "spectrum_type")])) {
    stop("Dataset library manifest recipe/type coverage is incomplete: ",
         manifest$slug, call. = FALSE)
  }
  paths <- c(
    as.character(manifest$source$path),
    as.character(manifest$artifacts$path),
    as.character(manifest$assessments$path),
    as.character(manifest$quarantine$path)
  )
  checksums <- c(
    as.character(manifest$source$checksum),
    as.character(manifest$artifacts$checksum),
    as.character(manifest$assessments$checksum),
    as.character(manifest$quarantine$checksum)
  )
  if (any(!file.exists(paths))) {
    stop("Dataset library manifest path is missing: ", manifest$slug,
         call. = FALSE)
  }
  if (isTRUE(verify_hashes)) {
    actual <- unname(vapply(paths, .reference_sha256, character(1L)))
    if (!identical(actual, checksums)) {
      stop("Dataset library manifest checksum differs: ", manifest$slug,
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

reference_read_dataset_manifest <- function(input_path, slug,
                                            verify_hashes = TRUE) {
  output_dir <- file.path(dirname(input_path), "OpenSpecy_reference_library")
  pointer_path <- file.path(output_dir, "current_dataset_manifest.rds")
  if (!file.exists(pointer_path)) {
    stop("Dataset library pointer is missing: ", slug, call. = FALSE)
  }
  pointer <- .reference_read_rds(pointer_path)
  if (!is.list(pointer) ||
      !identical(pointer$schema, "OpenSpecy_dataset_library_pointer_v1") ||
      !identical(pointer$slug, slug) ||
      !file.exists(pointer$manifest_path) ||
      !identical(.reference_sha256(pointer$manifest_path),
                 pointer$manifest_checksum)) {
    stop("Dataset library pointer is invalid: ", slug, call. = FALSE)
  }
  manifest <- .reference_read_rds(pointer$manifest_path)
  reference_validate_dataset_manifest(
    manifest, input_path = input_path, slug = slug,
    verify_hashes = verify_hashes
  )
  manifest
}

.reference_join_typed_paths <- function(paths, expected_counts, source_slugs,
                                        recipe, type, release_dir, signature,
                                        progress = TRUE) {
  if (!length(paths) || any(!file.exists(paths))) {
    stop("Final join requires existing typed artifact paths", call. = FALSE)
  }
  if (length(expected_counts) != length(paths) ||
      length(source_slugs) != length(paths) ||
      anyNA(source_slugs) || any(!nzchar(source_slugs)) ||
      anyDuplicated(source_slugs)) {
    stop("Final join requires one unique dataset slug per typed artifact path",
         call. = FALSE)
  }
  input_hashes <- vapply(paths, .reference_sha256, character(1L))
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", paste(recipe, type, sep = "_"))
  checkpoint_dir <- file.path(dirname(dirname(release_dir)), "checkpoints")
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  staging_dir <- tempfile(
    paste0("join-", safe, "-"),
    tmpdir = checkpoint_dir
  )
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(staging_dir)) {
    stop("Could not create final-join staging directory", call. = FALSE)
  }
  on.exit({
    normalized <- normalizePath(staging_dir, winslash = "/", mustWork = FALSE)
    parent <- normalizePath(checkpoint_dir, winslash = "/", mustWork = FALSE)
    if (startsWith(normalized, paste0(parent, "/join-")) &&
        dir.exists(staging_dir)) {
      unlink(staging_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  build_core <- .reference_internal(".lib_build_core")
  core <- build_core(
    x = paths,
    recipes = stats::setNames(list(list()), recipe),
    range = "full",
    res = 6,
    id_col = "sample_name",
    exclude_ids = NULL,
    dedupe = FALSE,
    dedupe_correlation_threshold = NULL,
    metadata_lookups = NULL,
    material_hierarchy = NULL,
    metadata_name_lookup = NULL,
    clean_metadata_values = FALSE,
    convert_intensity = FALSE,
    restrict_range_args = NULL,
    signal_noise = FALSE,
    assess = FALSE,
    prune = NULL,
    progress = progress,
    .recipe_staging_dir = staging_dir
  )
  if (.reference_internal(".lib_is_staged_recipe_collection")(core)) {
    object <- .reference_internal(".lib_materialize_staged_recipe")(
      core$artifacts[[recipe]]
    )
  } else {
    object <- core[[recipe]]
  }
  rm(core)
  gc(verbose = FALSE, full = TRUE)
  if (!.reference_valid_object(object)) {
    stop("Final joined type is not a valid OpenSpecy object: ",
         recipe, "/", type, call. = FALSE)
  }
  observed_type <- unique(tolower(as.character(object$metadata$spectrum_type)))
  observed_type <- observed_type[!is.na(observed_type) & nzchar(observed_type)]
  if (length(observed_type) != 1L || !identical(observed_type, type)) {
    stop("Final joined type has inconsistent spectrum_type: ",
         recipe, "/", type, call. = FALSE)
  }
  expected_counts <- as.numeric(expected_counts)
  if (anyNA(expected_counts) || any(!is.finite(expected_counts)) ||
      any(expected_counts < 1) || any(expected_counts != floor(expected_counts))) {
    stop("Final join requires positive integer source counts", call. = FALSE)
  }
  expected <- sum(expected_counts)
  if (!identical(as.numeric(ncol(object$spectra)), expected)) {
    stop("Final join changed the retained spectrum count for ",
         recipe, "/", type, call. = FALSE)
  }
  source_dataset <- rep(source_slugs, times = as.integer(expected_counts))
  ids <- as.character(object$metadata$sample_name)
  if (length(source_dataset) != length(ids)) {
    stop("Final join source provenance is not aligned with retained spectra: ",
         recipe, "/", type, call. = FALSE)
  }
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop("Final joined type requires nonempty sample_name IDs: ",
         recipe, "/", type, call. = FALSE)
  }
  source_starts <- cumsum(c(1L, head(as.integer(expected_counts), -1L)))
  source_ends <- cumsum(as.integer(expected_counts))
  local_duplicates <- vapply(seq_along(source_starts), function(i) {
    anyDuplicated(ids[source_starts[[i]]:source_ends[[i]]]) > 0L
  }, logical(1L))
  if (any(local_duplicates)) {
    stop("Final joined type contains dataset-local duplicate sample_name IDs: ",
         paste(source_slugs[local_duplicates], collapse = ", "), call. = FALSE)
  }
  temporary_ids <- make.unique(ids, sep = ".")
  if (!identical(temporary_ids, colnames(object$spectra))) {
    stop("Final joined type has misaligned staged sample_name IDs: ",
         recipe, "/", type, call. = FALSE)
  }
  object$metadata$source_dataset <- source_dataset
  object$metadata$source_sample_name <- ids
  collision <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  collision_groups <- unique(ids[collision])
  if (any(collision)) {
    ids[collision] <- paste(source_dataset[collision], ids[collision], sep = "::")
    object$metadata$sample_name <- ids
    colnames(object$spectra) <- ids
  }
  if (anyDuplicated(ids) || !identical(ids, colnames(object$spectra))) {
    stop("Dataset-qualified final sample_name IDs are not globally unique: ",
         recipe, "/", type, call. = FALSE)
  }
  object <- .reference_internal(".lib_slim_reference_object")(object)
  target <- file.path(release_dir, paste0(recipe, "_", type, ".rds"))
  promoted <- .reference_internal(".lib_promote_rds")(object, target)
  final_wavenumbers <- length(object$wavenumber)
  metadata_columns <- ncol(object$metadata)
  rm(object)
  gc(verbose = FALSE, full = TRUE)
  if (!identical(unname(vapply(paths, .reference_sha256, character(1L))),
                 unname(input_hashes))) {
    stop("Final-join input changed while it was being consumed: ",
         recipe, "/", type, call. = FALSE)
  }
  data.table::data.table(
    recipe = recipe,
    spectrum_type = type,
    spectra = as.integer(expected),
    wavenumbers = as.integer(final_wavenumbers),
    metadata_columns = as.integer(metadata_columns),
    sources = as.integer(length(paths)),
    path = as.character(promoted$path),
    size = as.numeric(promoted$size),
    checksum_algorithm = "sha256",
    checksum = as.character(promoted$checksum),
    join_signature = signature,
    dedupe = FALSE,
    cross_dataset_removals = 0L,
    cross_dataset_id_collision_groups = as.integer(length(collision_groups)),
    cross_dataset_id_collision_rows = as.integer(sum(collision)),
    id_collision_policy =
      "qualify_colliding_sample_name_with_dataset_slug_v1"
  )
}

reference_join_dataset_manifests <- function(manifests, output_dir,
                                             progress = TRUE) {
  if (!is.list(manifests) || !length(manifests)) {
    stop("Final join requires one or more dataset manifests", call. = FALSE)
  }
  manifest_slugs <- vapply(manifests, `[[`, character(1L), "slug")
  if (anyNA(manifest_slugs) || any(!nzchar(manifest_slugs)) ||
      anyDuplicated(manifest_slugs)) {
    stop("Final join requires globally unique dataset manifest slugs",
         call. = FALSE)
  }
  invisible(lapply(manifests, reference_validate_dataset_manifest,
                   verify_hashes = FALSE))
  source_rows <- data.table::rbindlist(lapply(manifests, function(manifest) {
    data.table::copy(manifest$artifacts)[, slug := manifest$slug]
  }), fill = TRUE)
  data.table::setcolorder(
    source_rows, c("slug", setdiff(names(source_rows), "slug"))
  )
  signature <- digest::digest(list(
    schema = "OpenSpecy_final_library_join_v2",
    artifact_checksums = source_rows[, .(
      slug, recipe, spectrum_type, spectra, checksum
    )],
    policy = list(
      range = "full", res = 6, dedupe = FALSE,
      cross_dataset_removals = FALSE,
      id_collision_policy =
        "qualify_colliding_sample_name_with_dataset_slug_v1",
      medoids = FALSE, models = FALSE
    )
  ), algo = "sha256")
  release_dir <- file.path(output_dir, "releases", substr(signature, 1L, 12L))
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "checkpoints"), recursive = TRUE,
             showWarnings = FALSE)

  groups <- split(
    seq_len(nrow(source_rows)),
    paste(source_rows$recipe, source_rows$spectrum_type, sep = "\r")
  )
  typed_rows <- lapply(groups, function(rows) {
    group <- source_rows[rows]
    .reference_join_typed_paths(
      paths = as.character(group$path),
      expected_counts = group$spectra,
      source_slugs = as.character(group$slug),
      recipe = group$recipe[[1L]],
      type = group$spectrum_type[[1L]],
      release_dir = release_dir,
      signature = signature,
      progress = progress
    )
  })
  typed <- data.table::rbindlist(typed_rows, fill = TRUE)
  data.table::setorder(typed, recipe, spectrum_type)

  promote <- .reference_internal(".lib_promote_rds")
  recipe_rows <- lapply(unique(typed$recipe), function(recipe_name) {
    rows <- typed[recipe == recipe_name]
    objects <- lapply(rows$path, .reference_read_rds)
    names(objects) <- rows$spectrum_type
    if (!all(vapply(objects, .reference_valid_object, logical(1L)))) {
      stop("Final recipe bundle contains an invalid typed object: ", recipe_name,
           call. = FALSE)
    }
    promoted <- promote(
      objects, file.path(release_dir, paste0(recipe_name, ".rds"))
    )
    spectra <- sum(vapply(objects, function(x) ncol(x$spectra), integer(1L)))
    rm(objects)
    gc(verbose = FALSE, full = TRUE)
    data.table::data.table(
      recipe = recipe_name,
      spectra = as.integer(spectra),
      types = nrow(rows),
      path = as.character(promoted$path),
      size = as.numeric(promoted$size),
      checksum_algorithm = "sha256",
      checksum = as.character(promoted$checksum)
    )
  })
  recipes <- data.table::rbindlist(recipe_rows, fill = TRUE)
  data.table::setorder(recipes, recipe)
  index <- list(
    schema = "OpenSpecy_final_reference_libraries_v1",
    build_signature = signature,
    policy = list(
      artifact_scope = "libraries",
      final_join_dedupe = FALSE,
      cross_dataset_removals = FALSE,
      medoids = FALSE,
      models = FALSE
    ),
    sources = source_rows,
    typed_artifacts = typed,
    libraries = recipes,
    dataset_manifests = stats::setNames(lapply(manifests, function(x) {
      list(
        slug = x$slug,
        build_signature = x$build_signature,
        source_path = x$source$path,
        source_checksum = x$source$checksum,
        source_spectra = x$source$spectra,
        assessments_path = x$assessments$path,
        assessments_checksum = x$assessments$checksum,
        quarantine_path = x$quarantine$path,
        quarantine_checksum = x$quarantine$checksum,
        quarantine_removals = x$quarantine$removals
      )
    }), vapply(manifests, `[[`, character(1L), "slug"))
  )
  index_row <- promote(
    index, file.path(release_dir, "reference_library_build.rds")
  )
  pointer <- list(
    schema = "OpenSpecy_final_reference_library_pointer_v1",
    build_signature = signature,
    release_dir = normalizePath(release_dir, winslash = "/", mustWork = TRUE),
    index_path = as.character(index_row$path),
    index_checksum = as.character(index_row$checksum)
  )
  .reference_internal(".lib_atomic_saveRDS")(
    pointer, file.path(output_dir, "current_reference_library.rds")
  )
  index
}

reference_freeze_overhaul_contract <- function(admitted, manifest_path,
                                               output_dir, package_dir,
                                               comparator_dir,
                                               probe_evidence_path) {
  required <- c(
    "slug", "raw_sha256", "output_path", "output_sha256", "cleaner_path",
    "cleaner_sha256"
  )
  if (!is.data.frame(admitted) || !all(required %in% names(admitted)) ||
      !nrow(admitted)) {
    stop("Admitted intake rows are incomplete for the frozen contract",
         call. = FALSE)
  }
  if (!is.character(probe_evidence_path) ||
      length(probe_evidence_path) != 1L ||
      !file.exists(probe_evidence_path)) {
    stop("Frozen representative-probe evidence does not exist",
         call. = FALSE)
  }
  code_paths <- sort(unique(c(
    file.path(package_dir, c("DESCRIPTION", "NAMESPACE")),
    list.files(
      file.path(package_dir, "R"), pattern = "[.]R$", full.names = TRUE
    ),
    file.path(package_dir, c(
      "workflows/reference_intake.R",
      "workflows/reference_library_staging.R",
      "workflows/OpenSpecy_reference_library.R"
    ))
  )))
  workflow_paths <- list.files(
    file.path(package_dir, "workflows", "data"),
    pattern = "[.]csv$", full.names = TRUE
  )
  consumed <- c(manifest_path, code_paths, workflow_paths,
                probe_evidence_path)
  if (any(!file.exists(consumed))) {
    stop("Frozen contract input is missing: ",
         paste(consumed[!file.exists(consumed)], collapse = ", "),
         call. = FALSE)
  }
  if (!dir.exists(comparator_dir)) {
    stop("Frozen comparator release does not exist: ", comparator_dir,
         call. = FALSE)
  }
  comparator_paths <- list.files(
    comparator_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE,
    no.. = TRUE
  )
  comparator_paths <- comparator_paths[file.exists(comparator_paths) &
    !file.info(comparator_paths)$isdir]
  if (!length(comparator_paths)) {
    stop("Frozen comparator release contains no files", call. = FALSE)
  }
  runtime_packages <- c("OpenSpecy", "data.table", "digest", "hdf5r")
  contract <- list(
    schema = "OpenSpecy_reference_overhaul_contract_v1",
    manifest = data.table::data.table(
      path = normalizePath(manifest_path, winslash = "/", mustWork = TRUE),
      checksum = .reference_sha256(manifest_path)
    ),
    admitted = admitted[, required, with = FALSE],
    consumed = data.table::data.table(
      path = normalizePath(consumed, winslash = "/", mustWork = TRUE),
      checksum = vapply(consumed, .reference_sha256, character(1L))
    ),
    output_dir = normalizePath(
      output_dir, winslash = "/", mustWork = FALSE
    ),
    comparator = data.table::data.table(
      path = normalizePath(
        comparator_paths, winslash = "/", mustWork = TRUE
      ),
      checksum = vapply(comparator_paths, .reference_sha256, character(1L))
    ),
    representative_probes = list(
      path = normalizePath(
        probe_evidence_path, winslash = "/", mustWork = TRUE
      ),
      checksum = .reference_sha256(probe_evidence_path),
      evidence = .reference_read_rds(probe_evidence_path)
    ),
    policy = list(
      datasets = nrow(admitted),
      source_hash_block = 20000L,
      correlation_query_block = 256L,
      correlation_candidate_block = 5000L,
      merge_block = 5000L,
      recipe_block = 5000L,
      metadata_join_block = 100000L,
      diagnostic_value_limit = 1000L,
      gc_interval = 10L,
      staged_completion_threshold = 250000L,
      storage_mode = "file_backed",
      correlation_threshold = 0.99,
      correlation_operator = ">",
      final_join_dedupe = FALSE,
      maximum_ram_fraction = 0.80,
      minimum_free_disk_bytes = 75 * 1024^3,
      no_progress_seconds = 600L,
      runtime_overrun_multiplier = 2,
      changed_contract_requires_fresh_target = TRUE,
      artifact_scope = "libraries",
      medoids = FALSE,
      models = FALSE
    ),
    runtime = list(
      R = R.version.string,
      platform = R.version$platform,
      packages = stats::setNames(vapply(runtime_packages, function(package) {
        if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
        as.character(utils::packageVersion(package))
      }, character(1L)), runtime_packages)
    )
  )
  signature <- digest::digest(contract, algo = "sha256")
  contract$signature <- signature
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(output_dir, "frozen_contract.rds")
  if (file.exists(path)) {
    existing <- .reference_read_rds(path)
    if (!identical(existing, contract)) {
      stop("Frozen overhaul contract changed; use a fresh output target",
           call. = FALSE)
    }
  } else {
    .reference_internal(".lib_atomic_saveRDS")(contract, path)
  }
  contract
}
