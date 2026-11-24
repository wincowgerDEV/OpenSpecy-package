# Create an immutable metadata-only child of a completed reference release.
#
# Direct use (defaults target the completed updatedv1 r7 release):
# Rscript workflows/patch_reference_library_metadata.R
#
# Optional positional arguments are parent pointer, admitted manifest,
# canonical dictionary, and output root. The workflow changes only
# library_name and spectrum_origin; it never filters spectra or builds medoids
# or models.

.reference_patch_chain <- function(state, value) {
  digest::digest(list(state = state, value = value), algo = "sha256")
}

.reference_patch_resource_guard <- function(output_dir, policy, stage) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    stop("The metadata patch requires the 'ps' package for resource guards",
         call. = FALSE)
  }
  memory <- ps::ps_system_memory()
  disk <- ps::ps_disk_usage(output_dir)
  used_fraction <- as.numeric(memory$percent) / 100
  available <- as.numeric(disk$available[[1L]])
  row <- data.table::data.table(
    observed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stage = as.character(stage), memory_used_fraction = used_fraction,
    memory_available_bytes = as.numeric(memory$avail),
    disk_available_bytes = available
  )
  if (used_fraction > policy$maximum_ram_fraction) {
    stop(sprintf(
      "Metadata patch RAM guard exceeded at %s (%.1f%% > %.1f%%)",
      stage, 100 * used_fraction, 100 * policy$maximum_ram_fraction
    ), call. = FALSE)
  }
  if (available < policy$minimum_free_disk_bytes) {
    stop(sprintf(
      "Metadata patch disk guard exceeded at %s (%.2f GiB < %.2f GiB)",
      stage, available / 1024^3,
      policy$minimum_free_disk_bytes / 1024^3
    ), call. = FALSE)
  }
  row
}

reference_metadata_patch_fingerprint <- function(
    object, metadata_block = 100000L, spectral_block = 5000L,
    target_columns = .reference_metadata_target_columns) {
  if (!.reference_valid_object(object)) {
    stop("Metadata patch fingerprint requires a valid OpenSpecy object",
         call. = FALSE)
  }
  metadata_block <- as.integer(metadata_block)
  spectral_block <- as.integer(spectral_block)
  if (anyNA(c(metadata_block, spectral_block)) || metadata_block < 1L ||
      spectral_block < 1L) {
    stop("Fingerprint blocks must be positive integers", call. = FALSE)
  }
  spectra <- object$spectra
  spectral_state <- digest::digest(list(
    schema = "OpenSpecy_spectral_fingerprint_v1",
    dimensions = dim(spectra), type = typeof(spectra),
    row_names = rownames(spectra), column_names = colnames(spectra)
  ), algo = "sha256")
  spectral_starts <- seq.int(1L, ncol(spectra), by = spectral_block)
  for (start in spectral_starts) {
    end <- min(ncol(spectra), start + spectral_block - 1L)
    spectral_state <- .reference_patch_chain(
      spectral_state, spectra[, start:end, drop = FALSE]
    )
  }
  metadata <- object$metadata
  untouched <- setdiff(names(metadata), target_columns)
  metadata_state <- digest::digest(list(
    schema = "OpenSpecy_untouched_metadata_fingerprint_v1",
    names = untouched,
    classes = lapply(metadata[, untouched, with = FALSE], class),
    rows = nrow(metadata)
  ), algo = "sha256")
  metadata_starts <- seq.int(1L, nrow(metadata), by = metadata_block)
  for (start in metadata_starts) {
    end <- min(nrow(metadata), start + metadata_block - 1L)
    block <- as.data.frame(
      metadata[start:end, untouched, with = FALSE], stringsAsFactors = FALSE
    )
    metadata_state <- .reference_patch_chain(metadata_state, block)
  }
  list(
    wavenumber_sha256 = digest::digest(object$wavenumber, algo = "sha256"),
    spectra_sha256 = spectral_state,
    ids_sha256 = digest::digest(colnames(spectra), algo = "sha256"),
    attributes_sha256 = digest::digest(attributes(object), algo = "sha256"),
    untouched_metadata_sha256 = metadata_state,
    spectra = as.integer(ncol(spectra)),
    wavenumbers = as.integer(length(object$wavenumber)),
    metadata_rows = as.integer(nrow(metadata)),
    spectral_blocks = as.integer(length(spectral_starts)),
    metadata_blocks = as.integer(length(metadata_starts))
  )
}

.reference_patch_equal_fingerprint <- function(before, after) {
  fields <- c(
    "wavenumber_sha256", "spectra_sha256", "ids_sha256",
    "attributes_sha256", "untouched_metadata_sha256", "spectra",
    "wavenumbers", "metadata_rows"
  )
  identical(before[fields], after[fields])
}

.reference_patch_artifact_inventory <- function(index) {
  typed <- data.table::copy(index$typed_artifacts)
  typed[, `:=`(artifact_kind = "typed", artifact_key = paste(
    recipe, spectrum_type, sep = "/"
  ))]
  bundled <- data.table::copy(index$libraries)
  bundled[, `:=`(
    artifact_kind = "bundle", artifact_key = as.character(recipe),
    spectrum_type = NA_character_
  )]
  columns <- c(
    "artifact_kind", "artifact_key", "recipe", "spectrum_type", "path",
    "size", "checksum_algorithm", "checksum"
  )
  data.table::rbindlist(list(typed[, ..columns], bundled[, ..columns]),
                        fill = TRUE)
}

reference_freeze_metadata_patch_contract <- function(
    parent_pointer_path, admitted_manifest_path, dictionary_path, output_dir,
    code_paths, metadata_block = 100000L, spectral_block = 5000L,
    minimum_free_disk_bytes = 50 * 1024^3) {
  paths <- c(
    parent_pointer_path, admitted_manifest_path, dictionary_path, code_paths
  )
  if (any(!file.exists(paths))) {
    stop("Metadata patch contract input is missing: ",
         paste(paths[!file.exists(paths)], collapse = ", "), call. = FALSE)
  }
  parent_pointer_path <- normalizePath(
    parent_pointer_path, winslash = "/", mustWork = TRUE
  )
  pointer <- .reference_read_rds(parent_pointer_path)
  if (!is.list(pointer) ||
      !identical(pointer$schema,
                 "OpenSpecy_final_reference_library_pointer_v1") ||
      !file.exists(pointer$index_path) ||
      !identical(.reference_sha256(pointer$index_path),
                 pointer$index_checksum)) {
    stop("Parent reference-library pointer is invalid", call. = FALSE)
  }
  index <- .reference_read_rds(pointer$index_path)
  if (!is.list(index) ||
      !identical(index$build_signature, pointer$build_signature) ||
      !is.data.frame(index$typed_artifacts) ||
      !is.data.frame(index$libraries)) {
    stop("Parent reference-library index is invalid", call. = FALSE)
  }
  parent_artifacts <- .reference_patch_artifact_inventory(index)
  if (any(!file.exists(parent_artifacts$path))) {
    stop("Parent reference-library artifact is missing", call. = FALSE)
  }
  actual <- vapply(
    parent_artifacts$path, .reference_sha256, character(1L), USE.NAMES = FALSE
  )
  if (!identical(actual, as.character(parent_artifacts$checksum))) {
    stop("Parent reference-library artifact checksum differs", call. = FALSE)
  }
  admitted <- data.table::fread(
    admitted_manifest_path, encoding = "UTF-8", na.strings = c("", "NA")
  )
  if ("state" %in% names(admitted)) admitted <- admitted[state == "admitted"]
  mapping <- reference_dataset_metadata(admitted, dictionary_path)
  contract <- list(
    schema = "OpenSpecy_reference_metadata_patch_contract_v1",
    parent = list(
      pointer_path = parent_pointer_path,
      pointer_checksum = .reference_sha256(parent_pointer_path),
      build_signature = pointer$build_signature,
      index_path = normalizePath(
        pointer$index_path, winslash = "/", mustWork = TRUE
      ),
      index_checksum = pointer$index_checksum,
      artifacts = parent_artifacts
    ),
    inputs = data.table::data.table(
      path = normalizePath(paths[-1L], winslash = "/", mustWork = TRUE),
      checksum = vapply(paths[-1L], .reference_sha256, character(1L))
    ),
    mapping = mapping,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    policy = list(
      rule_version = .reference_metadata_rule_version,
      target_columns = .reference_metadata_target_columns,
      allowed_spectrum_origin = c("Observed", "Computed"),
      metadata_block = as.integer(metadata_block),
      spectral_fingerprint_block = as.integer(spectral_block),
      maximum_ram_fraction = 0.80,
      minimum_free_disk_bytes = as.numeric(minimum_free_disk_bytes),
      no_progress_seconds = 600L,
      parent_mutation = FALSE,
      filtering = FALSE, pruning = FALSE, medoids = FALSE, models = FALSE
    )
  )
  signature <- digest::digest(contract, algo = "sha256")
  contract$signature <- signature
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  contract_path <- file.path(output_dir, "frozen_metadata_patch_contract.rds")
  if (file.exists(contract_path)) {
    existing <- .reference_read_rds(contract_path)
    if (!identical(existing, contract)) {
      stop("Metadata patch contract changed; use a fresh output target",
           call. = FALSE)
    }
  } else {
    .reference_internal(".lib_atomic_saveRDS")(contract, contract_path)
  }
  contract
}

.reference_patch_checkpoint <- function(path, signature, parent_checksum) {
  if (!file.exists(path)) return(NULL)
  checkpoint <- .reference_read_rds(path)
  if (!is.list(checkpoint) ||
      !identical(checkpoint$schema,
                 "OpenSpecy_reference_metadata_patch_checkpoint_v1") ||
      !identical(checkpoint$signature, signature) ||
      !identical(checkpoint$parent_checksum, parent_checksum) ||
      !is.data.frame(checkpoint$row) || nrow(checkpoint$row) != 1L ||
      !file.exists(checkpoint$row$path[[1L]]) ||
      !identical(.reference_sha256(checkpoint$row$path[[1L]]),
                 checkpoint$row$checksum[[1L]])) {
    stop("Metadata patch checkpoint is incompatible: ", path, call. = FALSE)
  }
  checkpoint
}

.reference_patch_typed_artifact <- function(
    parent_row, mapping, release_dir, checkpoint_dir, signature,
    metadata_block, spectral_block, progress = TRUE) {
  key <- paste(parent_row$recipe[[1L]], parent_row$spectrum_type[[1L]],
               sep = "_")
  checkpoint_path <- file.path(checkpoint_dir, paste0("typed_", key, ".rds"))
  checkpoint <- .reference_patch_checkpoint(
    checkpoint_path, signature, parent_row$checksum[[1L]]
  )
  if (!is.null(checkpoint)) {
    if (isTRUE(progress)) message("Reusing metadata patch: ", key)
    return(checkpoint)
  }
  if (isTRUE(progress)) message("Patching typed artifact: ", key)
  input_path <- parent_row$path[[1L]]
  if (!identical(.reference_sha256(input_path), parent_row$checksum[[1L]])) {
    stop("Parent typed artifact changed before patch: ", key, call. = FALSE)
  }
  object <- .reference_read_rds(input_path)
  before <- reference_metadata_patch_fingerprint(
    object, metadata_block = metadata_block, spectral_block = spectral_block
  )
  normalized <- reference_apply_dataset_metadata(
    object, dataset_metadata = mapping, block_size = metadata_block
  )
  telemetry <- normalized$telemetry
  object <- normalized$object
  rm(normalized)
  reference_validate_dataset_metadata_values(object, mapping)
  target <- file.path(release_dir, paste0(key, ".rds"))
  promoted <- .reference_internal(".lib_promote_rds")(object, target)
  rm(object)
  gc(verbose = FALSE, full = TRUE)
  restored <- .reference_read_rds(promoted$path)
  reference_validate_dataset_metadata_values(restored, mapping)
  after <- reference_metadata_patch_fingerprint(
    restored, metadata_block = metadata_block, spectral_block = spectral_block
  )
  if (!.reference_patch_equal_fingerprint(before, after)) {
    stop("Metadata patch changed protected content: ", key, call. = FALSE)
  }
  row <- data.table::copy(parent_row)
  row[, `:=`(
    path = as.character(promoted$path), size = as.numeric(promoted$size),
    checksum = as.character(promoted$checksum),
    parent_checksum = as.character(parent_row$checksum[[1L]]),
    metadata_rule_version = .reference_metadata_rule_version,
    metadata_rows = telemetry$rows,
    metadata_chunks = telemetry$chunks,
    metadata_largest_block = telemetry$largest_block,
    metadata_full_copy = FALSE,
    changed_library_name = telemetry$changed_library_name,
    changed_spectrum_origin = telemetry$changed_spectrum_origin,
    protected_wavenumber_sha256 = after$wavenumber_sha256,
    protected_spectra_sha256 = after$spectra_sha256,
    protected_ids_sha256 = after$ids_sha256,
    protected_attributes_sha256 = after$attributes_sha256,
    protected_metadata_sha256 = after$untouched_metadata_sha256
  )]
  checkpoint <- list(
    schema = "OpenSpecy_reference_metadata_patch_checkpoint_v1",
    signature = signature,
    parent_checksum = parent_row$checksum[[1L]],
    row = row, before = before, after = after
  )
  .reference_internal(".lib_atomic_saveRDS")(checkpoint, checkpoint_path)
  rm(restored)
  gc(verbose = FALSE, full = TRUE)
  checkpoint
}

.reference_patch_recipe_bundle <- function(
    recipe_name, typed_rows, mapping, release_dir, checkpoint_dir, signature,
    parent_row, metadata_block, spectral_block, progress = TRUE) {
  checkpoint_path <- file.path(
    checkpoint_dir, paste0("bundle_", recipe_name, ".rds")
  )
  checkpoint <- .reference_patch_checkpoint(
    checkpoint_path, signature, parent_row$checksum[[1L]]
  )
  if (!is.null(checkpoint)) {
    if (isTRUE(progress)) message("Reusing metadata bundle: ", recipe_name)
    return(checkpoint)
  }
  if (isTRUE(progress)) message("Writing metadata bundle: ", recipe_name)
  data.table::setorder(typed_rows, spectrum_type)
  objects <- lapply(typed_rows$path, .reference_read_rds)
  names(objects) <- typed_rows$spectrum_type
  invisible(lapply(objects, reference_validate_dataset_metadata_values,
                   dataset_metadata = mapping))
  promoted <- .reference_internal(".lib_promote_rds")(
    objects, file.path(release_dir, paste0(recipe_name, ".rds"))
  )
  spectra <- sum(vapply(objects, function(x) ncol(x$spectra), integer(1L)))
  rm(objects)
  gc(verbose = FALSE, full = TRUE)
  restored <- .reference_read_rds(promoted$path)
  if (!identical(sort(names(restored)), sort(typed_rows$spectrum_type))) {
    stop("Metadata patch bundle type coverage differs: ", recipe_name,
         call. = FALSE)
  }
  for (type in names(restored)) {
    reference_validate_dataset_metadata_values(restored[[type]], mapping)
    fingerprint <- reference_metadata_patch_fingerprint(
      restored[[type]], metadata_block = metadata_block,
      spectral_block = spectral_block
    )
    expected <- typed_rows[spectrum_type == type]
    observed <- c(
      fingerprint$wavenumber_sha256, fingerprint$spectra_sha256,
      fingerprint$ids_sha256, fingerprint$attributes_sha256,
      fingerprint$untouched_metadata_sha256
    )
    protected <- as.character(unlist(expected[1L, .(
      protected_wavenumber_sha256, protected_spectra_sha256,
      protected_ids_sha256, protected_attributes_sha256,
      protected_metadata_sha256
    )], use.names = FALSE))
    if (!identical(observed, protected)) {
      stop("Metadata patch bundle differs from typed artifact: ",
           recipe_name, "/", type, call. = FALSE)
    }
  }
  row <- data.table::data.table(
    recipe = recipe_name, spectra = as.integer(spectra),
    types = as.integer(length(restored)), path = as.character(promoted$path),
    size = as.numeric(promoted$size), checksum_algorithm = "sha256",
    checksum = as.character(promoted$checksum),
    parent_checksum = parent_row$checksum[[1L]],
    metadata_rule_version = .reference_metadata_rule_version
  )
  checkpoint <- list(
    schema = "OpenSpecy_reference_metadata_patch_checkpoint_v1",
    signature = signature,
    parent_checksum = parent_row$checksum[[1L]], row = row
  )
  .reference_internal(".lib_atomic_saveRDS")(checkpoint, checkpoint_path)
  rm(restored)
  gc(verbose = FALSE, full = TRUE)
  checkpoint
}

reference_patch_joined_library <- function(contract, progress = TRUE) {
  if (!is.list(contract) ||
      !identical(contract$schema,
                 "OpenSpecy_reference_metadata_patch_contract_v1")) {
    stop("Metadata patch contract is invalid", call. = FALSE)
  }
  signature <- contract$signature
  unsigned <- contract
  unsigned$signature <- NULL
  if (!identical(digest::digest(unsigned, algo = "sha256"), signature)) {
    stop("Metadata patch contract signature differs", call. = FALSE)
  }
  output_dir <- contract$output_dir
  release_dir <- file.path(output_dir, "releases", substr(signature, 1L, 12L))
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  review_dir <- file.path(output_dir, "review")
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  pointer_target <- file.path(output_dir, "current_reference_library.rds")
  if (file.exists(pointer_target)) {
    existing_pointer <- .reference_read_rds(pointer_target)
    if (identical(existing_pointer$build_signature, signature) &&
        file.exists(existing_pointer$index_path) &&
        identical(.reference_sha256(existing_pointer$index_path),
                  existing_pointer$index_checksum)) {
      return(.reference_read_rds(existing_pointer$index_path))
    }
    stop("Metadata patch output contains an incompatible current pointer",
         call. = FALSE)
  }
  if (!identical(.reference_sha256(contract$parent$pointer_path),
                 contract$parent$pointer_checksum) ||
      !identical(.reference_sha256(contract$parent$index_path),
                 contract$parent$index_checksum)) {
    stop("Metadata patch parent pointer or index changed", call. = FALSE)
  }
  parent_index <- .reference_read_rds(contract$parent$index_path)
  mapping <- data.table::as.data.table(data.table::copy(contract$mapping))
  parent_typed <- data.table::as.data.table(parent_index$typed_artifacts)
  parent_bundles <- data.table::as.data.table(parent_index$libraries)
  resource_telemetry <- list()
  record_resources <- function(stage) {
    resource_telemetry[[length(resource_telemetry) + 1L]] <<-
      .reference_patch_resource_guard(output_dir, contract$policy, stage)
  }
  typed_checkpoints <- lapply(
    seq_len(nrow(parent_typed)), function(i) {
      record_resources(paste0(
        "before_typed_", parent_typed$recipe[[i]], "_",
        parent_typed$spectrum_type[[i]]
      ))
      .reference_patch_typed_artifact(
        parent_row = data.table::copy(parent_typed[i]),
        mapping = mapping, release_dir = release_dir,
        checkpoint_dir = checkpoint_dir, signature = signature,
        metadata_block = contract$policy$metadata_block,
        spectral_block = contract$policy$spectral_fingerprint_block,
        progress = progress
      )
    }
  )
  typed <- data.table::rbindlist(
    lapply(typed_checkpoints, `[[`, "row"), fill = TRUE
  )
  data.table::setorder(typed, recipe, spectrum_type)
  bundle_checkpoints <- lapply(unique(typed$recipe), function(recipe_name) {
    record_resources(paste0("before_bundle_", recipe_name))
    parent_row <- data.table::copy(parent_bundles[recipe == recipe_name])
    .reference_patch_recipe_bundle(
      recipe_name = recipe_name,
      typed_rows = data.table::copy(typed[recipe == recipe_name]),
      mapping = mapping, release_dir = release_dir,
      checkpoint_dir = checkpoint_dir, signature = signature,
      parent_row = parent_row,
      metadata_block = contract$policy$metadata_block,
      spectral_block = contract$policy$spectral_fingerprint_block,
      progress = progress
    )
  })
  recipes <- data.table::rbindlist(
    lapply(bundle_checkpoints, `[[`, "row"), fill = TRUE
  )
  data.table::setorder(recipes, recipe)
  record_resources("before_parent_rehash")
  parent_after <- vapply(
    contract$parent$artifacts$path, .reference_sha256, character(1L),
    USE.NAMES = FALSE
  )
  if (!identical(parent_after,
                 as.character(contract$parent$artifacts$checksum))) {
    stop("Metadata patch mutated or raced with a parent artifact",
         call. = FALSE)
  }
  child_index <- parent_index
  child_index$build_signature <- signature
  child_index$parent_build_signature <- contract$parent$build_signature
  child_index$policy$metadata_patch <- list(
    enabled = TRUE, rule_version = contract$policy$rule_version,
    target_columns = contract$policy$target_columns,
    filtering = FALSE, pruning = FALSE, medoids = FALSE, models = FALSE,
    dataset_native_on_next_full_build = TRUE
  )
  child_index$typed_artifacts <- typed
  child_index$libraries <- recipes
  child_index$canonical_dataset_metadata <- mapping
  contract_path <- file.path(output_dir, "frozen_metadata_patch_contract.rds")
  child_index$metadata_patch_contract <- list(
    path = normalizePath(contract_path, winslash = "/", mustWork = TRUE),
    checksum = .reference_sha256(contract_path)
  )
  index_row <- .reference_internal(".lib_promote_rds")(
    child_index, file.path(release_dir, "reference_library_build.rds")
  )
  audit <- list(
    schema = "OpenSpecy_reference_metadata_patch_audit_v1",
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    signature = signature,
    parent_build_signature = contract$parent$build_signature,
    mapping = mapping,
    typed = typed,
    bundles = recipes,
    resource_telemetry = data.table::rbindlist(resource_telemetry),
    parent_artifacts_unchanged = TRUE,
    prohibited_stages = list(
      filtering = FALSE, pruning = FALSE, medoids = FALSE, models = FALSE
    )
  )
  .reference_internal(".lib_atomic_saveRDS")(
    audit, file.path(review_dir, "metadata_patch_audit.rds")
  )
  data.table::fwrite(mapping, file.path(review_dir, "metadata_mapping.csv"))
  data.table::fwrite(typed, file.path(review_dir, "metadata_patch_typed.csv"))
  data.table::fwrite(recipes,
                     file.path(review_dir, "metadata_patch_bundles.csv"))
  pointer <- list(
    schema = "OpenSpecy_final_reference_library_pointer_v1",
    build_signature = signature,
    release_dir = normalizePath(release_dir, winslash = "/", mustWork = TRUE),
    index_path = as.character(index_row$path),
    index_checksum = as.character(index_row$checksum),
    parent_build_signature = contract$parent$build_signature
  )
  .reference_internal(".lib_atomic_saveRDS")(pointer, pointer_target)
  child_index
}

reference_metadata_patch_main <- function() {
  command <- commandArgs(FALSE)
  script_argument <- command[grepl("^--file=", command)]
  if (!length(script_argument)) {
    stop("Cannot resolve patch script path", call. = FALSE)
  }
  script_path <- normalizePath(
    sub("^--file=", "", script_argument[[1L]]),
    winslash = "/", mustWork = TRUE
  )
  package_dir <- dirname(dirname(script_path))
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install 'devtools' before running the metadata patch.",
         call. = FALSE)
  }
  devtools::load_all(package_dir, quiet = TRUE)
  sys.source(
    file.path(package_dir, "workflows", "reference_library_staging.R"),
    envir = globalenv()
  )
  args <- commandArgs(trailingOnly = TRUE)
  parent_pointer <- if (length(args) >= 1L) args[[1L]] else file.path(
    package_dir, "rawrefdata", "updatedv1", "Processed",
    "reference-library-overhaul-r7", "current_reference_library.rds"
  )
  admitted_manifest <- if (length(args) >= 2L) args[[2L]] else file.path(
    package_dir, "rawrefdata", "updatedv1", "datasets.csv"
  )
  dictionary <- if (length(args) >= 3L) args[[3L]] else file.path(
    package_dir, "workflows", "data", "reference_dataset_metadata.csv"
  )
  output_dir <- if (length(args) >= 4L) args[[4L]] else file.path(
    package_dir, "rawrefdata", "updatedv1", "Processed",
    "reference-library-overhaul-r7-metadata-v1"
  )
  code_paths <- c(
    script_path,
    file.path(package_dir, "workflows", "reference_library_staging.R"),
    file.path(package_dir, "DESCRIPTION"),
    file.path(package_dir, "R", "build_lib.R"),
    file.path(package_dir, "R", "build_lib_staging.R")
  )
  contract <- reference_freeze_metadata_patch_contract(
    parent_pointer_path = parent_pointer,
    admitted_manifest_path = admitted_manifest,
    dictionary_path = dictionary,
    output_dir = output_dir,
    code_paths = code_paths
  )
  message("Metadata patch signature: ", contract$signature)
  message("Parent build signature: ", contract$parent$build_signature)
  message("Output root: ", output_dir)
  result <- reference_patch_joined_library(contract, progress = TRUE)
  message("Metadata-only child release complete: ",
          result$build_signature)
  invisible(result)
}

if (sys.nframe() == 0L) {
  reference_metadata_patch_main()
}
