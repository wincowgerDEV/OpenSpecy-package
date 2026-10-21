.lib_recipe_record_staging_enabled <- function() {
  value <- getOption("OpenSpecy.recipe_record_staging", FALSE)
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("Option 'OpenSpecy.recipe_record_staging' must be TRUE or FALSE",
         call. = FALSE)
  }
  value
}

.lib_recipe_chunk_size <- function() {
  value <- suppressWarnings(as.integer(getOption(
    "OpenSpecy.recipe_chunk_size", 20000L
  )))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop("Option 'OpenSpecy.recipe_chunk_size' must be one positive integer",
         call. = FALSE)
  }
  value
}

.lib_compact_staged_metadata_enabled <- function() {
  value <- getOption("OpenSpecy.compact_staged_metadata", FALSE)
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("Option 'OpenSpecy.compact_staged_metadata' must be TRUE or FALSE",
         call. = FALSE)
  }
  value
}

.lib_compact_staged_metadata <- function(metadata) {
  metadata <- data.table::as.data.table(metadata)
  if (!.lib_compact_staged_metadata_enabled() || !nrow(metadata)) {
    return(list(
      metadata = metadata,
      report = data.table::data.table(
        columns = 0L, bytes_before = as.numeric(object.size(metadata)),
        bytes_after = as.numeric(object.size(metadata))
      )
    ))
  }
  protected <- c(
    "sample_name", "spectrum_identity", "organization", "user_name",
    "material", "library_name", "spectrum_type"
  )
  candidates <- setdiff(
    names(metadata)[vapply(metadata, is.character, logical(1L))],
    protected
  )
  before <- as.numeric(object.size(metadata))
  compacted <- character()
  for (column in candidates) {
    levels_n <- data.table::uniqueN(metadata[[column]], na.rm = TRUE)
    if (levels_n <= 65535L &&
        levels_n <= max(256L, floor(0.1 * nrow(metadata)))) {
      data.table::set(
        metadata, j = column, value = factor(metadata[[column]])
      )
      compacted <- c(compacted, column)
    }
  }
  after <- as.numeric(object.size(metadata))
  list(
    metadata = metadata,
    report = data.table::data.table(
      columns = as.integer(length(compacted)),
      column_names = paste(compacted, collapse = "; "),
      bytes_before = before, bytes_after = after,
      bytes_saved = before - after
    )
  )
}

.lib_expand_staged_metadata <- function(metadata) {
  metadata <- data.table::as.data.table(metadata)
  factors <- names(metadata)[vapply(metadata, is.factor, logical(1L))]
  for (column in factors) {
    data.table::set(metadata, j = column,
                    value = as.character(metadata[[column]]))
  }
  metadata
}

.lib_save_quarantine_part_paths <- function(parts, directory, label) {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(list())
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) {
    stop("Could not create staged quarantine-part directory", call. = FALSE)
  }
  start <- length(list.files(directory, pattern = "[.]rds$"))
  out <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    path <- file.path(
      directory, sprintf("part-%06d.rds", start + i)
    )
    .lib_atomic_saveRDS(parts[[i]], path)
    out[[i]] <- structure(list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      sha256 = .lib_sha256_file(path), label = paste0(label, "_", i)
    ), class = c("OpenSpecyQuarantinePartPath", "list"))
  }
  names(out) <- vapply(out, `[[`, character(1L), "label")
  out
}

.lib_read_quarantine_part_path <- function(x) {
  if (!inherits(x, "OpenSpecyQuarantinePartPath") ||
      !is.character(x$path) || length(x$path) != 1L ||
      !file.exists(x$path) ||
      !identical(.lib_sha256_file(x$path), x$sha256)) {
    stop("Staged quarantine-part path is missing or changed", call. = FALSE)
  }
  object <- readRDS(x$path)
  if (!is_OpenSpecy(object)) {
    stop("Staged quarantine-part path does not contain OpenSpecy",
         call. = FALSE)
  }
  object
}

.lib_is_staged_recipe_collection <- function(x) {
  inherits(x, "OpenSpecyStagedRecipeCollection") &&
    is.list(x$artifacts) && length(x$artifacts) > 0L
}

.lib_validate_staged_recipe_artifact <- function(x, strong = FALSE) {
  required <- c(
    "schema", "recipe", "spectra_path", "metadata_path", "dataset",
    "wavenumber", "spectrum_names", "attributes", "spectra", "chunks",
    "chunk_size", "max_chunk_columns"
  )
  if (!inherits(x, "OpenSpecyStagedRecipeArtifact") ||
      !all(required %in% names(x)) ||
      !identical(x$schema, "OpenSpecy_staged_recipe_v1") ||
      !is.character(x$recipe) || length(x$recipe) != 1L ||
      !is.character(x$spectra_path) || length(x$spectra_path) != 1L ||
      !is.character(x$metadata_path) || length(x$metadata_path) != 1L ||
      !is.character(x$dataset) || length(x$dataset) != 1L ||
      !is.numeric(x$wavenumber) || !length(x$wavenumber) ||
      !is.character(x$spectrum_names) ||
      length(x$spectrum_names) != x$spectra || anyDuplicated(x$spectrum_names) ||
      !is.list(x$attributes) || !is.numeric(x$spectra) || x$spectra < 1L ||
      !is.numeric(x$chunks) || x$chunks < 1L ||
      !is.numeric(x$chunk_size) || x$chunk_size < 1L ||
      !is.numeric(x$max_chunk_columns) || x$max_chunk_columns < 1L) {
    stop("Staged recipe artifact is malformed", call. = FALSE)
  }
  paths <- c(x$spectra_path, x$metadata_path)
  if (any(!file.exists(paths))) {
    stop("Staged recipe artifact member is missing: ",
         paste(paths[!file.exists(paths)], collapse = ", "), call. = FALSE)
  }
  if (isTRUE(strong)) {
    observed <- unname(vapply(paths, .lib_sha256_file, character(1L)))
    expected <- c(x$spectra_sha256, x$metadata_sha256)
    if (!identical(observed, expected)) {
      stop("Staged recipe artifact content hash changed", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.lib_recipe_blocks <- function(n, chunk_size) {
  starts <- seq.int(1L, n, by = chunk_size)
  lapply(starts, function(start) seq.int(start, min(n, start + chunk_size - 1L)))
}

.lib_stage_recipe_collection <- function(
    x, recipes, staging_dir, signal_noise, assess, report) {
  if (!is_OpenSpecy(x)) {
    stop("Recipe staging requires one OpenSpecy object", call. = FALSE)
  }
  if (!is.list(recipes) || is.null(names(recipes)) ||
      any(names(recipes) == "") || anyDuplicated(names(recipes))) {
    stop("'recipes' must be a uniquely named list", call. = FALSE)
  }
  if (isTRUE(assess)) {
    stop("File-backed recipe staging does not yet support 'assess = TRUE'",
         call. = FALSE)
  }
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(staging_dir)) {
    stop("Could not create the recipe staging directory", call. = FALSE)
  }

  chunk_size <- .lib_recipe_chunk_size()
  blocks <- .lib_recipe_blocks(ncol(x$spectra), chunk_size)
  handles <- vector("list", length(recipes))
  datasets <- vector("list", length(recipes))
  names(handles) <- names(datasets) <- names(recipes)
  paths <- stats::setNames(file.path(
    staging_dir, paste0("recipe-", make.names(names(recipes)), ".h5")
  ), names(recipes))
  metadata_paths <- stats::setNames(file.path(
    staging_dir, paste0("recipe-", make.names(names(recipes)), "-metadata.rds")
  ), names(recipes))
  close_handles <- function() {
    for (handle in handles) {
      if (!is.null(handle)) try(handle$close_all(), silent = TRUE)
    }
  }
  on.exit(close_handles(), add = TRUE)
  for (recipe_name in names(recipes)) {
    if (file.exists(paths[[recipe_name]])) unlink(paths[[recipe_name]])
    handle <- hdf5r::H5File$new(paths[[recipe_name]], mode = "w")
    handles[[recipe_name]] <- handle
    datasets[[recipe_name]] <- handle$create_dataset(
      "spectra", dtype = hdf5r::h5types$H5T_IEEE_F64LE,
      dims = c(nrow(x$spectra), ncol(x$spectra)),
      chunk_dims = c(nrow(x$spectra), min(chunk_size, ncol(x$spectra))),
      gzip_level = 1L
    )
  }

  metadata <- data.table::as.data.table(x$metadata)
  had_sn <- "sn" %in% names(metadata)
  original_sn <- if (had_sn) metadata[["sn"]] else NULL
  sn_restored <- FALSE
  restore_sn <- function() {
    if (sn_restored || !isTRUE(signal_noise)) return(invisible(NULL))
    if (had_sn) {
      data.table::set(metadata, j = "sn", value = original_sn)
    } else if ("sn" %in% names(metadata)) {
      metadata[, sn := NULL]
    }
    sn_restored <<- TRUE
    invisible(NULL)
  }
  on.exit(restore_sn(), add = TRUE)
  recipe_attributes <- stats::setNames(vector("list", length(recipes)),
                                       names(recipes))
  sn <- if (isTRUE(signal_noise)) {
    stats::setNames(lapply(recipes, function(...) {
      rep(NA_real_, ncol(x$spectra))
    }), names(recipes))
  } else NULL
  started <- proc.time()[["elapsed"]]
  for (block_index in seq_along(blocks)) {
    columns <- blocks[[block_index]]
    source_block <- .subset_open_specy_columns(x, columns)
    for (recipe_name in names(recipes)) {
      recipe <- recipes[[recipe_name]]
      processed <- if (is.function(recipe)) {
        recipe(source_block)
      } else if (length(recipe) == 0L) {
        source_block
      } else {
        if (!is.list(recipe)) {
          stop("Each recipe must be a function or named argument list",
               call. = FALSE)
        }
        do.call(process_spec, c(list(source_block), recipe))
      }
      if (!is_OpenSpecy(processed) ||
          !identical(processed$wavenumber, x$wavenumber) ||
          !identical(dim(processed$spectra),
                     c(nrow(x$spectra), length(columns)))) {
        stop("File-backed recipe '", recipe_name,
             "' must preserve the staged wavenumber axis and spectrum count",
             call. = FALSE)
      }
      datasets[[recipe_name]][, columns] <- processed$spectra
      recipe_attributes[[recipe_name]] <- attributes(processed)[c(
        "intensity_unit", "derivative_order", "baseline", "spectra_type"
      )]
      if (isTRUE(signal_noise)) {
        sn[[recipe_name]][columns] <- sig_noise(processed, step = 10)
      }
      rm(processed)
    }
    rm(source_block)
    gc(verbose = FALSE)
    if (block_index == 1L || block_index == length(blocks) ||
        block_index %% 10L == 0L) {
      report(sprintf(
        "staged recipe block %d/%d (columns=%d)",
        block_index, length(blocks), length(columns)
      ))
    }
  }
  for (recipe_name in names(recipes)) {
    if (isTRUE(signal_noise)) {
      data.table::set(metadata, j = "sn", value = sn[[recipe_name]])
    }
    saveRDS(
      metadata, metadata_paths[[recipe_name]],
      compress = FALSE, version = 3
    )
    handles[[recipe_name]]$flush()
  }
  restore_sn()
  close_handles()
  handles[] <- list(NULL)

  artifacts <- lapply(names(recipes), function(recipe_name) {
    if (nrow(x$metadata) != ncol(x$spectra)) {
      stop("Staged recipe metadata row count changed", call. = FALSE)
    }
    structure(list(
      schema = "OpenSpecy_staged_recipe_v1",
      recipe = recipe_name,
      spectra_path = normalizePath(paths[[recipe_name]], winslash = "/",
                                   mustWork = TRUE),
      metadata_path = normalizePath(metadata_paths[[recipe_name]],
                                    winslash = "/", mustWork = TRUE),
      dataset = "spectra",
      wavenumber = as.numeric(x$wavenumber),
      spectrum_names = colnames(x$spectra),
      attributes = recipe_attributes[[recipe_name]],
      spectra = ncol(x$spectra), chunks = length(blocks),
      chunk_size = chunk_size,
      max_chunk_columns = max(lengths(blocks)),
      spectra_bytes = as.numeric(file.info(paths[[recipe_name]])$size),
      metadata_bytes = as.numeric(file.info(metadata_paths[[recipe_name]])$size),
      spectra_sha256 = .lib_sha256_file(paths[[recipe_name]]),
      metadata_sha256 = .lib_sha256_file(metadata_paths[[recipe_name]])
    ), class = c("OpenSpecyStagedRecipeArtifact", "list"))
  })
  names(artifacts) <- names(recipes)
  out <- structure(list(
    schema = "OpenSpecy_staged_recipe_collection_v1",
    artifacts = artifacts,
    telemetry = data.table::rbindlist(lapply(artifacts, function(artifact) {
      data.table::data.table(
        recipe = artifact$recipe, spectra = artifact$spectra,
        wavenumbers = length(artifact$wavenumber), chunks = artifact$chunks,
        chunk_size = artifact$chunk_size,
        max_chunk_columns = artifact$max_chunk_columns,
        spectra_bytes = artifact$spectra_bytes,
        metadata_bytes = artifact$metadata_bytes,
        elapsed_seconds = proc.time()[["elapsed"]] - started
      )
    }))
  ), class = c("OpenSpecyStagedRecipeCollection", "list"))
  for (artifact in out$artifacts) {
    .lib_validate_staged_recipe_artifact(artifact, strong = TRUE)
  }
  out
}

.lib_read_staged_recipe_block <- function(artifact, columns, rows = NULL) {
  .lib_validate_staged_recipe_artifact(artifact)
  columns <- as.integer(columns)
  if (!length(columns) || anyNA(columns) || min(columns) < 1L ||
      max(columns) > artifact$spectra) {
    stop("Staged recipe columns are outside the artifact", call. = FALSE)
  }
  if (is.null(rows)) rows <- seq_along(artifact$wavenumber)
  rows <- as.integer(rows)
  if (!length(rows) || anyNA(rows) || min(rows) < 1L ||
      max(rows) > length(artifact$wavenumber)) {
    stop("Staged recipe rows are outside the artifact", call. = FALSE)
  }
  handle <- hdf5r::H5File$new(artifact$spectra_path, mode = "r")
  on.exit(try(handle$close_all(), silent = TRUE), add = TRUE)
  value <- handle[[artifact$dataset]][rows, columns, drop = FALSE]
  if (!is.matrix(value)) {
    value <- matrix(value, nrow = length(rows))
  }
  colnames(value) <- artifact$spectrum_names[columns]
  value
}

.lib_materialize_staged_recipe <- function(artifact, columns = NULL,
                                            rows = NULL) {
  .lib_validate_staged_recipe_artifact(artifact, strong = TRUE)
  if (is.null(columns)) columns <- seq_len(artifact$spectra)
  columns <- as.integer(columns)
  if (is.null(rows)) rows <- seq_along(artifact$wavenumber)
  rows <- as.integer(rows)
  blocks <- split(
    columns, ceiling(seq_along(columns) / artifact$chunk_size)
  )
  spectra <- matrix(
    NA_real_, nrow = length(rows), ncol = length(columns),
    dimnames = list(NULL, artifact$spectrum_names[columns])
  )
  starts <- cumsum(c(1L, head(lengths(blocks), -1L)))
  for (i in seq_along(blocks)) {
    local <- starts[[i]] + seq_along(blocks[[i]]) - 1L
    spectra[, local] <- .lib_read_staged_recipe_block(
      artifact, blocks[[i]], rows = rows
    )
  }
  metadata <- readRDS(artifact$metadata_path)[columns]
  out <- .lib_make_open_specy(
    artifact$wavenumber[rows], spectra, metadata, artifact$attributes,
    copy_metadata = FALSE
  ) |>
    as_OpenSpecy()
  if (is.list(artifact$object_attributes)) {
    for (name in names(artifact$object_attributes)) {
      if (!is.null(artifact$object_attributes[[name]])) {
        attr(out, name) <- artifact$object_attributes[[name]]
      }
    }
  }
  out
}

.lib_materialize_staged_recipe_collection <- function(x) {
  if (!.lib_is_staged_recipe_collection(x)) {
    stop("Expected a staged recipe collection", call. = FALSE)
  }
  out <- lapply(x$artifacts, .lib_materialize_staged_recipe)
  attr(out, "quarantine_parts") <- x$quarantine_parts
  attr(out, "recipe_staging_report") <- x$telemetry
  out
}

.lib_quarantine_shard_writer <- function(directory) {
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) ||
      !nzchar(directory)) {
    stop("Quarantine shard directory must be one nonempty path", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) {
    stop("Could not create the quarantine shard directory", call. = FALSE)
  }
  shards <- list()
  ledgers <- list()
  counter <- 0L

  store <- function(object, label = NULL) {
    if (is.null(object)) return(invisible(NULL))
    if (!is_OpenSpecy(object) ||
        !isTRUE(suppressWarnings(check_OpenSpecy(object)))) {
      stop("Quarantine shards must contain valid OpenSpecy objects",
           call. = FALSE)
    }
    source <- if ("quarantine_source" %in% names(object$metadata)) {
      as.character(object$metadata$quarantine_source)
    } else {
      rep(NA_character_, nrow(object$metadata))
    }
    for (column in c("library_name", "organization", "user_name", "citation")) {
      if (!column %in% names(object$metadata)) next
      candidate <- as.character(object$metadata[[column]])
      fill <- is.na(source) | !nzchar(trimws(source))
      usable <- !is.na(candidate) & nzchar(trimws(candidate))
      source[fill & usable] <- candidate[fill & usable]
    }
    source[is.na(source) | !nzchar(trimws(source))] <- "(unknown_source)"
    object$metadata$quarantine_source <- source
    required <- c(
      "quarantine_key", "quarantine_source", "quarantine_artifact",
      "quarantine_stage", "quarantine_rule", "quarantine_reason"
    )
    .lib_require_cols(object$metadata, required, "quarantine shard metadata")
    counter <<- counter + 1L
    shard_name <- sprintf("quarantine-%06d.rds", counter)
    path <- file.path(directory, shard_name)
    .lib_atomic_saveRDS(object, path)
    checksum <- .lib_sha256_file(path)
    bytes <- as.numeric(file.info(path)$size)
    quarantine_object <- if (is.null(label) || !nzchar(label)) {
      tools::file_path_sans_ext(shard_name)
    } else {
      paste0(gsub("[^A-Za-z0-9_.-]+", "_", label), "_", counter)
    }
    descriptor <- structure(list(
      schema = "OpenSpecy_quarantine_shard_v1",
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      spectra = as.integer(ncol(object$spectra)),
      wavenumbers = as.integer(nrow(object$spectra)),
      bytes = bytes, sha256 = checksum,
      quarantine_object = quarantine_object
    ), class = c("OpenSpecyQuarantineShard", "list"))
    ledger <- data.table::copy(object$metadata)
    data.table::set(ledger, j = "quarantine_object",
                    value = quarantine_object)
    shards[[quarantine_object]] <<- descriptor
    ledgers[[quarantine_object]] <<- ledger
    invisible(descriptor)
  }

  capture <- function(x, rows, artifact, stage, rule, reason,
                      spectrum_type = NULL, evidence = list(),
                      source_rows = NULL) {
    parts <- .lib_quarantine_typed_capture(
      x, rows, artifact = artifact, stage = stage, rule = rule,
      reason = reason, evidence = evidence, source_rows = source_rows
    )
    for (name in names(parts)) store(parts[[name]], name)
    invisible(length(parts))
  }

  bundle <- function(conflicts = NULL, source_failures = NULL,
                     build_signature = NA_character_, thresholds = numeric()) {
    removals <- data.table::rbindlist(ledgers, fill = TRUE)
    if (!nrow(removals)) removals <- .lib_quarantine_ledger_schema()
    if (anyDuplicated(removals$quarantine_key)) {
      stop("Quarantine shard ledger contains duplicate disposition keys",
           call. = FALSE)
    }
    if (is.null(conflicts) || !nrow(conflicts)) {
      conflicts <- .lib_prune_conflict_edge_schema()
    } else {
      conflicts <- data.table::as.data.table(data.table::copy(conflicts))
    }
    if (is.null(source_failures) || !nrow(source_failures)) {
      source_failures <- .lib_source_failure_schema()
    } else {
      source_failures <- data.table::as.data.table(
        data.table::copy(source_failures)
      )
    }
    reasons <- if (nrow(removals)) {
      removals[, .(spectra = .N), by = .(
        quarantine_stage, quarantine_rule, quarantine_reason
      )][order(quarantine_stage, quarantine_rule, quarantine_reason)]
    } else {
      data.table::data.table(
        quarantine_stage = character(), quarantine_rule = character(),
        quarantine_reason = character(), spectra = integer()
      )
    }
    observed_thresholds <- if (
        nrow(removals) && "correlation_threshold" %in% names(removals)
      ) {
      as.numeric(removals$correlation_threshold)
    } else numeric()
    thresholds <- sort(unique(c(
      as.numeric(thresholds),
      observed_thresholds[is.finite(observed_thresholds)]
    )))
    out <- structure(list(
      spectra = shards, removals = removals, conflicts = conflicts,
      source_failures = source_failures,
      manifest = list(
        schema = "OpenSpecy_quarantined_spectra_v3",
        build_signature = as.character(build_signature),
        thresholds = thresholds,
        source_hashes = as.character(build_signature),
        removed_spectra = as.integer(nrow(removals)),
        shards = as.integer(length(shards)), removal_reasons = reasons
      )
    ), class = c("OpenSpecyQuarantine", "list"))
    .lib_validate_quarantine(out)
    out
  }

  list(store = store, capture = capture, bundle = bundle)
}

.lib_stage_artifact_writer <- function(path, metadata_path, wavenumber,
                                       capacity, chunk_size, recipe,
                                       attributes = list()) {
  if (capacity < 1L) {
    stop("A staged artifact writer needs positive capacity", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) unlink(path)
  if (file.exists(metadata_path)) unlink(metadata_path)
  handle <- hdf5r::H5File$new(path, mode = "w")
  space <- hdf5r::H5S$new(
    dims = c(length(wavenumber), capacity),
    maxdims = c(length(wavenumber), Inf)
  )
  dataset <- handle$create_dataset(
    "spectra", dtype = hdf5r::h5types$H5T_IEEE_F64LE,
    space = space,
    chunk_dims = c(length(wavenumber), min(chunk_size, capacity)),
    gzip_level = 1L
  )
  written <- 0L
  metadata_parts <- list()
  part_i <- 0L
  closed <- FALSE

  append <- function(spectra, metadata) {
    if (!is.matrix(spectra)) spectra <- as.matrix(spectra)
    metadata <- data.table::as.data.table(metadata)
    if (nrow(spectra) != length(wavenumber) ||
        ncol(spectra) != nrow(metadata)) {
      stop("Staged spectra and metadata are not aligned", call. = FALSE)
    }
    if (!ncol(spectra)) return(invisible(NULL))
    columns <- written + seq_len(ncol(spectra))
    if (max(columns) > capacity) {
      stop("Staged artifact exceeded its declared capacity", call. = FALSE)
    }
    dataset[, columns] <- spectra
    written <<- max(columns)
    part_i <<- part_i + 1L
    metadata_parts[[part_i]] <<- data.table::copy(metadata)
    invisible(columns)
  }

  finalize <- function(object_attributes = list()) {
    if (closed) stop("Staged artifact writer is already closed", call. = FALSE)
    if (written < 1L) {
      try(handle$close_all(), silent = TRUE)
      closed <<- TRUE
      stop("Staged processing removed every spectrum from ", recipe,
           call. = FALSE)
    }
    dataset$set_extent(c(length(wavenumber), written))
    handle$flush()
    handle$close_all()
    closed <<- TRUE
    metadata <- data.table::rbindlist(metadata_parts, fill = TRUE)
    if (nrow(metadata) != written) {
      stop("Final staged metadata count does not match spectra", call. = FALSE)
    }
    spectrum_names <- if ("sample_name" %in% names(metadata)) {
      as.character(metadata$sample_name)
    } else if ("col_id" %in% names(metadata)) {
      as.character(metadata$col_id)
    } else {
      paste0("spectrum_", seq_len(written))
    }
    if (anyNA(spectrum_names) || any(!nzchar(spectrum_names)) ||
        anyDuplicated(spectrum_names)) {
      stop("Final staged spectrum names must be nonblank and unique",
           call. = FALSE)
    }
    saveRDS(metadata, metadata_path, compress = FALSE, version = 3)
    structure(list(
      schema = "OpenSpecy_staged_recipe_v1", recipe = recipe,
      spectra_path = normalizePath(path, winslash = "/", mustWork = TRUE),
      metadata_path = normalizePath(
        metadata_path, winslash = "/", mustWork = TRUE
      ),
      dataset = "spectra", wavenumber = as.numeric(wavenumber),
      spectrum_names = spectrum_names, attributes = attributes,
      spectra = as.integer(written),
      chunks = as.integer(ceiling(written / chunk_size)),
      chunk_size = as.integer(chunk_size),
      max_chunk_columns = as.integer(min(chunk_size, written)),
      spectra_bytes = as.numeric(file.info(path)$size),
      metadata_bytes = as.numeric(file.info(metadata_path)$size),
      spectra_sha256 = .lib_sha256_file(path),
      metadata_sha256 = .lib_sha256_file(metadata_path),
      object_attributes = object_attributes
    ), class = c("OpenSpecyStagedRecipeArtifact", "list"))
  }

  close <- function() {
    if (!closed) {
      try(handle$close_all(), silent = TRUE)
      closed <<- TRUE
    }
    invisible(NULL)
  }
  list(append = append, finalize = finalize, close = close)
}

.lib_staged_quality_block <- function(x, artifact,
                                      artifact_ratio = 2, tail_n = 5L,
                                      max_crop = 0.2,
                                      snr_threshold = 2) {
  ids <- .lib_ids(x, "sample_name")
  type <- tolower(as.character(x$metadata$spectrum_type))
  metrics <- .artifact_ratio_metrics(
    x, tail_n = tail_n, co2_region = c(2200, 2420),
    silent_region = c(2420, 2550)
  )
  co2 <- type == "ftir" & is.finite(metrics$co2_ratio) &
    metrics$co2_ratio > artifact_ratio
  co2[is.na(co2)] <- FALSE
  if (any(co2)) {
    corrected <- flatten_range(
      filter_spec(x, co2), min = 2200, max = 2420, make_rel = FALSE
    )
    x$spectra[, co2] <- corrected$spectra
  }
  co2_after <- .artifact_ratio_metrics(
    x, tail_n = tail_n, co2_region = c(2200, 2420),
    silent_region = c(2420, 2550)
  )$co2_ratio
  failed_co2 <- co2 & (!is.finite(co2_after) |
                         co2_after > artifact_ratio)
  failed_co2[is.na(failed_co2)] <- TRUE

  tail_before <- .artifact_ratio_metrics(
    x, tail_n = tail_n, co2_region = c(2200, 2420),
    silent_region = c(2420, 2550)
  )$tail_ratio
  tail_flag <- is.finite(tail_before) & tail_before > artifact_ratio
  tail_flag[is.na(tail_flag)] <- FALSE
  correction <- .lib_trim_high_tail_matrix(
    x$wavenumber, x$spectra[, tail_flag, drop = FALSE],
    artifact_ratio = artifact_ratio, tail_n = tail_n,
    max_crop = max_crop, co2_region = c(2200, 2420)
  )
  if (any(tail_flag)) x$spectra[, tail_flag] <- correction$spectra
  tail_after <- .artifact_ratio_metrics(
    x, tail_n = tail_n, co2_region = c(2200, 2420),
    silent_region = c(2420, 2550)
  )$tail_ratio
  failed_tail <- rep(FALSE, ncol(x$spectra))
  failed_tail[tail_flag] <- !correction$passed
  snr <- sig_noise(x, metric = "run_sig_over_noise", step = 10,
                   na.rm = TRUE)
  low_snr <- !is.finite(snr) | snr < snr_threshold
  remove <- failed_co2 | failed_tail | low_snr
  reason <- vapply(seq_along(remove), function(i) {
    paste(c(
      if (failed_co2[[i]]) "co2_postcondition_failed",
      if (failed_tail[[i]]) "high_tail_correction_failed",
      if (low_snr[[i]]) {
        if (is.finite(snr[[i]])) "running_snr_below_threshold" else
          "running_snr_unavailable"
      }
    ), collapse = "; ")
  }, character(1L))
  assessment <- data.table::rbindlist(list(
    data.table::data.table(
      artifact = artifact, spectrum_id = ids[co2], spectrum_type = type[co2],
      check = "co2_region", action = ifelse(failed_co2[co2], "drop", "flatten"),
      before_value = metrics$co2_ratio[co2], after_value = co2_after[co2],
      threshold = artifact_ratio, removed = failed_co2[co2],
      reason = ifelse(
        failed_co2[co2], "co2_postcondition_failed",
        "co2_to_silent_ratio_corrected"
      )
    ),
    data.table::data.table(
      artifact = artifact, spectrum_id = ids[tail_flag],
      spectrum_type = type[tail_flag], check = "high_tail",
      action = ifelse(correction$passed, "trim", "drop"),
      before_value = tail_before[tail_flag], after_value = tail_after[tail_flag],
      threshold = artifact_ratio, removed = !correction$passed,
      reason = correction$reason
    ),
    data.table::data.table(
      artifact = artifact, spectrum_id = ids[low_snr],
      spectrum_type = type[low_snr], check = "low_snr", action = "drop",
      before_value = as.numeric(snr[low_snr]), after_value = NA_real_,
      threshold = snr_threshold, removed = TRUE,
      reason = ifelse(
        is.finite(snr[low_snr]), "running_snr_below_threshold",
        "running_snr_unavailable"
      )
    )
  ), fill = TRUE)
  list(
    object = x, remove = remove, sn = as.numeric(snr), reason = reason,
    evidence = list(
      co2_ratio = co2_after, tail_ratio = tail_after,
      signal_to_noise = as.numeric(snr)
    ), assessment = assessment
  )
}

.lib_staged_metadata_proxy <- function(metadata, attributes = list()) {
  metadata <- data.table::copy(data.table::as.data.table(metadata))
  ids <- if ("sample_name" %in% names(metadata)) {
    as.character(metadata$sample_name)
  } else if ("col_id" %in% names(metadata)) {
    as.character(metadata$col_id)
  } else {
    paste0("spectrum_", seq_len(nrow(metadata)))
  }
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Staged metadata proxy requires stable unique spectrum IDs",
         call. = FALSE)
  }
  spectra <- matrix(
    0, nrow = 2L, ncol = nrow(metadata),
    dimnames = list(NULL, ids)
  )
  .lib_make_open_specy(
    c(1, 2), spectra, metadata, attributes, copy_metadata = FALSE
  ) |>
    as_OpenSpecy()
}

.lib_complete_staged_metadata <- function(metadata, attributes, tables,
                                           remove_other, report) {
  metadata <- .lib_expand_staged_metadata(
    data.table::copy(data.table::as.data.table(metadata))
  )
  metadata[, .OpenSpecy_build_source_row := seq_len(.N)]
  prediction <- predict_class_reference(
    metadata, tables$classes_regex, return = "report"
  )
  if (nrow(prediction$clashes)) {
    stop(
      "Class regex clashes require exact material entries: ",
      paste(head(prediction$clashes$spectrum_identity, 20L), collapse = ", "),
      call. = FALSE
    )
  }
  proxy <- .lib_staged_metadata_proxy(prediction$data, attributes)
  proxy <- join_material_hierarchy(proxy, tables$material_hierarchy)
  proxy <- .lib_complete_reference_classes(
    proxy, classes = tables$classes_reference,
    hierarchy = tables$material_hierarchy,
    enforce_other_limit = !isTRUE(remove_other)
  )
  library_type <- .lib_complete_reference_library_type(proxy$metadata)
  proxy$metadata <- library_type$metadata
  class_coverage <- attr(proxy, "class_coverage_report", exact = TRUE)
  completed_all <- data.table::copy(proxy$metadata)

  other <- .lib_apply_other_policy(
    list(staged = proxy), remove_other = remove_other, report = report
  )
  proxy <- other$libraries$staged
  other$libraries <- NULL
  other$quarantine_parts <- NULL
  retained <- as.integer(proxy$metadata$.OpenSpecy_build_source_row)

  proxy$metadata <- .lib_enrich_material_group(
    proxy$metadata, tables$material_group_reference
  )
  form <- .lib_enrich_material_form(
    proxy$metadata, tables$material_form_regex
  )
  proxy$metadata <- form$data
  use <- .lib_enrich_common_use(
    proxy$metadata, tables$common_use_reference
  )
  proxy$metadata <- use$data

  superseded_drop <- c(
    "x", "y", "xunits", "interpretation", "form_factor", "shape",
    "x_unit", "spectrumid", "locationdescription", "datatype"
  )
  optional_drop <- grepl("^assessment_", tables$metadata_drop$metadata_column)
  drop_status <- data.table::data.table(
    metadata_column = tables$metadata_drop$metadata_column,
    status = ifelse(
      tables$metadata_drop$metadata_column %in% names(proxy$metadata),
      "present",
      ifelse(
        tables$metadata_drop$metadata_column %in% superseded_drop,
        "superseded_absent",
        ifelse(optional_drop, "optional_absent", "stale_absent")
      )
    )
  )
  drop_cols <- intersect(
    tables$metadata_drop$metadata_column, names(proxy$metadata)
  )
  if (length(drop_cols)) proxy$metadata[, (drop_cols) := NULL]
  if (any(is.na(proxy$metadata$material_type) |
          !nzchar(trimws(proxy$metadata$material_type)))) {
    stop("Blank material_type values remain after reviewed class cleanup",
         call. = FALSE)
  }
  type_coverage <- data.table::data.table(
    field = c("library_type", "spectrum_type"),
    populated = c(
      sum(!is.na(proxy$metadata$library_type) &
            nzchar(proxy$metadata$library_type)),
      sum(!is.na(proxy$metadata$spectrum_type) &
            nzchar(proxy$metadata$spectrum_type))
    ),
    total = nrow(proxy$metadata),
    source_reported = c(
      library_type$assessment$source_reported, NA_integer_
    ),
    provenance_fallback = c(
      library_type$assessment$provenance_fallback, NA_integer_
    ),
    fallback_values = c(
      library_type$assessment$fallback_values, NA_character_
    )
  )
  if (any(type_coverage$populated != type_coverage$total)) {
    stop("Blank library_type or spectrum_type values remain after source lookup",
         call. = FALSE)
  }
  list(
    metadata = data.table::copy(proxy$metadata),
    metadata_all = completed_all,
    retained_source_rows = retained,
    prediction = prediction[c("summary", "predictions", "clashes", "overlaps")],
    class_coverage = class_coverage,
    library_type = library_type$assessment,
    type_coverage = type_coverage,
    other_review = other$review,
    other_filter = other$summary,
    material_form = form[c("summary", "matches", "clashes")],
    common_use = use[c("summary", "coverage")],
    metadata_drop = drop_status
  )
}

.lib_capture_staged_positions <- function(
    artifact, positions, metadata, quarantine_writer, stage, rule, reason,
    evidence = list(), report = NULL) {
  positions <- sort(unique(as.integer(positions)))
  if (!length(positions)) return(invisible(0L))
  blocks <- split(
    positions,
    ceiling(seq_along(positions) / artifact$chunk_size)
  )
  captured <- 0L
  for (block in blocks) {
    object <- .lib_make_open_specy(
      artifact$wavenumber,
      .lib_read_staged_recipe_block(artifact, block),
      data.table::copy(metadata[block]), artifact$attributes,
      copy_metadata = FALSE
    ) |>
      as_OpenSpecy()
    local_reason <- if (length(reason) == artifact$spectra) {
      reason[block]
    } else {
      reason
    }
    local_evidence <- lapply(evidence, function(value) {
      if (length(value) == artifact$spectra) value[block] else value
    })
    quarantine_writer$capture(
      object, seq_len(ncol(object$spectra)), artifact = artifact$recipe,
      stage = stage, rule = rule, reason = local_reason,
      evidence = local_evidence, source_rows = block
    )
    captured <- captured + length(block)
    rm(object)
    gc(verbose = FALSE)
  }
  if (is.function(report)) report(sprintf(
    "quarantined staged removals (%s/%s; spectra=%d)",
    artifact$recipe, stage, captured
  ))
  invisible(captured)
}

.lib_stage_completed_recipe <- function(
    artifact, completed, staging_dir, quarantine_writer, report) {
  .lib_validate_staged_recipe_artifact(artifact, strong = TRUE)
  recipe <- artifact$recipe
  retained_source_rows <- completed$retained_source_rows
  removed_other <- setdiff(seq_len(artifact$spectra), retained_source_rows)
  if (length(removed_other)) {
    reason <- rep("unresolved_other_policy", artifact$spectra)
    review <- completed$other_review[action == "removed"]
    if (nrow(review)) {
      reason[review$source_row] <- as.character(review$reason)
    }
    .lib_capture_staged_positions(
      artifact, removed_other, completed$metadata_all,
      quarantine_writer = quarantine_writer,
      stage = "other_policy", rule = "remove_other",
      reason = reason, report = report
    )
  }

  path <- file.path(staging_dir, paste0("completed-", recipe, ".h5"))
  metadata_path <- file.path(
    staging_dir, paste0("completed-", recipe, "-metadata.rds")
  )
  writer <- .lib_stage_artifact_writer(
    path, metadata_path, wavenumber = artifact$wavenumber,
    capacity = length(retained_source_rows),
    chunk_size = artifact$chunk_size, recipe = recipe,
    attributes = artifact$attributes
  )
  on.exit(writer$close(), add = TRUE)
  blocks <- split(
    seq_along(retained_source_rows),
    ceiling(seq_along(retained_source_rows) / artifact$chunk_size)
  )
  assessments <- list()
  assessment_i <- 0L
  retained_total <- 0L
  for (block_i in seq_along(blocks)) {
    output_rows <- blocks[[block_i]]
    source_rows <- retained_source_rows[output_rows]
    metadata <- data.table::copy(completed$metadata[output_rows])
    object <- .lib_make_open_specy(
      artifact$wavenumber,
      .lib_read_staged_recipe_block(artifact, source_rows),
      metadata, artifact$attributes, copy_metadata = FALSE
    ) |>
      as_OpenSpecy()
    remove <- rep(FALSE, ncol(object$spectra))
    if (recipe %in% c("derivative", "nobaseline")) {
      quality <- .lib_staged_quality_block(object, artifact = recipe)
      object <- quality$object
      remove <- quality$remove
      object$metadata$sn <- quality$sn
      if (nrow(quality$assessment)) {
        assessment_i <- assessment_i + 1L
        assessments[[assessment_i]] <- quality$assessment
      }
      if (any(remove)) {
        quarantine_writer$capture(
          object, remove, artifact = recipe, stage = "quality_control",
          rule = "processed_spectrum_quality", reason = quality$reason,
          evidence = quality$evidence, source_rows = source_rows
        )
      }
      object$spectra <- round(object$spectra, 3)
      finite_range <- apply(object$spectra, 2L, function(values) {
        values <- values[is.finite(values)]
        if (length(values)) diff(range(values)) else 0
      })
      flat <- (!is.finite(finite_range) | finite_range <= 0) & !remove
      if (any(flat)) {
        assessment_i <- assessment_i + 1L
        assessments[[assessment_i]] <- data.table::data.table(
          artifact = recipe,
          spectrum_id = as.character(.lib_ids(object, "sample_name"))[flat],
          spectrum_type = as.character(object$metadata$spectrum_type)[flat],
          check = "flat_spectrum", action = "drop",
          before_value = finite_range[flat], after_value = NA_real_,
          threshold = 0, removed = TRUE,
          reason = "post_transform_flat_spectrum"
        )
        quarantine_writer$capture(
          object, flat, artifact = recipe, stage = "post_transform",
          rule = "flat_spectrum", reason = "post_transform_flat_spectrum",
          evidence = list(observed_range = finite_range, threshold = 0),
          source_rows = source_rows
        )
        remove <- remove | flat
      }
    }
    keep <- !remove
    if (any(keep)) {
      writer$append(
        object$spectra[, keep, drop = FALSE], object$metadata[keep]
      )
      retained_total <- retained_total + sum(keep)
    }
    rm(object)
    gc(verbose = FALSE)
    if (block_i == 1L || block_i == length(blocks) || block_i %% 10L == 0L) {
      report(sprintf(
        "completed staged recipe block %d/%d (%s; retained=%d)",
        block_i, length(blocks), recipe, retained_total
      ))
    }
  }
  output <- writer$finalize(object_attributes = artifact$object_attributes)
  .lib_validate_staged_recipe_artifact(output, strong = TRUE)
  list(
    artifact = output,
    quality = if (length(assessments)) {
      data.table::rbindlist(assessments, fill = TRUE)
    } else .lib_quality_schema()
  )
}

.lib_drop_blank_staged_metadata <- function(metadata) {
  metadata <- data.table::copy(data.table::as.data.table(metadata))
  blank <- vapply(metadata, function(column) {
    if (is.character(column)) {
      all(is.na(column) | !nzchar(trimws(column)))
    } else {
      all(is.na(column))
    }
  }, logical(1L))
  blank[names(blank) %in% c("material_form", "common_use")] <- FALSE
  if (any(blank)) metadata[, (names(blank)[blank]) := NULL]
  metadata
}

.lib_finalize_staged_artifact_metadata <- function(artifact, label) {
  metadata <- .lib_drop_blank_staged_metadata(readRDS(artifact$metadata_path))
  before <- ncol(metadata)
  missing <- vapply(metadata, function(column) {
    as.integer(sum(is.na(column)))
  }, integer(1L))
  fully_missing <- if (nrow(metadata)) {
    names(missing)[missing == nrow(metadata)]
  } else character()
  fully_missing <- setdiff(fully_missing, c("material_form", "common_use"))
  if (length(fully_missing)) metadata[, (fully_missing) := NULL]
  remaining <- missing[setdiff(names(missing), fully_missing)]
  if (length(remaining)) {
    data.table::setcolorder(
      metadata, names(remaining)[order(remaining, seq_along(remaining))]
    )
  }
  metadata[, .OpenSpecy_build_source_row := NULL]
  saveRDS(metadata, artifact$metadata_path, compress = FALSE, version = 3)
  artifact$metadata_bytes <- as.numeric(file.info(artifact$metadata_path)$size)
  artifact$metadata_sha256 <- .lib_sha256_file(artifact$metadata_path)
  assessment <- data.table::data.table(
    artifact = label, spectra = as.integer(nrow(metadata)),
    before_columns = as.integer(before),
    after_columns = as.integer(ncol(metadata)),
    dropped_columns = as.integer(length(fully_missing)),
    dropped_names = paste(fully_missing, collapse = "; "),
    minimum_missing = if (length(remaining)) {
      as.integer(min(remaining))
    } else NA_integer_,
    maximum_missing = if (length(remaining)) {
      as.integer(max(remaining))
    } else NA_integer_
  )
  list(artifact = artifact, assessment = assessment)
}

.lib_partition_staged_recipe <- function(
    artifact, staging_dir, quarantine_writer, report) {
  metadata <- data.table::copy(readRDS(artifact$metadata_path))
  type <- tolower(trimws(as.character(metadata$spectrum_type)))
  ranges <- .lib_type_ranges()
  unsupported <- is.na(type) | !type %in% names(ranges)
  if (any(unsupported)) {
    .lib_capture_staged_positions(
      artifact, which(unsupported), metadata, quarantine_writer,
      stage = "type_partition", rule = "supported_spectrum_type",
      reason = "unsupported_spectrum_type", report = report
    )
  }
  out <- list()
  quality_rows <- list()
  finalization <- list()
  for (spectrum_type in names(ranges)) {
    positions <- which(!unsupported & type == spectrum_type)
    if (!length(positions)) next
    limits <- ranges[[spectrum_type]]
    rows <- which(
      artifact$wavenumber >= limits[[1L]] &
        artifact$wavenumber <= limits[[2L]]
    )
    if (length(rows) < 2L) {
      .lib_capture_staged_positions(
        artifact, positions, metadata, quarantine_writer,
        stage = "type_partition", rule = "minimum_type_range",
        reason = "insufficient_type_range",
        evidence = list(
          observed_wavenumbers = length(rows), threshold = 2
        ), report = report
      )
      next
    }
    label <- paste(artifact$recipe, spectrum_type, sep = "/")
    safe <- paste(make.names(artifact$recipe), spectrum_type, sep = "-")
    writer <- .lib_stage_artifact_writer(
      file.path(staging_dir, paste0("typed-", safe, ".h5")),
      file.path(staging_dir, paste0("typed-", safe, "-metadata.rds")),
      wavenumber = artifact$wavenumber[rows], capacity = length(positions),
      chunk_size = artifact$chunk_size, recipe = label,
      attributes = artifact$attributes
    )
    blocks <- split(
      positions, ceiling(seq_along(positions) / artifact$chunk_size)
    )
    retained <- 0L
    for (block_i in seq_along(blocks)) {
      columns <- blocks[[block_i]]
      object <- .lib_make_open_specy(
        artifact$wavenumber[rows],
        .lib_read_staged_recipe_block(artifact, columns, rows = rows),
        data.table::copy(metadata[columns]), artifact$attributes,
        copy_metadata = FALSE
      ) |>
        as_OpenSpecy()
      flat <- .lib_flat_spectrum_mask(object)
      if (any(flat)) {
        source_rows <- as.integer(object$metadata$.OpenSpecy_build_source_row)
        quarantine_writer$capture(
          object, flat, artifact = artifact$recipe,
          stage = "range_restriction", rule = "flat_spectrum",
          reason = "post_partition_flat_spectrum",
          evidence = list(observed_range = 0, threshold = 0),
          source_rows = source_rows
        )
        quality_rows[[length(quality_rows) + 1L]] <- data.table::data.table(
          artifact = artifact$recipe,
          spectrum_id = as.character(.lib_ids(object, "sample_name"))[flat],
          spectrum_identity = if (
              "spectrum_identity" %in% names(object$metadata)
            ) as.character(object$metadata$spectrum_identity)[flat] else
              NA_character_,
          spectrum_type = spectrum_type, check = "flat_spectrum",
          action = "drop", before_value = 0, after_value = NA_real_,
          threshold = 0, removed = TRUE,
          reason = "post_partition_flat_spectrum"
        )
      }
      if (any(!flat)) {
        writer$append(
          object$spectra[, !flat, drop = FALSE], object$metadata[!flat]
        )
        retained <- retained + sum(!flat)
      }
      rm(object)
      gc(verbose = FALSE)
    }
    typed <- writer$finalize(object_attributes = artifact$object_attributes)
    finalized <- .lib_finalize_staged_artifact_metadata(typed, label)
    out[[spectrum_type]] <- finalized$artifact
    finalization[[spectrum_type]] <- finalized$assessment
    report(sprintf(
      "partitioned staged %s (spectra=%d; range=%g-%g; wavenumbers=%d)",
      label, retained, limits[[1L]], limits[[2L]], length(rows)
    ))
  }
  list(
    artifacts = out,
    quality = data.table::rbindlist(quality_rows, fill = TRUE),
    metadata_finalization = data.table::rbindlist(finalization, fill = TRUE)
  )
}

.lib_is_staged_typed_collection <- function(x) {
  inherits(x, "OpenSpecyStagedTypedCollection") &&
    is.list(x$artifacts) && length(x$artifacts) > 0L
}

.lib_staged_completion_required <- function(x) {
  if (!.lib_is_staged_recipe_collection(x)) return(FALSE)
  threshold <- suppressWarnings(as.integer(getOption(
    "OpenSpecy.staged_completion_threshold", 250000L
  )))
  if (length(threshold) != 1L || is.na(threshold) || threshold < 1L) {
    stop(
      "Option 'OpenSpecy.staged_completion_threshold' must be one positive integer",
      call. = FALSE
    )
  }
  max(vapply(x$artifacts, `[[`, numeric(1L), "spectra")) > threshold
}

.lib_staged_metadata_counts <- function(metadata, artifact, stage) {
  source <- .lib_library_name_values(metadata)
  source[is.na(source) | !nzchar(trimws(source))] <- "(missing)"
  data.table::data.table(
    stage = stage, artifact = artifact, library_name = source
  )[, .(spectra = .N), by = .(stage, artifact, library_name)]
}

.lib_complete_staged_reference_build <- function(
    libraries, tables, prune, remove_other, progress, report,
    staging_dir, build_signature = NA_character_) {
  if (!.lib_is_staged_recipe_collection(libraries)) {
    stop("Expected a staged recipe collection", call. = FALSE)
  }
  prune_targets <- intersect(names(prune), names(libraries$artifacts))
  if (length(prune_targets)) {
    stop(
      "Large file-backed completion requires prune = list(); source-local ",
      "hash and strict correlation deduplication remain active, but the old ",
      "global prune_lib correlation policy is not scalable or identity-local",
      call. = FALSE
    )
  }
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  quarantine_writer <- .lib_quarantine_shard_writer(
    file.path(staging_dir, "quarantine_shards")
  )
  for (name in names(libraries$quarantine_parts)) {
    part <- libraries$quarantine_parts[[name]]
    label <- name
    if (inherits(part, "OpenSpecyQuarantinePartPath")) {
      label <- part$label
      part <- .lib_read_quarantine_part_path(part)
    }
    quarantine_writer$store(part, label)
    rm(part)
    gc(verbose = FALSE)
  }

  raw_artifact <- libraries$artifacts[[
    if ("raw" %in% names(libraries$artifacts)) "raw" else 1L
  ]]
  metadata_input <- readRDS(raw_artifact$metadata_path)
  metadata_result <- .lib_complete_staged_metadata(
    metadata_input, raw_artifact$attributes, tables = tables,
    remove_other = remove_other, report = report
  )

  recipe_stage <- file.path(staging_dir, "completed_recipes")
  typed_stage <- file.path(staging_dir, "typed_libraries")
  dir.create(recipe_stage, recursive = TRUE, showWarnings = FALSE)
  dir.create(typed_stage, recursive = TRUE, showWarnings = FALSE)
  completed_recipes <- list()
  typed <- list()
  quality_rows <- list()
  finalization_rows <- list()
  retention <- list()
  for (recipe in names(libraries$artifacts)) {
    artifact <- libraries$artifacts[[recipe]]
    report(paste0("streaming staged completion (", recipe, ")"))
    completed <- .lib_stage_completed_recipe(
      artifact, metadata_result, staging_dir = recipe_stage,
      quarantine_writer = quarantine_writer, report = report
    )
    completed_recipes[[recipe]] <- completed$artifact
    quality_rows[[recipe]] <- completed$quality
    completed_metadata <- readRDS(completed$artifact$metadata_path)
    retention[[paste0(recipe, "_other")]] <- .lib_staged_metadata_counts(
      metadata_result$metadata, recipe, "post_other"
    )
    retention[[paste0(recipe, "_quality")]] <- .lib_staged_metadata_counts(
      completed_metadata, recipe, "post_quality"
    )
    retention[[paste0(recipe, "_prune")]] <- .lib_staged_metadata_counts(
      completed_metadata, recipe, "post_prune"
    )
    retention[[paste0(recipe, "_transform")]] <- .lib_staged_metadata_counts(
      completed_metadata, recipe, "post_transform"
    )
    partitioned <- .lib_partition_staged_recipe(
      completed$artifact, staging_dir = typed_stage,
      quarantine_writer = quarantine_writer, report = report
    )
    typed[[recipe]] <- partitioned$artifacts
    if (nrow(partitioned$quality)) {
      quality_rows[[paste0(recipe, "_partition")]] <- partitioned$quality
    }
    finalization_rows[[recipe]] <- partitioned$metadata_finalization
    final_metadata <- data.table::rbindlist(lapply(
      partitioned$artifacts,
      function(value) readRDS(value$metadata_path)
    ), fill = TRUE)
    retention[[paste0(recipe, "_final")]] <- .lib_staged_metadata_counts(
      final_metadata, recipe, "final"
    )
  }
  first_attributes <- raw_artifact$object_attributes
  core_retention <- first_attributes$library_retention_stages
  retention_stages <- data.table::rbindlist(
    c(list(core_retention), retention), fill = TRUE
  )
  identity_index <- data.table::data.table(
    sample_name = if ("sample_name" %in% names(metadata_input)) {
      as.character(metadata_input$sample_name)
    } else {
      raw_artifact$spectrum_names
    },
    spectrum_identity = if (
        "spectrum_identity" %in% names(metadata_input)
      ) as.character(metadata_input$spectrum_identity) else NA_character_
  )
  final_ids <- unique(unlist(lapply(typed, function(recipes) {
    unlist(lapply(recipes, function(artifact) {
      readRDS(artifact$metadata_path)$sample_name
    }), use.names = FALSE)
  }), use.names = FALSE))
  dropped <- identity_index[!sample_name %in% final_ids]
  core_dropped <- first_attributes$dropped_spectrum_identities
  dropped_spectrum_identities <- data.table::rbindlist(list(
    core_dropped,
    dropped[
      !is.na(spectrum_identity) & nzchar(trimws(spectrum_identity)),
      .(spectrum_identity)
    ]
  ), fill = TRUE)[
    !is.na(spectrum_identity) & nzchar(trimws(spectrum_identity)),
    .(spectrum_identity = sort(unique(spectrum_identity)))
  ]

  recipe_names <- names(libraries$artifacts)
  replicate_artifact <- function(table, add = TRUE) {
    data.table::rbindlist(lapply(recipe_names, function(recipe) {
      out <- data.table::copy(data.table::as.data.table(table))
      if (add) out[, artifact := recipe]
      out
    }), fill = TRUE)
  }
  class_prediction <- replicate_artifact(data.table::data.table(
    metric = names(metadata_result$prediction$summary),
    value = as.numeric(unlist(
      metadata_result$prediction$summary, use.names = FALSE
    ))
  ))
  class_coverage <- replicate_artifact(metadata_result$class_coverage)
  type_coverage <- replicate_artifact(metadata_result$type_coverage)
  material_form_summary <- replicate_artifact(
    metadata_result$material_form$summary
  )
  material_form_matches <- replicate_artifact(
    metadata_result$material_form$matches
  )
  material_form_clashes <- replicate_artifact(
    metadata_result$material_form$clashes
  )
  common_use_summary <- replicate_artifact(metadata_result$common_use$summary)
  common_use_coverage <- replicate_artifact(
    metadata_result$common_use$coverage
  )
  other_review <- data.table::rbindlist(lapply(recipe_names, function(recipe) {
    out <- data.table::copy(metadata_result$other_review)
    out[, artifact := recipe]
    out
  }), fill = TRUE)
  other_filter <- data.table::rbindlist(lapply(recipe_names, function(recipe) {
    out <- data.table::copy(metadata_result$other_filter)
    out[, artifact := recipe]
    out
  }), fill = TRUE)
  metadata_drop <- data.table::copy(metadata_result$metadata_drop)
  source_hash <- first_attributes$source_hash_chunk_report
  source_merge <- first_attributes$source_merge_chunk_report
  metadata_join <- first_attributes$metadata_join_chunk_report
  source_correlation <- first_attributes$correlation_dedupe_report
  if (is.null(source_hash)) source_hash <- data.table::data.table()
  if (is.null(source_merge)) source_merge <- data.table::data.table()
  if (is.null(metadata_join)) metadata_join <- data.table::data.table()
  if (is.null(source_correlation)) {
    source_correlation <- list(
      groups = .lib_correlation_dedupe_group_schema(),
      removals = .lib_correlation_dedupe_removal_schema()
    )
  }
  quarantine <- quarantine_writer$bundle(build_signature = build_signature)
  out <- structure(list(
    schema = "OpenSpecy_staged_typed_collection_v1",
    artifacts = typed,
    completed_recipes = completed_recipes,
    telemetry = libraries$telemetry,
    assessments = list(
      source_hash_chunking = source_hash,
      source_merge_chunking = source_merge,
      metadata_join_chunking = metadata_join,
      recipe_staging = libraries$telemetry,
      source_correlation_dedupe_groups = source_correlation$groups,
      source_correlation_dedupe_removals = source_correlation$removals,
      class_prediction = class_prediction,
      class_coverage = class_coverage,
      type_coverage = type_coverage,
      material_form_enrichment = material_form_summary,
      material_form_matches = material_form_matches,
      material_form_clashes = material_form_clashes,
      common_use_enrichment = common_use_summary,
      common_use_coverage = common_use_coverage,
      other_review = other_review,
      other_filter = other_filter,
      pruning = data.table::data.table(),
      pruning_excluded_classes = .lib_prune_excluded_assessment_schema(),
      pruning_reassignments = .lib_prune_reassignment_schema(),
      pruning_removals = .lib_prune_removal_assessment_schema(),
      pruning_conflicts = {
        value <- .lib_prune_conflict_edge_schema()
        value[, artifact := character()]
        value
      },
      correlation_closure = data.table::data.table(),
      automated_test_flags = .lib_collect_automated_test_flags(
        list(staged = .lib_staged_metadata_proxy(
          metadata_result$metadata, raw_artifact$attributes
        ))
      ),
      quality_control = data.table::rbindlist(quality_rows, fill = TRUE),
      library_retention = .lib_library_retention_report(retention_stages),
      dropped_spectrum_identities = dropped_spectrum_identities,
      filters = data.table::data.table(
        stage = "special_filter", before = nrow(metadata_input),
        after = length(final_ids), removed = nrow(metadata_input) - length(final_ids)
      ),
      metadata_drop = metadata_drop,
      metadata_finalization = data.table::rbindlist(
        finalization_rows, fill = TRUE
      )
    ),
    quarantine = quarantine
  ), class = c("OpenSpecyStagedTypedCollection", "list"))
  for (recipe in out$artifacts) {
    for (artifact in recipe) {
      .lib_validate_staged_recipe_artifact(artifact, strong = TRUE)
    }
  }
  out
}

.lib_staged_model_rows <- function(artifact, type) {
  wavenumber <- artifact$wavenumber
  switch(
    tolower(type),
    ftir = which(
      (wavenumber >= 800 & wavenumber <= 2200) |
        (wavenumber >= 2420 & wavenumber <= 3200)
    ),
    raman = which(wavenumber >= 800 & wavenumber <= 3200),
    nir = {
      rows <- which(wavenumber >= 4000 & wavenumber <= 12000)
      if (!length(rows)) stop("NIR spectra have no values in 4000-12000",
                              call. = FALSE)
      rows
    },
    stop("Unsupported spectrum type: ", type, call. = FALSE)
  )
}

.lib_build_staged_medoids <- function(libraries, report, progress = TRUE) {
  if (!.lib_is_staged_typed_collection(libraries)) {
    stop("Expected staged typed libraries", call. = FALSE)
  }
  out <- list()
  for (recipe in intersect(
      c("derivative", "nobaseline"), names(libraries$artifacts)
    )) {
    out[[recipe]] <- list()
    for (type in names(libraries$artifacts[[recipe]])) {
      artifact <- libraries$artifacts[[recipe]][[type]]
      metadata <- data.table::copy(readRDS(artifact$metadata_path))
      source <- .lib_model_source_groups(metadata)
      class <- as.character(metadata$material_class)
      groups <- split(seq_len(nrow(metadata)), paste(source, class, sep = "\r"))
      model_rows <- .lib_staged_model_rows(artifact, type)
      medoid_parts <- list()
      part_i <- 0L
      report(sprintf(
        "selecting staged medoids (%s/%s; spectra=%d; groups=%d)",
        recipe, type, artifact$spectra, length(groups)
      ))
      for (group_name in names(groups)) {
        columns <- groups[[group_name]]
        object <- .lib_materialize_staged_recipe(
          artifact, columns = columns, rows = model_rows
        )
        supported <- .lib_filter_spectral_support(object, min_fraction = 0.1)
        object <- supported$object
        flat <- .lib_flat_spectrum_mask(object)
        if (all(flat)) {
          rm(object, supported)
          gc(verbose = FALSE)
          next
        }
        if (any(flat)) object <- filter_spec(object, !flat)
        if (ncol(object$spectra) > 50L) {
          ids <- .lib_reduce_lib(
            object,
            group_cols = intersect(
              c("material_class"), names(object$metadata)
            ),
            k = 50L, min_n = 50L, return = "ids", progress = progress
          )
          object <- filter_spec(object, ids)
        }
        part_i <- part_i + 1L
        medoid_parts[[part_i]] <- object
        rm(object, supported)
        gc(verbose = FALSE)
      }
      if (!length(medoid_parts)) {
        stop("No staged medoids remain for ", recipe, "/", type,
             call. = FALSE)
      }
      medoid <- .lib_bind_same_axis(
        medoid_parts, paste("staged medoids", recipe, type)
      )
      attr(medoid, "medoid_parent_signature") <- digest::digest(list(
        schema = "OpenSpecy_staged_medoid_parent_v1",
        artifact = recipe, type = type,
        spectra_sha256 = artifact$spectra_sha256,
        metadata_sha256 = artifact$metadata_sha256,
        group_columns = c("canonical_source", "material_class"),
        k = 50L, min_n = 50L
      ), algo = "sha256")
      attr(medoid, "identification_range") <- range(medoid$wavenumber)
      out[[recipe]][[type]] <- medoid
      report(sprintf(
        "staged medoids complete (%s/%s; retained=%d)",
        recipe, type, ncol(medoid$spectra)
      ))
      rm(medoid_parts, medoid)
      gc(verbose = FALSE, full = TRUE)
    }
  }
  out
}

.lib_promote_staged_quarantine <- function(quarantine, release_dir) {
  .lib_validate_quarantine(quarantine)
  if (!identical(
      quarantine$manifest$schema, "OpenSpecy_quarantined_spectra_v3"
    )) {
    stop("Expected sharded quarantine v3", call. = FALSE)
  }
  shard_dir <- file.path(release_dir, "quarantine")
  dir.create(shard_dir, recursive = TRUE, showWarnings = FALSE)
  promoted <- lapply(names(quarantine$spectra), function(name) {
    shard <- quarantine$spectra[[name]]
    target <- file.path(shard_dir, basename(shard$path))
    if (file.exists(target)) {
      if (!identical(.lib_sha256_file(target), shard$sha256)) {
        stop("Immutable quarantine shard differs: ", target, call. = FALSE)
      }
    } else if (!file.copy(shard$path, target, overwrite = FALSE)) {
      stop("Could not promote quarantine shard: ", shard$path, call. = FALSE)
    }
    if (!identical(.lib_sha256_file(target), shard$sha256)) {
      stop("Promoted quarantine shard hash changed: ", target, call. = FALSE)
    }
    shard$path <- normalizePath(target, winslash = "/", mustWork = TRUE)
    shard$bytes <- as.numeric(file.info(target)$size)
    shard
  })
  names(promoted) <- names(quarantine$spectra)
  quarantine$spectra <- promoted
  .lib_validate_quarantine(quarantine)
  quarantine
}

.lib_promote_staged_libraries <- function(libraries, release_dir, report) {
  manifest <- list()
  paths <- character()
  for (recipe in names(libraries$artifacts)) {
    report(paste0("materializing release library: ", recipe))
    typed <- lapply(
      libraries$artifacts[[recipe]], .lib_materialize_staged_recipe
    )
    typed <- lapply(typed, .lib_slim_reference_object)
    if (!all(vapply(typed, function(object) {
      isTRUE(suppressWarnings(check_OpenSpecy(object)))
    }, logical(1L)))) {
      stop("Materialized staged library is invalid: ", recipe,
           call. = FALSE)
    }
    path <- file.path(release_dir, paste0(recipe, ".rds"))
    manifest[[recipe]] <- .lib_promote_rds(typed, path)
    paths[[recipe]] <- normalizePath(path, winslash = "/", mustWork = TRUE)
    rm(typed)
    gc(verbose = FALSE, full = TRUE)
  }
  list(
    paths = paths,
    manifest = data.table::rbindlist(manifest, fill = TRUE)
  )
}

.lib_staged_build_summary <- function(libraries, medoids) {
  library_rows <- data.table::rbindlist(lapply(
    names(libraries$artifacts), function(recipe) {
      data.table::rbindlist(lapply(
        names(libraries$artifacts[[recipe]]), function(type) {
          artifact <- libraries$artifacts[[recipe]][[type]]
          data.table::data.table(
            artifact = paste(recipe, type, sep = "_"),
            valid_OpenSpecy = TRUE, spectra = as.integer(artifact$spectra),
            wavenumbers = as.integer(length(artifact$wavenumber)),
            metadata_columns = as.integer(ncol(readRDS(artifact$metadata_path)))
          )
        }
      ))
    }
  ))
  medoid_objects <- .lib_named_typed_objects(medoids, prefix = "medoid_")
  medoid_rows <- data.table::rbindlist(lapply(
    names(medoid_objects), function(name) {
      object <- medoid_objects[[name]]
      data.table::data.table(
        artifact = name,
        valid_OpenSpecy = isTRUE(suppressWarnings(check_OpenSpecy(object))),
        spectra = as.integer(ncol(object$spectra)),
        wavenumbers = as.integer(length(object$wavenumber)),
        metadata_columns = as.integer(ncol(object$metadata))
      )
    }
  ))
  data.table::rbindlist(list(library_rows, medoid_rows), fill = TRUE)
}

.lib_staged_assessment_components <- function(libraries, medoids, models,
                                               model_warnings,
                                               output_manifest,
                                               previous_library_dir) {
  completed <- libraries$assessments
  model_tables <- .lib_model_assessment_tables(models)
  completed$build_summary <- .lib_staged_build_summary(libraries, medoids)
  completed$model_training_tests <- model_tables$tests
  completed$model_lambda_metrics <- model_tables$lambda_metrics
  completed$model_class_metrics <- model_tables$class_metrics
  completed$model_hierarchy_metrics <- model_tables$hierarchy_metrics
  completed$model_source_metrics <- model_tables$source_metrics
  completed$model_node_support <- model_tables$node_support
  completed$model_fold_assignments <- model_tables$fold_assignments
  completed$model_oob_metrics <- model_tables$oob_metrics
  completed$model_oob_class_accuracy <- model_tables$oob_class_accuracy
  completed$model_feature_importance <- model_tables$feature_importance
  completed$model_training_parameters <- model_tables$training_parameters
  completed$model_class_weights <- model_tables$class_weights
  completed$warnings <- data.table::rbindlist(list(
    model_warnings,
    if (!is.null(previous_library_dir)) data.table::data.table(
      algorithm = "reference_build", artifact = "file_backed",
      model = "legacy_comparison",
      warning = paste0(
        "The full legacy spectral comparison is deferred to the postbuild ",
        "release audit so multi-gigabyte candidate libraries never coexist ",
        "in memory with the comparator"
      )
    ) else NULL
  ), fill = TRUE)
  completed$output_manifest <- output_manifest
  completed
}

.lib_finish_staged_reference_build <- function(
    completed, output_dir, artifact_signature, previous_library_dir,
    model_method, progress, report, checkpoints) {
  if (!.lib_is_staged_typed_collection(completed)) {
    stop("Expected completed staged typed libraries", call. = FALSE)
  }
  if (!identical(model_method, "logistic_regression")) {
    stop(
      "Large file-backed builds currently support the official ",
      "logistic_regression model only", call. = FALSE
    )
  }
  medoids <- checkpoints$get("staged_medoids")
  if (is.null(medoids)) {
    medoids <- .lib_build_staged_medoids(
      completed, report = report, progress = progress
    )
    checkpoints$put("staged_medoids", medoids)
  }
  models <- checkpoints$get("staged_models_flat_logistic_v1")
  model_warnings <- if (is.null(models)) .lib_warning_schema() else
    .lib_collect_model_warnings(models)
  if (is.null(models)) {
    model_result <- .lib_with_model_workers(.lib_build_models(
      libraries = list(), medoids = medoids, report = report,
      checkpoints = checkpoints, methods = "logistic_regression"
    ))
    models <- model_result$models
    model_warnings <- model_result$warnings
    problems <- .lib_flat_model_checkpoint_problems(models, medoids)
    if (length(problems)) {
      stop(
        "Official staged logistic model build is incomplete: ",
        paste(problems, collapse = "; "), call. = FALSE
      )
    }
    checkpoints$put("staged_models_flat_logistic_v1", models)
  }
  assessment_key <- digest::digest(list(
    artifact_signature, previous = .lib_previous_signature(previous_library_dir),
    schema = "file-backed-assessment-v1"
  ), algo = "sha256")
  release_signature <- .lib_release_signature(assessment_key, models)
  release_dir <- file.path(
    output_dir, "releases", substr(release_signature, 1L, 12L)
  )
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

  libraries <- .lib_promote_staged_libraries(completed, release_dir, report)
  medoid_release <- .lib_slim_reference_collection(medoids)
  model_release <- .lib_slim_model_collection(models)
  artifact_manifest <- list(libraries$manifest)
  for (recipe in names(medoid_release)) {
    name <- paste0("medoid_", recipe)
    artifact_manifest[[length(artifact_manifest) + 1L]] <- .lib_promote_rds(
      medoid_release[[recipe]], file.path(release_dir, paste0(name, ".rds"))
    )
  }
  for (algorithm in names(model_release)) {
    for (recipe in names(model_release[[algorithm]])) {
      name <- paste("model", algorithm, recipe, sep = "_")
      artifact_manifest[[length(artifact_manifest) + 1L]] <- .lib_promote_rds(
        model_release[[algorithm]][[recipe]],
        file.path(release_dir, paste0(name, ".rds"))
      )
      if (identical(algorithm, "logistic_regression") &&
          recipe %in% c("derivative", "nobaseline")) {
        legacy_name <- paste0("model_", recipe)
        artifact_manifest[[length(artifact_manifest) + 1L]] <-
          .lib_promote_rds(
            model_release[[algorithm]][[recipe]],
            file.path(release_dir, paste0(legacy_name, ".rds"))
          )
      }
    }
  }
  quarantine <- .lib_promote_staged_quarantine(
    completed$quarantine, release_dir
  )
  quarantine$manifest$release_signature <- release_signature
  quarantine_manifest <- .lib_promote_rds(
    quarantine, file.path(release_dir, "quarantined_spectra.rds")
  )
  artifact_manifest[[length(artifact_manifest) + 1L]] <- quarantine_manifest
  artifact_manifest <- data.table::rbindlist(artifact_manifest, fill = TRUE)

  assessment_components <- .lib_staged_assessment_components(
    completed, medoids, models, model_warnings,
    output_manifest = artifact_manifest,
    previous_library_dir = previous_library_dir
  )
  assessments <- .lib_assessment_review(assessment_components)
  assessments_manifest <- .lib_promote_rds(
    assessments, file.path(release_dir, "assessments.rds")
  )
  index_inputs <- data.table::rbindlist(
    list(artifact_manifest, assessments_manifest), fill = TRUE
  )
  index_manifest <- .lib_promote_build_index(
    release_signature, index_inputs,
    file.path(release_dir, "reference_library_build.rds")
  )
  release_manifest <- data.table::rbindlist(
    list(index_inputs, index_manifest), fill = TRUE
  )
  release_manifest[, status := "available"]
  attr(release_manifest, "build_signature") <- release_signature
  .lib_promote_rds(
    release_manifest, file.path(release_dir, "release_manifest.rds")
  )
  quarantine$manifest$build_signature <- artifact_signature
  quarantine$manifest$source_hashes <- artifact_signature
  .lib_write_quarantine_review(quarantine, output_dir)
  result <- structure(list(
    schema = "OpenSpecy_file_backed_reference_build_v1",
    libraries = libraries$paths, medoids = medoids, models = models,
    assessments = assessments, quarantine = quarantine,
    release_index = file.path(release_dir, "reference_library_build.rds")
  ), class = c("OpenSpecyFileBackedReferenceBuild", "list"))
  attr(result, "output_dir") <- normalizePath(
    release_dir, winslash = "/", mustWork = TRUE
  )
  attr(result, "build_signature") <- release_signature
  result
}
