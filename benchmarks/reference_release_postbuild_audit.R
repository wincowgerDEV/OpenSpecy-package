# Audit a completed gated reference-library release against the immutable
# bb8cd82c7ecb release. The audit never writes inside either release.
#
# Safe discovery (does not require a completed release):
#   Rscript benchmarks/reference_release_postbuild_audit.R --stage=help
#
# Full audit:
#   Rscript benchmarks/reference_release_postbuild_audit.R --stage=audit \
#     --new-release=C:/path/to/output/releases/<signature-prefix>
#
# The new release can also be supplied through OPENSPECY_AUDIT_NEW_RELEASE.

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest", "devtools")
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

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) {
    return(normalize_path("benchmarks/reference_release_postbuild_audit.R"))
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

runner_version <- "reference-release-postbuild-audit-v5"
fit_runtime_ceiling_seconds <- 3600
prior_signature <-
  "bb8cd82c7ecbff60349f02cadbb37d86c2ecf8e46f0b4665fea21fb9d40bdb0a"
prior_manifest_sha256 <-
  "6ba9fa4dc070e941f9019a1e6345fbb5f022923775cc704db97c4c9736bccc41"

new_rds_files <- c(
  "raw.rds", "derivative.rds", "nobaseline.rds",
  "medoid_derivative.rds", "medoid_nobaseline.rds",
  "model_gated_hierarchical_logistic_regression_derivative.rds",
  "model_gated_hierarchical_logistic_regression_nobaseline.rds",
  "quarantined_spectra.rds", "assessments.rds",
  "reference_library_build.rds", "release_manifest.rds"
)

prior_rds_files <- c(
  "raw.rds", "derivative.rds", "nobaseline.rds",
  "medoid_derivative.rds", "medoid_nobaseline.rds",
  "model_logistic_regression_derivative.rds",
  "model_logistic_regression_nobaseline.rds",
  "model_derivative.rds", "model_nobaseline.rds",
  "quarantined_spectra.rds", "assessments.rds",
  "reference_library_build.rds", "release_manifest.rds"
)

usage <- function() {
  cat(paste0(
    "Post-build reference-library release audit\n\n",
    "Stages:\n",
    "  help   Print this text and perform no release reads or writes.\n",
    "  audit  Run the audit and write evidence outside both releases.\n\n",
    "Arguments use --name=value:\n",
    "  --new-release=PATH       Required completed release directory.\n",
    "  --prior-release=PATH     Immutable comparison release.\n",
    "  --output-dir=PATH        External audit output directory.\n",
    "  --vinyl-evidence=PATH    Optional source-held vinyl evidence CSV.\n",
    "  --stage=help|audit       Default: help.\n\n",
    "Environment fallbacks:\n",
    "  OPENSPECY_AUDIT_NEW_RELEASE\n",
    "  OPENSPECY_AUDIT_PRIOR_RELEASE\n",
    "  OPENSPECY_AUDIT_OUTPUT_DIR\n",
    "  OPENSPECY_AUDIT_VINYL_EVIDENCE\n"
  ))
  invisible(TRUE)
}

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (any(args %in% c("--help", "-h"))) args <- "--stage=help"
  out <- list(
    stage = "help",
    new_release = Sys.getenv("OPENSPECY_AUDIT_NEW_RELEASE", unset = ""),
    prior_release = Sys.getenv(
      "OPENSPECY_AUDIT_PRIOR_RELEASE",
      unset = paste0(
        "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/",
        "reference-library-build-2.0.0/releases/bb8cd82c7ecb"
      )
    ),
    output_dir = Sys.getenv("OPENSPECY_AUDIT_OUTPUT_DIR", unset = ""),
    vinyl_evidence = Sys.getenv(
      "OPENSPECY_AUDIT_VINYL_EVIDENCE", unset = ""
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
  if (!out$stage %in% c("help", "audit")) stop("Unknown --stage=", out$stage)
  out
}

path_is_within <- function(path, parent) {
  path <- normalize_path(path, must_work = FALSE)
  parent <- sub("/+$", "", normalize_path(parent, must_work = FALSE))
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

normalize_cli <- function(cli) {
  if (!nzchar(cli$new_release)) {
    stop(
      "--new-release (or OPENSPECY_AUDIT_NEW_RELEASE) is required for audit"
    )
  }
  cli$new_release <- normalize_path(cli$new_release)
  cli$prior_release <- normalize_path(cli$prior_release)
  if (!nzchar(cli$output_dir)) {
    build_root <- dirname(dirname(cli$new_release))
    cli$output_dir <- file.path(
      build_root, "audits",
      paste0(basename(cli$new_release), "-vs-bb8cd82c7ecb")
    )
  }
  cli$output_dir <- normalize_path(cli$output_dir, must_work = FALSE)
  if (path_is_within(cli$output_dir, cli$new_release) ||
      path_is_within(cli$output_dir, cli$prior_release)) {
    stop("Audit output must be outside both immutable release directories")
  }
  if (nzchar(cli$vinyl_evidence)) {
    cli$vinyl_evidence <- normalize_path(cli$vinyl_evidence)
  }
  cli
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) {
    return(list(
      ok = FALSE, value = NULL,
      error = paste0("file is absent: ", normalize_path(path, must_work = FALSE))
    ))
  }
  tryCatch(
    list(
      ok = TRUE, value = suppressWarnings(readRDS(path)),
      error = NA_character_
    ),
    error = function(error) list(
      ok = FALSE, value = NULL, error = conditionMessage(error)
    )
  )
}

as_dt <- function(x) {
  if (is.null(x)) data.table::data.table() else
    data.table::as.data.table(x)
}

compact_detail <- function(...) {
  value <- paste(..., collapse = "")
  gsub("[\r\n]+", " ", value)
}

new_check_recorder <- function() {
  rows <- list()
  i <- 0L
  add <- function(check, passed, detail, severity = "error") {
    i <<- i + 1L
    rows[[i]] <<- data.table::data.table(
      check = as.character(check), passed = isTRUE(passed),
      severity = as.character(severity), detail = compact_detail(detail)
    )
    invisible(isTRUE(passed))
  }
  table <- function() data.table::rbindlist(rows, fill = TRUE)
  list(add = add, table = table)
}

is_reference_object <- function(x) {
  is.list(x) && all(c("wavenumber", "spectra", "metadata") %in% names(x)) &&
    is.matrix(x$spectra) && is.data.frame(x$metadata)
}

reference_ids <- function(x) {
  if ("sample_name" %in% names(x$metadata)) {
    as.character(x$metadata$sample_name)
  } else {
    as.character(colnames(x$spectra))
  }
}

subset_reference <- function(x, column) {
  out <- x
  out$spectra <- x$spectra[, column, drop = FALSE]
  out$metadata <- x$metadata[column, , drop = FALSE]
  out
}

read_assessment_parts <- function(path, checks) {
  loaded <- safe_read_rds(path)
  checks$add("assessments_readable", loaded$ok, loaded$error)
  if (!loaded$ok) {
    return(list(review = NULL, evidence = list(), upstream = list()))
  }
  evidence <- attr(loaded$value, "evidence", exact = TRUE) %||% list()
  upstream <- attr(
    loaded$value, "upstream_assessments", exact = TRUE
  ) %||% list()
  hashes <- attr(evidence, "sha256", exact = TRUE)
  hash_ok <- is.character(hashes) &&
    setequal(names(hashes), names(evidence)) &&
    all(vapply(names(evidence), function(name) {
      identical(
        unname(hashes[[name]]),
        digest::digest(evidence[[name]], algo = "sha256")
      )
    }, logical(1L)))
  checks$add(
    "assessment_evidence_hashes", hash_ok,
    paste0("evidence tables=", length(evidence))
  )
  list(review = loaded$value, evidence = evidence, upstream = upstream)
}

validate_release <- function(directory, label, expected_files, checks,
                             expected_signature = NULL,
                             expected_manifest_hash = NULL) {
  actual_files <- sort(list.files(
    directory, pattern = "[.]rds$", full.names = FALSE
  ))
  expected_files <- sort(expected_files)
  checks$add(
    paste0(label, "_rds_inventory"), identical(actual_files, expected_files),
    paste0(
      "expected=", length(expected_files), "; actual=", length(actual_files),
      "; missing=", paste(setdiff(expected_files, actual_files), collapse = "|"),
      "; extra=", paste(setdiff(actual_files, expected_files), collapse = "|")
    )
  )

  index_path <- file.path(directory, "reference_library_build.rds")
  manifest_path <- file.path(directory, "release_manifest.rds")
  index_loaded <- safe_read_rds(index_path)
  manifest_loaded <- safe_read_rds(manifest_path)
  checks$add(paste0(label, "_index_readable"), index_loaded$ok,
             index_loaded$error)
  checks$add(paste0(label, "_manifest_readable"), manifest_loaded$ok,
             manifest_loaded$error)
  index <- index_loaded$value
  manifest <- as_dt(manifest_loaded$value)

  signature <- if (is.list(index)) index$build_signature else NA_character_
  schema_ok <- is.list(index) &&
    identical(index$schema, "OpenSpecy_reference_build_index_v1") &&
    is.character(index$artifacts) && length(index$artifacts) > 0L
  checks$add(paste0(label, "_index_schema"), schema_ok,
             paste0("signature=", signature))
  if (!is.null(expected_signature)) {
    checks$add(
      paste0(label, "_expected_signature"),
      identical(signature, expected_signature),
      paste0("expected=", expected_signature, "; actual=", signature)
    )
  } else {
    checks$add(
      paste0(label, "_signature_shape"),
      is.character(signature) && length(signature) == 1L &&
        grepl("^[[:xdigit:]]{64}$", signature) &&
        startsWith(signature, basename(directory)),
      paste0("directory=", basename(directory), "; signature=", signature)
    )
  }
  manifest_signature <- attr(
    manifest_loaded$value, "build_signature", exact = TRUE
  )
  checks$add(
    paste0(label, "_manifest_signature"),
    identical(manifest_signature, signature),
    paste0("manifest=", manifest_signature, "; index=", signature)
  )
  if (!is.null(expected_manifest_hash)) {
    actual_hash <- if (file.exists(manifest_path)) {
      sha256_file(manifest_path)
    } else NA_character_
    checks$add(
      paste0(label, "_manifest_pin"),
      identical(actual_hash, expected_manifest_hash),
      paste0("expected=", expected_manifest_hash, "; actual=", actual_hash)
    )
  }

  required_manifest_cols <- c(
    "component", "status", "path", "size", "checksum_algorithm", "checksum"
  )
  manifest_shape <- all(required_manifest_cols %in% names(manifest)) &&
    !anyDuplicated(manifest$component)
  checks$add(paste0(label, "_manifest_shape"), manifest_shape,
             paste0("rows=", nrow(manifest)))

  expected_components <- tools::file_path_sans_ext(setdiff(
    expected_files, "release_manifest.rds"
  ))
  checks$add(
    paste0(label, "_manifest_components"),
    manifest_shape && setequal(manifest$component, expected_components),
    paste0(
      "missing=", paste(setdiff(expected_components, manifest$component),
                         collapse = "|"),
      "; extra=", paste(setdiff(manifest$component, expected_components),
                         collapse = "|")
    )
  )

  # The release index intentionally lists assessments both in `artifacts` and
  # in its historical convenience field; only the index itself is excluded.
  index_components <- setdiff(expected_components, "reference_library_build")
  index_ok <- schema_ok &&
    setequal(names(index$artifacts), index_components) &&
    identical(index$assessments, "assessments.rds") &&
    identical(index$release_manifest, "release_manifest.rds")
  checks$add(
    paste0(label, "_index_components"), index_ok,
    paste0("indexed artifacts=", length(index$artifacts %||% character()))
  )

  inventory <- lapply(expected_files, function(file) {
    path <- file.path(directory, file)
    exists <- file.exists(path)
    component <- tools::file_path_sans_ext(file)
    info <- if (exists) file.info(path) else NULL
    manifest_row <- if (nrow(manifest)) {
      which(as.character(manifest[["component"]]) == component)
    } else integer()
    row <- if (length(manifest_row) == 1L) {
      manifest[manifest_row]
    } else data.table::data.table()
    checksum <- if (exists) sha256_file(path) else NA_character_
    checksum_ok <- if (identical(file, "release_manifest.rds")) {
      exists
    } else {
      nrow(row) == 1L && identical(as.character(row$checksum_algorithm), "sha256") &&
        identical(as.numeric(row$size), as.numeric(info$size)) &&
        identical(as.character(row$checksum), checksum) &&
        identical(basename(as.character(row$path)), file)
    }
    loaded <- if (exists) safe_read_rds(path) else list(
      ok = FALSE, error = "missing"
    )
    data.table::data.table(
      release = label, component = component, file = file,
      exists = exists, bytes = if (exists) as.numeric(info$size) else NA_real_,
      sha256 = checksum, manifested = nrow(row) == 1L,
      manifest_hash_and_size_ok = checksum_ok, readable = loaded$ok,
      read_error = loaded$error
    )
  })
  inventory <- data.table::rbindlist(inventory, fill = TRUE)
  checks$add(
    paste0(label, "_all_payload_hashes"),
    all(inventory$manifest_hash_and_size_ok),
    paste0("valid=", sum(inventory$manifest_hash_and_size_ok),
           "/", nrow(inventory))
  )
  checks$add(
    paste0(label, "_all_payloads_readable"), all(inventory$readable),
    paste0("readable=", sum(inventory$readable), "/", nrow(inventory))
  )
  list(
    directory = directory, index = index, manifest = manifest,
    inventory = inventory, signature = signature
  )
}

reference_compatibility <- function(new_dir, prior_dir, checks) {
  components <- c(
    "raw", "derivative", "nobaseline",
    "medoid_derivative", "medoid_nobaseline"
  )
  rows <- list()
  group_rows <- list()
  final_rows <- list()
  i <- 0L
  for (component in components) {
    new <- readRDS(file.path(new_dir, paste0(component, ".rds")))
    old <- readRDS(file.path(prior_dir, paste0(component, ".rds")))
    type_names <- sort(unique(c(names(new), names(old))))
    checks$add(
      paste0("reference_types_", component),
      identical(sort(names(new)), c("ftir", "nir", "raman")) &&
        identical(sort(names(old)), c("ftir", "nir", "raman")),
      paste0(
        "new=", paste(names(new), collapse = "|"),
        "; prior=", paste(names(old), collapse = "|")
      )
    )
    for (type in type_names) {
      i <- i + 1L
      new_object <- new[[type]]
      old_object <- old[[type]]
      valid_new <- is_reference_object(new_object) &&
        ncol(new_object$spectra) == nrow(new_object$metadata) &&
        nrow(new_object$spectra) == length(new_object$wavenumber)
      valid_old <- is_reference_object(old_object) &&
        ncol(old_object$spectra) == nrow(old_object$metadata) &&
        nrow(old_object$spectra) == length(old_object$wavenumber)
      new_ids <- if (valid_new) reference_ids(new_object) else character()
      old_ids <- if (valid_old) reference_ids(old_object) else character()
      new_alignment <- valid_new && length(new_ids) == ncol(new_object$spectra) &&
        !anyNA(new_ids) && all(nzchar(trimws(new_ids))) &&
        identical(new_ids, as.character(colnames(new_object$spectra)))
      old_alignment <- valid_old && length(old_ids) == ncol(old_object$spectra) &&
        !anyNA(old_ids) && all(nzchar(trimws(old_ids))) &&
        identical(old_ids, as.character(colnames(old_object$spectra)))
      shared <- sort(intersect(new_ids, old_ids))
      axis_equal <- valid_new && valid_old &&
        identical(new_object$wavenumber, old_object$wavenumber)
      shared_spectra_equal <- axis_equal
      if (shared_spectra_equal && length(shared)) {
        blocks <- split(
          seq_along(shared), ceiling(seq_along(shared) / 500L)
        )
        for (block in blocks) {
          left <- old_object$spectra[
            , match(shared[block], old_ids), drop = FALSE
          ]
          right <- new_object$spectra[
            , match(shared[block], new_ids), drop = FALSE
          ]
          dimnames(left) <- dimnames(right) <- NULL
          if (!identical(left, right)) {
            shared_spectra_equal <- FALSE
            break
          }
        }
      }
      required_metadata <- c(
        "sample_name", "spectrum_identity", "spectrum_type",
        "material_class", "material_type", "material_group",
        "organization", "library_name"
      )
      rows[[i]] <- data.table::data.table(
        component = component, spectrum_type = type,
        new_valid = valid_new, prior_valid = valid_old,
        new_ids_aligned = new_alignment, prior_ids_aligned = old_alignment,
        new_spectra = length(new_ids), prior_spectra = length(old_ids),
        shared_ids = length(shared), new_only_ids = length(setdiff(new_ids, old_ids)),
        prior_only_ids = length(setdiff(old_ids, new_ids)),
        duplicated_new_ids = anyDuplicated(new_ids),
        duplicated_prior_ids = anyDuplicated(old_ids),
        wavenumber_axis_identical = axis_equal,
        group_route_columns_present = valid_new && all(
          c("material_class", "material_type", "material_group") %in%
            names(new_object$metadata)
        ),
        core_metadata_present = valid_new && all(
          required_metadata %in% names(new_object$metadata)
        ),
        metadata_columns_new = ncol(new_object$metadata %||% data.frame()),
        metadata_columns_prior = ncol(old_object$metadata %||% data.frame()),
        metadata_added = paste(
          setdiff(names(new_object$metadata), names(old_object$metadata)),
          collapse = "|"
        ),
        metadata_removed = paste(
          setdiff(names(old_object$metadata), names(new_object$metadata)),
          collapse = "|"
        ),
        shared_spectra_identical = shared_spectra_equal
      )
      if (valid_new) {
        metadata <- data.table::as.data.table(new_object$metadata)
        ids <- reference_ids(new_object)
        required <- c("material_class", "material_type", "material_group")
        if (all(required %in% names(metadata))) {
          group_rows[[length(group_rows) + 1L]] <- data.table::data.table(
            component = component, spectrum_type = type,
            spectrum_id = ids,
            material_class = as.character(metadata$material_class),
            material_type = as.character(metadata$material_type),
            material_group = as.character(metadata$material_group)
          )
        }
        if (!startsWith(component, "medoid_")) {
          final_rows[[length(final_rows) + 1L]] <- data.table::data.table(
            artifact = component, spectrum_type = type,
            spectrum_id = ids,
            material_class = if ("material_class" %in% names(metadata))
              as.character(metadata$material_class) else NA_character_,
            material_type = if ("material_type" %in% names(metadata))
              as.character(metadata$material_type) else NA_character_,
            material_group = if ("material_group" %in% names(metadata))
              as.character(metadata$material_group) else NA_character_,
            spectrum_identity = if ("spectrum_identity" %in% names(metadata))
              as.character(metadata$spectrum_identity) else NA_character_,
            material_form = if ("material_form" %in% names(metadata))
              as.character(metadata$material_form) else NA_character_
          )
        }
      }
    }
    rm(new, old)
    gc(verbose = FALSE)
  }
  compatibility <- data.table::rbindlist(rows, fill = TRUE)
  expected_comparisons <- data.table::CJ(
    component = components,
    spectrum_type = c("ftir", "nir", "raman"),
    sorted = TRUE
  )
  data.table::setkey(expected_comparisons, NULL)
  actual_comparisons <- unique(compatibility[, .(component, spectrum_type)])
  data.table::setorder(actual_comparisons, component, spectrum_type)
  checks$add(
    "reference_comparison_contract",
    nrow(compatibility) == 15L &&
      identical(actual_comparisons, expected_comparisons),
    paste0("comparisons=", nrow(compatibility), "; expected=15")
  )
  checks$add(
    "reference_object_alignment",
    all(compatibility$new_valid & compatibility$prior_valid),
    paste0("objects=", nrow(compatibility))
  )
  checks$add(
    "reference_id_uniqueness",
    all(compatibility$duplicated_new_ids == 0L &
          compatibility$duplicated_prior_ids == 0L),
    "sample_name identifiers must be unique within each typed artifact"
  )
  checks$add(
    "reference_id_alignment",
    all(compatibility$new_ids_aligned & compatibility$prior_ids_aligned),
    "sample_name identifiers must be nonblank and equal spectra column names"
  )
  checks$add(
    "reference_axis_compatibility",
    all(compatibility$wavenumber_axis_identical),
    paste0("identical=", sum(compatibility$wavenumber_axis_identical),
           "/", nrow(compatibility))
  )
  checks$add(
    "reference_group_route_columns",
    all(compatibility$group_route_columns_present),
    paste0("complete=", sum(compatibility$group_route_columns_present),
           "/", nrow(compatibility))
  )
  checks$add(
    "reference_core_metadata",
    all(compatibility$core_metadata_present),
    paste0("complete=", sum(compatibility$core_metadata_present),
           "/", nrow(compatibility))
  )
  checks$add(
    "reference_shared_spectra_exact",
    all(compatibility$shared_spectra_identical),
    paste0("exact=", sum(compatibility$shared_spectra_identical),
           "/", nrow(compatibility))
  )
  list(
    compatibility = compatibility,
    group_rows = data.table::rbindlist(group_rows, fill = TRUE),
    final_rows = data.table::rbindlist(final_rows, fill = TRUE)
  )
}

audit_group_completeness <- function(group_rows, checks) {
  required_nonblank <- c("material_class", "material_type", "material_group")
  missing <- group_rows[, Reduce(`|`, lapply(.SD, function(value) {
    is.na(value) | !nzchar(trimws(value))
  })), .SDcols = required_nonblank]
  mapping <- unique(group_rows[, .(
    material_class, material_type, material_group
  )])
  conflicts <- mapping[, .(
    type_routes = data.table::uniqueN(material_type),
    group_routes = data.table::uniqueN(material_group)
  ), by = material_class][type_routes != 1L | group_routes != 1L]
  checks$add(
    "material_group_complete", nrow(group_rows) > 0L && !any(missing),
    paste0("rows=", nrow(group_rows), "; incomplete=", sum(missing))
  )
  checks$add(
    "material_group_one_path_per_class", nrow(conflicts) == 0L,
    paste0("conflicting classes=", nrow(conflicts))
  )
  list(mapping = mapping[order(material_type, material_group, material_class)],
       conflicts = conflicts)
}

audit_generic_reassignment <- function(upstream, final_rows, checks) {
  review <- as_dt(upstream$other_review)
  reassigned <- as_dt(upstream$pruning_reassignments)
  generic <- c("other", "other plastic", "other material", "unclassified")
  queued <- if (nrow(review) && "action" %in% names(review)) {
    review[action %in% c(
      "retained_for_correlation_reassignment", "reassigned",
      "retained_no_finite_candidate", "retained_no_eligible_candidate",
      "retained_reassignment_not_attempted", "removed_before_reassignment",
      "reassignment_audit_missing"
    )]
  } else data.table::data.table()
  generic_reassigned <- if (nrow(reassigned) && "prior_class" %in% names(reassigned)) {
    reassigned[tolower(trimws(prior_class)) %in% generic]
  } else data.table::data.table()

  invalid <- if (nrow(generic_reassigned)) {
    generic_reassigned[
      is.na(material_class) |
        tolower(trimws(material_class)) %in% generic |
        is.na(matched_id) | !nzchar(trimws(matched_id)) |
        !is.finite(correlation) |
        reason != "nearest_eligible_class"
    ]
  } else data.table::data.table()
  plastic_pool_invalid <- if (nrow(generic_reassigned)) {
    generic_reassigned[
      tolower(trimws(prior_class)) == "other plastic" &
        tolower(trimws(material_type)) != "plastic"
    ]
  } else data.table::data.table()
  nonplastic_pool_invalid <- if (nrow(generic_reassigned)) {
    generic_reassigned[
      tolower(trimws(prior_class)) == "other material" &
        tolower(trimws(material_type)) != "not plastic"
    ]
  } else data.table::data.table()

  status <- data.table::data.table()
  if (nrow(queued) && all(c("artifact", "spectrum_id") %in% names(queued))) {
    status <- unique(queued[, .(
      artifact = as.character(artifact), spectrum_id = as.character(spectrum_id),
      review_action = as.character(action),
      spectrum_identity = if ("spectrum_identity" %in% names(queued))
        as.character(spectrum_identity) else NA_character_,
      original_material_form = {
        current <- if ("original_material_form" %in% names(queued)) {
          as.character(original_material_form)
        } else rep(NA_character_, .N)
        legacy <- if ("material_form" %in% names(queued)) {
          as.character(material_form)
        } else rep(NA_character_, .N)
        fallback <- is.na(current) | !nzchar(trimws(current))
        current[fallback] <- legacy[fallback]
        current
      }
    )])
    reassignment_keys <- if (nrow(generic_reassigned)) {
      unique(generic_reassigned[, .(artifact, spectrum_id, material_class,
                                    matched_id, correlation)])
    } else data.table::data.table(
      artifact = character(), spectrum_id = character(),
      material_class = character(), matched_id = character(),
      correlation = numeric()
    )
    status <- merge(
      status, reassignment_keys, by = c("artifact", "spectrum_id"), all.x = TRUE
    )
    final_index <- unique(final_rows[, .(
      artifact, spectrum_id, final_material_class = material_class,
      final_material_type = material_type, final_material_group = material_group,
      final_spectrum_identity = spectrum_identity,
      final_material_form = material_form
    )])
    status <- merge(
      status, final_index, by = c("artifact", "spectrum_id"), all.x = TRUE,
      allow.cartesian = TRUE
    )
    followup <- as_dt(upstream$pruning_reassignments)
    followup <- if (nrow(followup) && all(c(
        "artifact", "spectrum_id", "material_class", "reason"
      ) %in% names(followup))) {
      followup[
        reason != "nearest_eligible_class",
        .(
          downstream_material_class = tail(as.character(material_class), 1L),
          downstream_reassignment_reason = paste(unique(reason), collapse = "|")
        ),
        by = .(artifact, spectrum_id)
      ]
    } else data.table::data.table(
      artifact = character(), spectrum_id = character(),
      downstream_material_class = character(),
      downstream_reassignment_reason = character()
    )
    status <- merge(
      status, followup, by = c("artifact", "spectrum_id"), all.x = TRUE
    )
    pruning_removals <- as_dt(upstream$pruning_removals)
    pruning_removals <- if (nrow(pruning_removals) && all(c(
        "artifact", "spectrum_id", "reason"
      ) %in% names(pruning_removals))) {
      pruning_removals[, .(
        downstream_removal_reason = paste(unique(reason), collapse = "|")
      ), by = .(artifact, spectrum_id)]
    } else data.table::data.table(
      artifact = character(), spectrum_id = character(),
      downstream_removal_reason = character()
    )
    status <- merge(
      status, pruning_removals,
      by = c("artifact", "spectrum_id"), all.x = TRUE
    )
    quality_removals <- as_dt(upstream$quality_control)
    quality_removals <- if (nrow(quality_removals) && all(c(
        "artifact", "spectrum_id", "removed", "reason"
      ) %in% names(quality_removals))) {
      quality_removals[removed == TRUE, .(
        quality_removal_reason = paste(unique(reason), collapse = "|")
      ), by = .(artifact, spectrum_id)]
    } else data.table::data.table(
      artifact = character(), spectrum_id = character(),
      quality_removal_reason = character()
    )
    status <- merge(
      status, quality_removals,
      by = c("artifact", "spectrum_id"), all.x = TRUE
    )
    status[, audit_status := data.table::fcase(
      review_action == "reassignment_audit_missing",
      "reassignment_audit_missing",
      !is.na(matched_id) & final_material_class == material_class,
      "reassigned_and_retained",
      !is.na(matched_id) & !is.na(final_material_class) &
        final_material_class == downstream_material_class,
      "reassigned_then_support_reassigned_and_retained",
      !is.na(matched_id) & is.na(final_material_class) &
        !is.na(downstream_removal_reason),
      "reassigned_then_pruned_with_audit",
      !is.na(matched_id) & is.na(final_material_class) &
        !is.na(quality_removal_reason),
      "reassigned_then_quality_removed_with_audit",
      is.na(matched_id) & is.na(final_material_class) &
        !is.na(quality_removal_reason),
      "queued_then_quality_removed_with_audit",
      review_action == "retained_reassignment_not_attempted" &
        is.na(matched_id) &
        tolower(trimws(final_material_class)) %in% generic,
      "unpruned_generic_retained_with_audit",
      is.na(matched_id) & tolower(trimws(final_material_class)) %in% generic,
      "audited_no_candidate_retained",
      default = "missing_or_mismatched_final_row"
    )]
  }
  status_ok <- nrow(status) > 0L &&
    !any(status$audit_status %in% c(
      "missing_or_mismatched_final_row", "reassignment_audit_missing"
    ))
  checks$add(
    "generic_reassignment_audit_available", nrow(queued) > 0L,
    paste0("queued=", nrow(queued), "; reassigned=", nrow(generic_reassigned))
  )
  checks$add(
    "generic_reassignment_valid", nrow(invalid) == 0L &&
      nrow(plastic_pool_invalid) == 0L && nrow(nonplastic_pool_invalid) == 0L,
    paste0(
      "invalid=", nrow(invalid), "; plastic-pool=", nrow(plastic_pool_invalid),
      "; nonplastic-pool=", nrow(nonplastic_pool_invalid)
    )
  )
  checks$add(
    "generic_reassignment_final_rows", status_ok,
    paste0("audited=", nrow(status), "; unresolved=",
           sum(status$audit_status %in% c(
             "missing_or_mismatched_final_row", "reassignment_audit_missing"
           )))
  )
  list(
    status = status,
    invalid = data.table::rbindlist(list(
    invalid[, issue := "invalid_reassignment"],
    plastic_pool_invalid[, issue := "plastic_pool_violation"],
      nonplastic_pool_invalid[, issue := "nonplastic_pool_violation"],
      status[audit_status %in% c(
        "missing_or_mismatched_final_row", "reassignment_audit_missing"
      ),
             issue := "unresolved_final_disposition"]
    ), fill = TRUE)
  )
}

audit_saved_closure <- function(evidence, upstream, checks) {
  closure <- as_dt(evidence$correlation_closure %||%
                     upstream$correlation_closure)
  expected <- data.table::CJ(
    artifact = c("derivative", "nobaseline"),
    spectrum_type = c("ftir", "nir", "raman")
  )
  keys_ok <- all(c("artifact", "spectrum_type") %in% names(closure)) &&
    setequal(
      paste(closure$artifact, closure$spectrum_type),
      paste(expected$artifact, expected$spectrum_type)
    )
  invariant_cols <- c("ids_unchanged", "classes_unchanged", "spectra_unchanged")
  invariant_ok <- all(invariant_cols %in% names(closure)) &&
    all(vapply(invariant_cols, function(column) {
      all(closure[[column]] %in% TRUE)
    }, logical(1L)))
  correlation_ok <- all(c(
    "wrong_class_pairs_above_threshold", "threshold",
    "max_wrong_class_correlation"
  ) %in% names(closure)) &&
    all(closure$wrong_class_pairs_above_threshold == 0L) &&
    all(is.na(closure$max_wrong_class_correlation) |
          closure$max_wrong_class_correlation <= closure$threshold)
  checks$add("saved_closure_six_parent_medoid_checks", keys_ok,
             paste0("rows=", nrow(closure)))
  checks$add("saved_closure_identity_invariants", invariant_ok,
             paste(invariant_cols, collapse = "|"))
  checks$add("saved_closure_correlation_invariant", correlation_ok,
             paste0("wrong pairs=", sum(
               closure$wrong_class_pairs_above_threshold %||% NA_integer_,
               na.rm = TRUE
             )))
  closure
}

model_converged <- function(model) {
  if (isTRUE(model$selected_lambda_converged)) return(TRUE)
  if (!length(model$gated_nodes)) return(FALSE)
  all(vapply(model$gated_nodes, function(node) {
    if (isTRUE(node$deterministic)) return(TRUE)
    if (!is.null(node$selected_lambda_converged)) {
      return(isTRUE(node$selected_lambda_converged))
    }
    error <- node$model$jerr
    is.numeric(error) && length(error) == 1L && !is.na(error) && error == 0L
  }, logical(1L)))
}

audit_gated_models <- function(new_dir, evidence, checks) {
  recipes <- c("derivative", "nobaseline")
  types <- c("raman", "ftir", "nir", "both")
  rows <- list()
  model_non_deterministic <- list()
  for (recipe in recipes) {
    path <- file.path(
      new_dir,
      paste0("model_gated_hierarchical_logistic_regression_", recipe, ".rds")
    )
    collection <- readRDS(path)
    checks$add(
      paste0("gated_collection_types_", recipe),
      identical(sort(names(collection)), sort(types)),
      paste(names(collection), collapse = "|")
    )
    for (type in types) {
      model <- collection[[type]]
      key <- paste(recipe, type, sep = "/")
      required <- c(
        "model", "model_type", "model_schema", "hierarchy_cols",
        "root_node_id", "gated_nodes", "gated_topology", "gated_edges",
        "class_names", "class_num", "fill", "all_variables",
        "training_elapsed_seconds"
      )
      complete <- is.list(model) && all(required %in% names(model)) &&
        is_reference_object(model$fill)
      hierarchy_ok <- complete &&
        identical(model$model_type, "gated_hierarchical_logistic_regression") &&
        identical(model$model_schema, "OpenSpecy_gated_hierarchy_v1") &&
        identical(
          as.character(model$hierarchy_cols),
          c("material_type", "material_group")
        ) &&
        identical(model$probability_semantics,
                  "terminal_conditional_probability") &&
        identical(model$selection_rule, "guardrailed") &&
        isTRUE(all.equal(
          as.numeric(model$selection_accuracy_tolerance), 0.01,
          tolerance = 0
        ))
      topology <- as_dt(model$gated_topology)
      edges <- as_dt(model$gated_edges)
      topology_ok <- tryCatch({
        required_topology <- c(
          "node_id", "parent_id", "incoming_label", "depth",
          "target_col", "terminal"
        )
        required_edges <- c("node_id", "label", "child_node_id")
        stopifnot(
          hierarchy_ok, nrow(topology) > 0L,
          all(required_topology %in% names(topology)),
          all(required_edges %in% names(edges)),
          !anyDuplicated(topology$node_id),
          sum(topology$depth == 0L) == 1L,
          model$root_node_id %in% topology$node_id,
          setequal(names(model$gated_nodes), topology$node_id),
          all(edges$node_id %in% topology$node_id),
          all(edges$child_node_id %in% topology$node_id),
          identical(
            sort(unique(topology$target_col[topology$depth == 0L])),
            "material_type"
          ),
          identical(
            sort(unique(topology$target_col[topology$depth == 1L])),
            "material_group"
          ),
          identical(
            sort(unique(topology$target_col[topology$terminal])),
            "material_class"
          )
        )
        for (current_node_id in topology[terminal == FALSE, node_id]) {
          stopifnot(setequal(
            as.character(model$gated_nodes[[current_node_id]]$class_names),
            as.character(edges[edges[["node_id"]] == current_node_id, label])
          ))
        }
        joined_edges <- merge(
          edges, topology[, .(
            child_node_id = node_id,
            child_parent_id = parent_id,
            child_incoming_label = incoming_label
          )],
          by = "child_node_id", all.x = TRUE, sort = FALSE
        )
        stopifnot(
          all(joined_edges$node_id == joined_edges$child_parent_id),
          all(joined_edges$label == joined_edges$child_incoming_label)
        )
        terminal_ids <- topology[terminal == TRUE, node_id]
        terminal_classes <- unlist(lapply(
          terminal_ids,
          function(node_id) model$gated_nodes[[node_id]]$class_names
        ), use.names = FALSE)
        stopifnot(
          !anyDuplicated(terminal_classes),
          setequal(as.character(terminal_classes), model$class_names)
        )
        TRUE
      }, error = function(...) FALSE)
      runtime <- suppressWarnings(as.numeric(model$training_elapsed_seconds))
      runtime_ok <- length(runtime) == 1L && is.finite(runtime) && runtime >= 0
      within_target <- runtime_ok && runtime < fit_runtime_ceiling_seconds
      converged <- complete && model_converged(model)
      non_deterministic <- complete && any(!vapply(
        model$gated_nodes, function(node) isTRUE(node$deterministic), logical(1L)
      ))
      model_non_deterministic[[key]] <- non_deterministic
      rows[[length(rows) + 1L]] <- data.table::data.table(
        algorithm = "gated_hierarchical_logistic_regression",
        artifact = recipe, model = type, complete = complete,
        hierarchy_valid = hierarchy_ok, topology_valid = topology_ok,
        selected_lambda_converged = converged,
        training_elapsed_seconds = if (length(runtime)) runtime[[1L]] else NA_real_,
        within_one_hour_target = within_target,
        nodes = nrow(topology), leaves = length(model$class_names %||% character()),
        non_deterministic = non_deterministic
      )
    }
    rm(collection)
    gc(verbose = FALSE)
  }
  audit <- data.table::rbindlist(rows, fill = TRUE)
  checks$add(
    "all_eight_gated_models_complete", nrow(audit) == 8L &&
      all(audit$complete & audit$hierarchy_valid & audit$topology_valid),
    paste0("valid=", sum(audit$complete & audit$hierarchy_valid &
                           audit$topology_valid), "/8")
  )
  checks$add(
    "all_gated_models_converged", all(audit$selected_lambda_converged),
    paste0("converged=", sum(audit$selected_lambda_converged), "/8")
  )
  finite_runtime <- audit$training_elapsed_seconds[
    is.finite(audit$training_elapsed_seconds)
  ]
  maximum_runtime <- if (length(finite_runtime)) {
    max(finite_runtime)
  } else NA_real_
  checks$add(
    "gated_training_runtime_target", all(audit$within_one_hour_target),
    paste0(
      "maximum seconds=", maximum_runtime,
      "; ceiling seconds=", fit_runtime_ceiling_seconds
    )
  )

  essential_tables <- c(
    "model_training_tests", "model_tests", "model_hierarchy_metrics",
    "model_source_metrics", "model_node_support", "model_fold_assignments",
    "model_class_metrics"
  )
  coverage_rows <- list()
  for (recipe in recipes) for (type in types) {
    key <- paste(recipe, type, sep = "/")
    for (table_name in essential_tables) {
      table <- as_dt(evidence[[table_name]])
      count <- if (all(c("algorithm", "artifact", "model") %in% names(table))) {
        nrow(table[
          algorithm == "gated_hierarchical_logistic_regression" &
            artifact == recipe & model == type
        ])
      } else 0L
      coverage_rows[[length(coverage_rows) + 1L]] <- data.table::data.table(
        artifact = recipe, model = type, evidence_table = table_name,
        rows = count, required = TRUE
      )
    }
    lambda <- as_dt(evidence$model_lambda_metrics)
    lambda_n <- if (all(c("algorithm", "artifact", "model") %in% names(lambda))) {
      nrow(lambda[
        algorithm == "gated_hierarchical_logistic_regression" &
          artifact == recipe & model == type
      ])
    } else 0L
    coverage_rows[[length(coverage_rows) + 1L]] <- data.table::data.table(
      artifact = recipe, model = type,
      evidence_table = "model_lambda_metrics", rows = lambda_n,
      required = isTRUE(model_non_deterministic[[key]])
    )
  }
  coverage <- data.table::rbindlist(coverage_rows)
  checks$add(
    "all_gated_model_evidence_present",
    all(!coverage$required | coverage$rows > 0L),
    paste0("missing required tables=", nrow(coverage[required & rows == 0L]))
  )

  folds <- as_dt(evidence$model_fold_assignments)
  gated_folds <- if (all(c("algorithm", "artifact", "model") %in% names(folds))) {
    folds[algorithm == "gated_hierarchical_logistic_regression"]
  } else data.table::data.table()
  group_leaks <- if (nrow(gated_folds) &&
      all(c("group_id", "fold") %in% names(gated_folds))) {
    gated_folds[, .(folds = data.table::uniqueN(fold)),
                by = .(artifact, model, group_id)][folds > 1L]
  } else data.table::data.table(issue = "missing fold evidence")
  source_leaks <- if (nrow(gated_folds) && all(c(
      "source_component_id", "source_fold"
    ) %in% names(gated_folds))) {
    gated_folds[, .(source_folds = data.table::uniqueN(source_fold)),
                by = .(artifact, model, source_component_id)][source_folds > 1L]
  } else data.table::data.table(issue = "missing source-fold evidence")
  split <- as_dt(evidence$split_manifest)
  split_leaks <- if (nrow(split) && all(c(
      "artifact", "source", "group_id", "split"
    ) %in% names(split))) {
    split[, .(splits = data.table::uniqueN(split)),
          by = .(artifact, source, group_id)][splits > 1L]
  } else data.table::data.table()
  checks$add(
    "gated_identity_fold_leakage", nrow(group_leaks) == 0L,
    paste0("leaking groups=", nrow(group_leaks))
  )
  checks$add(
    "gated_source_fold_leakage", nrow(source_leaks) == 0L,
    paste0("leaking source components=", nrow(source_leaks))
  )
  checks$add(
    "comparison_split_leakage", nrow(split_leaks) == 0L,
    paste0("leaking groups=", nrow(split_leaks))
  )

  training_tests <- as_dt(evidence$model_training_tests)
  gated_tests <- if (all(c("algorithm", "artifact", "model") %in%
                         names(training_tests))) {
    training_tests[
      algorithm == "gated_hierarchical_logistic_regression"
    ]
  } else data.table::data.table()
  oof_ok <- nrow(gated_tests) > 0L && "provenance" %in% names(gated_tests) &&
    all(grepl("oof", gated_tests$provenance, ignore.case = TRUE))
  checks$add(
    "gated_training_evidence_is_oof", oof_ok,
    paste0("rows=", nrow(gated_tests))
  )
  if (nrow(gated_tests) && nrow(gated_folds)) {
    keys <- c("algorithm", "artifact", "model", "spectrum_id")
    test_keys <- unique(gated_tests[, keys, with = FALSE])
    fold_keys <- unique(gated_folds[, keys, with = FALSE])
    missing_fold <- test_keys[!fold_keys, on = keys]
  } else {
    missing_fold <- data.table::data.table(issue = "missing tests or folds")
  }
  checks$add(
    "gated_oof_rows_have_fold_evidence", nrow(missing_fold) == 0L,
    paste0("missing fold rows=", nrow(missing_fold))
  )
  leakage <- data.table::rbindlist(list(
    group_leaks[, leakage_kind := "identity_group_multiple_folds"],
    source_leaks[, leakage_kind := "source_component_multiple_source_folds"],
    split_leaks[, leakage_kind := "comparison_group_multiple_splits"],
    missing_fold[, leakage_kind := "oof_row_missing_fold"]
  ), fill = TRUE)
  list(
    audit = audit, coverage = coverage, leakage = leakage,
    gated_tests = gated_tests, gated_folds = gated_folds
  )
}

untyped_model_label <- function(value) {
  sub("^[^_]+_", "", as.character(value))
}

audit_vinyl_evidence <- function(gated, external_path, checks) {
  tests <- gated$gated_tests
  folds <- gated$gated_folds
  keys <- c("algorithm", "artifact", "model", "spectrum_id")
  detail <- data.table::data.table()
  required_fold_columns <- c(
    keys, "source_id", "source_component_id", "source_fold", "fold",
    "material_group", "final_class"
  )
  if (nrow(tests) && nrow(folds) && all(keys %in% names(tests)) &&
      all(required_fold_columns %in% names(folds))) {
    keep <- unique(folds[, c(
      keys, "source_id", "source_component_id", "source_fold",
      "fold", "material_group", "final_class"
    ), with = FALSE])
    detail <- merge(tests, keep, by = keys, all = FALSE)
    detail[, `:=`(
      expected_material_group = material_group,
      predicted_material_group = if ("gate_2_name" %in% names(detail)) {
        as.character(gate_2_name)
      } else NA_character_
    )]
    detail[, `:=`(
      expected_material_group_taxonomy = untyped_model_label(
        expected_material_group
      ),
      predicted_material_group_taxonomy = untyped_model_label(
        predicted_material_group
      ),
      expected_class_taxonomy = untyped_model_label(expected_class),
      predicted_class_taxonomy = untyped_model_label(predicted_class)
    )]
    detail <- detail[
      tolower(trimws(expected_material_group_taxonomy)) ==
        "vinyl alcohol-acetate-acetal" |
        tolower(trimws(predicted_material_group_taxonomy)) ==
        "vinyl alcohol-acetate-acetal"
    ]
  }
  summary <- if (nrow(detail)) {
    detail[, .(
      observations = .N,
      sources = data.table::uniqueN(source_id),
      identity_folds = data.table::uniqueN(fold),
      source_folds = data.table::uniqueN(source_fold),
      leaf_accuracy = mean(expected_class == predicted_class, na.rm = TRUE),
      group_route_accuracy = mean(
        predicted_material_group_taxonomy ==
          expected_material_group_taxonomy,
        na.rm = TRUE
      )
    ), by = .(artifact, model)]
  } else data.table::data.table()
  vinyl_classes <- c(
    "polyethylene-vinyl acetate", "polyvinylalcohols", "polyvinylesters"
  )
  observed_vinyl_classes <- if (nrow(detail)) unique(detail[
    expected_material_group_taxonomy == "vinyl alcohol-acetate-acetal",
    expected_class_taxonomy
  ]) else character()
  checks$add(
    "vinyl_family_oof_evidence",
    nrow(detail) > 0L && all(vinyl_classes %in% observed_vinyl_classes),
    paste0(
      "rows=", nrow(detail), "; model/type groups=", nrow(summary),
      "; missing true classes=",
      paste(setdiff(vinyl_classes, observed_vinyl_classes), collapse = "|")
    )
  )

  external <- data.table::data.table()
  if (nzchar(external_path)) {
    external <- tryCatch(
      data.table::fread(external_path),
      error = function(error) data.table::data.table(
        audit_read_error = conditionMessage(error)
      )
    )
    external_required <- c(
      "source_id", "source_component_id", "source_fold", "source_fold_mode",
      "expected_material_group", "predicted_material_group",
      "expected_class", "predicted_class", "source_held_route", "split"
    )
    expected_vinyl_classes <- c(
      "polyethylene-vinyl acetate", "polyvinylalcohols", "polyvinylesters"
    )
    expected_group <- if (
      "expected_material_group_taxonomy" %in% names(external)
    ) external$expected_material_group_taxonomy else
      untyped_model_label(external$expected_material_group)
    expected_class <- if ("expected_class_taxonomy" %in% names(external)) {
      external$expected_class_taxonomy
    } else {
      untyped_model_label(external$expected_class)
    }
    external_ok <- nrow(external) > 0L &&
      !"audit_read_error" %in% names(external) &&
      all(external_required %in% names(external)) &&
      all(external$source_held_route %in% TRUE) &&
      all(external$split == "out_of_fold") &&
      all(expected_vinyl_classes %in% unique(expected_class[
        expected_group == "vinyl alcohol-acetate-acetal"
      ]))
    checks$add(
      "source_held_vinyl_external_evidence", external_ok,
      paste0("path=", external_path, "; rows=", nrow(external))
    )
  } else {
    checks$add(
      "source_held_vinyl_external_evidence", TRUE,
      "not supplied; identity-grouped OOF evidence with source-fold metadata retained",
      severity = "info"
    )
  }
  list(detail = detail, summary = summary, external = external)
}

audit_aliases_and_prior_models <- function(new_dir, prior_dir, checks) {
  rows <- list()
  for (recipe in c("derivative", "nobaseline")) {
    canonical_new_read <- safe_read_rds(file.path(
      new_dir, paste0("model_logistic_regression_", recipe, ".rds")
    ))
    alias_new_read <- safe_read_rds(file.path(
      new_dir, paste0("model_", recipe, ".rds")
    ))
    canonical_old_read <- safe_read_rds(file.path(
      prior_dir, paste0("model_logistic_regression_", recipe, ".rds")
    ))
    alias_old_read <- safe_read_rds(file.path(
      prior_dir, paste0("model_", recipe, ".rds")
    ))
    canonical_new <- canonical_new_read$value
    alias_new <- alias_new_read$value
    canonical_old <- canonical_old_read$value
    alias_old <- alias_old_read$value
    new_logistic_present <- canonical_new_read$ok || alias_new_read$ok
    if (new_logistic_present) {
      checks$add(
        paste0("new_logistic_collection_", recipe), canonical_new_read$ok,
        canonical_new_read$error
      )
      checks$add(
        paste0("new_logistic_alias_", recipe),
        canonical_new_read$ok && alias_new_read$ok &&
          identical(canonical_new, alias_new),
        "when a flat model is released, its historical alias must match"
      )
    } else {
      checks$add(
        paste0("new_logistic_collection_", recipe), TRUE,
        "not built in the gated-only candidate release", severity = "info"
      )
    }
    checks$add(
      paste0("prior_logistic_alias_", recipe),
      canonical_old_read$ok && alias_old_read$ok &&
        identical(canonical_old, alias_old),
      "prior alias pin"
    )
    for (type in sort(unique(c(names(canonical_new), names(canonical_old))))) {
      new_model <- canonical_new[[type]]
      old_model <- canonical_old[[type]]
      rows[[length(rows) + 1L]] <- data.table::data.table(
        artifact = recipe, model = type,
        new_present = !is.null(new_model), prior_present = !is.null(old_model),
        new_classes = length(new_model$class_names %||% character()),
        prior_classes = length(old_model$class_names %||% character()),
        shared_classes = length(intersect(
          new_model$class_names %||% character(),
          old_model$class_names %||% character()
        )),
        variable_axis_identical = identical(
          as.numeric(new_model$all_variables), as.numeric(old_model$all_variables)
        )
      )
    }
    rm(canonical_new, alias_new, canonical_old, alias_old)
    gc(verbose = FALSE)
  }
  data.table::rbindlist(rows, fill = TRUE)
}

audit_special_artifacts <- function(new_dir, prior_dir, new_signature, checks) {
  new_quarantine <- readRDS(file.path(new_dir, "quarantined_spectra.rds"))
  prior_quarantine <- readRDS(file.path(prior_dir, "quarantined_spectra.rds"))
  quarantine_ok <- is.list(new_quarantine) &&
    all(c("spectra", "conflicts", "manifest") %in% names(new_quarantine)) &&
    identical(
      new_quarantine$manifest$schema,
      "OpenSpecy_quarantined_spectra_v1"
    ) &&
    identical(new_quarantine$manifest$release_signature, new_signature) &&
    is.data.frame(new_quarantine$conflicts) &&
    all(vapply(new_quarantine$spectra, is_reference_object, logical(1L)))
  checks$add(
    "quarantine_artifact_contract", quarantine_ok,
    paste0(
      "new partitions=", length(new_quarantine$spectra %||% list()),
      "; prior partitions=", length(prior_quarantine$spectra %||% list())
    )
  )
  new_assessments <- readRDS(file.path(new_dir, "assessments.rds"))
  prior_assessments <- readRDS(file.path(prior_dir, "assessments.rds"))
  assessment_names <- c(
    "cleanup", "ref_lib", "medoid", "model", "functionality"
  )
  checks$add(
    "assessment_review_contract",
    identical(names(new_assessments), assessment_names) &&
      identical(names(prior_assessments), assessment_names),
    paste0(
      "new=", paste(names(new_assessments), collapse = "|"),
      "; prior=", paste(names(prior_assessments), collapse = "|")
    )
  )
  data.table::data.table(
    artifact = c("quarantined_spectra", "assessments"),
    new_elements = c(length(new_quarantine$spectra), length(new_assessments)),
    prior_elements = c(
      length(prior_quarantine$spectra), length(prior_assessments)
    ),
    contract_valid = c(
      quarantine_ok,
      identical(names(new_assessments), assessment_names) &&
        identical(names(prior_assessments), assessment_names)
    )
  )
}

run_representative_matching <- function(new_dir, prior_dir, checks) {
  devtools::load_all(repo_root(), quiet = TRUE)
  rows <- list()
  for (recipe in c("derivative", "nobaseline")) {
    library <- readRDS(file.path(new_dir, paste0(recipe, ".rds")))
    new_medoid <- readRDS(file.path(new_dir, paste0("medoid_", recipe, ".rds")))
    old_medoid <- readRDS(file.path(prior_dir, paste0("medoid_", recipe, ".rds")))
    for (type in c("raman", "ftir", "nir")) {
      query_object <- library[[type]]
      query <- subset_reference(query_object, 1L)
      for (release in c("new", "prior")) {
        reference <- if (release == "new") new_medoid[[type]] else old_medoid[[type]]
        result <- tryCatch(
          suppressWarnings(OpenSpecy::match_spec(
            query, library = reference, top_n = 1L,
            add_library_metadata = "sample_name"
          )),
          error = function(error) error
        )
        ok <- !inherits(result, "error") && nrow(result) == 1L &&
          "library_id" %in% names(result) &&
          !is.na(result$library_id[[1L]]) &&
          nzchar(as.character(result$library_id[[1L]])) &&
          "match_val" %in% names(result) && is.finite(result$match_val[[1L]])
        rows[[length(rows) + 1L]] <- data.table::data.table(
          artifact = recipe, spectrum_type = type,
          reference_release = release,
          query_id = reference_ids(query)[[1L]],
          expected_class = as.character(query$metadata$material_class[[1L]]),
          predicted_class = if (ok && "material_class" %in% names(result)) {
            as.character(result$material_class[[1L]])
          } else NA_character_,
          matched_id = if (ok) as.character(result$library_id[[1L]]) else NA_character_,
          score = if (ok) as.numeric(result$match_val[[1L]]) else NA_real_,
          passed = ok,
          error = if (inherits(result, "error")) conditionMessage(result) else NA_character_
        )
      }
    }
    rm(library, new_medoid, old_medoid)
    gc(verbose = FALSE)
  }
  rows <- data.table::rbindlist(rows, fill = TRUE)
  checks$add(
    "representative_matching", nrow(rows) == 12L && all(rows$passed),
    paste0("passed=", sum(rows$passed), "/", nrow(rows))
  )
  rows
}

write_outputs <- function(cli, outputs, checks, new_state, prior_state) {
  dir.create(cli$output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- c(outputs, list(audit_checks = checks$table()))
  for (name in names(tables)) {
    value <- tables[[name]]
    if (is.null(value)) value <- data.table::data.table()
    if (!ncol(value)) value <- data.table::data.table(status = character())
    data.table::fwrite(
      data.table::as.data.table(value),
      file.path(cli$output_dir, paste0(name, ".csv")), na = ""
    )
  }
  check_table <- checks$table()
  errors <- check_table[severity == "error" & !passed]
  warnings <- check_table[severity != "error" & !passed]
  status <- if (nrow(errors)) "FAILED" else "PASSED"
  report <- c(
    "# Reference release post-build audit",
    "",
    paste0("Status: **", status, "**"),
    "",
    paste0("- Runner: `", runner_version, "`"),
    paste0("- New release: `", cli$new_release, "`"),
    paste0("- New signature: `", new_state$signature, "`"),
    paste0("- Prior release: `", cli$prior_release, "`"),
    paste0("- Prior signature: `", prior_state$signature, "`"),
    paste0("- Error checks: ", nrow(check_table[severity == "error"]),
           "; failed: ", nrow(errors)),
    paste0("- Informational checks: ", nrow(check_table[severity != "error"]),
           "; failed: ", nrow(warnings)),
    "",
    "## Failed gates",
    "",
    if (nrow(errors)) paste0(
      "- `", errors$check, "`: ", errors$detail
    ) else "- None.",
    "",
    "## Evidence",
    "",
    "The CSV files beside this report contain both release inventories, direct",
    "15 typed-reference comparisons, taxonomy-route completeness, generic",
    "reassignment trace, saved closure, all eight gated-model checks, fold",
    "leakage checks, vinyl-family evidence, and representative matches.",
    "",
    "The source-held vinyl CSV is optional external evidence. When it is not",
    "supplied, the audit preserves the release's identity-grouped OOF rows with",
    "their source and source-fold assignments without mislabelling them as a",
    "separate source-held prediction run."
  )
  writeLines(report, file.path(cli$output_dir, "audit_report.md"))

  output_files <- sort(list.files(cli$output_dir, full.names = TRUE))
  output_files <- output_files[basename(output_files) != "audit_manifest.csv"]
  manifest <- data.table::data.table(
    file = basename(output_files),
    bytes = as.numeric(file.info(output_files)$size),
    sha256 = vapply(output_files, sha256_file, character(1L)),
    runner_version = runner_version,
    new_release_signature = new_state$signature,
    prior_release_signature = prior_state$signature
  )
  data.table::fwrite(manifest, file.path(cli$output_dir, "audit_manifest.csv"))
  invisible(status)
}

run_audit <- function(cli) {
  checks <- new_check_recorder()
  new_state <- validate_release(
    cli$new_release, "new", new_rds_files, checks
  )
  prior_state <- validate_release(
    cli$prior_release, "prior", prior_rds_files, checks,
    expected_signature = prior_signature,
    expected_manifest_hash = prior_manifest_sha256
  )
  inventory <- data.table::rbindlist(list(
    new_state$inventory,
    prior_state$inventory
  ), fill = TRUE)
  new_inventory <- data.table::copy(new_state$inventory)
  prior_inventory <- prior_state$inventory[, .(
    component, prior_exists = exists, prior_bytes = bytes,
    prior_sha256 = sha256, prior_readable = readable
  )]
  new_inventory <- merge(
    new_inventory, prior_inventory, by = "component", all.x = TRUE,
    sort = FALSE
  )
  new_inventory[, `:=`(
    prior_exists = data.table::fcoalesce(prior_exists, FALSE),
    compatibility_status = data.table::fcase(
      prior_exists, "direct_prior_artifact_available",
      startsWith(component, "model_gated_hierarchical"),
      "intentional_new_gated_artifact",
      default = "missing_unexpectedly_from_prior"
    ),
    byte_identical_to_prior = prior_exists & sha256 == prior_sha256
  )]
  checks$add(
    "new_release_payload_audit",
    nrow(new_inventory) == length(new_rds_files) &&
      all(new_inventory$exists & new_inventory$readable &
            new_inventory$manifest_hash_and_size_ok),
    paste0("valid=", sum(
      new_inventory$exists & new_inventory$readable &
        new_inventory$manifest_hash_and_size_ok
    ), "/", length(new_rds_files))
  )

  reference <- reference_compatibility(
    cli$new_release, cli$prior_release, checks
  )
  groups <- audit_group_completeness(reference$group_rows, checks)
  assessment <- read_assessment_parts(
    file.path(cli$new_release, "assessments.rds"), checks
  )
  generic <- audit_generic_reassignment(
    assessment$upstream, reference$final_rows, checks
  )
  closure <- audit_saved_closure(
    assessment$evidence, assessment$upstream, checks
  )
  gated <- audit_gated_models(
    cli$new_release, assessment$evidence, checks
  )
  vinyl <- audit_vinyl_evidence(gated, cli$vinyl_evidence, checks)
  legacy_models <- audit_aliases_and_prior_models(
    cli$new_release, cli$prior_release, checks
  )
  special <- audit_special_artifacts(
    cli$new_release, cli$prior_release, new_state$signature, checks
  )
  matching <- run_representative_matching(
    cli$new_release, cli$prior_release, checks
  )

  outputs <- list(
    release_payload_inventory = inventory,
    release_payload_compatibility = new_inventory,
    reference_artifact_compatibility = reference$compatibility,
    logistic_model_compatibility = legacy_models,
    special_artifact_compatibility = special,
    material_group_routes = groups$mapping,
    material_group_conflicts = groups$conflicts,
    generic_reassignment_audit = generic$status,
    generic_reassignment_failures = generic$invalid,
    saved_parent_medoid_closure = closure,
    gated_model_audit = gated$audit,
    gated_model_evidence_coverage = gated$coverage,
    fold_leakage_audit = gated$leakage,
    vinyl_family_oof_evidence = vinyl$detail,
    vinyl_family_oof_summary = vinyl$summary,
    source_held_vinyl_external_evidence = vinyl$external,
    representative_matching = matching
  )
  status <- write_outputs(
    cli, outputs, checks, new_state, prior_state
  )
  message("Audit ", tolower(status), ": ", cli$output_dir)
  if (!identical(status, "PASSED")) {
    stop("Post-build release audit failed; see audit_report.md", call. = FALSE)
  }
  invisible(cli$output_dir)
}

main <- function() {
  cli <- parse_cli()
  if (identical(cli$stage, "help")) return(usage())
  cli <- normalize_cli(cli)
  run_audit(cli)
}

if (sys.nframe() == 0L) main()
