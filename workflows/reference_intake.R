# Local reference-dataset intake helpers used by OpenSpecy_reference_library.R.
# Raw downloads, cleaners, plots, manifests, and CleanedOS outputs stay under
# ignored rawrefdata/. This tracked file defines the reproducible contract only.

.reference_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

.reference_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.reference_safe_path <- function(root, relative, label) {
  relative <- as.character(relative)
  if (length(relative) != 1L || .reference_blank(relative) ||
      grepl("^[A-Za-z]:|^[/\\\\]", relative) ||
      any(strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]] ==
            "..")) {
    stop(label, " must be one relative path below rawrefdata", call. = FALSE)
  }
  root <- normalizePath(root, mustWork = FALSE)
  path <- normalizePath(file.path(root, relative), mustWork = FALSE)
  root_key <- tolower(gsub("\\\\", "/", root))
  path_key <- tolower(gsub("\\\\", "/", path))
  prefix <- paste0(root_key, "/")
  if (!startsWith(path_key, prefix)) {
    stop(label, " escapes rawrefdata: ", relative, call. = FALSE)
  }
  path
}

reference_read_intake_manifest <- function(path, rawrefdata_dir) {
  if (!file.exists(path)) {
    stop("Reference intake manifest does not exist: ", path, call. = FALSE)
  }
  manifest <- data.table::fread(path, na.strings = c("", "NA"))
  required <- c(
    "candidate_id", "slug", "title", "canonical_release", "dataset_dir",
    "cleaner", "output", "state", "source_declared_license",
    "license_status", "maintainer_use_basis", "expected_spectrum_type",
    "citation", "origin", "diagnostic_plot", "visual_review_status",
    "downloaded_at", "raw_sha256", "cleaner_sha256", "output_sha256"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop("Reference intake manifest is missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(.reference_blank(manifest$slug)) || anyDuplicated(manifest$slug)) {
    stop("Reference intake slugs must be populated and unique", call. = FALSE)
  }
  if (any(!manifest$state %in% c(
      "planned", "downloaded", "cleaned", "reviewed", "admitted", "rejected"
  ))) {
    stop("Reference intake state is not recognized", call. = FALSE)
  }
  manifest[, dataset_path := vapply(seq_len(.N), function(i) {
    .reference_safe_path(rawrefdata_dir, dataset_dir[[i]], "dataset_dir")
  }, character(1L))]
  manifest[, cleaner_path := vapply(seq_len(.N), function(i) {
    .reference_safe_path(rawrefdata_dir, cleaner[[i]], "cleaner")
  }, character(1L))]
  manifest[, output_path := vapply(seq_len(.N), function(i) {
    .reference_safe_path(rawrefdata_dir, output[[i]], "output")
  }, character(1L))]
  manifest[, diagnostic_plot_path := vapply(seq_len(.N), function(i) {
    .reference_safe_path(
      rawrefdata_dir, diagnostic_plot[[i]], "diagnostic_plot"
    )
  }, character(1L))]
  expected_output <- file.path(
    manifest$dataset_path, "Processed", "CleanedOS_raw.rds"
  )
  if (any(tolower(normalizePath(manifest$output_path, mustWork = FALSE)) !=
          tolower(normalizePath(expected_output, mustWork = FALSE)))) {
    stop("Each output must be <dataset_dir>/Processed/CleanedOS_raw.rds",
         call. = FALSE)
  }
  admitted <- manifest$state == "admitted"
  admission_fields <- c(
    "canonical_release", "source_declared_license", "license_status",
    "maintainer_use_basis", "expected_spectrum_type", "citation", "origin",
    "diagnostic_plot", "visual_review_status", "downloaded_at",
    "raw_sha256", "cleaner_sha256", "output_sha256"
  )
  incomplete <- vapply(admission_fields, function(column) {
    any(admitted & .reference_blank(manifest[[column]]))
  }, logical(1L))
  if (any(incomplete)) {
    stop("Admitted manifest rows have blank fields: ",
         paste(admission_fields[incomplete], collapse = ", "), call. = FALSE)
  }
  manifest
}

reference_validate_cleaned_object <- function(object, manifest_row,
                                               require_visual = TRUE) {
  row <- data.table::as.data.table(manifest_row)
  if (nrow(row) != 1L) stop("One manifest row is required", call. = FALSE)
  warnings <- character()
  valid <- withCallingHandlers(
    check_OpenSpecy(object),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  problems <- character()
  if (!isTRUE(valid) || length(warnings)) {
    problems <- c(problems, paste(
      c("check_OpenSpecy_failed", warnings), collapse = "; "
    ))
  }
  metadata <- if (is_OpenSpecy(object)) object$metadata else data.table::data.table()
  if (!"spectrum_type" %in% names(metadata)) {
    problems <- c(problems, "missing spectrum_type metadata")
  } else {
    type <- tolower(trimws(as.character(metadata$spectrum_type)))
    expected <- trimws(strsplit(
      tolower(as.character(row$expected_spectrum_type)), ";", fixed = TRUE
    )[[1L]])
    expected <- expected[nzchar(expected)]
    if (any(.reference_blank(type)) || any(!type %in% c("ftir", "raman")) ||
        !length(expected) || any(!type %in% expected) ||
        any(!expected %in% unique(type))) {
      problems <- c(problems, "spectrum_type is not the source-verified ftir/raman value")
    }
  }
  if (!"intensity_units" %in% names(metadata) ||
      any(.reference_blank(metadata$intensity_units))) {
    problems <- c(problems, "intensity_units metadata is missing or blank")
  } else {
    transmission <- grepl(
      "transmi", tolower(as.character(metadata$intensity_units))
    )
    mode <- if ("instrument_mode" %in% names(metadata)) {
      tolower(as.character(metadata$instrument_mode))
    } else {
      rep(NA_character_, nrow(metadata))
    }
    if (any(transmission & (.reference_blank(mode) |
                            !grepl("transmi", mode)))) {
      problems <- c(problems, "transmittance spectra lack transmission instrument_mode")
    }
  }
  required_metadata <- c(
    "sample_name", "spectrum_identity", "citation", "license",
    "canonical_release", "source_declared_license", "license_status",
    "maintainer_use_basis", "origin"
  )
  absent <- required_metadata[
    !required_metadata %in% names(metadata) |
      vapply(required_metadata, function(column) {
        column %in% names(metadata) && any(.reference_blank(metadata[[column]]))
      }, logical(1L))
  ]
  if (length(absent)) {
    problems <- c(problems, paste0(
      "missing or blank required metadata: ", paste(absent, collapse = ", ")
    ))
  }
  if (is_OpenSpecy(object) &&
      (any(!is.finite(object$wavenumber)) || anyDuplicated(object$wavenumber) ||
       ncol(object$spectra) != nrow(metadata))) {
    problems <- c(problems, "spectra, axis, and metadata are not aligned")
  }
  if (isTRUE(require_visual) &&
      (!file.exists(row$diagnostic_plot_path) ||
       !identical(tolower(row$visual_review_status), "approved"))) {
    problems <- c(problems, "diagnostic plot has not been explicitly approved")
  }
  data.table::data.table(
    slug = as.character(row$slug), valid = !length(problems),
    spectra = if (is_OpenSpecy(object)) ncol(object$spectra) else 0L,
    wavenumbers = if (is_OpenSpecy(object)) length(object$wavenumber) else 0L,
    problems = if (length(problems)) paste(unique(problems), collapse = "; ") else NA_character_
  )
}

reference_write_diagnostic_plot <- function(object, path, sample_n = 24L,
                                            envelope_n = 512L) {
  stopifnot(is_OpenSpecy(object), isTRUE(check_OpenSpecy(object)))
  sample_n <- max(1L, min(as.integer(sample_n), ncol(object$spectra)))
  envelope_n <- max(1L, min(as.integer(envelope_n), ncol(object$spectra)))
  regular <- unique(round(seq(1, ncol(object$spectra), length.out = sample_n)))
  magnitude <- colMeans(abs(object$spectra), na.rm = TRUE)
  ranked <- order(magnitude, method = "radix")
  extremes <- unique(c(
    ranked[[1L]], ranked[[length(ranked)]],
    ranked[[max(1L, round(length(ranked) / 2L))]]
  ))
  columns <- unique(c(regular, extremes))
  diagnostic <- object$spectra[, columns, drop = FALSE]
  envelope_columns <- unique(round(seq(
    1, ncol(object$spectra), length.out = envelope_n
  )))
  envelope <- matrixStats::rowQuantiles(
    object$spectra[, envelope_columns, drop = FALSE],
    probs = c(0.01, 0.25, 0.5, 0.75, 0.99), na.rm = TRUE
  )
  units <- if ("intensity_units" %in% names(object$metadata)) {
    unique(trimws(as.character(object$metadata$intensity_units)))
  } else {
    character()
  }
  units <- units[!is.na(units) & nzchar(units)]
  ylab <- if (length(units) == 1L) {
    paste0("Intensity (", units, ")")
  } else {
    "Intensity"
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(path, width = 1800, height = 1100, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2, 1), mar = c(4.2, 4.5, 3.2, 1.2))
  graphics::plot(
    object$wavenumber, envelope[, 3L], type = "n",
    ylim = range(envelope[, c(1L, 5L)], finite = TRUE),
    xlab = expression("Wavenumber (cm"^-1*")"), ylab = ylab,
    main = paste0(
      "Full-axis quantile envelope; deterministic spectra n=",
      length(envelope_columns)
    )
  )
  graphics::polygon(
    c(object$wavenumber, rev(object$wavenumber)),
    c(envelope[, 1L], rev(envelope[, 5L])),
    border = NA, col = grDevices::adjustcolor("#9CB8DD", alpha.f = 0.45)
  )
  graphics::polygon(
    c(object$wavenumber, rev(object$wavenumber)),
    c(envelope[, 2L], rev(envelope[, 4L])),
    border = NA, col = grDevices::adjustcolor("#4A7BBC", alpha.f = 0.55)
  )
  graphics::lines(object$wavenumber, envelope[, 3L], col = "#123B70", lwd = 1.4)
  graphics::grid()
  graphics::matplot(
    object$wavenumber, diagnostic, type = "l", lty = 1,
    col = grDevices::adjustcolor("#2457A7", alpha.f = 0.25),
    xlab = expression("Wavenumber (cm"^-1*")"), ylab = ylab,
    main = paste0(
      "Representative plus magnitude-extreme traces: ",
      ncol(object$spectra), " total; plotted n=", length(columns)
    )
  )
  graphics::grid()
  invisible(path)
}

reference_collect_admitted_inputs <- function(rawrefdata_dir, manifest_path,
                                               run_cleaners = FALSE,
                                               protected = c("FTIR", "Raman")) {
  manifest <- reference_read_intake_manifest(manifest_path, rawrefdata_dir)
  admitted <- manifest[state == "admitted"]
  if (!nrow(admitted)) {
    return(list(files = character(), validation = data.table::data.table(),
                source_failures = data.table::data.table()))
  }
  top_component <- function(path) {
    relative <- substring(
      normalizePath(path, mustWork = FALSE),
      nchar(normalizePath(rawrefdata_dir, mustWork = FALSE)) + 2L
    )
    strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]][[1L]]
  }
  if (isTRUE(run_cleaners)) {
    locked <- vapply(admitted$dataset_path, function(path) {
      tolower(top_component(path)) %in% tolower(protected)
    }, logical(1L))
    if (any(locked)) {
      stop("Cleaner execution is write-locked for active FTIR/Raman folders: ",
           paste(admitted$slug[locked], collapse = ", "), call. = FALSE)
    }
    for (i in seq_len(nrow(admitted))) {
      if (!file.exists(admitted$cleaner_path[[i]])) {
        stop("Cleaner is missing: ", admitted$cleaner_path[[i]], call. = FALSE)
      }
      environment <- new.env(parent = globalenv())
      sys.source(admitted$cleaner_path[[i]], envir = environment)
      cleaner <- environment$clean_reference_dataset
      if (!is.function(cleaner)) {
        stop("Cleaner must define clean_reference_dataset(): ",
             admitted$cleaner_path[[i]], call. = FALSE)
      }
      cleaner_args <- list(
        dataset_dir = admitted$dataset_path[[i]],
        output_path = admitted$output_path[[i]]
      )
      if ("visual_review_status" %in% names(formals(cleaner))) {
        cleaner_args$visual_review_status <-
          admitted$visual_review_status[[i]]
      }
      do.call(cleaner, cleaner_args)
    }
  }
  missing_cleaner <- !file.exists(admitted$cleaner_path)
  if (any(missing_cleaner)) {
    stop("Admitted cleaner is missing: ",
         paste(admitted$cleaner_path[missing_cleaner], collapse = ", "),
         call. = FALSE)
  }
  cleaner_hash <- vapply(admitted$cleaner_path, .reference_sha256,
                         character(1L))
  if (any(cleaner_hash != admitted$cleaner_sha256)) {
    stop("Admitted cleaner_sha256 does not match: ",
         paste(admitted$slug[cleaner_hash != admitted$cleaner_sha256],
               collapse = ", "), call. = FALSE)
  }
  failures <- list()
  validation <- lapply(seq_len(nrow(admitted)), function(i) {
    path <- admitted$output_path[[i]]
    if (!file.exists(path)) {
      failures[[length(failures) + 1L]] <<- data.table::data.table(
        source = path, stage = "intake", reason = "missing_cleaned_output",
        error = "CleanedOS_raw.rds does not exist"
      )
      return(data.table::data.table(
        slug = admitted$slug[[i]], valid = FALSE, spectra = 0L,
        wavenumbers = 0L, problems = "missing CleanedOS_raw.rds"
      ))
    }
    object <- tryCatch(readRDS(path), error = function(error) error)
    if (inherits(object, "error")) {
      failures[[length(failures) + 1L]] <<- data.table::data.table(
        source = path, stage = "intake", reason = "unreadable_cleaned_output",
        error = conditionMessage(object)
      )
      return(data.table::data.table(
        slug = admitted$slug[[i]], valid = FALSE, spectra = 0L,
        wavenumbers = 0L, problems = conditionMessage(object)
      ))
    }
    result <- reference_validate_cleaned_object(object, admitted[i])
    expected_hash <- as.character(admitted$output_sha256[[i]])
    actual_hash <- .reference_sha256(path)
    if (.reference_blank(expected_hash) || !identical(expected_hash, actual_hash)) {
      result[, `:=`(
        valid = FALSE,
        problems = paste(na.omit(c(
          problems, "output_sha256 is blank or does not match"
        )), collapse = "; ")
      )]
    }
    result
  })
  validation <- data.table::rbindlist(validation, fill = TRUE)
  source_failures <- data.table::rbindlist(failures, fill = TRUE)
  invalid <- validation[valid == FALSE]
  if (nrow(invalid)) {
    invalid_failures <- invalid[, .(
      source = admitted$output_path[match(slug, admitted$slug)],
      stage = "intake", reason = "invalid_cleaned_output", error = problems
    )]
    source_failures <- unique(data.table::rbindlist(
      list(source_failures, invalid_failures), fill = TRUE
    ))
  }
  list(
    files = admitted$output_path,
    validation = validation,
    source_failures = source_failures
  )
}

reference_new_hash_deduper <- function(target_wavenumber, scale = 100,
                                       algo = "sha256") {
  target_wavenumber <- as.numeric(target_wavenumber)
  if (length(target_wavenumber) < 2L || any(!is.finite(target_wavenumber)) ||
      anyDuplicated(target_wavenumber)) {
    stop("A finite unique target wavenumber axis is required", call. = FALSE)
  }
  seen <- new.env(hash = TRUE, parent = emptyenv())
  process <- function(wavenumber, spectra, source_ids) {
    spectra <- as.matrix(spectra)
    if (nrow(spectra) != length(wavenumber) ||
        ncol(spectra) != length(source_ids)) {
      stop("Chunk spectra, axis, and source IDs are not aligned", call. = FALSE)
    }
    conformed <- vapply(seq_len(ncol(spectra)), function(i) {
      stats::approx(
        x = wavenumber, y = spectra[, i], xout = target_wavenumber,
        rule = 1, ties = "ordered"
      )$y
    }, numeric(length(target_wavenumber)))
    hashes <- vapply(seq_len(ncol(conformed)), function(i) {
      digest::digest(
        list(as.integer(target_wavenumber),
             as.integer(conformed[, i] * scale)), algo = algo
      )
    }, character(1L))
    duplicate <- vapply(hashes, exists, logical(1L), envir = seen,
                        inherits = FALSE) | duplicated(hashes)
    for (hash in hashes[!duplicate]) assign(hash, TRUE, envir = seen)
    list(
      wavenumber = target_wavenumber,
      spectra = conformed[, !duplicate, drop = FALSE],
      ledger = data.table::data.table(
        source_id = as.character(source_ids), content_hash = hashes,
        action = ifelse(duplicate, "removed_duplicate", "retained")
      )
    )
  }
  list(process = process, count = function() length(ls(seen, all.names = TRUE)))
}
