#' @rdname build_lib
#' @title Build spectral libraries
#'
#' @description
#' Helpers for creating reference libraries from source files and OpenSpecy
#' objects with metadata lookup tables. \code{build_lib()} provides the
#' standard end-to-end workflow while the supporting functions remain available
#' for advanced composition.
#'
#' @details
#' \code{build_lib()} combines sources over their full wavenumber range,
#' optionally adds ordinary and hierarchical metadata, removes requested
#' identifiers, optionally generates stable source-stage duplicate IDs, and
#' applies named processing recipes. Source-stage IDs follow the reference
#' library's legacy hash recipe: each source spectrum is trimmed with
#' \code{\link{manage_na}(type = "remove")}, conformed at resolution 8,
#' smoothed, and hashed from the resulting wavenumber/intensity vectors before
#' later merging and range restriction. The older 100--4000 cm-1 hash is kept in
#' \code{sample_name_old} when \code{id_col = "sample_name"} so
#' \code{exclude_ids} can remove both current and legacy curated bad IDs.
#' Metadata column names are first converted to lowercase
#' underscore names and known aliases are coalesced using
#' \code{metadata_name_lookup}; see \code{\link{lib_clean_metadata}()} for
#' automatic and regular-expression matching. Metadata values can optionally be
#' normalized to lowercase trimmed character values before lookup joins. By
#' default, each source is also
#' converted to absorbance before merging when its intensity units are known.
#' A nonempty \code{intensity_unit} object attribute takes precedence over the
#' per-spectrum \code{intensity_units} metadata column. Each recipe is either a
#' named list of arguments passed to \code{\link{process_spec}()} or a function
#' accepting one \code{OpenSpecy} object. An empty recipe returns an unprocessed
#' copy. Signal-to-noise is added by default, and optional
#' \code{\link{assess_spec}()} results are summarized into one metadata row per
#' spectrum.
#' Progress messages report named stages and elapsed time by default so
#' long-running builds remain observable.
#'
#' \code{make_lib_lookup_template()} creates a deduplicated table of metadata
#' values from an \code{OpenSpecy} or \code{Specs} object. Users can fill the
#' added columns in R or write the template to CSV and curate it elsewhere.
#'
#' \code{join_lib_metadata()} left-joins lookup columns onto object metadata and
#' reports unmatched metadata keys, duplicate lookup keys, and missing joined
#' values. Joins are exact; clean or harmonize values before calling this helper.
#'
#' \code{join_material_hierarchy()} joins user-defined hierarchical material
#' metadata. The supplied \code{levels} are tried from most-specific to
#' most-general so a material label can match any level in the hierarchy.
#'
#' \code{dedupe_spec()} hashes the current spectra and wavenumber axis to create
#' stable IDs and remove duplicated spectra. Process or conform spectra before
#' this step when that should affect duplicate detection.
#'
#' \code{reduce_lib()} uses PAM medoids to keep representative spectra within
#' each metadata group. It uses OpenSpecy's optimized correlation routine on
#' relative, mean-filled spectra.
#'
#' \code{build_model_lib()} trains the multinomial \code{glmnet} model structure
#' used by OpenSpecy model libraries. Filter, smooth, or otherwise preprocess
#' spectra before calling this helper.
#'
#' \code{assess_lib()} returns a compact summary of object validity, library
#' size, class balance, and optionally nearest-neighbor class consistency.
#'
#' @param x an \code{OpenSpecy} or \code{Specs} object for metadata helpers.
#' For \code{build_lib()}, one \code{OpenSpecy}, a nonempty list containing only
#' \code{OpenSpecy} objects, or a nonempty character vector of file paths.
#' Each RDS path may store either one \code{OpenSpecy} or a list of them; other
#' paths are read with \code{\link{read_any}()}.
#' Large same-axis source lists are prepared in bulk to avoid repeated legacy
#' object coercion.
#' @param lookup a data.frame, data.table, or csv file path used as a metadata
#' lookup table.
#' @param by named character vector mapping metadata columns to lookup columns,
#' or an unnamed character vector when the names are the same in both tables.
#' @param columns metadata columns to deduplicate into a template.
#' @param add blank columns to add to a template.
#' @param path optional csv path. If \code{NULL}, template helpers return a
#' data.table.
#' @param hierarchy a data.frame, data.table, or csv file path with hierarchical
#' material metadata.
#' @param key_col metadata column containing material labels to match.
#' @param levels hierarchy columns ordered from most-specific to most-general.
#' @param output_names names to use for hierarchy columns added to metadata.
#' @param require_complete logical; if \code{TRUE}, incomplete joins fail.
#' @param return whether to return an updated \code{OpenSpecy} object, joined
#' table, report list, or selected ids depending on the helper.
#' @param suffixes suffixes used when joined metadata and lookup tables share
#' non-key column names.
#' @param id_col metadata column used as the spectrum identifier.
#' @param exclude_ids identifiers to remove before returning a library.
#' @param duplicate how duplicated generated identifiers should be handled.
#' @param scale numeric multiplier used before hashing intensity values.
#' @param algo hash algorithm passed to \code{\link[digest]{digest}()}.
#' @param recipes named list of \code{\link{process_spec}()} argument lists or
#' functions. Names become names of the returned libraries.
#' @param dedupe logical; whether to generate stable IDs and remove duplicated
#' spectra in \code{build_lib()}.
#' @param range,res wavenumber range and resolution passed to \code{c_spec()}
#' when \code{build_lib()} combines multiple sources.
#' @param restrict_range_args optional named list of arguments passed to
#' \code{\link{restrict_range}()} after unit conversion and source merging.
#' Supplying the list triggers restriction; \code{make_rel = FALSE} is used
#' unless explicitly overridden.
#' @param metadata_lookups a lookup table, csv path, or list of lookup tables and
#' paths. If non-\code{NULL}, each is joined with
#' \code{join_lib_metadata()}. Automatic ordinary lookups use the single shared
#' column that has overlapping values and unique lookup keys. Lookups with no
#' usable shared key are skipped with a message; lookups with multiple usable
#' shared keys are considered ambiguous and stop. Lookup values that share
#' non-key metadata column names are coalesced back into those columns, with
#' non-missing lookup values taking precedence.
#' @param material_hierarchy hierarchy table or csv path used when
#' non-\code{NULL}. It is joined with \code{join_material_hierarchy()} using the
#' default \code{"material"} metadata key.
#' @param metadata_name_lookup a data.frame or data.table with
#' \code{canonical_name}, \code{source_name}, and optional \code{regex} columns.
#' The default is returned by \code{\link{lib_metadata_name_lookup}()}; use
#' \code{NULL} to clean names without coalescing aliases.
#' @param clean_metadata_values logical; whether \code{build_lib()} should also
#' lowercase, trim, ASCII-normalize, and normalize blank/unknown character
#' metadata values in source metadata and lookup tables before joining.
#' @param convert_intensity logical; whether to infer reflectance,
#' transmittance, or absorbance units from each source and convert known
#' non-absorbance spectra with \code{\link{adj_intens}()} before merging.
#' Object attribute \code{intensity_unit} is authoritative when supplied;
#' otherwise metadata column \code{intensity_units} is evaluated per spectrum.
#' @param signal_noise logical; whether to append the default
#' \code{\link{sig_noise}()} result as metadata column \code{sn}.
#' @param assess logical; whether to run \code{\link{assess_spec}()} on each
#' output library and append assessment summaries to its metadata.
#' @param progress logical; whether \code{build_lib()} reports named processing
#' stages and elapsed time.
#' @param group_cols metadata columns defining groups for reduction.
#' @param k maximum representatives to keep for groups larger than
#' \code{min_n}.
#' @param min_n groups with \code{min_n} or fewer spectra are kept whole.
#' @param class_col,type_col metadata columns used for model labels.
#' @param nearest logical; if \code{TRUE}, \code{assess_lib()} compares each
#' spectrum with its highest-correlation neighbor and reports the fraction where
#' that neighbor has the same \code{class_col} value.
#' @param alpha alpha value passed to \code{\link[glmnet]{glmnet}()}.
#' @param seed random seed used before model training.
#' @param grouped logical; whether multinomial coefficients use grouped
#' penalties.
#' @param weights logical; whether to use inverse class-frequency weights.
#' @param make_relative logical; whether to normalize model inputs with
#' \code{\link{make_rel}()}.
#' @param complete_cases logical; whether to remove spectra with any missing
#' training values.
#' @param \ldots further arguments passed to the underlying operation.
#'
#' @return
#' \code{build_lib()} returns a named list of \code{OpenSpecy} libraries.
#' \code{join_lib_metadata()}, \code{join_material_hierarchy()},
#' \code{dedupe_spec()}, and \code{reduce_lib()} return an updated spectral
#' object unless \code{return} requests a table, report, or ids.
#' \code{make_lib_lookup_template()} returns a data.table unless \code{path} is
#' supplied, in which case it writes the csv and invisibly returns the table.
#' \code{build_model_lib()} returns a list suitable for AI classification with
#' \code{\link{match_spec}()}. \code{assess_lib()} returns a data.table summary.
#'
#' @examples
#' wavenumber <- seq(100, 6100, by = 100)
#' base_a <- dnorm(seq(-3, 3, length.out = length(wavenumber)))
#' base_b <- rev(cumsum(seq_along(wavenumber)))
#' spectra <- cbind(base_a, base_a + 0.1, base_a + 0.2,
#'                  base_b, base_b + 0.1, base_b + 0.2)
#' colnames(spectra) <- paste0("s", seq_len(ncol(spectra)))
#' mini <- as_OpenSpecy(
#'   wavenumber,
#'   spectra = spectra,
#'   metadata = data.table::data.table(
#'     sample_name = colnames(spectra),
#'     source = rep(c("A", "B"), each = 3),
#'     label = c("nylon 6", "polyamides", "nylon 6",
#'               "pet", "polyesters", "pet"),
#'     material_class = rep(c("polyamides", "polyesters"), each = 3),
#'     spectrum_type = rep("ftir", 6),
#'     intensity_units = rep("absorbance", 6)
#'   ),
#'   attributes = list(intensity_unit = "absorbance")
#' )
#'
#' name_lookup <- lib_metadata_name_lookup()
#' name_lookup[name_lookup$canonical_name == "material_color", ]
#'
#' make_lib_lookup_template(mini, columns = "source", add = "library_type")
#'
#' source_lookup <- data.frame(
#'   source = c("A", "B"),
#'   library_type = c("lab", "field"),
#'   material = c("nylon 6", "pet")
#' )
#' joined <- join_lib_metadata(mini, source_lookup, by = "source",
#'                             require_complete = TRUE)
#'
#' hierarchy <- data.frame(
#'   material = c("nylon 6", "pet"),
#'   material_class = c("polyamides", "polyesters"),
#'   material_type = c("plastic", "plastic")
#' )
#' joined <- join_material_hierarchy(joined, hierarchy, key_col = "label",
#'                                   require_complete = TRUE)
#'
#' deduped <- dedupe_spec(joined)
#' reduced <- reduce_lib(deduped, group_cols = "material_class",
#'                       k = 1, min_n = 1)
#' libs <- build_lib(
#'   mini,
#'   recipes = list(
#'     raw = list(),
#'     derivative = list(
#'       conform_spec = FALSE,
#'       smooth_intens = TRUE,
#'       smooth_intens_args = list(window = 15, derivative = 1),
#'       make_rel = TRUE
#'     )
#'   ),
#'   metadata_lookups = source_lookup,
#'   material_hierarchy = hierarchy,
#'   restrict_range_args = list(min = 100, max = 6000),
#'   assess = TRUE,
#'   dedupe = FALSE
#' )
#'
#' model <- suppressWarnings(build_model_lib(
#'   joined, class_col = "material_class", type_col = NULL, min_n = 2,
#'   nlambda = 3
#' ))
#' assess_lib(libs$raw, class_col = "material_class", nearest = FALSE)
#'
#' @author
#' Win Cowger
#'
#' @importFrom data.table as.data.table data.table fread fwrite rbindlist setorder
#' @export
build_lib <- function(x, recipes = .default_lib_recipes(), range = "full",
                      res = 6, id_col = "sample_name", exclude_ids = NULL,
                      dedupe = TRUE, metadata_lookups = NULL,
                      material_hierarchy = NULL,
                      metadata_name_lookup = lib_metadata_name_lookup(),
                      clean_metadata_values = FALSE,
                      convert_intensity = TRUE, restrict_range_args = NULL,
                      signal_noise = TRUE, assess = FALSE, progress = TRUE,
                      ...) {
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(clean_metadata_values) ||
      length(clean_metadata_values) != 1L ||
      is.na(clean_metadata_values)) {
    stop("'clean_metadata_values' must be TRUE or FALSE", call. = FALSE)
  }
  started <- proc.time()[["elapsed"]]
  dot_args <- list(...)
  hash_scale <- if (is.null(dot_args$scale)) 100 else dot_args$scale
  hash_algo <- if (is.null(dot_args$algo)) "md5" else dot_args$algo
  dedupe_duplicate <- if (is.null(dot_args$duplicate)) {
    "first"
  } else {
    match.arg(dot_args$duplicate, c("first", "remove_all", "none"))
  }
  report <- function(stage) {
    if (isTRUE(progress)) {
      elapsed <- proc.time()[["elapsed"]] - started
      message(sprintf("build_lib [%.1fs]: %s", elapsed, stage))
    }
  }

  validate_sources <- function(source, label) {
    if (is_OpenSpecy(source)) {
      return(list(source))
    }
    if (is.list(source) && length(source) > 0L &&
        all(vapply(source, is_OpenSpecy, logical(1)))) {
      return(source)
    }
    stop(label, " must contain one OpenSpecy object or a nonempty list of ",
         "OpenSpecy objects", call. = FALSE)
  }

  report("starting")
  if (is_OpenSpecy(x)) {
    sources <- list(x)
    report("using one in-memory OpenSpecy source")
  } else if (is.character(x)) {
    if (length(x) == 0L || anyNA(x) || any(!nzchar(x))) {
      stop("'x' must contain one or more nonempty file paths", call. = FALSE)
    }
    sources <- unlist(lapply(seq_along(x), function(i) {
      report(sprintf(
        "reading path %d/%d (%s)",
        i, length(x), basename(x[[i]])
      ))
      if (grepl("\\.rds$", x[[i]], ignore.case = TRUE)) {
        source <- readRDS(x[[i]])
      } else {
        source <- read_any(x[[i]])
      }
      validate_sources(source, paste0("File path ", i))
    }), recursive = FALSE)
  } else {
    if (!is.list(x) || length(x) == 0L ||
        !all(vapply(x, is_OpenSpecy, logical(1)))) {
      stop("'x' must be one OpenSpecy object, file path(s), or a nonempty ",
           "list of OpenSpecy objects", call. = FALSE)
    }
    sources <- x
    report(sprintf("using %d in-memory OpenSpecy source(s)", length(sources)))
  }

  report(sprintf("preparing %d source object(s)", length(sources)))
  lib <- .lib_prepare_sources(
    sources,
    range = range,
    res = res,
    metadata_name_lookup = metadata_name_lookup,
    clean_metadata_values = clean_metadata_values,
    convert_intensity = convert_intensity,
    id_col = if (isTRUE(dedupe)) id_col else NULL,
    hash_scale = hash_scale,
    hash_algo = hash_algo,
    report = report
  )

  if (!is.null(restrict_range_args)) {
    report("restricting the wavenumber range")
    if (!is.list(restrict_range_args) ||
        is.null(names(restrict_range_args)) ||
        any(names(restrict_range_args) == "")) {
      stop("'restrict_range_args' must be a named list", call. = FALSE)
    }
    args <- utils::modifyList(
      list(make_rel = FALSE),
      restrict_range_args
    )
    lib <- do.call("restrict_range", c(list(lib), args))
  }

  if (!is.null(metadata_lookups)) {
    lookups <- if (is.character(metadata_lookups) &&
                   length(metadata_lookups) > 1L) {
      as.list(metadata_lookups)
    } else if (is.list(metadata_lookups) &&
               !inherits(metadata_lookups, c("data.frame", "data.table"))) {
      metadata_lookups
    } else {
      list(metadata_lookups)
    }

    for (i in seq_along(lookups)) {
      report(sprintf("joining metadata lookup %d/%d", i, length(lookups)))
      lookup <- lookups[[i]]
      lookup_table <- lib_clean_metadata(.lib_read_lookup(lookup),
                                         metadata_name_lookup,
                                         clean_values = clean_metadata_values)
      auto_key <- .lib_auto_lookup_key(lib$metadata, lookup_table)
      if (length(auto_key$shared) == 0L) {
        report(sprintf(
          "skipping metadata lookup %d/%d; no shared metadata column",
          i, length(lookups)
        ))
        next
      }
      if (length(auto_key$candidates) == 0L) {
        report(sprintf(
          "skipping metadata lookup %d/%d; no usable shared key values in: %s",
          i, length(lookups), paste(auto_key$shared, collapse = ", ")
        ))
        next
      }
      if (length(auto_key$candidates) > 1L) {
        stop("Each automatic metadata lookup must have exactly one usable ",
             "shared key. Candidate columns were: ",
             paste(auto_key$candidates, collapse = ", "),
             ". Use join_lib_metadata() directly for advanced joins",
             call. = FALSE)
      }
      coalesce_cols <- intersect(
        names(lib$metadata),
        setdiff(names(lookup_table), auto_key$candidates)
      )
      lib <- join_lib_metadata(lib, lookup_table, by = auto_key$candidates)
      lib$metadata <- .lib_coalesce_joined_metadata(
        lib$metadata,
        coalesce_cols
      )
    }
  }

  if (!is.null(material_hierarchy)) {
    report("joining the material hierarchy")
    hierarchy <- lib_clean_metadata(.lib_read_lookup(material_hierarchy),
                                    metadata_name_lookup,
                                    clean_values = clean_metadata_values)
    lib <- join_material_hierarchy(lib, hierarchy)
  }

  if (!is.null(exclude_ids)) {
    report("removing excluded identifiers")
    lib <- .lib_filter_excluded(lib, exclude_ids, id_col = id_col)
  }

  if (dedupe) {
    report("generating identifiers and removing duplicate spectra")
    existing <- .lib_dedupe_existing_ids(
      lib,
      id_col = id_col,
      duplicate = dedupe_duplicate
    )
    lib <- if (is.null(existing)) {
      dedupe_spec(lib, id_col = id_col, ...)
    } else {
      existing
    }
    if (!is.null(exclude_ids)) {
      lib <- .lib_filter_excluded(lib, exclude_ids, id_col = id_col)
    }
  }

  apply_recipe <- function(recipe, recipe_name, recipe_index) {
    report(sprintf(
      "processing recipe %d/%d (%s)",
      recipe_index, length(recipes), recipe_name
    ))
    out <- if (is.function(recipe)) {
      recipe(lib)
    } else if (length(recipe) == 0L) {
      lib
    } else {
      do.call(process_spec, c(list(lib), recipe))
    }
    if (!is_OpenSpecy(out)) {
      stop("Each recipe must return an OpenSpecy object", call. = FALSE)
    }

    if (isTRUE(signal_noise)) {
      report(sprintf("calculating signal-to-noise (%s)", recipe_name))
      out$metadata$sn <- sig_noise(out, step = 10)
    }

    if (isTRUE(assess)) {
      report(sprintf("assessing spectra (%s)", recipe_name))
      assessment <- assess_spec(out)
      out$metadata[, `:=`(
        assessment_flag = FALSE,
        assessment_issue_count = 0L,
        assessment_checks = NA_character_,
        assessment_issues = NA_character_,
        assessment_potential_fixes = NA_character_
      )]

      if (nrow(assessment) > 0L) {
        summary <- assessment[, .(
          assessment_flag = TRUE,
          assessment_issue_count = .N,
          assessment_checks = paste(unique(get("check")), collapse = "; "),
          assessment_issues = paste(unique(get("issue")), collapse = "; "),
          assessment_potential_fixes = paste(unique(get("potential_fix")),
                                             collapse = "; ")
        ), by = "spectrum_id"]
        idx <- match(colnames(out$spectra), summary$spectrum_id)
        found <- !is.na(idx)
        assessment_cols <- setdiff(names(summary), "spectrum_id")
        assessment_values <- summary[idx[found], assessment_cols, with = FALSE]
        out$metadata[found, (assessment_cols) := assessment_values]
      }
    }

    out
  }

  if (is.null(names(recipes)) || any(names(recipes) == "") ||
      anyDuplicated(names(recipes))) {
    stop("'recipes' must be a uniquely named list", call. = FALSE)
  }
  out <- lapply(seq_along(recipes), function(i) {
    apply_recipe(recipes[[i]], names(recipes)[[i]], i)
  })
  names(out) <- names(recipes)
  report("complete")
  out
}

.lib_prepare_sources <- function(sources, range, res, metadata_name_lookup,
                                 clean_metadata_values, convert_intensity,
                                 id_col, hash_scale, hash_algo, report) {
  records <- lapply(seq_along(sources), function(i) {
    .lib_source_record(
      sources[[i]],
      i,
      id_col = id_col,
      hash_scale = hash_scale,
      hash_algo = hash_algo
    )
  })
  metadata <- .lib_combined_metadata(
    records,
    metadata_name_lookup,
    clean_metadata_values = clean_metadata_values
  )

  if (isTRUE(convert_intensity)) {
    converted <- .lib_convert_records_intensity(
      records,
      metadata,
      source_label = "source list"
    )
    records <- converted$records
    metadata <- converted$metadata
  }

  shared_axis <- .lib_same_wavenumber(records)
  if (length(records) > 1L) {
    if (shared_axis) {
      report(sprintf(
        "combining %d same-axis source object(s)",
        length(records)
      ))
    } else {
      report(sprintf(
        "merging %d source object(s) with c_spec()",
        length(records)
      ))
    }
  }

  if (length(records) == 1L) {
    return(.lib_combine_same_axis_records(
      records,
      metadata,
      range = NULL,
      res = res
    ))
  }

  if (shared_axis) {
    return(.lib_combine_same_axis_records(
      records,
      metadata,
      range = range,
      res = res
    ))
  }

  .lib_combine_variable_axis_records(
    records,
    metadata,
    range = range,
    res = res
  )
}

.lib_source_record <- function(source, index, id_col = NULL,
                               hash_scale = 100, hash_algo = "md5") {
  if (!is_OpenSpecy(source)) {
    stop("Source ", index, " must be an OpenSpecy object", call. = FALSE)
  }

  spectra <- .as_spectra_matrix(source$spectra, message_conversion = FALSE)
  wavenumber <- source$wavenumber
  if (!is.numeric(wavenumber) || !is.vector(wavenumber)) {
    stop("Source ", index, " wavenumber must be a numeric vector",
         call. = FALSE)
  }
  if (length(wavenumber) != nrow(spectra)) {
    stop("Source ", index, " wavenumber length must match spectra rows",
         call. = FALSE)
  }
  ord <- order(wavenumber)
  if (!identical(ord, seq_along(wavenumber))) {
    wavenumber <- wavenumber[ord]
    spectra <- spectra[ord, , drop = FALSE]
  }

  metadata <- data.table::as.data.table(data.table::copy(source$metadata))
  if (nrow(metadata) != ncol(spectra)) {
    stop("Source ", index, " metadata rows must match spectra columns",
         call. = FALSE)
  }

  if (!is.null(id_col)) {
    source_ids <- .lib_source_hash_ids(
      wavenumber,
      spectra,
      metadata,
      scale = hash_scale,
      algo = hash_algo
    )
    metadata[[id_col]] <- source_ids$current
    metadata[[paste0(id_col, "_old")]] <- source_ids$old
    colnames(spectra) <- source_ids$current
  }

  object_unit <- attr(source, "intensity_unit", exact = TRUE)
  object_unit <- as.character(object_unit)
  object_unit <- object_unit[!is.na(object_unit) & trimws(object_unit) != ""]
  if (length(object_unit) > 1L) {
    stop("Source ", index,
         " attribute 'intensity_unit' must contain at most one value",
         call. = FALSE)
  }

  attribute_names <- c("intensity_unit", "derivative_order", "baseline",
                       "spectra_type")
  attrs <- lapply(attribute_names, attr, x = source)
  names(attrs) <- attribute_names

  list(
    wavenumber = wavenumber,
    spectra = spectra,
    metadata = metadata,
    n = ncol(spectra),
    intensity_attr = if (length(object_unit) == 1L) object_unit else NA_character_,
    attrs = attrs
  )
}

.lib_source_hash_ids <- function(wavenumber, spectra, metadata, scale = 100,
                                 algo = "md5") {
  ignored_info <- .spectra_ignore_info(spectra, lead_tail_only = TRUE,
                                       ig = c(NA))
  if (any(!ignored_info$has_valid)) {
    stop("All intensity values are NA, cannot remove or ignore with manage na.",
         call. = FALSE)
  }
  keep <- rowSums(ignored_info$ignored) == 0L
  wavenumber <- wavenumber[keep]
  spectra <- spectra[keep, , drop = FALSE]

  current <- .lib_hash_processed_matrix(
    wavenumber,
    spectra,
    range = NULL,
    res = 8,
    scale = scale,
    algo = algo
  )
  old <- .lib_hash_processed_matrix(
    wavenumber,
    spectra,
    range = c(100, 4000),
    res = 8,
    scale = scale,
    algo = algo,
    min_rows = 3L,
    short_value = "new format"
  )

  list(current = current, old = old)
}

.lib_hash_processed_matrix <- function(wavenumber, spectra, range = NULL,
                                       res = 8, scale = 100, algo = "md5",
                                       min_rows = 1L,
                                       short_value = NA_character_) {
  conformed <- .lib_conform_hash_matrix(wavenumber, spectra, range = range,
                                        res = res)
  if (nrow(conformed$spectra) < min_rows) {
    return(rep(short_value, ncol(spectra)))
  }
  smoothed <- .sgfilt_matrix(conformed$spectra, p = 3, n = 11, m = 1)
  smoothed <- make_rel(abs(smoothed))
  .lib_hash_spectra_columns(conformed$wavenumber, smoothed, scale, algo)
}

.lib_conform_hash_matrix <- function(wavenumber, spectra, range = NULL,
                                     res = 8) {
  if (is.null(range)) range <- wavenumber
  range2 <- c(max(min(range), min(wavenumber)),
              min(max(range), max(wavenumber)))
  wn <- conform_res(range2, res = res)
  spectra <- .conform_intens_matrix(
    x = wavenumber,
    y = spectra,
    xout = wn
  )
  list(wavenumber = wn, spectra = spectra)
}

.lib_hash_spectra_columns <- function(wavenumber, spectra, scale = 100,
                                      algo = "md5") {
  vapply(seq_len(ncol(spectra)), function(i) {
    digest::digest(
      list(as.integer(wavenumber), as.integer(spectra[, i] * scale)),
      algo = algo
    )
  }, FUN.VALUE = character(1))
}

.lib_combined_metadata <- function(records, metadata_name_lookup,
                                   clean_metadata_values = FALSE) {
  metadata <- data.table::rbindlist(
    lapply(records, `[[`, "metadata"),
    fill = TRUE
  )
  metadata <- lib_clean_metadata(
    metadata,
    metadata_name_lookup,
    clean_values = clean_metadata_values
  )
  metadata <- metadata[, setdiff(names(metadata), c("x", "y")), with = FALSE]
  metadata
}

.lib_same_wavenumber <- function(records) {
  if (length(records) <= 1L) return(TRUE)
  first <- records[[1L]]$wavenumber
  all(vapply(records[-1L], function(record) {
    identical(record$wavenumber, first)
  }, logical(1)))
}

.lib_common_record_attributes <- function(records) {
  attribute_names <- c("intensity_unit", "derivative_order", "baseline",
                       "spectra_type")
  out <- lapply(attribute_names, function(nm) {
    values <- lapply(records, function(record) record$attrs[[nm]])
    if (all(vapply(values[-1L], identical, logical(1), values[[1L]]))) {
      values[[1L]]
    } else {
      NULL
    }
  })
  names(out) <- attribute_names
  out
}

.lib_make_open_specy <- function(wavenumber, spectra, metadata, attributes) {
  metadata <- data.table::as.data.table(data.table::copy(metadata))
  metadata$col_id <- colnames(spectra)
  structure(
    list(wavenumber = wavenumber, spectra = spectra, metadata = metadata),
    class = c("OpenSpecy", "list"),
    intensity_unit = attributes$intensity_unit,
    derivative_order = attributes$derivative_order,
    baseline = attributes$baseline,
    spectra_type = attributes$spectra_type
  )
}

.lib_combine_same_axis_records <- function(records, metadata, range, res) {
  spectra <- do.call(cbind, lapply(records, `[[`, "spectra"))
  colnames(spectra) <- make.unique(
    unlist(lapply(records, function(record) colnames(record$spectra)),
           use.names = FALSE),
    sep = "."
  )
  attrs <- .lib_common_record_attributes(records)
  lib <- .lib_make_open_specy(
    records[[1L]]$wavenumber,
    spectra,
    metadata,
    attrs
  )

  if (!is.null(range)) {
    conform_range <- if (is.numeric(range)) {
      range
    } else if (length(range) == 1L && range %in% c("common", "full")) {
      base::range(lib$wavenumber)
    } else {
      stop("If range is specified it should be numeric, 'full', or 'common'",
           call. = FALSE)
    }
    lib <- conform_spec(
      lib,
      range = conform_range,
      res = res,
      allow_na = identical(range, "full") || is.numeric(range)
    )
  }

  as_OpenSpecy(lib)
}

.lib_combine_variable_axis_records <- function(records, metadata, range, res) {
  idx <- split(seq_len(nrow(metadata)), rep(seq_along(records),
                                           vapply(records, `[[`, integer(1),
                                                  "n")))
  sources <- lapply(seq_along(records), function(i) {
    .lib_make_open_specy(
      records[[i]]$wavenumber,
      records[[i]]$spectra,
      metadata[idx[[i]], ],
      records[[i]]$attrs
    )
  })
  if (length(sources) == 1L) {
    sources[[1L]]
  } else {
    c_spec(sources, range = range, res = res)
  }
}

.lib_convert_records_intensity <- function(records, metadata,
                                           source_label = "source") {
  attr_units <- unlist(lapply(records, function(record) {
    rep(record$intensity_attr, record$n)
  }), use.names = FALSE)
  has_attr_unit <- !is.na(attr_units)
  if (any(has_attr_unit)) {
    if (!"intensity_units" %in% names(metadata)) {
      metadata$intensity_units <- NA_character_
    }
    metadata$intensity_units[has_attr_unit] <- attr_units[has_attr_unit]
  }

  declared <- if ("intensity_units" %in% names(metadata)) {
    as.character(metadata$intensity_units)
  } else {
    rep(NA_character_, nrow(metadata))
  }
  canonical <- .lib_canonical_intensity_unit(declared)
  starts <- cumsum(c(1L, head(vapply(records, `[[`, integer(1), "n"), -1L)))

  for (i in seq_along(records)) {
    cols <- starts[[i]] + seq_len(records[[i]]$n) - 1L
    records[[i]]$spectra <- .lib_convert_spectra_matrix(
      records[[i]]$spectra,
      canonical[cols]
    )
    resolved_i <- !is.na(canonical[cols])
    if (all(resolved_i)) {
      records[[i]]$attrs$intensity_unit <- "absorbance"
    }
  }

  resolved <- !is.na(canonical)
  if (any(resolved)) {
    if (!"intensity_units" %in% names(metadata)) {
      metadata$intensity_units <- NA_character_
    }
    metadata$intensity_units[resolved] <- "absorbance"
  }

  .lib_warn_unknown_intensity(declared, resolved, source_label)
  list(records = records, metadata = metadata)
}

.lib_convert_spectra_matrix <- function(spectra, canonical) {
  for (type in c("reflectance", "transmittance")) {
    idx <- which(canonical == type)
    if (length(idx) > 0L) {
      spectra[, idx] <- switch(
        type,
        reflectance = (1 - spectra[, idx, drop = FALSE] / 100)^2 /
          (2 * spectra[, idx, drop = FALSE] / 100),
        transmittance = log(1 / .matrix_adj_neg(
          spectra[, idx, drop = FALSE]
        ))
      )
    }
  }
  spectra
}

.lib_warn_unknown_intensity <- function(declared, resolved, source_label) {
  if (!any(!resolved)) return(invisible(NULL))

  unresolved <- trimws(as.character(declared[!resolved]))
  unresolved[is.na(unresolved) | unresolved == ""] <- "<missing>"
  counts <- sort(table(unresolved), decreasing = TRUE)
  details <- paste0(names(counts), " (", as.integer(counts), ")")
  warning(
    "Automatic intensity conversion skipped ", sum(!resolved),
    " spectrum/s in ", source_label, " with unknown units: ",
    paste(details, collapse = ", "),
    ". Set attr(x, 'intensity_unit') or metadata$intensity_units to ",
    "'absorbance', 'reflectance', or 'transmittance'; use ",
    "convert_intensity = FALSE to preserve units without conversion.",
    call. = FALSE
  )
  invisible(NULL)
}

.lib_canonical_intensity_unit <- function(x) {
  value <- trimws(tolower(iconv(as.character(x), to = "ASCII", sub = "")))
  value[is.na(value) | value == ""] <- NA_character_
  out <- rep(NA_character_, length(value))
  out[!is.na(value) & grepl("absorb", value)] <- "absorbance"
  out[!is.na(value) & grepl("reflec", value)] <- "reflectance"
  out[!is.na(value) & grepl("transm", value)] <- "transmittance"
  out
}

.lib_convert_intensity <- function(x, source_label = "source") {
  x <- as_OpenSpecy(x)
  object_unit <- attr(x, "intensity_unit", exact = TRUE)
  object_unit <- as.character(object_unit)
  object_unit <- object_unit[!is.na(object_unit) & trimws(object_unit) != ""]
  if (length(object_unit) > 1L) {
    stop("Object attribute 'intensity_unit' must contain at most one value",
         call. = FALSE)
  }

  from_attribute <- length(object_unit) == 1L
  declared <- if (from_attribute) {
    rep(object_unit, ncol(x$spectra))
  } else if ("intensity_units" %in% names(x$metadata)) {
    as.character(x$metadata$intensity_units)
  } else {
    rep(NA_character_, ncol(x$spectra))
  }
  canonical <- .lib_canonical_intensity_unit(declared)

  for (type in c("reflectance", "transmittance")) {
    idx <- which(canonical == type)
    if (length(idx) > 0L) {
      converted <- adj_intens(
        filter_spec(x, idx),
        type = type,
        make_rel = FALSE
      )
      x$spectra[, idx] <- converted$spectra
    }
  }

  resolved <- !is.na(canonical)
  if (any(resolved)) {
    if (!"intensity_units" %in% names(x$metadata)) {
      x$metadata$intensity_units <- NA_character_
    }
    x$metadata$intensity_units[resolved] <- "absorbance"
  }

  if (all(resolved)) {
    attr(x, "intensity_unit") <- "absorbance"
  } else if (!from_attribute) {
    attr(x, "intensity_unit") <- NULL
  }

  if (any(!resolved)) {
    unresolved <- trimws(as.character(declared[!resolved]))
    unresolved[is.na(unresolved) | unresolved == ""] <- "<missing>"
    counts <- sort(table(unresolved), decreasing = TRUE)
    details <- paste0(names(counts), " (", as.integer(counts), ")")
    warning(
      "Automatic intensity conversion skipped ", sum(!resolved),
      " spectrum/s in ", source_label, " with unknown units: ",
      paste(details, collapse = ", "),
      ". Set attr(x, 'intensity_unit') or metadata$intensity_units to ",
      "'absorbance', 'reflectance', or 'transmittance'; use ",
      "convert_intensity = FALSE to preserve units without conversion.",
      call. = FALSE
    )
  }
  x
}

#' Create and apply metadata-name lookup rules
#'
#' @description
#' \code{lib_metadata_name_lookup()} returns the default editable rules used to
#' merge synonymous metadata columns. \code{lib_clean_name()} converts names to
#' lowercase underscore form. \code{lib_clean_metadata()} cleans table names and
#' coalesces columns that map to the same canonical name.
#'
#' @details
#' Exact rules determine a column's target before automatic matching that can
#' ignore underscores and a single terminal plural \code{s}. When values are
#' coalesced, canonical and mechanically equivalent canonical names come before
#' semantic aliases. Regular-expression rules are applied last to names that
#' remain unmatched. Regex patterns are evaluated against names after
#' \code{lib_clean_name()} has been applied.
#'
#' Matching options selected in \code{lib_metadata_name_lookup()} are stored
#' with the returned table and used by \code{lib_clean_metadata()}. User rules
#' supplied through \code{...} are merged with the defaults. Set
#' \code{defaults = FALSE} to construct a lookup from only user rules.
#'
#' @param ... named character vectors of exact aliases, where each argument name
#' is the canonical name, or data.frame/data.table rule tables with
#' \code{canonical_name}, \code{source_name}, and optional \code{regex} columns.
#' @param regex an optional named character vector or named list of regular
#' expressions. Names identify the canonical metadata names.
#' @param defaults logical; whether to include OpenSpecy's default semantic
#' aliases before merging user rules.
#' @param match_without_underscores logical; whether names that differ only by
#' underscores should match automatically.
#' @param match_singular_plural logical; whether names that differ only by one
#' terminal \code{s} should match automatically.
#' @param x a character vector of names for \code{lib_clean_name()}, or a
#' data.frame/data.table for \code{lib_clean_metadata()}.
#' @param name_lookup a table returned by \code{lib_metadata_name_lookup()} or a
#' compatible rule table. Use \code{NULL} to clean names without alias merging.
#' @param clean_values logical; whether \code{lib_clean_metadata()} should also
#' lowercase, trim, ASCII-normalize, and normalize blank/unknown character or
#' factor metadata values.
#'
#' @return \code{lib_metadata_name_lookup()} returns a data.table of rules.
#' \code{lib_clean_name()} returns a character vector.
#' \code{lib_clean_metadata()} returns a data.table with cleaned, coalesced
#' columns.
#'
#' @examples
#' lib_clean_name(c("User Name", "Laser (%)", "Method...3"))
#'
#' name_lookup <- lib_metadata_name_lookup(
#'   project_code = "campaign name",
#'   regex = list(instrument_mode = "^method_[0-9]+$")
#' )
#' metadata <- data.frame(
#'   UserName = c("A", NA),
#'   user_name = c(NA, "B"),
#'   Campaign.Name = c("one", "two"),
#'   Method.23 = c("ftir", "raman")
#' )
#' lib_clean_metadata(metadata, name_lookup)
#'
#' @export
lib_metadata_name_lookup <- function(..., regex = NULL, defaults = TRUE,
                                     match_without_underscores = TRUE,
                                     match_singular_plural = TRUE) {
  aliases <- if (isTRUE(defaults)) list(
    sample_name = character(),
    file_name = character(),
    library_type = character(),
    contact_info = character(),
    organization = character(),
    citation = character(),
    spectrum_identity = c("substance"),
    spectrum_type = character(),
    material_form = c("description",
                      "form_film_foam_pliable_hard", "form", "state",
                      "morphology"),
    material_producer = character(),
    material_purity = character(),
    material_quality = c("source_type"),
    material_color = c("color", "colour"),
    material_other = character(),
    cas_number = c("cas_registry_no"),
    instrument_used = c("spectrometer_datasystem"),
    instrument_accessories = c("instrumentaccesories",
                               "external_diffuse_reflectance_accessory"),
    instrument_mode = c("spectralcollectionmode", "method_3", "method_23"),
    intensity_units = c("y_unit"),
    spectral_resolution = c("resolution"),
    laser_light_used = c("laser_nm", "laser_frequency"),
    number_of_accumulations = c("number_of_sample_scans", "coadded_scans"),
    total_acquisition_time_s = c("collection_length", "acq_time_s"),
    data_processing_procedure = c("preprocessing", "data_processing",
                                  "data_processing_proceedure"),
    level_of_confidence_in_identification = character(),
    other_info = c("otherinformation", "otherinfo", "comment", "comments",
                   "notes"),
    baseline_correction = c("baseline"),
    smoother = c("smooth"),
    user_name = character(),
    sample_id = character(),
    longest_dimension = character(),
    width = character(),
    source = c("source_database", "origin", "nist_source"),
    date = c("longdate", "timestamp"),
    phase_correction = character(),
    apodization = c("apodization_function")
  ) else list()

  rules <- lapply(names(aliases), function(canonical) {
    data.table::data.table(
      canonical_name = canonical,
      source_name = c(canonical, aliases[[canonical]]),
      regex = NA_character_
    )
  })

  additions <- list(...)
  addition_names <- names(additions)
  if (is.null(addition_names)) addition_names <- rep("", length(additions))
  for (i in seq_along(additions)) {
    addition <- additions[[i]]
    if (inherits(addition, c("data.frame", "data.table"))) {
      addition <- data.table::as.data.table(data.table::copy(addition))
      .lib_require_cols(addition, "canonical_name", "metadata name rule")
      if (!"source_name" %in% names(addition)) {
        addition$source_name <- NA_character_
      }
      if (!"regex" %in% names(addition)) addition$regex <- NA_character_
      rules[[length(rules) + 1L]] <-
        addition[, c("canonical_name", "source_name", "regex"), with = FALSE]
    } else {
      canonical <- addition_names[i]
      if (is.na(canonical) || canonical == "") {
        stop("Exact alias additions in '...' must be named or supplied as ",
             "rule tables", call. = FALSE)
      }
      aliases_i <- unlist(addition, use.names = FALSE)
      if (!is.character(aliases_i)) {
        stop("Exact alias additions in '...' must be character vectors",
             call. = FALSE)
      }
      rules[[length(rules) + 1L]] <- data.table::data.table(
        canonical_name = canonical,
        source_name = c(canonical, aliases_i),
        regex = NA_character_
      )
    }
  }

  if (!is.null(regex)) {
    if (!is.list(regex)) regex <- as.list(regex)
    regex_names <- names(regex)
    if (is.null(regex_names) || any(is.na(regex_names) | regex_names == "")) {
      stop("'regex' must be a named character vector or named list",
           call. = FALSE)
    }
    regex_rules <- lapply(seq_along(regex), function(i) {
      patterns <- unlist(regex[[i]], use.names = FALSE)
      if (!is.character(patterns)) {
        stop("Each 'regex' entry must contain character patterns",
             call. = FALSE)
      }
      data.table::data.table(
        canonical_name = regex_names[i],
        source_name = NA_character_,
        regex = patterns
      )
    })
    rules <- c(rules, regex_rules)
  }

  lookup <- if (length(rules) == 0L) {
    data.table::data.table(
      canonical_name = character(),
      source_name = character(),
      regex = character()
    )
  } else {
    data.table::rbindlist(rules, use.names = TRUE, fill = TRUE)
  }
  lookup$canonical_name <- lib_clean_name(lookup$canonical_name)
  exact <- !is.na(lookup$source_name)
  lookup$source_name[exact] <- lib_clean_name(lookup$source_name[exact])
  empty_regex <- !is.na(lookup$regex) & lookup$regex == ""
  lookup$regex[empty_regex] <- NA_character_
  lookup <- unique(lookup)

  has_source <- !is.na(lookup$source_name)
  has_regex <- !is.na(lookup$regex)
  if (any(has_source == has_regex)) {
    stop("Each metadata name rule must contain exactly one of 'source_name' ",
         "or 'regex'", call. = FALSE)
  }
  attr(lookup, "match_without_underscores") <-
    isTRUE(match_without_underscores)
  attr(lookup, "match_singular_plural") <- isTRUE(match_singular_plural)
  lookup
}

#' @rdname build_lib
#' @export
make_lib_lookup_template <- function(x, columns, add = NULL, path = NULL) {
  if (!(is_OpenSpecy(x) || is_Specs(x))) {
    stop("'x' must be an OpenSpecy or Specs object", call. = FALSE)
  }
  metadata <- data.table::as.data.table(data.table::copy(x$metadata))
  .lib_require_cols(metadata, columns, "metadata")

  template <- data.table::as.data.table(metadata[, columns, with = FALSE])
  template <- unique(template)

  if (!is.null(add)) {
    for (col in add) {
      if (!col %in% names(template)) template[[col]] <- NA_character_
    }
  }

  if (!is.null(path)) {
    data.table::fwrite(template, path, na = "")
    return(invisible(template))
  }
  template
}

#' @rdname build_lib
#' @export
join_lib_metadata <- function(x, lookup, by, require_complete = FALSE,
                              return = c("object", "table", "report"),
                              suffixes = c(".x", ".y")) {
  return <- match.arg(return)
  is_os <- is_OpenSpecy(x)
  is_specs <- is_Specs(x)
  if (!(is_os || is_specs)) {
    stop("'x' must be an OpenSpecy or Specs object", call. = FALSE)
  }
  metadata <- data.table::as.data.table(data.table::copy(x$metadata))
  lookup <- .lib_read_lookup(lookup)

  keys <- if (is.null(names(by)) || all(names(by) == "")) {
    list(x = unname(by), y = unname(by))
  } else {
    list(x = names(by), y = unname(by))
  }
  .lib_require_cols(metadata, keys$x, "metadata")
  .lib_require_cols(lookup, keys$y, "lookup")

  make_key <- function(tab, cols) {
    vals <- lapply(cols, function(col) as.character(tab[[col]]))
    any_na <- Reduce(`|`, lapply(vals, is.na))
    key <- do.call(paste, c(vals, sep = "\r"))
    key[any_na] <- NA_character_
    key
  }

  metadata_key <- make_key(metadata, keys$x)
  lookup_key <- make_key(lookup, keys$y)
  value_cols <- setdiff(names(lookup), keys$y)

  dups <- duplicated(lookup_key) | duplicated(lookup_key, fromLast = TRUE)
  dups[is.na(lookup_key)] <- FALSE
  dup_report <- if (any(dups)) {
    data.table::data.table(value = lookup_key[dups])[
      , .(n = .N), by = "value"][
        , `:=`(problem = "duplicate_lookup_key",
               column = paste(keys$y, collapse = "|"))][
                 , .(problem, column, value, n)]
  } else {
    data.table::data.table(problem = character(), column = character(),
                           value = character(), n = integer())
  }
  if (nrow(dup_report) > 0) {
    attr(dup_report, "data") <- lookup
    stop("Lookup keys must be unique before joining. Duplicate keys: ",
         paste(head(dup_report$value, 10), collapse = ", "), call. = FALSE)
  }

  missing_key <- is.na(metadata_key) | !metadata_key %in% lookup_key
  unmatched <- data.table::data.table(value = metadata_key[missing_key])[
    !is.na(value), .(n = .N), by = "value"][
      , `:=`(problem = "unmatched_metadata_key",
             column = paste(keys$x, collapse = "|"))][
               , .(problem, column, value, n)]

  missing_values <- data.table::data.table()
  matched <- match(metadata_key, lookup_key)
  if (length(value_cols) > 0 && any(!is.na(matched))) {
    for (col in value_cols) {
      vals <- lookup[[col]][matched]
      miss <- !is.na(matched) & is.na(vals)
      if (any(miss)) {
        missing_values <- rbind(missing_values, data.table::data.table(
          problem = "missing_joined_value",
          column = col,
          value = metadata_key[miss],
          n = 1L
        )[, .(n = .N), by = .(problem, column, value)], fill = TRUE)
      }
    }
  }
  report <- data.table::rbindlist(list(unmatched, missing_values), fill = TRUE)
  .lib_alert_join_report(report, require_complete)

  lookup_values <- lookup[, setdiff(names(lookup), keys$y), with = FALSE]
  meta <- data.table::copy(metadata)
  look <- data.table::copy(lookup_values)
  meta$..join_key <- metadata_key
  meta$..row_id <- seq_len(nrow(meta))
  look$..join_key <- lookup_key
  joined <- merge(meta, look, by = "..join_key", all.x = TRUE, sort = FALSE,
                  suffixes = suffixes)
  data.table::setorder(joined, "..row_id")
  joined[, c("..join_key", "..row_id") := NULL]
  attr(joined, "join_report") <- report

  if (return == "report") return(list(data = joined, report = report))
  if (return == "table") return(joined)

  x$metadata <- joined
  attr(x, "join_report") <- report
  x
}

#' @rdname build_lib
#' @export
join_material_hierarchy <- function(x, hierarchy, key_col = "material",
                                    levels = c("material", "material_class",
                                               "material_type"),
                                    output_names = levels,
                                    require_complete = FALSE,
                                    return = c("object", "table", "report")) {
  return <- match.arg(return)
  is_os <- is_OpenSpecy(x)
  is_specs <- is_Specs(x)
  if (!(is_os || is_specs)) {
    stop("'x' must be an OpenSpecy or Specs object", call. = FALSE)
  }
  metadata <- data.table::as.data.table(data.table::copy(x$metadata))
  hierarchy <- .lib_read_lookup(hierarchy)
  if (!is.null(names(output_names)) && any(names(output_names) != "")) {
    missing <- setdiff(levels, names(output_names))
    if (length(missing) > 0) {
      stop("'output_names' is missing hierarchy levels: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    output_names <- unname(output_names[levels])
  }
  if (length(output_names) != length(levels)) {
    stop("'output_names' must have the same length as 'levels'", call. = FALSE)
  }

  .lib_require_cols(metadata, key_col, "metadata")
  .lib_require_cols(hierarchy, levels, "hierarchy")

  keys <- as.character(metadata[[key_col]])

  out <- metadata
  for (col in output_names) out[[col]] <- NA_character_
  matched_level <- rep(NA_character_, nrow(out))

  remaining <- seq_len(nrow(out))
  duplicate_reports <- list()

  for (i in seq_along(levels)) {
    if (length(remaining) == 0L) break
    level <- levels[i]
    cols <- levels[i:length(levels)]
    h <- unique(hierarchy[, cols, with = FALSE])
    h_key <- as.character(h[[level]])

    dups <- duplicated(h_key) | duplicated(h_key, fromLast = TRUE)
    dups[is.na(h_key)] <- FALSE
    if (any(dups)) {
      duplicate_reports[[level]] <- data.table::data.table(
        problem = "duplicate_hierarchy_key",
        level = level,
        value = unique(h_key[dups])
      )
      next
    }

    idx <- match(keys[remaining], h_key)
    found <- !is.na(idx)
    rows <- remaining[found]
    if (length(rows) > 0) {
      for (j in i:length(levels)) {
        out[[output_names[j]]][rows] <- h[[levels[j]]][idx[found]]
      }
      matched_level[rows] <- level
      remaining <- remaining[!found]
    }
  }

  unmatched <- data.table::data.table(value = keys[is.na(matched_level)])[
    !is.na(value), .(n = .N), by = "value"][
      , `:=`(problem = "unmatched_hierarchy_key",
             column = "hierarchy")][
               , .(problem, column, value, n)]
  duplicates <- data.table::rbindlist(duplicate_reports, fill = TRUE)
  if (nrow(duplicates) > 0) {
    duplicates[, `:=`(column = level, n = 1L)]
    duplicates <- duplicates[, .(problem, column, value, n)]
  }
  report <- data.table::rbindlist(list(unmatched, duplicates), fill = TRUE)
  .lib_alert_join_report(report, require_complete)
  attr(out, "join_report") <- report

  if (return == "report") return(list(data = out, report = report))
  if (return == "table") return(out)

  x$metadata <- out
  attr(x, "join_report") <- report
  x
}

#' @rdname build_lib
#' @export
dedupe_spec <- function(x, id_col = "sample_name", exclude_ids = NULL,
                        duplicate = c("first", "remove_all", "none"),
                        scale = 100, algo = "md5") {
  duplicate <- match.arg(duplicate)
  x <- as_OpenSpecy(x)

  ids <- vapply(seq_len(ncol(x$spectra)), function(i) {
    digest::digest(list(as.integer(x$wavenumber),
                        as.integer(x$spectra[, i] * scale)),
                   algo = algo)
  }, FUN.VALUE = character(1))
  x$metadata[[id_col]] <- ids
  colnames(x$spectra) <- ids
  x$metadata$col_id <- ids

  keep <- rep(TRUE, length(ids))
  if (!is.null(exclude_ids)) keep <- keep & !ids %in% exclude_ids
  if (duplicate == "first") keep <- keep & !duplicated(ids)
  if (duplicate == "remove_all") {
    keep <- keep & !(duplicated(ids) | duplicated(ids, fromLast = TRUE))
  }
  if (!all(keep)) x <- filter_spec(x, keep)
  x
}

#' @rdname build_lib
#' @export
reduce_lib <- function(x, group_cols = "material_class", id_col = "sample_name",
                       k = 50, min_n = k, return = c("object", "ids"), ...) {
  return <- match.arg(return)
  x <- as_OpenSpecy(x)
  .lib_require_cols(x$metadata, group_cols, "metadata")

  ids <- .lib_ids(x, id_col)
  reduction_obj <- x
  spectra <- make_rel(x$spectra, na.rm = TRUE)
  spectra <- .matrix_mean_replace(spectra)
  reduction_obj$spectra <- spectra

  groups <- do.call(paste, c(x$metadata[, group_cols, with = FALSE], sep = "_"))
  keep_ids <- unlist(lapply(split(seq_along(groups), groups), function(idx) {
    if (length(idx) <= min_n || length(idx) <= k) return(ids[idx])
    .pam_group_ids(filter_spec(reduction_obj, idx), id_col = id_col, k = k,
                   ...)
  }), use.names = FALSE)

  if (return == "ids") return(keep_ids)
  filter_spec(x, keep_ids)
}

#' @rdname build_lib
#' @export
build_model_lib <- function(x, class_col = "material_class",
                            type_col = "spectrum_type", min_n = 10,
                            alpha = 0.1, seed = 123,
                            grouped = TRUE, weights = TRUE,
                            make_relative = TRUE, complete_cases = TRUE,
                            ...) {
  x <- as_OpenSpecy(x)
  .lib_require_cols(x$metadata, class_col, "metadata")

  wavenumbers <- x$wavenumber
  spectra <- x$spectra
  if (make_relative) spectra <- make_rel(spectra, na.rm = TRUE)

  train <- t(spectra)
  colnames(train) <- as.character(wavenumbers)
  metadata <- data.table::copy(x$metadata)

  if (complete_cases) {
    ok <- stats::complete.cases(train)
    train <- train[ok, , drop = FALSE]
    metadata <- metadata[ok, ]
  }

  labels <- as.character(metadata[[class_col]])
  if (!is.null(type_col) && type_col %in% names(metadata)) {
    types <- as.character(metadata[[type_col]])
    labels <- ifelse(is.na(types), labels, paste(types, labels, sep = "_"))
  }
  keep <- !is.na(labels)
  tab <- table(labels[keep])
  keep <- keep & labels %in% names(tab)[tab >= min_n]

  train <- train[keep, , drop = FALSE]
  labels <- labels[keep]
  if (nrow(train) == 0 || length(unique(labels)) < 2) {
    stop("At least two classes with 'min_n' spectra are required to train a model",
         call. = FALSE)
  }

  outcome <- as.integer(factor(labels))
  weight_vec <- NULL
  if (weights) weight_vec <- 1 / (table(outcome)[as.character(outcome)] / length(outcome))

  set.seed(seed)
  glmnet_args <- list(
    x = train,
    y = outcome,
    alpha = alpha,
    family = "multinomial",
    intercept = FALSE,
    type.multinomial = if (grouped) "grouped" else "ungrouped"
  )
  if (!is.null(weight_vec)) glmnet_args$weights <- as.numeric(weight_vec)
  user_args <- list(...)
  glmnet_args[names(user_args)] <- user_args

  model <- do.call(glmnet::glmnet, glmnet_args)
  lambda <- min(model$lambda)
  coefficients <- stats::coef(model, s = lambda)

  dimension_conversion <- unique(data.table::data.table(
    factor_num = outcome,
    name = labels
  ))
  coef_list <- if (is.list(coefficients)) coefficients else list(coefficients)
  rows <- lapply(seq_along(coef_list), function(item) {
    data.table::data.table(
      dimensions_used = coef_list[[item]]@i,
      dimension_units = coef_list[[item]]@x,
      variable = item
    )
  })
  coefficient_values <- data.table::rbindlist(rows)
  wave <- data.table::data.table(
    names = coef_list[[1]]@Dimnames[[1]],
    id = seq_along(coef_list[[1]]@Dimnames[[1]]) - 1L
  )
  coefficients_join <- merge(coefficient_values, dimension_conversion,
                             by.x = "variable", by.y = "factor_num",
                             all.x = TRUE)
  coefficients_join <- merge(coefficients_join, wave,
                             by.x = "dimensions_used", by.y = "id",
                             all.x = FALSE)
  coefficients_join$names <- suppressWarnings(as.numeric(ifelse(
    coefficients_join$names == "(Intercept)", "0", coefficients_join$names
  )))

  predictions <- predict(model, newx = train, s = lambda, type = "response")
  pred <- .ai_prediction_table(predictions, n = nrow(train))
  actual <- data.table::data.table(row_id = seq_along(outcome),
                                   actual_label = outcome,
                                   actual_name = labels)
  accuracy <- merge(pred, actual, by.x = "x", by.y = "row_id", all.x = TRUE)
  accuracy <- merge(accuracy, dimension_conversion, by.x = "y",
                    by.y = "factor_num", all.x = TRUE)
  names(accuracy)[names(accuracy) == "name"] <- "predicted_name"
  accuracy$correct <- accuracy$actual_name == accuracy$predicted_name
  confusion <- accuracy[, .(label_accuracy = mean(correct),
                            observation_count = .N), by = "actual_name"]

  list(
    model = model,
    dimension_conversion = dimension_conversion,
    accuracy = accuracy,
    confusion = confusion,
    coefficients = coefficients_join,
    class_names = unique(labels),
    class_num = length(unique(outcome)),
    observation_count = length(labels),
    overall_accuracy = mean(accuracy$correct),
    class_accuracy = mean(confusion$label_accuracy),
    overall_accuracy2 = mean(accuracy$correct),
    variable_num = nrow(coefficients_join),
    all_variables = as.numeric(colnames(train)),
    variables_in = coefficients_join$names
  )
}

#' @rdname build_lib
#' @export
assess_lib <- function(x, class_col = NULL, id_col = "sample_name",
                       nearest = !is.null(class_col)) {
  x <- as_OpenSpecy(x)
  valid <- suppressWarnings(check_OpenSpecy(x))
  out <- data.table::data.table(
    metric = c("valid_OpenSpecy", "spectra", "wavenumbers"),
    value = c(as.character(valid), ncol(x$spectra), length(x$wavenumber))
  )

  if (!is.null(class_col) && class_col %in% names(x$metadata)) {
    counts <- data.table::data.table(class = x$metadata[[class_col]])[
      , .N, by = "class"]
    out <- rbind(out, data.table::data.table(
      metric = c("classes", "smallest_class"),
      value = c(length(unique(counts$class)), min(counts$N))
    ), fill = TRUE)
  }

  if (nearest && ncol(x$spectra) > 1 && class_col %in% names(x$metadata)) {
    cors <- cor_spec(x, x)
    diag(cors) <- NA
    top <- max_cor_named(cors)
    ids <- .lib_ids(x, id_col)
    matched <- x$metadata[[class_col]][match(names(top), ids)]
    accuracy <- mean(matched == x$metadata[[class_col]], na.rm = TRUE)
    out <- rbind(out, data.table::data.table(
      metric = "nearest_class_accuracy",
      value = accuracy
    ), fill = TRUE)
  }

  out
}

.default_lib_recipes <- function() {
  list(
    raw = list(),
    derivative = list(
      conform_spec = FALSE,
      smooth_intens = TRUE,
      smooth_intens_args = list(
        polynomial = 3,
        window = 15,
        derivative = 1,
        abs = TRUE
      ),
      subtr_baseline = FALSE,
      make_rel = TRUE
    ),
    nobaseline = list(
      conform_spec = FALSE,
      smooth_intens = FALSE,
      subtr_baseline = TRUE,
      make_rel = TRUE
    )
  )
}

.lib_auto_lookup_key <- function(metadata, lookup_table) {
  metadata <- data.table::as.data.table(metadata)
  lookup_table <- data.table::as.data.table(lookup_table)
  shared <- intersect(names(metadata), names(lookup_table))
  if (length(shared) == 0L) {
    return(list(shared = shared, candidates = character()))
  }

  usable <- vapply(shared, function(col) {
    metadata_values <- .lib_key_values(metadata[[col]])
    lookup_values <- .lib_key_values(lookup_table[[col]], unique = FALSE)
    if (length(metadata_values) == 0L || length(lookup_values) == 0L) {
      return(FALSE)
    }
    has_overlap <- any(metadata_values %in% unique(lookup_values))
    unique_lookup_keys <- !anyDuplicated(lookup_values)
    has_overlap && unique_lookup_keys
  }, logical(1))

  list(shared = shared, candidates = shared[usable])
}

.lib_key_values <- function(x, unique = TRUE) {
  values <- as.character(x)
  values <- values[!is.na(values)]
  values <- values[nzchar(values, keepNA = FALSE)]
  if (isTRUE(unique)) values <- unique(values)
  values
}

.lib_read_lookup <- function(x) {
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    return(data.table::fread(x))
  }
  data.table::as.data.table(data.table::copy(x))
}

#' @rdname lib_metadata_name_lookup
#' @export
lib_clean_name <- function(x) {
  x <- iconv(as.character(x), to = "ASCII", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("%", "perc", x, fixed = TRUE)
  x <- gsub("->", "_", x, fixed = TRUE)
  x <- gsub("[^a-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[is.na(x) | x == ""] <- "column"
  x
}

#' @rdname lib_metadata_name_lookup
#' @export
lib_clean_metadata <- function(x,
                               name_lookup = lib_metadata_name_lookup(),
                               clean_values = FALSE) {
  if (!is.logical(clean_values) || length(clean_values) != 1L ||
      is.na(clean_values)) {
    stop("'clean_values' must be TRUE or FALSE", call. = FALSE)
  }
  metadata <- data.table::as.data.table(data.table::copy(x))
  original_names <- names(metadata)
  cleaned_names <- lib_clean_name(original_names)
  canonical_names <- cleaned_names
  rule_priority <- rep(Inf, length(cleaned_names))
  lookup <- NULL

  if (!is.null(name_lookup)) {
    match_without_underscores <-
      attr(name_lookup, "match_without_underscores", exact = TRUE)
    match_singular_plural <-
      attr(name_lookup, "match_singular_plural", exact = TRUE)
    if (is.null(match_without_underscores)) match_without_underscores <- TRUE
    if (is.null(match_singular_plural)) match_singular_plural <- TRUE

    lookup <- data.table::as.data.table(data.table::copy(name_lookup))
    .lib_require_cols(lookup, "canonical_name", "metadata name lookup")
    if (!"source_name" %in% names(lookup)) {
      lookup$source_name <- NA_character_
    }
    if (!"regex" %in% names(lookup)) lookup$regex <- NA_character_
    lookup <- lookup[, c("canonical_name", "source_name", "regex"),
                     with = FALSE]
    lookup$canonical_name <- lib_clean_name(lookup$canonical_name)
    exact <- !is.na(lookup$source_name)
    lookup$source_name[exact] <- lib_clean_name(lookup$source_name[exact])
    empty_regex <- !is.na(lookup$regex) & lookup$regex == ""
    lookup$regex[empty_regex] <- NA_character_
    lookup <- unique(lookup)

    has_source <- !is.na(lookup$source_name)
    has_regex <- !is.na(lookup$regex)
    if (any(has_source == has_regex)) {
      stop("Each metadata name rule must contain exactly one of ",
           "'source_name' or 'regex'", call. = FALSE)
    }

    exact_rules <- lookup[has_source, ]
    source_groups <- split(exact_rules$canonical_name,
                           exact_rules$source_name)
    ambiguous <- names(source_groups)[vapply(
      source_groups,
      function(value) length(unique(value)) > 1L,
      logical(1)
    )]
    if (length(ambiguous) > 0) {
      stop("Exact metadata name aliases map to multiple canonical names: ",
           paste(ambiguous, collapse = ", "), call. = FALSE)
    }

    matched <- match(cleaned_names, exact_rules$source_name)
    found <- !is.na(matched)
    canonical_names[found] <- exact_rules$canonical_name[matched[found]]
    exact_is_canonical <- exact_rules$source_name ==
      exact_rules$canonical_name
    rule_priority[found] <- ifelse(
      exact_is_canonical[matched[found]],
      matched[found],
      2L * nrow(exact_rules) + matched[found]
    )

    smart_key <- function(value) {
      if (isTRUE(match_without_underscores)) {
        value <- gsub("_", "", value, fixed = TRUE)
      }
      if (isTRUE(match_singular_plural)) value <- sub("s$", "", value)
      value
    }
    unresolved <- !found
    if (any(unresolved) &&
        (isTRUE(match_without_underscores) ||
         isTRUE(match_singular_plural)) &&
        nrow(exact_rules) > 0L) {
      rule_keys <- smart_key(exact_rules$source_name)
      name_keys <- smart_key(cleaned_names)
      key_groups <- split(exact_rules$canonical_name, rule_keys)
      ambiguous_keys <- names(key_groups)[vapply(
        key_groups,
        function(value) length(unique(value)) > 1L,
        logical(1)
      )]
      conflicting <- which(unresolved & name_keys %in% ambiguous_keys)
      if (length(conflicting) > 0L) {
        details <- vapply(conflicting, function(i) {
          canonical <- unique(key_groups[[name_keys[i]]])
          paste0("'", original_names[i], "' -> ",
                 paste(canonical, collapse = ", "))
        }, character(1))
        stop("Automatic metadata name matching is ambiguous: ",
             paste(details, collapse = "; "),
             ". Add an exact alias or disable the relevant smart matching ",
             "option.", call. = FALSE)
      }

      smart_match <- match(name_keys[unresolved], rule_keys)
      smart_found <- !is.na(smart_match)
      unresolved_rows <- which(unresolved)
      rows <- unresolved_rows[smart_found]
      matched_rules <- smart_match[smart_found]
      canonical_names[rows] <- exact_rules$canonical_name[matched_rules]
      rule_priority[rows] <- ifelse(
        exact_is_canonical[matched_rules],
        nrow(exact_rules) + matched_rules,
        3L * nrow(exact_rules) + matched_rules
      )
      found[rows] <- TRUE
    }

    regex_rules <- lookup[has_regex, ]
    regex_matches <- vector("list", length(cleaned_names))
    if (nrow(regex_rules) > 0L) {
      pattern_hits <- lapply(seq_len(nrow(regex_rules)), function(i) {
        tryCatch(
          grepl(regex_rules$regex[i], cleaned_names, perl = TRUE),
          warning = function(w) {
            stop("Invalid metadata name regex '", regex_rules$regex[i],
                 "' for '", regex_rules$canonical_name[i], "': ",
                 conditionMessage(w), call. = FALSE)
          },
          error = function(e) {
            stop("Invalid metadata name regex '", regex_rules$regex[i],
                 "' for '", regex_rules$canonical_name[i], "': ",
                 conditionMessage(e), call. = FALSE)
          }
        )
      })
      for (i in seq_along(cleaned_names)) {
        regex_matches[[i]] <- which(vapply(
          pattern_hits,
          function(hit) isTRUE(hit[i]),
          logical(1)
        ))
      }
      overlapping <- which(lengths(regex_matches) > 1L)
      if (length(overlapping) > 0L) {
        details <- vapply(overlapping, function(i) {
          rows <- regex_matches[[i]]
          rules <- paste0(
            "'", regex_rules$regex[rows], "' -> '",
            regex_rules$canonical_name[rows], "'"
          )
          paste0("'", original_names[i], "' matched ",
                 paste(rules, collapse = ", "))
        }, character(1))
        stop("Multiple metadata name regular expressions matched the same ",
             "column: ", paste(details, collapse = "; "),
             ". Make the patterns mutually exclusive.", call. = FALSE)
      }

      regex_found <- !found & lengths(regex_matches) == 1L
      for (i in which(regex_found)) {
        row <- regex_matches[[i]]
        canonical_names[i] <- regex_rules$canonical_name[row]
        rule_priority[i] <- 4L * nrow(exact_rules) + row
      }
    }
  }

  output_names <- unique(canonical_names)
  output <- lapply(output_names, function(canonical) {
    positions <- which(canonical_names == canonical)
    if (!is.null(lookup)) {
      positions <- positions[order(
        cleaned_names[positions] != canonical,
        rule_priority[positions],
        positions
      )]
    } else {
      positions <- positions[order(cleaned_names[positions] != canonical,
                                   positions)]
    }

    values <- lapply(positions, function(position) metadata[[position]])
    signatures <- vapply(values, function(value) {
      paste(typeof(value), paste(class(value), collapse = "/"), sep = ":")
    }, character(1))
    if (length(unique(signatures)) > 1L ||
        any(vapply(values, is.factor, logical(1)))) {
      values <- lapply(values, as.character)
    }

    result <- values[[1]]
    if (length(values) > 1L) {
      for (candidate in values[-1L]) {
        fill <- is.na(result) & !is.na(candidate)
        result[fill] <- candidate[fill]
      }
    }
    result
  })
  names(output) <- output_names
  output <- data.table::as.data.table(output)
  if (isTRUE(clean_values)) output <- .lib_clean_metadata_values(output)
  output
}

.lib_clean_metadata_values <- function(x) {
  out <- data.table::as.data.table(data.table::copy(x))
  for (col in names(out)) {
    value <- out[[col]]
    if (!is.character(value) && !is.factor(value)) next
    value <- iconv(as.character(value), to = "ASCII", sub = "")
    value <- tolower(trimws(value))
    value[value %in% c("", "na", "null", "not available")] <- NA_character_
    data.table::set(out, j = col, value = value)
  }
  out
}

.lib_coalesce_joined_metadata <- function(metadata, columns,
                                          suffixes = c(".x", ".y")) {
  metadata <- data.table::as.data.table(metadata)
  for (col in columns) {
    left <- paste0(col, suffixes[[1L]])
    right <- paste0(col, suffixes[[2L]])
    if (!all(c(left, right) %in% names(metadata))) next
    result <- metadata[[left]]
    replacement <- metadata[[right]]
    use_replacement <- !is.na(replacement)
    if (is.character(replacement)) {
      use_replacement <- use_replacement & nzchar(replacement)
    }
    result[use_replacement] <- replacement[use_replacement]
    data.table::set(metadata, j = left, value = result)
    data.table::setnames(metadata, left, col)
    metadata[, (right) := NULL]
  }
  metadata
}

.lib_require_cols <- function(x, cols, label) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop("Missing ", label, " columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

.lib_alert_join_report <- function(report, require_complete) {
  if (nrow(report) == 0) return(invisible(NULL))
  summary <- report[, .(n = sum(n)), by = .(problem, column)]
  msg <- paste(apply(summary, 1, function(x) {
    paste0(x[["problem"]], " in ", x[["column"]], ": ", x[["n"]])
  }), collapse = "; ")
  if (require_complete) stop(msg, call. = FALSE)
  warning(msg, call. = FALSE)
  invisible(NULL)
}

.lib_ids <- function(x, id_col) {
  if (id_col %in% names(x$metadata)) return(as.character(x$metadata[[id_col]]))
  colnames(x$spectra)
}

.lib_filter_excluded <- function(x, exclude_ids, id_col = "sample_name") {
  exclude_ids <- as.character(exclude_ids)
  metadata_cols <- intersect(
    c(id_col, paste0(id_col, "_old"), "col_id"),
    names(x$metadata)
  )
  metadata_hit <- rep(FALSE, nrow(x$metadata))
  for (col in metadata_cols) {
    metadata_hit <- metadata_hit |
      as.character(x$metadata[[col]]) %in% exclude_ids
  }
  spectra_hit <- colnames(x$spectra) %in% exclude_ids
  keep <- !(metadata_hit | spectra_hit)
  if (!all(keep)) x <- filter_spec(x, keep)
  x
}

.lib_dedupe_existing_ids <- function(x, id_col = "sample_name",
                                     duplicate = "first") {
  if (!id_col %in% names(x$metadata)) return(NULL)
  ids <- as.character(x$metadata[[id_col]])
  if (length(ids) != ncol(x$spectra) ||
      any(is.na(ids) | !nzchar(ids))) {
    return(NULL)
  }

  x$metadata[[id_col]] <- ids
  colnames(x$spectra) <- ids
  x$metadata$col_id <- ids

  keep <- rep(TRUE, length(ids))
  if (duplicate == "first") keep <- keep & !duplicated(ids)
  if (duplicate == "remove_all") {
    keep <- keep & !(duplicated(ids) | duplicated(ids, fromLast = TRUE))
  }
  if (!all(keep)) x <- filter_spec(x, keep)
  x
}

.pam_distance_matrix <- function(distance) {
  if (inherits(distance, "dist")) {
    distance <- as.matrix(distance)
  } else {
    distance <- as.matrix(distance)
  }
  if (!is.numeric(distance) || nrow(distance) != ncol(distance)) {
    stop("'distance' must be a square numeric dissimilarity matrix",
         call. = FALSE)
  }
  if (anyNA(distance)) {
    stop("'distance' must not contain missing values", call. = FALSE)
  }
  if (!isTRUE(all.equal(distance, t(distance), tolerance = 1e-12,
                        check.attributes = FALSE))) {
    stop("'distance' must be symmetric", call. = FALSE)
  }

  if (any(distance < 0)) {
    stop("'distance' must contain non-negative dissimilarities",
         call. = FALSE)
  }
  diag(distance) <- 0
  storage.mode(distance) <- "double"
  distance
}

.pam_validate_k <- function(k, n) {
  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k < 1 || k >= n || k != floor(k)) {
    stop("'k' must be a positive integer smaller than the number of spectra",
         call. = FALSE)
  }
  as.integer(k)
}

.pam_validate_medoids <- function(medoids, k, n, names = NULL) {
  if (is.null(medoids)) return(NULL)
  if (is.character(medoids)) {
    if (is.null(names)) {
      stop("Character 'medoids' need named distance rows", call. = FALSE)
    }
    medoids <- match(medoids, names)
  }
  if (!is.numeric(medoids) || length(medoids) != k ||
      anyNA(medoids) || any(medoids != floor(medoids)) ||
      any(medoids < 1 | medoids > n) || anyDuplicated(medoids)) {
    stop("'medoids' must be unique observation indices of length 'k'",
         call. = FALSE)
  }
  as.integer(medoids)
}

.pam_assign_info <- function(distance, medoids) {
  d_med <- distance[, medoids, drop = FALSE]
  cluster <- max.col(-d_med, ties.method = "last")

  nearest <- d_med[cbind(seq_len(nrow(distance)), cluster)]

  second <- rep(Inf, nrow(distance))
  if (length(medoids) > 1L) {
    second_dist <- d_med
    second_dist[cbind(seq_len(nrow(distance)), cluster)] <- Inf
    second <- matrixStats::rowMins(second_dist)
  }

  list(cluster = cluster, nearest = nearest, second = second)
}

.pam_order_medoids <- function(distance, medoids) {
  sort.int(medoids)
}

.pam_build_medoids <- function(distance, k) {
  n <- nrow(distance)
  medoids <- integer()
  nearest <- rep(Inf, n)

  for (step in seq_len(k)) {
    candidates <- setdiff(seq_len(n), medoids)
    totals <- vapply(candidates, function(candidate) {
      sum(pmin(nearest, distance[, candidate]))
    }, numeric(1))
    best <- min(totals)
    candidate <- candidates[max(which(totals <= best))]
    medoids <- c(medoids, candidate)
    nearest <- pmin(nearest, distance[, candidate])
  }

  medoids
}

.pam_eager_swap <- function(distance, medoids) {
  n <- nrow(distance)
  tol <- sqrt(.Machine$double.eps)
  info <- .pam_assign_info(distance, medoids)

  repeat {
    changed <- FALSE
    for (candidate in seq_len(n)) {
      if (candidate %in% medoids) next

      candidate_distance <- distance[, candidate]
      new_distance <- pmin(candidate_distance, info$nearest)
      base_delta <- sum(new_distance - info$nearest)
      delta <- rep(base_delta, length(medoids))
      for (medoid_i in seq_along(medoids)) {
        removed <- info$cluster == medoid_i
        if (!any(removed)) next
        delta[medoid_i] <- base_delta +
          sum(pmin(candidate_distance[removed], info$second[removed]) -
                new_distance[removed])
      }

      best_medoid <- which.min(delta)
      if (delta[best_medoid] < -tol) {
        medoids[best_medoid] <- candidate
        info <- .pam_assign_info(distance, medoids)
        changed <- TRUE
      }
    }
    if (!changed) break
  }

  medoids
}

.pam_medoids <- function(distance, k, medoids = NULL, do.swap = TRUE,
                         pamonce = 6, trace.lev = 0) {
  if (!is.logical(do.swap) || length(do.swap) != 1L || is.na(do.swap)) {
    stop("'do.swap' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(trace.lev) || length(trace.lev) != 1L ||
      is.na(trace.lev) || trace.lev != 0) {
    stop("'trace.lev' is not supported by reduce_lib()'s internal PAM",
         call. = FALSE)
  }
  if (!is.numeric(pamonce) || length(pamonce) != 1L ||
      is.na(pamonce) || pamonce != 6) {
    stop("reduce_lib()'s internal PAM currently supports pamonce = 6",
         call. = FALSE)
  }

  distance <- .pam_distance_matrix(distance)
  n <- nrow(distance)
  k <- .pam_validate_k(k, n)
  medoids <- .pam_validate_medoids(medoids, k, n, rownames(distance))
  if (is.null(medoids)) medoids <- .pam_build_medoids(distance, k)
  if (isTRUE(do.swap)) medoids <- .pam_eager_swap(distance, medoids)
  .pam_order_medoids(distance, medoids)
}

.pam_group_ids <- function(x, id_col, k, ...) {
  x <- as_OpenSpecy(x)
  ids <- .lib_ids(x, id_col)
  if (ncol(x$spectra) <= k) return(ids)
  cors <- cor_spec(x, x, compute = "optimized")
  cors[is.na(cors)] <- 0
  cors <- pmax(pmin(cors, 1), -1)
  diag(cors) <- 1
  distance <- stats::as.dist(1 - cors)
  user_args <- list(...)
  if (length(user_args) > 0L) {
    if (is.null(names(user_args)) || any(names(user_args) == "")) {
      stop("PAM arguments passed through '...' must be named",
           call. = FALSE)
    }
    supported <- c("medoids", "do.swap", "pamonce", "trace.lev")
    unsupported <- setdiff(names(user_args), supported)
    if (length(unsupported) > 0L) {
      stop("Unsupported reduce_lib() PAM argument(s): ",
           paste(unsupported, collapse = ", "), call. = FALSE)
    }
  }

  pam_args <- c(list(distance = distance, k = min(k, length(ids) - 1L)),
                user_args)
  ids[do.call(.pam_medoids, pam_args)]
}
