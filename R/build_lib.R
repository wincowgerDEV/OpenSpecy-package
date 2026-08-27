#' @rdname build_lib
#' @title Build spectral libraries
#'
#' @description
#' Create reference libraries from source files and OpenSpecy objects.
#' With no \code{x}, or when \code{output_dir} is supplied,
#' \code{build_lib()} runs the official end-to-end workflow and returns raw,
#' processed, medoid, model, and assessment artifacts in one object. Supporting
#' functions remain available for advanced composition.
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
#' normalized to lowercase trimmed character values before lookup joins.
#' \code{spectrum_identity} is also reduced to a basename when it is a
#' recognizable path, then trailing extensions supported by
#' \code{\link{read_any}()} are removed. The same normalization is applied to
#' exact lookup keys. Regex class rules belong in a separate table and can be
#' applied afterward with \code{\link{predict_class_reference}()}.
#' This keeps filenames usable as identities without treating file containers
#' as part of a material name. By default, each source is also
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
#' The official workflow reads its environment-aware paths inside
#' \code{build_lib()}, writes each completed stage under
#' \code{output_dir/checkpoints}, and promotes validated legacy-compatible files
#' into a versioned release directory. With \code{reuse = TRUE}, a checkpoint
#' is reused only when its manifest signature matches the source files, curated
#' tables, relevant arguments, package version, and builder implementation.
#' Full assessments use the complete candidate and legacy artifacts. A seeded
#' ten-percent holdout is grouped by stable spectrum identity so the same
#' spectrum cannot occur in both reference training and testing partitions.
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
#' Omit \code{x} to discover the official sources from the path defaults.
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
#' paths. A lookup may instead be supplied as \code{list(lookup = x, by = key)}
#' to use an explicit key (including a named metadata-to-lookup key mapping).
#' \code{fill_only = TRUE} preserves existing nonblank metadata values while
#' filling gaps from the lookup. The older \code{fallback_by} lookup field is
#' deprecated; canonical metadata keys are now filled from reviewed internal
#' aliases before any external join.
#' If non-\code{NULL}, each is joined with
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
#' @param prune \code{NULL}, or a named list mapping recipe names to argument
#' lists for \code{\link{prune_lib}()}. Selected recipes are pruned independently
#' after processing and assessment. \code{NULL} preserves unpruned outputs.
#' @param progress logical; whether \code{build_lib()} reports named processing
#' stages and elapsed time.
#' @param data_dir default directory containing official source and build data.
#' @param source_file default raw source RDS path.
#' @param processed_dir default directory recursively searched for processed
#' source RDS files.
#' @param workflow_data directory containing the curated reference CSV tables.
#' @param output_dir \code{NULL} for the composable in-memory return, or a
#' directory that triggers the complete checkpointed workflow. When \code{x} is
#' omitted, the default is \code{reference-library-build} under \code{data_dir}.
#' @param previous_library_dir directory containing the seven legacy artifacts
#' used for complete old/new assessment, \code{"system"}, or \code{NULL} to skip
#' external comparison. The no-argument official workflow defaults to
#' \code{"system"} and retrieves missing artifacts with \code{get_lib()}.
#' @param reuse logical; whether manifest-compatible completed checkpoints and
#' versioned release files may be reused.
#' @param seed fixed seed for the grouped old/new assessment split.
#' @param holdout fraction of stable spectrum groups reserved for assessment.
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
#' @param material_type_col metadata column used to distinguish plastic from
#' non-plastic candidates when reassigning generic classes.
#' @param exclude numeric length-two wavenumber interval excluded from pruning
#' correlations.
#' @param \ldots further arguments passed to the underlying operation.
#'
#' @return
#' Each library returned by \code{build_lib()} includes a
#' \code{spectrum_identity_cleanup_report} attribute listing changed original
#' and normalized identities with their counts.
#' In composable mode, \code{build_lib()} returns a named list of
#' \code{OpenSpecy} libraries. Its end-to-end mode returns one list containing
#' \code{libraries}, \code{medoids}, \code{models}, and \code{assessments}; each
#' assessment item is a reviewable data.table, and each model contains one
#' \code{tests} data.table.
#' \code{join_lib_metadata()}, \code{join_material_hierarchy()},
#' \code{dedupe_spec()}, \code{prune_lib()}, and \code{reduce_lib()} return an updated spectral
#' object unless \code{return} requests a table, report, or ids.
#' \code{make_lib_lookup_template()} returns a data.table unless \code{path} is
#' supplied, in which case it writes the csv and invisibly returns the table.
#' \code{build_model_lib()} returns a list suitable for AI classification with
#' \code{\link{match_spec}()} and one tidy \code{tests} table instead of
#' separate accuracy/confusion summaries. \code{assess_lib()} returns a
#' data.table summary.
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
                      signal_noise = TRUE, assess = FALSE, prune = NULL,
                      progress = TRUE,
                      data_dir = Sys.getenv(
                        "OPENSPECY_LIBRARY_DATA",
                        unset = normalizePath("..", mustWork = FALSE)
                      ),
                      source_file = Sys.getenv(
                        "OPENSPECY_SOURCE_FILE",
                        unset = file.path(data_dir, "library_raw.rds")
                      ),
                      processed_dir = Sys.getenv(
                        "OPENSPECY_PROCESSED_DIR",
                        unset = paste0(
                          "H:/My Drive/Work/Projects/OpenSpecy/",
                          "SpectraFilesCodeProcessedSpectra"
                        )
                      ),
                      workflow_data = file.path("workflows", "data"),
                      output_dir = NULL, previous_library_dir = NULL,
                      reuse = TRUE, seed = 123, holdout = 0.1,
                      ...) {
  official_mode <- missing(x) || !is.null(output_dir)
  if (!is.logical(reuse) || length(reuse) != 1L || is.na(reuse)) {
    stop("'reuse' must be TRUE or FALSE", call. = FALSE)
  }

  if (!official_mode) {
    return(.lib_build_core(
      x = x, recipes = recipes, range = range, res = res, id_col = id_col,
      exclude_ids = exclude_ids, dedupe = dedupe,
      metadata_lookups = metadata_lookups,
      material_hierarchy = material_hierarchy,
      metadata_name_lookup = metadata_name_lookup,
      clean_metadata_values = clean_metadata_values,
      convert_intensity = convert_intensity,
      restrict_range_args = restrict_range_args,
      signal_noise = signal_noise, assess = assess, prune = prune,
      progress = progress, ...
    ))
  }

  if (is.null(output_dir)) {
    output_dir <- Sys.getenv(
      "OPENSPECY_LIBRARY_OUTPUT",
      unset = file.path(data_dir, "reference-library-build")
    )
  }
  sources <- if (missing(x)) {
    .lib_reference_sources(source_file, processed_dir, progress = progress)
  } else {
    x
  }
  if (is.null(previous_library_dir) && missing(x)) {
    previous_library_dir <- "system"
  }

  .lib_build_reference(
    x = sources, recipes = recipes, range = range, res = res,
    id_col = id_col, exclude_ids = exclude_ids, dedupe = dedupe,
    metadata_lookups = metadata_lookups,
    material_hierarchy = material_hierarchy,
    metadata_name_lookup = metadata_name_lookup,
    clean_metadata_values = clean_metadata_values,
    convert_intensity = convert_intensity,
    restrict_range_args = restrict_range_args,
    signal_noise = signal_noise, assess = assess, prune = prune,
    progress = progress, workflow_data = workflow_data,
    output_dir = output_dir, previous_library_dir = previous_library_dir,
    reuse = reuse, seed = seed, holdout = holdout, ...
  )
}

.lib_build_core <- function(x, recipes = .default_lib_recipes(), range = "full",
                            res = 6, id_col = "sample_name",
                            exclude_ids = NULL, dedupe = TRUE,
                            metadata_lookups = NULL,
                            material_hierarchy = NULL,
                            metadata_name_lookup = lib_metadata_name_lookup(),
                            clean_metadata_values = FALSE,
                            convert_intensity = TRUE,
                            restrict_range_args = NULL,
                            signal_noise = TRUE, assess = FALSE, prune = NULL,
                            progress = TRUE, ...) {
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
  build_stage_report <- data.table::data.table(
    stage = "prepared", spectra = ncol(lib$spectra), removed = 0L
  )
  identity_cleanup_report <- data.table::data.table()
  if ("spectrum_identity" %in% names(lib$metadata)) {
    identity_before <- as.character(lib$metadata$spectrum_identity)
    identity_after <- .lib_clean_spectrum_identity(identity_before)
    changed <- xor(is.na(identity_before), is.na(identity_after)) |
      (!is.na(identity_before) & !is.na(identity_after) &
         identity_before != identity_after)
    identity_cleanup_report <- data.table::data.table(
      original = identity_before[changed],
      spectrum_identity = identity_after[changed]
    )[, .(n = .N), by = .(original, spectrum_identity)]
    lib$metadata$spectrum_identity <- identity_after
    if (any(changed)) {
      report(sprintf("cleaned %d spectrum identity value(s)", sum(changed)))
    }
  }

  source_key_report <- .lib_fill_metadata_key(
    lib$metadata, canonical = "organization", fallback = "user_name"
  )
  if (nrow(source_key_report) > 0L) {
    filled <- source_key_report[problem == "filled_canonical_key", sum(n)]
    conflicts <- source_key_report[problem == "canonical_key_conflict", sum(n)]
    report(sprintf(
      "standardized source keys (filled=%d; conflicts=%d)",
      ifelse(length(filled), filled, 0L),
      ifelse(length(conflicts), conflicts, 0L)
    ))
  }

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

  lookup_reports <- list()
  if (nrow(source_key_report) > 0L) {
    lookup_reports$canonical_source_keys <- source_key_report
  }
  if (!is.null(metadata_lookups)) {
    lookups <- if (.lib_is_lookup_spec(metadata_lookups)) {
      list(metadata_lookups)
    } else if (is.character(metadata_lookups) &&
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
      lookup_spec <- .lib_normalize_lookup_spec(lookup)
      lookup_table <- lib_clean_metadata(.lib_read_lookup(lookup_spec$lookup),
                                         metadata_name_lookup,
                                         clean_values = clean_metadata_values)
      if ("spectrum_identity" %in% names(lookup_table)) {
        lookup_table$spectrum_identity <- .lib_clean_spectrum_identity(
          lookup_table$spectrum_identity
        )
      }
      lookup_key <- lookup_spec$by
      key_merge_report <- data.table::data.table()
      if (is.null(lookup_key)) {
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
               ". Supply list(lookup = x, by = key) for an explicit join",
               call. = FALSE)
        }
        lookup_key <- auto_key$candidates
      }
      if (!is.null(lookup_spec$fallback_by)) {
        warning(
          "'fallback_by' is deprecated; standardize canonical metadata keys ",
          "before supplying external lookup tables",
          call. = FALSE
        )
        metadata_key <- if (is.null(names(lookup_key)) ||
                            all(names(lookup_key) == "")) {
          unname(lookup_key)
        } else {
          names(lookup_key)
        }
        if (length(metadata_key) != 1L) {
          stop("'fallback_by' requires a one-column explicit lookup key",
               call. = FALSE)
        }
        .lib_require_cols(lib$metadata,
                          c(metadata_key, lookup_spec$fallback_by),
                          "metadata")
        primary <- as.character(lib$metadata[[metadata_key]])
        fallback <- as.character(lib$metadata[[lookup_spec$fallback_by]])
        primary_blank <- is.na(primary) | !nzchar(trimws(primary))
        fallback_present <- !is.na(fallback) & nzchar(trimws(fallback))
        filled <- primary_blank & fallback_present
        primary[filled] <- fallback[filled]
        lib$metadata[[metadata_key]] <- primary
        key_merge_report <- data.table::data.table(
          problem = "fallback_metadata_key",
          column = metadata_key,
          value = lookup_spec$fallback_by,
          n = sum(filled)
        )
      }
      coalesce_cols <- intersect(
        names(lib$metadata),
        setdiff(names(lookup_table), unname(lookup_key))
      )
      lib <- join_lib_metadata(lib, lookup_table, by = lookup_key)
      join_report <- attr(lib, "join_report")
      report(sprintf(
        "metadata lookup %d/%d complete (matched=%d; unmatched=%d)",
        i, length(lookups),
        nrow(lib$metadata) - sum(join_report[
          problem == "unmatched_metadata_key", n
        ]),
        sum(join_report[problem == "unmatched_metadata_key", n])
      ))
      lookup_reports[[paste0("lookup_", i)]] <- data.table::rbindlist(
        list(key_merge_report, join_report), fill = TRUE
      )
      lib$metadata <- .lib_coalesce_joined_metadata(
        lib$metadata, coalesce_cols,
        lookup_precedence = !isTRUE(lookup_spec$fill_only)
      )
    }
  }

  if (!is.null(material_hierarchy)) {
    report("joining the material hierarchy")
    hierarchy <- lib_clean_metadata(.lib_read_lookup(material_hierarchy),
                                    metadata_name_lookup,
                                    clean_values = clean_metadata_values)
    lib <- join_material_hierarchy(lib, hierarchy)
    hierarchy_report <- attr(lib, "join_report")
    report(sprintf(
      "material hierarchy complete (unmatched=%d)",
      sum(hierarchy_report[problem == "unmatched_metadata_key", n])
    ))
  }

  if (!is.null(exclude_ids)) {
    before <- ncol(lib$spectra)
    lib <- .lib_filter_excluded(lib, exclude_ids, id_col = id_col)
    build_stage_report <- data.table::rbindlist(list(
      build_stage_report,
      data.table::data.table(
        stage = "excluded_identifiers", spectra = ncol(lib$spectra),
        removed = before - ncol(lib$spectra)
      )
    ))
    report(sprintf("removed %d excluded identifier(s)",
                   before - ncol(lib$spectra)))
  }

  if (dedupe) {
    before <- ncol(lib$spectra)
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
    build_stage_report <- data.table::rbindlist(list(
      build_stage_report,
      data.table::data.table(
        stage = "deduplicated", spectra = ncol(lib$spectra),
        removed = before - ncol(lib$spectra)
      )
    ))
    report(sprintf("deduplication complete (removed=%d; retained=%d)",
                   before - ncol(lib$spectra), ncol(lib$spectra)))
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

    if (!is.null(prune) && recipe_name %in% names(prune)) {
      report(sprintf("pruning library (%s)", recipe_name))
      prune_args <- prune[[recipe_name]]
      if (is.null(prune_args)) prune_args <- list()
      if (!is.list(prune_args) ||
          (!is.null(names(prune_args)) && any(names(prune_args) == ""))) {
        stop("Each selected 'prune' recipe must contain a named argument list",
             call. = FALSE)
      }
      prune_args$return <- "object"
      if (is.null(prune_args$progress)) prune_args$progress <- progress
      out <- do.call(prune_lib, c(list(out), prune_args))
    }

    if (length(lookup_reports) > 0L) {
      attr(out, "metadata_lookup_reports") <- lookup_reports
    }
    attr(out, "spectrum_identity_cleanup_report") <- identity_cleanup_report
    attr(out, "build_stage_report") <- build_stage_report

    out
  }

  if (is.null(names(recipes)) || any(names(recipes) == "") ||
      anyDuplicated(names(recipes))) {
    stop("'recipes' must be a uniquely named list", call. = FALSE)
  }
  if (!is.null(prune)) {
    if (!is.list(prune) || is.null(names(prune)) ||
        any(is.na(names(prune)) | names(prune) == "") ||
        anyDuplicated(names(prune))) {
      stop("'prune' must be NULL or a uniquely named list", call. = FALSE)
    }
    unknown_prune <- setdiff(names(prune), names(recipes))
    if (length(unknown_prune) > 0L) {
      stop("'prune' names must identify recipes: ",
           paste(unknown_prune, collapse = ", "), call. = FALSE)
    }
  }
  out <- lapply(seq_along(recipes), function(i) {
    apply_recipe(recipes[[i]], names(recipes)[[i]], i)
  })
  names(out) <- names(recipes)
  report("complete")
  out
}

.lib_clean_spectrum_identity <- function(x) {
  out <- trimws(as.character(x))
  missing <- is.na(out)
  windows_path <- grepl("^[A-Za-z]:[\\\\/]", out) |
    grepl("\\\\", out)
  unix_path <- grepl("^(?:/|\\./|\\.\\./)", out, perl = TRUE)
  out <- gsub("\\\\", "/", out)
  extensions <- .supported_spectrum_extensions()
  pattern <- paste0(
    "\\.(?:", paste(extensions, collapse = "|"), ")$"
  )
  suffixed_path <- grepl("/", out, fixed = TRUE) &
    grepl(pattern, out, ignore.case = TRUE, perl = TRUE)
  path <- windows_path | unix_path | suffixed_path
  out[path] <- sub(".*/", "", out[path])
  repeat {
    cleaned <- sub(pattern, "", out, ignore.case = TRUE, perl = TRUE)
    if (identical(cleaned, out)) break
    out <- cleaned
  }
  out <- trimws(out)
  out[missing | !nzchar(out)] <- NA_character_
  out
}

#' Predict blank class-reference values with reviewed regex rules
#'
#' Applies a separate regex reference only to rows whose `material` is blank.
#' Populated exact materials are authoritative and are never overwritten, even
#' when a regex also matches them. A blank row is filled only when all matching
#' patterns name one material. Distinct-material matches remain blank and are
#' reported as clashes. Exact/regex overlaps are allowed and reported for QA.
#' If present, `match_identity` is used for pattern matching while
#' `spectrum_identity` remains the reported exact identity.
#'
#' @param metadata A data.frame or data.table with `spectrum_identity` and
#'   `material` columns. Row order is preserved.
#' @param regex_reference A data.frame or data.table with unique `pattern` and
#'   nonblank `material` columns.
#' @param return Return the updated table or an audit containing `data`,
#'   `summary`, `predictions`, `clashes`, and `overlaps`.
#'
#' @return An updated data.table, or an audit list when `return = "report"`.
#' @export
predict_class_reference <- function(metadata, regex_reference,
                                    return = c("table", "report")) {
  return <- match.arg(return)
  out <- data.table::as.data.table(data.table::copy(metadata))
  rules <- data.table::as.data.table(data.table::copy(regex_reference))
  .lib_require_cols(out, c("spectrum_identity", "material"),
                    "metadata")
  .lib_require_cols(rules, c("pattern", "material"), "regex reference")
  out$spectrum_identity <- as.character(out$spectrum_identity)
  out$material <- as.character(out$material)
  rules$pattern <- as.character(rules$pattern)
  rules$material <- as.character(rules$material)
  blank <- function(value) {
    is.na(value) | !nzchar(trimws(as.character(value)))
  }
  if (any(blank(rules$pattern)) || any(blank(rules$material))) {
    stop("Regex-reference patterns and materials must be nonblank",
         call. = FALSE)
  }
  if (anyDuplicated(rules$pattern)) {
    stop("Regex-reference patterns must be unique", call. = FALSE)
  }

  match_value <- if ("match_identity" %in% names(out)) {
    value <- as.character(out$match_identity)
    value[blank(value)] <- out$spectrum_identity[blank(value)]
    value
  } else {
    out$spectrum_identity
  }
  missing_identity <- blank(match_value)
  match_value[missing_identity] <- ""
  hits <- matrix(FALSE, nrow = nrow(out), ncol = nrow(rules))
  if (nrow(rules) > 0L && nrow(out) > 0L) {
    hits <- vapply(seq_len(nrow(rules)), function(i) {
      tryCatch(
        grepl(rules$pattern[[i]], match_value, perl = TRUE),
        warning = function(w) {
          stop("Invalid class regex '", rules$pattern[[i]], "': ",
               conditionMessage(w), call. = FALSE)
        },
        error = function(e) {
          stop("Invalid class regex '", rules$pattern[[i]], "': ",
               conditionMessage(e), call. = FALSE)
        }
      )
    }, logical(nrow(out)))
    if (is.null(dim(hits))) hits <- matrix(hits, ncol = 1L)
  }
  if (any(missing_identity) && ncol(hits) > 0L) {
    hits[missing_identity, ] <- FALSE
  }

  matched_materials <- lapply(seq_len(nrow(out)), function(i) {
    unique(rules$material[which(hits[i, ])])
  })
  matched_patterns <- lapply(seq_len(nrow(out)), function(i) {
    rules$pattern[which(hits[i, ])]
  })
  blank_material <- blank(out$material)
  unique_match <- lengths(matched_materials) == 1L
  clash <- lengths(matched_materials) > 1L
  predict <- blank_material & unique_match
  out$material[predict] <- vapply(
    matched_materials[predict], `[[`, character(1), 1L
  )

  prediction_rows <- which(predict)
  predictions <- data.table::data.table(
    spectrum_identity = out$spectrum_identity[prediction_rows],
    match_identity = match_value[prediction_rows],
    material = out$material[prediction_rows],
    patterns = vapply(matched_patterns[prediction_rows], paste,
                      character(1), collapse = "; ")
  )[, .(n = .N), by = .(spectrum_identity, match_identity, material, patterns)]

  clash_rows <- which(blank_material & clash)
  clashes <- if (length(clash_rows) == 0L) {
    data.table::data.table(
      spectrum_identity = character(), match_identity = character(),
      materials = character(), patterns = character(), n = integer()
    )
  } else {
    data.table::data.table(
      spectrum_identity = out$spectrum_identity[clash_rows],
      match_identity = match_value[clash_rows],
      materials = vapply(matched_materials[clash_rows], paste,
                         character(1), collapse = "; "),
      patterns = vapply(matched_patterns[clash_rows], paste,
                        character(1), collapse = "; ")
    )[, .(n = .N),
      by = .(spectrum_identity, match_identity, materials, patterns)]
  }

  overlap_rows <- which(!blank_material & lengths(matched_materials) > 0L)
  overlaps <- data.table::data.table(
    spectrum_identity = out$spectrum_identity[overlap_rows],
    exact_material = out$material[overlap_rows],
    regex_materials = vapply(matched_materials[overlap_rows], paste,
                             character(1), collapse = "; "),
    patterns = vapply(matched_patterns[overlap_rows], paste,
                      character(1), collapse = "; ")
  )[, agreement := vapply(seq_len(.N), function(i) {
    exact_material[[i]] %in% strsplit(regex_materials[[i]], "; ", fixed = TRUE)[[1L]]
  }, logical(1))][, .(n = .N),
                 by = .(spectrum_identity, exact_material, regex_materials,
                        patterns, agreement)]

  summary <- data.table::data.table(
    rows = nrow(out), regex_rules = nrow(rules),
    existing = sum(!blank_material), predicted = sum(predict),
    clashes = length(clash_rows), overlaps = length(overlap_rows),
    unmatched = sum(blank(out$material))
  )
  if (return == "report") {
    return(list(data = out, summary = summary, predictions = predictions,
                clashes = clashes, overlaps = overlaps))
  }
  out
}

# Complete official reference-library class coverage while retaining uncertain
# identities as an explicit review queue. This stays internal because the two
# normalization rules are contributor-specific, not a general package API.
.lib_complete_reference_classes <- function(x, classes, hierarchy) {
  metadata <- data.table::copy(x$metadata)
  blank <- function(value) {
    value <- as.character(value)
    is.na(value) | !nzchar(trimws(value))
  }
  missing_before <- blank(metadata$material_class)
  identity <- as.character(metadata$spectrum_identity)
  user <- as.character(metadata$user_name)
  lookup_key <- identity
  assignment <- ifelse(missing_before, NA_character_, "existing_or_exact_lookup")
  exact_classes <- classes

  normalized_key <- rep(NA_character_, nrow(metadata))
  gicquel <- missing_before & !is.na(user) &
    user == "gicquel et al. 2024" & !is.na(identity)
  normalized_key[gicquel] <- sub(
    "_ref(?:_0)?(?:[.]csv)?$", "", identity[gicquel], perl = TRUE
  )
  mffrc <- missing_before & !is.na(user) &
    user == "elise granek and kellie teague" & !is.na(identity)
  mffrc_base <- sub("[.][0-9]+$", "", identity[mffrc])
  mffrc_base <- sub("^mffrc[0-9]+_", "", mffrc_base)
  mffrc_base <- sub("_.*$", "", mffrc_base)
  mffrc_family <- trimws(sub("[[:space:]]*[(].*$", "", mffrc_base))
  mffrc_key <- mffrc_base
  use_family <- is.na(match(mffrc_key, exact_classes$spectrum_identity)) &
    !is.na(match(mffrc_family, exact_classes$spectrum_identity))
  mffrc_key[use_family] <- mffrc_family[use_family]
  normalized_key[mffrc] <- mffrc_key

  normalized_material <- exact_classes$material[
    match(normalized_key, exact_classes$spectrum_identity)
  ]
  resolved <- missing_before & !blank(normalized_material)
  lookup_key[resolved] <- normalized_key[resolved]
  metadata$material[resolved] <- normalized_material[resolved]

  hierarchy_material <- match(metadata$material[resolved], hierarchy$material)
  resolved_rows <- which(resolved)
  material_rows <- resolved_rows[!is.na(hierarchy_material)]
  material_lookup <- hierarchy_material[!is.na(hierarchy_material)]
  if (length(material_rows) > 0L) {
    metadata$material_class[material_rows] <- hierarchy$material_class[material_lookup]
    metadata$material_type[material_rows] <- hierarchy$material_type[material_lookup]
  }
  unresolved_rows <- resolved_rows[is.na(hierarchy_material)]
  if (length(unresolved_rows) > 0L) {
    class_pairs <- unique(hierarchy[, .(material_class, material_type)])
    class_lookup <- match(metadata$material[unresolved_rows],
                          class_pairs$material_class)
    class_rows <- unresolved_rows[!is.na(class_lookup)]
    metadata$material_class[class_rows] <- metadata$material[class_rows]
    metadata$material_type[class_rows] <-
      class_pairs$material_type[class_lookup[!is.na(class_lookup)]]
  }
  source_resolved <- resolved & !blank(metadata$material_class)
  assignment[source_resolved] <- "reviewed_source_key"
  unresolved <- blank(metadata$material_class)
  metadata$material[unresolved & blank(metadata$material)] <- "unclassified"
  metadata$material_class[unresolved] <- "unclassified"
  metadata$material_type[unresolved & blank(metadata$material_type)] <- "unknown"
  lookup_key[unresolved] <- identity[unresolved]
  assignment[unresolved] <- "unresolved_identity"
  metadata[, `:=`(class_lookup_key = lookup_key,
                  class_assignment_reason = assignment)]
  report <- data.table::data.table(
    stage = c("before", "after"),
    populated_class = c(sum(!missing_before), sum(!blank(metadata$material_class))),
    reviewed_source_key = c(0L, sum(source_resolved)),
    unclassified = c(0L, sum(unresolved)), total = nrow(metadata)
  )
  stopifnot(report[stage == "after", populated_class] == nrow(metadata))
  x$metadata <- metadata
  attr(x, "class_coverage_report") <- report
  x
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
  if (!any(keep)) {
    ids <- lapply(seq_len(ncol(spectra)), function(i) {
      valid <- !ignored_info$ignored[, i]
      current <- .lib_hash_processed_matrix(
        wavenumber[valid], spectra[valid, i, drop = FALSE],
        range = NULL, res = 8, scale = scale, algo = algo
      )
      old <- .lib_hash_processed_matrix(
        wavenumber[valid], spectra[valid, i, drop = FALSE],
        range = c(100, 4000), res = 8, scale = scale, algo = algo,
        min_rows = 3L, short_value = "new format"
      )
      list(current = current[[1L]], old = old[[1L]])
    })
    return(list(
      current = vapply(ids, `[[`, character(1), "current"),
      old = vapply(ids, `[[`, character(1), "old")
    ))
  }
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
          spectra[, idx, drop = FALSE], na.rm = TRUE
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
    spectrum_identity = c("substance", "interpretation"),
    spectrum_type = character(),
    material_form = c("description", "form_factor", "shape",
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
    data_type = c("datatype"),
    wavenumber_units = c("xunits", "x_unit"),
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
    spectrum_id = c("spectrumid"),
    location_description = c("locationdescription"),
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

.lib_fill_metadata_key <- function(metadata, canonical, fallback) {
  if (!inherits(metadata, "data.table")) data.table::setDT(metadata)
  if (!fallback %in% names(metadata)) return(data.table::data.table())
  if (!canonical %in% names(metadata)) metadata[[canonical]] <- NA_character_

  primary <- trimws(as.character(metadata[[canonical]]))
  alternate <- trimws(as.character(metadata[[fallback]]))
  primary_present <- !is.na(primary) & nzchar(primary)
  alternate_present <- !is.na(alternate) & nzchar(alternate)
  conflicts <- primary_present & alternate_present & primary != alternate
  filled <- !primary_present & alternate_present
  primary[filled] <- alternate[filled]
  data.table::set(metadata, j = canonical, value = primary)

  data.table::rbindlist(list(
    data.table::data.table(
      problem = "filled_canonical_key", column = canonical,
      value = fallback, n = sum(filled)
    )[n > 0L],
    data.table::data.table(
      problem = "canonical_key_conflict", column = canonical,
      value = fallback, n = sum(conflicts)
    )[n > 0L]
  ), fill = TRUE)
}

#' @rdname build_lib
#' @export
make_lib_lookup_template <- function(x, columns, add = NULL, path = NULL) {
  if (inherits(x, "FileSpecs"))
    .filespec_stop_unsupported("make_lib_lookup_template()")
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
  if (inherits(x, "FileSpecs"))
    .filespec_stop_unsupported("join_lib_metadata()")
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
  if (inherits(x, "FileSpecs"))
    .filespec_stop_unsupported("join_material_hierarchy()")
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
prune_lib <- function(x, class_col = "material_class",
                      type_col = "spectrum_type",
                      material_type_col = "material_type",
                      id_col = "sample_name", min_n = 10,
                      exclude = c(2200, 2420),
                      return = c("object", "ids", "report"),
                      progress = TRUE) {
  return <- match.arg(return)
  x <- as_OpenSpecy(x)
  .lib_require_cols(x$metadata, c(class_col, type_col, material_type_col),
                    "metadata")
  if (length(min_n) != 1L || is.na(min_n) || min_n < 1 || min_n %% 1 != 0) {
    stop("'min_n' must be one positive whole number", call. = FALSE)
  }
  if (!is.numeric(exclude) || length(exclude) != 2L || anyNA(exclude)) {
    stop("'exclude' must be two finite wavenumbers", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE", call. = FALSE)
  }

  metadata <- data.table::copy(x$metadata)
  ids <- .lib_ids(x, id_col)
  classes <- trimws(as.character(metadata[[class_col]]))
  material_types <- trimws(tolower(as.character(
    metadata[[material_type_col]]
  )))
  pools <- .lib_prune_pools(metadata[[type_col]])
  normalized <- .lib_prune_normalize(x$spectra, x$wavenumber, exclude)

  reassigned <- .lib_reassign_other_classes(
    classes, material_types, pools, normalized, ids
  )
  classes <- reassigned$classes
  metadata[[class_col]] <- classes
  protected <- tolower(classes) %in% "unclassified"

  schedule <- data.table::data.table(
    pool = pools,
    material_class = classes,
    is_protected = protected
  )[!is.na(pool) & !is.na(material_class) & nzchar(material_class),
    .(initial_n = .N, is_protected = all(is_protected)),
    by = .(pool, material_class)][is_protected == FALSE]
  if (nrow(schedule) > 0L) {
    data.table::setorder(schedule, pool, -initial_n, material_class)
    schedule[, schedule_order := seq_len(.N), by = pool]
  } else {
    schedule[, schedule_order := integer()]
  }

  active <- rep(TRUE, length(ids))
  removal_rows <- list()
  removal_i <- 0L
  if (isTRUE(progress)) {
    message(sprintf(
      "prune_lib: starting %d scheduled class(es) across %d spectrum pool(s)",
      nrow(schedule), length(unique(stats::na.omit(pools)))
    ))
  }
  if (nrow(schedule) > 0L) {
    for (s in seq_len(nrow(schedule))) {
      target_pool <- schedule$pool[[s]]
      target_class <- schedule$material_class[[s]]
      if (isTRUE(progress)) {
        message(sprintf(
          "prune_lib: %s / %s (%d initially)",
          target_pool, target_class, schedule$initial_n[[s]]
        ))
      }
      repeat {
        target <- which(active & pools == target_pool &
                          !is.na(classes) & classes == target_class)
        if (length(target) <= min_n) break
        candidates <- which(active & pools == target_pool & !protected)
        best <- .lib_prune_best_match(
          target, candidates, normalized, ids, exclude_self = TRUE
        )
        conflicts <- which(
          !is.na(best$index) & is.finite(best$correlation) &
            classes[best$index] != target_class
        )
        conflicts <- conflicts[!is.na(conflicts)]
        if (length(conflicts) == 0L) break
        removable_n <- min(length(conflicts), length(target) - min_n)
        if (removable_n < 1L) break
        conflict_rows <- data.table::data.table(
          query = target[conflicts],
          matched = best$index[conflicts],
          correlation = best$correlation[conflicts]
        )
        conflict_rows[, query_id := ids[query]]
        data.table::setorder(conflict_rows, -correlation, query_id)
        conflict_rows <- conflict_rows[seq_len(removable_n)]
        active[conflict_rows$query] <- FALSE
        removal_i <- removal_i + 1L
        removal_rows[[removal_i]] <- conflict_rows[, .(
          spectrum_id = ids[query],
          prior_class = classes[query],
          matched_id = ids[matched],
          matched_class = classes[matched],
          correlation,
          pool = target_pool,
          schedule_order = schedule$schedule_order[[s]],
          reason = "top_match_other_class"
        )]
      }
      if (isTRUE(progress)) {
        retained <- sum(active & pools == target_pool &
                          !is.na(classes) & classes == target_class)
        message(sprintf(
          "prune_lib: %s / %s complete (retained=%d; removed=%d)",
          target_pool, target_class, retained,
          schedule$initial_n[[s]] - retained
        ))
      }
    }
  }

  removals <- if (length(removal_rows) > 0L) {
    data.table::rbindlist(removal_rows, fill = TRUE)
  } else {
    data.table::data.table(
      spectrum_id = character(), prior_class = character(),
      matched_id = character(), matched_class = character(),
      correlation = numeric(), pool = character(), schedule_order = integer(),
      reason = character()
    )
  }
  retained_ids <- ids[active]
  out <- x
  out$metadata <- metadata
  if (!all(active)) out <- filter_spec(out, active)
  audit <- list(
    retained_ids = retained_ids,
    schedule = schedule,
    reassignments = reassigned$report,
    removals = removals,
    summary = data.table::data.table(
      before = length(ids),
      after = sum(active),
      reassigned = nrow(reassigned$report),
      removed = sum(!active)
    )
  )
  attr(out, "prune_report") <- audit
  if (isTRUE(progress)) {
    message(sprintf(
      "prune_lib: complete (before=%d; after=%d; reassigned=%d; removed=%d)",
      length(ids), sum(active), nrow(reassigned$report), sum(!active)
    ))
  }
  if (return == "ids") return(retained_ids)
  if (return == "report") return(c(list(object = out), audit))
  out
}

.lib_prune_pools <- function(type) {
  type <- trimws(tolower(as.character(type)))
  out <- ifelse(type %in% c("ftir", "nir"), "ftir_nir",
                ifelse(type == "raman", "raman", type))
  out[is.na(type) | !nzchar(type)] <- NA_character_
  out
}

.lib_prune_normalize <- function(spectra, wavenumber, exclude) {
  limits <- sort(exclude)
  use <- wavenumber < limits[[1L]] | wavenumber > limits[[2L]]
  if (!any(use)) {
    stop("'exclude' removes every wavenumber", call. = FALSE)
  }
  # Correlation normalization is invariant to the preceding per-spectrum
  # min/max transform. Work spectrum-by-wavenumber directly to avoid two
  # additional full-library matrices during large builds.
  values <- t(spectra[use, , drop = FALSE])
  means <- rowMeans(values, na.rm = TRUE)
  row_block <- 256L
  blocks <- split(seq_len(nrow(values)),
                  ceiling(seq_len(nrow(values)) / row_block))
  for (block in blocks) {
    part <- values[block, , drop = FALSE]
    missing <- is.na(part)
    if (any(missing)) {
      idx <- which(missing, arr.ind = TRUE)
      part[idx] <- means[block][idx[, "row"]]
    }
    part <- part - means[block]
    values[block, ] <- part
  }
  norms <- numeric(nrow(values))
  for (block in blocks) {
    part <- values[block, , drop = FALSE]
    part[!is.finite(part)] <- 0
    norms[block] <- sqrt(rowSums(part * part))
    values[block, ] <- part
  }
  valid <- is.finite(norms) & norms > 0
  for (block in blocks) {
    block <- block[valid[block]]
    if (length(block) > 0L) {
      values[block, ] <- values[block, , drop = FALSE] / norms[block]
    }
  }
  values[!valid, ] <- NA_real_
  values
}

.lib_prune_best_match <- function(query, candidates, normalized, ids,
                                  exclude_self = FALSE, block_size = 64L) {
  candidate_order <- order(ids[candidates], candidates, na.last = TRUE)
  candidates <- candidates[candidate_order]
  best_index <- rep(NA_integer_, length(query))
  best_correlation <- rep(NA_real_, length(query))
  blocks <- split(seq_along(query),
                  ceiling(seq_along(query) / as.integer(block_size)))
  for (block in blocks) {
    # Multiply against the resident normalized matrix and subset the small
    # correlation block, rather than copying the full candidate library for
    # every query block.
    cors <- tcrossprod(normalized[query[block], , drop = FALSE], normalized)
    cors <- cors[, candidates, drop = FALSE]
    cors[!is.finite(cors)] <- -Inf
    if (isTRUE(exclude_self)) {
      self_col <- match(query[block], candidates)
      has_self <- !is.na(self_col)
      cors[cbind(which(has_self), self_col[has_self])] <- -Inf
    }
    local <- max.col(cors, ties.method = "first")
    scores <- cors[cbind(seq_along(block), local)]
    ok <- is.finite(scores)
    best_index[block[ok]] <- candidates[local[ok]]
    best_correlation[block[ok]] <- scores[ok]
  }
  list(index = best_index, correlation = best_correlation)
}

.lib_reassign_other_classes <- function(classes, material_types, pools,
                                        normalized, ids) {
  class_keys <- tolower(classes)
  generic <- which(class_keys %in% c("other plastic", "other material"))
  rows <- list()
  for (i in generic) {
    plastic <- identical(class_keys[[i]], "other plastic")
    eligible <- !is.na(pools) & pools == pools[[i]] &
      !class_keys %in% c("other plastic", "other material", "unclassified") &
      !is.na(classes) & nzchar(classes)
    if (plastic) {
      eligible <- eligible & material_types == "plastic"
    } else {
      eligible <- eligible & !is.na(material_types) &
        material_types != "plastic"
    }
    candidates <- which(eligible)
    if (length(candidates) == 0L) next
    best <- .lib_prune_best_match(i, candidates, normalized, ids)
    if (is.na(best$index[[1L]])) next
    old <- classes[[i]]
    classes[[i]] <- classes[[best$index[[1L]]]]
    rows[[length(rows) + 1L]] <- data.table::data.table(
      spectrum_id = ids[[i]], prior_class = old,
      material_class = classes[[i]],
      matched_id = ids[[best$index[[1L]]]],
      correlation = best$correlation[[1L]], pool = pools[[i]],
      reason = "nearest_eligible_class"
    )
  }
  report <- if (length(rows) > 0L) {
    data.table::rbindlist(rows, fill = TRUE)
  } else {
    data.table::data.table(
      spectrum_id = character(), prior_class = character(),
      material_class = character(), matched_id = character(),
      correlation = numeric(), pool = character(), reason = character()
    )
  }
  list(classes = classes, report = report)
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
  metadata <- metadata[keep, ]
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
  tests <- merge(pred, actual, by.x = "x", by.y = "row_id", all.x = TRUE)
  tests <- merge(tests, dimension_conversion, by.x = "y",
                 by.y = "factor_num", all.x = TRUE)
  names(tests)[names(tests) == "name"] <- "predicted_class"
  ids <- if ("sample_name" %in% names(metadata)) {
    as.character(metadata$sample_name)
  } else {
    rownames(train)
  }
  if (is.null(ids)) ids <- paste0("spectrum_", seq_len(nrow(train)))
  technique <- if (!is.null(type_col) && type_col %in% names(metadata)) {
    as.character(metadata[[type_col]])
  } else {
    NA_character_
  }
  tests[, `:=`(
    spectrum_id = ids[x],
    technique = technique[x],
    expected_class = actual_name,
    correct = actual_name == predicted_class,
    score = value,
    split = "training",
    provenance = "model_fit"
  )]
  tests <- tests[, .(
    spectrum_id, technique, expected_class, predicted_class, correct, score,
    split, provenance
  )]

  list(
    model = model,
    dimension_conversion = dimension_conversion,
    tests = tests,
    coefficients = coefficients_join,
    class_names = unique(labels),
    class_num = length(unique(outcome)),
    observation_count = length(labels),
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

.lib_reference_sources <- function(source_file, processed_dir,
                                   progress = TRUE) {
  processed <- character()
  if (!is.null(processed_dir) && dir.exists(processed_dir)) {
    processed <- list.files(
      processed_dir, pattern = "[.]rds$", recursive = TRUE,
      full.names = TRUE, ignore.case = TRUE
    )
    processed <- processed[grepl(
      "[/\\\\]Processed[/\\\\]", processed, ignore.case = TRUE
    )]
  }
  source <- if (!is.null(source_file) && file.exists(source_file)) {
    source_file
  } else {
    character()
  }
  files <- unique(c(processed, source))
  if (length(files) == 0L) {
    stop(
      "No official reference-library sources were found. Set ",
      "OPENSPECY_SOURCE_FILE and/or OPENSPECY_PROCESSED_DIR, or supply 'x'",
      call. = FALSE
    )
  }
  if (isTRUE(progress)) {
    message(sprintf(
      "build_lib: discovered %d processed source(s) plus %d raw source(s)",
      length(processed), length(source)
    ))
  }
  files
}

.lib_build_reference <- function(x, recipes, range, res, id_col, exclude_ids,
                                 dedupe, metadata_lookups,
                                 material_hierarchy, metadata_name_lookup,
                                 clean_metadata_values, convert_intensity,
                                 restrict_range_args, signal_noise, assess,
                                 prune, progress, workflow_data, output_dir,
                                 previous_library_dir, reuse, seed, holdout,
                                 ...) {
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must be one nonempty path in end-to-end mode",
         call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop("'seed' must be one finite number", call. = FALSE)
  }
  if (!is.numeric(holdout) || length(holdout) != 1L || is.na(holdout) ||
      holdout <= 0 || holdout >= 1) {
    stop("'holdout' must be between zero and one", call. = FALSE)
  }

  started <- proc.time()[["elapsed"]]
  report <- function(stage) {
    if (isTRUE(progress)) {
      message(sprintf(
        "build_lib [%.1fs]: %s",
        proc.time()[["elapsed"]] - started, stage
      ))
    }
  }
  report("resolving official lookup and exclusion tables")
  tables <- .lib_reference_tables(workflow_data)
  .lib_validate_reference_regex(tables$classes_regex)

  classes_exact <- tables$classes_reference[
    !is.na(material) & nzchar(material), .(spectrum_identity, material)
  ]
  type_lookup <- data.table::copy(tables$library_types)
  if ("spectrum_type" %in% names(type_lookup)) {
    type_lookup[grepl(";", spectrum_type, fixed = TRUE),
                spectrum_type := NA_character_]
  }
  if (is.null(metadata_lookups)) {
    metadata_lookups <- list(
      list(lookup = classes_exact, by = "spectrum_identity"),
      list(lookup = type_lookup, by = "organization", fill_only = TRUE)
    )
  }
  if (is.null(material_hierarchy)) {
    material_hierarchy <- tables$material_hierarchy
  }
  if (is.null(exclude_ids)) exclude_ids <- tables$known_bad_ids$sample_name

  signature <- .lib_build_signature(
    x,
    workflow_paths = unlist(tables$paths, use.names = FALSE),
    arguments = list(
      recipes = recipes, range = range, res = res, id_col = id_col,
      exclude_ids = exclude_ids, dedupe = dedupe,
      clean_metadata_values = clean_metadata_values,
      convert_intensity = convert_intensity,
      restrict_range_args = restrict_range_args,
      signal_noise = signal_noise, assess = assess, prune = prune,
      seed = seed, holdout = holdout
    )
  )
  checkpoints <- .lib_checkpoint_manager(
    output_dir, signature = signature, reuse = reuse, report = report
  )

  core <- checkpoints$get("core_libraries")
  if (is.null(core)) {
    report("building raw, derivative, and nobaseline libraries")
    core <- .lib_build_core(
      x = x, recipes = recipes, range = range, res = res, id_col = id_col,
      exclude_ids = exclude_ids, dedupe = dedupe,
      metadata_lookups = metadata_lookups,
      material_hierarchy = material_hierarchy,
      metadata_name_lookup = metadata_name_lookup,
      clean_metadata_values = clean_metadata_values,
      convert_intensity = convert_intensity,
      restrict_range_args = restrict_range_args,
      signal_noise = signal_noise, assess = assess, prune = NULL,
      progress = progress, ...
    )
    checkpoints$put("core_libraries", core)
    for (name in names(core)) {
      checkpoints$put(paste0("core_library_", name), core[[name]])
    }
  }

  libraries <- checkpoints$get("libraries")
  local_assessments <- NULL
  if (is.null(libraries)) {
    completed <- .lib_complete_reference_build(
      core, tables = tables, prune = prune, progress = progress,
      report = report
    )
    libraries <- completed$libraries
    local_assessments <- completed$assessments
    checkpoints$put("libraries", libraries)
    for (name in names(libraries)) {
      checkpoints$put(paste0("library_", name), libraries[[name]])
    }
  }

  medoids <- checkpoints$get("medoids")
  if (is.null(medoids)) {
    medoids <- .lib_build_medoids(
      libraries, report = report, checkpoints = checkpoints
    )
    checkpoints$put("medoids", medoids)
  }

  models <- checkpoints$get("models")
  model_warnings <- data.table::data.table()
  if (is.null(models)) {
    model_result <- .lib_build_models(
      medoids, report = report, checkpoints = checkpoints
    )
    models <- model_result$models
    model_warnings <- model_result$warnings
    checkpoints$put("models", models)
  }

  build <- list(
    libraries = libraries,
    medoids = medoids,
    models = models,
    assessments = .lib_local_build_assessments(
      libraries, medoids, models, local_assessments, model_warnings
    )
  )

  prior_signature <- .lib_previous_signature(previous_library_dir)
  assessment_key <- digest::digest(
    list(signature, prior_signature, seed = seed, holdout = holdout),
    algo = "sha256"
  )
  cached_assessments <- checkpoints$get(
    "assessments", key = assessment_key
  )
  if (!is.null(cached_assessments)) {
    build$assessments <- cached_assessments
    validated_models <- checkpoints$get(
      "validated_models", key = assessment_key
    )
    if (!is.null(validated_models)) build$models <- validated_models
  } else if (!is.null(previous_library_dir)) {
    report("assessing complete candidate and legacy artifacts")
    comparison <- .lib_compare_reference_build(
      build, previous_library_dir = previous_library_dir,
      seed = seed, holdout = holdout, progress = progress,
      checkpoints = checkpoints, checkpoint_key = assessment_key
    )
    if (!is.null(comparison$models)) {
      build$models <- comparison$models
      checkpoints$put(
        "validated_models", build$models, key = assessment_key
      )
      comparison$models <- NULL
    }
    build$assessments[names(comparison)] <- comparison
  }
  build$assessments$output_manifest <- checkpoints$manifest()
  checkpoints$put("assessments", build$assessments, key = assessment_key)
  checkpoints$put("reference_library_build", build, key = assessment_key)

  report("promoting validated artifacts to a versioned release directory")
  release_dir <- .lib_promote_reference_build(
    build, output_dir = output_dir,
    signature = assessment_key, reuse = reuse
  )
  build$assessments$output_manifest <- data.table::rbindlist(list(
    checkpoints$manifest(),
    data.table::data.table(
      component = "release", status = "promoted", path = release_dir,
      signature = assessment_key
    )
  ), fill = TRUE)
  attr(build, "output_dir") <- normalizePath(release_dir, mustWork = FALSE)
  attr(build, "build_signature") <- signature
  .lib_atomic_saveRDS(
    build, file.path(release_dir, "reference_library_build.rds")
  )
  report("complete")
  build
}

.lib_reference_tables <- function(workflow_data) {
  if (!is.character(workflow_data) || length(workflow_data) != 1L ||
      is.na(workflow_data) || !nzchar(workflow_data)) {
    stop("'workflow_data' must be one nonempty directory", call. = FALSE)
  }
  files <- c(
    classes_reference = "classes_reference.csv",
    classes_regex = "classes_regex.csv",
    library_types = "library_types.csv",
    material_hierarchy = "material_hierarchy.csv",
    known_bad_ids = "known_bad_ids.csv",
    metadata_drop = "metadata_drop_columns.csv"
  )
  paths <- stats::setNames(file.path(workflow_data, unname(files)), names(files))
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop("Missing workflow data file(s): ",
         paste(paths[missing], collapse = ", "), call. = FALSE)
  }
  out <- lapply(paths, data.table::fread)
  out$paths <- as.list(paths)
  out
}

.lib_validate_reference_regex <- function(regex_reference) {
  .lib_require_cols(regex_reference, c("pattern", "material"),
                    "classes_regex")
  literal_only <- vapply(
    regex_reference$pattern, .lib_regex_is_exact_literal, logical(1)
  )
  if (any(literal_only)) {
    stop(
      "Literal-only anchored class patterns belong in classes_reference.csv: ",
      paste(regex_reference$pattern[literal_only], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.lib_regex_is_exact_literal <- function(pattern) {
  if (is.na(pattern) || !startsWith(pattern, "^") ||
      !endsWith(pattern, "$")) return(FALSE)
  body <- substr(pattern, 2L, nchar(pattern) - 1L)
  # Escaped punctuation still denotes one exact normalized identity; every
  # other regex metacharacter denotes controlled variability.
  body <- gsub("\\\\[().+*?{}|^$\\[\\]\\\\]", "", body, perl = TRUE)
  !grepl("[.()+*?{}|\\[\\]]|\\[:|\\d|\\s|\\w|\\x", body, perl = TRUE)
}

.lib_build_signature <- function(x, workflow_paths, arguments) {
  sources <- if (is.character(x)) {
    .lib_file_signatures(x)
  } else {
    data.table::data.table(
      path = "<in-memory>", size = as.numeric(object.size(x)),
      modified = NA_character_, checksum = digest::digest(x, algo = "sha256")
    )
  }
  workflow <- .lib_file_signatures(workflow_paths, checksum_limit = Inf)
  description <- if (file.exists("DESCRIPTION")) {
    read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  } else {
    NA_character_
  }
  code_checksum <- if (file.exists(file.path("R", "build_lib.R"))) {
    unname(tools::md5sum(file.path("R", "build_lib.R")))
  } else {
    NA_character_
  }
  digest::digest(
    list(sources = sources, workflow = workflow, arguments = arguments,
         version = description, code = code_checksum),
    algo = "sha256"
  )
}

.lib_file_signatures <- function(paths, checksum_limit = 50 * 1024^2) {
  paths <- unique(as.character(paths))
  info <- file.info(paths)
  checksum <- rep(NA_character_, length(paths))
  use <- !is.na(info$size) & info$size <= checksum_limit &
    !is.na(info$isdir) & !info$isdir
  if (any(use)) checksum[use] <- unname(tools::md5sum(paths[use]))
  data.table::data.table(
    path = normalizePath(paths, mustWork = FALSE),
    size = as.numeric(info$size),
    modified = as.character(info$mtime), checksum = checksum
  )
}

.lib_checkpoint_manager <- function(output_dir, signature, reuse, report) {
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  events <- list()
  event_i <- 0L
  record <- function(component, status, path, key) {
    event_i <<- event_i + 1L
    events[[event_i]] <<- data.table::data.table(
      component = component, status = status,
      path = normalizePath(path, mustWork = FALSE), signature = key
    )
  }
  paths <- function(stage) {
    safe <- gsub("[^A-Za-z0-9_.-]+", "_", stage)
    list(
      object = file.path(checkpoint_dir, paste0(safe, ".rds")),
      manifest = file.path(checkpoint_dir, paste0(safe, ".manifest.rds"))
    )
  }
  get <- function(stage, key = signature) {
    target <- paths(stage)
    if (!isTRUE(reuse) || !file.exists(target$object) ||
        !file.exists(target$manifest)) return(NULL)
    manifest <- tryCatch(readRDS(target$manifest), error = function(e) NULL)
    if (!is.list(manifest) || !identical(manifest$signature, key) ||
        !isTRUE(manifest$complete)) {
      record(stage, "invalidated", target$object, key)
      return(NULL)
    }
    object <- tryCatch(readRDS(target$object), error = function(e) NULL)
    if (is.null(object)) {
      record(stage, "unreadable", target$object, key)
      return(NULL)
    }
    report(paste0("reusing checkpoint: ", stage))
    record(stage, "reused", target$object, key)
    object
  }
  put <- function(stage, object, key = signature) {
    target <- paths(stage)
    .lib_atomic_saveRDS(object, target$object)
    .lib_atomic_saveRDS(
      list(signature = key, complete = TRUE, saved_at = Sys.time()),
      target$manifest
    )
    record(stage, "built", target$object, key)
    invisible(object)
  }
  manifest <- function() {
    if (length(events) == 0L) {
      return(data.table::data.table(
        component = character(), status = character(), path = character(),
        signature = character()
      ))
    }
    data.table::rbindlist(events, fill = TRUE)
  }
  list(get = get, put = put, manifest = manifest)
}

.lib_atomic_saveRDS <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."),
                        tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(object, temporary)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not promote completed component to ", path, call. = FALSE)
  }
  invisible(path)
}

.lib_complete_reference_build <- function(libraries, tables, prune, progress,
                                          report) {
  prediction_rows <- list()
  coverage_rows <- list()
  for (name in names(libraries)) {
    report(paste0("completing class metadata (", name, ")"))
    prediction <- predict_class_reference(
      libraries[[name]]$metadata, tables$classes_regex, return = "report"
    )
    if (nrow(prediction$clashes) > 0L) {
      stop("Class regex clashes require exact material entries: ",
           paste(head(prediction$clashes$spectrum_identity, 20L),
                 collapse = ", "), call. = FALSE)
    }
    libraries[[name]]$metadata <- prediction$data
    libraries[[name]] <- join_material_hierarchy(
      libraries[[name]], tables$material_hierarchy
    )
    attr(libraries[[name]], "class_prediction_report") <- prediction[
      c("summary", "predictions", "clashes", "overlaps")
    ]
    libraries[[name]] <- .lib_complete_reference_classes(
      libraries[[name]], classes = tables$classes_reference,
      hierarchy = tables$material_hierarchy
    )
    prediction_rows[[name]] <- data.table::data.table(
      artifact = name, metric = names(prediction$summary),
      value = as.character(unlist(prediction$summary, use.names = FALSE))
    )
    coverage <- attr(libraries[[name]], "class_coverage_report")
    coverage_rows[[name]] <- data.table::data.table(
      artifact = name, stage = coverage$stage,
      populated_class = coverage$populated_class,
      reviewed_source_key = coverage$reviewed_source_key,
      unclassified = coverage$unclassified
    )
  }

  type_coverage <- data.table::rbindlist(lapply(names(libraries), function(name) {
    metadata <- libraries[[name]]$metadata
    data.table::data.table(
      artifact = name, field = c("library_type", "spectrum_type"),
      populated = c(
        sum(!is.na(metadata$library_type) & nzchar(metadata$library_type)),
        sum(!is.na(metadata$spectrum_type) & nzchar(metadata$spectrum_type))
      ), total = nrow(metadata)
    )
  }))
  if (any(type_coverage$populated != type_coverage$total)) {
    stop("Blank library_type or spectrum_type values remain after source lookup",
         call. = FALSE)
  }

  prune_spec <- prune
  if (is.null(prune_spec)) {
    prune_spec <- list(derivative = list(), nobaseline = list())
  }
  prune_rows <- list()
  for (name in intersect(names(prune_spec), names(libraries))) {
    report(paste0("pruning ", name))
    args <- prune_spec[[name]]
    if (is.null(args)) args <- list()
    if (is.null(args$progress)) args$progress <- progress
    args$return <- "report"
    pruned <- do.call(prune_lib, c(list(libraries[[name]]), args))
    libraries[[name]] <- pruned$object
    prune_rows[[name]] <- data.table::copy(pruned$summary)[
      , artifact := name][]
  }

  superseded_drop <- c(
    "x", "y", "xunits", "interpretation", "form_factor", "shape",
    "x_unit", "spectrumid", "locationdescription", "datatype"
  )
  optional_drop <- grepl("^assessment_", tables$metadata_drop$metadata_column)
  drop_status <- data.table::data.table(
    metadata_column = tables$metadata_drop$metadata_column,
    status = ifelse(
      tables$metadata_drop$metadata_column %in% names(libraries$raw$metadata),
      "present",
      ifelse(
        tables$metadata_drop$metadata_column %in% superseded_drop,
        "superseded_absent",
        ifelse(optional_drop, "optional_absent", "stale_absent")
      )
    )
  )
  report(paste0(
    "metadata drop QA: ",
    paste(drop_status[, paste0(status, "=", .N), by = status]$V1,
          collapse = "; ")
  ))

  before_filter <- ncol(libraries$raw$spectra)
  keep <- !is.na(libraries$raw$metadata$material_type) &
    !grepl(
      paste0(
        "(6_f12)|(6_c8)|(7_b1)|(6_e5)|(7_c7)|(7_e6)|(7_c9)|",
        "(7_g6)|(7_c4)|(7_a8)|(6_h4)|(6_g5)"
      ),
      libraries$raw$metadata$spectrum_id, ignore.case = TRUE
    )
  keep[is.na(keep)] <- TRUE
  keep_ids <- .lib_ids(libraries$raw, "sample_name")[keep]
  libraries <- lapply(libraries, function(object) {
    filter_spec(
      object,
      .lib_ids(object, "sample_name") %in% keep_ids
    )
  })
  libraries <- lapply(libraries, function(object) {
    drop_cols <- intersect(
      tables$metadata_drop$metadata_column, names(object$metadata)
    )
    if (length(drop_cols) > 0L) object$metadata[, (drop_cols) := NULL]
    object
  })
  for (name in intersect(c("derivative", "nobaseline"), names(libraries))) {
    libraries[[name]]$spectra <- round(libraries[[name]]$spectra, 3)
  }
  report(sprintf("special filters complete (removed=%d; retained=%d)",
                 before_filter - ncol(libraries$raw$spectra),
                 ncol(libraries$raw$spectra)))

  list(
    libraries = libraries,
    assessments = list(
      class_prediction = data.table::rbindlist(prediction_rows, fill = TRUE),
      class_coverage = data.table::rbindlist(coverage_rows, fill = TRUE),
      type_coverage = type_coverage,
      pruning = data.table::rbindlist(prune_rows, fill = TRUE),
      filters = data.table::data.table(
        stage = "special_filter", before = before_filter,
        after = ncol(libraries$raw$spectra),
        removed = before_filter - ncol(libraries$raw$spectra)
      ),
      metadata_drop = drop_status
    )
  )
}

.lib_build_medoids <- function(libraries, report, checkpoints = NULL) {
  processed <- intersect(c("derivative", "nobaseline"), names(libraries))
  out <- lapply(processed, function(name) {
    stage <- paste0("medoid_", name)
    cached <- if (is.null(checkpoints)) NULL else checkpoints$get(stage)
    if (!is.null(cached)) return(cached)
    report(paste0("selecting medoids (", name, ")"))
    ids <- reduce_lib(
      libraries[[name]],
      group_cols = c("spectrum_type", "organization", "material_class"),
      k = 50, min_n = 50, return = "ids"
    )
    result <- filter_spec(libraries[[name]], ids)
    if (!is.null(checkpoints)) checkpoints$put(stage, result)
    result
  })
  names(out) <- processed
  out
}

.lib_build_models <- function(medoids, report, checkpoints = NULL) {
  models <- list()
  warnings <- list()
  for (recipe in names(medoids)) {
    x <- medoids[[recipe]]
    use <- x$wavenumber >= 800 & x$wavenumber <= 3200
    if (sum(use) >= 2L) {
      x <- restrict_range(x, min = 800, max = 3200, make_rel = FALSE)
    }
    sources <- list(
      both = x,
      ftir = .lib_filter_optional_type(x, "ftir"),
      raman = .lib_filter_optional_type(x, "raman")
    )
    models[[recipe]] <- list()
    for (type in names(sources)) {
      stage <- paste0("model_", recipe, "_", type)
      cached <- if (is.null(checkpoints)) NULL else checkpoints$get(stage)
      if (!is.null(cached)) {
        models[[recipe]][[type]] <- cached
        next
      }
      report(paste0("training model (", recipe, "/", type, ")"))
      if (is.null(sources[[type]])) {
        warnings[[length(warnings) + 1L]] <- data.table::data.table(
          artifact = recipe, model = type,
          warning = "No spectra were available for this technique"
        )
        models[[recipe]][[type]] <- NULL
        next
      }
      models[[recipe]][[type]] <- tryCatch(
        build_model_lib(sources[[type]]),
        error = function(error) {
          warnings[[length(warnings) + 1L]] <<- data.table::data.table(
            artifact = recipe, model = type,
            warning = conditionMessage(error)
          )
          NULL
        }
      )
      if (!is.null(checkpoints) && !is.null(models[[recipe]][[type]])) {
        checkpoints$put(stage, models[[recipe]][[type]])
      }
    }
  }
  list(
    models = models,
    warnings = data.table::rbindlist(warnings, fill = TRUE)
  )
}

.lib_filter_optional_type <- function(x, type) {
  keep <- !is.na(x$metadata$spectrum_type) &
    tolower(x$metadata$spectrum_type) == type
  if (!any(keep)) return(NULL)
  filter_spec(x, keep)
}

.lib_local_build_assessments <- function(libraries, medoids, models,
                                         completed, model_warnings) {
  artifacts <- c(
    libraries,
    setNames(medoids, paste0("medoid_", names(medoids)))
  )
  summary <- data.table::rbindlist(lapply(names(artifacts), function(name) {
    data.table::data.table(
      artifact = name,
      valid_OpenSpecy = suppressWarnings(check_OpenSpecy(artifacts[[name]])),
      spectra = ncol(artifacts[[name]]$spectra),
      wavenumbers = length(artifacts[[name]]$wavenumber),
      metadata_columns = ncol(artifacts[[name]]$metadata)
    )
  }), fill = TRUE)
  model_summary <- data.table::rbindlist(lapply(names(models), function(recipe) {
    data.table::rbindlist(lapply(names(models[[recipe]]), function(type) {
      model <- models[[recipe]][[type]]
      data.table::data.table(
        artifact = recipe, model = type,
        trained = !is.null(model),
        classes = if (is.null(model)) NA_integer_ else model$class_num,
        observations = if (is.null(model)) NA_integer_ else
          model$observation_count
      )
    }), fill = TRUE)
  }), fill = TRUE)
  lookup_coverage <- data.table::rbindlist(lapply(names(libraries), function(name) {
    reports <- attr(libraries[[name]], "metadata_lookup_reports")
    if (is.null(reports) || length(reports) == 0L) return(NULL)
    data.table::rbindlist(lapply(names(reports), function(stage) {
      out <- data.table::copy(reports[[stage]])
      out[, `:=`(artifact = name, stage = stage)]
      out
    }), fill = TRUE)
  }), fill = TRUE)
  identity_cleanup <- data.table::rbindlist(lapply(names(libraries), function(name) {
    out <- attr(libraries[[name]], "spectrum_identity_cleanup_report")
    if (is.null(out) || nrow(out) == 0L) return(NULL)
    out <- data.table::copy(out)
    out[, artifact := name]
    out
  }), fill = TRUE)
  exclusions <- data.table::rbindlist(lapply(names(libraries), function(name) {
    out <- attr(libraries[[name]], "build_stage_report")
    if (is.null(out) || nrow(out) == 0L) return(NULL)
    out <- data.table::copy(out)
    out[, artifact := name]
    out
  }), fill = TRUE)
  defaults <- list(
    build_summary = summary,
    lookup_coverage = lookup_coverage,
    identity_cleanup = identity_cleanup,
    class_prediction = data.table::data.table(),
    class_coverage = data.table::data.table(),
    type_coverage = data.table::data.table(),
    exclusions_deduplication = exclusions,
    filters = data.table::data.table(),
    metadata_drop = data.table::data.table(),
    pruning = data.table::data.table(),
    medoid_model_summary = model_summary,
    split_manifest = data.table::data.table(),
    library_identification = data.table::data.table(),
    model_identification = data.table::data.table(),
    assess_spec_shifts = data.table::data.table(),
    old_new_compatibility = data.table::data.table(),
    warnings = model_warnings,
    output_manifest = data.table::data.table()
  )
  if (!is.null(completed)) defaults[names(completed)] <- completed
  defaults
}

.lib_previous_signature <- function(path) {
  if (is.null(path)) return("none")
  resolved <- if (identical(path, "system")) {
    system.file("extdata", package = "OpenSpecy")
  } else {
    path
  }
  types <- c(
    "raw", "derivative", "nobaseline", "medoid_derivative",
    "medoid_nobaseline", "model_derivative", "model_nobaseline"
  )
  files <- file.path(resolved, paste0(types, ".rds"))
  digest::digest(.lib_file_signatures(files, checksum_limit = Inf),
                 algo = "sha256")
}

.lib_compare_reference_build <- function(build, previous_library_dir,
                                         seed, holdout, progress,
                                         checkpoints = NULL,
                                         checkpoint_key = NULL) {
  prior <- .lib_load_previous_libraries(previous_library_dir, progress)
  library_pairs <- list(
    raw = list(new = build$libraries$raw, old = prior$raw),
    derivative = list(new = build$libraries$derivative,
                      old = prior$derivative),
    nobaseline = list(new = build$libraries$nobaseline,
                      old = prior$nobaseline),
    medoid_derivative = list(new = build$medoids$derivative,
                             old = prior$medoid_derivative),
    medoid_nobaseline = list(new = build$medoids$nobaseline,
                             old = prior$medoid_nobaseline)
  )

  compatibility <- data.table::rbindlist(
    lapply(names(library_pairs), function(artifact) {
      .lib_compatibility_rows(
        library_pairs[[artifact]]$new,
        library_pairs[[artifact]]$old,
        artifact
      )
    }), fill = TRUE
  )
  if (!is.null(checkpoints)) {
    checkpoints$put("assessment_compatibility", compatibility,
                    key = checkpoint_key)
  }

  split_rows <- list()
  reference_tests <- list()
  split_by_artifact <- list()
  for (i in seq_along(library_pairs)) {
    artifact <- names(library_pairs)[[i]]
    if (isTRUE(progress)) {
      message(sprintf(
        "build_lib assessment: split and identify %s (%d/%d)",
        artifact, i, length(library_pairs)
      ))
    }
    pair <- library_pairs[[artifact]]
    split_stage <- paste0("assessment_split_", artifact)
    split <- if (is.null(checkpoints)) NULL else
      checkpoints$get(split_stage, key = checkpoint_key)
    if (is.null(split)) {
      split <- .lib_combined_split(pair$new, pair$old, artifact,
                                   seed = seed + i, holdout = holdout)
      if (!is.null(checkpoints)) {
        checkpoints$put(split_stage, split, key = checkpoint_key)
      }
    }
    split_by_artifact[[artifact]] <- split
    split_rows[[artifact]] <- split$manifest
    for (source in c("new", "old")) {
      stage <- paste0("assessment_reference_", artifact, "_", source)
      tests <- if (is.null(checkpoints)) NULL else
        checkpoints$get(stage, key = checkpoint_key)
      if (is.null(tests)) {
        tests <- .lib_reference_holdout_test(
          pair[[source]], split, artifact, source
        )
        if (!is.null(checkpoints)) {
          checkpoints$put(stage, tests, key = checkpoint_key)
        }
      }
      reference_tests[[paste0(artifact, "_", source)]] <- tests
    }
  }
  split_manifest <- data.table::rbindlist(split_rows, fill = TRUE)
  reference_tests <- data.table::rbindlist(reference_tests, fill = TRUE)
  library_identification <- .lib_identification_summary(reference_tests)

  model_tests <- list()
  updated_models <- build$models
  for (recipe in intersect(c("derivative", "nobaseline"),
                           names(build$models))) {
    artifact <- recipe
    split <- split_by_artifact[[artifact]]
    candidate <- build$libraries[[recipe]]
    group_ids <- .lib_comparison_group_ids(candidate)
    test_groups <- split$manifest[split == "test", group_id]
    candidate_test <- group_ids %in% test_groups
    if (!any(candidate_test) || all(candidate_test)) next
    train <- filter_spec(candidate, !candidate_test)
    test <- filter_spec(candidate, candidate_test)
    candidate_sources <- list(
      both = train,
      ftir = .lib_filter_optional_type(train, "ftir"),
      raman = .lib_filter_optional_type(train, "raman")
    )
    candidate_tests <- list(
      both = test,
      ftir = .lib_filter_optional_type(test, "ftir"),
      raman = .lib_filter_optional_type(test, "raman")
    )
    for (type in names(candidate_sources)) {
      if (is.null(candidate_sources[[type]]) ||
          is.null(candidate_tests[[type]]) ||
          is.null(updated_models[[recipe]][[type]])) next
      if (isTRUE(progress)) {
        message("build_lib assessment: held-out model ", recipe, "/", type)
      }
      stage <- paste0("assessment_model_", recipe, "_", type, "_new")
      tests <- if (is.null(checkpoints)) NULL else
        checkpoints$get(stage, key = checkpoint_key)
      if (is.null(tests)) {
        evaluation_model <- tryCatch(
          build_model_lib(candidate_sources[[type]]),
          error = function(error) NULL
        )
        if (!is.null(evaluation_model)) {
          tests <- .lib_model_holdout_test(
            evaluation_model, candidate_tests[[type]], recipe, type,
            source = "new", provenance = "heldout_model"
          )
          if (!is.null(checkpoints)) {
            checkpoints$put(stage, tests, key = checkpoint_key)
          }
        }
      }
      if (!is.null(tests)) {
        updated_models[[recipe]][[type]]$tests <- tests
        model_tests[[paste(recipe, type, "new", sep = "_")]] <- tests
      }
    }

    legacy_model <- prior[[paste0("model_", recipe)]]
    legacy_test <- library_pairs[[artifact]]$old
    legacy_groups <- .lib_comparison_group_ids(legacy_test)
    legacy_keep <- legacy_groups %in% test_groups
    if (!any(legacy_keep)) next
    legacy_test <- filter_spec(legacy_test, legacy_keep)
    for (type in intersect(c("both", "ftir", "raman"), names(legacy_model))) {
      query <- if (type == "both") legacy_test else
        .lib_filter_optional_type(legacy_test, type)
      if (is.null(query) || is.null(legacy_model[[type]])) next
      stage <- paste0("assessment_model_", recipe, "_", type, "_old")
      tests <- if (is.null(checkpoints)) NULL else
        checkpoints$get(stage, key = checkpoint_key)
      if (is.null(tests)) {
        tests <- .lib_model_holdout_test(
          legacy_model[[type]], query, recipe, type,
          source = "old",
          provenance = "published_model_unknown_training_membership"
        )
        if (!is.null(checkpoints)) {
          checkpoints$put(stage, tests, key = checkpoint_key)
        }
      }
      model_tests[[paste(recipe, type, "old", sep = "_")]] <- tests
    }
  }
  model_tests <- data.table::rbindlist(model_tests, fill = TRUE)
  model_identification <- .lib_identification_summary(model_tests)

  if (isTRUE(progress)) {
    message("build_lib assessment: summarizing assess_spec shifts")
  }
  assessment_summaries <- list()
  for (artifact in names(library_pairs)) {
    for (source in c("new", "old")) {
      stage <- paste0("assessment_spectra_", artifact, "_", source)
      summary <- if (is.null(checkpoints)) NULL else
        checkpoints$get(stage, key = checkpoint_key)
      if (is.null(summary)) {
        summary <- .lib_assess_spec_summary(
          library_pairs[[artifact]][[source]], artifact, source
        )
        if (!is.null(checkpoints)) {
          checkpoints$put(stage, summary, key = checkpoint_key)
        }
      }
      assessment_summaries[[paste0(artifact, "_", source)]] <- summary
    }
  }
  assess_spec_shifts <- .lib_assessment_shift_table(
    data.table::rbindlist(assessment_summaries, fill = TRUE)
  )

  list(
    models = updated_models,
    split_manifest = split_manifest,
    library_identification = library_identification,
    model_identification = model_identification,
    assess_spec_shifts = assess_spec_shifts,
    old_new_compatibility = compatibility
  )
}

.lib_load_previous_libraries <- function(path, progress) {
  types <- c(
    "raw", "derivative", "nobaseline", "medoid_derivative",
    "medoid_nobaseline", "model_derivative", "model_nobaseline"
  )
  resolved <- if (identical(path, "system")) {
    system.file("extdata", package = "OpenSpecy")
  } else {
    path
  }
  if (!nzchar(resolved)) {
    stop("Could not resolve 'previous_library_dir'", call. = FALSE)
  }
  missing <- !file.exists(file.path(resolved, paste0(types, ".rds")))
  if (any(missing)) {
    if (isTRUE(progress)) {
      message("build_lib assessment: retrieving missing legacy artifacts")
    }
    get_lib(types[missing], path = path)
  }
  missing <- !file.exists(file.path(resolved, paste0(types, ".rds")))
  if (any(missing)) {
    stop("Legacy artifact(s) remain unavailable: ",
         paste(types[missing], collapse = ", "), call. = FALSE)
  }
  setNames(lapply(file.path(resolved, paste0(types, ".rds")), readRDS), types)
}

.lib_compatibility_rows <- function(new, old, artifact) {
  new_ids <- .lib_ids(new, "sample_name")
  old_ids <- .lib_ids(old, "sample_name")
  new_names <- names(new$metadata)
  old_names <- names(old$metadata)
  data.table::data.table(
    artifact = artifact,
    metric = c(
      "new_spectra", "old_spectra", "shared_identifiers",
      "new_only_identifiers", "old_only_identifiers", "axes_identical",
      "new_wavenumbers", "old_wavenumbers", "new_metadata_columns",
      "old_metadata_columns", "new_only_metadata", "old_only_metadata"
    ),
    value = c(
      ncol(new$spectra), ncol(old$spectra), length(intersect(new_ids, old_ids)),
      length(setdiff(new_ids, old_ids)), length(setdiff(old_ids, new_ids)),
      identical(new$wavenumber, old$wavenumber), length(new$wavenumber),
      length(old$wavenumber), length(new_names), length(old_names),
      paste(setdiff(new_names, old_names), collapse = "; "),
      paste(setdiff(old_names, new_names), collapse = "; ")
    )
  )
}

.lib_combined_split <- function(new, old, artifact, seed, holdout) {
  rows <- data.table::rbindlist(list(
    .lib_split_rows(new, "new"), .lib_split_rows(old, "old")
  ), fill = TRUE)
  groups <- rows[, .(
    material_class = .lib_first_value(material_class),
    spectrum_type = .lib_first_value(spectrum_type),
    new_present = any(source == "new"), old_present = any(source == "old")
  ), by = group_id]
  groups[, stratum := paste(
    ifelse(is.na(spectrum_type), "unknown", spectrum_type),
    ifelse(is.na(material_class), "unclassified", material_class), sep = "\r"
  )]
  set.seed(seed)
  test_groups <- groups[, {
    n_test <- if (.N <= 1L) 0L else max(1L, floor(.N * holdout))
    list(group_id = if (n_test) sample(group_id, n_test) else character())
  }, by = stratum]$group_id
  groups[, `:=`(
    artifact = artifact,
    split = ifelse(group_id %in% test_groups, "test", "train")
  )]
  list(
    manifest = groups[, .(
      artifact, group_id, split, material_class, spectrum_type,
      new_present, old_present
    )],
    rows = rows
  )
}

.lib_split_rows <- function(x, source) {
  metadata <- x$metadata
  ids <- .lib_comparison_group_ids(x)
  data.table::data.table(
    source = source, row = seq_along(ids), group_id = ids,
    material_class = if ("material_class" %in% names(metadata))
      as.character(metadata$material_class) else NA_character_,
    spectrum_type = if ("spectrum_type" %in% names(metadata))
      as.character(metadata$spectrum_type) else NA_character_
  )
}

.lib_comparison_group_ids <- function(x) {
  ids <- as.character(.lib_ids(x, "sample_name"))
  metadata <- x$metadata
  if (!"sample_name_old" %in% names(metadata)) return(ids)
  legacy_ids <- trimws(as.character(metadata$sample_name_old))
  usable <- !is.na(legacy_ids) & nzchar(legacy_ids) &
    tolower(legacy_ids) != "new format"
  ids[usable] <- legacy_ids[usable]
  ids
}

.lib_first_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[[1L]] else NA_character_
}

.lib_reference_holdout_test <- function(x, split, artifact, source,
                                        block_size = 32L) {
  ids <- .lib_ids(x, "sample_name")
  group_ids <- .lib_comparison_group_ids(x)
  test_groups <- split$manifest[split == "test", group_id]
  test_idx <- which(group_ids %in% test_groups)
  train_idx <- which(!group_ids %in% test_groups)
  if (length(test_idx) == 0L || length(train_idx) == 0L) {
    return(data.table::data.table())
  }
  library <- filter_spec(x, train_idx)
  library_ids <- .lib_ids(library, "sample_name")
  blocks <- split(test_idx, ceiling(seq_along(test_idx) / block_size))
  out <- lapply(blocks, function(index) {
    query <- filter_spec(x, index)
    cors <- cor_spec(query, library = library, compute = "optimized")
    top <- max_cor_named(cors)
    matched_ids <- names(top)
    matched_rows <- match(matched_ids, library_ids)
    data.table::data.table(
      artifact = artifact, source = source,
      spectrum_id = ids[index],
      technique = as.character(query$metadata$spectrum_type),
      expected_class = as.character(query$metadata$material_class),
      predicted_class = as.character(
        library$metadata$material_class[matched_rows]
      ),
      correct = as.character(query$metadata$material_class) ==
        as.character(library$metadata$material_class[matched_rows]),
      score = as.numeric(top), split = "test",
      provenance = "reference_holdout"
    )
  })
  data.table::rbindlist(out, fill = TRUE)
}

.lib_model_holdout_test <- function(model, x, artifact, type, source,
                                    provenance) {
  x <- .lib_align_model_input(x, model)
  prediction <- ai_classify(x, model)
  predicted <- if ("name" %in% names(prediction)) {
    as.character(prediction$name)
  } else if ("predicted_name" %in% names(prediction)) {
    as.character(prediction$predicted_name)
  } else {
    rep(NA_character_, nrow(prediction))
  }
  expected <- as.character(x$metadata$material_class)
  technique <- if ("spectrum_type" %in% names(x$metadata)) {
    as.character(x$metadata$spectrum_type)
  } else {
    NA_character_
  }
  typed_expected <- paste(technique, expected, sep = "_")
  use_typed <- !is.na(typed_expected) & typed_expected %in% model$class_names
  expected[use_typed] <- typed_expected[use_typed]
  data.table::data.table(
    artifact = artifact, model = type, source = source,
    spectrum_id = .lib_ids(x, "sample_name"), technique = technique,
    expected_class = expected, predicted_class = predicted,
    correct = expected == predicted,
    score = if ("value" %in% names(prediction)) prediction$value else NA_real_,
    split = "test", provenance = provenance
  )
}

.lib_align_model_input <- function(x, model) {
  variables <- as.numeric(model$all_variables)
  keep <- match(variables, x$wavenumber)
  if (anyNA(keep)) {
    stop("Model variables are not available on the comparison spectrum axis",
         call. = FALSE)
  }
  out <- x
  out$wavenumber <- x$wavenumber[keep]
  out$spectra <- x$spectra[keep, , drop = FALSE]
  out
}

.lib_identification_summary <- function(tests) {
  if (nrow(tests) == 0L) return(data.table::data.table())
  group_cols <- intersect(
    c("artifact", "model", "source", "technique", "provenance"),
    names(tests)
  )
  overall <- tests[, .(
    spectra = .N, evaluated = sum(!is.na(correct)),
    accuracy = mean(correct, na.rm = TRUE),
    mean_score = mean(score, na.rm = TRUE)
  ), by = group_cols]
  class_summary <- tests[, .(
    class_accuracy = mean(correct, na.rm = TRUE), class_spectra = .N
  ), by = c(group_cols, "expected_class")]
  macro <- class_summary[, .(
    macro_class_accuracy = mean(class_accuracy, na.rm = TRUE),
    classes = .N
  ), by = group_cols]
  merge(overall, macro, by = group_cols, all = TRUE)
}

.lib_assess_spec_summary <- function(x, artifact, source,
                                     block_size = 500L) {
  blocks <- split(seq_len(ncol(x$spectra)),
                  ceiling(seq_len(ncol(x$spectra)) / block_size))
  rows <- lapply(blocks, function(index) {
    assessed <- assess_spec(filter_spec(x, index), report = "all")
    assessed[, .(
      count = .N,
      finding_count = sum(finding_count, na.rm = TRUE),
      example_ids = paste(head(unique(spectrum_id[status != "pass"]), 20L),
                          collapse = "; ")
    ), by = .(check, status)]
  })
  summary <- data.table::rbindlist(rows, fill = TRUE)[, .(
    count = sum(count), finding_count = sum(finding_count),
    example_ids = paste(head(unique(unlist(strsplit(
      example_ids[nzchar(example_ids)], "; ", fixed = TRUE
    ))), 20L), collapse = "; ")
  ), by = .(check, status)]
  summary[, `:=`(
    artifact = artifact, source = source,
    spectra = ncol(x$spectra), rate = count / ncol(x$spectra)
  )]
  summary
}

.lib_assessment_shift_table <- function(summary) {
  if (nrow(summary) == 0L) return(summary)
  old <- summary[source == "old"]
  new <- summary[source == "new"]
  by <- c("artifact", "check", "status")
  out <- merge(
    old, new, by = by, all = TRUE, suffixes = c("_old", "_new")
  )
  for (column in c("count", "finding_count", "spectra", "rate")) {
    old_col <- paste0(column, "_old")
    new_col <- paste0(column, "_new")
    out[[paste0(column, "_shift")]] <- out[[new_col]] - out[[old_col]]
  }
  out$relative_rate_shift <- ifelse(
    is.na(out$rate_old) | out$rate_old == 0,
    NA_real_, (out$rate_new - out$rate_old) / out$rate_old
  )
  out
}

.lib_promote_reference_build <- function(build, output_dir, signature, reuse) {
  release_dir <- file.path(output_dir, "releases", substr(signature, 1L, 12L))
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
  artifacts <- c(
    build$libraries,
    setNames(build$medoids, paste0("medoid_", names(build$medoids))),
    setNames(build$models, paste0("model_", names(build$models)))
  )
  for (name in names(artifacts)) {
    path <- file.path(release_dir, paste0(name, ".rds"))
    if (!isTRUE(reuse) || !file.exists(path)) {
      .lib_atomic_saveRDS(artifacts[[name]], path)
    }
  }
  .lib_atomic_saveRDS(build, file.path(release_dir,
                                       "reference_library_build.rds"))
  release_dir
}

.lib_is_lookup_spec <- function(x) {
  is.list(x) && !inherits(x, c("data.frame", "data.table")) &&
    all(c("lookup", "by") %in% names(x))
}

.lib_normalize_lookup_spec <- function(x) {
  if (!.lib_is_lookup_spec(x)) {
    return(list(lookup = x, by = NULL, fallback_by = NULL,
                fill_only = FALSE))
  }
  extra <- setdiff(names(x), c("lookup", "by", "fallback_by", "fill_only"))
  if (length(extra) > 0L) {
    stop("Explicit metadata lookup specifications only accept 'lookup', ",
         "'by', 'fallback_by', and 'fill_only'", call. = FALSE)
  }
  if (is.null(x$by) || !is.character(x$by) || length(x$by) < 1L ||
      anyNA(x$by) || any(!nzchar(x$by))) {
    stop("Explicit metadata lookup 'by' must contain column names",
         call. = FALSE)
  }
  if (!is.null(x$fallback_by) &&
      (!is.character(x$fallback_by) || length(x$fallback_by) != 1L ||
       is.na(x$fallback_by) || !nzchar(x$fallback_by))) {
    stop("Explicit metadata lookup 'fallback_by' must be one column name",
         call. = FALSE)
  }
  if (is.null(x$fallback_by)) x$fallback_by <- NULL
  if (is.null(x$fill_only)) x$fill_only <- FALSE
  if (!is.logical(x$fill_only) || length(x$fill_only) != 1L ||
      is.na(x$fill_only)) {
    stop("Explicit metadata lookup 'fill_only' must be TRUE or FALSE",
         call. = FALSE)
  }
  x
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
                                          suffixes = c(".x", ".y"),
                                          lookup_precedence = TRUE) {
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
    if (!isTRUE(lookup_precedence)) {
      keep_existing <- !is.na(result)
      if (is.character(result)) keep_existing <- keep_existing & nzchar(result)
      use_replacement <- use_replacement & !keep_existing
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
