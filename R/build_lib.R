#' @rdname build_lib
#' @title Build spectral libraries
#'
#' @description
#' Helpers for creating reference libraries from OpenSpecy objects, source
#' files, and metadata lookup tables. These functions keep spectra columns and
#' metadata rows aligned while letting users supply their own curation tables,
#' processing recipes, reduction settings, and model options.
#'
#' @param x an \code{OpenSpecy} object, a list of \code{OpenSpecy} objects, a
#' list of file paths, or a metadata table depending on the helper.
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
#' @param standardize logical; whether to trim, ASCII-convert, and lower-case
#' join/template values before matching.
#' @param metadata_aliases named list where each name is a canonical metadata
#' column and each value is a vector of aliases to coalesce into it.
#' @param na_values character values that should be treated as missing.
#' @param id_col metadata column used as the spectrum identifier.
#' @param exclude_ids identifiers to remove before returning a library.
#' @param duplicate how duplicated generated identifiers should be handled.
#' @param conform_args,smooth_args named argument lists passed to
#' \code{\link{conform_spec}()} and \code{\link{smooth_intens}()} before ID
#' generation.
#' @param scale numeric multiplier used before hashing intensity values.
#' @param algo hash algorithm passed to \code{\link[digest]{digest}()}.
#' @param recipes named list of processing recipes or functions for
#' \code{build_lib()}.
#' @param dedupe logical; whether to generate stable IDs and remove duplicated
#' spectra in \code{build_lib()}.
#' @param range,res wavenumber range and resolution passed to \code{c_spec()}.
#' @param split_sources logical; whether to split sources to one spectrum per
#' object before concatenating.
#' @param group_cols metadata columns defining groups for reduction.
#' @param k maximum representatives to keep for groups larger than
#' \code{min_n}.
#' @param min_n groups with \code{min_n} or fewer spectra are kept whole.
#' @param exclude_range numeric vector of length two, or a list of such vectors,
#' with wavenumbers excluded before reduction or model training.
#' @param preprocess logical; whether to min-max normalize and mean-fill spectra
#' before reduction.
#' @param class_col,type_col metadata columns used for model labels.
#' @param nearest logical; whether \code{assess_lib()} should estimate nearest
#' class consistency.
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
#' \code{standardize_lib_metadata()}, \code{join_lib_metadata()},
#' \code{join_material_hierarchy()}, \code{dedupe_spec()}, and
#' \code{reduce_lib()} return an \code{OpenSpecy} object or data.table depending
#' on input and \code{return}. \code{make_lib_lookup_template()} returns a
#' data.table unless \code{path} is supplied, in which case it writes the csv and
#' invisibly returns the table. \code{build_model_lib()} returns a list suitable
#' for AI classification with \code{\link{match_spec}()}.
#'
#' @examples
#' data("raman_hdpe")
#'
#' make_lib_lookup_template(raman_hdpe, columns = "spectrum_identity",
#'                          add = "material")
#'
#' lookup <- data.frame(spectrum_identity = "hdpe",
#'                      material = "polyethylene")
#' joined <- join_lib_metadata(raman_hdpe, lookup,
#'                             by = c(spectrum_identity = "spectrum_identity"))
#' check_OpenSpecy(joined)
#'
#' @author
#' Win Cowger
#'
#' @importFrom data.table as.data.table data.table fread fwrite rbindlist setorder
#' @importFrom cluster pam
#' @export
build_lib <- function(x, recipes = .default_lib_recipes(), range = NULL,
                      res = 5, split_sources = FALSE, id_col = "sample_name",
                      exclude_ids = NULL, dedupe = TRUE, ...) {
  lib <- .lib_source_to_OpenSpecy(x, range = range, res = res,
                                  split_sources = split_sources)

  if (!is.null(exclude_ids)) {
    ids <- .lib_ids(lib, id_col)
    keep <- !(ids %in% exclude_ids | colnames(lib$spectra) %in% exclude_ids)
    lib <- filter_spec(lib, keep)
  }

  if (dedupe) {
    lib <- dedupe_spec(lib, id_col = id_col, ...)
  }

  out <- lapply(recipes, function(recipe) .apply_lib_recipe(lib, recipe))
  names(out) <- names(recipes)
  out
}

#' @rdname build_lib
#' @export
standardize_lib_metadata <- function(x, columns = NULL, metadata_aliases = NULL,
                                     na_values = c("", "na", "n/a", "null",
                                                   "not available", "nan"),
                                     standardize = TRUE) {
  is_os <- is_OpenSpecy(x)
  metadata <- .lib_table(if (is_os) x$metadata else x)

  if (is.null(columns)) columns <- names(metadata)
  missing_cols <- setdiff(columns, names(metadata))
  if (length(missing_cols) > 0) {
    stop("Missing metadata columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  if (standardize) {
    for (col in columns) {
      if (is.character(metadata[[col]]) || is.factor(metadata[[col]])) {
        metadata[[col]] <- .lib_standardize_value(metadata[[col]], na_values)
      }
    }
  }

  if (!is.null(metadata_aliases)) {
    for (target in names(metadata_aliases)) {
      aliases <- metadata_aliases[[target]]
      aliases <- aliases[aliases %in% names(metadata)]
      if (!target %in% names(metadata)) metadata[[target]] <- NA
      for (alias in aliases) {
        fill <- is.na(metadata[[target]]) & !is.na(metadata[[alias]])
        metadata[[target]][fill] <- metadata[[alias]][fill]
      }
    }
  }

  if (is_os) {
    x$metadata <- metadata
    return(x)
  }
  metadata
}

#' @rdname build_lib
#' @export
make_lib_lookup_template <- function(x, columns, add = NULL, path = NULL,
                                     standardize = TRUE,
                                     na_values = c("", "na", "n/a", "null",
                                                   "not available", "nan")) {
  metadata <- .lib_table(if (is_OpenSpecy(x)) x$metadata else x)
  .lib_require_cols(metadata, columns, "metadata")

  template <- data.table::as.data.table(metadata[, columns, with = FALSE])
  if (standardize) {
    for (col in columns) {
      template[[col]] <- .lib_standardize_value(template[[col]], na_values)
    }
  }
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
                              standardize = TRUE,
                              na_values = c("", "na", "n/a", "null",
                                            "not available", "nan"),
                              suffixes = c(".x", ".y")) {
  return <- match.arg(return)
  is_os <- is_OpenSpecy(x)
  metadata <- .lib_table(if (is_os) x$metadata else x)
  lookup <- .lib_read_lookup(lookup)

  keys <- .lib_join_keys(by)
  .lib_require_cols(metadata, keys$x, "metadata")
  .lib_require_cols(lookup, keys$y, "lookup")

  metadata_key <- .lib_key(metadata, keys$x, standardize, na_values)
  lookup_key <- .lib_key(lookup, keys$y, standardize, na_values)
  value_cols <- setdiff(names(lookup), keys$y)

  dup_report <- .lib_duplicate_report(lookup_key, lookup, keys$y)
  if (nrow(dup_report) > 0) {
    attr(dup_report, "data") <- lookup
    stop("Lookup keys must be unique before joining. Duplicate keys: ",
         paste(head(dup_report$value, 10), collapse = ", "), call. = FALSE)
  }

  report <- .lib_join_report(metadata_key, lookup_key, lookup, value_cols,
                             keys$x)
  .lib_alert_join_report(report, require_complete)

  joined <- .lib_left_join(metadata, lookup, metadata_key, lookup_key, suffixes)
  attr(joined, "join_report") <- report

  if (return == "report") return(list(data = joined, report = report))
  if (return == "table" || !is_os) return(joined)

  x$metadata <- joined
  attr(x, "join_report") <- report
  x
}

#' @rdname build_lib
#' @export
join_material_hierarchy <- function(x, hierarchy, key_col,
                                    levels = c("material", "material_class",
                                               "material_type"),
                                    output_names = levels,
                                    require_complete = FALSE,
                                    return = c("object", "table", "report"),
                                    standardize = TRUE,
                                    na_values = c("", "na", "n/a", "null",
                                                  "not available", "nan")) {
  return <- match.arg(return)
  is_os <- is_OpenSpecy(x)
  metadata <- .lib_table(if (is_os) x$metadata else x)
  hierarchy <- .lib_read_lookup(hierarchy)
  output_names <- .lib_hierarchy_names(levels, output_names)

  .lib_require_cols(metadata, key_col, "metadata")
  .lib_require_cols(hierarchy, levels, "hierarchy")

  keys <- .lib_standardize_value(metadata[[key_col]], na_values)
  if (!standardize) keys <- as.character(metadata[[key_col]])

  out <- metadata
  for (col in output_names) out[[col]] <- NA_character_
  matched_level <- rep(NA_character_, nrow(out))

  remaining <- seq_len(nrow(out))
  duplicate_reports <- list()

  for (i in seq_along(levels)) {
    level <- levels[i]
    cols <- levels[i:length(levels)]
    h <- unique(hierarchy[, cols, with = FALSE])
    h_key <- if (standardize) {
      .lib_standardize_value(h[[level]], na_values)
    } else {
      as.character(h[[level]])
    }

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

  report <- .lib_hierarchy_report(keys, matched_level, duplicate_reports)
  .lib_alert_join_report(report, require_complete)
  attr(out, "join_report") <- report

  if (return == "report") return(list(data = out, report = report))
  if (return == "table" || !is_os) return(out)

  x$metadata <- out
  attr(x, "join_report") <- report
  x
}

#' @rdname build_lib
#' @export
dedupe_spec <- function(x, id_col = "sample_name", exclude_ids = NULL,
                        duplicate = c("first", "remove_all", "none"),
                        conform_args = list(res = 8),
                        smooth_args = list(), scale = 100, algo = "md5") {
  duplicate <- match.arg(duplicate)
  x <- as_OpenSpecy(x)

  ids <- .lib_hash_ids(x, conform_args = conform_args, smooth_args = smooth_args,
                       scale = scale, algo = algo)
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
                       k = 50, min_n = k, exclude_range = c(2200, 2420),
                       preprocess = TRUE,
                       return = c("object", "ids"), ...) {
  return <- match.arg(return)
  x <- as_OpenSpecy(x)
  .lib_require_cols(x$metadata, group_cols, "metadata")

  ids <- .lib_ids(x, id_col)
  use <- .lib_wavenumber_filter(x$wavenumber, exclude_range)
  spectra <- x$spectra[use, , drop = FALSE]
  if (preprocess) {
    spectra <- make_rel(spectra, na.rm = TRUE)
    spectra <- .matrix_mean_replace(spectra)
  }

  groups <- do.call(paste, c(x$metadata[, group_cols, with = FALSE], sep = "_"))
  keep_ids <- unlist(lapply(split(seq_along(groups), groups), function(idx) {
    if (length(idx) <= min_n || length(idx) <= k) return(ids[idx])
    .pam_group_ids(spectra[, idx, drop = FALSE], ids[idx], k = k, ...)
  }), use.names = FALSE)

  if (return == "ids") return(keep_ids)
  filter_spec(x, keep_ids)
}

#' @rdname build_lib
#' @export
build_model_lib <- function(x, class_col = "material_class",
                            type_col = "spectrum_type",
                            id_col = "sample_name",
                            range = c(800, 3200),
                            exclude_range = c(2200, 2420),
                            min_n = 10, alpha = 0.1, seed = 123,
                            grouped = TRUE, weights = TRUE,
                            make_relative = TRUE, complete_cases = TRUE,
                            ...) {
  x <- as_OpenSpecy(x)
  .lib_require_cols(x$metadata, class_col, "metadata")

  use <- x$wavenumber >= min(range) & x$wavenumber <= max(range)
  use <- use & .lib_wavenumber_filter(x$wavenumber, exclude_range)
  wavenumbers <- x$wavenumber[use]
  spectra <- x$spectra[use, , drop = FALSE]
  if (make_relative) spectra <- make_rel(spectra, na.rm = TRUE)

  train <- t(spectra)
  colnames(train) <- as.character(wavenumbers)
  metadata <- data.table::copy(x$metadata)

  if (complete_cases) {
    ok <- stats::complete.cases(train)
    train <- train[ok, , drop = FALSE]
    metadata <- metadata[ok, ]
  }

  labels <- .lib_model_labels(metadata, class_col, type_col)
  labels <- .lib_standardize_value(labels)
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
  coefficients_join <- .lib_model_coefficients(coefficients, outcome, labels)
  predictions <- predict(model, newx = train, s = lambda, type = "response")
  accuracy <- .lib_model_accuracy(predictions, outcome, labels, nrow(train))
  confusion <- accuracy[, .(label_accuracy = mean(correct),
                            observation_count = .N), by = "actual_name"]

  list(
    model = model,
    dimension_conversion = unique(data.table::data.table(
      factor_num = outcome,
      name = labels
    )),
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
      manage_na_fun = smooth_intens,
      manage_na_args = list(lead_tail_only = TRUE, ig = c(NA), window = 15),
      signal_noise = TRUE,
      signal_noise_args = list(step = 10),
      attributes = list(derivative_order = "1")
    ),
    nobaseline = list(
      manage_na_fun = subtr_baseline,
      manage_na_args = list(lead_tail_only = TRUE, ig = c(NA)),
      signal_noise = TRUE,
      signal_noise_args = list(step = 10),
      attributes = list(baseline = "nobaseline")
    )
  )
}

.apply_lib_recipe <- function(lib, recipe) {
  if (is.function(recipe)) return(recipe(lib))
  out <- lib

  if (!is.null(recipe$manage_na_fun)) {
    out <- do.call(manage_na, c(list(out, fun = recipe$manage_na_fun),
                                recipe$manage_na_args))
  }
  if (!is.null(recipe$process_args)) {
    out <- do.call(process_spec, c(list(out), recipe$process_args))
  }
  if (isTRUE(recipe$signal_noise)) {
    out$metadata$sn <- do.call(sig_noise, c(list(out), recipe$signal_noise_args))
  }
  if (!is.null(recipe$round)) out$spectra <- round(out$spectra, recipe$round)
  if (!is.null(recipe$attributes)) {
    for (nm in names(recipe$attributes)) attr(out, nm) <- recipe$attributes[[nm]]
  }

  out
}

.lib_source_to_OpenSpecy <- function(x, range = NULL, res = 5,
                                     split_sources = FALSE) {
  if (is_OpenSpecy(x)) return(as_OpenSpecy(x))
  if (is.character(x)) x <- lapply(x, read_any)
  if (!is.list(x)) stop("'x' must be an OpenSpecy object, list, or file path",
                        call. = FALSE)
  if (split_sources) x <- split_spec(x)
  if (length(x) == 1 && is_OpenSpecy(x[[1]])) return(as_OpenSpecy(x[[1]]))
  c_spec(x, range = range, res = res)
}

.lib_table <- function(x) {
  data.table::as.data.table(data.table::copy(x))
}

.lib_read_lookup <- function(x) {
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    return(data.table::fread(x))
  }
  .lib_table(x)
}

.lib_require_cols <- function(x, cols, label) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop("Missing ", label, " columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

.lib_standardize_value <- function(x, na_values = c("", "na", "n/a", "null",
                                                    "not available", "nan")) {
  x <- as.character(x)
  x <- trimws(tolower(iconv(x, to = "ASCII", sub = "")))
  x[x %in% na_values] <- NA_character_
  x
}

.lib_join_keys <- function(by) {
  if (is.null(names(by)) || all(names(by) == "")) {
    return(list(x = unname(by), y = unname(by)))
  }
  list(x = names(by), y = unname(by))
}

.lib_key <- function(x, cols, standardize, na_values) {
  vals <- lapply(cols, function(col) {
    if (standardize) .lib_standardize_value(x[[col]], na_values)
    else as.character(x[[col]])
  })
  any_na <- Reduce(`|`, lapply(vals, is.na))
  key <- do.call(paste, c(vals, sep = "\r"))
  key[any_na] <- NA_character_
  key
}

.lib_duplicate_report <- function(keys, lookup, key_cols) {
  dups <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  dups[is.na(keys)] <- FALSE
  if (!any(dups)) {
    return(data.table::data.table(problem = character(), column = character(),
                                  value = character(), n = integer()))
  }
  data.table::data.table(value = keys[dups])[
    , .(n = .N), by = "value"][
      , `:=`(problem = "duplicate_lookup_key",
             column = paste(key_cols, collapse = "|"))][
               , .(problem, column, value, n)]
}

.lib_join_report <- function(metadata_key, lookup_key, lookup, value_cols,
                             metadata_cols) {
  missing_key <- is.na(metadata_key) | !metadata_key %in% lookup_key
  unmatched <- data.table::data.table(value = metadata_key[missing_key])[
    !is.na(value), .(n = .N), by = "value"][
      , `:=`(problem = "unmatched_metadata_key",
             column = paste(metadata_cols, collapse = "|"))][
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

  data.table::rbindlist(list(unmatched, missing_values), fill = TRUE)
}

.lib_left_join <- function(metadata, lookup, metadata_key, lookup_key,
                           suffixes) {
  meta <- data.table::copy(metadata)
  look <- data.table::copy(lookup)
  meta$..join_key <- metadata_key
  meta$..row_id <- seq_len(nrow(meta))
  look$..join_key <- lookup_key
  joined <- merge(meta, look, by = "..join_key", all.x = TRUE, sort = FALSE,
                  suffixes = suffixes)
  data.table::setorder(joined, "..row_id")
  joined[, c("..join_key", "..row_id") := NULL]
  joined
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

.lib_hierarchy_names <- function(levels, output_names) {
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
  output_names
}

.lib_hierarchy_report <- function(keys, matched_level, duplicate_reports) {
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
  data.table::rbindlist(list(unmatched, duplicates), fill = TRUE)
}

.lib_ids <- function(x, id_col) {
  if (id_col %in% names(x$metadata)) return(as.character(x$metadata[[id_col]]))
  colnames(x$spectra)
}

.lib_hash_ids <- function(x, conform_args, smooth_args, scale, algo) {
  spec <- x
  if (!is.null(conform_args)) {
    spec <- do.call(conform_spec, c(list(spec), conform_args))
  }
  if (!is.null(smooth_args)) {
    spec <- do.call(smooth_intens, c(list(spec), smooth_args))
  }
  vapply(seq_len(ncol(spec$spectra)), function(i) {
    digest::digest(list(as.integer(spec$wavenumber),
                        as.integer(spec$spectra[, i] * scale)),
                   algo = algo)
  }, FUN.VALUE = character(1))
}

.lib_wavenumber_filter <- function(wavenumber, exclude_range) {
  keep <- rep(TRUE, length(wavenumber))
  if (is.null(exclude_range)) return(keep)
  ranges <- if (is.list(exclude_range)) exclude_range else list(exclude_range)
  for (range in ranges) {
    if (length(range) != 2) stop("'exclude_range' entries must have length 2",
                                call. = FALSE)
    keep <- keep & !(wavenumber >= min(range) & wavenumber <= max(range))
  }
  keep
}

.pam_group_ids <- function(spectra, ids, k, ...) {
  if (ncol(spectra) <= k) return(ids)
  cors <- stats::cor(spectra, use = "pairwise.complete.obs")
  cors[is.na(cors)] <- 0
  cors <- pmax(pmin(cors, 1), -1)
  diag(cors) <- 1
  distance <- stats::as.dist(1 - cors)
  pam_args <- list(x = distance, k = min(k, length(ids) - 1L), diss = TRUE,
                   pamonce = 6)
  user_args <- list(...)
  pam_args[names(user_args)] <- user_args
  res <- do.call(cluster::pam, pam_args)
  ids[res$id.med]
}

.lib_model_labels <- function(metadata, class_col, type_col) {
  classes <- as.character(metadata[[class_col]])
  if (!is.null(type_col) && type_col %in% names(metadata)) {
    return(paste(as.character(metadata[[type_col]]), classes, sep = "_"))
  }
  classes
}

.lib_model_coefficients <- function(coefficients, outcome, labels) {
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
  df <- data.table::rbindlist(rows)
  wave <- data.table::data.table(
    names = coef_list[[1]]@Dimnames[[1]],
    id = seq_along(coef_list[[1]]@Dimnames[[1]]) - 1L
  )
  out <- merge(df, dimension_conversion, by.x = "variable",
               by.y = "factor_num", all.x = TRUE)
  out <- merge(out, wave, by.x = "dimensions_used", by.y = "id",
               all.x = FALSE)
  out$names <- suppressWarnings(as.numeric(ifelse(out$names == "(Intercept)",
                                                  "0", out$names)))
  out
}

.lib_model_accuracy <- function(predictions, outcome, labels, n) {
  pred <- .ai_prediction_table(predictions, n = n)
  dimension_conversion <- unique(data.table::data.table(
    factor_num = outcome,
    name = labels
  ))
  actual <- data.table::data.table(row_id = seq_along(outcome),
                                   actual_label = outcome,
                                   actual_name = labels)
  out <- merge(pred, actual, by.x = "x", by.y = "row_id", all.x = TRUE)
  out <- merge(out, dimension_conversion, by.x = "y", by.y = "factor_num",
               all.x = TRUE)
  names(out)[names(out) == "name"] <- "predicted_name"
  out$correct <- out$actual_name == out$predicted_name
  out
}
