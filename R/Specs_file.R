#' Open a large spectral source as file-backed Specs
#'
#' `FileSpecs` keeps a durable, read-only description of a large H5 or ENVI
#' source. Spectral values are read only when an explicit bounded selection is
#' materialized. The object never stores an open connection or modifies a
#' source member.
#'
#' @param path path to an H5 file, an ENVI binary file, or its `.hdr` file.
#' @param cache_dir directory for immutable derived cache generations. The
#'   default uses the user cache directory, never the source directory.
#' @param x a `FileSpecs` object.
#' @param index positive row positions in the current file-backed view.
#' @param region optional region names to materialize.
#' @param roi optional numeric `c(xmin, xmax, ymin, ymax)` selection or a list
#'   with two-element `x` and `y` ranges.
#' @param bands optional positive spectral-band positions to materialize.
#' @param by grouping field; file-backed splitting currently supports only
#'   `"region"`.
#' @param ... additional arguments reserved for methods.
#'
#' @return `open_specs()` returns a descriptor-only `FileSpecs` object.
#'   `decompress_spec()` returns a bounded `OpenSpecy` materialization and
#'   `split_spec()` returns lightweight `FileSpecs` views.
#'
#' @export
open_specs <- function(path, cache_dir = NULL) {
  path <- .filespec_source_path(path)
  backend <- .filespec_backend(path)
  cache_root <- .filespec_cache_root(cache_dir)

  opened <- switch(
    backend,
    h5 = .filespec_open_h5(path),
    envi = .filespec_open_envi(path)
  )
  .filespec_validate_members(opened$source$members, strong = FALSE)

  source_id <- digest::digest(list(
    schema = "filespec-source-1",
    backend = backend,
    hashes = opened$source$members$sha256,
    axis = opened$source$axis,
    layout = opened$source$layout
  ), algo = "sha256")
  opened$source$id <- source_id

  out <- structure(
    list(
      source = opened$source,
      index = opened$index,
      view = NULL,
      cache = list(
        root = cache_root,
        generation = source_id,
        schema = "filespec-cache-1"
      ),
      recipe = list()
    ),
    class = c("FileSpecs", "Specs", "list")
  )
  attr(out, "specs_version") <- "0.2.0"
  attr(out, "variable_model") <- NULL
  attr(out, "hilbert_model") <- NULL
  attr(out, "spectrum_compression") <- list(method = "file-backed")
  attr(out, "transformations") <- list(list(
    method = "open_specs",
    backend = backend,
    source_id = source_id
  ))
  attr(out, "visual_image") <- opened$source$visual
  .filespec_validate_object(out)
  out
}

#' @rdname open_specs
#' @export
as_Specs.FileSpecs <- function(x, ...) {
  x
}

#' @rdname open_specs
#' @export
print.FileSpecs <- function(x, ...) {
  .filespec_validate_object(x)
  attached <- tryCatch({
    .filespec_validate_source(x, strong = FALSE)
    TRUE
  }, error = function(e) FALSE)
  cat("<FileSpecs>\n",
      " Backend: ", toupper(x$source$backend), "\n",
      " Spectra: ", .filespec_n_spectra(x), "\n",
      " Bands:   ", length(x$source$axis), "\n",
      " Regions: ", paste(.filespec_regions(x), collapse = ", "), "\n",
      " Source:  ", if (attached) "attached (read-only)" else "detached/changed",
      "\n", sep = "")
  invisible(x)
}

#' @rdname open_specs
#' @export
check_Specs.FileSpecs <- function(x, ...) {
  error <- tryCatch({
    .filespec_validate_object(x)
    .filespec_validate_source(x, strong = TRUE)
    NULL
  }, error = identity)
  if (!is.null(error)) {
    warning(conditionMessage(error), call. = FALSE)
    return(FALSE)
  }
  TRUE
}

#' @rdname open_specs
#' @export
decompress_spec.FileSpecs <- function(x, index = NULL, region = NULL,
                                      roi = NULL, bands = NULL, ...) {
  if (is.null(index) && is.null(region) && is.null(roi)) {
    stop("FileSpecs materialization must specify 'index', 'region', or 'roi'; ",
         "whole-source decompression is disabled", call. = FALSE)
  }

  idx <- .filespec_index(x)
  positions <- seq_len(nrow(idx))
  if (!is.null(index))
    positions <- .filespec_positions(index, nrow(idx), "index")
  if (!is.null(region)) {
    if (!is.character(region) || !length(region) || anyNA(region))
      stop("'region' must contain one or more region names", call. = FALSE)
    positions <- positions[idx$region[positions] %in% region]
  }
  if (!is.null(roi)) {
    bounds <- .filespec_roi(roi)
    positions <- positions[
      idx$x[positions] >= bounds[[1L]] &
        idx$x[positions] <= bounds[[2L]] &
        idx$y[positions] >= bounds[[3L]] &
        idx$y[positions] <= bounds[[4L]]
    ]
  }
  if (!length(positions))
    stop("the requested FileSpecs selection is empty", call. = FALSE)

  .filespec_read(x, positions, bands = bands)
}

#' @rdname open_specs
#' @export
split_spec.FileSpecs <- function(x, by = "region", ...) {
  if (!identical(by, "region"))
    stop("FileSpecs can currently be split only by 'region'", call. = FALSE)

  idx <- .filespec_index(x)
  regions <- unique(as.character(idx$region))
  stats::setNames(lapply(regions, function(region) {
    out <- x
    out$view <- idx$index[idx$region == region]
    out$recipe <- c(x$recipe, list(list(method = "view", by = by,
                                        value = region)))
    transforms <- attr(x, "transformations")
    attr(out, "transformations") <- c(
      transforms, list(list(method = "view", by = by, value = region))
    )
    out
  }), regions)
}

#' @rdname Specs
#' @export
write_specs.FileSpecs <- function(x, file, compress = "xz", ...) {
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = TRUE)
  target <- .filespec_descriptor_target(file)
  .filespec_assert_not_source(x, target)
  if (file.exists(target))
    stop("a FileSpecs descriptor already exists at 'file'; choose a new path",
         call. = FALSE)

  stage <- tempfile(".filespec-", tmpdir = dirname(target), fileext = ".rds")
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  saveRDS(x, file = stage, compress = compress, ...)
  if (!file.rename(stage, target))
    stop("could not atomically publish the FileSpecs descriptor", call. = FALSE)
  invisible(NULL)
}

#' @name io_spec
#' @export
write_spec.FileSpecs <- function(x, file, method = NULL, ...) {
  if (!is.null(method)) {
    stop("custom 'method' writers are disabled for FileSpecs; export to a ",
         "new ENVI target so source-member guards remain enforced",
         call. = FALSE)
  }
  .filespec_write_envi(x, file = file, ...)
}

.filespec_write_envi <- function(
    x, file, chunk_size = getOption("OpenSpecy.filespec.chunk_size", 8192L)) {
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = TRUE)
  targets <- .filespec_envi_targets(file)
  .filespec_assert_not_source(x, targets$header)
  .filespec_assert_not_source(x, targets$binary)
  existing <- c(targets$header, targets$binary)[
    file.exists(c(targets$header, targets$binary))
  ]
  if (length(existing)) {
    stop("refusing to overwrite an existing ENVI output member: ",
         paste(basename(existing), collapse = ", "), call. = FALSE)
  }

  index <- .filespec_index(x)
  regions <- unique(as.character(index$region))
  if (length(regions) != 1L) {
    stop("ENVI export requires a one-region FileSpecs view; use ",
         "split_spec(x, by = \"region\") first", call. = FALSE)
  }
  if (!all(c("row", "col") %in% names(index)))
    stop("ENVI export requires integer 'row' and 'col' coordinates",
         call. = FALSE)
  index[[".view_position"]] <- seq_len(nrow(index))
  data.table::setorder(index, row, col)
  rows <- sort(unique(index$row))
  cols <- sort(unique(index$col))
  complete <- nrow(index) == length(rows) * length(cols) &&
    !anyDuplicated(index[, c("row", "col"), with = FALSE]) &&
    identical(as.integer(rows), seq.int(min(rows), max(rows))) &&
    identical(as.integer(cols), seq.int(min(cols), max(cols)))
  if (!isTRUE(complete)) {
    stop("ENVI export requires a complete rectangular regional view",
         call. = FALSE)
  }
  chunk_size <- as.integer(chunk_size)
  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L)
    stop("'chunk_size' must be one positive integer", call. = FALSE)

  stage_id <- paste0(".filespec-envi-", Sys.getpid(), "-",
                     sample.int(.Machine$integer.max, 1L))
  stage_header <- file.path(dirname(targets$header),
                            paste0(stage_id, ".hdr"))
  stage_binary <- file.path(dirname(targets$binary),
                            paste0(stage_id, ".dat"))
  published_binary <- FALSE
  on.exit({
    unlink(c(stage_header, stage_binary), force = TRUE)
    if (published_binary && !file.exists(targets$header) &&
        file.exists(targets$binary)) {
      unlink(targets$binary, force = TRUE)
    }
  }, add = TRUE)

  con <- file(stage_binary, open = "wb")
  connection_open <- TRUE
  on.exit(if (connection_open) close(con), add = TRUE)
  positions <- index$.view_position
  export_axis <- .filespec_read(x, index = positions[[1L]])$wavenumber
  chunks <- split(positions,
                  ceiling(seq_along(positions) / chunk_size))
  for (selection in chunks) {
    block <- .filespec_read_values(x, index = selection)
    writeBin(as.numeric(block$spectra), con, size = 8L,
             endian = "little")
  }
  close(con)
  connection_open <- FALSE

  header <- c(
    "ENVI",
    "description = {OpenSpecy FileSpecs bounded export}",
    paste0("samples = ", length(cols)),
    paste0("lines = ", length(rows)),
    paste0("bands = ", length(export_axis)),
    "header offset = 0",
    "file type = ENVI Standard",
    "data type = 5",
    "interleave = bip",
    "byte order = 0",
    paste0("data file = ", basename(targets$binary)),
    paste0("wavelength = {",
           paste(trimws(formatC(export_axis, digits = 17L, format = "g",
                                flag = "#")),
                 collapse = ", "), "}")
  )
  writeLines(header, stage_header, useBytes = TRUE)

  if (!file.rename(stage_binary, targets$binary))
    stop("could not atomically publish the ENVI binary member", call. = FALSE)
  published_binary <- TRUE
  if (!file.rename(stage_header, targets$header))
    stop("could not atomically publish the ENVI header member", call. = FALSE)
  published_binary <- FALSE
  invisible(targets)
}

.filespec_envi_targets <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))
    stop("'file' must be one ENVI .hdr, .dat, or .img path", call. = FALSE)
  extension <- tolower(tools::file_ext(file))
  if (!extension %in% c("hdr", "dat", "img"))
    stop("FileSpecs write_spec() supports ENVI .hdr, .dat, or .img targets",
         call. = FALSE)
  directory <- dirname(file)
  if (!dir.exists(directory))
    stop("the ENVI output directory does not exist", call. = FALSE)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  stem <- tools::file_path_sans_ext(basename(file))
  binary_extension <- if (extension %in% c("dat", "img")) extension else "dat"
  list(
    header = file.path(directory, paste0(stem, ".hdr")),
    binary = file.path(directory, paste0(stem, ".", binary_extension))
  )
}

.filespec_stop_unsupported <- function(operation) {
  stop(operation, " is not available for FileSpecs because it would require ",
       "an unsafe whole-source materialization; use a supported file-backed ",
       "pipeline or decompress_spec() with a bounded selection",
       call. = FALSE)
}

.filespec_index <- function(x) {
  .filespec_validate_object(x)
  idx <- if (is.null(x$view)) {
    x$index
  } else {
    positions <- match(x$view, x$index$index)
    x$index[positions]
  }
  data.table::copy(idx)
}

.filespec_regions <- function(x) {
  unique(as.character(.filespec_index(x)$region))
}

.filespec_axis <- function(x) {
  .filespec_validate_object(x)
  as.numeric(x$source$axis)
}

.filespec_n_spectra <- function(x) {
  nrow(.filespec_index(x))
}

.filespec_read <- function(x, index, bands = NULL) {
  values <- .filespec_read_values(x, index = index, bands = bands)
  .filespec_values_to_OpenSpecy(x, values)
}

.filespec_values_to_OpenSpecy <- function(x, values) {
  selected <- values$index
  metadata <- .filespec_materialized_metadata(x$source, selected)
  coords <- metadata[, c("x", "y"), with = FALSE]
  metadata <- metadata[, setdiff(names(metadata), c("x", "y")), with = FALSE]
  out <- as_OpenSpecy(
    values$wavenumber, spectra = values$spectra, metadata = metadata,
    coords = coords, session_id = FALSE, compute_file_id = FALSE
  )
  attr(out, "filespec_source") <- list(
    id = x$source$id,
    backend = x$source$backend,
    generation = x$cache$generation
  )
  out
}

# Convert a target in-memory chunk budget (default ~100MB, double precision
# at 8 bytes/value) into a spectrum count, given the number of spectral
# bands. Used to size reads relative to a byte budget rather than a flat
# spectrum count, since the two diverge widely across instruments.
.filespec_chunk_size_for_bytes <- function(nband, target_bytes = 100 * 1024^2) {
  bytes_per_spectrum <- max(1, as.numeric(nband)) * 8
  max(1L, as.integer(floor(target_bytes / bytes_per_spectrum)))
}

# Positions for a bounded, locality-preserving block of up to `chunk_size`
# spectra around `position`, all from the same region as `position`. Index
# rows within one region are stored in the source's on-disk scan order, so a
# contiguous window of positions is also a spatially local neighborhood.
.filespec_block_positions <- function(index, position, chunk_size) {
  region <- index$region[[position]]
  candidates <- which(index$region == region)
  half <- as.integer(chunk_size) %/% 2L
  lo <- max(min(candidates), position - half)
  hi <- min(max(candidates), lo + as.integer(chunk_size) - 1L)
  lo <- max(min(candidates), hi - as.integer(chunk_size) + 1L)
  candidates[candidates >= lo & candidates <= hi]
}

# Read a block of up to ~100MB (see .filespec_chunk_size_for_bytes())
# containing `position`, instead of a single spectrum. Callers that browse
# pixel-by-pixel (e.g. the Shiny app) cache the returned block and only
# re-read when a later position falls outside it, turning repeat browsing
# within a neighborhood into a cache hit instead of a fresh disk read.
.filespec_read_block <- function(x, index, position, chunk_size = NULL) {
  if (is.null(chunk_size)) {
    chunk_size <- .filespec_chunk_size_for_bytes(length(x$source$axis))
  }
  positions <- .filespec_block_positions(index, position, chunk_size)
  .filespec_read_values(x, index = positions, bands = NULL)
}

.filespec_read_values <- function(x, index, bands = NULL) {
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = FALSE)
  idx <- .filespec_index(x)
  positions <- .filespec_positions(index, nrow(idx), "index")
  selected <- idx[positions]
  bands <- .filespec_bands(bands, length(x$source$axis))
  band_order <- order(x$source$axis[bands])
  bands <- bands[band_order]

  spectra <- switch(
    x$source$backend,
    h5 = .filespec_read_h5(x$source, selected, bands),
    envi = .filespec_read_envi(x$source, selected, bands),
    stop("unsupported FileSpecs backend", call. = FALSE)
  )
  colnames(spectra) <- selected$col_id
  list(wavenumber = x$source$axis[bands], spectra = spectra, index = selected)
}

.filespec_positions <- function(index, n, name) {
  if (!is.numeric(index) || !length(index) || anyNA(index) ||
      any(!is.finite(index)) || any(index != floor(index)) || any(index < 1L)) {
    stop("'", name, "' must be a nonempty positive whole-number vector",
         call. = FALSE)
  }
  index <- as.integer(index)
  if (anyDuplicated(index))
    stop("'", name, "' must not contain duplicate values", call. = FALSE)
  if (any(index > n))
    stop("'", name, "' contains a position outside the FileSpecs view",
         call. = FALSE)
  index
}

.filespec_bands <- function(bands, n) {
  if (is.null(bands))
    return(seq_len(n))
  .filespec_positions(bands, n, "bands")
}

.filespec_roi <- function(roi) {
  if (is.list(roi) && all(c("x", "y") %in% names(roi)) &&
      length(roi$x) == 2L && length(roi$y) == 2L) {
    roi <- c(roi$x, roi$y)
  }
  if (!is.numeric(roi) || length(roi) != 4L || anyNA(roi) ||
      any(!is.finite(roi))) {
    stop("'roi' must be c(xmin, xmax, ymin, ymax) or a list with x/y ranges",
         call. = FALSE)
  }
  if (roi[[1L]] > roi[[2L]] || roi[[3L]] > roi[[4L]])
    stop("'roi' minimum bounds must not exceed maximum bounds",
         call. = FALSE)
  as.numeric(roi)
}

.filespec_source_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("'path' must be one existing H5 or ENVI source path", call. = FALSE)
  if (!file.exists(path) || dir.exists(path))
    stop("FileSpecs source does not exist: ", path, call. = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.filespec_backend <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("h5", "hdf5"))
    return("h5")
  if (extension %in% c("hdr", "dat", "img"))
    return("envi")
  stop("FileSpecs currently supports H5 and ENVI (.hdr/.dat/.img) sources",
       call. = FALSE)
}

.filespec_open_h5 <- function(path) {
  members <- .filespec_member_table(path)
  h5 <- hdf5r::H5File$new(path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  if (!h5$exists("/Regions"))
    stop("H5 FileSpecs source has no /Regions group", call. = FALSE)
  regions <- names(h5[["/Regions"]])
  if (!length(regions))
    stop("H5 FileSpecs source has no regions", call. = FALSE)

  file_metadata <- .read_h5_file_metadata(h5)
  layouts <- vector("list", length(regions))
  index <- vector("list", length(regions))
  region_metadata <- vector("list", length(regions))
  region_extents <- vector("list", length(regions))
  names(layouts) <- names(region_metadata) <- names(region_extents) <- regions
  offset <- 0L
  axis <- NULL

  for (i in seq_along(regions)) {
    region <- regions[[i]]
    dataset_path <- paste0("/Regions/", region, "/Dataset")
    if (!h5$exists(dataset_path))
      stop("H5 region is missing Dataset: ", region, call. = FALSE)
    dataset <- h5[[dataset_path]]
    dims <- .h5_dataset_dims(dataset)
    if (length(dims) != 3L)
      stop("H5 FileSpecs region Dataset must have three dimensions",
           call. = FALSE)
    spectral_dim <- .h5_spectral_dim(dims, file_metadata)
    spatial_dims <- setdiff(seq_along(dims), spectral_dim)
    current_axis <- .h5_wavenumbers(file_metadata, dims[[spectral_dim]])
    if (is.null(axis)) {
      axis <- current_axis
    } else if (!identical(as.numeric(axis), as.numeric(current_axis))) {
      stop("all H5 FileSpecs regions must share one spectral axis",
           call. = FALSE)
    }

    ny <- dims[[spatial_dims[[1L]]]]
    nx <- dims[[spatial_dims[[2L]]]]
    grid <- expand.grid(row = seq_len(ny), col = seq_len(nx))
    particle <- region
    ids <- paste0(region, "_r", grid$row, "c", grid$col)
    n_region <- length(ids)
    index[[i]] <- data.table::data.table(
      index = offset + seq_len(n_region),
      region = region,
      row = as.integer(grid$row),
      col = as.integer(grid$col),
      x = as.numeric(grid$col - 1L),
      y = as.numeric(grid$row - 1L),
      col_id = ids,
      source_id = ids,
      value_id = ids,
      file_name = basename(path),
      particle_id = particle,
      subpixel = seq_len(n_region)
    )
    stage <- .h5_region_stage(h5, region, ny = ny, nx = nx)
    if (is.null(stage)) {
      stage <- .h5_region_stage_from_metadata(
        file_metadata, region_index = i, ny = ny, nx = nx, region = region
      )
    }
    region_extents[[region]] <- stage
    if (!is.null(stage)) {
      index[[i]][["stage_x_nm"]] <- stage$stage_x[grid$col]
      index[[i]][["stage_y_nm"]] <- stage$stage_y[grid$row]
      index[[i]][["stage_z_nm"]] <- stage$stage_z
      index[[i]][["stage_units"]] <- "nm"
    }
    layouts[[region]] <- list(
      dataset = dataset_path,
      dims = dims,
      spectral_dim = spectral_dim,
      spatial_dims = spatial_dims,
      ny = ny,
      nx = nx,
      stage = stage
    )
    region_metadata[[region]] <- .h5_region_metadata(h5, region)
    offset <- offset + n_region
  }

  list(
    source = list(
      backend = "h5",
      members = members,
      axis = as.numeric(axis),
      layout = list(regions = layouts),
      file_metadata = file_metadata,
      region_metadata = region_metadata,
      visual = .filespec_h5_visual_descriptor(h5, region_extents)
    ),
    index = data.table::rbindlist(index, use.names = TRUE, fill = TRUE)
  )
}

.filespec_h5_visual_descriptor <- function(h5, region_extents) {
  if (!h5$exists("/Mosaic")) return(NULL)
  mosaic <- h5[["/Mosaic"]]
  images <- .h5_mosaic_image_names(mosaic)
  centers <- if (h5$exists("/Mosaic/Centers")) {
    tryCatch(data.table::as.data.table(h5[["/Mosaic/Centers"]]$read()),
             error = function(e) NULL)
  } else {
    NULL
  }
  tiles <- .h5_mosaic_stage_tiles(centers)
  if (!is.null(tiles) && nrow(tiles)) {
    tiles <- tiles[tiles$center_index <= length(images)]
    tiles[["source"]] <- paste0("/Mosaic/", images[tiles$center_index])
  }
  region_extents <- Filter(Negate(is.null), region_extents)
  regions <- lapply(region_extents, function(extent) {
    hits <- if (is.null(tiles) || !nrow(tiles)) integer() else
      .h5_intersecting_tiles(tiles, extent)
    list(
      region = extent$region,
      stage_extent = extent[setdiff(names(extent),
                                    c("stage_x", "stage_y"))],
      tiles = if (length(hits)) data.table::copy(tiles[hits]) else
        data.table::data.table(),
      transform = list(
        method = "h5_mosaic_centers",
        stage_units = "nm",
        image_col_axis = "stage_x_increasing",
        image_row_axis = "stage_y_decreasing"
      )
    )
  })
  names(regions) <- vapply(region_extents, `[[`, character(1L), "region")
  list(
    source = "/Mosaic",
    image_datasets = paste0("/Mosaic/", images),
    centers = centers,
    tiles = tiles,
    regions = regions,
    transform = list(
      method = "h5_mosaic_centers",
      stage_units = "nm",
      image_col_axis = "stage_x_increasing",
      image_row_axis = "stage_y_decreasing"
    )
  )
}

.filespec_materialize_visual <- function(x) {
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = FALSE)
  if (!identical(x$source$backend, "h5")) return(NULL)
  descriptor <- visual_image(x)
  if (is.null(descriptor) || is.null(descriptor$stage_extent) ||
      is.null(descriptor$tiles) || !nrow(descriptor$tiles)) {
    return(NULL)
  }
  region <- .filespec_regions(x)
  if (length(region) != 1L) {
    stop("H5 visual materialization requires a one-region FileSpecs view",
         call. = FALSE)
  }
  key <- digest::digest(list(
    schema = "filespec-h5-visual-1",
    source = x$source$id,
    region = region,
    tiles = descriptor$tiles$source,
    transform = descriptor$transform
  ), algo = "sha256")
  cache_file <- .filespec_cache_path(x, "visual", paste0(key, ".rds"))
  if (file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }

  source_path <- x$source$members$path[
    tolower(tools::file_ext(x$source$members$path)) %in% c("h5", "hdf5")
  ][[1L]]
  h5 <- hdf5r::H5File$new(source_path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  if (!h5$exists("/Mosaic")) return(NULL)
  result <- .h5_stitch_region_mosaic(
    h5[["/Mosaic"]], data.table::copy(descriptor$tiles),
    descriptor$stage_extent
  )
  if (!is.null(result)) .filespec_atomic_save_rds(result, cache_file)
  result
}

.filespec_open_envi <- function(path) {
  pair <- .filespec_envi_pair(path)
  members <- .filespec_member_table(c(pair$header, pair$binary))
  header <- .read_envi_header(pair$header)
  required <- c("samples", "lines", "bands", "data type")
  missing <- setdiff(required, names(header))
  if (length(missing))
    stop("ENVI header is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)

  samples <- .filespec_positive_scalar(header$samples, "samples")
  lines <- .filespec_positive_scalar(header$lines, "lines")
  bands <- .filespec_positive_scalar(header$bands, "bands")
  data_type <- .filespec_positive_scalar(header[["data type"]], "data type")
  type <- .filespec_envi_type(data_type)
  interleave <- tolower(trimws(as.character(header$interleave %||% "bsq")))
  if (!interleave %in% c("bsq", "bil", "bip"))
    stop("ENVI interleave must be BSQ, BIL, or BIP", call. = FALSE)
  byte_order <- as.integer(header[["byte order"]] %||% 0L)
  if (!byte_order %in% c(0L, 1L))
    stop("ENVI byte order must be 0 (little) or 1 (big)", call. = FALSE)
  header_offset <- as.numeric(header[["header offset"]] %||% 0)
  if (length(header_offset) != 1L || is.na(header_offset) ||
      header_offset < 0 || header_offset != floor(header_offset)) {
    stop("ENVI header offset must be a nonnegative whole number",
         call. = FALSE)
  }
  expected <- header_offset + as.double(samples) * lines * bands * type$bytes
  binary_size <- file.info(pair$binary)$size
  if (!is.finite(binary_size) || binary_size < expected)
    stop("ENVI binary is shorter than the dimensions declared by its header",
         call. = FALSE)

  axis <- header$wavelength
  if (is.null(axis)) {
    warning("ENVI wavelengths not found, using band index values instead",
            call. = FALSE)
    axis <- seq_len(bands)
  }
  axis <- as.numeric(axis)
  if (length(axis) != bands || anyNA(axis))
    stop("ENVI wavelength count must equal the declared band count",
         call. = FALSE)

  grid <- expand.grid(col = seq_len(samples), row = seq_len(lines))
  ids <- paste0("Region1_r", grid$row, "c", grid$col)
  index <- data.table::data.table(
    index = seq_len(nrow(grid)),
    region = "Region1",
    row = as.integer(grid$row),
    col = as.integer(grid$col),
    x = as.numeric(grid$col - 1L),
    y = as.numeric(grid$row - 1L),
    col_id = ids,
    source_id = ids,
    value_id = ids,
    file_name = basename(pair$binary),
    particle_id = "Region1",
    subpixel = seq_len(nrow(grid))
  )
  header_metadata <- header[setdiff(names(header), "wavelength")]

  list(
    source = list(
      backend = "envi",
      members = members,
      axis = axis,
      layout = list(
        binary = pair$binary,
        header = pair$header,
        samples = samples,
        lines = lines,
        bands = bands,
        data_type = data_type,
        bytes = type$bytes,
        storage = type$storage,
        interleave = interleave,
        endian = if (byte_order == 0L) "little" else "big",
        header_offset = header_offset
      ),
      file_metadata = header_metadata,
      region_metadata = list()
    ),
    index = index
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

.filespec_positive_scalar <- function(x, name) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x < 1L ||
      x != floor(x)) {
    stop("ENVI '", name, "' must be a positive whole number", call. = FALSE)
  }
  as.integer(x)
}

.filespec_envi_type <- function(data_type) {
  types <- list(
    `1` = list(bytes = 1L, storage = "uint8"),
    `2` = list(bytes = 2L, storage = "int16"),
    `3` = list(bytes = 4L, storage = "int32"),
    `4` = list(bytes = 4L, storage = "float32"),
    `5` = list(bytes = 8L, storage = "float64"),
    `12` = list(bytes = 2L, storage = "uint16"),
    `13` = list(bytes = 4L, storage = "uint32")
  )
  out <- types[[as.character(data_type)]]
  if (is.null(out)) {
    stop("ENVI data type ", data_type,
         " is not yet supported for bounded FileSpecs reads", call. = FALSE)
  }
  out
}

.filespec_envi_pair <- function(path) {
  extension <- tolower(tools::file_ext(path))
  directory <- dirname(path)
  if (identical(extension, "hdr")) {
    header <- path
    parsed <- .read_envi_header(header)
    declared <- parsed[["data file"]]
    candidates <- character()
    if (!is.null(declared)) {
      declared <- gsub('^["\']|["\']$', "", trimws(as.character(declared)))
      candidates <- file.path(directory, declared)
    }
    stem <- tools::file_path_sans_ext(basename(header))
    candidates <- c(candidates, file.path(directory,
      paste0(stem, c(".dat", ".img"))))
    binary <- .filespec_unique_existing(candidates, "ENVI binary")
  } else {
    binary <- path
    stem <- tools::file_path_sans_ext(basename(binary))
    candidates <- file.path(directory,
      c(paste0(stem, ".hdr"), paste0(basename(binary), ".hdr")))
    header <- .filespec_unique_existing(candidates, "ENVI header")
  }
  list(
    header = normalizePath(header, winslash = "/", mustWork = TRUE),
    binary = normalizePath(binary, winslash = "/", mustWork = TRUE)
  )
}

.filespec_unique_existing <- function(candidates, label) {
  candidates <- unique(candidates[nzchar(candidates)])
  directory <- unique(dirname(candidates))
  found <- character()
  for (dir in directory) {
    if (!dir.exists(dir)) next
    available <- list.files(dir, full.names = TRUE, all.files = TRUE,
                            no.. = TRUE)
    match_idx <- match(tolower(basename(candidates[dirname(candidates) == dir])),
                       tolower(basename(available)))
    found <- c(found, available[stats::na.omit(match_idx)])
  }
  found <- unique(found[file.exists(found) & !dir.exists(found)])
  if (!length(found))
    stop(label, " paired file was not found", call. = FALSE)
  if (length(found) > 1L)
    stop("multiple candidate ", label, " files were found", call. = FALSE)
  found[[1L]]
}

.filespec_read_h5 <- function(source, selected, bands) {
  h5 <- hdf5r::H5File$new(source$members$path[[1L]], mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  out <- matrix(NA_real_, nrow = length(bands), ncol = nrow(selected))
  region_groups <- split(seq_len(nrow(selected)), selected$region)

  for (region in names(region_groups)) {
    positions <- region_groups[[region]]
    layout <- source$layout$regions[[region]]
    if (is.null(layout))
      stop("FileSpecs index refers to an unknown H5 region", call. = FALSE)
    dataset <- h5[[layout$dataset]]
    region_index <- selected[positions]
    rows <- sort(unique(region_index$row))
    cols <- sort(unique(region_index$col))
    rectangular <- nrow(region_index) == length(rows) * length(cols) &&
      !anyDuplicated(region_index[, c("row", "col"), with = FALSE])
    if (isTRUE(rectangular)) {
      args <- rep(list(quote(expr = )), 3L)
      args[[layout$spectral_dim]] <- bands
      args[[layout$spatial_dims[[1L]]]] <- rows
      args[[layout$spatial_dims[[2L]]]] <- cols
      values <- dataset$read(args = args, drop = FALSE)
      values <- aperm(values, c(layout$spectral_dim, layout$spatial_dims))
      values <- matrix(values, nrow = length(bands),
                       ncol = length(rows) * length(cols))
      grid <- expand.grid(row = rows, col = cols)
      requested <- match(
        paste(region_index$row, region_index$col, sep = ":"),
        paste(grid$row, grid$col, sep = ":")
      )
      out[, positions] <- values[, requested, drop = FALSE]
      next
    }
    col_groups <- split(positions, selected$col[positions])
    for (group in col_groups) {
      args <- rep(list(quote(expr = )), 3L)
      args[[layout$spectral_dim]] <- bands
      args[[layout$spatial_dims[[1L]]]] <- selected$row[group]
      args[[layout$spatial_dims[[2L]]]] <- selected$col[group][[1L]]
      values <- dataset$read(args = args, drop = FALSE)
      values <- aperm(values, c(layout$spectral_dim, layout$spatial_dims))
      out[, group] <- matrix(values, nrow = length(bands),
                             ncol = length(group))
    }
  }
  out
}

.filespec_read_envi <- function(source, selected, bands) {
  layout <- source$layout
  con <- file(layout$binary, open = "rb")
  on.exit(close(con), add = TRUE)
  out <- matrix(NA_real_, nrow = length(bands), ncol = nrow(selected))
  pixel <- (selected$row - 1) * layout$samples + selected$col - 1

  if (identical(layout$interleave, "bip")) {
    for (j in seq_len(nrow(selected))) {
      offset <- layout$header_offset + pixel[[j]] * layout$bands * layout$bytes
      seek(con, where = offset, origin = "start")
      values <- .filespec_envi_read_values(con, layout$bands, layout)
      out[, j] <- values[bands]
    }
  } else if (identical(layout$interleave, "bsq")) {
    ordered <- order(pixel)
    runs <- .filespec_consecutive_runs(pixel[ordered])
    plane <- as.double(layout$lines) * layout$samples
    for (b in seq_along(bands)) {
      band <- bands[[b]] - 1
      for (run in runs) {
        targets <- ordered[run]
        offset <- layout$header_offset +
          (band * plane + pixel[targets[[1L]]]) * layout$bytes
        seek(con, where = offset, origin = "start")
        out[b, targets] <- .filespec_envi_read_values(con, length(targets),
                                                       layout)
      }
    }
  } else {
    line_groups <- split(seq_len(nrow(selected)), selected$row)
    for (line in names(line_groups)) {
      positions <- line_groups[[line]]
      ordered <- positions[order(selected$col[positions])]
      runs <- .filespec_consecutive_runs(selected$col[ordered])
      for (b in seq_along(bands)) {
        band <- bands[[b]] - 1
        for (run in runs) {
          targets <- ordered[run]
          offset_values <- ((as.numeric(line) - 1) * layout$bands + band) *
            layout$samples + selected$col[targets[[1L]]] - 1
          seek(con, where = layout$header_offset +
                 offset_values * layout$bytes, origin = "start")
          out[b, targets] <- .filespec_envi_read_values(con, length(targets),
                                                         layout)
        }
      }
    }
  }
  out
}

.filespec_consecutive_runs <- function(values) {
  if (!length(values)) return(list())
  split(seq_along(values), cumsum(c(TRUE, diff(values) != 1)))
}

.filespec_envi_read_values <- function(con, n, layout) {
  storage <- layout$storage
  endian <- layout$endian
  values <- switch(
    storage,
    uint8 = readBin(con, integer(), n = n, size = 1L, signed = FALSE,
                    endian = endian),
    int16 = readBin(con, integer(), n = n, size = 2L, signed = TRUE,
                    endian = endian),
    int32 = readBin(con, integer(), n = n, size = 4L, signed = TRUE,
                    endian = endian),
    float32 = readBin(con, numeric(), n = n, size = 4L, endian = endian),
    float64 = readBin(con, numeric(), n = n, size = 8L, endian = endian),
    uint16 = readBin(con, integer(), n = n, size = 2L, signed = FALSE,
                     endian = endian),
    uint32 = {
      value <- as.numeric(readBin(con, integer(), n = n, size = 4L,
                                  signed = TRUE, endian = endian))
      value[value < 0] <- value[value < 0] + 2^32
      value
    },
    stop("unsupported ENVI storage type", call. = FALSE)
  )
  if (length(values) != n)
    stop("ENVI source ended before the requested bounded read completed",
         call. = FALSE)
  as.numeric(values)
}

.filespec_materialized_metadata <- function(source, selected) {
  out <- data.table::copy(selected)
  canonical_index_fields <- names(out)
  if (!"file_id" %in% names(out))
    out[["file_id"]] <- source$id

  file_metadata <- source$file_metadata
  if (length(file_metadata)) {
    for (name in setdiff(names(file_metadata), names(out))) {
      out[[name]] <- rep(.filespec_scalar(file_metadata[[name]]), nrow(out))
    }
  }
  if (length(source$region_metadata)) {
    for (region in intersect(unique(out$region),
                             names(source$region_metadata))) {
      metadata <- source$region_metadata[[region]]
      if (!nrow(metadata)) next
      rows <- which(out$region == region)
      for (name in setdiff(names(metadata), canonical_index_fields)) {
        if (!name %in% names(out)) out[[name]] <- NA
        out[[name]][rows] <- .filespec_scalar(metadata[[name]])
      }
    }
  }
  out
}

.filespec_scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NA)
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(x) == 1L) return(x)
  paste(x, collapse = ";")
}

.filespec_member_table <- function(paths) {
  paths <- vapply(paths, .filespec_source_path, FUN.VALUE = character(1L))
  data.table::rbindlist(lapply(paths, function(path) {
    fingerprint <- .filespec_fingerprint(path, strong = TRUE)
    data.table::as.data.table(fingerprint)
  }))
}

.filespec_fingerprint <- function(path, strong = TRUE) {
  info <- file.info(path)
  if (!nrow(info) || is.na(info$size) || is.na(info$mtime))
    stop("could not fingerprint FileSpecs source member: ", path,
         call. = FALSE)
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    sha256 = if (isTRUE(strong)) {
      digest::digest(path, algo = "sha256", file = TRUE)
    } else {
      NA_character_
    }
  )
}

.filespec_validate_members <- function(members, strong = FALSE) {
  for (i in seq_len(nrow(members))) {
    expected <- members[i]
    if (!file.exists(expected$path) || dir.exists(expected$path))
      stop("FileSpecs source member is missing: ", expected$path,
           call. = FALSE)
    observed <- .filespec_fingerprint(expected$path, strong = strong)
    if (!identical(as.numeric(observed$size), as.numeric(expected$size)) ||
        abs(as.numeric(observed$mtime) - as.numeric(expected$mtime)) > 1e-6) {
      stop("FileSpecs source member changed since it was opened: ",
           expected$path, call. = FALSE)
    }
    if (isTRUE(strong) && !identical(observed$sha256, expected$sha256))
      stop("FileSpecs source content fingerprint changed: ", expected$path,
           call. = FALSE)
  }
  invisible(TRUE)
}

.filespec_validate_source <- function(x, strong = FALSE) {
  .filespec_validate_object(x)
  .filespec_validate_members(x$source$members, strong = strong)
}

.filespec_validate_object <- function(x) {
  if (!inherits(x, "FileSpecs") || !inherits(x, "Specs"))
    stop("object is not a FileSpecs Specs subtype", call. = FALSE)
  expected_names <- c("source", "index", "view", "cache", "recipe")
  if (!identical(names(x), expected_names))
    stop("FileSpecs descriptor components are invalid", call. = FALSE)
  if (!is.list(x$source) ||
      !x$source$backend %in% c("h5", "envi") ||
      !is.numeric(x$source$axis) || !length(x$source$axis) ||
      !is.list(x$source$layout)) {
    stop("FileSpecs source descriptor is invalid", call. = FALSE)
  }
  members <- x$source$members
  if (!inherits(members, "data.frame") || !nrow(members) ||
      !all(c("path", "size", "mtime", "sha256") %in% names(members)) ||
      anyNA(members[, c("path", "size", "mtime", "sha256")])) {
    stop("FileSpecs source-member fingerprints are invalid", call. = FALSE)
  }
  if (any(!grepl("^[[:xdigit:]]{64}$", members$sha256)))
    stop("FileSpecs source-member strong fingerprints are invalid",
         call. = FALSE)
  required_index <- c("index", "region", "row", "col", "x", "y", "col_id",
                      "source_id", "value_id")
  if (!data.table::is.data.table(x$index) || !nrow(x$index) ||
      !all(required_index %in% names(x$index)) ||
      anyNA(x$index[, required_index, with = FALSE]) ||
      anyDuplicated(x$index$index) || anyDuplicated(x$index$col_id)) {
    stop("FileSpecs source index is invalid", call. = FALSE)
  }
  if (!is.null(x$view)) {
    if (!is.numeric(x$view) || anyNA(x$view) || anyDuplicated(x$view) ||
        any(!x$view %in% x$index$index)) {
      stop("FileSpecs view index is invalid", call. = FALSE)
    }
  }
  if (!is.list(x$cache) || !is.character(x$cache$root) ||
      length(x$cache$root) != 1L || !nzchar(x$cache$root) ||
      !is.character(x$cache$generation) ||
      length(x$cache$generation) != 1L || !is.list(x$recipe)) {
    stop("FileSpecs cache or recipe descriptor is invalid", call. = FALSE)
  }
  invisible(TRUE)
}

.filespec_cache_root <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tools::R_user_dir("OpenSpecy", "cache"),
                           "filespecs")
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    stop("'cache_dir' must be one directory path", call. = FALSE)
  }
  if (file.exists(cache_dir) && !dir.exists(cache_dir))
    stop("'cache_dir' points to a file", call. = FALSE)
  if (!dir.exists(cache_dir) && !dir.create(cache_dir, recursive = TRUE,
                                             showWarnings = FALSE)) {
    stop("could not create the FileSpecs cache directory", call. = FALSE)
  }
  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

.filespec_cache_path <- function(x, ...) {
  .filespec_validate_object(x)
  components <- as.character(list(...))
  if (!length(components) || anyNA(components) || any(!nzchar(components)))
    stop("cache path components must be nonempty", call. = FALSE)
  separators <- strsplit(components, "[/\\\\]", perl = TRUE)
  if (any(grepl("^(?:[A-Za-z]:|[/\\\\])", components, perl = TRUE)) ||
      any(vapply(separators, function(parts) {
        any(!nzchar(parts) | parts %in% c(".", ".."))
      }, logical(1)))) {
    stop("cache path escapes the FileSpecs cache root", call. = FALSE)
  }
  root <- normalizePath(x$cache$root, winslash = "/", mustWork = FALSE)
  candidate <- normalizePath(do.call(file.path, c(list(root), components)),
                             winslash = "/", mustWork = FALSE)
  prefix <- paste0(tolower(root), "/")
  if (!identical(tolower(candidate), tolower(root)) &&
      !startsWith(tolower(candidate), prefix)) {
    stop("cache path escapes the FileSpecs cache root", call. = FALSE)
  }
  structure(candidate, filespec_cache_root = root)
}

.filespec_atomic_save_rds <- function(object, path, compress = FALSE) {
  root <- attr(path, "filespec_cache_root", exact = TRUE)
  if (is.null(root) || !is.character(root) || length(root) != 1L)
    stop("'path' must come directly from .filespec_cache_path()",
         call. = FALSE)
  target <- normalizePath(as.character(path), winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!startsWith(tolower(target), paste0(tolower(root), "/")))
    stop("cache path escapes the FileSpecs cache root", call. = FALSE)
  if (file.exists(target)) return(target)
  parent <- dirname(target)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE,
                                          showWarnings = FALSE)) {
    stop("could not create the FileSpecs cache namespace", call. = FALSE)
  }
  lock_root <- file.path(root, "locks")
  dir.create(lock_root, recursive = TRUE, showWarnings = FALSE)
  lock <- file.path(lock_root, paste0(digest::digest(target), ".lock"))
  owned <- dir.create(lock, recursive = FALSE, showWarnings = FALSE)
  if (!owned)
    stop("FileSpecs cache path is locked by another operation", call. = FALSE)
  on.exit(if (owned && dir.exists(lock)) {
    unlink(lock, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  if (file.exists(target)) return(target)

  stage <- tempfile(".filespec-", tmpdir = parent, fileext = ".rds")
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  saveRDS(object, stage, compress = compress)
  if (!file.rename(stage, target))
    stop("could not atomically publish the FileSpecs cache value",
         call. = FALSE)
  target
}

.filespec_cache_key <- function(x, key) {
  digest::digest(list(
    schema = x$cache$schema,
    source = x$source$id,
    generation = x$cache$generation,
    recipe = x$recipe,
    key = key
  ), algo = "sha256")
}

.filespec_acquire_lock <- function(x, key) {
  lock_root <- .filespec_cache_path(x, "locks")
  dir.create(lock_root, recursive = TRUE, showWarnings = FALSE)
  lock <- .filespec_cache_path(x, "locks",
                               paste0(.filespec_cache_key(x, key), ".lock"))
  if (!dir.create(lock, recursive = FALSE, showWarnings = FALSE))
    stop("FileSpecs cache generation is locked by another operation",
         call. = FALSE)
  lock
}

.filespec_release_lock <- function(x, lock) {
  expected_root <- .filespec_cache_path(x, "locks")
  lock <- normalizePath(lock, winslash = "/", mustWork = FALSE)
  if (!startsWith(tolower(lock), paste0(tolower(expected_root), "/")))
    stop("refusing to release a lock outside the FileSpecs cache root",
         call. = FALSE)
  if (dir.exists(lock)) unlink(lock, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

.filespec_cache_commit <- function(x, key, writer) {
  if (!is.function(writer))
    stop("'writer' must be a function", call. = FALSE)
  generation_root <- .filespec_cache_path(x, "generations")
  dir.create(generation_root, recursive = TRUE, showWarnings = FALSE)
  key_hash <- .filespec_cache_key(x, key)
  target <- .filespec_cache_path(x, "generations", key_hash)
  manifest <- file.path(target, "manifest.rds")
  if (file.exists(manifest)) return(target)
  if (dir.exists(target))
    stop("an incomplete unowned cache generation already exists",
         call. = FALSE)

  lock <- .filespec_acquire_lock(x, key)
  on.exit(.filespec_release_lock(x, lock), add = TRUE)
  if (file.exists(manifest)) return(target)
  stage <- tempfile(paste0(".", key_hash, "-"), tmpdir = generation_root)
  if (!dir.create(stage, showWarnings = FALSE))
    stop("could not create a FileSpecs cache staging directory",
         call. = FALSE)
  on.exit(if (dir.exists(stage)) unlink(stage, recursive = TRUE, force = TRUE),
          add = TRUE)

  result <- writer(stage)
  files <- list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  saveRDS(list(
    schema = "filespec-cache-manifest-1",
    source_id = x$source$id,
    key = key,
    result = result,
    files = files
  ), file.path(stage, "manifest.rds"), compress = FALSE)
  if (!file.rename(stage, target))
    stop("could not atomically publish the FileSpecs cache generation",
         call. = FALSE)
  target
}

.filespec_descriptor_target <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))
    stop("'file' must be one descriptor path", call. = FALSE)
  directory <- dirname(file)
  if (!dir.exists(directory))
    stop("the FileSpecs descriptor directory does not exist", call. = FALSE)
  file.path(normalizePath(directory, winslash = "/", mustWork = TRUE),
            basename(file))
}

.filespec_assert_not_source <- function(x, path) {
  source_paths <- tolower(normalizePath(x$source$members$path, winslash = "/",
                                        mustWork = TRUE))
  candidate <- tolower(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (candidate %in% source_paths)
    stop("refusing to write over a FileSpecs source member", call. = FALSE)
  invisible(TRUE)
}
