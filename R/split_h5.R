#' Split a large HDF5 spectral map by metadata category
#'
#' `split_h5()` groups an H5 or HDF5 spectral map by a metadata field. The
#' default region-to-H5 path discovers region groups directly and copies them
#' without hashing the complete source, building a per-pixel index, or loading
#' spectral values into R. Other metadata fields use a file-backed
#' [FileSpecs][open_specs] descriptor. `format = "rds"` reads and saves one
#' category at a time; each RDS category must fit in memory.
#'
#' Output files are named `<input stem>_<field category>.<format>`. Characters
#' that are not valid in Windows file names are replaced with underscores.
#' Existing output files are never overwritten. Native H5 output requires each
#' category to contain complete source regions because the source schema stores
#' spectra as three-dimensional regional grids. Use `format = "rds"` for a
#' field that divides pixels within a region. File-level metadata and selected
#' region groups are retained in H5 outputs. When registered mosaic metadata is
#' available, each H5 output also retains the intersecting image tiles and their
#' corresponding centers so visual-image registration remains self-contained.
#'
#' @param file path to one `.h5` or `.hdf5` file.
#' @param field metadata field used to group spectra. Defaults to `"region"`.
#' @param output_dir directory in which to save the output files. Defaults to the
#'   source file's directory.
#' @param format output format. `"h5"` (the default) uses native HDF5 object
#'   copies for whole-region categories. `"rds"` materializes one category as
#'   an `OpenSpecy` object before saving it.
#'
#' @return Invisibly, a named character vector of output paths. Names are the
#'   original field-category labels. H5 outputs remain readable by [read_h5()]
#'   and [open_specs()]; each RDS file contains one `OpenSpecy` object.
#'
#' @examples
#' \dontrun{
#' split_h5("large_map.h5")
#' split_h5("large_map.h5", field = "particle_id", output_dir = "regions",
#'          format = "rds")
#' }
#'
#' @seealso [open_specs()], [decompress_spec()]
#' @export
split_h5 <- function(file, field = "region", output_dir = dirname(file),
                     format = c("h5", "rds")) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(file)) {
    stop("'file' must be one H5 or HDF5 path", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(file))
  if (!extension %in% c("h5", "hdf5"))
    stop("'file' must have an .h5 or .hdf5 extension", call. = FALSE)
  file <- .filespec_source_path(file)
  if (!is.character(field) || length(field) != 1L || is.na(field) ||
      !nzchar(field)) {
    stop("'field' must be one nonempty metadata field name", call. = FALSE)
  }
  format <- match.arg(format)
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must be one nonempty directory path", call. = FALSE)
  }
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("could not create 'output_dir'", call. = FALSE)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  if (identical(format, "h5") && identical(field, "region"))
    return(.split_h5_regions_native(file, output_dir))

  source <- open_specs(file)
  index <- .filespec_index(source)
  if (field %in% names(index)) {
    values <- index[[field]]
  } else {
    metadata <- .filespec_materialized_metadata(source$source, index)
    if (!field %in% names(metadata)) {
      stop("metadata field not found: ", field, ". Available fields: ",
           paste(names(metadata), collapse = ", "), call. = FALSE)
    }
    values <- metadata[[field]]
    rm(metadata)
  }
  categories <- unique(values)
  labels <- as.character(categories)
  labels[is.na(categories)] <- "NA"
  labels[!nzchar(labels)] <- "blank"
  paths <- .split_h5_paths(file, labels, output_dir, format)

  groups <- match(values, categories)
  names(paths) <- labels
  if (identical(format, "h5")) {
    groups_by_region <- split(groups, as.character(index$region))
    split_regions <- names(groups_by_region)[vapply(
      groups_by_region, function(x) length(unique(x)) != 1L, logical(1L)
    )]
    if (length(split_regions)) {
      stop("H5 output requires every field category to contain complete ",
           "regions; field '", field, "' divides region(s): ",
           paste(split_regions, collapse = ", "),
           ". Use format = \"rds\" for within-region categories.",
           call. = FALSE)
    }
  }

  h5_input <- NULL
  if (identical(format, "h5")) {
    h5_input <- hdf5r::H5File$new(file, mode = "r")
    on.exit(try(h5_input$close_all(), silent = TRUE), add = TRUE)
  }

  for (i in seq_along(categories)) {
    positions <- which(groups == i)
    if (identical(format, "h5")) {
      regions <- unique(as.character(index$region[positions]))
      message("split_h5: copying ", labels[[i]], " (", length(positions),
              " spectra in ", length(regions), " region(s); ", i, "/",
              length(categories), ")")
      .split_h5_atomic_copy(h5_input, regions, paths[[i]])
    } else {
      message("split_h5: reading ", labels[[i]], " (", length(positions),
              " spectra; ", i, "/", length(categories), ")")
      object <- decompress_spec(source, index = positions)
      .split_h5_atomic_save_rds(object, paths[[i]])
      rm(object)
      gc(verbose = FALSE)
    }
  }

  invisible(paths)
}

.split_h5_regions_native <- function(file, output_dir) {
  input <- hdf5r::H5File$new(file, mode = "r")
  on.exit(try(input$close_all(), silent = TRUE), add = TRUE)
  if (!input$exists("/Regions"))
    stop("H5 source has no /Regions group", call. = FALSE)
  regions <- names(input[["/Regions"]])
  if (!length(regions))
    stop("H5 source has no regions", call. = FALSE)
  paths <- .split_h5_paths(file, regions, output_dir, "h5")

  for (i in seq_along(regions)) {
    message("split_h5: copying ", regions[[i]], " (", i, "/",
            length(regions), ")")
    .split_h5_atomic_copy(input, regions[[i]], paths[[i]])
  }
  invisible(paths)
}

.split_h5_paths <- function(file, labels, output_dir, format) {
  file_labels <- vapply(labels, .split_h5_file_label, character(1L))
  if (anyDuplicated(file_labels)) {
    duplicated_labels <- unique(file_labels[duplicated(file_labels) |
                                              duplicated(file_labels,
                                                         fromLast = TRUE)])
    stop("field categories produce duplicate output names after filename ",
         "sanitization: ", paste(duplicated_labels, collapse = ", "),
         call. = FALSE)
  }
  stem <- tools::file_path_sans_ext(basename(file))
  paths <- file.path(output_dir,
                     paste0(stem, "_", file_labels, ".", format))
  existing <- paths[file.exists(paths)]
  if (length(existing)) {
    stop("refusing to overwrite existing output file(s): ",
         paste(basename(existing), collapse = ", "), call. = FALSE)
  }
  names(paths) <- labels
  paths
}

.split_h5_file_label <- function(x) {
  x <- gsub("[[:cntrl:]<>:\"/\\\\|?*]+", "_", x, perl = TRUE)
  x <- gsub("[. ]+$", "", x, perl = TRUE)
  x <- gsub("_+", "_", x, perl = TRUE)
  if (!nzchar(x)) x <- "category"
  if (nchar(x, type = "width") > 100L) {
    suffix <- substr(digest::digest(x, algo = "sha256"), 1L, 8L)
    x <- paste0(substr(x, 1L, 90L), "_", suffix)
  }
  x
}

.split_h5_atomic_save_rds <- function(object, path) {
  stage <- tempfile(".split-h5-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  saveRDS(object, stage)
  if (!file.rename(stage, path))
    stop("could not atomically publish output file: ", path, call. = FALSE)
  invisible(path)
}

.split_h5_atomic_copy <- function(input, regions, path) {
  stage <- tempfile(".split-h5-", tmpdir = dirname(path), fileext = ".h5")
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  output <- hdf5r::H5File$new(stage, mode = "w")
  output_open <- TRUE
  on.exit(if (output_open) try(output$close_all(), silent = TRUE), add = TRUE)

  if (input$exists("/FileInfo"))
    output$obj_copy_from(input, "/FileInfo", "/FileInfo")
  output$create_group("/Regions")
  for (region in regions) {
    source_path <- paste0("/Regions/", region)
    output$obj_copy_from(input, source_path, source_path)
  }
  .split_h5_copy_mosaic(input, output, regions)
  output$flush()
  output$close_all()
  output_open <- FALSE

  if (!file.rename(stage, path))
    stop("could not atomically publish output file: ", path, call. = FALSE)
  invisible(path)
}

.split_h5_copy_mosaic <- function(input, output, regions) {
  if (!input$exists("/Mosaic")) return(invisible(FALSE))
  mosaic <- input[["/Mosaic"]]
  image_names <- .h5_mosaic_image_names(mosaic)
  if (!length(image_names) || !input$exists("/Mosaic/Centers")) {
    output$obj_copy_from(input, "/Mosaic", "/Mosaic")
    return(invisible(TRUE))
  }
  centers <- tryCatch(input[["/Mosaic/Centers"]]$read(),
                      error = function(e) NULL)
  tiles <- tryCatch(.h5_mosaic_stage_tiles(centers), error = function(e) NULL)
  if (is.null(centers) || is.null(tiles) || !nrow(tiles)) {
    output$obj_copy_from(input, "/Mosaic", "/Mosaic")
    return(invisible(TRUE))
  }

  file_meta <- .read_h5_file_metadata(input)
  extents <- lapply(seq_along(regions), function(i) {
    region <- regions[[i]]
    dataset <- input[[paste0("/Regions/", region, "/Dataset")]]
    dims <- .h5_dataset_dims(dataset)
    spectral_dim <- .h5_spectral_dim(dims, file_meta)
    spatial_dims <- setdiff(seq_along(dims), spectral_dim)
    extent <- .h5_region_stage(
      input, region, ny = dims[[spatial_dims[[1L]]]],
      nx = dims[[spatial_dims[[2L]]]]
    )
    if (is.null(extent)) {
      extent <- .h5_region_stage_from_metadata(
        file_meta, region_index = match(region, names(input[["/Regions"]])),
        ny = dims[[spatial_dims[[1L]]]], nx = dims[[spatial_dims[[2L]]]],
        region = region
      )
    }
    extent
  })
  extents <- Filter(Negate(is.null), extents)
  if (!length(extents)) {
    output$obj_copy_from(input, "/Mosaic", "/Mosaic")
    return(invisible(TRUE))
  }
  hits <- sort(unique(unlist(lapply(extents, function(extent) {
    .h5_intersecting_tiles(tiles, extent)
  }), use.names = FALSE)))
  hits <- hits[hits <= length(image_names)]
  if (!length(hits)) return(invisible(FALSE))

  output_mosaic <- output$create_group("/Mosaic")
  for (attr_name in hdf5r::h5attr_names(mosaic) %||% character()) {
    attr_value <- tryCatch(
      hdf5r::h5attr(mosaic, attr_name), error = function(e) NULL
    )
    if (!is.null(attr_value)) {
      output_mosaic$create_attr(attr_name, robj = attr_value)
    }
  }
  if (is.matrix(centers) || is.data.frame(centers)) {
    output_mosaic[["Centers"]] <- centers[hits, , drop = FALSE]
  } else if (length(centers) %% nrow(tiles) == 0L) {
    matrix_centers <- matrix(centers, nrow = nrow(tiles), byrow = TRUE)
    output_mosaic[["Centers"]] <- matrix_centers[hits, , drop = FALSE]
  } else {
    output$obj_copy_from(input, "/Mosaic/Centers", "/Mosaic/Centers")
  }
  for (i in seq_along(hits)) {
    source <- paste0("/Mosaic/", image_names[[hits[[i]]]])
    target <- paste0("/Mosaic/Image", i - 1L)
    output$obj_copy_from(input, source, target)
  }
  other <- setdiff(names(mosaic), c("Centers", image_names))
  for (name in other) {
    output$obj_copy_from(input, paste0("/Mosaic/", name),
                         paste0("/Mosaic/", name))
  }
  invisible(TRUE)
}
