#' @title Read ENVI data
#'
#' @description
#' This function allows ENVI data import.
#'
#' @param file name of the binary file.
#' @param header name of the ASCII header file. If `NULL`, the name of the
#' header file is guessed by looking for a second file with the same basename as
#' `file` but with .hdr extension.
#' @param spectral_smooth logical value determines whether spectral smoothing
#' will be performed.
#' @param sigma if \code{spectral_smooth} then this option applies the 3d
#' standard deviations for the \code{gaussianSmooth} function from the
#' \code{mmand} package to describe how spectral smoothing occurs on each dimension.
#' The first two dimensions are x and y, the third is the wavenumbers.
#' @param representation return an ordinary dense \code{"OpenSpecy"} (the
#' default) or an in-memory compact \code{"Specs"} map.
#' @param background_filter optional policy returned by
#' \code{\link{specs_background_filter}()}. In compact mode, BIP maps are read
#' in halo-aware row blocks and only foreground spectra are retained; source
#' mapping 0 records every rejected or non-finite background pixel.
#' @param metadata a named list of the metadata; see
#' \code{\link{as_OpenSpecy}()} for details.
#' @param \ldots further arguments passed to the submethods.
#'
#' @details
#' ENVI data usually consists of two files, an ASCII header and a binary data
#' file. The header contains all information necessary for correctly reading
#' the binary file via \code{\link[caTools]{read.ENVI}()}.
#'
#' @return
#' An `OpenSpecy` object, or a compact `Specs` object when requested.
#'
#' @author Zacharias Steinmetz, Claudia Beleites
#'
#' @seealso
#' \code{\link{read_spec}()} for reading .json, .rds, or .csv (OpenSpecy)
#' files;
#' \code{\link{read_text}()}, \code{\link{read_asp}()}, \code{\link{read_spa}()},
#' \code{\link{read_spc}()}, and \code{\link{read_jdx}()} for text files, .asp,
#' .spa, .spa, .spc, and .jdx formats, respectively;
#' \code{\link{read_opus}()} for reading .0 (OPUS) files;
#' \code{\link{read_zip}()} and \code{\link{read_any}()} for wrapper functions;
#' \code{\link[caTools]{read.ENVI}()}
#' \code{\link[mmand]{gaussianSmooth}()}
#'
#' @importFrom utils modifyList
#' @importFrom caTools read.ENVI
#' @importFrom mmand gaussianSmooth
#' @export
read_envi <- function(file, header = NULL, 
                       spectral_smooth = F, sigma = c(1,1,1),
                       representation = c("OpenSpecy", "Specs"),
                       background_filter = NULL,
                       metadata = list(
                        file_name = basename(file),
                        user_name = NULL,
                        contact_info = NULL,
                        organization = NULL,
                        citation = NULL,
                        spectrum_type = NULL,
                        spectrum_identity = NULL,
                        material_form = NULL,
                        material_phase = NULL,
                        material_producer = NULL,
                        material_purity = NULL,
                        material_quality = NULL,
                        material_color = NULL,
                        material_other = NULL,
                        cas_number = NULL,
                        instrument_used = NULL,
                        instrument_accessories = NULL,
                        instrument_mode = NULL,
                        spectral_resolution = NULL,
                        laser_light_used = NULL,
                        number_of_accumulations = NULL,
                        total_acquisition_time_s = NULL,
                        data_processing_procedure = NULL,
                        level_of_confidence_in_identification = NULL,
                        other_info = NULL,
                        license = "CC BY-NC"),
                      ...) {
  if(is.null(header))
    header <- sub(pattern = "(.*)\\..*$", replacement = "\\1", file) |>
      paste0(".hdr")
  
  

  hdr <- .read_envi_header(header)
  representation <- match.arg(representation)
  background_filter <- .validate_specs_background_filter(background_filter)
  if (identical(representation, "Specs")) {
    return(.read_envi_specs(
      file = file, header = header, hdr = hdr,
      spectral_smooth = spectral_smooth,
      sigma = sigma, metadata = metadata,
      background_filter = background_filter
    ))
  }
  if(spectral_smooth) {
    arr <- read.ENVI(file, header)
    arr <- gaussianSmooth(arr, sigma)
    dims <- dim(arr)
    ny <- dims[1]
    nx <- dims[2]
    n_bands <- dims[3]
    spectra <- matrix(aperm(arr, c(3, 2, 1)),
                      nrow = n_bands, ncol = ny * nx)
  } else {
    ny <- as.integer(hdr[["lines"]])
    nx <- as.integer(hdr[["samples"]])
    n_bands <- as.integer(hdr[["bands"]])
    spectra <- .read_envi_spectra(file, hdr)
  }

  md <- hdr[names(hdr) != "wavelength"]

  coords <- data.frame(
    y = as.numeric(rep(seq_len(ny) - 1, each = nx)),
    x = as.numeric(rep(seq_len(nx) - 1, times = ny))
  )
  colnames(spectra) <- paste(coords$y, coords$x, sep = "_")

  if("wavelength" %in% names(hdr)) {
      wavenumbers <- hdr$wavelength
  } else if(is.character(file) && length(file) == 1L &&
            grepl("\\.img$", file) &&
            file.exists(gsub("\\.img$", ".parms", file))) {
      metadata <- readLines(gsub("\\.img$", ".parms", file))
      names <- gsub("=.*", "", metadata)
      vals <- gsub(".*=", "", metadata)
      df_metadata <- as.data.frame(t(vals))
      colnames(df_metadata) <- names
      wavenumbers <- seq(to = as.numeric(df_metadata[["LXV"]]),
                         from = as.numeric(df_metadata[["FXV"]]),
                         length.out = as.numeric(df_metadata[["NPT"]]))
  } else {
      wavenumbers <- NULL
  }

  if(is.null(wavenumbers)) {
    warning("wavenumbers not found, using index values instead")
    wavenumbers <- seq_len(n_bands)
  }

  collapse_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    if (length(x) == 1) return(x)
    paste(x, collapse = ";")
  }

  if (is.list(metadata))
    metadata <- metadata[!vapply(metadata, is.null, logical(1))]

  meta <- lapply(c(metadata, md), collapse_scalar)
  meta <- data.table::as.data.table(meta)[rep(1L, nrow(coords))]

  os <- as_OpenSpecy(x = wavenumbers,
                     spectra = spectra,
                     metadata = meta,
                     coords = coords,
                     session_id = T,
                     ...)

  return(os)
}

.read_envi_specs <- function(file, header, hdr, spectral_smooth, sigma, metadata,
                             background_filter, block_rows = 32L) {
  ny <- as.integer(hdr[["lines"]])
  nx <- as.integer(hdr[["samples"]])
  n_bands <- as.integer(hdr[["bands"]])
  if (anyNA(c(ny, nx, n_bands)) || any(c(ny, nx, n_bands) <= 0L)) {
    stop("read.ENVI: data sizes missing or incorrect", call. = FALSE)
  }
  wavenumbers <- .envi_wavenumbers(hdr, file, n_bands)
  source_metadata <- .envi_specs_metadata(metadata, hdr, ny * nx)
  policy <- .validate_specs_background_filter(background_filter)

  if (is.null(policy)) {
    if (isTRUE(spectral_smooth)) {
      if (inherits(file, "connection")) {
        stop("spectral smoothing without a background filter requires a file path",
             call. = FALSE)
      }
      cube <- mmand::gaussianSmooth(caTools::read.ENVI(file, header),
                                    sigma = sigma)
      spectra <- matrix(aperm(cube, c(3, 2, 1)), nrow = n_bands,
                        ncol = ny * nx)
    } else {
      spectra <- .read_envi_spectra(file, hdr)
    }
    mapping <- seq_len(ny * nx)
    return(.envi_build_specs(
      spectra, wavenumbers, nx, ny, mapping, source_metadata,
      background = NULL
    ))
  }

  interleave <- tolower(gsub("[[:space:]]+", "",
                             hdr[["interleave"]] %||% "bsq"))
  if (identical(interleave, "bip")) {
    streamed <- .read_envi_bip_background(
      file, hdr, wavenumbers, policy, block_rows = block_rows,
      return_smoothed = isTRUE(spectral_smooth)
    )
  } else {
    raw <- .read_envi_spectra(file, hdr)
    cube <- aperm(array(raw, dim = c(n_bands, nx, ny)), c(3, 2, 1))
    basis <- if (is.null(policy$sigma)) cube else
      mmand::gaussianSmooth(cube, sigma = policy$sigma)
    classify <- matrix(aperm(basis, c(3, 2, 1)), nrow = n_bands,
                       ncol = ny * nx)
    values <- if (isTRUE(spectral_smooth)) classify else raw
    metric_object <- as_OpenSpecy(
      wavenumbers, spectra = classify,
      metadata = data.frame(x = rep(seq_len(nx) - 1L, times = ny),
                            y = rep(seq_len(ny) - 1L, each = nx))
    )
    snr <- sig_noise(metric_object, metric = policy$metric,
                     step = policy$step, spatial_smooth = FALSE, abs = FALSE)
    streamed <- .envi_background_result(values, snr, policy)
  }

  .envi_build_specs(
    streamed$values, wavenumbers, nx, ny, streamed$mapping,
    source_metadata, background = streamed$background
  )
}

.envi_wavenumbers <- function(hdr, file, n_bands) {
  if ("wavelength" %in% names(hdr)) return(as.numeric(hdr$wavelength))
  if (is.character(file) && length(file) == 1L && grepl("\\.img$", file) &&
      file.exists(gsub("\\.img$", ".parms", file))) {
    lines <- readLines(gsub("\\.img$", ".parms", file))
    keys <- gsub("=.*", "", lines)
    vals <- gsub(".*=", "", lines)
    parms <- as.list(vals)
    names(parms) <- keys
    return(seq(from = as.numeric(parms[["FXV"]]),
               to = as.numeric(parms[["LXV"]]),
               length.out = as.numeric(parms[["NPT"]])))
  }
  warning("wavenumbers not found, using index values instead", call. = FALSE)
  seq_len(n_bands)
}

.envi_specs_metadata <- function(metadata, hdr, n_source) {
  collapse_scalar <- function(x) {
    if (is.null(x) || !length(x)) return(NA)
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    if (length(x) == 1L) return(x)
    paste(x, collapse = ";")
  }
  if (is.list(metadata))
    metadata <- metadata[!vapply(metadata, is.null, logical(1))]
  header_metadata <- hdr[setdiff(names(hdr), "wavelength")]
  one <- data.table::as.data.table(lapply(c(metadata, header_metadata),
                                          collapse_scalar))
  model <- .encode_specs_metadata(one)
  model$n <- as.integer(n_source)
  model
}

.envi_build_specs <- function(values, wavenumbers, nx, ny, mapping,
                              source_metadata, background = NULL) {
  value_ids <- paste0("V", seq_len(ncol(values)))
  colnames(values) <- value_ids
  regions <- data.table::data.table(
    name = "Region1", n = as.integer(nx * ny), nx = as.integer(nx),
    ny = as.integer(ny), x_origin = 0, y_origin = 0,
    x_step = 1, y_step = 1, id_prefix = ""
  )
  coords <- .compact_specs_coords(
    regions, mapping,
    source_id = paste(rep(seq_len(ny) - 1L, each = nx),
                      rep(seq_len(nx) - 1L, times = ny), sep = "_")
  )
  out <- Specs(
    variables = wavenumbers, values = values, coords = coords,
    metadata = data.table::data.table(value_id = value_ids),
    attributes = list(source_metadata = source_metadata,
                      background = background)
  )
  if (is.null(background)) return(out)
  .append_specs_transformation(out, list(
    method = "background", retained = ncol(values),
    suppressed = sum(mapping == 0L), metric = background$policy$metric,
    minimum = background$policy$minimum,
    maximum = background$policy$maximum,
    sigma = background$policy$sigma, lossy = TRUE
  ))
}

.envi_background_result <- function(values, snr, policy) {
  keep <- is.finite(snr) & snr > policy$minimum & snr < policy$maximum
  reason <- integer(length(snr))
  reason[!is.finite(snr)] <- 3L
  reason[is.finite(snr) & snr <= policy$minimum] <- 1L
  reason[is.finite(snr) & snr >= policy$maximum] <- 2L
  mapping <- integer(length(snr))
  mapping[keep] <- seq_len(sum(keep))
  list(
    values = values[, keep, drop = FALSE], mapping = mapping,
    background = list(
      mask = !keep, signal_to_noise = as.numeric(snr), reason = reason,
      reason_levels = c("foreground", "below_minimum", "above_maximum",
                        "nonfinite"), policy = policy
    )
  )
}

.read_envi_bip_background <- function(file, hdr, wavenumbers, policy,
                                      block_rows = 32L,
                                      return_smoothed = FALSE) {
  nx <- as.integer(hdr[["samples"]])
  ny <- as.integer(hdr[["lines"]])
  n_bands <- as.integer(hdr[["bands"]])
  spec <- .envi_read_spec(hdr[["data type"]])
  byte_order <- suppressWarnings(as.integer(hdr[["byte order"]] %||% -1L))
  platform_order <- if (identical(.Platform$endian, "big")) 1L else 0L
  endian <- if (byte_order < 0L || byte_order == platform_order)
    .Platform$endian else "swap"
  header_offset <- suppressWarnings(as.numeric(hdr[["header offset"]] %||% 0))
  connection_input <- inherits(file, "connection")
  connection <- if (connection_input) file else base::file(file, "rb")
  close_connection <- !connection_input
  if (connection_input && !isOpen(connection)) {
    open(connection, "rb")
    close_connection <- TRUE
  }
  if (close_connection) on.exit(close(connection), add = TRUE)
  if (header_offset > 0) {
    skipped <- readBin(connection, what = "raw", n = header_offset)
    if (length(skipped) != header_offset)
      stop("ENVI binary ended inside the declared header offset", call. = FALSE)
  }

  sigma <- policy$sigma
  halo <- if (is.null(sigma)) 0L else {
    size <- ceiling(6 * sigma[[1L]])
    size <- if (size %% 2L == 1L) size else size + 1L
    as.integer((size - 1L) / 2L)
  }
  block_rows <- max(1L, as.integer(block_rows))
  buffer <- array(numeric(), dim = c(0L, nx, n_bands))
  buffer_start <- 1L
  last_read <- 0L
  value_chunks <- list()
  snr <- rep(NA_real_, nx * ny)
  mapping <- integer(nx * ny)
  retained <- 0L

  append_rows <- function(existing, rows) {
    if (!dim(existing)[1L]) return(rows)
    out <- array(NA_real_, dim = c(dim(existing)[1L] + dim(rows)[1L],
                                   nx, n_bands))
    out[seq_len(dim(existing)[1L]), , ] <- existing
    out[dim(existing)[1L] + seq_len(dim(rows)[1L]), , ] <- rows
    out
  }
  read_rows <- function(count) {
    values <- .read_envi_block(
      connection, count * nx * n_bands, spec, endian
    )
    aperm(array(values, dim = c(n_bands, nx, count)), c(3, 2, 1))
  }

  for (core_start in seq.int(1L, ny, by = block_rows)) {
    core_end <- min(ny, core_start + block_rows - 1L)
    required_start <- max(1L, core_start - halo)
    required_end <- min(ny, core_end + halo)
    if (required_start > buffer_start) {
      first_keep <- required_start - buffer_start + 1L
      if (first_keep > dim(buffer)[1L]) {
        buffer <- array(numeric(), dim = c(0L, nx, n_bands))
      } else {
        buffer <- buffer[first_keep:dim(buffer)[1L], , , drop = FALSE]
      }
      buffer_start <- required_start
    }
    if (last_read < required_end) {
      fresh <- read_rows(required_end - last_read)
      buffer <- append_rows(buffer, fresh)
      last_read <- required_end
    }
    basis <- if (is.null(sigma)) buffer else
      mmand::gaussianSmooth(buffer, sigma = sigma)
    local <- (core_start:core_end) - buffer_start + 1L
    raw_core <- buffer[local, , , drop = FALSE]
    basis_core <- basis[local, , , drop = FALSE]
    classify <- matrix(aperm(basis_core, c(3, 2, 1)), nrow = n_bands)
    metric_object <- as_OpenSpecy(
      wavenumbers, spectra = classify,
      metadata = data.frame(x = rep(seq_len(nx) - 1L,
                                    times = length(local)),
                            y = rep(core_start:core_end - 1L, each = nx))
    )
    block_snr <- sig_noise(
      metric_object, metric = policy$metric, step = policy$step,
      spatial_smooth = FALSE, abs = FALSE
    )
    global <- ((core_start - 1L) * nx + 1L):(core_end * nx)
    snr[global] <- block_snr
    keep <- is.finite(block_snr) & block_snr > policy$minimum &
      block_snr < policy$maximum
    if (any(keep)) {
      selected <- if (isTRUE(return_smoothed)) classify else
        matrix(aperm(raw_core, c(3, 2, 1)), nrow = n_bands)
      value_chunks[[length(value_chunks) + 1L]] <- selected[, keep, drop = FALSE]
      mapping[global[keep]] <- retained + seq_len(sum(keep))
      retained <- retained + sum(keep)
    }
  }
  values <- if (length(value_chunks)) do.call(cbind, value_chunks) else
    matrix(numeric(), nrow = n_bands, ncol = 0L)
  .envi_background_result_from_mapping(values, mapping, snr, policy)
}

.envi_background_result_from_mapping <- function(values, mapping, snr, policy) {
  reason <- integer(length(snr))
  reason[!is.finite(snr)] <- 3L
  reason[is.finite(snr) & snr <= policy$minimum] <- 1L
  reason[is.finite(snr) & snr >= policy$maximum] <- 2L
  list(
    values = values, mapping = mapping,
    background = list(
      mask = mapping == 0L, signal_to_noise = snr, reason = reason,
      reason_levels = c("foreground", "below_minimum", "above_maximum",
                        "nonfinite"), policy = policy
    )
  )
}

.envi_read_spec <- function(data_type) {
  switch(
    as.character(data_type),
    "1" = list(what = integer(), size = 1L, signed = FALSE,
               storage = "integer"),
    "2" = list(what = integer(), size = 2L, signed = TRUE,
               storage = "integer"),
    "3" = list(what = integer(), size = 4L, signed = TRUE,
               storage = "integer"),
    "4" = list(what = double(), size = 4L, signed = TRUE,
               storage = "double"),
    "5" = list(what = double(), size = 8L, signed = TRUE,
               storage = "double"),
    "9" = list(what = complex(), size = NA_integer_, signed = TRUE,
               storage = "complex"),
    "12" = list(what = integer(), size = 2L, signed = FALSE,
                storage = "integer"),
    stop("read.ENVI: Error in input header file data type is missing, ",
         "incorrect or unsupported", call. = FALSE)
  )
}

.read_envi_block <- function(connection, count, spec, endian) {
  args <- list(
    con = connection,
    what = spec$what,
    n = as.integer(count),
    endian = endian
  )
  if (!is.na(spec$size)) args$size <- spec$size
  if (identical(spec$what, integer())) args$signed <- spec$signed
  values <- do.call(readBin, args)
  if (length(values) != count) {
    stop("ENVI binary ended before all declared values were read", call. = FALSE)
  }
  values
}

.read_envi_spectra <- function(file, hdr, block_pixels = 8192L) {
  nx <- as.integer(hdr[["samples"]])
  ny <- as.integer(hdr[["lines"]])
  n_bands <- as.integer(hdr[["bands"]])
  if (anyNA(c(nx, ny, n_bands)) || any(c(nx, ny, n_bands) <= 0L)) {
    stop("read.ENVI: data sizes missing or incorrect", call. = FALSE)
  }
  connection_input <- inherits(file, "connection")
  if (!connection_input && !file.exists(file)) {
    stop("read.ENVI: Could not open input file: ", file, call. = FALSE)
  }

  spec <- .envi_read_spec(hdr[["data type"]])
  interleave <- tolower(gsub("[[:space:]]+", "", hdr[["interleave"]] %||% "bsq"))
  if (!interleave %in% c("bip", "bil", "bsq")) {
    stop("read.ENVI: incorrect interleave type", call. = FALSE)
  }
  byte_order <- suppressWarnings(as.integer(hdr[["byte order"]] %||% -1L))
  platform_order <- if (identical(.Platform$endian, "big")) 1L else 0L
  endian <- if (byte_order < 0L || byte_order == platform_order) {
    .Platform$endian
  } else {
    "swap"
  }
  header_offset <- suppressWarnings(as.numeric(hdr[["header offset"]] %||% 0))
  if (!is.finite(header_offset) || header_offset < 0) {
    stop("read.ENVI: header offset is missing or incorrect", call. = FALSE)
  }

  n_pixels <- nx * ny
  connection <- if (connection_input) file else file(file, "rb")
  close_connection <- !connection_input
  if (connection_input && !isOpen(connection)) {
    open(connection, "rb")
    close_connection <- TRUE
  }
  if (close_connection) on.exit(close(connection), add = TRUE)
  if (header_offset > 0) {
    if (connection_input) {
      skipped <- readBin(connection, what = "raw", n = header_offset)
      if (length(skipped) != header_offset) {
        stop("ENVI binary ended inside the declared header offset",
             call. = FALSE)
      }
    } else {
      seek(connection, where = header_offset, origin = "start")
    }
  }

  if (identical(interleave, "bip") && n_pixels <= block_pixels) {
    return(matrix(
      .read_envi_block(connection, n_bands * n_pixels, spec, endian),
      nrow = n_bands, ncol = n_pixels
    ))
  }

  empty <- switch(
    spec$storage,
    integer = NA_integer_,
    double = NA_real_,
    complex = NA_complex_
  )
  spectra <- matrix(empty, nrow = n_bands, ncol = n_pixels)

  if (identical(interleave, "bip")) {
    starts <- seq.int(1L, n_pixels, by = block_pixels)
    for (start in starts) {
      finish <- min(n_pixels, start + block_pixels - 1L)
      count <- finish - start + 1L
      values <- .read_envi_block(
        connection, n_bands * count, spec, endian
      )
      spectra[, start:finish] <- matrix(values, nrow = n_bands)
    }
  } else if (identical(interleave, "bil")) {
    for (row in seq_len(ny)) {
      columns <- ((row - 1L) * nx + 1L):(row * nx)
      values <- .read_envi_block(connection, nx * n_bands, spec, endian)
      spectra[, columns] <- t(matrix(values, nrow = nx, ncol = n_bands))
    }
  } else {
    starts <- seq.int(1L, n_pixels, by = block_pixels)
    for (band in seq_len(n_bands)) {
      for (start in starts) {
        finish <- min(n_pixels, start + block_pixels - 1L)
        spectra[band, start:finish] <- .read_envi_block(
          connection, finish - start + 1L, spec, endian
        )
      }
    }
  }

  spectra
}

.read_envi_header <- function(headerfile, ...) {
  tr <- file.path(headerfile) |> file(...)
  hdr <- tr |> readLines()
  close(tr)

  if(!grepl("ENVI", hdr[1])) {
    stop("envi header not found", call. = F)
  } else {
    hdr <- hdr[-1]
  }

  hdr <- gsub("\\{([^}]*)\\}", "\\1", hdr)

  l <- grep("\\{", hdr)
  r <- grep("\\}", hdr)

  if (length(l) != length(r) || any(r <= l))
    stop("header data does not match", call. = F)

  hdr[l] <- sub("\\{", "", hdr[l])
  hdr[r] <- sub("\\}", "", hdr[r])

  for (i in rev(seq_along(l))) {
    hdr <- c(
      hdr[seq_len(l[i] - 1)],
      paste(hdr[l[i]:r[i]], collapse = " "),
      hdr[-seq_len(r[i])]
    )
  }

  hdr <- sapply(hdr, .split_line, "=", USE.NAMES = FALSE)
  names(hdr) <- tolower(names(hdr))

  tmp <- names(hdr) %in% c("samples", "lines", "bands", "data type",
                           "header offset")
  hdr[tmp] <- lapply(hdr[tmp], as.numeric)

  if("wavelength" %in% names(hdr)){
      hdr$wavelength <- strsplit(hdr$wavelength, "[,;[:blank:]]+") |> unlist() |>
          as.numeric()        
  }

  return(hdr)
}

.split_line <- function(x, sep, trim.blank = TRUE) {
  tmp <- regexpr(sep, x)

  key <- substr(x, 1, tmp - 1)
  val <- substr(x, tmp + 1, nchar(x))

  if (trim.blank) {
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub(blank.pattern, "\\1", key)
    val <- sub(blank.pattern, "\\1", val)
  }

  val <- as.list(val)
  names(val) <- key

  return(val)
}
