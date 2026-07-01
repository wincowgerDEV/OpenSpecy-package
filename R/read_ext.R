#' @rdname read_ext
#'
#' @title Read spectral data
#'
#' @description
#' Functions for reading spectral data from external file types.
#' Currently supported reading formats are .csv and other text files, .asp,
#' .spa, .spc, .xyz, and .jdx.
#' Additionally, .0 (OPUS) and .dat (ENVI) files are supported via
#' \code{\link{read_opus}()} and \code{\link{read_envi}()}, respectively.
#' \code{\link{read_zip}()} takes any of the files listed above.
#' Note that proprietary file formats like .0, .asp, and .spa are poorly
#' supported but will likely still work in most cases.
#'
#' @param file file to be read from or written to.
#' @param colnames character vector of \code{length = 2} indicating the column
#' names for the wavenumber and intensity; if \code{NULL} columns are guessed.
#' @param comma_decimal logical(1) whether commas may represent decimals. 
#' @param method submethod to be used for reading text files; defaults to
#' \code{\link[data.table]{fread}()} but \code{\link[utils]{read.csv}()} works
#' as well.
#' @param metadata a named list of the metadata; see
#' @param collapse whether or not to use \code{\link{collapse_spec}()} by
#' particle_id. For \code{read_h5()}, the default is \code{FALSE} so region
#' pixels are returned in raw form.
#' @param spectral_smooth logical; whether H5 cubes should be smoothed before
#' matrix conversion.
#' @param sigma numeric vector passed to \code{\link[mmand]{gaussianSmooth}()}.
#' @param read_visual logical; whether H5 mosaic images should be attached as
#' visual-image attributes when present.
#' \code{\link{as_OpenSpecy}()} for details.
#' @param \ldots further arguments passed to the submethods.
#'
#' @details
#' \code{read_spc()} and \code{read_jdx()} are wrappers around the
#' functions provided by the \link[hyperSpec:hyperSpec-package]{hyperSpec}.
#' Other functions have been adapted various online sources.
#' Metadata is harvested if possible.
#' There are many unique iterations of spectral file formats so there may be
#' bugs in the file conversion. Please contact us if you identify any.
#'
#' @return
#' All \code{read_*()} functions return data frames containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' read_extdata("raman_hdpe.csv") |> read_text()
#' read_extdata("raman_atacamit.spc") |> read_spc()
#' read_extdata("ftir_ldpe_soil.asp") |> read_asp()
#' read_extdata("testdata_zipped.zip") |> read_zip()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_spec}()} for reading .json, .rds, or .csv (OpenSpecy)
#' files;
#' \code{\link{read_opus}()} for reading .0 (OPUS) files;
#' \code{\link{read_envi}()} for reading .dat (ENVI) files;
#' \code{\link{read_zip}()} and \code{\link{read_any}()} for wrapper functions;
#' \code{\link[hyperSpec]{read.jdx}()}; \code{\link[hyperSpec]{read.spc}()}
#'
#' @importFrom data.table data.table as.data.table fread transpose
#' @export
read_text <- function(file, colnames = NULL, method = "fread",
                      comma_decimal = TRUE,
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
  dt <- do.call(method, list(file, ...)) |> as.data.table()

  if (all(grepl("^X[0-9]*", names(dt))))
    stop("missing header: use 'header = FALSE' or an alternative read method",
         call. = F)
  #This is for the siMPle csv format. 
  if(sum(c("WaveNumber", "Raw spectrum", "1st derivative", "2nd derivative") %in% names(dt)) > 2){
      dt <- as.data.table(lapply(dt, as.numeric))
      wavenumbers <- as.numeric(dt[["WaveNumber"]])
      spectra <- dt[,-"WaveNumber"]
  }
  else if(grepl("\\.xyz$", basename(file), ignore.case = TRUE)){
      wavenumbers <- as.numeric(unlist(dt[1, -c(1:2), with = FALSE],
                                       use.names = FALSE))
      metadata <- as.data.table(dt[-1, 1:2, with = FALSE])
      data.table::setnames(metadata, c("x", "y"))
      metadata$file_name <- basename(file)
      spectra <- transpose(dt[-1, -c(1:2), with = FALSE])
      spectra <- as.data.table(lapply(spectra, as.numeric))
      colnames(spectra) <- paste(metadata$x, metadata$y, sep = "_")
  }
  else if(sum(grepl("^[0-9]{1,}$",colnames(dt))) > 4) {
    wavenumbers <- colnames(dt)[grepl("^[0-9]{1,}$",colnames(dt))]
    spectra <- transpose(dt[,wavenumbers, with = FALSE])
    wavenumbers <- as.numeric(wavenumbers)
    metadata_names <- colnames(dt)[!grepl("^[0-9]{1,}$",colnames(dt))]

    metadata <- dt[, metadata_names, with = FALSE]
  } 
  else {
    os <- as_OpenSpecy(dt, colnames = colnames, metadata = metadata, comma_decimal = comma_decimal,
                       session_id = T)
    return(os)
  }
  
  os <- as_OpenSpecy(x = wavenumbers, spectra = spectra,
                     metadata = metadata, comma_decimal = comma_decimal)

  return(os)
}

#' @rdname read_ext
#'
#' @export
read_asp <- function(file,
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
  if (!grepl("\\.asp$", ignore.case = T, file))
    stop("file type should be 'asp'", call. = F)

  tr <- file.path(file) |> file(...)
  lns <- tr |> readLines() |> as.numeric()
  close(tr)

  y <- lns[-c(1:6)]
  x <- seq(lns[2], lns[3], length.out = lns[1])

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata,
                     session_id = T)

  return(os)
}

#' @rdname read_ext
#'
#' @importFrom utils read.table
#' @export
read_spa <- function(file, 
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
  if (!grepl("\\.spa$", ignore.case = T, file))
    stop("file type should be 'spa'", call. = F)

    # Read a *.spa file
    # Returns:
    #   A list containing Spectra, Wavelengths (nm), and Titles
    # Converted to r from https://github.com/lerkoah/spa-on-python/blob/master/LoadSpectrum.py
    con <- file(file, "rb", ...) # Open the file in binary mode
    on.exit(close(con)) # Ensure the file is closed
    
    seek(con, where = 564, rw = "r")
    Spectrum_Pts <- readBin(con, integer(), size = 4, n = 1, endian = "little")
    
    seek(con, where = 30, rw = "r")
    SpectraTitlesRaw <- readBin(con, raw(), n = 255)
    SpectraTitles <- rawToChar(SpectraTitlesRaw[SpectraTitlesRaw != as.raw(0)])
    
    seek(con, where = 576, rw = "r")
    Max_Wavenum <- readBin(con, numeric(), size = 4, n = 1, endian = "little")
    Min_Wavenum <- readBin(con, numeric(), size = 4, n = 1, endian = "little")
    
    # Generate wavenumbers
    Wavenumbers <- rev(seq(Min_Wavenum, Max_Wavenum, length.out = Spectrum_Pts))
    
    seek(con, where = 288, rw = "r")
    
    Flag <- 0
    while (Flag != 3) {
        Flag <- readBin(con, integer(), size = 2, n = 1, endian = "little")
    }
    
    DataPosition <- readBin(con, integer(), size = 2, n = 1, endian = "little")
    seek(con, where = DataPosition, rw = "r")
    
    Spectra <- readBin(con, numeric(), size = 4, n = Spectrum_Pts, endian = "little")
    
    # Return the results
    metadata$title <- SpectraTitles
    
    os <- as_OpenSpecy(Wavenumbers, data.table(intensity = Spectra), metadata,
                     session_id = T)

  return(os)
}


#' @rdname read_ext
#'
#' @importFrom hyperSpec read.spc
#' @export
read_spc <- function(file,
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
    
    con <- file(file, "rb")
    on.exit(close(con))
    raw_data <- readBin(con, what = "raw", n = 544)
    fexp = readBin(raw_data[4], "integer", 1, 1, signed = TRUE)
    
    if(fexp == -128){
        fnpts = readBin(raw_data[5:8], "integer", 1, 4)
        ffirst = readBin(raw_data[9:16], "double", 1, 8)
        flast = readBin(raw_data[17:24], "double", 1, 8)

        os <- as_OpenSpecy(seq(from = ffirst, to = flast, length.out = fnpts), 
                           data.table(intensity = readBin(con, what = "numeric", n = fnpts, size = 4, endian = "little")),
                           metadata = metadata,
                           session_id = T) 
    }
    else{
        spc <- read.spc(file)
        
        x <- spc@wavelength
        y <- as.numeric(unname(spc@data$spc[1,]))
        
        os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata,
                           session_id = T)      
    }
    
  

  return(os)
}

#' @rdname read_ext
#'
#' @importFrom hyperSpec read.jdx
#' @export
read_jdx <- function(file, 
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
  jdx <- read.jdx(file, encoding = 'latin1')

  x <- jdx@wavelength
  y <- as.numeric(unname(jdx@data$spc[1,]))

  lns <- readLines(file)
  test <- lns[grepl("##", lns)|grepl("[:alpha:]", lns)]
  vals <- ifelse(grepl("##",test), gsub("##.{1,}=", "", test),
                   gsub(".{1,}:", "", test))
  names <- ifelse(grepl("##",test), gsub("##", "", gsub("=.{1,}", "", test)),
                  gsub(":.{1,}", "", test))
  df_metadata <- as.data.table(t(vals))
  colnames(df_metadata) <- names

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = df_metadata,
                     session_id = T)


  return(os)
}

#' @rdname read_ext
#'
#' @export
read_extdata <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "OpenSpecy"))
  }
  else {
    system.file("extdata", file, package = "OpenSpecy", mustWork = TRUE)
  }
}

#' @rdname read_ext
#' @import hdf5r 
#' @export
read_h5 <- function(file, collapse = FALSE, spectral_smooth = FALSE,
                    sigma = c(1, 1, 1), read_visual = TRUE, ...) {

    h5 <- H5File$new(file, mode = "r")
    on.exit({
        try(h5$close_all(), silent = TRUE)
    }, add = TRUE)

    if (!h5$exists("/Regions")) {
        message("X No /Regions group found: ", file)
        return(invisible(NULL))
    }

    regions <- names(h5[["/Regions"]])
    if (length(regions) == 0) {
        message("X No Region_* groups found under /Regions: ", file)
        return(invisible(NULL))
    }

    file_meta <- .read_h5_file_metadata(h5)
    first_dims <- .h5_dataset_dims(h5[[paste0("/Regions/", regions[1],
                                             "/Dataset")]])
    if (length(first_dims) != 3L)
        stop("Expected 3D dataset. Got dims: ",
             paste(first_dims, collapse = "x"), call. = FALSE)
    spec_dim <- .h5_spectral_dim(first_dims, file_meta)
    n_wavenumber <- first_dims[spec_dim]
    wavenumbers <- .h5_wavenumbers(file_meta, n_wavenumber)

    mats <- vector("list", length(regions))
    metas <- vector("list", length(regions))
    ids_all <- vector("list", length(regions))

    for (i in seq_along(regions)) {
        reg <- regions[i]
        dset <- h5[[paste0("/Regions/", reg, "/Dataset")]]
        dims <- .h5_dataset_dims(dset)
        if (length(dims) != 3L)
            stop("Expected 3D dataset. Got dims: ",
                 paste(dims, collapse = "x"), call. = FALSE)
        spec_dim <- .h5_spectral_dim(dims, file_meta)
        cube <- dset$read()
        cube <- aperm(cube, c(spec_dim, setdiff(seq_along(dims), spec_dim)))
        if (isTRUE(spectral_smooth))
            cube <- mmand::gaussianSmooth(cube, sigma = sigma)

        n <- dim(cube)[1L]
        ny <- dim(cube)[2L]
        nx <- dim(cube)[3L]
        if (n != length(wavenumbers))
            wavenumbers <- .h5_wavenumbers(file_meta, n)

        mat <- matrix(cube, nrow = n, ncol = ny * nx)
        id_digits <- gsub("\\D", "", reg)
        particle_lab <- if (nzchar(id_digits)) paste0("Region", id_digits) else reg
        grid <- expand.grid(row = seq_len(ny), col = seq_len(nx))
        ids <- paste0(particle_lab, "_r", grid$row, "c", grid$col)
        colnames(mat) <- ids

        mats[[i]] <- mat
        ids_all[[i]] <- ids
        metas[[i]] <- data.table(
            id = ids,
            file_name = basename(file),
            region = reg,
            particle_id = particle_lab,
            subpixel = seq_len(nrow(grid)),
            row = grid$row,
            col = grid$col,
            x = grid$col - 1L,
            y = grid$row - 1L
        )
        metas[[i]] <- cbind(metas[[i]], .h5_region_metadata(h5, reg))
    }

    spectra <- do.call(cbind, mats)
    metadata <- data.table::rbindlist(metas, fill = TRUE)
    if (length(file_meta)) {
        scalar_meta <- as.data.table(file_meta)
        scalar_meta <- scalar_meta[rep(1L, nrow(metadata))]
        metadata <- cbind(metadata, scalar_meta)
    }
    coords <- metadata[, c("x", "y"), with = FALSE]
    metadata_no_coords <- metadata[, setdiff(names(metadata), c("x", "y")),
                                   with = FALSE]
    os <- as_OpenSpecy(wavenumbers, spectra = spectra,
                       metadata = metadata_no_coords, coords = coords,
                       session_id = TRUE)

    if (isTRUE(read_visual)) {
        vi <- .read_h5_visual_image(h5)
        if (!is.null(vi$image)) {
            os <- add_visual_image(
                os, vi$image, source = vi$source,
                diagnostics = vi$diagnostics
            )
        }
    }

    if (isTRUE(collapse)) {
        warning("read_h5(collapse = TRUE) is retained for compatibility; ",
                "the default is raw-region output. Prefer explicit ",
                "collapse_spec() after reading.", call. = FALSE)
        os <- collapse_spec(os, fun = mean, column = "particle_id", ...)
    }
    os
}

.h5_dataset_dims <- function(dset) {
    dims <- tryCatch(dset$dims, error = function(e) NULL)
    if (is.null(dims)) dims <- dim(dset$read())
    as.integer(dims)
}

.read_h5_file_metadata <- function(h5) {
    out <- list()
    if (h5$exists("/FileInfo")) {
        fi <- h5[["/FileInfo"]]
        attrs <- tryCatch(fi$attr_names, error = function(e) character())
        for (a in attrs) {
            val <- tryCatch(fi$attr_open(a)$read(), error = function(e) NULL)
            if (!is.null(val) && length(val) == 1L) out[[a]] <- val
        }
    }
    if (h5$exists("/FileInfo/MetaData")) {
        xml <- .h5_metadata_text(h5[["/FileInfo/MetaData"]]$read())
        out <- utils::modifyList(out, .h5_xml_scalar_vars(xml))
    }
    out
}

.h5_metadata_text <- function(x) {
    raw <- as.raw(x)
    raw <- raw[raw != as.raw(0)]
    rawToChar(raw, multiple = FALSE)
}

.h5_xml_scalar_vars <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(list())
    pattern <- "<VAR[^>]*NAME=\"([^\"]+)\"[^>]*>([^<]*)</VAR>"
    hits <- regmatches(txt, gregexpr(pattern, txt, perl = TRUE))[[1]]
    if (!length(hits)) return(list())
    names <- sub(pattern, "\\1", hits, perl = TRUE)
    values <- sub(pattern, "\\2", hits, perl = TRUE)
    keep <- nzchar(names) & nchar(values) <= 120L
    names <- make.unique(names[keep])
    values <- values[keep]
    values <- lapply(values, .h5_scalar_value)
    names(values) <- names
    values
}

.h5_scalar_value <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (length(num) == 1L && !is.na(num)) return(num)
    x
}

.h5_meta_first <- function(meta, keys) {
    for (key in keys) {
        if (!is.null(meta[[key]])) return(meta[[key]])
    }
    NULL
}

.h5_spectral_dim <- function(dims, file_meta) {
    n <- .h5_meta_first(file_meta, c("SpectrumPoints", "m_SpectrumPoints",
                                    "m_NumberOfPoints", "NPT"))
    if (!is.null(n)) {
        idx <- which(dims == as.integer(n))[1L]
        if (!is.na(idx)) return(idx)
    }
    which.max(dims)
}

.h5_wavenumbers <- function(file_meta, n) {
    start <- .h5_meta_first(file_meta, c("SpectralRangeStart",
                                        "m_StartFrequency", "m_StartFreq",
                                        "m_StartFrequency.1"))
    end <- .h5_meta_first(file_meta, c("SpectralRangeEnd",
                                      "m_EndFrequency", "m_EndFreq",
                                      "m_EndFrequency.1"))
    if (!is.null(start) && !is.null(end) &&
        is.finite(as.numeric(start)) && is.finite(as.numeric(end))) {
        return(seq(as.numeric(start), as.numeric(end), length.out = n))
    }
    warning("H5 spectral range not found, using index values instead",
            call. = FALSE)
    seq_len(n)
}

.h5_region_metadata <- function(h5, reg) {
    path <- paste0("/Regions/", reg, "/-StagePosXYZ")
    if (!h5$exists(path)) return(data.table())
    vals <- tryCatch(as.numeric(h5[[path]]$read()), error = function(e) NULL)
    if (is.null(vals)) return(data.table())
    out <- as.data.table(as.list(vals))
    names(out) <- paste0("stage_pos_", seq_along(vals))
    out
}

.read_h5_visual_image <- function(h5) {
    if (!h5$exists("/Mosaic")) return(list(image = NULL))
    mosaic <- h5[["/Mosaic"]]
    images <- names(mosaic)[grepl("^Image", names(mosaic))]
    if (!length(images)) return(list(image = NULL))
    raw_img <- tryCatch(mosaic[[images[1L]]]$read(), error = function(e) NULL)
    img <- tryCatch(.read_visual_bmp_bytes(raw_img), error = function(e) NULL)
    centers <- if (h5$exists("/Mosaic/Centers")) {
        tryCatch(as.data.table(h5[["/Mosaic/Centers"]]$read()),
                 error = function(e) NULL)
    } else {
        NULL
    }
    list(
        image = img,
        source = paste0("/Mosaic/", images[1L]),
        diagnostics = list(images = images, centers = centers)
    )
}
