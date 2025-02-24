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
#' \code{\link{read_spec}()} for reading .y(a)ml, .json, or .rds (OpenSpecy)
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
  else if(grepl("\\.xyz$", basename(file), ignore.case = T)){
      wavenumbers <- as.numeric(dt[1, ])[-c(1:2)]
      # Remove the first row
      metadata <- dt[-1,1:2]
      colnames(xy) <- c("x", "y")
      spectra <- transpose(dt[-1, -c(1:2)])
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
