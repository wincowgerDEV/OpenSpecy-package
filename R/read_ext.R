#' @rdname read_ext
#'
#' @title Read spectral data
#'
#' @description
#' Functions for reading spectral data from external file types.
#' Currently supported reading formats are .csv and other text files, .asp,
#' .spa, .spc, and .jdx.
#' Additionally, .0 (OPUS) and .dat (ENVI) files are supported via
#' \code{\link{read_opus}()} and \code{\link{read_envi}()}, respectively.
#' \code{\link{read_zip}()} takes any of the files listed above.
#' Note that proprietary file formats like .0, .asp, and .spa are poorly
#' supported but will likely still work in most cases.
#'
#' @param file file to be read from or written to.
#' @param colnames character vector of \code{length = 2} indicating the column
#' names for the wavenumber and intensity; if \code{NULL} columns are guessed.
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
  if(sum(grepl("^[0-9]{1,}$",colnames(dt))) > 4) {
    wavenumbers <- colnames(dt)[grepl("^[0-9]{1,}$",colnames(dt))]
    spectra <- transpose(dt[,wavenumbers, with = FALSE])
    metadata_names <- colnames(dt)[!grepl("^[0-9]{1,}$",colnames(dt))]

    metadata <- dt[, metadata_names, with = FALSE]

    os <- as_OpenSpecy(x = as.numeric(wavenumbers), spectra = spectra,
                       metadata = metadata)
  } else {
    os <- as_OpenSpecy(dt, colnames = colnames, metadata = metadata,
                       session_id = T)
  }

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

  trb <- file.path(file) |> file(open = "rb", ...)

  seek(trb, 576, origin = "start")
  spr <- readBin(trb, "numeric", n = 2, size = 4)

  if (!all(spr >= 0 & spr <= 15000 & spr[1] > spr[2]))
    stop("unknown spectral range", call. = F)

  # Read the start offset
  seek(trb, 386, origin = "start")
  startOffset <- readBin(trb, "int", n = 1, size = 2)
  # Read the length
  seek(trb, 390, origin = "start")
  readLength <- readBin(trb, "int", n = 1, size = 2)

  # seek to the start
  seek(trb, startOffset, origin = "start")

  # we'll read four byte chunks
  floatCount <- readLength / 4

  # read all our floats
  floatData <- c(readBin(trb, "double", floatCount, size = 4))

  close(trb)

  x <- seq(spr[1], spr[2], length = length(floatData))
  y <- floatData

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata,
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
  spc <- read.spc(file)

  x <- spc@wavelength
  y <- as.numeric(unname(spc@data$spc[1,]))

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata,
                     session_id = T)

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
