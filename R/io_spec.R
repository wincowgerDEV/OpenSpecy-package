#' @rdname io_spec
#'
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading spectral data types including .asp, .jdx,
#' .spc, .spa, .0, and .csv.
#'
#' @details
#' \code{read_spc()} and \code{read_jdx()} are just a wrapper around the
#' functions provided by the \link[hyperSpec:hyperSpec-package]{hyperSpec}
#' package.
#' Other functions have been adapted various online sources.
#' All functions convert datasets to a 2 column table with one column labeled
#' "wavenumber" and the other "intensity". There are many unique iterations of
#' spectral file formats so there may be bugs in the file conversion.
#' Please contact us if you identify any.

#' The \code{metadata} argument may contain a named list with the following
#' details (\code{*} = minimum recommended):
#'
#' \tabular{ll}{
#' \code{id*}: \tab A unique user and/or session ID; defaults to
#' \code{paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")}\cr
#' \code{file_name*}: \tab The file name, defaults to
#' \code{\link[base]{basename}()} if not specified\cr
#' \code{user_name*}: \tab User name, e.g. "Win Cowger"\cr
#' \code{contact_info}: \tab Contact information, e.g. "1-513-673-8956,
#' wincowger@@gmail.com"\cr
#' \code{organization}: \tab Affiliation, e.g. "University of California,
#' Riverside"\cr
#' \code{citation}: \tab Data citation, e.g. "Primpke, S., Wirth, M., Lorenz,
#' C., & Gerdts, G. (2018). Reference database design for the automated analysis
#' of microplastic samples based on Fourier transform infrared (FTIR)
#' spectroscopy. \emph{Analytical and Bioanalytical Chemistry}.
#' \doi{10.1007/s00216-018-1156-x}"\cr
#' \code{spectrum_type*}: \tab Raman or FTIR\cr
#' \code{spectrum_identity*}: \tab Material/polymer analyzed, e.g.
#' "Polystyrene"\cr
#' \code{material_form}: \tab Form of the material analyzed, e.g. textile fiber,
#' rubber band, sphere, granule \cr
#' \code{material_phase}: \tab Phase of the material analyzed (liquid, gas,
#' solid) \cr
#' \code{material_producer}: \tab Producer of the material analyzed,
#' e.g. Dow \cr
#' \code{material_purity}: \tab Purity of the material analyzed, e.g. 99.98\%
#' \cr
#' \code{material_quality}: \tab Quality of the material analyzed, e.g.
#' consumer product, manufacturer material, analytical standard,
#' environmental sample \cr
#' \code{material_color}: \tab Color of the material analyzed,
#' e.g. blue, #0000ff, (0, 0, 255) \cr
#' \code{material_other}: \tab Other material description, e.g. 5 µm diameter
#' fibers, 1 mm spherical particles \cr
#' \code{cas_number}: \tab CAS number, e.g. 9003-53-6 \cr
#' \code{instrument_used}: \tab Instrument used, e.g. Horiba LabRam \cr
#' \code{instrument_accessories}: \tab Instrument accessories, e.g.
#' Focal Plane Array, CCD\cr
#' \code{instrument_mode}: \tab Instrument modes/settings, e.g.
#' transmission, reflectance \cr
#' \code{spectral_resolution}: \tab Spectral resolution, e.g. 4/cm \cr
#' \code{laser_light_used}: \tab Wavelength of the laser/light used, e.g.
#' 785 nm \cr
#' \code{number_of_accumulations}: \tab Number of accumulations, e.g 5 \cr
#' \code{total_acquisition_time_s}: \tab Total acquisition time (s), e.g. 10 s
#' \cr
#' \code{data_processing_procedure}: \tab Data processing procedure,
#' e.g. spikefilter, baseline correction, none \cr
#' \code{level_of_confidence_in_identification}: \tab Level of confidence in
#' identification, e.g. 99\% \cr
#' \code{other_info}: \tab Other information \cr
#' \code{license}: \tab The license of the shared spectrum; defaults to
#' \code{"CC BY-NC"} (see
#' \url{https://creativecommons.org/licenses/by-nc/4.0/} for details). Any other
#' creative commons license is allowed, for example, CC0 or CC BY \cr
#' }
#'
#' @param object a list object of class \code{OpenSpecy}.
#' @param file file to be read from.
#' @param encoding file encoding; defaults to \code{"UTF-8"}.
#' @param colnames character vector of \code{length = 2} indicating the colum
#' names for the wavenumber and intensity; if \code{NULL} columns are guessed.
#' @param method submethod to be used for reading text files; defaults to
#' \link[utils]{read.csv} but \link[data.table]{fread} works as well.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param metadata a named list of the metadata; see details below.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return data frames containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' read_text(read_extdata("raman_hdpe.csv"))
#' read_asp(read_extdata("ftir_ldpe_soil.asp"))
#' read_0(read_extdata("ftir_ps.0"))
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[hyperSpec]{read.jdx}()}; \code{\link[hyperSpec]{read.spc}()};
#' \code{\link[hexView]{readRaw}()}; \code{\link{share_spec}()}
#'
#' @importFrom magrittr %>%
#' @importFrom data.table data.table as.data.table fread
#' @export
write_OpenSpecy <- function(object, ...) {
  UseMethod("write_OpenSpecy")
}

#' @rdname io_spec
#'
#' @export
write_OpenSpecy.default <- function(object, ...) {
  stop("object needs to be of class 'OpenSpecy'")
}

#' @rdname io_spec
#'
#' @importFrom yaml write_yaml
#' @export
write_OpenSpecy.OpenSpecy <- function(object, file = ".", encoding = "UTF-8",
                                      ...) {
  write_yaml(object, file = file, fileEncoding = encoding)
}

#' @rdname io_spec
#'
#' @importFrom yaml read_yaml
#' @export
read_OpenSpecy <- function(file = ".", encoding = "UTF-8", share = NULL,
                           metadata = NULL, ...) {
  yml <- read_yaml(file = file, fileEncoding = encoding)

  os <- as_OpenSpecy(yml$wavenumber,
                     spectra = as.data.table(yml$spectra),
                     coords = as.data.table(yml$coords),
                     metadata = metadata, ...)

  # TODO: update sharing
  # if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @export
read_text <- function(file = ".", colnames = NULL, method = "fread",
                      share = NULL,
                      metadata = list(
                        id = paste(digest(Sys.info()),
                                   digest(sessionInfo()),
                                   sep = "/"),
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
  dt <- do.call(method, list(file, ...)) %>% as.data.table()

  if (all(grepl("^X[0-9]*", names(dt)))) stop("missing header: ",
                                              "use 'header = FALSE' or an ",
                                              "alternative read method")

  os <- as_OpenSpecy(dt, colnames = colnames, metadata = metadata)

  # TODO: update sharing
  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @export
read_asp <- function(file = ".", share = NULL,
                     metadata = list(
                       id = paste(digest(Sys.info()),
                                  digest(sessionInfo()),
                                  sep = "/"),
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
    stop("file type should be 'asp'")

  tr <- file.path(file) %>% file(...)
  lns <- tr %>% readLines() %>% as.numeric()
  close(tr)

  y <- lns[-c(1:6)]
  x <- seq(lns[2], lns[3], length.out = lns[1])

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata)

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @importFrom utils read.table
#' @export
read_spa <- function(file = ".", share = NULL,
                     metadata = list(
                       id = paste(digest(Sys.info()),
                                  digest(sessionInfo()),
                                  sep = "/"),
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
    stop("file type should be 'spa'")

  trb <- file.path(file) %>% file(open = "rb", ...)

  seek(trb, 576, origin = "start")
  spr <- readBin(trb, "numeric", n = 2, size = 4)

  if (!all(spr >= 0 & spr <= 15000 & spr[1] > spr[2]))
    stop("unknown spectral range")

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

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata)

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @importFrom hyperSpec read.jdx
#' @export
read_jdx <- function(file = ".", share = NULL,
                     metadata = list(
                       id = paste(digest(Sys.info()),
                                  digest(sessionInfo()),
                                  sep = "/"),
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
  jdx <- read.jdx(file, ...)

  x <- jdx@wavelength
  y <- as.numeric(unname(jdx@data$spc[1,]))

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata)

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @importFrom hyperSpec read.spc
#' @export
read_spc <- function(file = ".", share = NULL,
                     metadata = list(
                       id = paste(digest(Sys.info()),
                                  digest(sessionInfo()),
                                  sep = "/"),
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

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata)

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @importFrom hexView readRaw blockString
#' @export
read_0 <- function(file = ".", share = NULL,
                   metadata = list(
                     id = paste(digest(Sys.info()),
                                digest(sessionInfo()),
                                sep = "/"),
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
  if (!grepl("\\.[0-999]$", ignore.case = T, file))
    stop("file type should be '0'")

  pa <- readRaw(file, offset = 0, nbytes = file.info(file)$size, human = "char",
                size = 1, endian = "little")
  pr <- pa$fileRaw

  # Get positions where the following parameters are found in the file
  codes <- c("ZFF", "RES", "SNM", "DAT", "LWN", "FXV", "LXV", "NPT", "MXY",
             "MNY", "END", "TIM")

  z <- grepRaw(codes[1], pr, all = TRUE)[1] + 5
  re <- grepRaw(codes[2], pr, all = TRUE)[1] + 5
  snm <- grepRaw(codes[3], pr, all = TRUE)[1] + 7
  dat <- grepRaw(codes[4], pr, all = TRUE)[1] + 7
  lwn <- grepRaw(codes[5], pr, all = TRUE)[1] + 7
  fx <- grepRaw(codes[6], pr, all = TRUE)[3] + 7
  lx <- grepRaw(codes[7], pr, all = TRUE)[3] + 7
  npt0 <- grepRaw(codes[8], pr, all = TRUE)[2] + 3
  npt1 <- grepRaw(codes[8], pr, all = TRUE)[3] + 7
  mxy <- grepRaw(codes[9], pr, all = TRUE)[1] + 7
  mny <- grepRaw(codes[10], pr, all = TRUE)[3] + 7
  end <- grepRaw(codes[11], pr, all = TRUE) + 11
  tim <- grepRaw(codes[12], pr, all = TRUE) + 11

  ## Calculate end and start of each block
  offs <- sapply(5:10, function(x){end[x]})
  byts <- diff(offs)
  ZFF <- readRaw(file, offset = z, nbytes = 4, human = "int", size = 2)[[5]][1]
  RES <- readRaw(file, offset = re, nbytes = 4, human = "int", size = 2)[[5]][1]
  snm.lab.material <- blockString(readRaw(file, offset = snm, nbytes = 22,
                                          human = "char", size = 1,
                                          endian = "little"))

  ## Get number of data points for each spectra data block
  NPT0 <- readRaw(file, offset = npt0, nbytes = 12, human = "int", size = 4)[[5]][2]
  NPT1 <- readRaw(file, offset = npt1, nbytes = 4, human = "int", size = 4)[[5]][1]
  fxv <- readRaw(file, offset = fx, nbytes = 16, human = "real", size = 8)[[5]][1] ## fxv = frequency of first point
  lxv <- readRaw(file, offset = lx, nbytes = 16, human = "real", size = 8)[[5]][1] ## lxv = frequency of last point
  x <- rev(seq(lxv, fxv, (fxv - lxv) / (NPT1 - 1)))

  ## Read all through all the data blocks inside the OPUS file:
  nbytes1 <- NPT0 * 4 ## initial parameters
  smxa <- c()
  smna <- c()
  nbytes.f <- NPT1 * 4
  if(offs[1] < 2000) {
    offs.f <- offs[3]
  }

  if(offs[1] > 20000) {
    offs.f <- offs[2]
  }

  # Selected spectra block
  opus.p <- readRaw(file, width = NULL, offset = offs.f - 4,
                    nbytes = nbytes.f, human = "real", size = 4,
                    endian = "little")
  y <- opus.p[[5]]

  os <- as_OpenSpecy(x, data.table(intensity = y), metadata = metadata)

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
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
