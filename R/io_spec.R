#' @rdname io_spec
#'
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading and writing spectral data from Open Specy .yaml, .json,
#' or .rds.
#'
#' @details
#'
#' The \code{metadata} argument may contain a named list with the following
#' details (\code{*} = minimum recommended):
#'
#' \tabular{ll}{
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
#' \code{session_id}: \tab A unique user and session identifier; populated
#' automatically with \code{paste(digest(Sys.info()), digest(sessionInfo()),
#' sep = "/")}\cr
#' \code{file_id}: \tab A unique file identifier; populated automatically
#' with \code{digest(object[c("wavenumber", "spectra")])}\cr
#' }
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param file file to be read from or written to.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param method submethod to be used for reading text files; defaults to
#' \code{\link[data.table]{fread}()} but \code{\link[utils]{read.csv}()} works
#' as well.
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
#'
#' @importFrom magrittr %>%
#' @importFrom data.table data.table as.data.table fread
#' @export
write_spec <- function(x, ...) {
  UseMethod("write_spec")
}

#' @rdname io_spec
#'
#' @export
write_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname io_spec
#'
#' @importFrom yaml write_yaml
#' @importFrom jsonlite write_json
#' @export
write_spec.OpenSpecy <- function(x, file, method = NULL, ...) {
  if (is.null(method)) {
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      write_yaml(x, file = file, ...)
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      write_json(x, path = file, dataframe = "columns", ...)
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      saveRDS(x, file = file, ...)
    } else {
      stop("unknown file type: specify a method to write custom formats",
           call. = F)
    }
  } else {
    do.call(method, list(x, file, ...))
  }
}

#' @rdname io_spec
#'
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @export
read_spec <- function(file, share = NULL, method = NULL, ...) {
  if (is.null(method)) {
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      yml <- read_yaml(file = file, ...)

      os <- as_OpenSpecy(yml$wavenumber,
                         spectra = as.data.table(yml$spectra),
                         coords = as.data.table(yml$coords),
                         metadata = as.data.table(yml$metadata))
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      jsn <- read_json(file, simplifyVector = T, ...)

      os <- as_OpenSpecy(jsn$wavenumber,
                         spectra = as.data.table(jsn$spectra),
                         coords = as.data.table(jsn$coords),
                         metadata = as.data.table(jsn$metadata))
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      os <- readRDS(file, ...)
    } else {
      stop("unknown file type: specify a method to read custom formats",
           call. = F)
    }
  } else {
    io <- do.call(method, list(file, ...))

    os <- OpenSpecy(io)
  }

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#'
#' @importFrom data.table rbindlist
#' @export
c_spec <- function(...) {
    cin <- c(...)

    lst <- tapply(cin, names(cin), FUN = function(x) unname((x)))

    as_OpenSpecy(
        x = lst$wavenumber[[1]],
        # TODO: Probably should add a check to make sure all the wavenumbers are
        # aligned before doing this.
        spectra = as.data.table(lst$spectra),
        metadata = rbindlist(lst$metadata, fill = T)
    )
}
