#' @title Share data with the Open Specy community
#'
#' @description
#' This helper function shares spectral data and metadata with the Open Specy
#' community.
#'
#' @details
#' The \code{metadata} argument may contain a named vector with the following
#' details (\code{*} = mandatory):
#'
#' \tabular{ll}{
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
#' @param data a data frame containing the spectral data; columns should be
#' named \code{"wavenumber"} and \code{"intensity"}.
#' @param metadata a named vector of the metadata to share; see details below.
#' @param share accepts any local directory to save the spectrum for later
#' sharing via e-mail to \email{wincowger@@gmail.com};
#' \code{"system"} (default) uses the Open Specy package directory at
#' \code{system.file("extdata", package = "OpenSpecy")};
#' if a correct API token exists, \code{"dropbox"} shares the spectrum with the
#' cloud.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' \code{share_spec()} returns only messages/warnings
#'
#' @examples
#' data("raman_hdpe")
#' share_spec(raman_hdpe)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_text}()}
#'
#' @importFrom magrittr %>%
#' @importFrom digest digest
#' @importFrom utils write.csv
#'
#' @export
share_spec <- function(data, ...) {
  UseMethod("share_spec")
}

#' @rdname share_spec
#'
#' @export
share_spec.default <- function(data, ...) {
  stop("object needs to be of class 'data.frame'")
}

#' @rdname share_spec
#'
#' @export
share_spec.data.frame <- function(data,
                                  metadata = c(user_name = "",
                                               contact_info = "",
                                               organization = "",
                                               citation = "",
                                               spectrum_type = "",
                                               spectrum_identity = "",
                                               material_form = "",
                                               material_phase = "",
                                               material_producer = "",
                                               material_purity = "",
                                               material_quality = "",
                                               material_color = "",
                                               material_other = "",
                                               cas_number = "",
                                               instrument_used = "",
                                               instrument_accessories = "",
                                               instrument_mode = "",
                                               spectral_resolution = "",
                                               laser_light_used = "",
                                               number_of_accumulations = "",
                                               total_acquisition_time_s = "",
                                               data_processing_procedure = "",
                                               level_of_confidence_in_identification = "",
                                               other_info = "",
                                               license = "CC BY-NC"),
                                  share = "system", ...) {
  if (is.null(names(metadata))) stop("'metadata' needs to be a named vector")
  if (any(is.na(metadata[c("user_name", "spectrum_type", "spectrum_identity")])) |
      metadata["user_name"] == "" | metadata["spectrum_type"] == "" |
      metadata["spectrum_identity"] == "") {
    mex <- FALSE
    warning("fields 'user_name', 'spectrum_type', and 'spectrum_identity' ",
            "must not be empty if you like to share your metadata")
  } else {
    mex <- TRUE
  }

  id <- paste(human_ts(), digest(data, algo = "md5"), sep = "_")

  mdata <- data.frame(variable = names(metadata), input = metadata,
                      row.names = NULL)

  if (share == "system") {
    pkg <- system.file("extdata", package = "OpenSpecy")
    dir.create(file.path(pkg, "user_spectra"), showWarnings = F)
    fp <- file.path(pkg, "user_spectra")
  } else if (share == "dropbox") {
    if (!requireNamespace("rdrop2"))
      stop("share = 'dropbox' requires package 'rdrop2'")
    fp <- file.path(tempdir())
  } else {
    if (!dir.exists(share)) dir.create(share, showWarnings = F)
    fp <- file.path(share)
  }

  fd <- file.path(fp, paste0(id, ".csv"))
  fm <- file.path(fp, paste0(id, "_form", ".csv"))

  write.csv(data, fd, row.names = FALSE, quote = TRUE)

  if (mex) write.csv(mdata, fm, row.names = FALSE, quote = TRUE)

  if (share == "dropbox") {
    rdrop2::drop_upload(fd, path = "Spectra", ...)
    if (mex) rdrop2::drop_upload(fm, path = "Spectra", ...)
  }

  message("thank you for your willigness to share your data\n",
          "if you run Open Specy locally, you may consider mailing your ",
          "files to Win Cowger <wincowger@gmail.com>")
}
