#' @title Read spectral data from Bruker OPUS binary files
#'
#' @description
#' Read file(s) acquired with a Bruker Vertex FTIR Instrument. This function
#' is basically a fork of \code{opus_read()} from
#' \url{https://github.com/pierreroudier/opusreader}.
#'
#' @param file character vector with path to file(s).
#' @param metadata a named list of the metadata; see
#' \code{\link{as_OpenSpecy}()} for details.
#' @param type character vector of spectra types to extract from OPUS binary
#' file. Default is `"spec"`, which will extract the final spectra, e.g.
#' expressed in absorbance (named `AB` in Bruker OPUS programs). Possible
#' additional values for the character vector supplied to `type` are
#' `"spec_no_atm_comp"` (spectrum of the sample without compensation for
#' atmospheric gases, water vapor and/or carbon dioxide),
#' `"sc_sample"` (single channel spectrum of the sample measurement), `"sc_ref"`
#' (single channel spectrum of the reference measurement),
#' `"ig_sample"` (interferogram of the sample measurement) and `"ig_ref"`
#' (interferogram of the reference measurement).
#' @param digits Integer that specifies the number of decimal places used to
#' round the wavenumbers (values of x-variables).
#' @param atm_comp_minus4offset Logical whether spectra after atmospheric
#' compensation are read with an offset of -4 bytes from Bruker OPUS files;
#' default is `FALSE`.
#'
#' @details
#' The type of spectra returned by the function when using
#' `type = "spec"` depends on the setting of the Bruker instrument: typically,
#' it can be either absorbance or reflectance.
#'
#' The type of spectra to extract from the file can also use Bruker's OPUS
#' software naming conventions, as follows:
#'
#' - `ScSm` corresponds to `sc_sample`
#' - `ScRf` corresponds to `sc_ref`
#' - `IgSm` corresponds to `ig_sample`
#' - `IgRf` corresponds to `ig_ref`
#'
#' @return
#' An \code{OpenSpecy} object.
#'
#' @examples
#' read_extdata("ftir_ps.0") |> read_opus()
#'
#' @author Philipp Baumann, Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_spec}()} for reading .y(a)ml, .json, or .rds (OpenSpecy)
#' files;
#' \code{\link{read_text}()}, \code{\link{read_asp}()}, \code{\link{read_spa}()},
#' \code{\link{read_spc}()}, and \code{\link{read_jdx}()} for text files, .asp,
#' .spa, .spa, .spc, and .jdx formats, respectively;
#' \code{\link{read_text}()} for reading .dat (ENVI) files;
#' \code{\link{read_zip}()} and \code{\link{read_any}()} for wrapper functions;
#' \code{\link{read_opus_raw}()};
#'
#' @importFrom stats approx
#' @importFrom data.table as.data.table data.table
#' @export
read_opus <- function(file,
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
                      type = "spec", digits = 1L,
                      atm_comp_minus4offset = FALSE) {
  if (!all(grepl("\\.[0-999]$", ignore.case = T, file)))
    stop("file type should be '0'", call. = F)

  res <- lapply(
    file,
    function(fn) {

      if (!file.exists(fn)) stop(paste0("file '", fn, "' does not exist"),
                                 call. = F)

      # Get raw vector
      rw <- readBin(fn, "raw", n = file.size(fn))
      out <- read_opus_raw(rw, type = type,
                           atm_comp_minus4offset = atm_comp_minus4offset)

      return(out)
    })

  # If there was only one file to read, we un-nest the list one level
  if (length(file) == 1) {
    res <- res[[1]]
    res$spec <- as.data.table(c(res$spec))
  } else {
    if (length(type) > 1) {
      stop("simple output is currently only implemented for one value of the ",
           "`type` option.\nA workaround this limitation is to use the ",
           "`lapply` function, e.g.:\n\nlapply(c('spec', 'sc_ref'), ",
           "function(x) read_opus(file, type = x))", call. = F)
    }

    # Fetch wavenumbers
    wns <- lapply(res, function(x) round(x$wavenumbers, digits = digits))

    # Arbitrarily take the first rounded WN as the reference one
    wn_ref <- wns[[1]]

    # Check the wavenumbers have all the same length
    if (length(unique(sapply(wns, length))) > 1) {
      warning("Spectra don't all have the same number of wavenumbers; ",
              "interpolation will be used to combine them in a matrix",
              call. = F)
    }

    specs <- lapply(
      res,
      function(x) {

        id <- switch(type,
                     spec = "spec",
                     spec_no_atm_comp = "spec_no_atm_comp",
                     sc_sample = "sc_sample",
                     sc_ref = "sc_ref",
                     ig_sample = "ig_sample",
                     ig_ref = "ig_ref"
        )

        # Grab correct wavenumbers for interpolation
        wn <- switch(type,
                     spec = x$wavenumbers,
                     spec_no_atm_comp = x$wavenumbers,
                     sc_sample = x$wavenumbers_sc_sample,
                     sc_ref = x$wavenumbers_sc_ref,
                     ig_sample = x$wavenumbers_sc_sample,
                     ig_ref = x$wavenumbers_sc_ref
        )

        # Linear interpolation to get spectra at rounded wavenumber
        s <- approx(
          x = wn,
          y = x[[id]],
          xout = wn_ref,
          method = "linear"
        )$y
        names(s) <- as.character(wn_ref)

        return(s)
      })

    md <- do.call(rbind, lapply(res, function(x) x$metadata))

    res <- list(
      wavenumbers = wns[[1]],
      spec = as.data.table(specs),
      metadata = as.data.table(md)
    )

  }

  os <- as_OpenSpecy(x = res$wavenumbers,
                     spectra = res$spec,
                     metadata = cbind(as.data.table(metadata), res$metadata),
                     session_id = T)

  return(os)
}
