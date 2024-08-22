#' @rdname as_OpenSpecy
#'
#' @title Create \code{OpenSpecy} objects
#'
#' @description
#' Functions to check if an object is an OpenSpecy, or coerce it if
#' possible.
#'
#' @param x depending on the method, a list with all OpenSpecy parameters,
#' a vector with the wavenumbers for all spectra, or a data.frame with a full
#' spectrum in the classic Open Specy format.
#' @param spectra spectral intensities formatted as a data.table with one column
#' per spectrum.
#' @param metadata metadata for each spectrum with one row per spectrum,
#' see details.
#' @param attributes a list of attributes describing critical aspects for interpreting the spectra.
#' see details.
#' @param coords spatial coordinates for the spectra.
#' @param session_id logical. Whether to add a session ID to the metadata.
#' The session ID is based on current session info so metadata of the same
#' spectra will not return equal if session info changes. Sometimes that is
#' desirable.
#' @param colnames names of the wavenumber column and spectra column, makes
#' assumptions based on column names or placement if \code{NULL}.
#' @param n number of spectra to generate the spatial coordinate grid with.
#' @param \ldots additional arguments passed to submethods.
#'
#' @details
#' \code{as_OpenSpecy()} converts spectral datasets to a three part list;
#' the first with a vector of the wavenumbers of the spectra,
#' the second with a \code{data.table} of all spectral intensities ordered as
#' columns,
#' the third item is another \code{data.table} with any metadata the user
#' provides or is harvested from the files themselves.
#'
#' The \code{metadata} argument may contain a named list with the following
#' details (\code{*} = minimum recommended).
#'
#' \describe{
#'   \item{`file_name*`}{The file name, defaults to
#'   \code{\link[base]{basename}()} if not specified}
#'   \item{`user_name*`}{User name, e.g. "Win Cowger"}
#'   \item{`contact_info`}{Contact information, e.g. "1-513-673-8956,
#'   wincowger@@gmail.com"}
#'   \item{`organization`}{Affiliation, e.g. "University of California,
#'   Riverside"}
#'   \item{`citation`}{Data citation, e.g. "Primpke, S., Wirth, M., Lorenz, C.,
#'   & Gerdts, G. (2018). Reference database design for the automated analysis
#'   of microplastic samples based on Fourier transform infrared (FTIR)
#'   spectroscopy. \emph{Analytical and Bioanalytical Chemistry}.
#'   \doi{10.1007/s00216-018-1156-x}"}
#'   \item{`spectrum_type*`}{Raman or FTIR}
#'   \item{`spectrum_identity*`}{Material/polymer analyzed, e.g.
#'   "Polystyrene"}
#'   \item{`material_form`}{Form of the material analyzed, e.g. textile fiber,
#'   rubber band, sphere, granule }
#'   \item{`material_phase`}{Phase of the material analyzed (liquid, gas, solid) }
#'   \item{`material_producer`}{Producer of the material analyzed, e.g. Dow }
#'   \item{`material_purity`}{Purity of the material analyzed, e.g. 99.98%}
#'   \item{`material_quality`}{Quality of the material analyzed, e.g.
#'   consumer product, manufacturer material, analytical standard,
#'   environmental sample }
#'   \item{`material_color`}{Color of the material analyzed,
#'   e.g. blue, #0000ff, (0, 0, 255) }
#'   \item{material_other}{Other material description, e.g. 5 µm diameter
#'   fibers, 1 mm spherical particles }
#'   \item{`cas_number`}{CAS number, e.g. 9003-53-6 }
#'   \item{`instrument_used`}{Instrument used, e.g. Horiba LabRam }
#'   \item{instrument_accessories}{Instrument accessories, e.g.
#'   Focal Plane Array, CCD}
#'   \item{`instrument_mode`}{Instrument modes/settings, e.g.
#'   transmission, reflectance }
#'   \item{`intensity_units*`}{Units of the intensity values for the spectrum,
#'   options transmittance, reflectance, absorbance }
#'   \item{`spectral_resolution`}{Spectral resolution, e.g. 4/cm }
#'   \item{`laser_light_used`}{Wavelength of the laser/light used, e.g.
#'   785 nm }
#'   \item{`number_of_accumulations`}{Number of accumulations, e.g 5 }
#'   \item{`total_acquisition_time_s`}{Total acquisition time (s), e.g. 10 s}
#'   \item{`data_processing_procedure`}{Data processing procedure,
#'   e.g. spikefilter, baseline correction, none }
#'   \item{`level_of_confidence_in_identification`}{Level of confidence in
#'   identification, e.g. 99% }
#'   \item{`other_info`}{Other information }
#'   \item{`license`}{The license of the shared spectrum; defaults to
#'   \code{"CC BY-NC"} (see \url{https://creativecommons.org/licenses/by-nc/4.0/}
#'   for details). Any other creative commons license is allowed, for example,
#'   CC0 or CC BY}
#'   \item{`session_id`}{A unique user and session identifier; populated
#'   automatically with \code{paste(digest(Sys.info()), digest(sessionInfo()),
#'   sep = "/")}}
#'   \item{`file_id`}{A unique file identifier; populated automatically
#'   with \code{digest(object[c("wavenumber", "spectra")])}}
#' }
#'
#' The \code{attributes} argument may contain a named list with the following
#' details, when set, they will be used to automate transformations and warning messages:
#'
#' \describe{
#'   \item{`intensity_units`}{supported options include `"absorbance"`,
#'   `"transmittance"`, or `"reflectance"`}
#'   \item{`derivative_order`}{supported options include `"0"`, `"1"`, or
#'   `"2"`}
#'   \item{`baseline`}{supported options include `"raw"` or `"nobaseline"`}
#'   \item{`spectra_type`}{supported options include `"ftir"` or `"raman"`}
#' }
#'
#'
#' @return
#' \code{as_OpenSpecy()} and \code{OpenSpecy()} returns three part lists
#' described in details.
#' \code{is_OpenSpecy()} returns \code{TRUE} if the object is an OpenSpecy and
#' \code{FALSE} if not.
#' \code{gen_grid()} returns a \code{data.table} with \code{x} and \code{y}
#' coordinates to use for generating a spatial grid for the spectra if one is
#' not specified in the data.
#'
#' @examples
#' data("raman_hdpe")
#'
#' # Inspect the spectra
#' raman_hdpe # see how OpenSpecy objects print.
#' raman_hdpe$wavenumber # look at just the wavenumbers of the spectra.
#' raman_hdpe$spectra # look at just the spectral intensities data.table.
#' raman_hdpe$metadata # look at just the metadata of the spectra.
#'
#' # Creating a list and transforming to OpenSpecy
#' as_OpenSpecy(list(wavenumber = raman_hdpe$wavenumber,
#'                   spectra = raman_hdpe$spectra,
#'                   metadata = raman_hdpe$metadata[,-c("x", "y")]))
#'
#' # If you try to produce an OpenSpecy using an OpenSpecy it will just return
#' # the same object.
#' as_OpenSpecy(raman_hdpe)
#'
#' # Creating an OpenSpecy from a data.frame
#' as_OpenSpecy(x = data.frame(wavenumber = raman_hdpe$wavenumber,
#'                             spectra = raman_hdpe$spectra$intensity))
#'
#' # Test that the spectrum is formatted as an OpenSpecy object.
#' is_OpenSpecy(raman_hdpe)
#' is_OpenSpecy(raman_hdpe$spectra)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_spec}()} for reading \code{OpenSpecy} objects.
#'
#' @importFrom data.table as.data.table
#' @importFrom digest digest
#' @export
as_OpenSpecy <- function(x, ...) {
  UseMethod("as_OpenSpecy")
}

#' @rdname as_OpenSpecy
#'
#' @export
as_OpenSpecy.OpenSpecy <- function(x, session_id = FALSE, ...) {
  if(session_id)
    x$metadata$session_id <- paste(digest(Sys.info()),
                                   digest(sessionInfo()),
                                   sep = "/")
  if(!c("file_id") %in% names(x$metadata))
    x$metadata$file_id = digest(x[c("wavenumber", "spectra")])

  return(x)
}

#' @rdname as_OpenSpecy
#'
#' @export
as_OpenSpecy.list <- function(x, ...) {
  do.call("as_OpenSpecy", unname(x))
}

#' @rdname as_OpenSpecy
#'
#' @export
as_OpenSpecy.hyperSpec <- function(x, ...) {
  do.call("as_OpenSpecy", list(x = x@wavelength,
                               spectra = as.data.table(t(x$spc)), ...))
}

#' @rdname as_OpenSpecy
#'
#' @export
as_OpenSpecy.data.frame <- function(x, colnames = list(wavenumber = NULL,
                                                       spectra = NULL), ...) {
  x <- as.data.table(x)

  # Try to find spectral data
  if (is.null(colnames$wavenumber)) {
    if (any(grepl("wav", ignore.case = T, names(x)))) {
      if (length(grep("wav", ignore.case = T, names(x))) > 1L)
        message("Ambiguous column names: taking 'wavenumber' data from the",
                " first column; use 'colnames' to supply user-defined columns",
                call. = F)
      wavenumber <- x[[grep("wav", ignore.case = T, names(x))[1L]]]
      wn <- names(x)[grep("wav", ignore.case = T, names(x))]
    } else {
      message("Ambiguous column names: taking 'wavenumber' data from the",
              " first column; use 'colnames' to supply user-defined columns",
              call. = F)
      wavenumber <- x[[1L]]
      wn <- names(x)[1L]
    }
  } else {
    wavenumber <- x[[colnames$wavenumber]]
    wn <- colnames$wavenumber
  }

  if (is.null(colnames$spectra)) {
    if (any(grep("(transmit.*)|(reflect.*)|(abs.*)|(intens.*)", ignore.case = T,
                 names(x)))) {
      spectra <- x[, grepl("(transmit.*)|(reflect.*)|(abs.*)|(intens.*)",
                           ignore.case = T, names(x)), with = F]
    } else {
      message("Ambiguous column names: taking 'spectra' data from all but the",
              " 'wavenumber' column; use 'colnames' to supply user-defined",
              " columns", call. = F)
      spectra <- x[, -wn, with = F]
    }
  } else {
    spectra <- x[, colnames$spectra, with = F]
  }

  do.call("as_OpenSpecy",
          list(x = wavenumber, spectra = spectra, ...))
}

#' @rdname as_OpenSpecy
#'
#' @export
as_OpenSpecy.default <- function(x, spectra,
                                 metadata = list(
                                   file_name = NULL,
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
                                   intensity_units = NULL,
                                   spectral_resolution = NULL,
                                   laser_light_used = NULL,
                                   number_of_accumulations = NULL,
                                   total_acquisition_time_s = NULL,
                                   data_processing_procedure = NULL,
                                   level_of_confidence_in_identification = NULL,
                                   other_info = NULL,
                                   license = "CC BY-NC"),
                                 attributes = list(
                                   intensity_unit = NULL,
                                   derivative_order = NULL,
                                   baseline = NULL,
                                   spectra_type = NULL
                                 ),
                                 coords = "gen_grid",
                                 session_id = FALSE,
                                 ...) {
  if (!is.numeric(x) || !is.vector(x))
    stop("'x' must be numeric vector", call. = F)
  if (!inherits(spectra, c("data.frame", "matrix")))
    stop("'spectra' must inherit from data.frame or matrix", call. = F)
  if (!sapply(spectra, is.numeric)[1L] && !sapply(spectra, is.complex)[1L] &&
      !sapply(spectra, is.logical)[1L])
    stop("at least the first column of 'spectra' must be numeric or logical",
         call. = F)
  if(length(unique(names(spectra))) != ncol(spectra))
    stop("column names in 'spectra' must be unique", call. = F)
  if (length(x) != nrow(spectra))
    stop("'x' and 'spectra' must be of equal length", call. = F)

  obj <- structure(list(),
                   class = c("OpenSpecy", "list"),
                   intensity_unit = attributes$intensity_unit,
                   derivative_order = attributes$derivative_order,
                   baseline = attributes$baseline,
                   spectra_type = attributes$spectra_type
  )

  obj$wavenumber <- x[order(x)]

  obj$spectra <- as.data.table(spectra)[order(x)]

  if (inherits(coords, "character") && !any(is.element(c("x", "y"),
                                                       names(metadata)))) {
    obj$metadata <- do.call(coords, list(ncol(obj$spectra)))
  } else if(inherits(coords, c("data.frame", "list")) &&
             all(is.element(c("x", "y"), names(coords)))) {
    obj$metadata <- as.data.table(coords)
  } else {
    if(!all(is.element(c("x", "y"), names(metadata))))
      stop("inconsistent input for 'coords'", call. = F)
    obj$metadata <- data.table()
  }
  if (!is.null(metadata)) {
    if (inherits(metadata, c("data.frame", "list"))) {
      obj$metadata <- cbind(obj$metadata, as.data.table(metadata))
      obj$metadata$col_id <- names(obj$spectra)
      if(session_id)
        obj$metadata$session_id <- paste(digest(Sys.info()),
                                         digest(sessionInfo()),
                                         sep = "/")
      if(!c("file_id") %in% names(obj$metadata))
        obj$metadata$file_id <- digest(obj[c("wavenumber", "spectra")])
    } else {
      stop("inconsistent input for 'metadata'", call. = F)
    }
  }

  return(obj)
}

#' @rdname as_OpenSpecy
#'
#' @export
is_OpenSpecy <- function(x) {
  inherits(x, "OpenSpecy")
}

#' @rdname as_OpenSpecy
#'
#' @importFrom data.table is.data.table
#' @export
check_OpenSpecy <- function(x) {
  if(!(cos <- is_OpenSpecy(x)))
    warning("Object 'x' is not of class 'OpenSpecy'", call. = F)
  if(!(cln <- identical(names(x), c("wavenumber", "spectra", "metadata"))))
    warning("Names of the object components are incorrect", call. = F)
  if(!(cw <- is.vector(x$wavenumber)))
    warning("Wavenumber is not a vector", call. = F)
  if(!(cwn <- !any(is.na(x$wavenumber))))
    warning("Wavenumber values have NA", call. = F)
  if(!(cs <- is.data.table(x$spectra)))
    warning("Spectra are not of class 'data.table'", call. = F)
  if(!(csn <- !any(vapply(x$spectra, function(x){all(is.na(x))},
                          FUN.VALUE = logical(1)))))
    warning("Some of the spectra have all NA values", call. = F)
  if(!(cm <- is.data.table(x$metadata)))
    warning("Metadata are not a 'data.table'", call. = F)
  if(!(cr <- ncol(x$spectra) == nrow(x$metadata)))
    warning("Number of columns in spectra is not equal to number of rows ",
            "in metadata", call. = F)
  if(!(csz <- nrow(x$spectra) != 0))
    warning("There are no spectral intensities in your spectra", call. = F)
  if(!(cl <- length(x$wavenumber) == nrow(x$spectra)))
    warning("Length of wavenumber is not equal to number of rows in spectra",
            call. = F)
  if(!(cu <- length(unique(names(x$spectra))) == ncol(x$spectra)))
    warning("Column names in spectra are not unique", call. = F)
  if(!(cv <- length(unique(names(x$metadata))) == ncol(x$metadata)))
    warning("Column names in metadata are not unique", call. = F)
  if(!(co <- identical(order(x$wavenumber), 1:length(x$wavenumber)) |
       identical(order(x$wavenumber), length(x$wavenumber):1)))
    warning("Wavenumbers should be a continuous sequence for all OpenSpecy ",
            "functions to run smoothly", call. = F)

  chk <- all(cw, cs, cm, cr, cl, cu, cv, co, csz, csn, cwn, cln, cos)

  return(chk)
}

#' @rdname as_OpenSpecy
#'
#' @export
OpenSpecy <- function(x, ...) {
  if (is_OpenSpecy(x)) {
    return(x)
  } else {
    do.call("as_OpenSpecy", list(x, ...))
  }
}

#' @rdname as_OpenSpecy
#'
#' @export
gen_grid <- function(n) {
  base <- sqrt(n)

  expand.grid(x = 1:ceiling(base), y = 1:ceiling(base))[1:n,] |>
    as.data.table()
}
