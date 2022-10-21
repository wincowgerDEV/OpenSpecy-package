#' @rdname as_OpenSpecy
#'
#' @title Coerce to Open Specy
#'
#' @description
#' Functions to check if an object is an Open Specy spectrum, or coerce it if
#' possible.
#'
#' @details
#' details
#'
#' @param x x.
#' @param spectra spectra.
#' @param coords coords = "gen_grid".
#' @param metadata file = NULL.
#' @param colnames cols.
#' @param \ldots args.
#'
#' @return
#' return
#'
#' @examples
#' c()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' seealso.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @export
as_OpenSpecy <- function(x, ...) {
  UseMethod("as_OpenSpecy")
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
        warning("ambiguous column names: taking 'wavenumber' data from the",
                " first column; use 'colnames' to supply user-defined columns")
      wavenumber <- x[[grep("wav", ignore.case = T, names(x))[1L]]]
      wn <- names(x)[grep("wav", ignore.case = T, names(x))]
    } else {
      warning("ambiguous column names: taking 'wavenumber' data from the",
              " first column; use 'colnames' to supply user-defined columns")
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
      warning("ambiguous column names: taking 'spectra' data from all but the",
              " 'wavenumber' column; use 'colnames' to supply user-defined",
              " columns")
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
as_OpenSpecy.default <- function(x, spectra, coords = "gen_grid",
                                 metadata = list(
                                   id = paste(digest(Sys.info()),
                                              digest(sessionInfo()),
                                              sep = "/"),
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
                                   spectral_resolution = NULL,
                                   laser_light_used = NULL,
                                   number_of_accumulations = NULL,
                                   total_acquisition_time_s = NULL,
                                   data_processing_procedure = NULL,
                                   level_of_confidence_in_identification = NULL,
                                   other_info = NULL,
                                   license = "CC BY-NC"),
                                 ...) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x))
    stop("'x' must be numeric or logical")
  if (!inherits(spectra, c("data.frame", "matrix")))
    stop("'spectra' must inherit from data.frame or matrix")
  if (!sapply(spectra, is.numeric)[1L] && !sapply(spectra, is.complex)[1L] &&
      !sapply(spectra, is.logical)[1L])
    stop("at least the first column of 'spectra' must be numeric or logical")
  if (length(x) != nrow(spectra))
    stop("'x' and 'spectra' must be of equal length")

  obj <- structure(list(), class = c("list", "OpenSpecy"))

  obj$wavenumber <- x
  obj$spectra <- as.data.table(spectra)

  if (inherits(coords, "character")) {
    obj$coords <- do.call(coords, list(ncol(obj$spectra)))
  } else if (inherits(coords, c("data.frame", "list")) &&
             all(is.element(c("x", "y"), names(coords)))) {
    obj$coords <- as.data.table(coords)
  } else {
    stop("inconsistent input for 'coords'")
  }

  if (!is.null(metadata)) {
    if (inherits(metadata, c("data.frame", "list"))) {
      obj$coords <- cbind(obj$coords, as.data.table(metadata))
    } else {
      stop("inconsistent input for 'metadata'")
    }
  }

  return(obj)
}

#' @rdname as_OpenSpecy
#'
#' @export
is_OpenSpecy <- function (x) {
  inherits(x, "OpenSpecy")
}

#' @rdname as_OpenSpecy
#'
#' @export
OpenSpecy <- as_OpenSpecy

#' @rdname as_OpenSpecy
#'
#' @export
gen_grid <- function(x) {
  base <- sqrt(x)
  expand.grid(x = 1:ceiling(base), y = 1:ceiling(base))[1:x,] %>%
    as.data.table
}
