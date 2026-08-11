#' @name io_spec
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading and writing spectral data to and from OpenSpecy format.
#' \code{OpenSpecy} objects are lists with components `wavenumber`, `spectra`,
#' and `metadata`; their supported formats are .json, .csv, and .rds.
#' A file-backed `FileSpecs` method writes a new ENVI pair.
#'
#' @param x an object of class \code{\link{OpenSpecy}} or a file-backed
#'   `FileSpecs` descriptor. File-backed objects are exported as a new ENVI
#'   header/binary pair and never overwrite source members.
#' @param file file path to be read from or written to.
#' @param method optional custom reader or `OpenSpecy` writer. `FileSpecs`
#'   rejects custom writers so its source-protection guarantee cannot be
#'   bypassed. Otherwise defaults to the file extension's method.
#' @param digits number of significant digits to use when formatting numeric
#' values; defaults to \code{\link[base]{getOption}("digits")}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @details
#' Due to floating point number errors there may be some differences in the
#' precision of the numbers returned if using multiple devices for .json and
#' .csv files but the numbers should be nearly identical.
#' \code{\link[base]{readRDS}()} should return the exact same object every time.
#' `write_spec.FileSpecs()` streams one complete rectangular region to a new
#' ENVI BIP pair using float64 spectra and a round-trip-safe wavelength axis.
#' It refuses custom writers, source-member targets, existing outputs, and
#' multi-region or incomplete views.
#'
#' @return
#' \code{read_spec()} reads data formatted as an \code{OpenSpecy} object and
#' returns a list object of class \code{\link{OpenSpecy}} containing spectral
#' data.
#' \code{write_spec()} writes spectral data. For `FileSpecs`, it invisibly
#' returns the new ENVI header and binary paths; other methods are called for
#' their file-writing side effect.
#' \code{as_hyperspec()} converts an \code{OpenSpecy} object to a
#' \code{\link[hyperSpec]{hyperSpec-class}} object.
#'
#' @examples
#' read_extdata("raman_hdpe.json") |> read_spec()
#' read_extdata("raman_hdpe.rds") |> read_spec()
#' read_extdata("raman_hdpe.csv") |> read_spec()
#'
#' \dontrun{
#' data(raman_hdpe)
#' write_spec(raman_hdpe, "raman_hdpe.json")
#' write_spec(raman_hdpe, "raman_hdpe.rds")
#' write_spec(raman_hdpe, "raman_hdpe.csv")
#'
#' # Convert an OpenSpecy object to a hyperSpec object
#' hyper <- as_hyperSpec(raman_hdpe)
#' }
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{OpenSpecy}()};
#' \code{\link{read_text}()}, \code{\link{read_asp}()}, \code{\link{read_spa}()},
#' \code{\link{read_spc}()}, and \code{\link{read_jdx}()} for text files, .asp,
#' .spa, .spa, .spc, and .jdx formats, respectively;
#' \code{\link{read_zip}()} and \code{\link{read_any}()} for wrapper functions;
#' \code{\link[base]{saveRDS}()}; \code{\link[base]{readRDS}()};
#' \code{\link[jsonlite]{write_json}()}; \code{\link[jsonlite]{read_json}()};
#'
#' @importFrom jsonlite write_json read_json
#' @importFrom data.table as.data.table fwrite
#'
#' @export
write_spec <- function(x, ...) {
  UseMethod("write_spec")
}

#' @rdname io_spec
#'
#' @export
write_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname io_spec
#'
#' @export
write_spec.OpenSpecy <- function(x, file, method = NULL,
                                 digits = getOption("digits"),
                                 ...) {
  x <- as_OpenSpecy(x)

  if (is.null(method)) {
    if (grepl("\\.json$", file, ignore.case = TRUE)) {
      io <- x
      io$spectra <- as.data.frame(x$spectra, check.names = FALSE)
      write_json(io, path = file, dataframe = "columns", digits = digits, ...)
    } else if (grepl("\\.rds$", file, ignore.case = TRUE)) {
      saveRDS(x, file = file, ...)
    } else if (grepl("\\.csv$", file, ignore.case = TRUE)) {
      wave_names <- round(x$wavenumber, 0)
      spectra <- t(x$spectra)
      colnames(spectra) <- wave_names
      flat_specy <- cbind(spectra, x$metadata)
      fwrite(flat_specy, file = file)
    }
    else {
      stop("unknown file type: specify a method to write custom formats or ",
           "provide one of the supported .json, .rds, or .csv formats as ",
           "file extension", call. = F)
    }
  } else {
    do.call(method, list(x, file, ...))
  }
}

#' @rdname io_spec
#'
#' @export
read_spec <- function(file, method = NULL, ...) {
  if (is.null(method)) {
    if (grepl("\\.json$", file, ignore.case = TRUE)) {
      jsn <- read_json(file, simplifyVector = T, ...)

      os <- as_OpenSpecy(jsn$wavenumber,
                         spectra = as.data.frame(jsn$spectra,
                                                 check.names = FALSE),
                         metadata = data.table(as.data.table(jsn$metadata),
                                               file_name = basename(file)),
                         coords = NULL)
    } else if (grepl("\\.rds$", file, ignore.case = TRUE)) {
      os <- as_OpenSpecy(readRDS(file, ...))
      os$metadata$file_name <- basename(file)
    }
      else if (grepl("\\.csv$", file, ignore.case = TRUE)) {
          os <- read_text(file, ...)
          os$metadata$file_name <- basename(file)
    } else {
      stop("unknown file type: specify a method to read custom formats or ",
           "provide files of one of the supported file types .json, .rds, .csv",
           call. = F)
      }
  } else {
    io <- do.call(method, list(file, ...))

    os <- OpenSpecy(io, coords = NULL)
    os$metadata$file_name <- basename(file)
  }

  return(os)
}

#' @rdname io_spec
#' @importFrom methods new
#'
#' @export
as_hyperSpec <- function(x) {
  x <- as_OpenSpecy(x)
  new("hyperSpec", spc = t(x$spectra),
      wavelength = x$wavenumber)
}
