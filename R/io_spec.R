#' @rdname io_spec
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading and writing spectral data to and from OpenSpecy format.
#' OpenSpecy objects are lists with components `wavenumber`, `spectra`, and `metadata`.
#' Currently supported formats are .yaml, .json, or .rds.
#'
#' @details
#' Due to floating point number errors there may be some differences in the precision 
#' of the numbers returned if using multiple devices for json and yaml files
#' but the numbers should be nearly identical. readRDS should return the exact same object every time. 
#'
#' @param x An object of class \code{\link{OpenSpecy}}.
#' @param file For `read_spec()`, file path to be read from. For `write_spec()`, file path to be written to. If writing, files will be written as the type designated in the file name.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param method Optional. A function to be used as a custom reader or writer. Defaults to the appropriate function based on the file extension.
#' @param digits number of significant digits to use when formatting numeric
#' values; defaults to \code{\link[base]{getOption}("digits")}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' \code{read_spec()} reads data formatted as an OpenSpecy object and returns a list object of class \code{\link{OpenSpecy}} containing spectral data. \cr
#' \code{write_spec()} writes a file for an object of class \code{\link{OpenSpecy}} containing spectral data. \cr
#' \code{to_hyperspec()} converts an OpenSpecy object to a hyperSpec object.
#'
#' @examples
#' read_spec(read_extdata("raman_hdpe.yml"))
#' read_spec(read_extdata("raman_hdpe.json"))
#' read_spec(read_extdata("raman_hdpe.rds"))
#' 
#' \dontrun{
#' data(raman_hdpe)
#' #Specify the file type you want to write to using the extension at the end of the file name.
#' write_spec(raman_hdpe, "raman_hdpe.yml")
#' write_spec(raman_hdpe, "raman_hdpe.json")
#' write_spec(raman_hdpe, "raman_hdpe.rds")
#'
#' # Convert an OpenSpecy object to a hyperSpec object
#' hyperOpenSpecy <- to_hyperSpec(raman_hdpe)
#' }
#'
#' @seealso
#' \code{\link{OpenSpecy}()}
#' \code{\link[base]{saveRDS}()}; \code{\link[base]{readRDS}()};
#' \code{\link[yaml]{write_yaml}()}; \code{\link[yaml]{read_yaml}()};
#' \code{\link[jsonlite]{write_json}()}; \code{\link[jsonlite]{read_json}()};
#'
#' @importFrom yaml write_yaml read_yaml
#' @importFrom jsonlite write_json read_json
#' @importFrom data.table as.data.table
#' @import hyperSpec
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
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
  if (is.null(method)) {
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      write_yaml(x, file = file, precision = digits, ...)
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      write_json(x, path = file, dataframe = "columns", digits = digits, ...)
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      saveRDS(x, file = file, ...)
    } else {
      stop("unknown file type: specify a method to write custom formats or provide one of the supported .yml, .json, or .rds formats as the file extension",
           call. = F)
    }
  } else {
    do.call(method, list(x, file, ...))
  }
}

#' @rdname io_spec
#'
#' @export
read_spec <- function(file, share = NULL, method = NULL, ...) {
  if (is.null(method)) {
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      yml <- read_yaml(file = file, ...)

      os <- as_OpenSpecy(yml$wavenumber,
                         spectra = as.data.table(yml$spectra),
                         metadata = as.data.table(yml$metadata),
                         coords = NULL)
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      jsn <- read_json(file, simplifyVector = T, ...)

      os <- as_OpenSpecy(jsn$wavenumber,
                         spectra = as.data.table(jsn$spectra),
                         metadata = as.data.table(jsn$metadata),
                         coords = NULL)
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      os <- readRDS(file, ...)
    } else {
      stop("unknown file type: specify a method to read custom formats or provide files of one of the supported file types .yml, .json, .rds",
           call. = F)
    }
  } else {
    io <- do.call(method, list(file, ...))

    os <- OpenSpecy(io, coords = NULL)
  }

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}

#' @rdname io_spec
#' 
#' @export
to_hyperSpec <- function(x) {
    new("hyperSpec", spc = as.matrix(transpose(x$spectra)), wavelength = x$wavenumber)
}
