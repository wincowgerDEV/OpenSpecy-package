#' @rdname io_spec
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading and writing spectral data to and from OpenSpecy format.
#' \code{OpenSpecy} objects are lists with components `wavenumber`, `spectra`,
#' and `metadata`. Currently supported formats are .y(a)ml, .json, .csv, or .rds.
#'
#' @param x an object of class \code{\link{OpenSpecy}}.
#' @param file file path to be read from or written to.
#' @param method optional; function to be used as a custom reader or writer.
#' Defaults to the appropriate function based on the file extension.
#' @param digits number of significant digits to use when formatting numeric
#' values; defaults to \code{\link[base]{getOption}("digits")}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @details
#' Due to floating point number errors there may be some differences in the
#' precision of the numbers returned if using multiple devices for .json and
#' .yaml files but the numbers should be nearly identical.
#' \code{\link[base]{readRDS}()} should return the exact same object every time.
#'
#' @return
#' \code{read_spec()} reads data formatted as an \code{OpenSpecy} object and
#' returns a list object of class \code{\link{OpenSpecy}} containing spectral
#' data.
#' \code{write_spec()} writes a file for an object of class
#' \code{\link{OpenSpecy}} containing spectral data.
#' \code{as_hyperspec()} converts an \code{OpenSpecy} object to a
#' \code{\link[hyperSpec]{hyperSpec-class}} object.
#'
#' @examples
#' read_extdata("raman_hdpe.yml") |> read_spec()
#' read_extdata("raman_hdpe.json") |> read_spec()
#' read_extdata("raman_hdpe.rds") |> read_spec()
#' read_extdata("raman_hdpe.csv") |> read_spec()
#'
#' \dontrun{
#' data(raman_hdpe)
#' write_spec(raman_hdpe, "raman_hdpe.yml")
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
#' \code{\link[yaml]{write_yaml}()}; \code{\link[yaml]{read_yaml}()};
#' \code{\link[jsonlite]{write_json}()}; \code{\link[jsonlite]{read_json}()};
#'
#' @importFrom yaml write_yaml read_yaml
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
  if (is.null(method)) {
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      write_yaml(x, file = file, precision = digits, ...)
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      write_json(x, path = file, dataframe = "columns", digits = digits, ...)
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      saveRDS(x, file = file, ...)
    } else if (grepl("\\.csv$", file, ignore.case = T)) {
      wave_names <- round(x$wavenumber, 0)
      spectra <- t(x$spectra)
      colnames(spectra) <- wave_names
      flat_specy <- cbind(spectra, x$metadata)
      fwrite(flat_specy, file = file)
    }
    else {
      stop("unknown file type: specify a method to write custom formats or ",
           "provide one of the supported .yml, .json, or .rds formats as ",
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
    if (grepl("(\\.yaml$)|(\\.yml$)", file, ignore.case = T)) {
      yml <- read_yaml(file = file, ...)

      os <- as_OpenSpecy(yml$wavenumber,
                         spectra = as.data.table(yml$spectra),
                         metadata = data.table(as.data.table(yml$metadata),
                                               file_name = basename(file)),
                         coords = NULL)
    } else if (grepl("\\.json$", file, ignore.case = T)) {
      jsn <- read_json(file, simplifyVector = T, ...)

      os <- as_OpenSpecy(jsn$wavenumber,
                         spectra = as.data.table(jsn$spectra),
                         metadata = data.table(as.data.table(jsn$metadata),
                                               file_name = basename(file)),
                         coords = NULL)
    } else if (grepl("\\.rds$", file, ignore.case = T)) {
      os <- readRDS(file, ...)
      os$metadata$file_name <- basename(file)
    }
      else if (grepl("\\.csv$", file, ignore.case = T)) {
          os <- read_text(file, ...)
          os$metadata$file_name <- basename(file)
    } else {
      stop("unknown file type: specify a method to read custom formats or ",
           "provide files of one of the supported file types .yml, .json, .rds",
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
  new("hyperSpec", spc = as.matrix(transpose(x$spectra)),
      wavelength = x$wavenumber)
}
