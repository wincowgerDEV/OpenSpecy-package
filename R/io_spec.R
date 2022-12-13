#' @rdname io_spec
#'
#' @title Read and write spectral data
#'
#' @description
#' Functions for reading and writing spectral data from Open Specy .yaml, .json,
#' or .rds.
#'
#' @details
#' reference to qs
#'
#' @param x a list object of class \code{\link{OpenSpecy}}.
#' @param file file to be read from or written to.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param method submethod to be used for reading text files; defaults to
#' \code{\link[data.table]{fread}()} but \code{\link[utils]{read.csv}()} works
#' as well.
#' @param digits number of significant digits to use when formatting numeric
#' values; defaults to \code{\link[base]{getOption}("digits")}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' \code{read_spec()} returns a list object of class \code{\link{OpenSpecy}()}
#' containing spectral data.
#'
#' @examples
#' read_spec(read_extdata("raman_hdpe.yml"))
#' read_spec(read_extdata("raman_hdpe.json"))
#' read_spec(read_extdata("raman_hdpe.rds"))
#'
#' \dontrun{
#' write_spec(raman_hdpe, "raman_hdpe.yml")
#' write_spec(raman_hdpe, "raman_hdpe.json")
#' write_spec(raman_hdpe, "raman_hdpe.rds")
#' }
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{OpenSpecy}()}
#' \code{\link[base]{saveRDS}()}; \code{\link[base]{readRDS}()};
#' \code{\link[yaml]{write_yaml}()}; \code{\link[yaml]{read_yaml}()};
#' \code{\link[jsonlite]{write_json}()}; \code{\link[jsonlite]{read_json}()};
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
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname io_spec
#'
#' @importFrom yaml write_yaml
#' @importFrom jsonlite write_json
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
      stop("unknown file type: specify a method to read custom formats",
           call. = F)
    }
  } else {
    io <- do.call(method, list(file, ...))

    os <- OpenSpecy(io, coords = NULL)
  }

  if (!is.null(share)) share_spec(os, file = file, share = share)

  return(os)
}
