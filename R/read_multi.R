#' @rdname read_multi
#' @title Read spectral data from multiple files
#'
#' @description
#' Wrapper functions for reading files in batch.
#'
#' @details
#' \code{read_any()} provides a single function to quickly read in any of the
#' supported formats, it assumes that the file extension will tell it how to
#' process the spectra.
#' \code{read_zip()} provides functionality for reading in spectral map files
#' with ENVI file format or as individual files in a zip folder. If individual
#' files, spectra are concatenated.
#'
#' @param file file to be read from or written to.
#' @param range argument passed to \code{c_spec()} when reading multiple files from zip folder.
#' @param res spectral resolution for merge, argument passed to \code{c_spec()}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return \code{OpenSpecy} objects
#'
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' read_extdata("raman_hdpe.csv") |> read_any()
#' read_extdata("ftir_ldpe_soil.asp") |> read_any()
#' read_extdata("testdata_zipped.zip") |> read_zip()
#' read_extdata("CA_tiny_map.zip") |> read_zip()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_spec}()}
#'
#' @export
read_any <- function(file, ...) {
  if (grepl("(\\.csv$)|(\\.tsv$)|(\\.txt$)", ignore.case = T, file)) {
    os <- read_text(file = file, ...)
  } else if (grepl("\\.[0-999]$", ignore.case = T, file)) {
    os <- read_opus(file = file, ...)
  }
    else if (grepl("(\\.jdx$)|(\\.dx$)", ignore.case = T, file)) {
        os <- read_jdx(file = file, ...)
    }
    else if (grepl("(\\.asp$)|(\\.spa$)|(\\.spc$)",
                   ignore.case = T, file)) {
    ex <- gsub(".*\\.", "", file)
    os <- do.call(paste0("read_", tolower(ex)), list(file = file, ...))
  } else if (grepl("(\\.zip$)", ignore.case = T, file)) {
    os <- read_zip(file = file, ...)
  } else {
    os <- read_spec(file = file, ...)
  }

  return(os)
}

#' @rdname read_multi
#'
#' @importFrom utils unzip
#' @importFrom data.table transpose
#' @export
read_zip <- function(file, range = NULL, ...) {
  flst <- unzip(zipfile = file, list = T)
  
  flst <- flst[!grepl("_MACOSX", flst$Name), ]

  tmp <- file.path(tempdir(), "OpenSpecy-unzip")
  dir.create(tmp, showWarnings = F)

  unzip(file, exdir = tmp)

  if(nrow(flst) == 2 & any(grepl("\\.dat$", ignore.case = T, flst$Name)) &
      any(grepl("\\.hdr$", ignore.case = T, flst$Name))) {
    dat <- flst$Name[grepl("\\.dat$", ignore.case = T, flst$Name)]
    hdr <- flst$Name[grepl("\\.hdr$", ignore.case = T, flst$Name)]

    os <- read_envi(file.path(tmp, dat), file.path(tmp, hdr), ...)
  } else {
    lst <- lapply(file.path(tmp, flst$Name), read_any, ...)

    os <- c_spec(lst, range = range)
  }

  unlink(tmp, recursive = T)
  return(os)
}
