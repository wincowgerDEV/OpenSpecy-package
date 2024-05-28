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
#' \code{read_many()} provides functionality for reading multiple files
#' in a character vector and will return a list.
#'
#' @param file file to be read from or written to.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return \code{OpenSpecy} objects if a single
#' spectrum or map is provided, otherwise the provide a list of \code{OpenSpecy} objects.
#'
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' read_extdata("raman_hdpe.csv") |> read_any()
#' read_extdata("ftir_ldpe_soil.asp") |> read_any()
#' read_extdata("testdata_zipped.zip") |> read_many()
#' read_extdata("CA_tiny_map.zip") |> read_many()
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_spec}()} for submethods.
#' \code{\link{c_spec}()} for combining lists of Open Specys.
#'
#' @importFrom utils unzip
#' @importFrom data.table transpose
#'
#' @export
read_any <- function(file, ...) {
  if(length(file) == 2 & any(grepl("(\\.dat$)", ignore.case = T, file)) & any(grepl("(\\.hdr$)", ignore.case = T, file))){
    os <- read_envi(file = file[grepl("(\\.dat$)", ignore.case = T, file)], header = file[grepl("(\\.hdr$)", ignore.case = T, file)], ...)
  }
  else if(length(file) > 1){
    os <- read_many(file = file, ...)
  }
  else if (grepl("(\\.zip$)", ignore.case = T, file)) {
    os <- read_zip(file = file, ...)
  }
  else if (grepl("(\\.csv$)|(\\.tsv$)|(\\.txt$)", ignore.case = T, file)) {
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
  }  else {
    os <- read_spec(file = file, ...)
  }

  return(os)
}

#' @rdname read_multi
#'
#' @export
read_many <- function(file, ...) {
  lapply(file, read_any, ...)
}

#' @rdname read_multi
#' @export
read_zip <- function(file, ...) {
  flst <- unzip(zipfile = file, list = T)$Name
  flst <- flst[!grepl("_MACOSX", flst)]

  tmp <- file.path(tempdir(), "OpenSpecy-unzip")
  dir.create(tmp, showWarnings = F)

  unzip(file, exdir = tmp)

  flst <- file.path(tmp, flst)

  if(length(flst) == 2 & any(grepl("\\.dat$", ignore.case = T, flst)) &
     any(grepl("\\.hdr$", ignore.case = T, flst))) {
    dat <- flst[grepl("\\.dat$", ignore.case = T, flst)]
    hdr <- flst[grepl("\\.hdr$", ignore.case = T, flst)]

    os <- read_envi(dat, hdr, ...)
  } else {
    os <- read_many(flst,  ...)
  }

  unlink(tmp, recursive = T)
  return(os)
}
