#' @rdname read_multi
#'
#' @title Read spectral data from multiple files
#'
#' @description
#' Wrapper functions for reading
#'
#' @details
#' x
#'
#' @param file file to be read from or written to.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return data frames containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' read_text(read_extdata("raman_hdpe.csv"))
#' read_asp(read_extdata("ftir_ldpe_soil.asp"))
#' read_opus(read_extdata("ftir_ps.0"))
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#'
#' @export
read_any <- function(file, ...) {
  if (grepl("(\\.csv$)|(\\.txt$)", ignore.case = T, file)) {
    os <- read_text(file = file, ...)
  } else if (grepl("\\.[0-999]$", ignore.case = T, file)) {
    os <- read_opus(file = file, ...)
  } else if (grepl("(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)",
                   ignore.case = T, file)) {
    ex <- strsplit(basename(file), split="\\.")[[1]][-1]
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
#' @importFrom hyperSpec read.ENVI.Nicolet
#' @importFrom data.table transpose
#' @export
read_zip <- function(file, ...) {
  flst <- unzip(zipfile = file, list = T)

  tmp <- file.path(tempdir(), "OpenSpecy-unzip")
  dir.create(tmp, showWarnings = F)

  unzip(file, exdir = tmp)

  if (nrow(flst) == 2 & any(grepl("\\.dat$", ignore.case = T, flst$Name)) &
      any(grepl("\\.hdr$", ignore.case = T, flst$Name))) {
    dat <- flst$Name[grepl("\\.dat$", ignore.case = T, flst$Name)]
    hdr <- flst$Name[grepl("\\.hdr$", ignore.case = T, flst$Name)]
    hs_envi <- read.ENVI.Nicolet(file = file.path(tmp, dat),
                                 headerfile = file.path(tmp, hdr), ...)

    os <- as_OpenSpecy(x = hs_envi@wavelength,
                       spectra = as.data.table(hs_envi@data$spc) |> transpose(),
                       metadata = data.table(file = basename(hs_envi@data$file)),
                       coords = data.table(x = hs_envi@data$x,
                                           y = hs_envi@data$y)
    )
  } else {
    lst <- lapply(file.path(tmp, flst$Name), read_any, ...)

    # TODO: list of spectra needs to be concatenated/combined into one OpenSpecy
    # object
    os <- lst
  }

  unlink(tmp, recursive = T)
  return(os)
}
