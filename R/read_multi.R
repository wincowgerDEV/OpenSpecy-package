.supported_spectrum_extensions <- function() {
  c(
    "json", "rds", "csv", "tsv", "txt", "xyz", "jdx", "dx",
    "asp", "spa", "spc", "zip", "h5", "hdf5", "dat", "img", "hdr",
    "[0-9]+"
  )
}

#' @rdname read_multi
#' @title Read spectral data from multiple files
#'
#' @description
#' Wrapper functions for reading files in batch.
#'
#' @details
#' \code{read_any()} provides a single function to quickly read in any of the
#' supported formats, it assumes that the file extension will tell it how to
#' process the spectra. OPUS extensions are a period followed only by one or
#' more digits, including multi-digit extensions such as `.10`.
#' \code{read_zip()} provides functionality for reading in spectral map files
#' with ENVI file format or as individual files in a zip folder. If individual
#' files, spectra are concatenated.
#' \code{read_many()} provides functionality for reading multiple files
#' in a character vector and will return a list.
#'
#' @param file file to be read from or written to.
#' @param c_spec logical, if multiple spectra should be concatenated or not. 
#' Multiple spectra will return a list if this is false. 
#' @param c_spec_args list of arguments passed to \code{c_spec()}
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return \code{OpenSpecy} objects if a single
#' spectrum or map is provided, otherwise they provide a list of
#' \code{OpenSpecy} objects. Map readers can return \code{Specs} when that
#' representation is explicitly forwarded.
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
read_any <- function(file, c_spec = T, 
                     c_spec_args = list(range = NULL, res = NULL), ...) {
  if(length(file) == 2 & any(grepl("(\\.dat$)|(\\.img$)", ignore.case = T, file)) & any(grepl("(\\.hdr$)", ignore.case = T, file))){
    os <- read_envi(file = file[grepl("(\\.dat$)|(\\.img$)", ignore.case = T, file)], header = file[grepl("(\\.hdr$)", ignore.case = T, file)], ...)
  }
  else if(length(file) > 1){
    os <- read_many(file = file, ...)
    if(c_spec & !is_OpenSpecy(os) & !is_Specs(os) & is.list(os)){
        os <- do.call("c_spec", c(list(os), c_spec_args))
    }
  }
  else if(any(grepl("(\\.dat$)|(\\.img$)", ignore.case = T, file))){
    os <- read_envi(file = file[grepl("(\\.dat$)|(\\.img$)", ignore.case = T, file)], ...)
  }
  else if (grepl("(\\.zip$)", ignore.case = T, file)) {
    os <- read_zip(file = file, ...)
    if(c_spec & !is_OpenSpecy(os) & !is_Specs(os) & is.list(os)){
        os <- do.call("c_spec", c(list(os), c_spec_args))
    }
  }
    else if (grepl("(\\.h5$)|(\\.hdf5$)", ignore.case = T, file)) {
        os <- read_h5(file = file, ...)
    }
  else if (grepl("(\\.xyz$)|(\\.csv$)|(\\.tsv$)|(\\.txt$)", ignore.case = T, file)) {
    os <- read_text(file = file, ...)
  } else if (grepl("\\.[0-9]+$", ignore.case = T, file)) {
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
  archive_members <- unzip(zipfile = file, list = T)
  archive_members <- archive_members[
    !grepl("_MACOSX", archive_members$Name), , drop = FALSE
  ]
  flst <- archive_members$Name

  args <- list(...)
  envi_pair <- length(flst) == 2L &&
    any(grepl("\\.dat$", ignore.case = TRUE, flst)) &&
    any(grepl("\\.hdr$", ignore.case = TRUE, flst))

  # The webR heap is shared by R and its in-memory filesystem. Extracting a
  # large ENVI DAT into MEMFS before allocating the final double matrix can
  # therefore consume both copies at once. The default blockwise ENVI reader
  # is sequential, so stream the compressed member directly into that reader.
  # A compact background read performs its own halo-aware smoothing and can
  # therefore keep streaming even when spectral_smooth=TRUE. Exact smoothed
  # reads still need a seekable filename until their no-filter tile path is
  # implemented.
  compact_background_stream <-
    identical(args[["representation"]], "Specs") &&
    !is.null(args[["background_filter"]])
  if (envi_pair && (!isTRUE(args[["spectral_smooth"]]) ||
                    compact_background_stream)) {
    dat_name <- flst[grepl("\\.dat$", ignore.case = TRUE, flst)][[1L]]
    hdr_name <- flst[grepl("\\.hdr$", ignore.case = TRUE, flst)][[1L]]
    header_size <- archive_members$Length[
      match(hdr_name, archive_members$Name)
    ]
    header_connection <- unz(file, hdr_name, open = "rb")
    header_raw <- tryCatch(
      readBin(header_connection, what = "raw", n = header_size),
      finally = close(header_connection)
    )
    if (length(header_raw) != header_size) {
      stop("ENVI header ended before its declared ZIP member size",
           call. = FALSE)
    }

    header_file <- tempfile("OpenSpecy-envi-", fileext = ".hdr")
    on.exit(unlink(header_file, force = TRUE), add = TRUE)
    header_output <- file(header_file, open = "wb")
    tryCatch(
      writeBin(header_raw, header_output),
      finally = close(header_output)
    )

    data_connection <- unz(file, dat_name, open = "rb")
    on.exit(close(data_connection), add = TRUE)
    args[c("file", "header")] <- NULL
    if (is.null(args[["metadata"]])) {
      args[["metadata"]] <- list(
        file_name = basename(dat_name),
        license = "CC BY-NC"
      )
    }
    return(do.call(
      read_envi,
      c(list(file = data_connection, header = header_file), args)
    ))
  }

  tmp <- tempfile("OpenSpecy-unzip-")
  dir.create(tmp, showWarnings = F)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  unzip(file, exdir = tmp)

  flst <- file.path(tmp, flst)

  if(length(flst) == 2 & any(grepl("\\.dat$", ignore.case = T, flst)) &
     any(grepl("\\.hdr$", ignore.case = T, flst))) {
    dat <- flst[grepl("\\.dat$", ignore.case = T, flst)]
    hdr <- flst[grepl("\\.hdr$", ignore.case = T, flst)]

    os <- read_envi(dat, hdr, ...)
  } else {
    ordinary_args <- args
    ordinary_args[c(
      "representation", "background_filter", "spectral_smooth", "sigma"
    )] <- NULL
    os <- do.call(read_many, c(list(file = flst), ordinary_args))
  }

  return(os)
}
