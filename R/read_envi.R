#' @title Read ENVI data
#'
#' @description
#' This function allows ENVI data import.
#'
#' @details
#' ENVI data usually consists of two files, an ASCII header and a binary data
#' file. The header contains all information necessary for correctly reading
#' the binary file.
#'
#' @param file name of the binary file.
#' @param header name of the ASCII header file. If `NULL`, the name of the
#' header file is guessed by looking for a second file with the same basename as
#' `file` but with .hdr extension.
#' @param metadata a named list of the metadata; see \code{\link{read_text}()}
#' for details.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' An `OpenSpecy` object.
#'
#' @author Zacharias Steinmetz, Claudia Beleites
#'
#' @importFrom utils modifyList
#' @importFrom data.table as.data.table dcast
#' @importFrom caTools read.ENVI
#' @export
read_envi <- function(file, header = NULL,
                      metadata = list(
                        file_name = basename(file),
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
  if(is.null(header))
    header <- sub(pattern = "(.*)\\..*$", replacement = "\\1", file) |>
      paste0(".hdr")

  hdr <- .read_envi_header(header)
  arr <- read.ENVI(file, header)
  dt <- as.data.table(arr)
  names(dt) <- c("x", "y", "z", "value")
  dt[, 1:2] <- dt[, 1:2] -1

  os <- as_OpenSpecy(x = hdr$wavelength,
                     spectra = dcast(dt, z ~ x + y)[, -1],
                     metadata = data.table(file = basename(file)),
                     coords = dt[, 1:2] |> unique())

  return(os)
}

.read_envi_header <- function(headerfile, ...) {
  tr <- file.path(headerfile) |> file(...)
  hdr <- tr |> readLines()
  close(tr)

  if(!grepl("ENVI", hdr[1])) {
    stop("envi header not found", call. = F)
  } else {
    hdr <- hdr[-1]
  }

  hdr <- gsub("\\{([^}]*)\\}", "\\1", hdr)

  l <- grep("\\{", hdr)
  r <- grep("\\}", hdr)

  if (length(l) != length(r) || any(r <= l))
    stop("header data does not match", call. = F)

  hdr[l] <- sub("\\{", "", hdr[l])
  hdr[r] <- sub("\\}", "", hdr[r])

  for (i in rev(seq_along(l))) {
    hdr <- c(
      hdr[seq_len(l[i] - 1)],
      paste(hdr[l[i]:r[i]], collapse = " "),
      hdr[-seq_len(r[i])]
    )
  }

  hdr <- sapply(hdr, .split_line, "=", USE.NAMES = FALSE)
  names(hdr) <- tolower(names(hdr))

  tmp <- names(hdr) %in% c("samples", "lines", "bands", "data type",
                           "header offset")
  hdr[tmp] <- lapply(hdr[tmp], as.numeric)

  hdr$wavelength <- strsplit(hdr$wavelength, "[,;[:blank:]]+") |> unlist() |>
    as.numeric()

  return(hdr)
}

.split_line <- function(x, sep, trim.blank = TRUE) {
  tmp <- regexpr(sep, x)

  key <- substr(x, 1, tmp - 1)
  val <- substr(x, tmp + 1, nchar(x))

  if (trim.blank) {
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub(blank.pattern, "\\1", key)
    val <- sub(blank.pattern, "\\1", val)
  }

  val <- as.list(val)
  names(val) <- key

  return(val)
}
