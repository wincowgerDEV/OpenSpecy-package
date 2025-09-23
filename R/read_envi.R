#' @title Read ENVI data
#'
#' @description
#' This function allows ENVI data import.
#'
#' @param file name of the binary file.
#' @param header name of the ASCII header file. If `NULL`, the name of the
#' header file is guessed by looking for a second file with the same basename as
#' `file` but with .hdr extension.
#' @param spectral_smooth logical value determines whether spectral smoothing
#' will be performed.
#' @param sigma if \code{spectral_smooth} then this option applies the 3d
#' standard deviations for the \code{gaussianSmooth} function from the
#' \code{mmand} package to describe how spectral smoothing occurs on each dimension.
#' The first two dimensions are x and y, the third is the wavenumbers.
#' @param metadata a named list of the metadata; see
#' \code{\link{as_OpenSpecy}()} for details.
#' @param \ldots further arguments passed to the submethods.
#'
#' @details
#' ENVI data usually consists of two files, an ASCII header and a binary data
#' file. The header contains all information necessary for correctly reading
#' the binary file via \code{\link[caTools]{read.ENVI}()}.
#'
#' @return
#' An `OpenSpecy` object.
#'
#' @author Zacharias Steinmetz, Claudia Beleites
#'
#' @seealso
#' \code{\link{read_spec}()} for reading .y(a)ml, .json, or .rds (OpenSpecy)
#' files;
#' \code{\link{read_text}()}, \code{\link{read_asp}()}, \code{\link{read_spa}()},
#' \code{\link{read_spc}()}, and \code{\link{read_jdx}()} for text files, .asp,
#' .spa, .spa, .spc, and .jdx formats, respectively;
#' \code{\link{read_opus}()} for reading .0 (OPUS) files;
#' \code{\link{read_zip}()} and \code{\link{read_any}()} for wrapper functions;
#' \code{\link[caTools]{read.ENVI}()}
#' \code{\link[mmand]{gaussianSmooth}()}
#'
#' @importFrom utils modifyList
#' @importFrom caTools read.ENVI
#' @importFrom mmand gaussianSmooth
#' @export
read_envi <- function(file, header = NULL, spectral_smooth = F, sigma = c(1,1,1),
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

  if(spectral_smooth)
    arr <- gaussianSmooth(arr, sigma)

  md <- hdr[names(hdr) != "wavelength"]

  dims <- dim(arr)
  coords <- data.frame(
    y = as.numeric(rep(seq_len(dims[1]) - 1, each = dims[2])),
    x = as.numeric(rep(seq_len(dims[2]) - 1, times = dims[1]))
  )
  spectra <- data.table::as.data.table(
    matrix(aperm(arr, c(3, 2, 1)), nrow = dims[3])
  )
  data.table::setnames(spectra, paste(coords$y, coords$x, sep = "_"))

  if("wavelength" %in% names(hdr)) {
      wavenumbers <- hdr$wavelength
  } else if(grepl("\\.img$", file) & file.exists(gsub("\\.img$", ".parms", file))) {
      metadata <- readLines(gsub("\\.img$", ".parms", file))
      names <- gsub("=.*", "", metadata)
      vals <- gsub(".*=", "", metadata)
      df_metadata <- as.data.frame(t(vals))
      colnames(df_metadata) <- names
      wavenumbers <- seq(to = as.numeric(df_metadata[["LXV"]]),
                         from = as.numeric(df_metadata[["FXV"]]),
                         length.out = as.numeric(df_metadata[["NPT"]]))
  } else {
      wavenumbers <- NULL
  }

  if(is.null(wavenumbers))
    warning("wavenumbers not found, using index values instead")

  os <- as_OpenSpecy(x = if(!is.null(wavenumbers)) wavenumbers else 1:dims[3],
                     spectra = spectra,
                     metadata = c(metadata, md),
                     coords = coords,
                     session_id = T)

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

  if("wavelength" %in% names(hdr)){
      hdr$wavelength <- strsplit(hdr$wavelength, "[,;[:blank:]]+") |> unlist() |>
          as.numeric()        
  }

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
