############################################################################ ~
###
###  read_envi - read ENVI files, missing header files may be replaced by
###              list in parameter header.
###
###  * read_envi_nic for ENVI files written by Nicolet spectrometers
###  * adapted from caTools read_envi
###
###  Time-stamp: <Claudia Beleites on Saturday, 2011-02-05 at 19:19:00 on cb>
###
############################################################################ ~


# Helper functions (General) -------------------------------------------------

# split.line - split line into list of key-value pairs
split.line <- function(x, separator, trim.blank = TRUE) {
  tmp <- regexpr(separator, x)

  key <- substr(x, 1, tmp - 1)
  value <- substr(x, tmp + 1, nchar(x))

  if (trim.blank) {
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub(blank.pattern, "\\1", key)
    value <- sub(blank.pattern, "\\1", value)
  }

  value <- as.list(value)
  names(value) <- key

  value
}


# Helper functions (ENVI-specific) -------------------------------------------

# guesses ENVI header file name
.find_envi_header <- function(file, headerfilename) {
  if (is.null(headerfilename)) {
    headerfilename <- paste(dirname(file),
      sub("[.][^.]+$", ".*", basename(file)),
      sep = "/"
    )

    tmp <- Sys.glob(headerfilename)

    headerfilename <- tmp[!grepl(file, tmp)]

    if (length(headerfilename) > 1L) {
      headerfilename <- headerfilename[grepl("[.][hH][dD][rR]$", headerfilename)]

      if (length(headerfilename == 1L)) {
        message(".find_envi_header: Guessing header file name ", headerfilename)
      }
    }

    if (length(headerfilename) != 1L) {
      stop("cannot guess header file name", call. = F)
    }
  }

  if (!file.exists(headerfilename)) {
    stop("ENVI header file: ", headerfilename, " not found.", call. = F)
  }

  headerfilename
}

# .............................................................................

.read_envi_split_header <- function(header, pull.lines = TRUE) {
  # check ENVI at beginning of file
  if (!grepl("ENVI", header[1])) {
    stop("not an ENVI header (ENVI keyword missing)", call. = F)
  } else {
    header <- header[-1]
  }

  # remove curly braces and put multi-line key-value-pairs into one line
  header <- gsub("\\{([^}]*)\\}", "\\1", header)

  l <- grep("\\{", header)
  r <- grep("\\}", header)

  if (length(l) != length(r)) {
    stop("error matching curly braces in header (differing numbers).", call. = F)
  }

  if (any(r <= l)) {
    stop("mismatch of curly braces in header.", call. = F)
  }

  header[l] <- sub("\\{", "", header[l])
  header[r] <- sub("\\}", "", header[r])

  if (pull.lines) {
    for (i in rev(seq_along(l))) {
      header <- c(
        header[seq_len(l[i] - 1)],
        paste(header[l[i]:r[i]], collapse = " "),
        header[-seq_len(r[i])]
      )
    }
  }

  # split key = value constructs into list with keys as names
  header <- sapply(header, split.line, "=", USE.NAMES = FALSE)
  names(header) <- tolower(names(header))

  # process numeric values
  tmp <- names(header) %in% c("samples", "lines", "bands", "data type", "header offset")
  header[tmp] <- lapply(header[tmp], as.numeric)

  header
}

# .............................................................................

.read_envi_bin <- function(file,
                           header,
                           block.lines.skip = NULL,
                           block.lines.size = NULL) {
  DATA_TYPE_SIZES <- as.integer(c(1, 2, 4, 4, 8, NA, NA, NA, 16, NA, NA, 2))

  if (is.null(header$interleave)) {
    header$interleave <- "bsq"
  }

  if (any(is.null(header[c("samples", "lines", "bands", "data type")])) ||
    any(    is.na(header[c("samples", "lines", "bands", "data type")]))) {
    stop(
      "ENVI header (required entry missing or incorrect)\n header: ",
      paste(names(header), " = ", header, collapse = ", ")
    )
  }

  if (header$samples <= 0) {
    stop("ENVI header: incorrect data size (", header$samples, ")", call. = F)
  }
  if (header$lines <= 0) {
    stop("ENVI header: incorrect data size (", header$lines, ")", call. = F)
  }
  if (header$bands <= 0) {
    stop("ENVI header: incorrect data size (", header$bands, ")", call. = F)
  }

  if (!(header$`data type` %in% c(1:5, 9, 12))) {
    stop(
      "ENVI header: data type incorrect or unsupported (",
      header$`data type`,
      ")"
    )
  }

  if (is.null(header$`byte order`)) {
    header$`byte order` <- .Platform$endian
    message(".read_envi_bin: 'byte order' not given => Guessing '",
      .Platform$endian, "'\n",
      sep = ""
    )
  }

  if (!header$`byte order` %in% c("big", "little", "swap")) {
    header$`byte order` <- as.numeric(header$`byte order`)
    if (!header$`byte order` %in% 0:1) {
      header$`byte order` <- .Platform$endian
      warning("Byte order incorrect. Guessing '", .Platform$endian, "'", call. = F)
    } else if (header$`byte order` == 0) {
      header$`byte order` <- "little"
    } else {
      header$`byte order` <- "big"
    }
  }

  if (!file.exists(file)) {
    stop("binary file not found: ", file)
  }

  f <- file(file, "rb")
  if (!is.null(header$`header offset`)) {
    seek(f, where = header$`header offset`, origin = "start")
  }

  # size of data point in bytes
  size <- DATA_TYPE_SIZES[header$`data type`]

  # read blocks of data
  if (block.lines.skip > 0) {
    skip <- switch(tolower(header$interleave),
      bil = header$samples * header$bands * block.lines.skip,
      bip = header$bands * header$samples * block.lines.skip,
      bsq = stop(
        "skipping of band sequential (BSQ) ENVI files not yet supported."
      ),
      stop(
        "Unknown interleave (", header$interleave, ") - should be one of ",
        "'BSQ', 'BIL', 'BIP'."
      )
    )

    skip <- skip * size
    seek(f, where = skip, start = "current")
  }

  if (!is.null(block.lines.size)) {
    header$lines <- min(block.lines.size, header$lines - block.lines.skip)
  }

  # number of data points to read
  n <- header$samples * header$lines * header$bands

  switch(header$`data type`,
    spc <- readBin(f, integer(), n = n, size = size, signed = FALSE),
    spc <- readBin(f, integer(), n = n, size = size, endian = header$`byte order`),
    spc <- readBin(f, integer(), n = n, size = size, endian = header$`byte order`),
    spc <- readBin(f, double(), n = n, size = size, endian = header$`byte order`),
    spc <- readBin(f, double(), n = n, size = size, endian = header$`byte order`),
    stop("ENVI data type (", header$`data type`, ") unknown", call. = F), # 6 unused
    stop("ENVI data type (", header$`data type`, ") unknown", call. = F), # 7 unused
    stop("ENVI data type (", header$`data type`, ") unknown", call. = F), # 8 unused
    spc <- readBin(f, complex(), n = n, size = size, endian = header$`byte order`),
    stop("ENVI data type (", header$`data type`, ") unknown", call. = F), # 10 unused
    stop("ENVI data type (", header$`data type`, ") unknown", call. = F), # 11 unused
    spc <- readBin(f, integer(),
      n = n, size = size, endian = header$`byte order`,
      signed = FALSE
    )
  )

  close(f)

  switch(tolower(header$interleave),
    bil = {
      dim(spc) <- c(header$samples, header$bands, header$lines)
      spc <- aperm(spc, c(3, 1, 2))
    },
    bip = {
      dim(spc) <- c(header$bands, header$samples, header$lines)
      spc <- aperm(spc, c(3, 2, 1))
    },
    bsq = {
      dim(spc) <- c(header$samples, header$lines, header$bands)
      spc <- aperm(spc, c(2, 1, 3))
    },
    stop(
      "Unknown interleave (",
      header$interleave,
      ", should be one of 'BSQ', 'BIL', 'BIP')"
    )
  )

  dim(spc) <- c(header$samples * header$lines, header$bands)

  spc
}


# Function -------------------------------------------------------------------

#' Import of ENVI data
#'
#' @aliases read_envi read_envi_nic read_envi_HySpex
#'
#' @description
#' This function allows ENVI data import as `hyperSpec` object.
#'
#' `read_envi_nic()` should be a good starting point for writing custom
#' wrappers for `read_envi()` that take into account your manufacturer's
#' special entries in the header file.
#'
#' @details
#' ENVI data usually consists of two files, an ASCII header and a binary data
#' file. The header contains all information necessary for correctly reading
#' the binary file.
#'
#' I experienced missing header files (or rather: header files without any
#' contents) produced by Bruker Opus' ENVI export.
#'
#' In this case the necessary information can be given as a list in parameter
#' `header` instead:
#'
#' \tabular{lll}{
#' `header$`          \tab values        \tab meaning\cr
#' `samples`          \tab integer       \tab no of columns / spectra in x direction\cr
#' `lines`            \tab integer       \tab no of lines / spectra in y direction\cr
#' `bands`            \tab integer       \tab no of wavelengths / data points per spectrum\cr
#' ``data type``      \tab               \tab format of the binary file\cr
#'                    \tab 1             \tab 1 byte unsigned integer \cr
#'                    \tab 2             \tab 2 byte signed integer \cr
#'                    \tab 3             \tab 4 byte signed integer \cr
#'                    \tab 4             \tab 4 byte float \cr
#'                    \tab 5             \tab 8 byte double \cr
#'                    \tab 9             \tab 16 (2 x 8) byte complex double \cr
#'                    \tab 12            \tab 2 byte unsigned integer \cr
#'  ``header offset`` \tab integer       \tab number of bytes to skip before binary data starts\cr
#'  `interleave`      \tab               \tab directions of the data cube \cr
#'                    \tab "BSQ"         \tab band sequential (indexing: \[sample, line, band\])\cr
#'                    \tab "BIL"         \tab band interleave by line (indexing: \[sample, line, band\])\cr
#'                    \tab "BIP"         \tab band interleave by pixel (indexing: \[band, line, sample\])\cr
#'  ``byte order``    \tab 0 or "little" \tab little endian \cr
#'                    \tab 1 or "big"    \tab big endian \cr
#'                    \tab "swap"        \tab swap byte order
#' }
#'
#' Some more information that is not provided by the ENVI files may be given:
#'
#' Wavelength axis and axis labels in the respective parameters. For more
#' information, see [hyperSpec::initialize()].
#'
#' The spatial information is by default a sequence from 0 to
#' `header$samples - 1` and `header$lines - 1`, respectively.
#' `x` and `y` give offset of the first spectrum and step size.
#'
#' Thus, the object's `$x` column is: `(0 : header$samples - 1) * x
#' [2] + x [1]`.  The `$y` column is calculated analogously.
#'
#'
#' @param file Complete name of the binary file.
#'
#' @param headerfile name of the ASCII header file. If `NULL`, the name
#'        of the header file is guessed by looking for a second file with
#'        the same basename as `file` but `hdr` or `HDR` suffix.
#' @param header list with header information, see details. Overwrites
#'        information extracted from the header file.
#' @param x,y vectors of form c(offset, step size) for the position vectors,
#'        see details.
#' @param wavelength,label lists that overwrite the respective information
#'        from the ENVI header file. These data is then handed to
#'        [hyperSpec::initialize()]
#' @param block.lines.skip,block.lines.size BIL and BIP ENVI files may be read
#'        in blocks of lines: skip the first `block.lines.skip` lines, then
#'        read a block of `block.lines.size` lines. If `block.lines.NULL`, the
#'        whole file is read.
#'        Blocks are silently truncated at the end of the file (more precisely:
#'        to `header$lines`).
#' @param keys.hdr2data determines which fields of the header file should be
#'        put into the extra data. Defaults to none.
#'
#' To specify certain entries, give character vectors containing the lowercase
#'   names of the header file entries.
#'
#' @param ... currently unused by `read_envi()`, `read_envi_nic()` hands
#'        those arguments over to `read_envi()`.
#'
#' @param pull.header.lines (internal) flag whether multi-line header entries
#'        grouped by curly braces should be pulled into one line each.
#'
#' @return  `hyperSpec` object.
#'
#' @author C. Beleites, testing for the Nicolet files C. Dicko
#'
#' @seealso [caTools::read.ENVI()]
#'
#' @references This function was adapted from [caTools::read.ENVI()]:
#'
#' Jarek Tuszynski (2008). caTools: Tools: moving window statistics, GIF,
#'   Base64, ROC AUC, etc.. R package version 1.9.
#'
#' @importFrom utils modifyList
#' @importFrom methods new
#' @importFrom hyperSpec hy_get_option .spc_io_postprocess_optional
#'
#' @export
read_envi <- function(file = stop("read_envi: file name needed"),
                      headerfile = NULL,
                      header = list(),
                      keys.hdr2data = FALSE,
                      x = 0:1,
                      y = x,
                      wavelength = NULL,
                      label = list(),
                      block.lines.skip = 0,
                      block.lines.size = NULL, ...,
                      pull.header.lines = TRUE) {
  force(y)

  if (!file.exists(file)) {
    stop("file not found:", file)
  }

  if (!is.list(header)) { # catch a common pitfall
    if (is.character(header)) {
      stop(
        "header must be a list of parameters; ",
        "did you mean headerfile instead?"
      )
    } else {
      stop("header must be a list of parameters.", call. = F)
    }
  }

  if (is.null(headerfile)) {
    headerfile <- .find_envi_header(file, headerfile)
  }

  tmp <- readLines(headerfile)
  tmp <- .read_envi_split_header(tmp, pull.lines = pull.header.lines)
  header <- modifyList(tmp, header)

  ## read the binary file
  spc <- .read_envi_bin(file, header,
    block.lines.skip = block.lines.skip,
    block.lines.size = block.lines.size
  )

  ## wavelength should contain the mean wavelength of the respective band
  if (!is.null(header$wavelength)) {
    header$wavelength <-
      as.numeric(unlist(strsplit(header$wavelength, "[,;[:blank:]]+")))

    if (!any(is.na(header$wavelength)) && is.null(wavelength)) {
      wavelength <- header$wavelength
    }
  }

  ## set up spatial coordinates
  x <- seq(0, header$samples - 1) * x[2] + x[1]
  y <- seq(0, header$lines - 1) * y[2] + y[1]

  block.lines.size <- min(block.lines.size, nrow(spc) / header$samples)
  x <- rep(x, each = block.lines.size)

  y <- y[block.lines.skip + seq_len(block.lines.size)]
  y <- rep(y, header$samples)

  ## header lines => extra data columns
  extra.data <- header[keys.hdr2data]

  if (hy_get_option("gc")) gc()

  if (length(extra.data) > 0) {
    extra.data <- lapply(extra.data, rep, length.out = length(x))
    data <- data.frame(x = x, y = y, extra.data)
  } else {
    data <- data.frame(x = x, y = y)
  }

  if (hy_get_option("gc")) gc()

  # finally put together the hyperSpec object
  spc <- new("hyperSpec", data = data, spc = spc, wavelength = wavelength, labels = label)

  # consistent file import behavior across import functions
  .spc_io_postprocess_optional(spc, file)
}
