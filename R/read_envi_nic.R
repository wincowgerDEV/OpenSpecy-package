#' @describeIn read_envi
#'
#' @details
#' Nicolet uses some more keywords in their header file.
#' They are interpreted as follows:
#' \tabular{ll}{
#' description   \tab giving the position of the first spectrum \cr
#' z plot titles \tab wavelength and intensity axis units, comma separated \cr
#' pixel size    \tab interpreted as x and y step size
#'                    (specify `x = NA` and `y = NA`)
#' }
#' These parameters can be overwritten by giving a list with the respective
#' elements in parameter `header`.
#'
#' The values in header line description seem to be microns while the pixel
#' size seems to be in microns. If `nicolet.correction` is true, the
#' pixel size values (i.e. the step sizes) are multiplied by 1000.
#'
#' @param nicolet.correction see details
#'
#' @importFrom utils modifyList
#'
#' @export

read_envi_nic <- function(file = stop("read_envi: file name needed"),
                              headerfile = NULL, header = list(), ...,
                              x = NA, y = NA,
                              nicolet.correction = FALSE) {
  # the additional keywords to interpret must be read from headerfile
  headerfile <- .find_ENVI_header(file, headerfile)
  keys <- readLines(headerfile)
  keys <- .read_envi_split_header(keys)
  keys <- keys[c("description", "z plot titles", "pixel size")]

  header <- modifyList(keys, header)

  # most work is done by read_envi
  spc <- read_envi(
    file = file, headerfile = headerfile, header = header, ...,
    x = if (is.na(x)) 0:1 else x,
    y = if (is.na(y)) 0:1 else y
  )

  # From here on processing the additional keywords in Nicolet's ENVI header *

  # z plot titles -----------------------------------------------------------
  # default labels
  label <- list(
    x = expression(`/`(x, micro * m)),
    y = expression(`/`(y, micro * m)),
    spc = "I / a.u.",
    .wavelength = expression(tilde(nu) / cm^-1)
  )

  # get labels from header information
  if (!is.null(header$"z plot titles")) {
    pattern <- "^[[:blank:]]*([[:print:]^,]+)[[:blank:]]*,.*$"
    tmp <- sub(pattern, "\\1", header$"z plot titles")

    if (grepl("Wavenumbers (cm-1)", tmp, ignore.case = TRUE)) {
      label$.wavelength <- expression(tilde(nu) / cm^(-1))
    } else {
      label$.wavelength <- tmp
    }

    pattern <- "^[[:blank:]]*[[:print:]^,]+,[[:blank:]]*([[:print:]^,]+).*$"
    tmp <- sub(pattern, "\\1", header$"z plot titles")
    if (grepl("Unknown", tmp, ignore.case = TRUE)) {
      label$spc <- "I / a.u."
    } else {
      label$spc <- tmp
    }
  }

  # modify the labels accordingly
  spc@label <- modifyList(label, spc@label)

  # Set up spatial coordinates ----------------------------------------------
  # look for x and y in the header only if x and y are NULL
  # they are in `description` and `pixel size`

  # set up regular expressions to extract the values
  p.description <- paste(
    "^Spectrum position[[:digit:]]+ of[[:digit:]]+ positions,",
    "X = ([[:digit:].-]+), Y = ([[:digit:].-]+)$"
  )
  p.pixel.size <- "^[[:blank:]]*([[:digit:].-]+),[[:blank:]]*([[:digit:].-]+).*$"

  if (is.na(x) && is.na(y) &&
    !is.null(header$description) && grepl(p.description, header$description) &&
    !is.null(header$"pixel size") && grepl(p.pixel.size, header$"pixel size")) {
    x[1] <- as.numeric(sub(p.description, "\\1", header$description))
    y[1] <- as.numeric(sub(p.description, "\\2", header$description))

    x[2] <- as.numeric(sub(p.pixel.size, "\\1", header$"pixel size"))
    y[2] <- as.numeric(sub(p.pixel.size, "\\2", header$"pixel size"))

    # it seems that the step size is given in mm while the offset is in micron
    if (nicolet.correction) {
      x[2] <- x[2] * 1000
      y[2] <- y[2] * 1000
    }

    # now calculate and set the x and y coordinates
    x <- x[2] * spc$x + x[1]
    if (!any(is.na(x))) {
      spc@data$x <- x
    }

    y <- y[2] * spc$y + y[1]
    if (!any(is.na(y))) {
      spc@data$y <- y
    }
  }

  # consistent file import behavior across import functions
  # .spc_io_postprocess_optional() is called already by read_envi()

  spc
}
