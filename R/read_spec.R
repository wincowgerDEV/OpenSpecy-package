#' @rdname read_spec
#'
#' @title Read spectral data
#'
#' @description
#' Functions for reading spectral data types including .asp, .jdx,
#' .spc, .spa, .0, and .csv.
#'
#' @details
#' \code{read_spc()} and \code{read_jdx()} are just a wrapper around the
#' functions provided by the \link[hyperSpec:hyperSpec-package]{hyperSpec}
#' package.
#' Other functions have been adapted various online sources.
#' All functions convert datasets to a 2 column table with one column labeled
#' "wavenumber" and the other "intensity". There are many unique iterations of
#' spectral file formats so there may be bugs in the file conversion.
#' Please contact us if you identify any.
#'
#' @param file file to be read from.
#' @param cols character vector of \code{length = 2} indicating the colum names
#' for the wavenumber and intensity; if \code{NULL} columns are guessed.
#' @param method submethod to be used for reading text files; defaults to
#' \link[utils]{read.csv} but \link[data.table]{fread} works as well.
#' @param share defaults to \code{NULL}; needed to share spectra with the
#' Open Specy community; see \code{\link{share_spec}()} for details.
#' @param id a unique user and/or session ID; defaults to
#' \code{paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")}.
#' @param \ldots further arguments passed to the submethods.
#'
#' @return
#' All \code{read_*()} functions return data frames containing two columns
#' named \code{"wavenumber"} and \code{"intensity"}.
#'
#' @examples
#' read_text(read_extdata("raman_hdpe.csv"))
#' read_asp(read_extdata("ftir_ldpe_soil.asp"))
#' read_0(read_extdata("ftir_ps.0"))
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[hyperSpec]{read.jdx}()}; \code{\link[hyperSpec]{read.spc}()};
#' \code{\link[hexView]{readRaw}()}; \code{\link{share_spec}()}
#'
#' @importFrom dplyr %>%
#' @export
read_text <- function(file = ".", cols = NULL, method = "read.csv",
                      share = NULL, id = paste(digest(Sys.info()),
                                               digest(sessionInfo()),
                                               sep = "/"),
                      ...) {
  df <- do.call(method, list(file, ...)) %>%
    data.frame()

  if (all(grepl("^X[0-9]*", names(df)))) stop("missing header: ",
                                              "use 'header = FALSE' or an ",
                                              "alternative read method")

  # Try to guess column names
  if (is.null(cols)) {
    if (all(grepl("^V[0-9]*", names(df)))) {
      cols <- 1:2
      warning("missing header: guessing 'wavenumber' and 'intensity' data ",
              "from the first two columns; use 'cols' to supply user-defined ",
              "columns")
    } else {
      cols <- c(names(df)[grep("(wav*)", ignore.case = T, names(df))][1L],
                names(df)[grep("(transmit*)|(reflect*)|(abs*)|(intens*)",
                               ignore.case = T, names(df))][1L])
    }
  }
  if (any(is.na(cols))) stop("undefined columns selected; columns should be ",
                             "named 'wavenumber' and 'intensity'")
  if (cols[1] == cols[2]) stop("inconsistent input format")

  df <- df[cols]

  # Check if columns are numeric
  if (!all(sapply(df, is.numeric))) stop("input not numeric")

  names(df) <- c("wavenumber", "intensity")

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @export
read_asp <- function(file = ".", share = NULL, id = paste(digest(Sys.info()),
                                                          digest(sessionInfo()),
                                                          sep = "/"),
                     ...) {
  if (!grepl("\\.asp$", ignore.case = T, file))
    stop("file type should be 'asp'")

  tr <- file.path(file) %>% file(...)
  lns <- tr %>% readLines() %>% as.numeric()
  close(tr)

  y <- lns[-c(1:6)]
  x <- seq(lns[2], lns[3], length.out = lns[1])

  df <- data.frame(wavenumber = x, intensity = y)

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @importFrom utils read.table
#' @export
read_spa <- function(file = ".", share = NULL, id = paste(digest(Sys.info()),
                                                          digest(sessionInfo()),
                                                          sep = "/"),
                     ...) {
  if (!grepl("\\.spa$", ignore.case = T, file))
    stop("file type should be 'spa'")

  trb <- file.path(file) %>% file(open = "rb", ...)

  seek(trb, 576, origin = "start")
  spr <- readBin(trb, "numeric", n = 2, size = 4)

  if (!all(spr >= 0 & spr <= 15000 & spr[1] > spr[2]))
    stop("unknown spectral range")

  # Read the start offset
  seek(trb, 386, origin = "start")
  startOffset <- readBin(trb, "int", n = 1, size = 2)
  # Read the length
  seek(trb, 390, origin = "start")
  readLength <- readBin(trb, "int", n = 1, size = 2)

  # seek to the start
  seek(trb, startOffset, origin = "start")

  # we'll read four byte chunks
  floatCount <- readLength / 4

  # read all our floats
  floatData <- c(readBin(trb, "double", floatCount, size = 4))

  close(trb)

  df <- data.frame(wavenumber = seq(spr[1], spr[2], length = length(floatData)),
                   intensity = floatData)

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @importFrom hyperSpec read.jdx
#' @export
read_jdx <- function(file = ".", share = NULL, id = paste(digest(Sys.info()),
                                                          digest(sessionInfo()),
                                                          sep = "/"),
                     ...) {
  jdx <- read.jdx(file, ...)

  df <- data.frame(wavenumber = jdx@wavelength,
                   intensity = as.numeric(unname(jdx@data$spc[1,])))

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @importFrom hyperSpec read.spc
#' @export
read_spc <- function(file = ".", share = NULL, id = paste(digest(Sys.info()),
                                                          digest(sessionInfo()),
                                                          sep = "/"),
                     ...) {
  spc <- read.spc(file)

  df <- data.frame(wavenumber = spc@wavelength,
                   intensity = as.numeric(unname(spc@data$spc[1,])))

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @importFrom hexView readRaw blockString
#' @export
read_0 <- function(file = ".", share = NULL, id = paste(digest(Sys.info()),
                                                        digest(sessionInfo()),
                                                        sep = "/"),
                   ...) {
  if (!grepl("\\.[0-999]$", ignore.case = T, file))
    stop("file type should be '0'")

  pa <- readRaw(file, offset = 0, nbytes = file.info(file)$size, human = "char",
                size = 1, endian = "little")
  pr <- pa$fileRaw

  # Get positions where the following parameters are found in the file
  codes <- c("ZFF", "RES", "SNM", "DAT", "LWN", "FXV", "LXV", "NPT", "MXY",
             "MNY", "END", "TIM")

  z <- grepRaw(codes[1], pr, all = TRUE)[1] + 5
  re <- grepRaw(codes[2], pr, all = TRUE)[1] + 5
  snm <- grepRaw(codes[3], pr, all = TRUE)[1] + 7
  dat <- grepRaw(codes[4], pr, all = TRUE)[1] + 7
  lwn <- grepRaw(codes[5], pr, all = TRUE)[1] + 7
  fx <- grepRaw(codes[6], pr, all = TRUE)[3] + 7
  lx <- grepRaw(codes[7], pr, all = TRUE)[3] + 7
  npt0 <- grepRaw(codes[8], pr, all = TRUE)[2] + 3
  npt1 <- grepRaw(codes[8], pr, all = TRUE)[3] + 7
  mxy <- grepRaw(codes[9], pr, all = TRUE)[1] + 7
  mny <- grepRaw(codes[10], pr, all = TRUE)[3] + 7
  end <- grepRaw(codes[11], pr, all = TRUE) + 11
  tim <- grepRaw(codes[12], pr, all = TRUE) + 11

  ## Calculate end and start of each block
  offs <- sapply(5:10, function(x){end[x]})
  byts <- diff(offs)
  ZFF <- readRaw(file, offset = z, nbytes = 4, human = "int", size = 2)[[5]][1]
  RES <- readRaw(file, offset = re, nbytes = 4, human = "int", size = 2)[[5]][1]
  snm.lab.material <- blockString(readRaw(file, offset = snm, nbytes = 22,
                                          human = "char", size = 1,
                                          endian = "little"))

  ## Get number of data points for each spectra data block
  NPT0 <- readRaw(file, offset = npt0, nbytes = 12, human = "int", size = 4)[[5]][2]
  NPT1 <- readRaw(file, offset = npt1, nbytes = 4, human = "int", size = 4)[[5]][1]
  fxv <- readRaw(file, offset = fx, nbytes = 16, human = "real", size = 8)[[5]][1] ## fxv = frequency of first point
  lxv <- readRaw(file, offset = lx, nbytes = 16, human = "real", size = 8)[[5]][1] ## lxv = frequency of last point
  x <- rev(seq(lxv, fxv, (fxv - lxv) / (NPT1 - 1)))

  ## Read all through all the data blocks inside the OPUS file:
  nbytes1 <- NPT0 * 4 ## initial parameters
  smxa <- c()
  smna <- c()
  nbytes.f <- NPT1 * 4
  if(offs[1] < 2000) {
    offs.f <- offs[3]
  }

  if(offs[1] > 20000) {
    offs.f <- offs[2]
  }

  # Selected spectra block
  opus.p <- readRaw(file, width = NULL, offset = offs.f - 4,
                    nbytes = nbytes.f, human = "real", size = 4,
                    endian = "little")
  y <- opus.p[[5]]

  df <- data.frame(wavenumber = x, intensity = y)

  if (!is.null(share)) share_spec(df, file = file, share = share, id = id)

  return(df)
}

#' @rdname read_spec
#'
#' @export
read_extdata <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "OpenSpecy"))
  }
  else {
    system.file("extdata", file, package = "OpenSpecy", mustWork = TRUE)
  }
}
