#' @rdname read_spectra
#'
#' @title Read spectral data
#'
#' @description
#' Functions for reading in spectral data types including asp, jdx, spc, spa, 0, and csv.
#'
#' @details
#' \code{read_spc()} and \code{read_jdx()} are just a wrapper around the
#' functions provided by the \link[hyperSpec:hyperSpec-package]{hyperSpec} package.
#' Other functions have been adapted various online sources.
#' All functions convert datasets to a 2 column table with one column labeled
#' "wavenumber" and the other "intensity". There are many unique iterations of
#' spectral file formats so there may be bugs in the file conversion. Please contact us if you identify any.
#'
#' @param file file you to from.
#' @param \ldots further arguments passed to the submethods.
#'
#' @seealso
#' \code{\link[hyperSpec]{read.jdx}()}; \code{\link[hyperSpec]{read.spc}()};
#' \code{\link[hexView]{readRaw}()}
#'
#' @examples
#' ftir_ldpe_soil <- read_asp(read_extdata("ftir_ldpe_soil.asp"))
#'
#' ftir_ps <- read_0(read_extdata("ftir_ps.0"))
#'
#' @importFrom magrittr %>%
#' @export
read_asp <- function(file = ".", ...) {
  if (!grepl("\\.asp$", ignore.case = T, file)) stop("file type should be 'asp'")

  tr <- file.path(file) %>% file(...)
  lns <- tr %>% readLines() %>% as.numeric()
  close(tr)

  y <- lns[-c(1:6)]
  x <- seq(lns[2], lns[3], length.out = lns[1])

  data.frame(wavenumber = x, intensity = y)
}

#' @rdname read_spectra
#'
#' @importFrom utils read.table
#' @export
read_spa <- function(file = ".", ...) {
  if (!grepl("\\.spa$", ignore.case = T, file)) stop("file type should be 'spa'")

  trr <- file.path(file) %>% file(...)
  lns <- trr %>% readLines(n = 10, warn = FALSE)
  close(trr)

  mgn <- lns[grepl("Resolution", lns, useBytes = T)]

  start <- strsplit(mgn, " ")[[1]][5] %>% as.numeric()
  end <- strsplit(mgn, " ")[[1]][7] %>% as.numeric()

  trb <- file.path(file) %>% file(open = "rb", ...)

  # Read the start offset
  seek(trb, 386, origin = "start")
  startOffset <- readBin(trb, "int", n = 1, size = 2)
  # Read the length
  seek(trb, 390, origin = "start")
  readLength <- readBin(trb, "int", n = 1, size = 2)

  # seek to the start
  seek(trb, startOffset, origin = "start")

  # we'll read four byte chunks
  floatCount <- readLength/4

  # read all our floats
  floatData <- c(readBin(trb, "double", floatCount, size = 4))

  close(trb)

  data.frame(wavenumber = seq(end, start, length = length(floatData)),
             intensity = floatData)
}

#' @rdname read_spectra
#'
#' @importFrom hyperSpec read.jdx
#' @export
read_jdx <- function(file = ".", ...) {
  jdx <- read.jdx(file, ...)

  data.frame(wavenumber = jdx@wavelength,
             intensity = as.numeric(unname(jdx@data$spc[1,])))
}

#' @rdname read_spectra
#'
#' @importFrom hyperSpec read.spc
#' @export
read_spc <- function(file = ".", ...) {
  spc <- read.spc(file)

  data.frame(wavenumber = spc@wavelength,
             intensity = as.numeric(unname(spc@data$spc[1,])))
}

#' @rdname read_spectra
#'
#' @importFrom hexView readRaw blockString
#' @export
read_0 <- function(file = ".", ...) {
  if (!grepl("\\.[0-999]$", ignore.case = T, file)) stop("file type should be '0'")

  pa <- readRaw(file, offset = 0, nbytes = file.info(file)$size, human = "char",
                size = 1, endian = "little")
  pr <- pa$fileRaw

  # Get positions where the following parameters are found in the file
  codes <- c("ZFF", "RES", "SNM", "DAT", "LWN", "FXV", "LXV", "NPT", "MXY",
             "MNY", "END", "TIM")

  z <- grepRaw(codes[1],pr,all=TRUE)[1]+5
  re <- grepRaw(codes[2],pr,all=TRUE)[1]+5
  snm <- grepRaw(codes[3],pr,all=TRUE)[1]+7
  dat <- grepRaw(codes[4],pr,all=TRUE)[1]+7
  lwn <- grepRaw(codes[5],pr,all=TRUE)[1]+7
  fx <- grepRaw(codes[6],pr,all=TRUE)[3]+7
  lx <- grepRaw(codes[7],pr,all=TRUE)[3]+7
  npt0 <- grepRaw(codes[8],pr,all=TRUE)[2]+3
  npt1 <- grepRaw(codes[8],pr,all=TRUE)[3]+7
  mxy <- grepRaw(codes[9],pr,all=TRUE)[1]+7
  mny <- grepRaw(codes[10],pr,all=TRUE)[3]+7
  end <- grepRaw(codes[11],pr,all=TRUE)+11
  tim <- grepRaw(codes[12],pr,all=TRUE)+11

  ## calculate end and start of each block:
  offs <- sapply(5:10, function(x){end[x]})
  byts <- diff(offs)
  ZFF <- readRaw(file, offset=z, nbytes=4, human="int", size=2)[[5]][1]
  RES <- readRaw(file, offset=re, nbytes=4, human="int", size=2)[[5]][1]
  snm.lab.material <- blockString(readRaw(file, offset = snm, nbytes = 22, human = "char", size = 1, endian = "little"))

  ## Get number of data points for each spectra data block
  NPT0 <- readRaw(file, offset=npt0, nbytes=12, human="int", size=4)[[5]][2]
  NPT1 <- readRaw(file, offset=npt1, nbytes=4, human="int", size=4)[[5]][1]
  fxv <- readRaw(file, offset=fx, nbytes=16, human="real", size=8)[[5]][1] ## fxv:	Frequency of first point
  lxv <- readRaw(file, offset=lx, nbytes=16, human="real", size=8)[[5]][1] ## lxv:	Frequency of last point
  x <- rev(seq(lxv, fxv, (fxv-lxv)/(NPT1-1)))

  ## Read all through all the data blocks inside the OPUS file:
  nbytes1 <- NPT0*4 ## initial parameters
  smxa <- c()
  smna <- c()
  nbytes.f <- NPT1*4
  if(offs[1]<2000){
    offs.f<-offs[3]
  }

  if(offs[1]>20000){
    offs.f<-offs[2]
  }

  # Selected spectra block
  opus.p <- readRaw(file, width = NULL, offset = offs.f - 4,
                    nbytes = nbytes.f, human = "real", size = 4, endian = "little")
  y <- opus.p[[5]]

  data.frame(wavenumber = x, intensity = y)
}

#' @rdname read_spectra
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
